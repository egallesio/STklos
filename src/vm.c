/*
 * v m . c                              -- The STklos Virtual Machine
 *
 * Copyright © 2000-2024 Erick Gallesio <eg@stklos.net>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
 * USA.
 *
 *           Author: Erick Gallesio [eg@unice.fr]
 *    Creation date:  1-Mar-2000 19:51 (eg)
 */

#include "stklos.h"
#include "object.h"
#include "vm.h"
#include "vm-instr.h"
#include "struct.h"

// #define DEBUG_VM
/* #define STAT_VM  */

#ifdef STAT_VM
#  define DEBUG_VM
static int couple_instr[NB_VM_INSTR][NB_VM_INSTR];
static int cpt_inst[NB_VM_INSTR];
#endif

#ifdef DEBUG_VM
static int debug_level = 0;     /* 0 is quiet, 1, 2, ... are more verbose */
#endif


#if defined(__GNUC__) && !defined(DEBUG_VM)
   /* Use computed gotos to have better performance */
#  define USE_COMPUTED_GOTO
#  define CASE(x)       lab_##x:
#  define NEXT          goto *jump_table[fetch_next()]
#else
   /* Standard C compiler. Use the classic switch statement */
#  define CASE(x)       case x:
#  define NEXT          goto VM_LOOP_TOP /* NOT continue, as it interacts badly with
                                            the do{...}while(0) guards. */
#endif

#define NEXT0           do{vm->val = STk_void; vm->valc = 0; NEXT;}while(0)
#define NEXT1           do{vm->valc = 1; NEXT;}while(0)


#ifdef sparc
#  define FLUSH_REGISTERS_WINDOW()      asm("t 0x3") /* Stolen in Elk 2.0 source */
#else
#  define FLUSH_REGISTERS_WINDOW()
#endif


#define MY_SETJMP(jb)           (jb.blocked = get_signal_mask(), setjmp(jb.j))
#define MY_LONGJMP(jb, val)     (longjmp((jb).j, val))

#define FX(v)                   (STk_fixval(v))

static inline sigset_t get_signal_mask(void)
{
  sigset_t new, old;

  sigemptyset(&new);
  sigprocmask(SIG_BLOCK, &new, &old);
  return old;
}

static inline void set_signal_mask(sigset_t mask)
{
  sigprocmask(SIG_SETMASK, &mask, NULL);
}

/*===========================================================================*\
 *
 *                              G L O B A L S
 *
\*===========================================================================*/

/*
 * All the global variables values of a program (from all modules) are stored
 * in the global_store array. The global variable names of a module are
 * stored in a module hashtable (see hash.c). All the names with same hash
 * value are stored in a A-list of the form ((foo . i1) (bar . i2) ...) where
 * the value associated to a key it an integer (the index where the variable
 * is stored in STk_global_store).
 */
#define GLOBAL_STORE_INIT_SIZE 3000 // ~3000 symbols used when we are in REPL

SCM **STk_global_store; /* The store for all global variables */

int STk_reserve_store(void)
{
  static int global_store_len  = GLOBAL_STORE_INIT_SIZE;
  static int global_store_used = 0;
  MUT_DECL(store_lock);
  
  int res; // Build result in the mutex lock section

  MUT_LOCK(store_lock);

  if (global_store_used >= global_store_len) { /* resize the checked  array */
    // fprintf(stderr, "**** Resizing storage from %d to %d\n", global_store_len,
    //                  global_store_len + global_store_len/2);

    global_store_len += global_store_len / 2;
    STk_global_store  = STk_must_realloc(STk_global_store,
                                         global_store_len * sizeof(SCM*));
  }
  res = global_store_used++;

  MUT_UNLOCK(store_lock);
  return res;
}


SCM STk_global_store_define(SCM descr, SCM var, SCM value)
{
  // descr is
  //  - NULL if variable var was not already defined
  //  - a list of the form (var . index) otherwise where index locates the
  //   index of the value in STk_global_store array
  // return value is a filled descriptor

  if (!descr) {
    /* Define a new variable (not defined before) */
    descr = STk_cons(var, MAKE_INT(STk_reserve_store()));
  } else {
    /* It's a redefinition, not a new binding, clear the CONST bit */
    BOXED_INFO(descr) &= (~CONS_CONST);

    /* If variable was an alias, unalias it. */
    if (BOXED_INFO(descr) & CONS_ALIAS) {
      /* We redefine an alias to a new value */
      CDR(descr) = MAKE_INT(STk_reserve_store());
      BOXED_INFO(descr)  &= ~CONS_ALIAS;
    }
  }

  /* Finally, set the variable to the given value */
  vm_global_set(descr, value);
  return descr;
}

SCM STk_global_store_alias(SCM descr, SCM v, SCM old)
{
  if (descr) {
    /* Variable already exists. Change its index*/
    CDR(descr) = old;
  } else {
    /* Enter the new variable in table */
    descr = STk_cons(v, old);
  }
  /* Retain that we have an alias (and that this symbol is read-only) */
  BOXED_INFO(descr) |= (CONS_CONST | CONS_ALIAS);

  return descr;
}


/*===========================================================================*\
 *
 *                      V M   S T A C K   &   C O D E
 *
\*===========================================================================*/

/* ==== Stack access macros ==== */
#define push(v)         (*(--(vm->sp)) = (v))
#define pop()           (*((vm->sp)++))
//FIX: Optim de la fin
#define IS_IN_STACKP(a) ((vm->stack <= (SCM*)(a))                 &&  \
                         ((SCM*)(a) < &vm->stack[vm->stack_len]))

/* ==== Code access macros ==== */
#define fetch_next()           (*(vm->pc)++)
#define fetch_const()          (vm->constants[fetch_next()])
#define look_const()           (vm->constants[*(vm->pc)])
#define fetch_global()         (STk_global_store[(unsigned) fetch_next()])
#define global_var_index(ref)  ((unsigned)INT_VAL(CDR(ref)))


/*===========================================================================*\
 *
 *                      V M   T H R E A D
 *
\*===========================================================================*/
vm_thread_t *STk_allocate_vm(int stack_size)
{
  vm_thread_t *vm = STk_must_malloc(sizeof(vm_thread_t));

  /* Allocate the stack */
  vm->stack_len = stack_size;
  vm->stack     = STk_must_malloc(stack_size * sizeof(SCM));
  if (!vm->stack) {
    fprintf(stderr, "cannot allocate a stack with a size of %d cells\n", stack_size);
    fflush(stderr);
    STk_exit(MAKE_INT(1));
  }

  /* Initialize the VM registers */
  vm->sp             = vm->stack + vm->stack_len;
  vm->fp             = vm->sp;
  vm->val            = STk_void;
  vm->current_module = STk_current_module();
  vm->env            = vm->current_module;
  vm->handlers       = NULL;
  vm->top_jmp_buf    = NULL;
  vm->start_stack    = 0;               /* MUST be initialized later */
  vm->scheme_thread  = STk_false;
  vm->dynwind_stack  = LIST1(STk_false);

  return vm;
}


/*
 * Activation records
 *
 */

#define ACT_RECORD_SIZE    7

#define ACT_VARARG(reg)    ((reg)[0]) /* placeholder for &rest parameters */
#define ACT_SAVE_ENV(reg)  ((reg)[1])
#define ACT_SAVE_PC(reg)   ((reg)[2])
#define ACT_SAVE_CST(reg)  ((reg)[3])
#define ACT_SAVE_FP(reg)   ((reg)[4])
#define ACT_SAVE_PROC(reg) ((reg)[5])
#define ACT_SAVE_INFO(reg) ((reg)[6])

/*
 * VM state
 *
 */
#define VM_STATE_SIZE 5
#define VM_STATE_PC(reg)        ((reg)[0])
#define VM_STATE_CST(reg)       ((reg)[1])
#define VM_STATE_ENV(reg)       ((reg)[2])
#define VM_STATE_FP(reg)        ((reg)[3])
#define VM_STATE_JUMP_BUF(reg)  ((reg)[4])

#define SAVE_VM_STATE()               do{               \
  vm->sp                   -= VM_STATE_SIZE;            \
  VM_STATE_PC(vm->sp)       = (SCM) vm->pc;             \
  VM_STATE_CST(vm->sp)      = (SCM) vm->constants;      \
  VM_STATE_ENV(vm->sp)      = (SCM) vm->env;            \
  VM_STATE_FP(vm->sp)       = (SCM) vm->fp;             \
  VM_STATE_JUMP_BUF(vm->sp) = (SCM) vm->top_jmp_buf;    \
}while(0)

#define FULL_RESTORE_VM_STATE(p)      do{                       \
  vm->pc                     = (STk_instr *) VM_STATE_PC(p);    \
  RESTORE_VM_STATE(p);                                          \
}while(0)

#define RESTORE_VM_STATE(p)           do{                       \
  /* pc is not restored here. See FULL_RESTORE_VM_STATE */      \
  vm->constants          = (SCM *)  VM_STATE_CST(p);            \
  vm->env                = (SCM)    VM_STATE_ENV(p);            \
  vm->fp                 = (SCM *)  VM_STATE_FP(p);             \
  vm->top_jmp_buf        = (jbuf *) VM_STATE_JUMP_BUF(p);       \
  vm->sp                += VM_STATE_SIZE;                       \
}while(0)


/*
 * Handlers
 *
 */
#define EXCEPTION_HANDLER_SIZE 3

#define HANDLER_PROC(reg)       ((reg)[0])
#define HANDLER_END(reg)        ((reg)[1])
#define HANDLER_PREV(reg)       ((reg)[2])


#define SAVE_HANDLER_STATE(proc, addr)  do{             \
  vm->sp                   -= EXCEPTION_HANDLER_SIZE;   \
  HANDLER_PROC(vm->sp)  =  (SCM) (proc);                \
  HANDLER_END(vm->sp)   =  (SCM) (addr);                \
  HANDLER_PREV(vm->sp)  =  (SCM) vm->handlers;          \
  vm->handlers          = vm->sp;                       \
}while(0)

#define UNSAVE_HANDLER_STATE()  do{                     \
  SCM *old = vm->handlers;                              \
                                                        \
  vm->handlers = (SCM *) HANDLER_PREV(vm->handlers);    \
  vm->sp       = old + EXCEPTION_HANDLER_SIZE;          \
}while(0)


/*===========================================================================*\
 *
 *                      C A L L S
 *
\*===========================================================================*/

#define PREP_CALL() do{                                 \
  SCM fp_save = (SCM)(vm->fp);                          \
                                                        \
  /* Push an activation record on the stack */          \
  vm->sp -= ACT_RECORD_SIZE;                            \
  vm->fp  = vm->sp;                                     \
  ACT_SAVE_FP(vm->fp)   = fp_save;                      \
  ACT_SAVE_PROC(vm->fp) = STk_false;                    \
  ACT_SAVE_INFO(vm->fp) = STk_false;                    \
  /* Other fields will be initialized later */          \
}while(0)


#define RET_CALL() do{                                  \
  vm->sp        = vm->fp + ACT_RECORD_SIZE;             \
  vm->env       = ACT_SAVE_ENV(vm->fp);                 \
  vm->pc        = ACT_SAVE_PC(vm->fp);                  \
  vm->constants = ACT_SAVE_CST(vm->fp);                 \
  vm->fp        = ACT_SAVE_FP(vm->fp);                  \
}while(0)


/*
 *                       M i s c .
 */



#define FIRST_BYTE(n)  ((n) >> 8)
#define SECOND_BYTE(n) ((n) & 0xff)




#define PUSH_ENV(nargs, func, next_env)  do{    \
    BOXED_TYPE(vm->sp)   = tc_frame;            \
    FRAME_LENGTH(vm->sp) = nargs;               \
    FRAME_NEXT(vm->sp)   = next_env;            \
    FRAME_OWNER(vm->sp)  = func;                \
}while(0)

#define CALL_CLOSURE(func) do{                  \
    vm->pc        = CLOSURE_BCODE(func);        \
    vm->constants = CLOSURE_CONST(func);        \
    vm->env       = (SCM) vm->sp;               \
}while(0)



typedef SCM (*prim0)(void);
typedef SCM (*prim1)(SCM);
typedef SCM (*prim2)(SCM,SCM);
typedef SCM (*prim3)(SCM,SCM,SCM);
typedef SCM (*prim4)(SCM,SCM,SCM,SCM);
typedef SCM (*prim5)(SCM,SCM,SCM,SCM,SCM);
typedef SCM (*primv)(int,SCM*);


#define CALL_PRIM(v, args) do{                  \
    ACT_SAVE_PROC(vm->fp) = v;                  \
    v = PRIMITIVE_FUNC(v)args;                  \
}while(0)



#define CALL_PRIMITIVE(type, v, args) do{       \
    ACT_SAVE_PROC(vm->fp) = v;                  \
    v = (* (type) PRIMITIVE_FUNC(v))args;       \
}while(0)

#define CALL_PRIM0(v, args) CALL_PRIMITIVE(prim0, v, args)
#define CALL_PRIM1(v, args) CALL_PRIMITIVE(prim1, v, args)
#define CALL_PRIM2(v, args) CALL_PRIMITIVE(prim2, v, args)
#define CALL_PRIM3(v, args) CALL_PRIMITIVE(prim3, v, args)
#define CALL_PRIM4(v, args) CALL_PRIMITIVE(prim4, v, args)
#define CALL_PRIM5(v, args) CALL_PRIMITIVE(prim5, v, args)
#define CALL_PRIMV(v, args) CALL_PRIMITIVE(primv, v, args)




#define REG_CALL_PRIM(name) do{                           \
  extern struct primitive_obj CPP_CONCAT(STk_o_, name);   \
  ACT_SAVE_PROC(vm->fp) = &CPP_CONCAT(STk_o_, name);      \
}while(0)


#define RETURN_FROM_PRIMITIVE() do{             \
    vm->sp = vm->fp + ACT_RECORD_SIZE;          \
    vm->fp = (SCM *) ACT_SAVE_FP(vm->fp);       \
}while(0)

static void run_vm(vm_thread_t *vm);


/*===========================================================================*\
 *
 *                              Utilities
 *
\*===========================================================================*/

#ifdef DEBUG_VM
void STk_print_vm_registers(char *msg, STk_instr *code)
{
  vm_thread_t *vm = STk_get_current_vm();
  if (IS_IN_STACKP(vm->env))
    STk_fprintf(STk_stderr, "%s VAL=~S PC=%d SP=%d FP=%d CST=%x ENV=%x (%d)\n",
                msg, vm->val, vm->pc - code, vm->sp - vm->stack,
                vm->fp - vm->stack, vm->constants, vm->env,
                (SCM*)vm->env - vm->stack);
  else
    STk_fprintf(STk_stderr, "%s VAL=~S PC=%d SP=%d FP=%d CST=%x ENV=%x (%d)",
                msg, vm->val, vm->pc - code, vm->sp - vm->stack,
                vm->fp - vm->stack, vm->constants, vm->env,
                (SCM*)vm->env - vm->stack);
}

#endif


/* listify_top (n, vm):
 *   Pops n values from the stack of virtual machine vm
 *   and returns a list with them.  The CAR of the list
 *   will be the element that was deepest on the stack.
 */
static inline SCM listify_top(int n, vm_thread_t *vm)
{
  SCM *p, res = STk_nil;

  for (p = vm->sp, vm->sp+=n; p < vm->sp; p++)
    res = STk_cons(*p, res);
  return res;
}


static inline SCM clone_env(SCM e, vm_thread_t *vm)
{
  /* clone environment til we find one which is in the heap */
  if (IS_IN_STACKP(e))
      if (FRAMEP(e)){
          e = STk_clone_frame(e);
          FRAME_NEXT(e) = clone_env((SCM) FRAME_NEXT(e), vm);
      }
  return e;
}

static void patch_environment(vm_thread_t *vm)
{
  SCM *lfp;

  //STk_debug("<<<<<<<<<<");
  for (lfp = vm->fp; lfp; lfp = ACT_SAVE_FP(lfp)) {
    if (!ACT_SAVE_ENV(lfp)) break;

    //STk_debug("++++ %d", ACT_SAVE_ENV(lfp));
    ACT_SAVE_ENV(lfp) = clone_env(ACT_SAVE_ENV(lfp), vm);
    //STk_debug("---");
  }
  //STk_debug(">>>>>>>>>>");
}

static void error_bad_arity(SCM func, int arity, int16_t given_args, vm_thread_t *vm)
{
  ACT_SAVE_PROC(vm->fp) = func;
  if (arity >= 0)
    STk_error("%d argument%s required in call to ~S (%d provided)",
              arity, ((arity>1)? "s": ""), func, given_args);
  else
    STk_error("~S requires at least %d argument%s (%d provided)",
              func, -arity-1, ((-arity-1) > 1 ? "s" : ""), given_args);
}


/* adjust_stack_get_arity (func, nargs, vm):
 *    This function has one side effect and one result:
 *    SIDE EFFECT:
 *    - if arity >= 0 then nargs must be equal to it, or this is an error
 *    - if arity <  0 then
 *         + if the procedure was defined as (f a b . rest), there is a
 *           minimum number of arguments; the rest should be passed as a list.
 *           The stack is then transformed:
 *
 *           +---+---+---+---+---       +---+---+---
 *           | 1 | 2 | 3 | 4 | 5    ->  | 1 | 2 | L
 *           +---+---+---+---+---       +---+---+---
 *
 *            L = (3 4 5)
 *
 *           So the arguments seen inside the function are (as expected)
 *           a=1, b=2, rest=(3 4 5).
 *
 *         + if the procedure has optional or keyword arguments, they
 *           will be pushed into the stack in the following order:
 *           1. optionals
 *           2. tests for optionals
 *           3. keyword args
 *           4. tests for keyword args
 *           5. rest list
 *
 *           So, for example, if the procedure is
 *
 *           (G a b :optional (c 10 c?) (d 20)
 *                  :key      (e -1 e?) (f -2))
 *
 *           And the user calls
 *
 *           (G 1 2 -10 :f -20)
 *
 *           The stack is transformed
 *
 *           +---+---+-----+----+----
 *           | 1 | 2 | -10 | :f | -20
 *           +---+---+-----+----+----
 *
 *                  +---+---+-----+----+----+----+-----+----+---
 *              ->  | 1 | 2 | -10 | 20 | #t | -1 | -20 | #f | L
 *                  +---+---+-----+----+----+----+-----+----+---
 *                             ^     ^    ^    ^    ^     ^
 *                             c     d    c?   e    f     f?
 *
 *                  L = ( -10 :f -20 )
 *
 *    RETURN VALUE:
 *    - The return value is the adjusted arity: if arity >= 0,
 *      it is returned. If arity < 0, then it is because the arity is not
 *      fixed, and the procedure can take several arguments, but at least
 *      k are mandatory. Then k is returned.
 */
static inline int16_t adjust_stack_get_arity(SCM func, int16_t nargs, vm_thread_t *vm)
{
  int16_t arity = CLOSURE_ARITY(func);

  if (arity != nargs) {
    if (arity >= 0)
      error_bad_arity(func, arity, nargs, vm);
    else {
      int16_t min_arity = -arity-1;

      if (nargs < min_arity)
        error_bad_arity(func, arity, nargs, vm);
      else {
        /* Make a list from the non-required arguments which are on the stack. */
        SCM rest = listify_top(nargs - min_arity, vm);

        /* Reminder for optional and keyword args list:
           --------------------------------------------

           Keyword and optionals lists are reprsented as follows.
           If the formal parameters used in the definition of the
           lambda are:

           (... :key (a 5 a?) (b "b") c)

           Then the list will be represented internally in the
           procedure the assoc list:

           ( (:a 5 a?) (:b "b") (:c #f) )

           And

           (... :optional (a 5 a?) (b "b") c)

           Then the list will be represented internally in the
           procedure the assoc list:

           ( (a 5 a?) (b "b") (c #f) )

           (The only difference between optionals and keyword specifications
            internally in the procedure is that the keyword list has keywords
            where the optional list has symbols)

           So:
           - If the user does not include the predicate, we don't
             include it in the keyword list either.
           - If the user does not include a default, we DO include #f.        */

        /* Reminder for the order of arguments:
           ------------------------------------

           The code generated by the compiler expects variables to be as this
           in the stack:
           - mandatory variables
           - optionals (ALL of them must be pushed)
           - optional test values (only those that the user defined)
           - keywords (ALL of them must be pushed)
           - keyword test values (only those that the user defined)
           - rest list                                                         */

        SCM optionals = STk_key_get(CLOSURE_PLIST(vm->val),
                                    STk_key_opt,
                                    STk_nil);

        SCM opt = optionals;
        int n_opts = 0; /* we'll count how many */

        SCM keywords = STk_key_get(CLOSURE_PLIST(vm->val),
                                    STk_key_key,
                                    STk_nil);
        SCM key = keywords;
        SCM ptr = rest;

        /* I) Optionals */

        /* We first push the optional arguments given by the user...
           We stop when:
           - we reach nil, or
           - we find some keyword that was specified for this lambda. */
        while ((opt != STk_nil) &&
               (ptr != STk_nil) &&
               (STk_assq(CAR(ptr), key) == STk_false)) {
            /* Note about the assq above: if there was a keyword K with the
               default value #f, then the assoc list would include
               (K #f), and assq would return that list, "(K #f)", so
               checking for false there is OK. */
            push(CAR(ptr));
            opt = CDR(opt);
            ptr = CDR(ptr);
            n_opts++;
        }
        /* If the user defined a :rest parameter, it should hold the list
           of parameters *after* the optionals: */
        rest = ptr;

        /* Optionals given by the user are over, so now we push
           defaults for any others that were missing. Since the
           optional list is an assoc list, we push the cadar. */
        while (opt != STk_nil) {
          push(CAR(CDR(CAR(opt))));
          opt = CDR(opt);
        }

        /* Tests for optionals */
        opt = optionals;
        int i=0;
        while (opt != STk_nil) {
          /* (car opt) is (var value test?) so
             The test is caddar of opt, and we just need to check
             if caddar is false or not. */
          /* FIXME: we programatically generate this list so that the
                    CADDAR below is always valid. Should we actually
                    check for CONSP(CAR(opt)), then CONSP(CDR(CAR(opt))),
                    and so on? Looks like it'd potentially slow things
                    down a bit too much... */
          if (CAR(CDR(CDR(CAR(opt)))) != STk_false) { /* test was specified */
            push ((i < n_opts)
                  ? STk_true
                  : STk_false);
          }
          i++;
          opt = CDR(opt);
        }


        /* II) Keywords */

        if (optionals != STk_nil && keywords == STk_nil && ptr != STk_nil)
            STk_error("no keyword list allowed, but arguments found after optionals");

        /* For each defined keyword in this procedure, we push
           either what the user passed (by using get-key in ptr),
           or its default (the cadar of keyword list). */
        while (key != STk_nil) {
          push( STk_key_get(ptr,
                            CAR(CAR(key)),        /* key name */
                            CAR(CDR(CAR(key))))); /* key default */
          key = CDR(key);
        }

        /* Tests for keywords */
        key = keywords;

        /* `dummy` will be used below for searching the value of
           the test for presence of a variable.
           So dummy just needs to be not EQ? any key_get result. We
           build a list, which won't be EQ? to any other object. */
        SCM dummy = LIST1(MAKE_INT(42));

        while (key != STk_nil) {
          /* (car opt) is (var value test?) so
             The test is caddar of opt, and we just need to check
             if caddar is false or not. */

          if (CAR(CDR(CDR(CAR(key)))) != STk_false) { /* test was specified */
            if (STk_key_get(ptr,
                            CAR(CAR(key)),     /* key name */
                            dummy) == dummy) { /* key not found */
              push (STk_false);
            } else {
              push (STk_true);
            }
          }
          key = CDR(key);
        }

        /* III) Rest */

        /* We push the rest list, which is the list of whatever comes
           after the optionals (even if there are keyword
           arguments): */
        push(rest);

      }

      return -arity;
    }
  }
  return arity;
}

/*===========================================================================*\
 *
 *                                    C A L L S
 *
\*===========================================================================*/

/*
<doc  apply
 * (apply proc arg1 ... args)
 *
 * |Proc| must be a procedure and |args| must be a list. Calls |proc| with the
 * elements of the list
 * @lisp
 * (append (list arg1 ...) args)
 * @end lisp
 * as the actual arguments.
 * @lisp
 * (apply + (list 3 4))              =>  7
 *
 * (define compose
 *   (lambda (f g)
 *      (lambda args
 *        (f (apply g args)))))
 *
 * ((compose sqrt *) 12 75)          =>  30
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("apply", scheme_apply, apply, (void))
{
  /* This function is never called. It is just here to declare the primitive
   * apply, as a primitive of type tc_apply
   */
  STk_panic("Inside apply. Should not occur");
  return STk_void;
}


/*===========================================================================*\
 *
 *                              S T k _ C _ a p p l y
 *
 *
 * Execute a Scheme function from C. This function can be used as
 * an "execv" or an "execl" function. If nargs is > 0 it is as a Unix "execl"
 * function:
 *    STk_C_apply(STk_cons, 2, MAKE_INT(1), MAKE_INT(2)) => (1 . 2)
 * If nargs is < 0, we have something similar to an "execv" function
 *    STk_C_apply(...STk_intern("cons")..., -2, Argv)
 * where Argv[0] == MAKE_INT(1) and Argv[1] == MAKE_INT(2) ==> (1 . 2)
 *
\*===========================================================================*/
SCM STk_C_apply(SCM func, int nargs, ...)
{
  STk_instr code[]= {INVOKE, 0, END_OF_CODE};
  va_list ap;
  vm_thread_t *vm = STk_get_current_vm();
  int i;

  va_start(ap, nargs);
  //  sp -= VM_STATE_SIZE;
  SAVE_VM_STATE();                                  /* Save the VM regs */
  PREP_CALL();                                      /* PREPARE_CALL */

  if (nargs < 0) {                                  /* Push the arguments */
    /* args are in argc/argv form */
    SCM *argv = va_arg(ap, SCM*);

    nargs = -nargs;

    for (i = 0; i < nargs; i++) push(*argv++);
  } else {
    /* We have nargs SCM parameters to read */
    for (i = 0; i < nargs; i++) push(va_arg(ap, SCM));
  }
  va_end(ap);

  code[1]     = (int16_t) nargs;                    /* Patch # of args  */
  vm->val     = func;                               /* Store fun in VAL */
  vm->pc      = code;
  run_vm(vm);

  FULL_RESTORE_VM_STATE(vm->sp);

  return (vm->valc) ? vm->val : STk_void;
}

/* Another way to call apply from C. This time with a Scheme list */
SCM STk_C_apply_list(SCM func, SCM l)
{
  int i, argc = STk_int_length(l);
  SCM *argv = NULL;

  if (argc > 0) {
    argv = STk_must_malloc(argc * sizeof (SCM *));
    for (i = 0; i < argc; i++) {
      argv[i] = CAR(l);
      l = CDR(l);
    }
  }
  return STk_C_apply(func, -argc, argv);
}


DEFINE_PRIMITIVE("%execute", execute, subr23, (SCM code, SCM consts, SCM envt))
{
  int i, len;
  STk_instr *vinstr, *p;
  vm_thread_t *vm = STk_get_current_vm();

  if (!envt) envt = vm->current_module;

  if (!VECTORP(code))   STk_error("bad code vector ~S", code);
  if (!VECTORP(consts)) STk_error("bad constant list ~S", consts);
  if (!MODULEP(envt))   STk_error("bad module for evaluation ~S", envt);

  /* convert code to a vector of instructions */
  len = VECTOR_SIZE(code);
  vinstr = p = STk_must_malloc(len * sizeof(STk_instr));

  for (i = 0; i < len; i++)
    *p++ = (STk_instr) STk_integer_value(VECTOR_DATA(code)[i]);

  SAVE_VM_STATE();
  vm->pc        = vinstr;
  vm->constants = VECTOR_DATA(consts);
  vm->env       = envt;
  run_vm(vm);
  FULL_RESTORE_VM_STATE(vm->sp);

  return vm->val;
}


/*===========================================================================*\
 *
 *                              V A L U E S
 *
\*===========================================================================*/
/*
<doc values
 * (values obj ...)
 *
 * Delivers all of its arguments to its continuation.
 *
 * NOTE:  R5RS imposes to use multiple values in the context
 * of a |call-with-values|. In STklos, if |values| is not used with
 * |call-with-values|, only the first value is used (i.e. others values are
 * _ignored_)).
 *
doc>
*/
DEFINE_PRIMITIVE("values", values, vsubr, (int argc, SCM *argv))
{
  vm_thread_t *vm = STk_get_current_vm();
  int i;

  if (argc == 0)
    vm->val = STk_void;
  else {
    vm->val = argv[0];
    if (argc <= MAX_VALS) {
      for (i = 1; i < argc; i++)
        vm->vals[i] = argv[-i];
    } else {
      /* More than MAX_VALS values. Use a vector and store it in vals[0] */
      SCM tmp = STk_makevect(argc, (SCM) NULL);

      for (i = 0; i < argc; i++) VECTOR_DATA(tmp)[i] = *argv--;
      vm->vals[0] = tmp;
    }
  }

  /* Retain in valc the number of values */
  vm->valc = argc;
  return vm->val;
}


DEFINE_PRIMITIVE("%call-for-values", call_for_values, subr1, (SCM prod))
{
  vm_thread_t *vm = STk_get_current_vm();
  int len;

  /* Don't test that prod is procedure (apply will fail if this not the case) */
  STk_C_apply(prod, 0);
  len = vm->valc;
  vm->valc = 1;

  /* We don't use STk_values2vector here since we will call apply with the
   * values produced by "prod" ⟹ build a list here. There is too much allocation
   * here. :-(
   */
  switch (len) {
    case 0: return STk_nil;
    case 1: return LIST1(vm->val);
    default:  {
                SCM  res = STk_nil;
                if (len <= MAX_VALS) {
                  for (int i = len-1; i >= 1; i--)
                    res = STk_cons(vm->vals[i], res);
                  return STk_cons(vm->val, res);
                } else {
                  return STk_vector2list(vm->vals[0]);
                }
              }
  }
}


SCM STk_n_values(int n, ...)
{
  vm_thread_t *vm = STk_get_current_vm();

  vm->valc = n;

  if (!n)
    vm->val = STk_void;
  else {
    va_list ap;
    int i;

    va_start(ap, n);
    vm->val = va_arg(ap, SCM);

    if (n <= MAX_VALS) {
      for (i = 1; i < n; i++)
        vm->vals[i] = va_arg(ap, SCM);
    } else {
      /* More than MAX_VALS values. Use a vector and store it in vals[0] */
      SCM tmp = STk_makevect(n, (SCM) NULL);

      for (i = 0; i < n; i++) VECTOR_DATA(tmp)[i] = va_arg(ap, SCM);
      vm->vals[0] = tmp;
    }
    va_end(ap);
  }
  return vm->val;
}



SCM STk_values2vector(SCM obj, SCM vect)
{
  vm_thread_t *vm = STk_get_current_vm();
  SCM src, retval;
  int len = vm->valc;

  if (vect) {
    /* User has provided a vector for storing result */

    /* "not a vector" is an error on the C side of things;
       a wrong number of values could be triggered by
       errors in C or on the Scheme side... We'll give
       a clear message in this case (expected and given
       number of values). */
    if (!VECTORP(vect))
	STk_error("bad vector ~S", vect);
    if (VECTOR_SIZE(vect) != len)
	STk_error("expected %d values, but %d were given",
            VECTOR_SIZE(vect), len);
    retval = vect;
  } else {
    /* Allocate a new vector for result */
    retval = STk_makevect(len, STk_void);
  }

  vm->val  = obj;
  vm->valc = 1;

  if (len > 1) {                    /* multiple values */
    if (len <= MAX_VALS)  {
      vm->vals[0] = obj;
      src = vm->vals;
    } else {                        /* mono value */
      src = VECTOR_DATA(vm->vals[0]);
    }

    memcpy(VECTOR_DATA(retval), src, len * sizeof(SCM));
  } else if (len == 1) {
    *VECTOR_DATA(retval) = vm->val;
  }

  return retval;
}


/*===========================================================================*\
 *
 *                              V M _ D E B U G
 *
\*===========================================================================*/

/* Add support for debugging
 * vm_debug is called with the kind of desired support and sp. It returns
 * the number of elements used on the stack.
 */

// static void vm_debug(int kind, vm_thread_t *vm)
// {
//   switch (kind) {
//   case 0: /* old trace code position. Don't use it anymore. */
//     {
//       SCM line = vm->val;
//       SCM file = pop();
//       STk_panic("Recompile code in file ~S (contains obsolete line information)",
//                 file, line);
//       break;
//     }
//   case 1: /* Embed line information in a procedure call */
//     {
//       SCM line = vm->val;
//
//       ACT_SAVE_INFO(vm->fp) = STk_cons(pop(), line);
//       break;
//     }
//   }
// }

DEFINE_PRIMITIVE("%vm-backtrace", vm_bt, subr0, (void))
{
  SCM res, *lfp;
  vm_thread_t *vm = STk_get_current_vm();

  res = STk_nil;
  for (lfp = vm->fp; lfp; lfp = ACT_SAVE_FP(lfp)) {
    SCM self = (SCM) (ACT_SAVE_PROC(lfp));

    if (!self) break;

    res = STk_cons(STk_cons(self, ACT_SAVE_INFO(lfp)),
                   res);
  }
  return STk_dreverse(res);
}



#ifdef DEBUG_VM
#  ifdef STAT_VM
#    define DEFINE_NAME_TABLE
#    include "vm-instr.h"

static void dump_couple_instr(void)
{
  int i, j;
  FILE *dump;

  dump = fopen("/tmp/dump.out", "w");
  fprintf(dump, "[\n");

  for (i = NOP; i < NB_VM_INSTR; i++) {
    fprintf(dump, "((%s %d) ", name_table[i], cpt_inst[i]);
    for (j = NOP; j < NB_VM_INSTR; j++)
      fprintf(dump, "(%s %4d) ", name_table[j], couple_instr[i][j]);
    fprintf(dump, ")\n");
  }
  fprintf(dump, "\n]\n");
}
# endif
#endif


#ifdef STK_DEBUG
static void patch_environment(vm_thread_t *vm);
DEFINE_PRIMITIVE("%vm", set_vm_debug, vsubr, (int _UNUSED(argc), SCM _UNUSED(*argv)))
{
  /*
   * This function is just a placeholder for debugging the VM. Its body is
   * changed depending on the current bug to track.
   */

  //  patch_environment(STk_get_current_vm());
  STk_debug("Value %d = ~s", 1, MAKE_INT(1));

  return STk_void;
}

//#define VM_OFFSET(x) ((SCM) x - (SCM) vm->sp)
//
//static void show_stack_content(void)
//{
//  int i = 0;
//  vm_thread_t *vm = STk_get_current_vm();
//  char buff[10];
//
//  /* Show the registers */
//  STk_debug("=====================");
//  STk_debug("FP = %d", VM_OFFSET(vm->fp));
//  for (i=0; ;i++) {
//    STk_debug("offset %d value %d (0x%x)", i, vm->sp[i], vm->sp[i]);
//    fgets(buff, 10, stdin);
//    switch(*buff) {
//    case 's': STk_debug("Scheme value ~S",  vm->sp[i]); break;
//    case 'q': return;
//    default: /* nothing */;
//    }
//  }
//}
#endif


/*===========================================================================*\
 *
 *                     S T k l o s   V i r t u a l   M a c h i n e
 *
\*===========================================================================*/

/*
 * VM LOCKING
 * For optimization, some opcode/operand pairs get patched on the fly,
 * and replaced by another operation.  It's important that the two
 * reads (opcode and operand) happen atomically. If not, we can get this
 * situation:
 *   1) Thread A reads opcode at [n]
 *   2) Thread B suspends thread A, changes opcode at [n] and operand
 *      at [n+1]
 *   3) Thread A resumes, reads new operand at [n+1], which does not
 *      match the old opcode.
 *
 * To avoid this situation, and avoid a global lock around each
 * operation, we can do this:
 *    1) When we jump into one of the to-be-optimized opcodes, obtain
 *       the global lock.
 *    2) In case we hit the race condition (2, above), re-fetch and
 *       dispatch the current operand. We will either:
 *   3a) Re-dispatch to the same (to-be-optimized) opcode. Go ahead
 *       and optimize, then release lock.
 *   3b) We hit the race condition, and are dispatched to the new
 *       operand. Release the global lock and process the operation.
 *
 * We need to patch the opcode last, otherwise:
 *   1) Thread A obtains lock
 *   2) Modifies opcode at [n]
 *   3) Thread B interrupts thread A. Reads new opcode at [n], old
 *      operand at [n+1]
 *   4) Thread A resumes, updates operand at [n+1], releases lock
 */

MUT_DECL(global_code_lock);  /* Lock to permit code patching */

#define LOCK_AND_RESTART                     do{\
  if (!have_code_lock) {                        \
    MUT_LOCK(global_code_lock);                 \
    have_code_lock=1;                           \
    (vm->pc)--;                                 \
    NEXT;                                       \
  }                                             \
}while(0)
#define RELEASE_LOCK                         do{\
   {                                            \
    MUT_UNLOCK(global_code_lock);               \
    have_code_lock=0;                           \
   }                                            \
}while(0)
#define RELEASE_POSSIBLE_LOCK                do{\
  if (have_code_lock) {                         \
    MUT_UNLOCK(global_code_lock);               \
    have_code_lock=0;                           \
  }                                             \
}while(0)


static void run_vm(vm_thread_t *vm)
{
  jbuf jb;
  int16_t tailp;
  volatile int offset,
               have_code_lock = 0;     /* if true, we're patching the code */
  int nargs=0;

#if defined(USE_COMPUTED_GOTO)
#  define DEFINE_JUMP_TABLE
#  include "vm-instr.h"
#else
   int16_t byteop;
#endif
#if defined(DEBUG_VM)
#    define DEFINE_NAME_TABLE
#    include "vm-instr.h"
  static STk_instr *code_base = NULL;
#endif
#if defined(STAT_VM)
  static int16_t previous_op = NOP;
#endif

#if defined(USE_COMPUTED_GOTO)
  NEXT;
#else
  for ( ; ; ) {
  VM_LOOP_TOP:     /* Execution loop */
    byteop = fetch_next();
#  ifdef DEBUG_VM
    if (debug_level > 1)
      fprintf(stderr, "%08x [%03d]: %20s  sp=%-6d fp=%-6d env=%p\n",
              vm->pc - 1,
              vm->pc - code_base-1,
              name_table[(int)byteop],
              vm->sp - vm->stack,
              vm->fp - vm->stack, vm->env);
#    ifdef STAT_VM
    couple_instr[previous_op][byteop]++;
    cpt_inst[byteop]++;
    previous_op = byteop;
#    endif
#  endif
    switch (byteop) {
#endif /*  USE_COMPUTED_GOTO */


CASE(NOP) { NEXT; }


CASE(IM_FALSE)  { vm->val = STk_false;       NEXT1;}
CASE(IM_TRUE)   { vm->val = STk_true;        NEXT1;}
CASE(IM_NIL)    { vm->val = STk_nil;         NEXT1;}
CASE(IM_MINUS1) { vm->val = MAKE_INT(-1UL);  NEXT1;}
CASE(IM_ZERO)   { vm->val = MAKE_INT(0);     NEXT1;}
CASE(IM_ONE)    { vm->val = MAKE_INT(1);     NEXT1;}
CASE(IM_VOID)   { vm->val = STk_void;        NEXT1;}

CASE(SMALL_INT) { vm->val = MAKE_INT(fetch_next());             NEXT1;}
CASE(CONSTANT)  { vm->val = fetch_const();                      NEXT1;}

CASE(FALSE_PUSH)  { push(STk_false);       NEXT;}
CASE(TRUE_PUSH)   { push(STk_true);        NEXT;}
CASE(NIL_PUSH)    { push(STk_nil);         NEXT;}
CASE(MINUS1_PUSH) { push(MAKE_INT(-1UL));  NEXT;}
CASE(ZERO_PUSH)   { push(MAKE_INT( 0UL));  NEXT;}
CASE(ONE_PUSH)    { push(MAKE_INT(+1UL));  NEXT;}
CASE(VOID_PUSH)   { push(STk_void);        NEXT;}


CASE(INT_PUSH)      { push(MAKE_INT(fetch_next())) ; NEXT; }
CASE(CONSTANT_PUSH) { push(fetch_const());           NEXT; }


CASE(PUSH_GLOBAL_REF)
CASE(GLOBAL_REF) {
  SCM ref = NULL;
  int16_t orig_opcode;
  SCM orig_operand;

  LOCK_AND_RESTART;
  orig_opcode  = vm->pc[-1];
  orig_operand = fetch_const();

  if (orig_opcode == PUSH_GLOBAL_REF)
    push(vm->val);

  vm->val= STk_lookup(orig_operand, vm->env, &ref, FALSE);
  if (!ref) {
    RELEASE_LOCK;
    STk_error_unbound_variable(orig_operand, vm->current_module);
  }

  /* patch the code for optimize next accesses */
  vm->pc[-1]  = global_var_index(ref);
  vm->pc[-2]  = (orig_opcode == GLOBAL_REF) ? UGLOBAL_REF: PUSH_UGLOBAL_REF;
  RELEASE_LOCK;
  NEXT1;
}

CASE(PUSH_UGLOBAL_REF)
  push(vm->val);        /* Fall through */
CASE(UGLOBAL_REF) {     /* Never produced by compiler */
  /* Because of optimization, we may get re-dispatched to here. */
  RELEASE_POSSIBLE_LOCK;

  vm->val = fetch_global();
  NEXT1;
}

CASE(GLOBAL_REF_PUSH) {
  SCM ref = NULL;
  SCM orig_operand;
  SCM res;

  LOCK_AND_RESTART;
  orig_operand = fetch_const();

  res = STk_lookup(orig_operand, vm->env, &ref, FALSE);
  if (!ref) {
    RELEASE_LOCK;
    STk_error_unbound_variable(orig_operand, vm->current_module);
  }

  push(res);

  /* patch the code for optimize next accesses */
  vm->pc[-1]  = global_var_index(ref);
  vm->pc[-2]  = UGLOBAL_REF_PUSH;
  RELEASE_LOCK;
  NEXT1;
}

CASE(UGLOBAL_REF_PUSH) { /* Never produced by compiler */
  /* Because of optimization, we may get re-dispatched to here. */
  RELEASE_POSSIBLE_LOCK;

  push(fetch_global());
  NEXT1;
}


CASE(PUSH_GREF_INVOKE)
CASE(GREF_INVOKE) {
  SCM ref = NULL;
  int16_t orig_opcode;
  SCM orig_operand;

  LOCK_AND_RESTART;
  orig_opcode  = vm->pc[-1];
  orig_operand = fetch_const();

  if (orig_opcode == PUSH_GREF_INVOKE)
    push(vm->val);

  vm->val = STk_lookup(orig_operand, vm->env, &ref, FALSE);
  if (!ref) {
    RELEASE_LOCK;
    STk_error_unbound_variable(orig_operand, vm->current_module);
  }

  nargs = fetch_next();
  /* patch the code for optimize next accesses (pc[-1] is already equal to nargs)*/
  vm->pc[-2]  = global_var_index(ref);
  vm->pc[-3]  = (vm->pc[-3] == GREF_INVOKE)? UGREF_INVOKE : PUSH_UGREF_INVOKE;
  RELEASE_LOCK;

  /*and now invoke */
  tailp=FALSE; goto FUNCALL;
}

CASE(PUSH_UGREF_INVOKE)
  push(vm->val);        /* Fall through */
CASE(UGREF_INVOKE) { /* Never produced by compiler */

  /* Because of optimization, we may get re-dispatched to here. */
  RELEASE_POSSIBLE_LOCK;

  vm->val = fetch_global();
  nargs   = fetch_next();

  /* invoke */
  tailp = FALSE; goto FUNCALL;
}

CASE(PUSH_GREF_TAIL_INV)
CASE(GREF_TAIL_INVOKE) {
  SCM ref = NULL;
  int16_t orig_opcode;
  SCM orig_operand;

  LOCK_AND_RESTART;
  orig_opcode  = vm->pc[-1];
  orig_operand = fetch_const();

  if (orig_opcode == PUSH_GREF_TAIL_INV)
    push(vm->val);

  vm->val = STk_lookup(orig_operand, vm->env, &ref, FALSE);
  if (!ref) {
    RELEASE_LOCK;
    STk_error_unbound_variable(orig_operand, vm->current_module);
  }

  nargs = fetch_next();
  /* patch the code for optimize next accesses (pc[-1] is already equal to nargs)*/
  vm->pc[-2]  = global_var_index(ref);
  vm->pc[-3]  = (vm->pc[-3] == GREF_TAIL_INVOKE) ?
                     UGREF_TAIL_INVOKE: PUSH_UGREF_TAIL_INV;
  RELEASE_LOCK;

  /* and now invoke */
  tailp=TRUE; goto FUNCALL;
}

CASE(PUSH_UGREF_TAIL_INV)
  push(vm->val);        /* Fall through */
CASE(UGREF_TAIL_INVOKE) { /* Never produced by compiler */
  /* Because of optimization, we may get re-dispatched to here. */
  RELEASE_POSSIBLE_LOCK;

  vm->val = fetch_global();
  nargs   = fetch_next();

  /* invoke */
  tailp = TRUE; goto FUNCALL;
}



CASE(LOCAL_REF0) { vm->val = FRAME_LOCAL(vm->env, 0);        NEXT1;}
CASE(LOCAL_REF1) { vm->val = FRAME_LOCAL(vm->env, 1);        NEXT1;}
CASE(LOCAL_REF2) { vm->val = FRAME_LOCAL(vm->env, 2);        NEXT1;}
CASE(LOCAL_REF3) { vm->val = FRAME_LOCAL(vm->env, 3);        NEXT1;}
CASE(LOCAL_REF4) { vm->val = FRAME_LOCAL(vm->env, 4);        NEXT1;}
CASE(LOCAL_REF)  { vm->val = FRAME_LOCAL(vm->env, fetch_next()); NEXT1;}
CASE(DEEP_LOCAL_REF) {
  int level, info = fetch_next();
  SCM e = vm->env;

  /* STklos organizes local environments as this: each level has a
     maximum of 256 variables. Both the level and the address of local
     variables are encoded in a single 16-bit integer, as "256v1+v2".
     For example, 2*256 + 03 = 0x0203. The first byte, 0x02,
     identifies the level, and the second byte, 0x03, identifies the
     variable.  */

  /* Go down in the dynamic environment */
  for (level = FIRST_BYTE(info); level; level--)
    e = (SCM) FRAME_NEXT(e);

  vm->val = FRAME_LOCAL(e, SECOND_BYTE(info));
  NEXT1;
}

CASE(DEEP_LOC_REF_FAR) {
  /* DEEP-LOCAL-REF but FAR (arg is a cons). (This is inefficient but rare) */
  SCM info = fetch_const();
  int level;
  SCM e = vm->env;

  if (!CONSP(info) || !INTP(CAR(info)) || !INTP(CDR(info)))
    STk_panic("DEEP_LOCAL_REF_FAR with ~S", info);

  /* Go down in the dynamic environment */
  for (level = INT_VAL(CAR(info)); level; level--)
    e = (SCM) FRAME_NEXT(e);

  vm->val = FRAME_LOCAL(e, INT_VAL(CDR(info)));
  NEXT1;
}


CASE(DEEP_LOC_REF_PUSH) {
  int level, info = fetch_next();
  SCM e = vm->env;

  /* Go down in the dynamic environment */
  for (level = FIRST_BYTE(info); level; level--)
    e = (SCM) FRAME_NEXT(e);

  push(vm->val = FRAME_LOCAL(e, SECOND_BYTE(info)));
  NEXT1;
}



CASE(LOCAL_REF0_PUSH) {push(FRAME_LOCAL(vm->env, 0));  NEXT1;}
CASE(LOCAL_REF1_PUSH) {push(FRAME_LOCAL(vm->env, 1));  NEXT1;}
CASE(LOCAL_REF2_PUSH) {push(FRAME_LOCAL(vm->env, 2));  NEXT1;}
CASE(LOCAL_REF3_PUSH) {push(FRAME_LOCAL(vm->env, 3));  NEXT1;}
CASE(LOCAL_REF4_PUSH) {push(FRAME_LOCAL(vm->env, 4));  NEXT1;}

CASE(GLOBAL_SET) {
  SCM ref = NULL;
  SCM orig_operand;

  LOCK_AND_RESTART;
  orig_operand = fetch_const();

  STk_lookup(orig_operand, vm->env, &ref, FALSE);
  if (!ref) {
    RELEASE_LOCK;
    STk_error_unbound_variable(orig_operand, vm->current_module);
  }
  if (BOXED_INFO(ref) & CONS_CONST) {
    RELEASE_LOCK;
    STk_error("cannot mute the value of ~S in ~S", orig_operand, vm->current_module);
  }
  vm_global_set(ref, vm->val);
  /* patch the code for optimize next accesses */
  vm->pc[-1] = global_var_index(ref);
  vm->pc[-2] = UGLOBAL_SET;

  if (CLOSUREP(vm->val) && CLOSURE_NAME(vm->val) == STk_false) {
    /* We do something like (set! foo (lambda () ....)) and the lambda doesn't have a procedure
     * name in it. Just force the name of the closure to "foo". */
    CLOSURE_NAME(vm->val) = orig_operand;
  }

  RELEASE_LOCK;
  NEXT0;
 }


CASE(UGLOBAL_SET) { /* Never produced by compiler */
  /* Because of optimization, we may get re-dispatched to here. */
  RELEASE_POSSIBLE_LOCK;

  fetch_global() = vm->val; NEXT0;
}


CASE(LOCAL_SET0) { FRAME_LOCAL(vm->env, 0)           = vm->val; NEXT0;}
CASE(LOCAL_SET1) { FRAME_LOCAL(vm->env, 1)           = vm->val; NEXT0;}
CASE(LOCAL_SET2) { FRAME_LOCAL(vm->env, 2)           = vm->val; NEXT0;}
CASE(LOCAL_SET3) { FRAME_LOCAL(vm->env, 3)           = vm->val; NEXT0;}
CASE(LOCAL_SET4) { FRAME_LOCAL(vm->env, 4)           = vm->val; NEXT0;}
CASE(LOCAL_SET)  { FRAME_LOCAL(vm->env,fetch_next()) = vm->val; NEXT0;}


CASE(DEEP_LOCAL_SET) {
  int level, info = fetch_next();
  SCM e = vm->env;

  /* Go down in the dynamic environment */
  for (level = FIRST_BYTE(info); level; level--)
    e = (SCM) FRAME_NEXT(e);

  FRAME_LOCAL(e, SECOND_BYTE(info)) = vm->val;
  NEXT0;
}


CASE(DEEP_LOC_SET_FAR) {
  /* DEEP-LOCAL-SET but FAR (arg is a cons) (This is inefficient but rare) */
  SCM info = fetch_const();
  int level;
  SCM e = vm->env;

  if (!CONSP(info) || !INTP(CAR(info)) || !INTP(CDR(info)))
    STk_panic("DEEP_LOCAL_SET_FAR with ~S", info);

  /* Go down in the dynamic environment */
  for (level = INT_VAL(CAR(info)); level; level--)
    e = (SCM) FRAME_NEXT(e);

  FRAME_LOCAL(e, INT_VAL(CDR(info))) = vm->val;
  NEXT0;
}



CASE(GOTO) { offset = fetch_next(); vm->pc += offset; NEXT;}
CASE(JUMP_FALSE) {
  offset = fetch_next();
  if (vm->val == STk_false) vm->pc += offset;
  NEXT;
}
CASE(JUMP_TRUE) {
  offset = fetch_next();
  if (vm->val != STk_false) vm->pc += offset;
  NEXT;
}

CASE(JUMP_NUMDIFF) {
  offset = fetch_next(); if (!STk_numeq2(pop(), vm->val)) vm->pc += offset; NEXT;
}
CASE(JUMP_NUMEQ) {
  offset = fetch_next(); if (STk_numeq2(pop(), vm->val)) vm->pc += offset; NEXT;
}
CASE(JUMP_NUMLT) {
  offset = fetch_next(); if (STk_numlt2(pop(), vm->val)) vm->pc += offset; NEXT;
}
CASE(JUMP_NUMLE) {
  offset = fetch_next(); if (STk_numle2(pop(), vm->val)) vm->pc += offset; NEXT;
}
CASE(JUMP_NUMGT) {
  offset = fetch_next(); if (STk_numgt2(pop(), vm->val)) vm->pc += offset; NEXT;
}
CASE(JUMP_NUMGE) {
  offset = fetch_next(); if (STk_numge2(pop(), vm->val)) vm->pc += offset; NEXT;
}
CASE(JUMP_NOT_EQ) {
  offset = fetch_next(); if ((pop() != vm->val)) vm->pc += offset; NEXT;
}
CASE(JUMP_NOT_EQV) {
  offset = fetch_next();
  if ((STk_eqv(pop(), vm->val) == STk_false)) vm->pc += offset;
  NEXT;
}
CASE(JUMP_NOT_EQUAL) {
  offset = fetch_next();
  if ((STk_equal(pop(), vm->val)==STk_false)) vm->pc += offset;
  NEXT;
}


CASE(DEFINE_SYMBOL) {
  SCM var = fetch_const();

  STk_define_variable(var, vm->val, vm->env);
  if (CLOSUREP(vm->val) && CLOSURE_NAME(vm->val) == STk_false)
    CLOSURE_NAME(vm->val) = var;
  vm->val     = STk_void;
  vm->vals[1] = var;
  vm->valc    = 2;
  NEXT;
}


CASE(SET_CUR_MOD) {
  vm->env = vm->val;
  STk_select_module(vm->val);
  NEXT0;
}


CASE(POP)     { vm->val = pop(); NEXT1; }
CASE(PUSH)    { push(vm->val);   NEXT; }

CASE(CREATE_CLOSURE) {
  /* pc[0] = offset; pc[1] = arity ; code of the routine starts in pc+2 */
  vm->env    = clone_env(vm->env, vm);
  vm->val    = STk_make_closure(vm->pc+2, vm->pc[0]-1, vm->pc[1],
                                vm->constants, vm->env);
  vm->pc    += vm->pc[0] + 1;
  NEXT1;
}
CASE(CREATE_CLOSURE_FAR) {
  /* CREATE_CLOSURE but with a pc[0] which is a long constant */
  SCM offset = look_const();

  if (!INTP(offset)) STk_panic("CREATE_CLOSURE_FAR with offset=~S", offset);

  vm->env    = clone_env(vm->env, vm);
  vm->val    = STk_make_closure(vm->pc+2, INT_VAL(offset)-1, vm->pc[1],
                                vm->constants, vm->env);
  vm->pc    += INT_VAL(offset) + 1;
  NEXT1;
}

CASE(PREPARE_CALL) { PREP_CALL(); NEXT; }
CASE(RETURN)       { RET_CALL();  NEXT; }
CASE(INVOKE)       {
  nargs = fetch_next();
  tailp = FALSE; goto FUNCALL;
}

CASE(TAIL_INVOKE) {
  nargs = fetch_next();
  tailp = TRUE;
  goto FUNCALL;
}

CASE(PUSH_PREPARE_CALL) {push(vm->val); PREP_CALL(); NEXT; }

CASE(ENTER_LET_STAR) {
  nargs = fetch_next();

  /* roughly equivalent to PREPARE_CALL; nargs * PUSH; ENTER_LET */
  PREP_CALL();
  vm->sp -= nargs + (sizeof(struct frame_obj) - sizeof(SCM)) / sizeof(SCM);
  PUSH_ENV(nargs, vm->val, vm->env);
  vm->env = (SCM) vm->sp;
  NEXT;
}

CASE(ENTER_LET) {
  nargs = fetch_next();

  /* Push a new env. on the stack. Activation record does not need to be updated  */
  vm->sp -= (sizeof(struct frame_obj) - sizeof(SCM)) / sizeof(SCM);
  PUSH_ENV(nargs, vm->val, vm->env);
  vm->env = (SCM) vm->sp;
  NEXT;
}

CASE(LEAVE_LET) {
  vm->sp  = vm->fp + ACT_RECORD_SIZE;
  vm->env = FRAME_NEXT(vm->env);
  vm->fp  = ACT_SAVE_FP(vm->fp);
  NEXT;
}


CASE(ENTER_TAIL_LET_STAR) {
  nargs = fetch_next();

  /* roughly equivalent to PREPARE_CALL; nargs * PUSH; ENTER_TAIL_LET */
  PREP_CALL();
  vm->sp -= nargs;
  goto enter_tail_let;
}

CASE(ENTER_TAIL_LET) {
  nargs = fetch_next();
 enter_tail_let:
  {
    SCM *old_fp = (SCM *) ACT_SAVE_FP(vm->fp);

    /* Move the arguments of the function to the old_fp as in TAIL_INVOKE */
    if (IS_IN_STACKP(vm->env)) {
      if (nargs) memmove(((SCM*)vm->env)-nargs, vm->sp, nargs*sizeof(SCM));
      vm->fp = old_fp;

      /* Push a new environment on the stack */
      vm->sp = ((SCM*)vm->env) - nargs -
               ((sizeof(struct frame_obj) - sizeof(SCM)) / sizeof(SCM));
    }
    else {
      if (nargs) memmove(old_fp-nargs, vm->sp, nargs*sizeof(SCM));
      vm->fp = old_fp;
      vm->sp = vm->fp - nargs -
               ((sizeof(struct frame_obj) - sizeof(SCM)) / sizeof(SCM));
    }

    PUSH_ENV(nargs, vm->val, vm->env);
    vm->env  = (SCM) vm->sp;
    NEXT;
  }
}


CASE(PUSH_HANDLER) {
  offset = fetch_next();

do_push_handler:

  /* place the value in val on the stack as well as the value of handlers */
  if (STk_procedurep(vm->val) == STk_false)
    STk_error("bad exception handler ~S", vm->val);

  vm->top_jmp_buf = &jb;

  if (MY_SETJMP(jb)) {
    /* We come back from an error. */
    set_signal_mask(jb.blocked);
  }
  else {
    SAVE_VM_STATE();
    SAVE_HANDLER_STATE(vm->val, vm->pc+offset);
  }
  NEXT;
}

CASE(PUSH_HANDLER_FAR) {
  offset = INT_VAL(fetch_const());   /* Read a FAR offset in constants */
  goto do_push_handler;
}

CASE(POP_HANDLER) {
  UNSAVE_HANDLER_STATE();
  RESTORE_VM_STATE(vm->sp);
  NEXT;
}


CASE(FORMALS) {
  SCM formals = fetch_const();

  if (vm->valc == 1 && CLOSUREP(vm->val)) {
    CLOSURE_PLIST(vm->val) = STk_key_set(CLOSURE_PLIST(vm->val),
                                         STk_key_formals,
                                         formals);
  }
  NEXT;
}

CASE(DOCSTRG) {
  SCM str = fetch_const();

  if (vm->valc == 1 && CLOSUREP(vm->val)) {
    CLOSURE_PLIST(vm->val) = STk_key_set(CLOSURE_PLIST(vm->val),
                                         STk_key_doc,
                                         str);
  }
  NEXT;
}

 CASE(PROCNAME) {
   SCM name = fetch_const();

   if (vm->valc == 1 && CLOSUREP(vm->val)) {
     CLOSURE_NAME(vm->val) = name;
  }
  NEXT;
}

CASE(SOURCE) {
  SCM src = fetch_const();

  if (vm->valc == 1 && CLOSUREP(vm->val)) {
    CLOSURE_PLIST(vm->val) = STk_key_set(CLOSURE_PLIST(vm->val),
                                         STk_key_source,
                                         src);
  }
  NEXT;
}

CASE(CLOSURE_SET_OPT_KEY) {
  SCM str = fetch_const();

  if (vm->valc == 1 && CLOSUREP(vm->val)) {
    CLOSURE_PLIST(vm->val) = STk_key_set(CLOSURE_PLIST(vm->val),
                                         STk_key_opt,
                                         str);

    str = fetch_const();
    CLOSURE_PLIST(vm->val) = STk_key_set(CLOSURE_PLIST(vm->val),
                                         STk_key_key,
                                         str);
  }
  NEXT;
}

CASE(CALL_LOCATION) {
   ACT_SAVE_INFO(vm->fp) = STk_cons(pop(),                   /* file */
                                    MAKE_INT(fetch_next())); /* line */
  NEXT1;
}

CASE(INSCHEME) {
  vm->val = STk_symb_in_scheme(vm->val);
  NEXT1;
 }


CASE(END_OF_CODE) {
   return;
 }


CASE(DBG_VM)  {
  ;
}

CASE(UNUSED_4)
CASE(UNUSED_5)
CASE(UNUSED_6)
CASE(UNUSED_7)
CASE(UNUSED_8)
CASE(UNUSED_9)
CASE(UNUSED_10)
CASE(UNUSED_11)
CASE(UNUSED_12)
CASE(UNUSED_13)
CASE(UNUSED_14)
CASE(UNUSED_15)
CASE(UNUSED_16)
CASE(UNUSED_17)
CASE(UNUSED_18)
CASE(UNUSED_19)
CASE(UNUSED_20)
CASE(UNUSED_21)
CASE(UNUSED_22)
CASE(UNUSED_23)
CASE(UNUSED_24)
CASE(UNUSED_25)
CASE(UNUSED_26)
CASE(UNUSED_27)
CASE(UNUSED_28)
{
  ;
}




/******************************************************************************
 *
 *                           I n l i n e d   F u n c t i o n s
 *
 ******************************************************************************/
#define SCHEME_NOT(x) (((x) == STk_false) ? STk_true: STk_false)


CASE(IN_ADD2)   { REG_CALL_PRIM(plus);
                  vm->val = STk_add2(pop(), vm->val); NEXT1;}
CASE(IN_SUB2)   { REG_CALL_PRIM(difference);
                  vm->val = STk_sub2(pop(), vm->val); NEXT1;}
CASE(IN_MUL2)   { REG_CALL_PRIM(multiplication);
                  vm->val = STk_mul2(pop(), vm->val); NEXT1;}
CASE(IN_DIV2)   { REG_CALL_PRIM(division);
                  vm->val = STk_div2(pop(), vm->val); NEXT1;}

CASE(IN_FXADD2)   { REG_CALL_PRIM(fxplus);
                  vm->val = STk_fxplus(pop(), vm->val); NEXT1;}
CASE(IN_FXSUB2)   { REG_CALL_PRIM(fxminus);
                  vm->val = STk_fxminus(pop(), vm->val); NEXT1;}
CASE(IN_FXMUL2)   { REG_CALL_PRIM(fxtime);
                  vm->val = STk_fxtime(pop(), vm->val); NEXT1;}
CASE(IN_FXDIV2)   { REG_CALL_PRIM(fxdiv);
                  vm->val = STk_fxdiv(pop(), vm->val); NEXT1;}


CASE(IN_SINT_ADD2) { REG_CALL_PRIM(plus);
                     vm->val = STk_add2(vm->val, MAKE_INT(fetch_next())); NEXT1;}
CASE(IN_SINT_SUB2) { REG_CALL_PRIM(difference);
                     vm->val = STk_sub2(MAKE_INT(fetch_next()), vm->val); NEXT1;}
CASE(IN_SINT_MUL2) { REG_CALL_PRIM(multiplication);
                     vm->val = STk_mul2(vm->val, MAKE_INT(fetch_next())); NEXT1;}
CASE(IN_SINT_DIV2) { REG_CALL_PRIM(division);
                     vm->val = STk_div2(vm->val, MAKE_INT(fetch_next())); NEXT1;}


CASE(IN_SINT_FXADD2) { REG_CALL_PRIM(fxplus);
                     vm->val = STk_fxplus(vm->val, MAKE_INT(fetch_next())); NEXT1;}
CASE(IN_SINT_FXSUB2) { REG_CALL_PRIM(fxminus);
                     vm->val = STk_fxminus(vm->val, MAKE_INT(fetch_next())); NEXT1;}
CASE(IN_SINT_FXMUL2) { REG_CALL_PRIM(fxtime);
                     vm->val = STk_fxtime(vm->val, MAKE_INT(fetch_next())); NEXT1;}
CASE(IN_SINT_FXDIV2) { REG_CALL_PRIM(fxdiv);
                     vm->val = STk_fxdiv(vm->val, MAKE_INT(fetch_next())); NEXT1;}


CASE(IN_NUMEQ)  { REG_CALL_PRIM(numeq);
                  vm->val = MAKE_BOOLEAN(STk_numeq2(pop(), vm->val));      NEXT1;}
CASE(IN_NUMDIFF){ REG_CALL_PRIM(numeq);
                  vm->val = MAKE_BOOLEAN(!STk_numeq2(pop(), vm->val));     NEXT1;}
CASE(IN_NUMLT)  { REG_CALL_PRIM(numlt);
                  vm->val = MAKE_BOOLEAN(STk_numlt2(pop(), vm->val));      NEXT1;}
CASE(IN_NUMGT)  { REG_CALL_PRIM(numgt);
                  vm->val = MAKE_BOOLEAN(STk_numgt2(pop(), vm->val));      NEXT1;}
CASE(IN_NUMLE)  { REG_CALL_PRIM(numle);
                  vm->val = MAKE_BOOLEAN(STk_numle2(pop(), vm->val));      NEXT1;}
CASE(IN_NUMGE)  { REG_CALL_PRIM(numge);
                  vm->val = MAKE_BOOLEAN(STk_numge2(pop(), vm->val));      NEXT1;}

CASE(IN_FXEQ)  { REG_CALL_PRIM(fxeq);
                 vm->val = MAKE_BOOLEAN(STk_fixnum_cmp(pop(),vm->val)==0); NEXT1;}
CASE(IN_FXDIFF){ REG_CALL_PRIM(fxeq);
                 vm->val = MAKE_BOOLEAN(STk_fixnum_cmp(pop(),vm->val)!=0); NEXT1;}
CASE(IN_FXLT)  { REG_CALL_PRIM(fxlt);
                 vm->val = MAKE_BOOLEAN(STk_fixnum_cmp(pop(),vm->val)<0);  NEXT1;}
CASE(IN_FXGT)  { REG_CALL_PRIM(fxgt);
                 vm->val = MAKE_BOOLEAN(STk_fixnum_cmp(pop(),vm->val)>0);  NEXT1;}
CASE(IN_FXLE)  { REG_CALL_PRIM(fxle);
                 vm->val = MAKE_BOOLEAN(STk_fixnum_cmp(pop(),vm->val)<=0); NEXT1;}
CASE(IN_FXGE)  { REG_CALL_PRIM(fxge);
                 vm->val = MAKE_BOOLEAN(STk_fixnum_cmp(pop(),vm->val)>=0); NEXT1;}


CASE(IN_INCR)   { REG_CALL_PRIM(plus);
                  vm->val = STk_add2(vm->val, MAKE_INT(1)); NEXT1;}
CASE(IN_DECR)   { REG_CALL_PRIM(difference);
                  vm->val = STk_sub2(vm->val, MAKE_INT(1)); NEXT1;}

CASE(IN_CONS)   { vm->val = STk_cons(pop(), vm->val);                      NEXT1;}
CASE(IN_CAR)    { REG_CALL_PRIM(car); vm->val = STk_car(vm->val);          NEXT1;}
CASE(IN_CDR)    { REG_CALL_PRIM(cdr); vm->val = STk_cdr(vm->val);          NEXT1;}
CASE(IN_NULLP)  { vm->val = MAKE_BOOLEAN(vm->val == STk_nil);              NEXT1;}
CASE(IN_LIST)   { vm->val = listify_top(fetch_next(), vm);                 NEXT1;}
CASE(IN_NOT)    { vm->val = SCHEME_NOT(vm->val);                           NEXT1;}

CASE(IN_EQUAL)  { vm->val = STk_equal(pop(), vm->val);                     NEXT1;}
CASE(IN_EQV)    { vm->val = STk_eqv(pop(), vm->val);                       NEXT1;}
CASE(IN_EQ)     { vm->val = MAKE_BOOLEAN(pop() == vm->val);                NEXT1;}

CASE(IN_NOT_EQUAL) { vm->val = SCHEME_NOT(STk_equal(pop(), vm->val));      NEXT1; }
CASE(IN_NOT_EQV)   { vm->val = SCHEME_NOT(STk_eqv(pop(), vm->val));        NEXT1; }
CASE(IN_NOT_EQ)    { vm->val = MAKE_BOOLEAN(pop() != vm->val);             NEXT1; }

CASE(IN_ASSOC)    {
  SCM arg= pop();
  switch (fetch_next()) {
    case 1: vm->val= STk_assq(arg, vm->val); break;
    case 2: vm->val= STk_assv(arg, vm->val); break;
    case 3: vm->val= STk_assoc(arg, vm->val, NULL); break;
    default: {
      SCM prev = pop ();
      vm->val= STk_assoc(prev, arg, vm->val); break;
    }
  }
  NEXT1;
}

 CASE(IN_MEMBER)    {
  SCM arg= pop();
  switch (fetch_next()) {
    case 1: vm->val= STk_memq(arg, vm->val); break;
    case 2: vm->val= STk_memv(arg, vm->val); break;
    case 3: vm->val= STk_member(arg, vm->val, NULL); break;
    default: {
      SCM prev = pop ();
      vm->val= STk_member(prev, arg, vm->val); break;
    }
  }
  NEXT1;
}


CASE(IN_VREF) {
  REG_CALL_PRIM(vector_ref);
  vm->val = STk_vector_ref(pop(), vm->val);
  NEXT1;
}
CASE(IN_SREF) {
  REG_CALL_PRIM(string_ref);
  vm->val = STk_string_ref(pop(), vm->val);
  NEXT1;
}
CASE(IN_VSET)   {
  SCM index = pop();
  REG_CALL_PRIM(vector_set);
  STk_vector_set(pop(), index, vm->val);
  NEXT0;
}
CASE(IN_SSET)   {
  SCM index = pop();
  REG_CALL_PRIM(string_set);
  STk_string_set(pop(), index, vm->val);
  NEXT0;
}
CASE(IN_CXR) {
  vm->val= STk_cxr(vm->val, fetch_const());
  NEXT1;
 }

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
FUNCALL:  /* (int nargs, int tailp) */
{
  switch (STYPE(vm->val)) {

    case tc_instance: {
      if (PUREGENERICP(vm->val)) {
        SCM *argv = vm->sp+nargs-1;
        SCM methods, nm;

        /* methods is the list of applicable methods. Apply the first
         * one with the tail of the list as first parameter
         * (next-method). If methods is STk_nil, that's because the
         * no-applicable-method triggered didn't yield an error.
         */
        methods = STk_compute_applicable_methods(vm->val, nargs, argv, FALSE);
        if (NULLP(methods)) { vm->val = STk_void; return; }

        /* Place the procedure of the first method in the VAL register and
         * store the next method in the ``next-method'' variable.
         */
        nm       = STk_make_next_method(vm->val, nargs, argv, methods);
        vm->val  = INST_SLOT(CAR(methods), S_procedure);
        SET_NEXT_METHOD(vm->val, nm);
        /* FALLTHROUGH */
      } else {
        SCM gf, args;

        /* Use the MOP and do the call (apply-generic gf args) */
        args = listify_top(nargs, vm);
        push(vm->val);
        push(args);
        vm->val = STk_lookup(STk_intern("apply-generic"), vm->current_module,
                             &gf, FALSE);
        nargs = 2;
        goto FUNCALL;
      }
    }
    /* FALLTHROUGH */

    case tc_closure: {
      nargs = adjust_stack_get_arity(vm->val, nargs, vm);

      if (tailp) {
        /* Tail call: Reuse the old frame for this call.*/
        SCM *old_fp = (SCM *) ACT_SAVE_FP(vm->fp);

        /* Move the arguments of the function to the old_fp */
        if (nargs) memmove(old_fp-nargs, vm->sp, nargs*sizeof(SCM));
        vm->fp = old_fp;

        /* Push a new environment on the stack */
        vm->sp = vm->fp - nargs -
                 ((sizeof(struct frame_obj) - sizeof(SCM)) / sizeof(SCM));
        PUSH_ENV(nargs, vm->val, CLOSURE_ENV(vm->val));
      } else {
        /* Push a new environment on the stack */
        vm->sp -= (sizeof(struct frame_obj) - sizeof(SCM)) / sizeof(SCM);
        PUSH_ENV(nargs, vm->val, CLOSURE_ENV(vm->val));

        /* Finish initialisation of current activation record */
        ACT_SAVE_ENV(vm->fp)  = vm->env;
        ACT_SAVE_PC(vm->fp)   = vm->pc;
        ACT_SAVE_CST(vm->fp)  = vm->constants;
      }

      ACT_SAVE_PROC(vm->fp) = vm->val;

      /* Do the call */
      CALL_CLOSURE(vm->val);
      goto end_funcall;
    }

    case tc_next_method: {
      SCM methods, nm, *argv, proc;
      int i;

      methods = NXT_MTHD_METHODS(vm->val);

      if (nargs == 0) {
        /* no argument given, place the ones of the original call on top of stack */
        nargs = NXT_MTHD_ARGC(vm->val);
        argv  = NXT_MTHD_ARGV(vm->val);

        for (i = 0; i < nargs; i++)
          push(argv[i]);
      }

      argv  = vm->sp+nargs-1;

      if (NULLP(methods)) {
        /* Do the call (no-next-method gf method args) */
        argv = listify_top(nargs, vm);
        push(NXT_MTHD_GF(vm->val));
        push(NXT_MTHD_METHOD(vm->val));
        push(argv);
        nargs = 3;
        vm->val   = STk_lookup(STk_intern("no-next-method"), vm->current_module,
                               &proc, FALSE);
      } else {
        /* Call the next method after creating a new next-method */
        nm      = STk_make_next_method(vm->val, nargs, argv, methods);
        vm->val = INST_SLOT(CAR(methods), S_procedure);
        SET_NEXT_METHOD(vm->val, nm);
      }
      goto FUNCALL;
    }

    case tc_apply: {
      SCM l, func, *tmp, *argv;
      int len;

      if (nargs == 0) STk_error("no function given to apply");

      nargs -= 1;
      argv   = vm->sp + nargs;
      func   = *argv;

      if (nargs > 0) {
        /* look at last argument */
        l   = *vm->sp;
        len = STk_int_length(l);

        if (len < 0)
          STk_error("last argument of apply is not a list: ~S", l);
        else {
          /* move all the arguments, except the last one, one cell lower in the
           * stack (i.e. overwrite the function to call) */
          for (tmp = argv-1; tmp > vm->sp; tmp--)
            *(tmp+1) = *tmp;

          vm->sp = tmp + 2;
          if (len != 0) {
            /* Unfold the last argument in place */
            while (!NULLP(l)) {
              push(CAR(l));
              l = CDR(l);
            }
          }
          nargs += len-1;
        }
      }

      /* Now we can call "func" with "nargs" arguments */
      vm->val = func;
      goto FUNCALL;
    }

    case tc_subr0:
      if (nargs == 0) { CALL_PRIM0(vm->val, ());                          break;}
      goto error_invoke;
    case tc_subr1:
      if (nargs == 1) { CALL_PRIM1(vm->val, (vm->sp[0]));                 break;}
      goto error_invoke;
    case tc_subr2:
      if (nargs == 2) { CALL_PRIM2(vm->val, (vm->sp[1], vm->sp[0]));      break;}
      goto error_invoke;
    case tc_subr3:
      if (nargs == 3) { CALL_PRIM3(vm->val, (vm->sp[2], vm->sp[1],
                                             vm->sp[0]));                 break;}
      goto error_invoke;
    case tc_subr4:
      if (nargs==4) { CALL_PRIM4(vm->val, (vm->sp[3], vm->sp[2],
                                           vm->sp[1], vm->sp[0]));        break;}
      goto error_invoke;
    case tc_subr5:
      if (nargs==5) { CALL_PRIM5(vm->val, (vm->sp[4], vm->sp[3],
                                           vm->sp[2], vm->sp[1],
                                           vm->sp[0]));                   break;}
      goto error_invoke;

    case tc_subr01:
      if (nargs == 0) { CALL_PRIM1(vm->val, ((SCM) NULL));                break;}
      if (nargs == 1) { CALL_PRIM1(vm->val, (vm->sp[0]));                 break;}
      goto error_invoke;
    case tc_subr12:
      if (nargs == 1) { CALL_PRIM2(vm->val, (vm->sp[0], (SCM) NULL));     break;}
      if (nargs == 2) { CALL_PRIM2(vm->val, (vm->sp[1], vm->sp[0]));      break;}
      goto error_invoke;
    case tc_subr23:
      if (nargs == 2) { CALL_PRIM3(vm->val, (vm->sp[1], vm->sp[0],
                                             (SCM)NULL));                 break;}
      if (nargs == 3) { CALL_PRIM3(vm->val, (vm->sp[2], vm->sp[1],
                                             vm->sp[0]));                 break;}
       goto error_invoke;
    case tc_subr34:
      if (nargs == 3) { CALL_PRIM4(vm->val, (vm->sp[2], vm->sp[1],
                                             vm->sp[0], (SCM) NULL));     break;}
      if (nargs == 4) { CALL_PRIM4(vm->val, (vm->sp[3], vm->sp[2],
                                             vm->sp[1], vm->sp[0]));      break;}
      goto error_invoke;
    case tc_vsubr: CALL_PRIMV(vm->val, (nargs, vm->sp+nargs-1));          break;

    case tc_parameter:
      if (nargs == 0) {vm->val = STk_get_parameter(vm->val);            break;}
      if (nargs == 1) {vm->val = STk_set_parameter(vm->val, vm->sp[0]); break;}
      goto error_invoke;

#ifdef HAVE_FFI
    case tc_ext_func:
      ACT_SAVE_PROC(vm->fp) = vm->val;
      vm->val = STk_call_ext_function(vm->val, nargs, vm->sp+nargs-1);  break;
#endif

    default:
      STk_error("bad function ~S. Cannot be applied", vm->val);
    error_invoke:
      ACT_SAVE_PROC(vm->fp) = vm->val;
      /* We are here when we had a primitive call with a bad number of parameters */
      STk_error("incorrect number of parameters (%d) in call to ~S", nargs, vm->val);

  }
  /* We are here when we have called a primitive */
  RETURN_FROM_PRIMITIVE();
end_funcall:
  NEXT;
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
#ifndef USE_COMPUTED_GOTO
      default:
        STk_panic("INSTRUCTION %d NOT IMPLEMENTED\n", byteop);
    }
  } /* for( ; ; ) */
#endif
  STk_panic("abnormal exit from the VM");
}

void STk_raise_exception(SCM cond)
{
  SCM proc, *save_vm_state;
  vm_thread_t *vm = STk_get_current_vm();

  save_vm_state = (vm->handlers) + EXCEPTION_HANDLER_SIZE;

  if (vm->handlers == NULL) {
    STk_print(cond, STk_stderr, DSP_MODE);
    STk_fprintf(STk_stderr, ": ");
    STk_print(STk_int_struct_ref(cond, STk_intern("message")),
              STk_stderr,
              DSP_MODE);
    STk_fprintf(STk_stderr, "\n**** FATAL ERROR: no handler present!\nABORT\n");
    exit(1);
  }

  /*
   * Grab the handler info
   */
  proc   = (SCM)         HANDLER_PROC(vm->handlers);
  vm->pc = (STk_instr *) HANDLER_END(vm->handlers);

  UNSAVE_HANDLER_STATE();

  RESTORE_VM_STATE(save_vm_state);

  /* Execute the procedure handler on behalf of the old handler (since the
   * procedure can be itself erroneous).
   */
  vm->val = STk_C_apply(proc, 1, cond);

  /*
   * Return to the good "run_vm" incarnation
   */
  MY_LONGJMP(*(vm->top_jmp_buf), 1);
}

/*
<doc EXT current-exception-handler
 * (current-exception-handler)
 *
 * Returns the current exception handler. This procedure is defined in
 * ,(link-srfi 18).
doc>
*/
DEFINE_PRIMITIVE("current-exception-handler", current_handler, subr0, (void))
{
  vm_thread_t *vm = STk_get_current_vm();

  if (vm->handlers == NULL)
    return STk_false;
  else
    return (SCM) HANDLER_PROC(vm->handlers);
}

/*
DEFINE_PRIMITIVE("%pop-exception-handler", pop_handler, subr0, (void))
{
  vm_thread_t *vm = STk_get_current_vm();

  UNSAVE_HANDLER_STATE();
  RESTORE_VM_STATE(vm->sp);
  return STk_void;
}

*/

/*===========================================================================*\
 *
 *                         C O N T I N U A T I O N S
 *
\*===========================================================================*/

#ifndef __clang__
  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wdangling-pointer"
#endif

void STk_get_stack_pointer(void **addr)
{
  char c;
  *addr = (void *) &c;
}

DEFINE_PRIMITIVE("%make-continuation", make_continuation, subr0, (void))
{
  SCM z;
  struct continuation_obj *k;
  vm_thread_t *vm = STk_get_current_vm();
  unsigned int csize, ssize;
  void *cstart, *sstart, *cend, *send;
  void *addr, *start_stack;

  /* Determine the size of the C stack and the start address */
  STk_get_stack_pointer(&addr);
  start_stack = vm->start_stack;

  if ((unsigned long) addr < (unsigned long) start_stack) {
    csize  = (unsigned long) start_stack - (unsigned long) addr;
    cstart = addr;
    cend   = start_stack;
  } else {
    csize  = (unsigned long) addr - (unsigned long) start_stack;
    cstart = start_stack;
    cend   = addr;
  }

  /* Determine the size of the Scheme stack */
  sstart = vm->sp;
  send   = vm->stack + vm->stack_len;
  ssize  = (unsigned long) send - (unsigned long) sstart;

  /* Allocate a continuation object */
  NEWCELL_WITH_LEN(z, continuation, sizeof(struct continuation_obj) + ssize + csize);
  k = (struct continuation_obj *) z;

  k->csize      = csize;
  k->cstart     = cstart;
  k->cend       = cend;

  k->ssize      = ssize;
  k->sstart     = sstart;
  k->send       = send;

  patch_environment(vm);

  k->pc          = vm->pc;
  k->fp          = vm->fp;
  k->sp          = vm->sp;
  k->env         = vm->env = clone_env(vm->env, vm);
  k->constants   = vm->constants;
  k->handlers    = vm->handlers;
  k->jb          = vm->top_jmp_buf;

  /* Save the Scheme stack */
  k->sstack = STk_must_malloc(ssize);
  memcpy(k->sstack, k->sp, ssize);

  /* Save the C stack */
  k->cstack = STk_must_malloc(csize);
  memcpy(k->cstack, cstart, csize);

  k->fresh = 1;

  if (MY_SETJMP(k->state) == 0) {
    /* This is the initial call to %make_continuation */
    return z;
  } else {
    /* We come back and restore the continuation */
    /* Since we are not sure of the way locals are allocated by the compiler
     * we cannot be sure that the vm has kept its value. So we get the current
     * vm data again. */
    return STk_get_current_vm()->val;
  }
}

#define CALL_CC_SPACE   1024    /* Add some space for restoration bookkeeping */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Winfinite-recursion"

static void restore_cont_jump(struct continuation_obj *k, void *addr){
  char unused_buf[1024];  /* needed here to arbitrarily use some stack space */
  vm_thread_t *vm = STk_get_current_vm();
  int cur_stack_size;

  cur_stack_size = vm->start_stack - addr;

  if (cur_stack_size < 0) cur_stack_size = -cur_stack_size;
  if (cur_stack_size <= (k->csize + CALL_CC_SPACE)) {
    /* Not enough space, recurse */
    STk_get_stack_pointer(&addr);
    restore_cont_jump(k, &addr);
  } else {
    memcpy(k->cstart, k->cstack, k->csize);

    /* Return */
    MY_LONGJMP(k->state, 1);
  }
}
#pragma GCC diagnostic pop


DEFINE_PRIMITIVE("%restore-continuation", restore_cont, subr2, (SCM cont, SCM value))
{
  struct continuation_obj *k;
  void *addr;
  vm_thread_t *vm = STk_get_current_vm();

  if (!CONTP(cont)) STk_error("bad continuation ~S", cont);

  k = (struct continuation_obj *) cont;

  vm->val               = value;

  vm->pc                = k->pc;
  vm->fp                = k->fp;
  vm->sp                = k->sp;
  vm->env               = k->env;
  vm->constants         = k->constants;
  vm->handlers          = k->handlers;
  vm->top_jmp_buf       = k->jb;

  k->fresh = 0;
  /* Restore the Scheme stack */
  memcpy(k->sp, k->sstack, k->ssize);

  /* Restore the C stack */
  STk_get_stack_pointer(&addr);
  restore_cont_jump(k, addr);

  /* never reached */
  return STk_void;
}

#ifndef __clang__
#  pragma GCC diagnostic pop
#endif

DEFINE_PRIMITIVE("%continuation?", continuationp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(CONTP(obj));
}

DEFINE_PRIMITIVE("%fresh-continuation?", fresh_continuationp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(CONTP(obj) && (((struct continuation_obj *) obj)->fresh));
}


static void print_continuation(SCM cont, SCM port, int _UNUSED(mode))
{
  STk_fprintf(port, "#[continuation (C=%d S=%d) %x]",
              ((struct continuation_obj *)cont)->csize,
              ((struct continuation_obj *)cont)->ssize,
              (unsigned long) cont);
}

static struct extended_type_descr xtype_continuation = {
  .name  = "continuation",
  .print = print_continuation
};


/*===========================================================================*\
 *
 *                         Bytecode file dump/load stuff
 *
\*===========================================================================*/

static int system_has_booted = 0;



/* This function is used to dump the code in a file */
DEFINE_PRIMITIVE("%dump-code", dump_code, subr2, (SCM f, SCM v))
{
  int size, i;
  SCM *tmp;
  STk_instr instr;

  if (!FPORTP(f))  STk_error("bad file port ~S", f);
  if (!VECTORP(v)) STk_error("bad code vector ~S", v);

  size = VECTOR_SIZE(v); tmp = VECTOR_DATA(v);

  /* Print size as a Scheme value */
  STk_print(MAKE_INT(size), f, DSP_MODE);
  STk_putc('\n', f);

  /* Print the content of the vector as bytes */
  for (i = 0; i < size; i++) {
    if (!INTP(*tmp))
      STk_error("bad value (~S) at index %d in code vector ~S", *tmp, i, v);

    instr = (STk_instr) INT_VAL(*tmp++);
    STk_putc(FIRST_BYTE(instr), f);
    STk_putc(SECOND_BYTE(instr), f);

  }
  STk_putc('\n', f);
  return STk_void;
}


static inline STk_instr* read_code(SCM f, unsigned int len) /* read a code phrase */
{
  STk_instr *res, *tmp;
  unsigned int i;
  int c1, c2;

  tmp = res = STk_must_malloc_atomic(len * sizeof(STk_instr));

  /* skip the separator */
  STk_getc(f);

  /* Read 'len' instruction (coded on 2 bytes) */
  for (i = 0; i < len; i++) {
    c1 = STk_getc(f);
    c2 = STk_getc(f);
    if (c2 == EOF) /* not useful to test c1 */
      STk_error("truncated bytecode file ~S", f);

    *tmp++ = (STk_instr) (c1 << 8 | c2);
  }

  return res;
}

SCM STk_load_bcode_file(SCM f)
{
  SCM consts, code_size, *save_constants, save_env;
  STk_instr *save_pc;
  long size;
  vm_thread_t *vm = STk_get_current_vm();

  /* Save machine state */
  save_pc = vm->pc; save_constants = vm->constants; save_env = vm->env;

  /* Signature has been skipped during file type analysis (but not information) */
  STk_read(f, TRUE); /* skip info */

  for ( ; ; ) {
    consts = STk_read_constant(f, TRUE);                   /* Read  the constants */
    if (consts == STk_eof) break;

    code_size = STk_read(f, PORT_CASE_SENSITIVEP(f));      /* Read the code size */
    size      = STk_integer_value(code_size);
    if (size < 0) {
      if (system_has_booted)
        STk_error("Bad bytecode file ~S", f);
      else
        return STk_false;
    }

    vm->pc        = read_code(f, size);                      /* Read the code */
    vm->constants = VECTOR_DATA(consts);
    vm->env       = vm->current_module;
    run_vm(vm);
  }

  /* restore machine state */
  vm->pc = save_pc; vm->constants = save_constants, vm->env = save_env;
  return STk_true;
}


int STk_load_boot(char *filename)
{
  SCM f, tmp;

  f = STk_open_file(filename, "rb");
  if (f == STk_false) return -1;

  /* Verify that the file is a bytecode file */
  tmp = STk_read(f, TRUE);
  if (tmp != STk_intern("STklos")) return -2;

  tmp = STk_load_bcode_file(f);
  if (tmp == STk_false) return -3;

  /* The system has booted on the given file */
  system_has_booted = 1;
  return 0;
}

int STk_boot_from_C(void)
{
  SCM port, consts;
  vm_thread_t *vm = STk_get_current_vm();

  /* Get the constants */
  port = STk_open_C_string(STk_boot_consts);
  consts = STk_read(port, TRUE);

  /* Run the VM */
  vm->pc        = STk_boot_code;
  vm->constants = VECTOR_DATA(consts);
  vm->env       = vm->current_module;
  run_vm(vm);

  system_has_booted = 1;
  return 0;
}


SCM STk_execute_C_bytecode(SCM all_consts, STk_instr *instr)
{
  SCM consts, *save_constants, save_env;
  STk_instr *save_pc;
  vm_thread_t *vm = STk_get_current_vm();

  consts = STk_read(STk_open_C_string(all_consts), TRUE);
  /* Save machine state */
  save_pc = vm->pc; save_constants = vm->constants; save_env = vm->env;

  /* Go */
  vm->pc = instr;
  vm->constants = VECTOR_DATA(consts);
  vm->env       = vm->current_module;
  run_vm(vm);

  /* restore machine state */
  vm->pc = save_pc; vm->constants = save_constants, vm->env = save_env;
  return STk_void;
}


int STk_init_vm()
{
  DEFINE_XTYPE(continuation, &xtype_continuation);

  /* Initialize the global_store array */
  STk_global_store = STk_must_malloc(GLOBAL_STORE_INIT_SIZE * sizeof(SCM));
  return TRUE;
}

int STk_late_init_vm()
{
  /* Add the apply primitive */
  ADD_PRIMITIVE(scheme_apply);
  ADD_PRIMITIVE(execute);
  ADD_PRIMITIVE(dump_code);
  ADD_PRIMITIVE(vm_bt);

  ADD_PRIMITIVE(values);
  ADD_PRIMITIVE(call_for_values);

  ADD_PRIMITIVE(current_handler);
  /* ADD_PRIMITIVE(pop_handler); */

  ADD_PRIMITIVE(make_continuation);
  ADD_PRIMITIVE(restore_cont);
  ADD_PRIMITIVE(continuationp);
  ADD_PRIMITIVE(fresh_continuationp);

#ifdef STK_DEBUG
  ADD_PRIMITIVE(set_vm_debug);
#endif
  return TRUE;
}
