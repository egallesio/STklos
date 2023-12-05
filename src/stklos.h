/*
 * stklos.h     -- stklos.h
 *
 * Copyright © 1999-2023 Erick Gallesio <eg@stklos.net>
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
 *    Creation date: 28-Dec-1999 22:58 (eg)
 */


#ifndef STKLOS_H
#define STKLOS_H

#ifdef __cplusplus
extern "C"
{
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <limits.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
#include <setjmp.h>
#include <memory.h>
#include <locale.h>
#include <stdint.h>
#ifndef THEADS_NONE
#  include <pthread.h>
#  define GC_THREADS 1
#  define _REENTRANT 1
#endif

#include "stklosconf.h"
#include "extraconf.h"

/* To debug the GC uncomment the following line */
/* #define GC_DEBUG 1 */

#ifdef HAVE_GC
# include <gc/gc.h>
#else
# include <gc.h>
#endif


/*===========================================================================*\
 *
 *              Declaration of some constants (mainly maxima)
 *
\*===========================================================================*/

#ifndef FALSE
#  define FALSE 0
#endif

#ifndef TRUE
#  define TRUE (!FALSE)
#endif

#ifdef PATH_MAX
#  define MAX_PATH_LENGTH     PATH_MAX
#else
#  define MAX_PATH_LENGTH        4096
#endif


#define CPP_CONCAT(x, y)        x##y


#define AS_LONG(x)              ((unsigned long) (x))
#define AS_SCM(x)               ((SCM) ((unsigned long) (x)))

/* UNTAG removes the tag bits. It is useful in optimized fixnum
   operations (see fixnum.c). --jpellegrini */
#define UNTAG(x)                (((long) x) & (~ ((unsigned long) 3)))

/*===========================================================================*\
 *
 *                              Threads stuff
 *
\*===========================================================================*/
#ifdef THREADS_NONE
#  define MUT_DECL(lck)
#  define MUT_FIELD(lck)
#  define MUT_INIT(lck)
#  define MUT_LOCK(lck)
#  define MUT_UNLOCK(lck)
#else
#  define MUT_DECL(lck)    static pthread_mutex_t lck = PTHREAD_MUTEX_INITIALIZER
#  define MUT_FIELD(lck)   pthread_mutex_t lck
#  define MUT_INIT(lck)    pthread_mutex_init(&lck, NULL)
#  define MUT_LOCK(lck)    pthread_mutex_lock(&lck)
#  define MUT_UNLOCK(lck)  pthread_mutex_unlock(&lck)
#endif

/*===========================================================================*\
 *
 *                      Memory allocation
 *
\*===========================================================================*/
  /*
   * This code is an excerpt of the function used in Boehm GC (i.e. all the
   * functions of the GC used in the interpreter must be declared here since
   * the file <gc.h> is not included in the source file in order to simplify
   * header file management (i.e. only this header file is necessary to use
   * the stklos library)).
   * Don't use the functions GC_*, as they can be changed. The only allocation
   * functions that must be used are functions of the form STk_*
   */

  /* GC interface. *** DON'T USE IT DIRECTLY *** */
//
// #define GC_API extern
//
// typedef void (*GC_finalization_proc) (void * obj, void * client_data);
//
// GC_API void * GC_malloc(size_t size_in_bytes);
// GC_API void * GC_malloc_atomic(size_t size_in_bytes);
// GC_API void * GC_realloc(void * old_object, size_t new_size_in_bytes);
// GC_API void GC_free(void * object_addr);
//
// GC_API void GC_register_finalizer(void * obj, GC_finalization_proc fn,
//                                void * cd, GC_finalization_proc *ofn,
//                                void * *ocd);
//
// GC_API void GC_gcollect(void);
// GC_API void GC_init(void);

  /* Scheme interface. *** THIS IS THE INTERFACE TO USE ***  */


#define STk_must_malloc(size)           GC_MALLOC(size)
#define STk_must_malloc_atomic(size)    GC_MALLOC_ATOMIC(size)
#define STk_must_realloc(ptr, size)     GC_REALLOC((ptr), (size))
#define STk_free(ptr)                   GC_FREE(ptr)
#define STk_register_finalizer(ptr, f)  GC_REGISTER_FINALIZER( \
                                            (void *) (ptr),             \
                                            (GC_finalization_proc)(f),  \
                                            0, 0, 0)
#define STk_gc()                        GC_gcollect()

void STk_gc_init(void);



/*===========================================================================*\
 *
 *              Declaration of the SCM type
 *
\*===========================================================================*/

#define MAX_CELL_TYPES          256

typedef void* SCM;

typedef enum {
  tc_not_boxed=-1,
  tc_cons, tc_integer, tc_real, tc_bignum,  tc_rational,                /* 0 */
  tc_complex, tc_symbol, tc_keyword, tc_string, tc_module,              /* 5 */
  tc_instance, tc_closure, tc_subr0, tc_subr1, tc_subr2,                /* 10 */
  tc_subr3, tc_subr4, tc_subr5, tc_subr01, tc_subr12,                   /* 15 */
  tc_subr23, tc_subr34, tc_vsubr, tc_apply, tc_vector,                  /* 20 */
  tc_uvector, tc_hash_table, tc_port, tc_frame, tc_next_method,         /* 25 */
  tc_promise, tc_regexp, tc_process, tc_continuation, tc_values,        /* 30 */
  tc_parameter, tc_socket, tc_struct_type, tc_struct, tc_thread,        /* 35 */
  tc_mutex, tc_condv, tc_box, tc_ext_func, tc_pointer,                  /* 40 */
  tc_callback, tc_syntax,                                               /* 45 */
  tc_last_standard /* must be last as indicated by its name */
} type_cell;


  /*
   * Internal representation of SCM objects. Objects use the two least
   * significant bits as tag. We have the following representation
   *
   *     .........00            pointer on an object descriptor (a box)
   *     .........01            integer
   *     .........10            small object (see below for more detail)
   *     .........11            small constant (#t #f '() ... see below for details)
   */

#define MAKE_SCONST(n)   (AS_SCM(n << 2 | 3))
#define SCONSTP(n)       ((AS_LONG(n) & 0x3) == 3)

  /*
   * Header which must always be put in front of the various boxed types
   * used by STklos. This field must be declared as the first field of
   * the structure.
   */

typedef struct {
  /* Order is important, changing it can improve the perfomances depending
   * on the compiler. If you change this definition, change DEFINE_PRIMITIVE
   * accordingly
   */
  int16_t type, cell_info;
} stk_header;


#define BOXED_TYPE(x)           (((stk_header *) x)->type)
#define BOXED_INFO(x)           (((stk_header *) x)->cell_info)
#define BOXED_OBJP(x)           (!(AS_LONG(x) & 3))
#define BOXED_TYPE_EQ(x, y)     (BOXED_OBJP(x) && BOXED_TYPE(x) == y)
#define STYPE(x)                (BOXED_OBJP(x)? BOXED_TYPE(x): tc_not_boxed)


#define NEWCELL(_var, _type)    do{                                             \
        _var = (SCM) STk_must_malloc(sizeof(struct CPP_CONCAT(_type,_obj)));    \
        BOXED_TYPE(_var) = CPP_CONCAT(tc_, _type);                              \
        BOXED_INFO(_var) = 0;                                                   \
        }while(0)

#define NEWCELL_WITH_LEN(_var, _type, _len)     do{     \
        _var = (SCM) STk_must_malloc(_len);             \
        BOXED_TYPE(_var) = CPP_CONCAT(tc_, _type);      \
        BOXED_INFO(_var) = 0;                           \
        }while(0)

#define NEWCELL_ATOMIC(_var, _type, _len)       do{     \
        _var = (SCM) STk_must_malloc_atomic(_len);      \
        BOXED_TYPE(_var) = CPP_CONCAT(tc_, _type);      \
        BOXED_INFO(_var) = 0;                           \
        }while(0)

  /*
   * PRIMITIVES
   *
   * Primitives are defined with the macro DEFINE_PRIMITIVE. An example of
   * usage of this  macro is given below:
   *    DEFINE_PRIMITIVE("pair?", pairp, subr1, (SCM obj)) {
   *       <body>
   *    }
   * It will be expansed in
   *    SCM STk_pairp(SCM obj);
   *    static struct obj_primitive obj_pairp = { "pair?", tc_subr1, STk_pairp};
   *    SCM STk_pairp(SCM obj){
   *      <body>
   *    }
   */

struct primitive_obj {
  stk_header header;
  char *name;
  SCM (*code)();
  SCM plist;
};

#define PRIMITIVE_NAME(p)       (((struct primitive_obj *) (p))->name)
#define PRIMITIVE_FUNC(p)       (((struct primitive_obj *) (p))->code)
#define PRIMITIVE_PLIST(p)      (((struct primitive_obj *) (p))->plist)

#define DEFINE_PRIMITIVE(_sname, _cname, _type, _params)        \
  SCM CPP_CONCAT(STk_, _cname) _params;                         \
  struct primitive_obj CPP_CONCAT(STk_o_, _cname) = {           \
        {CPP_CONCAT(tc_, _type), 0},                            \
        _sname, CPP_CONCAT(STk_, _cname), STk_nil};             \
  SCM CPP_CONCAT(STk_, _cname) _params

#define EXTERN_PRIMITIVE(_sname, _cname, _type, _params)        \
  /* the same one as before but without function definition */  \
  extern SCM CPP_CONCAT(STk_, _cname) _params;                  \
  extern struct primitive_obj CPP_CONCAT(STk_o_, _cname)


#define ENTER_PRIMITIVE(x)     /* here for compability with pre 0.62 version */
#define THE_PRIMITIVE(_name)   ((SCM) CPP_CONCAT(&STk_o_, _name))
#define ADD_PRIMITIVE(_name)   STk_add_primitive(CPP_CONCAT(&STk_o_, _name))
#define ADD_PRIMITIVE_IN_MODULE(_name, _mod) \
                 STk_add_primitive_in_module(CPP_CONCAT(&STk_o_, _name), _mod)

/*
  ------------------------------------------------------------------------------
  ----
  ----                           B A S E 6 4 . C
  ----
  ------------------------------------------------------------------------------
*/
int STk_init_base64(void);



/*
  ------------------------------------------------------------------------------
  ----
  ----                           B O O L E A N . C
  ----
  ------------------------------------------------------------------------------
*/

#define MAKE_BOOLEAN(_cond)     ((_cond) ? STk_true : STk_false)
#define BOOLEANP(o)             (((o) == STk_true) || ((o) == STk_false))


EXTERN_PRIMITIVE("eq?",    eq,    subr2, (SCM x,SCM y));
EXTERN_PRIMITIVE("eqv?",   eqv,   subr2, (SCM x,SCM y));
EXTERN_PRIMITIVE("equal?", equal, subr2, (SCM x,SCM y));


int STk_init_boolean(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                           B O X . C
  ----
  ------------------------------------------------------------------------------
*/
struct box_obj {
  stk_header header;
  int arity;
  SCM values[1];
};

#define BOXP(p)         (BOXED_TYPE_EQ((p), tc_box))
#define BOX_ARITY(p)    (((struct box_obj *) (p))->arity)
#define BOX_VALUES(p)   (((struct box_obj *) (p))->values)
#define BOX_CONST       (1 << 0)

EXTERN_PRIMITIVE("box", box, vsubr, (int argc, SCM * argv));
SCM STk_make_box(SCM obj);


int STk_init_box(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                            C H A R  . C
  ----
  ------------------------------------------------------------------------------
*/

  /*
   * characters are coded as .....XXXXX110 where XXXXX is the code of the
   * character. Consequently, we can have 29 bits long characters (on a 32 bits
   * machine)
   */

#define MAKE_CHARACTER(n) (AS_SCM((n) << 3 | 0x6))
#define CHARACTER_VAL(n)  ((AS_LONG(n) >> 3))
#define CHARACTERP(n)     ((AS_LONG(n) & 7) == 6)


/* Simple  character conversion functions */
uint32_t STk_to_upper(uint32_t c);
uint32_t STk_to_lower(uint32_t c);
uint32_t STk_to_fold(uint32_t c);


char *STk_char2string(int c);
int STk_string2char(char *s);
int STk_init_char(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                           C O N D  . C
  ----
  ------------------------------------------------------------------------------
*/

extern SCM STk_message_condition, STk_err_mess_condition, STk_exit_condition;

SCM STk_make_C_cond(SCM type, int nargs, ...);

EXTERN_PRIMITIVE("make-condition-type", make_cond_type, subr3,
                 (SCM name, SCM parent, SCM slots));
EXTERN_PRIMITIVE("raise", raise, subr1, (SCM obj));

SCM STk_defcond_type(char *name, SCM parent, SCM slots, SCM module);
SCM STk_condition_type_is_a(SCM type, SCM t);
int STk_init_cond(void);

/*
  ------------------------------------------------------------------------------
  ----
  ----                           C P O I N T E R  . C
  ----
  ------------------------------------------------------------------------------
*/
struct pointer_obj {
  stk_header header;
  void *value;
  SCM type;
  SCM  data;
};

#define CPOINTERP(p)            (BOXED_TYPE_EQ((p), tc_pointer))
#define CPOINTER_VALUE(p)       (((struct pointer_obj *) (p))->value)
#define CPOINTER_TYPE(p)        (((struct pointer_obj *) (p))->type)
#define CPOINTER_DATA(p)        (((struct pointer_obj *) (p))->data)

SCM STk_make_Cpointer(void *ptr, SCM type, SCM data);
int STk_init_cpointer(void);

/*
  ------------------------------------------------------------------------------
  ----
  ----                            D Y N L O A D . C
  ----
  ------------------------------------------------------------------------------
*/

#define MODULE_ENTRY_START(_name)                               \
    void STk_module_main(void)  {                               \
      static int __already_loaded = 0;                          \
      if (__already_loaded++) {                                 \
        STk_error("module %S already loaded", (_name));         \
        return;                                                 \
      } {

#define MODULE_ENTRY_END        } }

#define MODULE_ENTRY_INFO() SCM STk_module_info(void)  /* old form */

#define DEFINE_MODULE_INFO                                /* new form */ \
  SCM STk_module_info(void) { return STk_read_from_C_string(__module_infos); }



void *STk_find_external_function(char *path, char *fname, int error_if_absent);
SCM STk_load_object_file(SCM f, char *fname);
SCM STk_info_object_file(char *fname);

/*
  ------------------------------------------------------------------------------
  ----
  ----                            E R R O R . C
  ----
  ------------------------------------------------------------------------------
*/

void STk_signal_error(SCM type, SCM who, SCM str, SCM msg, SCM irritants);
void STk_error(char *format, ...);
void STk_error_with_location(SCM loc, char *format, ...);
SCM  STk_make_error(char *format, ...);
SCM  STk_format_error(char *format, ...);
void STk_warning(char *format, ...);
void STk_panic(char *format, ...);
void STk_signal(char *str);

#ifdef STK_DEBUG
   void STk_debug(char *format, ...);
#  define Debug  STk_debug

#endif

/*
  ------------------------------------------------------------------------------
  ----
  ----                            E N V . C
  ----
  ------------------------------------------------------------------------------
*/

struct frame_obj {
  stk_header header;
  SCM next_frame;
  SCM owner;
  SCM locals[1];        /* the values associated to the names */
};

#define FRAME_LENGTH(p)         (BOXED_INFO(p))
#define FRAME_NEXT(p)           (((struct frame_obj *) (p))->next_frame)
#define FRAME_OWNER(p)          (((struct frame_obj *) (p))->owner)
#define FRAME_LOCALS(p)         (((struct frame_obj *) (p))->locals)
#define FRAME_LOCAL(p, i)       (FRAME_LOCALS(p)[i])
#define FRAMEP(p)               (BOXED_TYPE_EQ((p), tc_frame))

#define MODULE_CONST            (1 << 0)

/* modules are defined in env.c but are private */
#define MODULEP(p)              (BOXED_TYPE_EQ((p), tc_module))

SCM STk_make_frame(int len);
SCM STk_clone_frame(SCM f);

SCM STk_lookup(SCM symbol, SCM env, SCM *ref, int err_if_unbound);
void STk_error_unbound_variable(SCM symbol, SCM module);
void STk_define_variable(SCM symbol, SCM value, SCM module);
SCM STk_symb_in_scheme(SCM symb); // value of symb in module SCHEME

int STk_init_env(void);
int STk_late_init_env(void); /* must be done after symbol initialization */

extern SCM STk_STklos_module;

EXTERN_PRIMITIVE("%create-module", create_module, subr1, (SCM name));
EXTERN_PRIMITIVE("current-module", current_module, subr0, (void));
EXTERN_PRIMITIVE("%select-module", select_module, subr1, (SCM module));

void STk_export_all_symbols(SCM module);

/*
  ------------------------------------------------------------------------------
  ----
  ----                          E X T E N D . C
  ----
  ------------------------------------------------------------------------------
*/
  /* The `extended_type_descr' structure is used for the types which need
   * more information (such as modules, ports, ...). All the extended
   * descriptors are stored in the STk_xtypes array.
   */
struct extended_type_descr {
  char *name;
  void (*print)(SCM exp, SCM port, int mode);
  SCM  (*equal)(SCM o1, SCM o2);
  SCM  (*eqv)(SCM o1, SCM o2);
  SCM  class_of;
  SCM  describe_proc;
};

extern struct extended_type_descr *STk_xtypes[];

#define HAS_USER_TYPEP(o)      (BOXED_OBJP(o) && (BOXED_TYPE(o) > tc_last_standard))
#define BOXED_XTYPE(o)                   (STk_xtypes[((stk_header *) o)->type])
#define XTYPE_NAME(d)                    ((d)->name)
#define XTYPE_PRINT(d)                   ((d)->print)
#define XTYPE_EQUAL(d)                   ((d)->equal)
#define XTYPE_EQV(d)                     ((d)->eqv)
#define XTYPE_CLASS_OF(d)                ((d)->class_of)
#define XTYPE_DESCRIBE(d)                ((d)->describe_proc)
#define DEFINE_XTYPE(_type, _descr)      (STk_xtypes[CPP_CONCAT(tc_, _type)]=_descr)
#define DEFINE_USER_TYPE(_type, _descr)  { _type = STk_new_user_type(_descr); }

int STk_new_user_type(struct extended_type_descr *);
SCM STk_extended_eqv(SCM o1, SCM o2);
SCM STk_extended_equal(SCM o1, SCM o2);
SCM STk_extended_class_of(SCM o);
int STk_init_extend(void);

/*
  ------------------------------------------------------------------------------
  ----
  ----                          F F I . C
  ----
  ------------------------------------------------------------------------------
*/
SCM STk_call_ext_function(SCM fct, int argc, SCM *argv);
SCM STk_ext_func_name(SCM fct);
int STk_init_ffi(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                          F I X N U M . C
  ----
  ------------------------------------------------------------------------------
*/
long STk_fixval(SCM v);
long STk_fixnum_cmp(SCM a, SCM b);
unsigned int STk_bit_count(unsigned long n);
EXTERN_PRIMITIVE("fx+",        fxplus,  subr2, (SCM o1, SCM o2));
EXTERN_PRIMITIVE("fx-",        fxminus, subr2, (SCM o1, SCM o2));
EXTERN_PRIMITIVE("fx*",        fxtime,  subr2, (SCM o1, SCM o2));
EXTERN_PRIMITIVE("fxquotient", fxdiv,   subr2, (SCM o1, SCM o2));
int STk_init_fixnum(void);

/* TAG_FIXNUM forces a fixnum tag on x. */
#define TAG_FIXNUM(x)      ((UNTAG(x)) | 1)

/*
  ------------------------------------------------------------------------------
  ----
  ----                           K E Y W O R D . C
  ----
  ------------------------------------------------------------------------------
*/
struct keyword_obj {
  stk_header header;
  const char *pname;            /* must be at the same offset as for symbols */
};

#define KEYWORD_PNAME(p)        (((struct keyword_obj *) (p))->pname)
#define KEYWORDP(p)             (BOXED_TYPE_EQ((p),tc_keyword))

#define KEYWORD_NEEDS_BARS      (1 << 0)        /* Info flag */
#define KEYWORD_HAS_UPPER       (1 << 1)

EXTERN_PRIMITIVE("key-set!", key_set, subr3, (SCM l, SCM key, SCM val));
EXTERN_PRIMITIVE("key-get", key_get, subr23, (SCM l, SCM key, SCM dflt));

SCM STk_makekey(const char *token);
int STk_init_keyword(void);

/*
  ------------------------------------------------------------------------------
  ----
  ----                                  L I B . C
  ----
  ------------------------------------------------------------------------------
*/
extern int   STk_library_initialized; /* True when successfully initialized */

int STk_init_library(int *argc, char ***argv, int stack_size);

/*
  ------------------------------------------------------------------------------
  ----
  ----                                L I S T . C
  ----
  ------------------------------------------------------------------------------
*/

struct cons_obj {
  stk_header header;
  SCM car;
  SCM cdr;
};

#define CAR(p)          (((struct cons_obj *) (p))->car)
#define CDR(p)          (((struct cons_obj *) (p))->cdr)
#define CONSP(p)        (BOXED_TYPE_EQ((p), tc_cons))
#define NULLP(p)        ((p) == STk_nil)

#define CONS_CONST              (1 << 0)
#define CONS_PLACEHOLDER        (1 << 1)        /* used for #n= and #n# notation */
#define CONS_ECONS              (1 << 2)        /* used for extended conses      */
#define CONS_ALIAS              (1 << 3)        /* used for implementing aliases */

#define LIST1(a)                 STk_cons((a), STk_nil)
#define LIST2(a,b)               STk_cons((a), LIST1(b))
#define LIST3(a,b,c)             STk_cons((a), LIST2((b),(c)))
#define LIST4(a,b,c,d)           STk_cons((a), LIST3((b),(c),(d)))
#define LIST5(a,b,c,d,e)         STk_cons((a), LIST4((b),(c),(d),(e)))
#define LIST6(a,b,c,d,e,f)       STk_cons((a), LIST5((b),(c),(d),(e),(f)))
#define LIST7(a,b,c,d,e,f,g)     STk_cons((a), LIST6((b),(c),(d),(e),(f),(g)))
#define LIST8(a,b,c,d,e,f,g,h)   STk_cons((a), LIST7((b),(c),(d),(e),(f),(g),(h)))
#define LIST9(a,b,c,d,e,f,g,h,i) \
  STk_cons((a), LIST8((b),(c),(d),(e),(f),(g),(h),(i)))
#define LIST10(a,b,c,d,e,f,g,h,i,j) \
  STk_cons((a), LIST9((b),(c),(d),(e),(f),(g),(h),(i),(j)))


int STk_int_length(SCM l);              /* len of a list -1 if badly formed */
SCM STk_int_assq(SCM obj, SCM alist);   /* internal version of assq */
SCM STk_argv2list(int argc, SCM *argv);
SCM STk_append2(SCM l1, SCM l2);
SCM STk_dappend2(SCM l1, SCM l2);       /* destructive append */
SCM STk_dremq(SCM obj, SCM list);       /* destructive remove with eq? */
SCM STk_econs(SCM car, SCM cdr, char *file, int line, int pos);

EXTERN_PRIMITIVE("cons", cons, subr2, (SCM x, SCM y));
EXTERN_PRIMITIVE("car", car, subr1, (SCM x));
EXTERN_PRIMITIVE("cdr", cdr, subr1, (SCM x));
EXTERN_PRIMITIVE("%cxr", cxr, subr2, (SCM l, SCM name));
EXTERN_PRIMITIVE("list", list, vsubr, (int argc, SCM * argv));
EXTERN_PRIMITIVE("memq", memq, subr2, (SCM obj, SCM list));
EXTERN_PRIMITIVE("reverse", reverse, subr1, (SCM l));
EXTERN_PRIMITIVE("reverse!", dreverse, subr1, (SCM l));
EXTERN_PRIMITIVE("list-copy", list_copy, subr1, (SCM l));
EXTERN_PRIMITIVE("assq", assq, subr2, (SCM obj, SCM alist));
EXTERN_PRIMITIVE("assv", assv, subr2, (SCM obj, SCM alist));
EXTERN_PRIMITIVE("assoc", assoc, subr23, (SCM obj, SCM alist, SCM cmp));


int STk_init_list(void);

/*
  ------------------------------------------------------------------------------
  ----
  ----                                  M D 5 . C
  ----
  ------------------------------------------------------------------------------
*/
int STk_init_md5(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                                  M I S C . C
  ----
  ------------------------------------------------------------------------------
*/

#ifdef STK_DEBUG
extern int STk_interactive_debug;
#endif

char *STk_strdup(const char *s);
void STk_add_primitive(struct primitive_obj *o);
void STk_add_primitive_in_module(struct primitive_obj *o, SCM module);
SCM STk_eval_C_string(const char *str, SCM module);
SCM STk_read_from_C_string(const char *str);
void STk_verify_address(unsigned long addr, SCM object);

int STk_init_misc(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                               N U M B E R . C
  ----
  ------------------------------------------------------------------------------
*/
  /****
   **** Integer
   ****/
  /* As said before, integers are not allocated but have their two
   * least significant bits set to 01.
   */

#define INT_VAL(p)      (((long) p) >> 2)
#define INTP(p)         ((((unsigned long) p) & 3) == 1)
#define SCM_LONG(n)     (((n) << 2) | 1)
#define MAKE_INT(n)     (AS_SCM(SCM_LONG(n)))
#define INT_MIN_VAL     ((LONG_MIN & ~3) >> 2)
#define INT_MAX_VAL     ((LONG_MAX & ~3) >> 2)
#define INT_LENGTH      (sizeof(long) * 8 - 2)

long STk_integer_value(SCM x); /* Returns LONG_MIN if not representable as long */
unsigned long STk_uinteger_value(SCM x); /* Returns ULONG_MAX if not an ulong */

  /****
   **** Real
   ****/

#define REAL_FORMAT_SIZE         15 /* default format for real */

struct real_obj {
  stk_header header;
  double val;
};

#define REAL_VAL(p)     (((struct real_obj *) (p))->val)
#define REALP(p)        (BOXED_TYPE_EQ((p), tc_real))

double STk_dbl_true_min(void); /* return (or compute) DBL_TRUE_MIN */

extern double STk_NaN;     /* IEEE NaN special value */


  /****
   **** Bignum
   ****/
struct bignum_obj;      /* complete declaration is in number.c */

#define BIGNUMP(p)      (BOXED_TYPE_EQ((p), tc_bignum))

  /****
   **** Rational
   ****/
struct rational_obj {
  stk_header header;
  SCM num, den;
};

#define RATIONAL_NUM(p)         (((struct rational_obj *) (p))->num)
#define RATIONAL_DEN(p)         (((struct rational_obj *) (p))->den)
#define RATIONALP(p)            (BOXED_TYPE_EQ((p), tc_rational))
#define EXACT_RATIONALP(p)      (RATIONALP(p)             && \
                                 !REALP(RATIONAL_NUM(p))  && \
                                 !REALP(RATIONAL_DEN(p)))

  /****
   **** Complex
   ****/
struct complex_obj {
  stk_header header;
  SCM real, imag;
};

#define COMPLEX_REAL(p)         (((struct complex_obj *) (p))->real)
#define COMPLEX_IMAG(p)         (((struct complex_obj *) (p))->imag)
#define COMPLEXP(p)             (BOXED_TYPE_EQ((p), tc_complex))
#define EXACT_COMPLEXP(p)       (COMPLEXP(p)              && \
                                 !REALP(COMPLEX_REAL(p))  && \
                                 !REALP(COMPLEX_IMAG(p)))


  /****
   **** Conversions
   ****/
SCM             STk_Cstr2number(char *str, long base);
char           *STk_bignum2Cstring(SCM n, int base);
SCM             STk_long2integer(long n);
SCM             STk_ulong2integer(unsigned long n);
SCM             STk_double2real(double d);
double          STk_number2double(SCM n);
long            STk_integer2int32(SCM n, int *overflow);
unsigned long   STk_integer2uint32(SCM n, int *overflow);
EXTERN_PRIMITIVE("number->string", number2string, subr12, (SCM n, SCM base));

  /****
   **** Arithmetic
   ****/
SCM STk_add2(SCM o1, SCM o2);
SCM STk_sub2(SCM o1, SCM o2);
SCM STk_mul2(SCM o1, SCM o2);
SCM STk_div2(SCM o1, SCM o2);

long STk_numeq2(SCM o1, SCM o2);
long STk_numdiff2(SCM o1, SCM o2);
long STk_numlt2(SCM o1, SCM o2);
long STk_numgt2(SCM o1, SCM o2);
long STk_numle2(SCM o1, SCM o2);
long STk_numge2(SCM o1, SCM o2);
void STk_double2Cstr(char *buffer, size_t buuflen, double n);


  /****
   **** Predicate
   ****/
int STk_real_isoddp(SCM n);   /* n MUST be a real */
int STk_isnan(SCM z);
EXTERN_PRIMITIVE("nan=?", nan_equalp, subr2, (SCM n1, SCM n2));


int    STk_init_number(void);

#define NUMBERP(x)      (INTP(x) || BIGNUMP(x) || REALP(x) || RATIONALP(x) || \
                         COMPLEXP(x))
#define EXACTP(x)       (INTP(x) || BIGNUMP(x) || EXACT_RATIONALP(x) || \
                         EXACT_COMPLEXP(x))

/*
  ------------------------------------------------------------------------------
  ----
  ----                               O B J E C T . C
  ----
  ------------------------------------------------------------------------------
*/

int STk_init_object(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                             P A R A M E T E R . C
  ----
  ------------------------------------------------------------------------------
*/

int STk_init_parameter(void);

SCM STk_get_parameter(SCM param);
SCM STk_set_parameter(SCM param, SCM value);
SCM STk_make_C_parameter(char *name, SCM value, SCM (*proc)(SCM new_value),
                         SCM module);
SCM STk_make_C_parameter2(char *name,SCM (*value)(void),SCM (*proc)(SCM new_value),
                          SCM module);


/*
  ------------------------------------------------------------------------------
  ----
  ----                                  P A T H . C
  ----
  ------------------------------------------------------------------------------
*/
char *STk_expand_file_name(const char *s);
SCM STk_do_glob(int argc, SCM *argv);
SCM STk_resolve_link(const char *path, int count);


/*
  ------------------------------------------------------------------------------
  ----
  ----                                  P O R T . C
  ----
  ---- (and s, fport.c, sport.c, vport.c)
  ----
  ------------------------------------------------------------------------------
*/

  /* Code for port is split in several files:
   *    - s contains the low level IO functions which mimic the C IO. All
   *      these functions take Scheme ports as parameter instead of FILE *
   *    - fport.c contains the specific code for port associated to files
   *    - sport.c contains the specific code for port associated to string ports
   *    - vport.c contains the specific code for port associated to virtual ports
   *    - port.c contains the code which can be used on any kind of port
   */

struct port_obj {
  stk_header header;
  void *stream;                 /* stream descriptor != for strings, file, virt. */
  int  flags;                   /* associated flags */
  int  ungetted_char;           /* character ungetted, EOF if none */
  char *filename;               /* File name (for file port, a const otherwise) */
  int  line;                    /* Line number  (unused when writing) */
  int  pos;                     /* position from the start of file */
  int  keyword_colon_pos;       /* position of the ':' in keywords */
  SCM  close_hook;              /* hook called when a file is closed */

  /* virtual functions (in the object 'cause the # of ports should be low ) */
  void  (*print_it)  (SCM obj, SCM port);  /* used to display or print object */
  void  (*release_it)(SCM obj);
  int   (*creadyp)   (void *stream);
  int   (*cgetc)     (void *stream);
  int   (*ceofp)     (void *stream);
  int   (*cclose)    (void *stream);
  int   (*cputc)     (int c, void *stream);
  int   (*cputs)     (const char *s, void *stream);
  int   (*cnputs)    (void *stream, const char *str, int len);
  int   (*cputstring)(void *stream, SCM str);
  int   (*cflush)    (void *stream);
  int   (*read_buff) (void *stream, void *buf, int count);
  int   (*write_buff)(void *stream, const void *buf, int count);
  off_t (*seek)      (void *stream, off_t offset, int whence);
};

#define PORT_MAX_PRINTF 4096    /* max size for snprintf buffer */

#define PORT_READ               (1<<0)
#define PORT_WRITE              (1<<1)
#define PORT_RW                 (1<<2)
#define PORT_CLOSED             (1<<3)
#define PORT_IS_PIPE            (1<<4)
#define PORT_IS_FILE            (1<<5)
#define PORT_IS_STRING          (1<<6)
#define PORT_IS_BYTEVECTOR      (1<<7)
#define PORT_IS_VIRTUAL         (1<<8)
#define PORT_IS_INTERACTIVE     (1<<9)
#define PORT_CASE_SENSITIVE     (1<<10)
#define PORT_TEXTUAL            (1<<11)
#define PORT_BINARY             (1<<12)

#define PORT_STREAM(x)     (((struct port_obj *) (x))->stream)
#define PORT_FLAGS(x)      (((struct port_obj *) (x))->flags)
#define PORT_UNGETC(x)     (((struct port_obj *) (x))->ungetted_char)
#define PORT_LINE(x)       (((struct port_obj *) (x))->line)
#define PORT_POS(x)        (((struct port_obj *) (x))->pos)
#define PORT_FNAME(x)      (((struct port_obj *) (x))->filename)
#define PORT_KW_COL_POS(x) (((struct port_obj *) (x))->keyword_colon_pos)
#define PORT_CLOSEHOOK(x)  (((struct port_obj *) (x))->close_hook)

#define PORT_PRINT(x)     (((struct port_obj *) (x))->print_it)
#define PORT_RELEASE(x)   (((struct port_obj *) (x))->release_it)
#define PORT_READY(x)     (((struct port_obj *) (x))->creadyp)
#define PORT_GETC(x)      (((struct port_obj *) (x))->cgetc)
#define PORT_EOFP(x)      (((struct port_obj *) (x))->ceofp)
#define PORT_CLOSE(x)     (((struct port_obj *) (x))->cclose)
#define PORT_PUTC(x)      (((struct port_obj *) (x))->cputc)
#define PORT_PUTS(x)      (((struct port_obj *) (x))->cputs)
#define PORT_PUTSTRING(x) (((struct port_obj *) (x))->cputstring)
#define PORT_NPUTS(x)     (((struct port_obj *) (x))->cnputs)
#define PORT_FLUSH(x)     (((struct port_obj *) (x))->cflush)
#define PORT_BREAD(x)     (((struct port_obj *) (x))->read_buff)
#define PORT_BWRITE(x)    (((struct port_obj *) (x))->write_buff)
#define PORT_SEEK(x)      (((struct port_obj *) (x))->seek)

#define PORTP(x)   (BOXED_TYPE_EQ((x), tc_port))
#define IPORTP(x)  (PORTP(x) && (PORT_FLAGS(x) & (PORT_READ|PORT_RW)))
#define OPORTP(x)  (PORTP(x) && (PORT_FLAGS(x) & (PORT_WRITE|PORT_RW)))

#define FPORTP(x)  (PORTP(x)  && (PORT_FLAGS(x) & (PORT_IS_FILE|PORT_IS_PIPE)))
#define IFPORTP(x) (FPORTP(x) && (PORT_FLAGS(x) & (PORT_READ|PORT_RW)))
#define OFPORTP(x) (FPORTP(x) && (PORT_FLAGS(x) & (PORT_WRITE|PORT_RW)))

#define SPORTP(x)  (PORTP(x)  && (PORT_FLAGS(x) & PORT_IS_STRING))
#define ISPORTP(x) (SPORTP(x) && (PORT_FLAGS(x) & (PORT_READ|PORT_RW)))
#define OSPORTP(x) (SPORTP(x) && (PORT_FLAGS(x) & (PORT_WRITE|PORT_RW)))

#define BPORTP(x)  (PORTP(x)  && (PORT_FLAGS(x) & PORT_IS_BYTEVECTOR))
#define IBPORTP(x) (BPORTP(x) && (PORT_FLAGS(x) & (PORT_READ|PORT_RW)))
#define OBPORTP(x) (BPORTP(x) && (PORT_FLAGS(x) & (PORT_WRITE|PORT_RW)))

#define VPORTP(x)  (PORTP(x)  && (PORT_FLAGS(x) & PORT_IS_VIRTUAL))
#define IVPORTP(x) (VPORTP(x) && (PORT_FLAGS(x) & (PORT_READ|PORT_RW)))
#define OVPORTP(x) (VPORTP(x) && (PORT_FLAGS(x) & (PORT_WRITE|PORT_RW)))

#define PORT_IS_CLOSEDP(x)      (PORT_FLAGS(x) & PORT_CLOSED)
#define PORT_CASE_SENSITIVEP(x) (PORT_FLAGS(x) & PORT_CASE_SENSITIVE)
#define PORT_BINARYP(x)         (PORT_FLAGS(x) & PORT_BINARY)
#define PORT_TEXTUALP(x)        (PORT_FLAGS(x) & PORT_TEXTUAL)


/****
 ****           sio.h primitives
 ****/


int STk_readyp(SCM port);
int STk_getc(SCM port);
int STk_get_character(SCM port); /* result may be a wide char */
int STk_ungetc(int c, SCM port);
int STk_close(SCM port);
int STk_putc(int c, SCM port);
int STk_put_character(int c, SCM port);   /* c may be a wide char */
int STk_puts(const char *s, SCM port);
int STk_putstring(SCM s, SCM port);
int STk_nputs(SCM port, const char *s, int len);
off_t STk_seek(SCM port, off_t offset, int whence);
off_t STk_tell(SCM port);
void STk_rewind(SCM port);
int STk_flush(SCM port);
int STk_feof(SCM port);
int STk_fprintf(SCM port, char *format, ...);
int STk_read_buffer(SCM port, void *buff, int count);
int STk_write_buffer(SCM port, void *buff, int count);


/****
 ****           fport.h primitives
 ****/
SCM STk_open_file(char *filename, char *mode);
SCM STk_add_port_idle(SCM port, SCM idle_func);
SCM STk_fd2scheme_port(int fd, const char *mode, char *identification);
void STk_set_line_buffered_mode(SCM port);
int STk_init_fport(void);
SCM STk_current_input_port(void);
SCM STk_current_output_port(void);
SCM STk_current_error_port(void);
void STk_close_all_ports(void);


/****
 ****           sport.h primitives
 ****/
EXTERN_PRIMITIVE("open-output-string", open_output_string, subr0, (void));
SCM STk_get_output_string(SCM port);
SCM STk_open_C_string(const char *str);
int STk_init_sport(void);

/****
 ****           vport.h primitives
 ****/
int STk_init_vport(void);


/****
 ****           port.h primitives
 ****/
EXTERN_PRIMITIVE("close-port", close_port, subr1, (SCM port));
EXTERN_PRIMITIVE("read-line", read_line, subr01, (SCM port));

void STk_error_bad_port(SCM p);
void STk_error_bad_file_name(SCM f);
void STk_error_bad_io_param(char *fmt, SCM p);
void STk_error_file_name(char *fmt, SCM fn);

int STk_init_port(void);



/****
 ****           Port global variables
 ****/

extern char *STk_current_filename;              /* Name of the file we read   */
extern SCM STk_stdin, STk_stdout, STk_stderr;   /* unredirected ports         */
extern int STk_interactive;                     /* We are in interactive mode */

/*
  ------------------------------------------------------------------------------
  ----
  ----                            P R I N T . C
  ----
  ------------------------------------------------------------------------------
*/
#define DSP_MODE                0
#define WRT_MODE                1

void STk_print(SCM exp, SCM port, int mode);
void STk_print_star(SCM exp, SCM port, int mode);

int STk_init_printer(void);

/*
  ------------------------------------------------------------------------------
  ----
  ----                          P R O C E S S . C
  ----
  ------------------------------------------------------------------------------
*/
int STk_init_process(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                          P R O M I S E . C
  ----
  ------------------------------------------------------------------------------
*/
int STk_init_promise(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                                  P R O C . C
  ----
  ------------------------------------------------------------------------------
*/

typedef int16_t STk_instr;

struct closure_obj {
  stk_header header;
  int16_t arity;
  uint16_t code_size;
  SCM env;
  SCM plist;
  SCM name;
  SCM* constants;
  STk_instr *bcode;
};

#define CLOSURE_ARITY(p)        (((struct closure_obj *) (p))->arity)
#define CLOSURE_SIZE(p)         (((struct closure_obj *) (p))->code_size)
#define CLOSURE_ENV(p)          (((struct closure_obj *) (p))->env)
#define CLOSURE_PLIST(p)        (((struct closure_obj *) (p))->plist)
#define CLOSURE_NAME(p)         (((struct closure_obj *) (p))->name)
#define CLOSURE_CONST(p)        (((struct closure_obj *) (p))->constants)
#define CLOSURE_BCODE(p)        (((struct closure_obj *) (p))->bcode)
#define CLOSUREP(p)             (BOXED_TYPE_EQ((p), tc_closure))

EXTERN_PRIMITIVE("procedure?", procedurep, subr1, (SCM obj));
EXTERN_PRIMITIVE("%procedure-arity", proc_arity, subr1, (SCM proc));


SCM STk_make_closure(STk_instr *code, int size, int arity, SCM *cst, SCM env);
int STk_init_proc(void);

extern SCM STk_key_source, STk_key_formals, STk_key_doc;

/*
  ------------------------------------------------------------------------------
  ----
  ----                            R E A D . C
  ----
  ------------------------------------------------------------------------------
*/
SCM   STk_read(SCM port, int case_significant);
SCM   STk_read_constant(SCM port, int case_significant);
char *STk_quote2str(SCM symb);
int   STk_init_reader(void);
int   STk_keyword_colon_convention(void); // pos. of ':' in symbol to make a  keyword
extern int STk_read_case_sensitive;


/*
  ------------------------------------------------------------------------------
  ----
  ----                            R E G E X P . C
  ----
  ------------------------------------------------------------------------------
*/
int STk_init_regexp(void);
EXTERN_PRIMITIVE("string->regexp", str2regexp, subr1, (SCM re));
EXTERN_PRIMITIVE("regexp-match", regexec, subr2, (SCM re, SCM str));


/*
  ------------------------------------------------------------------------------
  ----
  ----                          S I G N A L  . C
  ----
  ------------------------------------------------------------------------------
*/
struct codeset_code {   // Used by SRFI 238
  const char* name;
  int code;
};

extern struct codeset_code STk_signal_names[];


int STk_get_signal_value(SCM sig);
int STk_init_signal(void);

/*
  ------------------------------------------------------------------------------
  ----
  ----                          S O C K E T . C
  ----
  ------------------------------------------------------------------------------
*/
int STk_init_socket(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                            S T R  . C
  ----
  ------------------------------------------------------------------------------
*/
struct string_obj {
  stk_header header;
  int space;            /* allocated size  */
  int size;             /* # of bytes used */
  int length;           /* "external" length of the string */
  char *chars;
};

#define STRING_SPACE(p)  (((struct string_obj *) (p))->space)
#define STRING_SIZE(p)   (((struct string_obj *) (p))->size)
#define STRING_LENGTH(p) (((struct string_obj *) (p))->length)
#define STRING_CHARS(p)  (((struct string_obj *) (p))->chars)
#define STRINGP(p)       (BOXED_TYPE_EQ((p), tc_string))

#define STRING_CONST     (1 << 0)

#define STRING_MONOBYTE(str)    (STRING_LENGTH(str) == STRING_SIZE(str))

SCM STk_makestring(int len, const char *init);
SCM STk_Cstring2string(const char *str);           /* Embed a C string in Scheme world  */

EXTERN_PRIMITIVE("string=?", streq, subr2, (SCM s1, SCM s2));
EXTERN_PRIMITIVE("string-ref", string_ref, subr2, (SCM str, SCM index));
EXTERN_PRIMITIVE("string-set!", string_set, subr3, (SCM str, SCM index, SCM value));
EXTERN_PRIMITIVE("string-downcase!", string_ddowncase, vsubr, (int argc, SCM *argv));
int STk_init_string(void);

/*
  ------------------------------------------------------------------------------
  ----
  ----                           S T R U C T  . C
  ----
  ------------------------------------------------------------------------------
*/
int STk_init_struct(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                           S Y M B O L . C
  ----
  ------------------------------------------------------------------------------
*/
struct symbol_obj {
  stk_header header;    /* must be at the same offset as for keywords */
  const char *pname;
};

#define SYMBOL_PNAME(p) (((struct symbol_obj *) (p))->pname)
#define SYMBOLP(p)      (BOXED_TYPE_EQ((p),tc_symbol))

#define SYMBOL_NEEDS_BARS       (1 << 0)        /* Info flag */
#define SYMBOL_HAS_UPPER        (1 << 1)

EXTERN_PRIMITIVE("string->symbol", string2symbol, subr1, (SCM string));

int STk_symbol_flags(const char *s);
SCM STk_intern(char *name);
SCM STk_make_uninterned_symbol(const char *name);
int STk_init_symbol(void);

  /*
  ------------------------------------------------------------------------------
  ----
  ----                           S Y N T A X  . C
  ----
  ------------------------------------------------------------------------------
*/
int STk_init_syntax(void);

/*
  ------------------------------------------------------------------------------
  ----
  ----                           S Y S T E M . C
  ----
  ------------------------------------------------------------------------------
*/
extern struct codeset_code STk_errno_names[];

extern SCM  STk_posix_error_condition;  /* condition type &posix-error */

void STk_error_posix(int err,char *proc_name, SCM arg1, SCM arg2);

int STk_dirp(const char *path);
int STk_init_system(void);

EXTERN_PRIMITIVE("%pre-exit", pre_exit, subr1, (SCM retcode));
EXTERN_PRIMITIVE("exit", exit, subr01, (SCM retcode));


/*
  ------------------------------------------------------------------------------
  ----
  ----                           T H R E A D . C
  ----
  ------------------------------------------------------------------------------
*/
EXTERN_PRIMITIVE("current-thread", current_thread, subr0, (void));
int STk_init_threads(int stack_size, void *start_stack);
int STk_init_mutexes(void);

/*
  ------------------------------------------------------------------------------
  ----
  ----                           U T F 8  . C
  ----
  ------------------------------------------------------------------------------
*/

#define UTF8_INCORRECT_SEQUENCE (-2)

extern int STk_use_utf8;

#define VALID_UTF8_VALUE(c)                                                       \
  /* Unicode defines characters in the range [0, #xd7FF] U [#xE000, #x10FFFF] */  \
  ((0 <= (c)  && (c) <=  0xd7ff) || (0xE000 <=(c) && (c) <= 0x10FFFF))


char *STk_utf8_grab_char(char *str, uint32_t *c); /* result = pos. after current one */
int STk_char2utf8(int ch, char *str); /* result = length of the UTF-8 repr. */
int STk_utf8_strlen(const char *s, int max);
int STk_utf8_read_char(SCM port);
int STk_utf8_sequence_length(const char *str); /* # of bytes of sequence starting at str */
int STk_utf8_char_bytes_needed(unsigned int ch); /* # of bytes needed to represent ch*/
int STk_utf8_verify_sequence(char *s, int len); /* s constitutes a valid UTF8? */
char *STk_utf8_index(char *s, int i, int max); /* return the address of ith char of s*/
int STk_utf8_char_from_byte(char *s, int i, int max); /*  byte index => char index */

int STk_init_utf8(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                                 U V E C T O R . C
  ----
  ------------------------------------------------------------------------------
*/

struct uvector_obj {
  stk_header header;
  int vect_type;
  int size;
  char data[1];
};

#define UVECT_S8        0
#define UVECT_U8        1
#define UVECT_S16       2
#define UVECT_U16       3
#define UVECT_S32       4
#define UVECT_U32       5
#define UVECT_S64       6
#define UVECT_U64       7
#define UVECT_F32       8
#define UVECT_F64       9
#define UVECT_C64       10
#define UVECT_C128      11

/*
 * 64 bits values are always represented with bignums even on 64 bits machines
 * Here are the interesting maxima for 64 bits.
 */
#define S64_MIN "-9223372036854775808"
#define S64_MAX  "9223372036854775807"
#define U64_MAX "18446744073709551615"


#define UVECTOR_TYPE(p) (((struct uvector_obj *) (p))->vect_type)
#define UVECTOR_SIZE(p) (((struct uvector_obj *) (p))->size)
#define UVECTOR_DATA(p) (((struct uvector_obj *) (p))->data)
#define UVECTORP(p)     (BOXED_TYPE_EQ((p), tc_uvector))

#define BYTEVECTORP(p)  (UVECTORP(p) && UVECTOR_TYPE(p) == UVECT_U8)

extern int STk_uvectors_allowed;

int STk_uniform_vector_tag(char *s);
int STk_uvector_equal(SCM u1, SCM u2);
SCM STk_list2uvector(int type, SCM l);
SCM STk_uvector_get(SCM v, long i);
void STk_uvector_put(SCM v, long i, SCM value);
int STk_init_uniform_vector(void);

SCM STk_make_C_bytevector(int len);
SCM STk_make_bytevector_from_C_string(char *str, long len);


/*
  ------------------------------------------------------------------------------
  ----
  ----                                 V E C T O R . C
  ----
  ------------------------------------------------------------------------------
*/

struct vector_obj {
  stk_header header;
  int size;
  SCM data[1];
};

#define VECTOR_SIZE(p)        (((struct vector_obj *) (p))->size)
#define VECTOR_DATA(p)        (((struct vector_obj *) (p))->data)
#define VECTORP(p)    (BOXED_TYPE_EQ((p), tc_vector))

#define VECTOR_CONST   (1 << 0)

EXTERN_PRIMITIVE("vector-ref", vector_ref, subr2, (SCM v, SCM index));
EXTERN_PRIMITIVE("vector-set!", vector_set, subr3, (SCM v, SCM index, SCM value));
EXTERN_PRIMITIVE("vector->list", vector2list, subr1, (SCM v));
EXTERN_PRIMITIVE("list->vector", list2vector, subr1, (SCM l));

SCM STk_makevect(int len, SCM init);
int STk_init_vector(void);


/*
  ------------------------------------------------------------------------------
  ----
  ----                           V M . C
  ----
  ------------------------------------------------------------------------------
*/
#define DEFAULT_STACK_SIZE 100000

void STk_raise_exception(SCM cond);
SCM STk_C_apply(SCM func, int nargs, ...);
SCM STk_C_apply_list(SCM func, SCM l);
void STk_get_stack_pointer(void **addr);
SCM STk_n_values(int n, ...);
SCM STk_values2vector(SCM obj, SCM vect);

EXTERN_PRIMITIVE("values", values, vsubr, (int argc, SCM *argv));
EXTERN_PRIMITIVE("%vm-backtrace", vm_bt, subr0, (void));

SCM STk_load_bcode_file(SCM f);
int STk_load_boot(char *s);
int STk_boot_from_C(void);
SCM STk_execute_C_bytecode(SCM consts, STk_instr *instr);

int STk_init_vm(void);
int STk_late_init_vm(void);   // run when env.c is fully initialized

/*****************************************************************************/

extern char *STk_boot_consts;
extern STk_instr STk_boot_code[];



/* Special constants */

#define STk_nil         ((SCM) MAKE_SCONST(0))
#define STk_false       ((SCM) MAKE_SCONST(1))
#define STk_true        ((SCM) MAKE_SCONST(2))
#define STk_eof         ((SCM) MAKE_SCONST(3))
#define STk_void        ((SCM) MAKE_SCONST(4)) // must be the last constant

/* STk_void must be the last small constant, since it is used by 'read_address'
   in read.c, to validate small  constants. Insert new constants before STk_void,
   if needed. It is also used in read.c to build special constant for the reader.
   NOTE: STk_void is the last *READABLE* and *REFERENTIABLE constant.
*/


/* Misc */
#if defined(__GNUC__) || defined(__clang__)
#  define _UNUSED(x) __attribute__((__unused__)) x
#else
#  define _UNUSED(x) x
#endif

#ifdef __cplusplus
}
#endif


#endif /* STKLOS_H */
