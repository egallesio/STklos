/*
 *
 * p r o c . c                          -- Things about procedures
 *
 * Copyright © 1993-2025 Erick Gallesio <eg@stklos.net>
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
 *           Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: 15-Nov-1993 22:02
 */

#include "stklos.h"
#include "object.h"


/* Keywords used in procedure plists */
SCM STk_key_source, STk_key_formals, STk_key_doc;


/*===========================================================================*\
 *
 *                                  Utilities
 *
\*===========================================================================*/

static void error_malformed_list(SCM obj)
{
  STk_error("malformed list ~S", obj);
}

static void error_bad_procedure(SCM obj)
{
  STk_error("bad procedure ~S", obj);
}


SCM STk_make_closure(STk_instr *code, int size, int arity, SCM *cst, SCM env)
{
  SCM z;

  NEWCELL(z, closure);
  CLOSURE_ENV(z)   = env;
  CLOSURE_PLIST(z) = STk_nil;
  CLOSURE_NAME(z)  = STk_false;
  CLOSURE_ARITY(z) = arity;
  CLOSURE_CONST(z) = cst;
  CLOSURE_BCODE(z) = code;
  CLOSURE_SIZE(z)  = size;
  return z;
}


static void print_lambda(SCM closure, SCM port, int mode)
{
  if (CLOSURE_NAME(closure) != STk_false)
    STk_fprintf(port, "#[closure %s", SYMBOL_PNAME(CLOSURE_NAME(closure)));
  else
    STk_fprintf(port, "#[closure %lx", (unsigned long) closure);

  SCM formals = STk_key_get(CLOSURE_PLIST(closure), STk_key_formals, STk_false);
  if (formals != STk_false) {
    STk_nputs(port, " ", 1);
    STk_print(formals, port, mode);
  }
  STk_nputs(port,"]", 1);
}


/*
 * The stucture which describes the closure type
 */
static struct extended_type_descr xtype_closure = {
  .name  = "closure",
  .print = print_lambda
};


/*=============================================================================*/

/*
<doc  procedure?
 * (procedure? obj)
 *
 * Returns |#t| if |obj| is a procedure, otherwise returns |#f|.
 *
 * @lisp
 * (procedure? car)                            =>  #t
 * (procedure? 'car)                           =>  #f
 * (procedure? (lambda (x) (* x x)))           =>  #t
 * (procedure? '(lambda (x) (* x x)))          =>  #f
 * (call-with-current-continuation procedure?) =>  #t
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("procedure?", procedurep, subr1, (SCM obj))
{
  switch (STYPE(obj)) {
    case tc_subr0:
    case tc_subr1:
    case tc_subr2:
    case tc_subr3:
    case tc_subr4:
    case tc_subr5:
    case tc_subr01:
    case tc_subr12:
    case tc_subr23:
    case tc_subr34:
    case tc_vsubr:
    case tc_apply:
    case tc_next_method:
    case tc_continuation:
    case tc_parameter:
    case tc_closure:
    case tc_ext_func:    return STk_true;
    case tc_instance:    return (STk_methodp(obj) != STk_false) ?
                                   STk_true:
                                   STk_genericp(obj);
#ifdef FIXME
//     case tc_call_cc:
//     case tc_dynwind:    return STk_true;
//     default:                 if (EXTENDEDP(obj))
//                                return STk_extended_procedurep(obj) ? STk_true : STk_false;
//                      else
//                        return STk_false;
#endif
    default: return STk_false;
  }
}

DEFINE_PRIMITIVE("%procedure-name", procedure_name, subr1, (SCM obj))
{
  switch (STYPE(obj)) {
    case tc_subr0:
    case tc_subr1:
    case tc_subr2:
    case tc_subr3:
    case tc_subr4:
    case tc_subr5:
    case tc_subr01:
    case tc_subr12:
    case tc_subr23:
    case tc_subr34:
    case tc_vsubr:
    case tc_apply:    return STk_Cstring2string(PRIMITIVE_NAME(obj));
#ifdef HAVE_FFI
    case tc_ext_func: return STk_ext_func_name(obj);
#endif
    case tc_closure:  if (CLOSURE_NAME(obj) != STk_false)
                         return STk_Cstring2string(SYMBOL_PNAME(CLOSURE_NAME(obj)));
                      /* FALLTHROUGH */
    default:         return obj;
  }
}

DEFINE_PRIMITIVE("%set-procedure-name!", set_procedure_name, subr2, (SCM obj, SCM v))
{
  if (!CLOSUREP(obj)) error_bad_procedure(obj);
  if (!SYMBOLP(v))    STk_error("bad symbol ~S", v);
  CLOSURE_NAME(obj) = v;
  return STk_void;
}



/*
<doc EXT closure?
 * (closure? obj)
 *
 * Returns |#t| if |obj| is a procedure created with the |lambda|
 * syntax and |#f| otherwise.
 *
 * Note that primitive procedures (those which are written in C) are
 * *not* closures:
 *
 * @lisp
 * (define (cube x) (* x x x))
 * (closure? cube)               => #t
 *
 * (define square-root sqrt)
 * (eq? square-root sqrt)        => #t
 * (closure? square-root)        => #f
 *
 * (closure? 10)                 => #f
 * (closure? display)            => #f
 * (closure? (lambda (x) (- x))) => #t
 * (closure? any)                => #t
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("closure?", closurep, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(CLOSUREP(obj));
}


DEFINE_PRIMITIVE("%procedure-plist", proc_plist, subr1, (SCM obj))
{
  switch (STYPE(obj)) {
    case tc_subr0:
    case tc_subr1:
    case tc_subr2:
    case tc_subr3:
    case tc_subr4:
    case tc_subr5:
    case tc_subr01:
    case tc_subr12:
    case tc_subr23:
    case tc_subr34:
    case tc_vsubr:   return PRIMITIVE_PLIST(obj);
    case tc_closure: return CLOSURE_PLIST(obj);
    default:         error_bad_procedure(obj);
  }
  return STk_void;
}

DEFINE_PRIMITIVE("%set-procedure-plist!", set_proc_plist, subr2, (SCM obj, SCM v))
{
  switch (STYPE(obj)) {
    case tc_subr0:
    case tc_subr1:
    case tc_subr2:
    case tc_subr3:
    case tc_subr4:
    case tc_subr5:
    case tc_subr01:
    case tc_subr12:
    case tc_subr23:
    case tc_subr34:
    case tc_vsubr:   PRIMITIVE_PLIST(obj) = v; break;
    case tc_closure: CLOSURE_PLIST(obj)   = v; break;
    default:         error_bad_procedure(obj);
  }
  return STk_void;
}


DEFINE_PRIMITIVE("%procedure-arity", proc_arity, subr1, (SCM proc))
{
  int res;

  switch (STYPE(proc)) {
    case tc_subr0:        res = 0;  break;
    case tc_subr1:        res = 1;  break;
    case tc_subr2:        res = 2;  break;
    case tc_subr3:        res = 3;  break;
    case tc_subr4:        res = 4;  break;
    case tc_subr5:        res = 5;  break;
    case tc_subr01:       res = -1; break;
    case tc_subr12:       res = -2; break;
    case tc_subr23:       res = -3; break;
    case tc_subr34:       res = -4; break;
    case tc_vsubr:        res = -1; break;
    case tc_apply:        res = -1; break;
      /*  case tc_next_method: */
    case tc_continuation: res = 1;  break;
    case tc_parameter:    res = -1; break;
    case tc_closure:      res = CLOSURE_ARITY(proc); break;
      /* case tc_instance: */
    default : return STk_false;
  }
  return MAKE_INT(res);
}


DEFINE_PRIMITIVE("%procedure-code", proc_code, subr1, (SCM proc))
{
  int i, len;
  SCM v;
  STk_instr *p;

  if (!CLOSUREP(proc)) return STk_false;

  /* Make a copy of the code */
  len = CLOSURE_SIZE(proc);
  v   = STk_makevect(len, (SCM) NULL);

  for (i=0, p=CLOSURE_BCODE(proc); i < len; i++, p++)
    VECTOR_DATA(v)[i] = MAKE_INT(*p);

  return v;
}

DEFINE_PRIMITIVE("%procedure-doc", proc_doc, subr1, (SCM proc))
{
  if (!CLOSUREP(proc)) return STk_false;
  return STk_key_get(CLOSURE_PLIST(proc), STk_key_doc, STk_false);
}

DEFINE_PRIMITIVE("%procedure-environment", proc_env, subr1, (SCM proc))
{
  if (!CLOSUREP(proc)) return STk_false;
  return CLOSURE_ENV(proc);
}

/*
<doc EXT procedure-formals
 * (procedure-formals proc)
 *
 * Returns the formal parameters of procedure |proc|.
 * Note that procedure formal parameters are kept in memory only if
 * the compiler flag <<"compiler:keep-formals">> is set at its creation.
 * If |proc| formal parameters are not available, |procedure-formals|
 * returns |#f|.
 *
 * @lisp
 * (compiler:keep-formals #t)
 *
 * (define (f x y) (+ (* 3 x) y))
 * (procedure-formals f)           => (x y)
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("procedure-formals", proc_formals, subr1, (SCM proc))
{
  if (!CLOSUREP(proc)) error_bad_procedure(proc);
  return STk_key_get(CLOSURE_PLIST(proc), STk_key_formals, STk_false);
}

/*
<doc EXT procedure-source
 * (procedure-source proc)
 *
 * Returns the source form used to define procedure |proc|.
 * Note that procedure source is kept in memory only if the compiler flag
 * <<"compiler:keep-source">> is set at its creation. If |proc| source is
 * not available, |procedure-source| returns |#f|.
 *
 * @lisp
 * (compiler:keep-source #t)
 *
 * (define (f x y) (+ (* 3 x) y))
 * (procedure-source f)           => (lambda (x y) (+ (* 3 x) y))
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("procedure-source", proc_source, subr1, (SCM proc))
{
  if (!CLOSUREP(proc)) error_bad_procedure(proc);
  else {
    SCM args = STk_key_get(CLOSURE_PLIST(proc), STk_key_formals, STk_false);
    SCM body = STk_key_get(CLOSURE_PLIST(proc), STk_key_source,  STk_false);

    if (args != STk_false && body != STk_false) {
      return STk_cons(STk_intern("lambda"),
                      STk_cons(args, body));
    }
  }
  return STk_false;
}

/*===========================================================================*\
 *
 *                      M A P   &   F O R - E A C H
 *
\*===========================================================================*/

/* map does both 'map' and 'for-each':
   - when in_map is non-zero, the result of each operation will be stored
     in a result list
   - when it is zero, no result is stored, and AN EMPTY LIST is returned
     (not void -- the primitive "for-each" will do that). */
static SCM map(int argc, SCM *argv, int in_map)
{
  SCM fct, res, ptr, v, tmp, *args;
  int i, j, len, tmplen;

  if (argc <= 1) STk_error("expected at least 2 arguments (given %d)", argc);

  fct   = *argv--;
  argc -= 1;
  res   = STk_nil;


  if (argc == 1) {
    /* frequent case (map (lambda (x) ...) list). Do it specially */

    v = *argv;

    if (in_map) {
        /* Calculate the length so we can use STk_C_make_list(): */
        len = STk_int_length(v);
        if (len < 0) STk_error("bad list ~W", v);
        /* Allocate the result list: */
        res = STk_C_make_list(len, STk_false);
    }

    /* Just fill in the list and return it: */
    for (ptr = res; !NULLP(v); v = CDR(v)) {
      if (!CONSP(v)) error_malformed_list(v);
      tmp = STk_C_apply(fct, 1, CAR(v));
      if (in_map) {
        CAR(ptr) = tmp;
        ptr = CDR(ptr);
      }
    }
    return res;

  } else {

    /* General case */
    v    = STk_makevect(argc, (SCM) NULL);
    args = VECTOR_DATA(v);

      if (in_map) {
        /* Find out the length of the smaller list and allocate
           the reulting list of that size. */
        len = INT_MAX;
        for (i=0, j=0; i < argc; i++, j--) {
          tmplen = STk_int_length(argv[j]);
          /* If tmplen is negative, we ignore it. This is because we
             should accept circular lists, so long as there is a
             non-circular list also in the arguments See SRFI 1 test:
             (map + '(3 1 4 1) (circular-list 1 0)) expects (4 1 5 1) */
          if (tmplen >= 0 && tmplen < len) len = tmplen;
        }
        if (len == INT_MAX) STk_error("at least one proper list required");

        res = STk_C_make_list(len, STk_false);
        ptr = res;
    }

    for ( ; ; ) {
      /* Build the parameter list.
         'argc' was decreased by one, so it is now *exactlty* the
         number of lists on which MAP will operate. Which is also
         the argument count of the function to be applied. */
      for (i=0, j=0; i < argc; i++, j--) {
        if (NULLP(argv[j]))
          return res;
        if (!CONSP(argv[j])) error_malformed_list(argv[j]);

        args[i] = CAR(argv[j]); /* set i-th argument to fct */
        argv[j] = CDR(argv[j]); /* get the next argument from MAP */
      }

      tmp = STk_C_apply(fct, -argc, args);

      /* If we're storing the resulting list, set the CAR at this
         position, and update the pointer. */
      if (in_map) {
        CAR(ptr) = tmp;
        ptr = CDR(ptr);
      }
    }
  }
  return STk_void;      /* never reached */
}

/*
<doc map
 * (map proc list1 list2 ...)
 *
 * The |list|s must be lists, and |proc| must be a procedure taking as many
 * arguments as there are lists and returning a single value.
 * If more than one list is given, then they must all be the same length.
 * |Map| applies |proc| element-wise to the elements of the |list|s and returns
 * a list of the results, in order.  The dynamic order in which proc is applied
 * to the elements of the lists is unspecified.
 * @lisp
 * (map cadr '((a b) (d e) (g h)))   =>  (b e h)
 *
 * (map (lambda (n) (expt n n))
 *      '(1 2 3 4 5))                =>  (1 4 27 256 3125)
 *
 * (map + '(1 2 3) '(4 5 6))         =>  (5 7 9)
 *
 * (let ((count 0))
 *   (map (lambda (ignored)
 *       (set! count (+ count 1))
 *       count)
 *        '(a b)))                   =>  (1 2) ,(emph "or") (2 1)
 * @end lisp
doc>
*/

DEFINE_PRIMITIVE("map", map, vsubr, (int argc, SCM* argv))
{
  return map(argc, argv, 1);
}


/*
<doc for-each
 * (for-each proc list1 list2 ...)
 *
 * The arguments to |for-each| are like the arguments to |map|, but |for-each|
 * calls proc for its side effects rather than for its values.
 * Unlike |map|, |for-each| is guaranteed to call proc on the elements of
 * the lists in order from the first element(s) to the last, and the value
 * returned by |for-each| is *_void_*.
 * @lisp
 * (let ((v (make-vector 5)))
 *   (for-each (lambda (i)
 *               (vector-set! v i (* i i)))
 *             '(0 1 2 3 4))
 *   v)                                =>  #(0 1 4 9 16)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("for-each", for_each, vsubr, (int argc, SCM* argv))
{
  map(argc, argv, 0);
  return STk_void;
}

/*
<doc EXT fold
 * (fold kons knil list1 list2 ...)
 *
 * The procedure |kons| will first be applied to |(arg1 arg2 ... argn
 * X)|, where |argj| is the CAR of the j-th list and |X| is |knil|.
 * Then, this will be repeated, but each |argj| is updated to the next
 * element in the j-th list, and |X| is updated to the result of the
 * previous pass.
 *
 * @lisp
 * (fold - 100 '())            => 100
 * (fold - 100 '(1 2 3))       => -98
 * (fold - 100 '(1 4) '(5 10)) => 98
 * @end lisp
 *
 * In the first example, there is only an empty list, so |kons| is not
 * even called, and |knil| is returned.
 *
 * In the second case, the operations performed were
 * @lisp
 * (- 1 100)  => -99       ; X is -99
 * (- 2 -99)  => 101       ; X is 101
 * (- 3 101)  => -98       ; final result
 * @end lisp
 *
 * In the the third example,
 * @lisp
 * (- 1 5 100)   => -104   ; X is -104
 * (- 4 10 -104) =>  98    ; final result
 * @end lisp
doc>
*/
static SCM fold(SCM kons, SCM knil, int n, SCM* lists)
{
  if (n == 1) { /* Fast path for common case */
    SCM tmp = knil;
    for (SCM v = *lists; !NULLP(v); v = CDR(v)) {
      if (!CONSP(v)) error_malformed_list(v);
      tmp = STk_C_apply(kons, 2, CAR(v), tmp);
    }
    return tmp;
  } else {
    SCM v     = STk_makevect(n + 1, (SCM) NULL);
    SCM *args = VECTOR_DATA(v);
    SCM tmp = knil;

    for ( ; ; ) {
      /* Build the parameter list */
      for (int i=0, j=0; i < n; i++,j--) {
        if (NULLP(lists[j]))
          return tmp;
        if (!CONSP(lists[j])) error_malformed_list(lists[j]);

        args[i]  = CAR(lists[j]);
        lists[j] = CDR(lists[j]);
      }
      args[n] = tmp;

      tmp = STk_C_apply(kons, -(n+1), args);
    }
  }
}


DEFINE_PRIMITIVE("fold", fold, vsubr, (int argc, SCM* argv))
{
  if (argc < 3) STk_error("expected at least 3 arguments (given %d)", argc);

  SCM kons = *argv--;
  SCM knil = *argv--;
  SCM *lists = argv;
  SCM *ptr = lists;

  argc -= 2;
  if (STk_procedurep(kons) == STk_false) error_bad_procedure(kons);

  for (int i = 0; i < argc; i++)
    if (!NULLP(*ptr) && !CONSP(*ptr)) STk_error("bad list ~s", *ptr);

  return fold(kons, knil, argc, lists);
}


/*
<doc EXT reduce
 * (reduce f init lst)
 *
 * If |lst| is null, the returned value is |init|. Otherwise,
 * it is equivalent to |(fold f (car lst) (cdr lst))|.
 * That is, |reduce| will operate on each par of consecutive
 * elements of lst beginning at the leftmost position, accumulating
 * the result value.
 *
 * @lisp
 * (reduce - 'a '())        => 'a
 * (reduce - 'a '(10 40 90) => 60
 * @end lisp
 *
 * In the second example, the computations are:
 * @lisp
 * (- 40 10)   => 30 ; will be used as second argument
 * (- 90 30)   => 60 ; final value
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("reduce", reduce, subr3, (SCM f, SCM id, SCM list))
{
  if (STk_procedurep(f) == STk_false) error_bad_procedure(f);
  if (NULLP(list)) return id;
  if (!CONSP(list)) STk_error("bad list ~s", list);

  return fold(f,CAR(list), 1, &(CDR(list)));
}


int STk_init_proc(void)
{
  // Define some keywords to avoid calls to STk_makekey (which uses a mutex!)
  STk_key_source  = STk_makekey("source");
  STk_key_formals = STk_makekey("formals");
  STk_key_doc     = STk_makekey("documentation");

  DEFINE_XTYPE(closure, &xtype_closure);
  ADD_PRIMITIVE(procedurep);
  ADD_PRIMITIVE(closurep);
  ADD_PRIMITIVE(proc_plist);
  ADD_PRIMITIVE(set_proc_plist);
  ADD_PRIMITIVE(proc_code);
  ADD_PRIMITIVE(proc_doc);
  ADD_PRIMITIVE(proc_source);
  ADD_PRIMITIVE(proc_arity);
  ADD_PRIMITIVE(procedure_name);
  ADD_PRIMITIVE(set_procedure_name);
  ADD_PRIMITIVE(proc_formals);
  ADD_PRIMITIVE(proc_env);

  ADD_PRIMITIVE(map);
  ADD_PRIMITIVE(for_each);

  ADD_PRIMITIVE(fold);
  ADD_PRIMITIVE(reduce);
return TRUE;
}
