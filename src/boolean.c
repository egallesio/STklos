
/*
 *
 * b o o l e a n . c			-- Booleans and Equivalence predicates
 *
 * Copyright © 1993-2011 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *           Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: 23-Oct-1993 21:37
 * Last file update: 19-Aug-2011 11:26 (eg)
 */

#include "stklos.h"
#include "object.h"
#include "struct.h"

DEFINE_PRIMITIVE("not", not, subr1, (SCM x))
/*
<doc not
 * (not obj)
 *
 * Not returns |#t| if |obj| is false, and returns |#f| otherwise.
 *
 * @lisp
 *   (not #t)         =>  #f
 *   (not 3)          =>  #f
 *   (not (list 3))   =>  #f
 *   (not #f)         =>  #t
 *   (not '())        =>  #f
 *   (not (list))     =>  #f
 *   (not 'nil)       =>  #f
 * @end lisp
doc>
 */
{
  return MAKE_BOOLEAN(x==STk_false);
}

DEFINE_PRIMITIVE("boolean?", booleanp, subr1, (SCM x))
/*
<doc  boolean?
 * (boolean? obj)
 *
 * |Boolean?| returns |#t| if |obj| is either |#t| or |#f| and returns
 * |#f| otherwise.
 * @lisp
 *   (boolean? #f)         =>  #t
 *   (boolean? 0)          =>  #f
 *   (boolean? '())        =>  #f
 * @end lisp
doc>
 */
{
  return MAKE_BOOLEAN(BOOLEANP(x));
}



/*
<doc  eqv?
 * (eqv? obj1 obj2)
 *
 * The |eqv?| procedure defines a useful equivalence relation on objects.
 * Briefly, it returns |#t| if |obj1| and |obj2| should normally be regarded
 * as the same object. This relation is left slightly open to interpretation,
 * but the following partial specification of |eqv?| holds for all
 * implementations of Scheme.
 *
 * The |eqv?| procedure returns |#t| if:
 * ,(itemize
 * (item [
 * |obj1| and |obj2| are both |#t| or both |#f|.
 * ])
 *
 * (item [
 * |obj1| and |obj2| are both symbols and
 * @lisp
 * (string=? (symbol->string obj1)
 *           (symbol->string obj2))
 *                      =>  #t
 * @end lisp
 *
 * ,(bold "Note:") This assumes that neither |obj1| nor |obj2| is an
 * "uninterned symbol".
 * ])
 *
 * (item [
 * |obj1| and |obj2| are both keywords and
 * @lisp
 * (string=? (keyword->string obj1)
 *           (keyword->string obj2))
 *                      =>  #t
 * @end lisp
 * ])
 *
 * (item [
 * |obj1| and |obj2| are both numbers, are numerically equal
 * (see ,(ref :mark "=")), and are either both exact or both inexact.
 * ])
 *
 * (item [
 * |obj1| and |obj2| are both characters and are the same character
 * according to the |char=?| procedure (see ,(ref :mark "char=?")).
 * ])
 *
 * (item [
 * both |obj1| and |obj2| are the empty list.
 * ])
 *
 * (item [
 * |obj1| and |obj2| are pairs, vectors, or strings that denote
 * the same locations in the store.
 * ])
 *
 * (item [
 * |obj1| and |obj2| are procedures whose location tags are equal.
 * ])
 * )
 *
 * ,(bold "Note:") STklos extends R5RS |eqv?| to take into account
 * the keyword type.
 * ,(linebreak)
 * Here are some examples:
 * @lisp
 * (eqv? 'a 'a)                     =>  #t
 * (eqv? 'a 'b)                     =>  #f
 * (eqv? 2 2)                       =>  #t
 * (eqv? :foo :foo)                 =>  #t
 * (eqv? :foo :bar)                 =>  #f
 * (eqv? '() '())                   =>  #t
 * (eqv? 100000000 100000000)       =>  #t
 * (eqv? (cons 1 2) (cons 1 2))     =>  #f
 * (eqv? (lambda () 1)
 *       (lambda () 2))             =>  #f
 * (eqv? #f 'nil)                   =>  #f
 * (let ((p (lambda (x) x)))
 *   (eqv? p p))                    =>  #t
 * @end lisp
 *
 * The following examples illustrate cases in which the above rules do
 * not fully specify the behavior of |eqv?|. All that can be said about
 * such cases is that the value returned by eqv? must be a boolean.
 * @lisp
 * (eqv? "" "")             =>  unspecified
 * (eqv? '#() '#())         =>  unspecified
 * (eqv? (lambda (x) x)
 *       (lambda (x) x))    =>  unspecified
 * (eqv? (lambda (x) x)
 *       (lambda (y) y))    =>  unspecified
 * @end lisp
 *
 * ,(bold "Note:") In fact, the value returned by STklos depends of
 * the way code is entered and can yield |#t| in some cases and |#f|
 * in others.
 * ,(linebreak)
 * See R5RS for more details on |eqv?|.
doc>
 */
DEFINE_PRIMITIVE("eqv?", eqv, subr2, (SCM x, SCM y))
{
  if (x == y) return STk_true;

  switch (STYPE(x)) {
    case tc_symbol:
      	if (SYMBOLP(y) && strcmp(SYMBOL_PNAME(x), SYMBOL_PNAME(y)) == 0)
	  return STk_true;
	break;

    case tc_real:
    case tc_bignum:
    case tc_complex:
    case tc_rational:
      if (NUMBERP(y)) {
	if (EXACTP(x) != EXACTP(y))
	  return STk_false;
	return MAKE_BOOLEAN(STk_numeq2(x, y));
      }
      break;
    case tc_instance:
      if (STk_oo_initialized) {
	SCM fg, res;

	fg = STk_lookup(STk_intern("object-eqv?"), STk_current_module(),
			&res, FALSE);
	res = STk_C_apply(fg, 2, x, y);
	return res;
      }
      break;
    case tc_pointer:
      if (CPOINTERP(y) && (CPOINTER_VALUE(x) == CPOINTER_VALUE(y)))
	return STk_true;
      break;
#ifdef FIXME
//EG:       default: if (EXTENDEDP(x) && EXTENDEDP(y) && TYPE(x) == TYPE(y))
//EG: 		  return STk_extended_compare(x, y, FALSE);
#endif
    default: break;
  }
  /* What can we do else? */
  return STk_false;
}



/*
<doc eq?
 * (eq? obj1 obj2)
 *
 * |Eq?| is similar to |eqv?| except that in some cases it is capable of
 * discerning distinctions finer than those detectable by |eqv?|.
 * ,(linebreak)
 * |Eq?| and |eqv?| are guaranteed to have the same behavior on symbols,
 * keywords, booleans, the empty list, pairs, procedures, and non-empty strings
 * and vectors. |Eq?|'s behavior on numbers and characters is
 * implementation-dependent, but it will always return either true or false,
 * and will return true only when |eqv?| would also return true.
 * |Eq?| may also behave differently from |eqv?| on empty vectors
 * and empty strings.
 * ,(linebreak)
 * ,(bold "Note:") STklos extends R5RS |eq?| to take into account
 * the keyword type.
 * ,(linebreak)
 * ,(bold "Note:") In STklos, comparison of character returns |#t| for identical
 * characters and |#f| otherwise.
 *
 * @lisp
 * (eq? 'a 'a)                     =>  #t
 * (eq? '(a) '(a))                 =>  unspecified
 * (eq? (list 'a) (list 'a))       =>  #f
 * (eq? "a" "a")                   =>  unspecified
 * (eq? "" "")                     =>  unspecified
 * (eq? :foo :foo)                 =>  #t
 * (eq? :foo :bar)                 =>  #f
 * (eq? '() '())                   =>  #t
 * (eq? 2 2)                       =>  unspecified
 * (eq? #\\A #\\A)                   =>  #t (unspecified in R5RS)
 * (eq? car car)                   =>  #t
 * (let ((n (+ 2 3)))
 *   (eq? n n))                    =>  #t (unspecified in R5RS)
 * (let ((x '(a)))
 *   (eq? x x))                    =>  #t
 * (let ((x '#()))
 *   (eq? x x))                    =>  #t
 * (let ((p (lambda (x) x)))
 *   (eq? p p))                    =>  #t
 * (eq? :foo :foo)                 =>  #t
 * (eq? :bar bar:)                 =>  #t
 * (eq? :bar :foo)                 =>  #f
 * @end lisp
 *
doc>
 */
DEFINE_PRIMITIVE("eq?", eq, subr2, (SCM x,SCM y))
{
  return MAKE_BOOLEAN(x == y);
}


/*
<doc  equal?
 * (equal? obj1 obj2)
 *
 * |Equal?| recursively compares the contents of pairs, vectors, and
 * strings, applying |eqv?| on other objects such as numbers and symbols.
 * A rule of thumb is that objects are generally |equal?| if they print the
 * same. |Equal?| may fail to terminate if its arguments are circular
 * data structures.
 * @lisp
 * (equal? 'a 'a)                  =>  #t
 * (equal? '(a) '(a))              =>  #t
 * (equal? '(a (b) c)
 *         '(a (b) c))             =>  #t
 * (equal? "abc" "abc")            =>  #t
 * (equal? 2 2)                    =>  #t
 * (equal? (make-vector 5 'a)
 *         (make-vector 5 'a))     =>  #t
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("equal?", equal, subr2, (SCM x, SCM y))
{
 Top:
  if (STk_eqv(x, y) == STk_true) return STk_true;

  switch (STYPE(x)) {
    case tc_cons:
      	if (CONSP(y)) {
	  if (STk_equal(CAR(x), CAR(y)) == STk_false) return STk_false;
	  x = CDR(x); y = CDR(y);
	  goto Top;
	}
	break;
    case tc_string:
      if (STRINGP(y))
	return STk_streq(x, y);
      	break;
    case tc_vector:
      if (VECTORP(y)) {
	long lx, ly, i;
	SCM *vx, *vy;

	lx = VECTOR_SIZE(x); ly = VECTOR_SIZE(y);
	if (lx == ly) {
	  vx = VECTOR_DATA(x);
	  vy = VECTOR_DATA(y);
	  for (i=0; i < lx;  i++) {
	    if (STk_equal(vx[i], vy[i]) == STk_false) return STk_false;
	  }
	  return STk_true;
	}
      }
      break;
    case tc_instance:
      if (STk_oo_initialized) {
	SCM fg, res;

	fg = STk_lookup(STk_intern("object-equal?"),STk_current_module(),
			&res,FALSE);
	res = STk_C_apply(fg, 2, x, y);
	return res;
      }
      break;
    case tc_struct:
      if (STRUCTP(y) && (STRUCT_TYPE(x) == STRUCT_TYPE(y)))
	return STk_equal(STk_struct2list(x), STk_struct2list(y));
      break;
    case tc_box:
      if (BOXP(y))
	return STk_equal(BOX_VALUE(x), BOX_VALUE(y));
      break;
    case tc_uvector:
      if (BOXED_TYPE_EQ(y, tc_uvector))
	return MAKE_BOOLEAN(STk_uvector_equal(x, y));
      break;
#ifdef FIXME
//EG:       default:
//EG: 		if (EXTENDEDP(x) && EXTENDEDP(y) && TYPE(x) == TYPE(y))
//EG: 		  return STk_extended_compare(x, y, TRUE);
#endif
    default: break;
  }
  return STk_false;
}

int STk_init_boolean(void)
{
  ADD_PRIMITIVE(not);
  ADD_PRIMITIVE(booleanp);
  ADD_PRIMITIVE(eq);
  ADD_PRIMITIVE(eqv);
  ADD_PRIMITIVE(equal);
  return TRUE;
}
