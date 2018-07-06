/*
 *
 * v e c t o r . c                      -- vectors management
 *
 * Copyright © 1993-2018 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: ??? 1993
 * Last file update:  6-Jul-2018 15:39 (eg)
 */

#include <string.h>
#include "stklos.h"

/*
 * Utilities
 *
 */

static void error_change_const_vector(SCM v)
{
  STk_error("changing the constant vector ~s is not allowed", v);
}

static void error_bad_vector(SCM v)
{
  STk_error("bad vector ~s", v);
}

static void error_bad_index(SCM index)
{
  STk_error("index ~S is invalid or out of bounds", index);
}

static void error_bad_length(SCM length)
{
  STk_error("invalid vector length ~s", length);
}

static void error_bad_list(SCM l)
{
  STk_error("bad list ~s", l);
}


static SCM control_index(int argc, SCM *argv, int *pstart, int *pend)
{
  SCM v = NULL;
  long len, start=0, end=-1;

  /* Controling number of arguments */
  switch (argc) {
  case 1: v = argv[0]; break;
  case 2: v = argv[0]; start = STk_integer_value(argv[-1]); break;
  case 3: v = argv[0]; start = STk_integer_value(argv[-1]);
          end = STk_integer_value(argv[-2]); break;
  default: STk_error("incorrect number of arguments (%d)", argc);
  }

  /* Controlling s */
  if (!VECTORP(v)) error_bad_vector(v);
  len = VECTOR_SIZE(v);

  /* Controlling start index */
  if (start == LONG_MIN || start < 0 || start > len)
    /* argc cannot be 1 (start would be 0) */
    STk_error("bad starting index ~S", argv[-1]);

  /* Controlling end index */
  if (end == -1)
    end = len;
  else
    if (end == LONG_MIN  || end < 0 || end > len)
      /* We have an end index ==> argc = 3 */
      STk_error("bad ending index ~S", argv[-2]);

  if (start > end)
    STk_error("low index is greater than high index");

  /* everything is correct, return values */
  *pstart = start;
  *pend   = end;
  return v;
}

static SCM r5rs_vector_copy(SCM vect)
{
  SCM z;
  int n;

  if (!VECTORP(vect)) error_bad_vector(vect);

  n = VECTOR_SIZE(vect);
  z = STk_makevect(n, (SCM) NULL);
  memcpy(VECTOR_DATA(z), VECTOR_DATA(vect), n * sizeof(SCM));
  return z;
}


SCM STk_makevect(int len, SCM init)
{
  register long i;
  SCM  z;

  NEWCELL_WITH_LEN(z, vector, sizeof(struct vector_obj) + (len-1)* sizeof(SCM));
  VECTOR_SIZE(z) = len;

  if (init) {
    register SCM *tmp = VECTOR_DATA(z);
    for(i=0; i < len; i++) *tmp++ = init;
  }

  return z;
}


/*===========================================================================*/

/*
<doc  vector?
 * (vector? obj)
 *
 * Returns |#t| if |obj| is a vector, otherwise returns |#f|.
doc>
 */
DEFINE_PRIMITIVE("vector?", vectorp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(VECTORP(obj));
}


/*
<doc  make-vector
 * (make-vector k)
 * (make-vector k fill)
 *
 * Returns a newly allocated vector of |k| elements. If a second argument is
 * given, then each element is initialized to |fill|. Otherwise the initial
 * contents of each element is unspecified.
doc>
 */
DEFINE_PRIMITIVE("make-vector", make_vector, subr12, (SCM len, SCM init))
{
  long l = STk_integer_value(len);

  if (l < 0) error_bad_length(len);
  return STk_makevect(l, init ? init : STk_void);
}


/*
<doc  vector
 * (vector obj ...)
 *
 * Returns a newly allocated vector whose elements contain the given arguments.
 * Analogous to |list|.
 *
 * @lisp
 * (vector 'a 'b 'c)               =>  #(a b c)
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("vector", vector, vsubr, (int argc, SCM *argv))
{
  SCM z = STk_makevect(argc, (SCM) NULL);
  SCM *p;

  p = VECTOR_DATA(z);
  while (argc--) *p++ = *argv--;

  return z;
}


/*
<doc  vector-length
 * (vector-length vector)
 *
 * Returns the number of elements in |vector| as an exact integer.
doc>
 */
DEFINE_PRIMITIVE("vector-length", vector_length, subr1, (SCM v))
{
  if (!VECTORP(v)) error_bad_vector(v);

  return MAKE_INT(VECTOR_SIZE(v));
}


/*
<doc  vector-ref
 * (vector-ref vector k)
 *
 * |k| must be a valid index of |vector|. |Vector-ref| returns the contents of
 * element |k| of vector.
 * @lisp
 * (vector-ref '#(1 1 2 3 5 8 13 21)
 *             5)         =>  8
 * (vector-ref '#(1 1 2 3 5 8 13 21)
 *             (let ((i (round (* 2 (acos -1)))))
 *               (if (inexact? i)
 *                   (inexact->exact i)
 *                   i))) => 13
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("vector-ref", vector_ref, subr2, (SCM v, SCM index))
{
  long i;

  if (!VECTORP(v)) error_bad_vector(v);

  i = STk_integer_value(index);
  if (i < 0 || i >= VECTOR_SIZE(v)) error_bad_index(index);
  return VECTOR_DATA(v)[i];
}



/*
<doc  vector-set!
 * (vector-set! vector k obj)
 *
 * |k| must be a valid index of |vector|. |Vector-set!| stores |obj| in element
 * |k| of |vector|. The value returned by |vector-set!| is ,(emph "void").
 *
 * @lisp
 * (let ((vec (vector 0 '(2 2 2 2) "Anna")))
 *   (vector-set! vec 1 '("Sue" "Sue"))
 *   vec)      =>  #(0 ("Sue" "Sue") "Anna")
 *
 * (vector-set! '#(0 1 2) 1 "doe")  =>  error  ; constant vector
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("vector-set!", vector_set, subr3, (SCM v, SCM index, SCM value))
{
  long i = STk_integer_value(index);

  if (!VECTORP(v))                  error_bad_vector(v);
  if (BOXED_INFO(v) & VECTOR_CONST) error_change_const_vector(v);

  if (i < 0 || i >= VECTOR_SIZE(v)) error_bad_index(index);
  VECTOR_DATA(v)[i] = value;
  return STk_void;
}


/*
<doc R57RS vector->list list->vector
 * (vector->list vector)
 * (vector->list vector start)
 * (vector->list vector start end)
 * (list->vector list)
 *
 * |Vector->list| returns a newly allocated list of the objects contained in
 * the elements of |vector| between start an end. |List->vector| returns a
 * newly created vector initialized to the elements of the list |list|.
 *
 * In both procedures, order is preserved.
 *
 * @lisp
 * (vector->list '#(dah dah didah))     =>  (dah dah didah)
 * (vector->list '#(dah dah didah) 1 2) =>  (dah)
 * (list->vector '(dididit dah))        =>  #(dididit dah)
 * @end lisp
 *
 *,@("Note"): The R5RS version of |vector->list| accepts only one
 * parameter.
doc>
 */

/* Following version implements only the R5RS version of vector->list (1 arg only) */
DEFINE_PRIMITIVE("vector->list", vector2list, subr1, (SCM v))
{
  int j, len;
  SCM z, tmp;

  if (!VECTORP(v)) error_bad_vector(v);

  len = VECTOR_SIZE(v);
  if (!len) return STk_nil;

  /* len > 0. Build the fist cell and iterate */
  tmp = z = STk_cons(*VECTOR_DATA(v), STk_nil);
  for (j=1; j<len; j++) {
    tmp = CDR(tmp) = STk_cons(VECTOR_DATA(v)[j], STk_nil);
  }
  return z;
}


DEFINE_PRIMITIVE("list->vector", list2vector, subr1, (SCM l))
{
  long len;
  register long i;
  register SCM z;

  if ((len = STk_int_length(l)) < 0) error_bad_list(l);

  z = STk_makevect(len, (SCM) NULL);
  for (i = 0; i < len; i++) {
    VECTOR_DATA(z)[i] = CAR(l);
    l = CDR(l);
  }
  return z;
}



/*
<doc R57RS vector-copy
 * (vector-copy v)
 * (vector-copy string start)
 * (vector-copy string start stop)
 *
 * Return a newly allocated copy of the elements of the given
 * vector between |start| and |end| . The elements of the new
 * vector are the same (in the sense of eqv?) as the elements
 * of the old.
 *
 * Note that, if |v| is a constant vector, its copy is not constant.
 *
 * @lisp
 * (define a #(1 8 2 8))         ; a is immutable
 * (define b (vector-copy a))    ; b is mutable
 * (vector-set! b 0 3)
 * b                            => #(3 8 2 8)
 * (define c (vector-copy b 1 3))
 * c                            => #(8 2)
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("vector-copy", vector_copy, vsubr, (int argc, SCM *argv))
{
  int start, end, n;
  SCM z, vect;

  vect = control_index(argc, argv, &start, &end);
  n = end-start;
  z = STk_makevect(n, (SCM) NULL);
  memcpy(VECTOR_DATA(z), VECTOR_DATA(vect)+start, n * sizeof(SCM));
  return z;
}


/*
<doc  vector-append
 * (vector-append vector ...)
 *
 * Returns a newly allocated vector whose elements are the
 * concatenation of the elements of the given vectors.
 *
 * @lisp
 * (vector-append #(a b c) #(d e f)) => #(a b c d e f)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("vector-append", vector_append, vsubr, (int argc, SCM *argv))
{
  int i, len = 0, start = 0;
  SCM z;

  /* compute length of final result */
  for (i = 0; i < argc; i++) {
    if (!VECTORP(argv[-i])) error_bad_vector(argv[-i]);
    len += VECTOR_SIZE(argv[-i]);
  }

  /* copy vectors */
  z = STk_makevect(len, (SCM) NULL);

  for (i = 0; i < argc; i++) {
    int sz = VECTOR_SIZE(argv[-i]);
    memcpy(VECTOR_DATA(z+start), VECTOR_DATA(argv[-i]), sz * sizeof(SCM));
    start += sz *sizeof(SCM);
  }
  return z;
}

/*
<doc  vector-fill!
 * (vector-fill! vector fill)
 *
 * Stores |fill| in every element of |vector|. The value returned by
 * |vector-fill!| is ,(emph "void").
doc>
 */
DEFINE_PRIMITIVE("vector-fill!", vector_fill, subr2, (SCM v, SCM fill))
{
  int j, len;
  SCM *p;

  if (!VECTORP(v))                  error_bad_vector(v);
  if (BOXED_INFO(v) & VECTOR_CONST) error_change_const_vector(v);

  for (j=0, len=VECTOR_SIZE(v), p=VECTOR_DATA(v); j < len; j++)
    *p++ = fill;

  return STk_void;
}


/*
 *
 * STk bonus
 *
 */


/*
<doc EXT vector-resize
 * (vector-resize v size)
 * (vector-resize v size fill)
 *
 * Returns a copy of v of the given |size|. If |size| is greater
 * than the vector size of |v|, the contents of the newly allocated vector cells
 * is  set to the value of |fill|. If |fill| is omitted the content of the
 * new cells is ,(emph "void").
doc>
 */
DEFINE_PRIMITIVE("vector-resize", vector_resize, subr23,(SCM vect,SCM size,SCM val))
{
  long old_size, new_size = STk_integer_value(size);
  SCM new, *p1, *p2;
  int i;

  if (!VECTORP(vect))                  error_bad_vector(vect);
  if (new_size<0)                      STk_error("bad new size ~S", size);

  old_size = VECTOR_SIZE(vect);
  new      = STk_makevect(new_size, (SCM) NULL);
  p1       = VECTOR_DATA(new);
  p2       = VECTOR_DATA(vect);

  /* Copy the elements of the old vector in the new one */
  if (new_size < old_size) {
    for (i=0; i < new_size; i++) *p1++ = *p2++;
  } else {
    if (!val) val = STk_void;

    for (i=0; i < old_size; i++) *p1++ = *p2++;
    for (   ; i < new_size; i++) *p1++ = val;
  }
  return new;
}
/*
<doc EXT vector-mutable?
 * (vector-mutable? obj)
 *
 * Returns |#t| if |obj| is a mutable vector, otherwise returns |#f|.
 * @lisp
 * (vector-mutable? '#(1 2 a b))            => #f
 * (vector-mutable? (vector-copy '#(1 2)))  => #t
 * (vector-mutable? (vector 1 2 3))         => #t
 * (vector-mutable? 12)                     => #f
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("vector-mutable?", vector_mutable, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(VECTORP(obj) && !(BOXED_INFO(obj) & VECTOR_CONST));
}


/*
<doc EXT sort
 * (sort obj predicate)
 *
 * |Obj| must be a list or a vector. |Sort| returns a copy of |obj| sorted
 * according to |predicate|. |Predicate| must be a procedure which takes
 * two arguments and returns a true value if the first argument is strictly
 * ``before'' the second.
 *
 * @lisp
 * (sort '(1 2 -4 12 9 -1 2 3) <)
 *                => (-4 -1 1 2 2 3 9 12)
 * (sort '#("one" "two" "three" "four")
 *       (lambda (x y) (> (string-length x) (string-length y))))
 *                => '#("three" "four" "one" "two")
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("sort", sort, subr2, (SCM obj, SCM test))
{
  SCM *v;
  register int i, j, incr, n;
  int list = 0;

  if (NULLP(obj))        { return STk_nil;                           }
  else if (CONSP(obj))   { obj  = STk_list2vector(obj); list = 1;    }
  else if (VECTORP(obj)) { obj  = r5rs_vector_copy(obj);             }
  else                   { STk_error("bad object to sort: ~S", obj); }

  /*
   * Use a shell sort. It has good performances on small arrays
   * This sort should have better performances than a cleverer one
   * for the sorts we'll have to do in practice (which are often small
   * arrays).
   */
  v    = VECTOR_DATA(obj);
  n    = VECTOR_SIZE(obj);

  for (incr = n / 2; incr; incr /= 2) {
    for (i = incr; i < n; i++) {
      for (j = i-incr; j >= 0; j -= incr) {
        if (STk_C_apply(test, 2, v[j], v[j+incr]) != STk_false)
          break;
        else {
          SCM tmp   = v[j+incr];
          v[j+incr] = v[j];
          v[j]      = tmp;
        }
      }
    }
  }
  return list ? STk_vector2list(obj) : obj;
}


/*===========================================================================*\
 *
 *  Vector extended type definition
 *
\*===========================================================================*/

static void print_vector(SCM vect, SCM port, int mode)
{
  int i, n  = VECTOR_SIZE(vect);
  SCM *tmp = VECTOR_DATA(vect);

  STk_nputs(port, "#(", 2);
  for (i = 0; i < n; i++) {
    STk_print(*tmp++, port, mode);
    if (i < n - 1) STk_putc(' ', port);
  }
  STk_putc(')', port);
}


static struct extended_type_descr xtype_vector = {
  "vector",
  print_vector
};



int STk_init_vector(void)
{
  DEFINE_XTYPE(vector, &xtype_vector);

  ADD_PRIMITIVE(vectorp);
  ADD_PRIMITIVE(make_vector);
  ADD_PRIMITIVE(vector);
  ADD_PRIMITIVE(vector_length);
  ADD_PRIMITIVE(vector_ref);
  ADD_PRIMITIVE(vector_set);
  ADD_PRIMITIVE(vector2list);
  ADD_PRIMITIVE(list2vector);
  ADD_PRIMITIVE(vector_append);
  ADD_PRIMITIVE(vector_fill);

  ADD_PRIMITIVE(vector_copy);
  ADD_PRIMITIVE(vector_resize);
  ADD_PRIMITIVE(vector_mutable);
  ADD_PRIMITIVE(sort);

  return TRUE;
}
