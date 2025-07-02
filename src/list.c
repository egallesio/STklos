/*
 *
 * l i s t . c                  -- Lists procedures
 *
 * Copyright © 1993-2025 Erick Gallesio <eg@stklos.net>
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
 *    Creation date: ??-Oct-1993 21:37
 */

#include "stklos.h"
#include "vm.h"      // FIXME:
/*===========================================================================*\
 *
 *                              Utilities
 *
\*===========================================================================*/

static void error_wrong_type(SCM x)
{
  STk_error("wrong type of argument ~S", x);
}

static void error_const_cell(SCM x)
{
  STk_error("changing the constant ~s is not allowed", x);
}

static void error_bad_list(SCM x)
{
  STk_error("bad list ~W", x);
}

static void error_improper_list(SCM x)
{
  STk_error("improper list ~W", x);
}

static void error_bad_proc(SCM x)
{
  STk_error("bad procedure ~S", x);
}

static void error_circular_list(SCM x)
{
  STk_error("list ~W is circular", x);
}

static void error_too_short(SCM x)
{
  STk_error("list ~W too short", x);
}

static void error_not_exact_positive(SCM x)
{
  STk_error("index ~W is not an exact positive integer", x);
}
static void error_bad_comparison_function(SCM x)
{
  STk_error("bad comparison function ~S", x);
}

int STk_int_length(SCM l)
{
  register SCM start = l;
  register int len   = 0;

  for ( ; ; ) {
    if (NULLP(l)) return len;
    if ((l == start && len) || !CONSP(l)) return -1;
    l = CDR(l);
    len += 1;
  }
}


static SCM simple_list_copy(SCM l, int len)
{
  if (NULLP(l)) return STk_nil;
  else {
    SCM res = STk_C_make_list(len, STk_void);
    SCM ptr_to = res;
    SCM ptr_from = l;

    while (--len) {
      CAR(ptr_to) = CAR(ptr_from);
      ptr_from = CDR(ptr_from);
      ptr_to   = CDR(ptr_to);
    }

    /* last pair of the result */
    CAR(ptr_to) = CAR(ptr_from);
    CDR(ptr_to) = STk_nil;

    return res;
  }
}

/* STk_C_make_list
 *
 * Allocate a list of n cons cells, all of them initialized with the value of init.
 *
 * We use here the (weird) function STk_must_malloc_many. This function
 * returns a list of objects, linked through their first word. It is faster
 * than n successive calls to STk_must_malloc, since the allocation lock can
 * be acquired and released many fewer times.  Note that STk_must_malloc_many
 * returns an unknown (small) number of objects If this number is less than n,
 * we call another time STk_must_malloc_many. If it is bigger, the unused
 * objects will be collected back later by the GC (if we do nothing). To avoid
 * to "abandon" unused objects when the function returns, we keep the (already
 * allocated, but unused, cells, in the `pool` variable. When the function
 * is entered again, it will be used, in place of a call to the function
 * `STk_must_malloc_many`. Hence, this reduces tremendously the work of the GC.
 */

SCM STk_C_make_list(int n, SCM init)
{
  if (!n)
    return STk_nil;
  else {
    vm_thread_t *vm = STk_get_current_vm();
    register size_t index = sizeof(struct cons_obj) / sizeof(SCM);
    SCM ptr, start, next = STk_nil;
    SCM pool = vm->cell_pool[index];

    // Initialize start from a previous call to STk_C_make_list, if possible.
    // Otherwise call the special GC function
    ptr = start = pool? pool: STk_must_malloc_many(sizeof(struct cons_obj));

    for (int i=0; i < n; i++) {
      if (!GC_NEXT(ptr)) {
        // Enlarge current list chunk by allocatting some new pairs
        GC_NEXT(ptr) = STk_must_malloc_many(sizeof(struct cons_obj));
      }
      next = GC_NEXT(ptr);
      BOXED_TYPE((struct cons_obj* ) ptr) = tc_cons;
      BOXED_INFO((struct cons_obj* ) ptr) = 0;
      CAR((struct cons_obj* ) ptr) = init;
      CDR((struct cons_obj* ) ptr) = (i < n-1) ? next: STk_nil ;
      ptr = next;
    }

    vm->cell_pool[index] = next;

    if (STk_count_allocations)
      STk_vm_inc_allocs(vm, n * sizeof(struct cons_obj));
    return start;
  }
}



/* list_type_and_length():
 *
 * List checking (proper improper, cyclic) AND length computing.
 *
 * If l is:
 *    1. PROPER LIST: return STk_nil.
 *    2. CYCLIC LIST: A CONS cell (this is where the cycle starts).
 *    3. FINITE IMPROPER LIST: The last CDR.
 *    4. NOT A LIST: NULL.
 *
 *   - In cases (1), (2), and (3) the length of the list is returned
 *     in the len argument.
 *   - If the list consists of a single cycle, then we guarantee that
 *    the pointer returned is to the FIRST cell of the list.
 */
static SCM list_type_and_length(SCM l, int *len)
{
  //     About the cycle detecting and copying algorithm:
  //
  //     1. Detect cycles using Floyd's algorithm.  The algorithm runs two
  //        pointers, "fast" and "slow" through the list. Both are placed
  //        at the CAR, then at each step, slow goes forward one link, and
  //        fast goes forward two links.
  //
  //     2. If there is a cycle:
  //
  //        2.i) we position fast back at the CAR and now move the two
  //             pointers *one* link at each time. The pointers will
  //             *necessarily meet* exactly *at the beginning of the cycle.
  //
  //        2.ii) If there is a cycle, we mark the cell in which it starts
  //              example, the cycle below starts at C:
  //
  //                         +--------------+
  //                         |              |
  //                         v              |
  //               A -> B -> C -> D -> E -> F
  //
  //               We keep an extra pointer to C.
  //
  //        2.iii) Now we count the number of cells from the beginning (A)
  //               to the SECOND time we see C (minus one). This is the
  //               "length" of the list (the size to be allocated to the
  //               copy).
  //
  //     3. If there is no cycle, count the number of elements as usual (but
  //        allowing for improper lists)

  *len = 0;
  if (NULLP(l)) return STk_nil; /* Case (1) in function description, specifically
                                   for the proper list '().*/
  if (!CONSP(l)) return NULL;  /* Case (4) in function description */

  /* 1. Detect possible cycles using Floyd's algorithm */
  SCM slow  = l;
  SCM fast  = l;
  int cycle = 0;
  SCM cycle_start;
  int single_cycle = 0;
  int allocs = 0;     // # of needed of cells (not significant if we have a cycle)

  while(CONSP(fast) && CONSP(CDR(fast))) {
    allocs += 2;
    slow = CDR(slow);
    fast = CDR(CDR(fast));
    if(slow == fast) {
      cycle = 1;
      break;
    }
  }

  if (cycle) {
    /* 2.i Put fast back in the beginning, and now move slow and
       fast one link at a time. When they meet, we found the
       cycle start. */
    fast = l;
    while(CDR(slow)!=CDR(fast)){
      slow = CDR(slow);
      fast = CDR(fast);
    }

    /* 2.ii Keep a link to the cell where the cycle starts */

    /* Special case: if slow == fast, then the list is a single cycle,
       and we'd be counting one more cons cell than necesary. */
    if (slow == fast)
      single_cycle = 1;

    cycle_start = CDR(slow);

    /* 2.iii Count the cyclic list size. */
    slow = l;
    int passed_cycle = 0;
    while (1) {
      (*len)++;
      if (CDR(slow) == cycle_start) {
        if (passed_cycle) break;
        else              passed_cycle = 1;
      }
      slow = CDR(slow);
    }

    /* LEN now has the length of the list to be copied, and we only need to
       return CDR(slow), which will be what we promised (NIL for proper lists,
       cycle_start for cyclic lists, or the last-cdr for improper lists.
       Except that -- if it's a single cycle, we don't want to return the
       (random) place where slow and fast met. We'll return the start ot the
       list! */
    return (single_cycle) ?  l : CDR(slow);
  } else {
    /* Not a cycle.
       If L is a proper list, FAST is the last pair of L or '() depending
       of the parity of list's length.
       If L is an improper list, FAST is a dotted pair of an atom depending
       of the parity of list's length.
    */
    if (CONSP(fast)) allocs++;
    (*len) = allocs;
    return (CONSP(fast) ? CDR(fast): fast);
  }
}


SCM STk_argv2list(int argc, SCM *argv)
{
  SCM res = STk_nil;

  while (argc--) {
    res = STk_cons(argv[-argc], res);
  }
  return res;
}


DEFINE_PRIMITIVE("pair?", pairp, subr1, (SCM x))
/*
<doc pair?
 * (pair? obj)
 *
 * |Pair?| returns |#t| if |obj| is a pair, and otherwise returns |#f|.
doc>
 */
{
  return CONSP(x) ? STk_true : STk_false;
}


DEFINE_PRIMITIVE("cons", cons, subr2, (SCM x, SCM y))
/*
<doc cons
 * (cons obj1 obj2)
 *
 * Returns a newly allocated pair whose car is obj1 and whose cdr is obj2.
 * The pair is guaranteed to be different (in the sense of eqv?) from every
 * existing object.
 * @lisp
 *     (cons 'a '())           =>  (a)
 *     (cons '(a) '(b c d))    =>  ((a) b c d)
 *     (cons "a" '(b c))       =>  ("a" b c)
 *     (cons 'a 3)             =>  (a . 3)
 *     (cons '(a b) 'c)        =>  ((a b) . c)
 * @end lisp
doc>
 */
{
  SCM z;

  NEWCELL(z, cons);
  CAR(z) = x;
  CDR(z) = y;
  return z;
}


DEFINE_PRIMITIVE("car", car, subr1, (SCM x))
/*
<doc car
 * (car pair)
 *
 * Returns the contents of the car field of pair.
 * Note that it is an error to take the |car| of the empty list.
 * @lisp
 *     (car '(a b c))          =>  a
 *     (car '((a) b c d))      =>  (a)
 *     (car '(1 . 2))          =>  1
 *     (car '())               =>  error
 * @end lisp
doc>
 */
{
  if (CONSP(x)) return CAR(x);
  error_wrong_type(x);
  return STk_void; /* never reached */
}


DEFINE_PRIMITIVE("cdr", cdr, subr1, (SCM x))
/*
<doc cdr
 * (cdr pair)
 *
 * Returns the contents of the cdr field of pair.
 * Note that it is an error to take the |cdr| of the empty list.
 * @lisp
 *     (cdr '((a) b c d))      =>  (b c d)
 *     (cdr '(1 . 2))          =>  2
 *     (cdr '())               =>  error
 * @end lisp
doc>
 */
{
  if (CONSP(x)) return CDR(x);
  error_wrong_type(x);
  return STk_void; /* never reached */
}


DEFINE_PRIMITIVE("set-car!", setcar, subr2, (SCM cell, SCM value))
/*
<doc set-car!
 * (set-car! pair obj)
 *
 * Stores |obj| in the car field of |pair|.
 * The value returned by |set-car!| is *_void_*.
 * @lisp
 *    (define (f) (list 'not-a-constant-list))
 *    (define (g) '(constant-list))
 *    (set-car! (f) 3)
 *    (set-car! (g) 3)             =>  error
 * @end lisp
doc>
 */
{
  if (!CONSP(cell))                  error_wrong_type(cell);
  if (BOXED_INFO(cell) & CONS_CONST) error_const_cell(cell);

  CAR(cell) = value;
  return STk_void;
}


DEFINE_PRIMITIVE("set-cdr!", setcdr, subr2, (SCM cell, SCM value))
/*
<doc set-cdr!
 * (set-cdr! pair obj)
 *
 * Stores |obj| in the cdr field of |pair|.
 * The value returned by |set-cdr!| is *_void_*.
 *
doc>
 */
{
  if (!CONSP(cell)) error_wrong_type(cell);
  if (BOXED_INFO(cell) & CONS_CONST) error_const_cell(cell);

  CDR(cell) = value;
  return STk_void;
}

DEFINE_PRIMITIVE("%cxr", cxr, subr2, (SCM l, SCM name))
{
  /* Special function to compute cadr, cddr, ...
   * (%cxr lst #:dda) returns the caddr of lst
   * The key given indicates the value of x (reversed)
   * Using the non reversed value of x is simpler but incurs
   * a certain time penalty.
   * In case of error, we can display a clear message (with some work to
   * rebuild the original function name), but we have time.
   * NOTE: using strings (instead of keywords) is less efficient because
   * the char * is at the end of the object. Using symbols is also fast
   * (even a bit faster, don't know why), but it is harder  to detect that
   * that we can inline when we have (%cxr lst 'daa), because of the quote.
   */
  if (KEYWORDP(name)) {
    SCM lst   = l;
    const char *str = KEYWORD_PNAME(name);

    for (const char *s = str;  *s; s++) {
      if (CONSP(lst))
        lst = (*s == 'a') ? CAR(lst): CDR(lst);
      else {
        int len   = strlen(str);
        char *loc = STk_must_malloc_atomic(len+3); // 'c' + X + 'r' + \0

        /* build location */
        loc[0] = 'c';
        for (int i = 0; i < len; i++) loc[i+1] = str[len-1-i];
        loc[len+1] = 'r';
        loc[len+2] = '\0';

        /* display clear error */
        name = STk_intern(loc);
        STk_error_with_location(name, "wrong type of argument ~S for c%cr in (~s '~w)",
                                lst, *s, name, l);
      }
    }
    return lst;
  }
  else {
    error_wrong_type(name);
    return STk_void; // to avoid a compiler warning
  }
}

DEFINE_PRIMITIVE("null?", nullp, subr1, (SCM x))
/*
<doc null?
 * (null? obj)
 *
 * Returns |#t| if |obj| is the empty list, otherwise returns |#f|.
doc>
 */
{
  return MAKE_BOOLEAN(x == STk_nil);
}


DEFINE_PRIMITIVE("list?", listp, subr1, (SCM x))
/*
<doc list?
 * (list? obj)
 *
 * Returns |#t| if |obj| is a list, otherwise returns |#f|. By definition,
 * all lists have finite length and are terminated by the empty list.
 * @lisp
 *    (list? '(a b c))     =>  #t
 *    (list? '())          =>  #t
 *    (list? '(a . b))     =>  #f
 *    (let ((x (list 'a)))
 *      (set-cdr! x x)
 *      (list? x))         =>  #f
 * @end lisp
doc>
 */
{
  int len;
  return MAKE_BOOLEAN(NULLP(list_type_and_length(x, &len)));
}

/*
<doc R7RS make-list
 * (make-list k)
 * (make-list k fill)
 *
 * Returns a newly allocated list of k elements. If a second
 * argument is given, then each element is initialized to fill .
 * Otherwise the initial contents of each element is unspecified.
doc>
*/
DEFINE_PRIMITIVE("make-list", make_list, subr12, (SCM n, SCM init)) {
  if (!INTP(n)) STk_error("bad integer ~s", n);
  if (!init) init = STk_void;
  return STk_C_make_list(INT_VAL(n), init);
}


DEFINE_PRIMITIVE("list", list, vsubr, (int argc, SCM * argv))
/*
<doc list
 * (list obj ...)
 *
 * Returns a newly allocated list of its arguments.
 * @lisp
 *    (list 'a (+ 3 4) 'c)            =>  (a 7 c)
 *    (list)                          =>  ()
 * @end lisp
doc>
 */
{
  register SCM *tmp, l = STk_nil;

  for (tmp = argv-argc+1; tmp <= argv; tmp++)
    l = STk_cons(*tmp, l);

  return l;
}

DEFINE_PRIMITIVE("length", list_length, subr1, (SCM l))
/*
<doc length
 * (length list)
 *
 * Returns the length of |list|.
 *
 * @lisp
 *    (length '(a b c))               =>  3
 *    (length '(a (b) (c d e)))       =>  3
 *    (length '())                    =>  0
 * @end lisp
doc>
 */
{
  if (NULLP(l)) return MAKE_INT(0);

  if (!CONSP(l))
    STk_error("bad list ~s", l);
  else {
    int len = STk_int_length(l);

    if (len >= 0) return MAKE_INT(len);
    STk_error("length of improper list ~W is not calculable", l);
  }
  return STk_void; /* never reached */
}


/*
<doc append
 * (append list ...)
 *
 * Returns a list consisting of the elements of the first list
 * followed by the elements of the other lists.
 *
 * @lisp
 *    (append '(x) '(y))              =>  (x y)
 *    (append '(a) '(b c d))          =>  (a b c d)
 *    (append '(a (b)) '((c)))        =>  (a (b) (c))
 * @end lisp
 *
 * The resulting list is always newly allocated, except that it shares
 * structure with the last list argument. The last argument may actually
 * be any object; an improper list results if the last argument is not a
 * proper list.
 *
 * @lisp
 *    (append '(a b) '(c . d))        =>  (a b c . d)
 *    (append '() 'a)                 =>  a
 * @end lisp
doc>
 */
SCM STk_append2(SCM l1, SCM l2)
{
  register SCM prev, tmp, l;
  SCM res;

  if (NULLP(l1)) return l2;
  if (!CONSP(l1)) goto Error;

  prev = res = STk_nil;
  for (l = l1; ; l = CDR(l)) {
    if (NULLP(l)) break;
    if (!CONSP(l)) goto Error;
    tmp = STk_cons(CAR(l), STk_nil);

    if (res == STk_nil) {
      prev = res = tmp;
    } else {
      CDR(prev) = tmp;
      prev = tmp;
    }
  }
  CDR(prev) = l2;
  return res;
Error:
  error_bad_list(l1);
  return STk_void; /* never reached */
}


DEFINE_PRIMITIVE("append", append, vsubr, (int argc, SCM* argv))
{
  switch (argc) {
    case 0:  return STk_nil;
    case 1:  return *argv;
    case 2:  return STk_append2(argv[0], argv[-1]);
    default: return STk_append2(argv[0], STk_append(argc-1, argv-1));
  }
}



/*
<doc reverse
 * (reverse list)
 *
 * Returns a newly allocated list consisting of the elements of |list| in
 * reverse order.
 *
 * @lisp
 *    (reverse '(a b c))              =>  (c b a)
 *    (reverse '(a (b c) d (e (f))))  =>  ((e (f)) d (b c) a)
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("reverse", reverse, subr1, (SCM l))
{
  int len;
  SCM x = list_type_and_length(l, &len);

  if (!x) error_bad_list(l);
  if (CONSP(x)) error_circular_list(l);
  if (!NULLP(x)) error_improper_list(l);

  /* WARNING: do not use STk_list_copy here. It will make STklos enter a loop
     in some situations, running out of stack space. */
  return STk_dreverse(simple_list_copy(l, len));
}

/*
<doc list-tail
 * (list-tail list k)
 *
 * Returns the sublist of |list| obtained by omitting the first |k| elements.
 * It is an error if list has fewer than |k| elements. List-tail could
 * be defined by
 * @lisp
 *    (define list-tail
 *       (lambda (x k)
 *          (if (zero? k)
 *             x
 *             (list-tail (cdr x) (- k 1)))))
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("list-tail", list_tail, subr2, (SCM list, SCM k))
{
  register long x;
  SCM l;

  if (!CONSP(list) && !NULLP(list)) error_bad_list(list);

  x = STk_integer_value(k);
  if (x >= 0) {
    for (l=list; x > 0; x--) {
      if (NULLP(l) || !CONSP(l)) error_too_short(list);
      l = CDR(l);
    }
    return l;
  }

  error_not_exact_positive(k);
  return STk_void; /* never reached */
}


/*
<doc list-ref
 * (list-ref list k)
 *
 * Returns the |k|th element of |list|. (This is the same as the car
 * of |(list-tail list k)|.) It is an error if list has fewer than |k|
 * elements.
 *
 * @lisp
 *    (list-ref '(a b c d) 2)                 =>  c
 *    (list-ref '(a b c d)
 *              (inexact->exact (round 1.8))) =>  c
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("list-ref", list_ref, subr2, (SCM list, SCM k))
{
  register long x;
  SCM l = list;

  if (!CONSP(list)) error_bad_list(list);

  x = STk_integer_value(k);
  if (x >= 0) {
    for ( ; x > 0; x--) {
      if (NULLP(l) || !CONSP(l)) goto Error;
      l = CDR(l);
    }
    if (CONSP(l)) return CAR(l);
  Error:
    error_too_short(list);
  }
  error_not_exact_positive(k);
  return STk_void; /* never reached */
}

/*
<doc R7RS list-set!
 * (list-set! list k obj)
 *
 * The |list-set!| procedure stores |obj| in element |k| of |list|.
 * It is an error if |k| is not a valid index of |list|.
 * @lisp
 * (let ((ls (list 'one 'two 'five!)))
 *    (list-set! ls 2 'three)
 *    ls)                              => (one two three)
 * (list-set! '(0 1 2) 1 "oops")       => error (constant list)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("list-set!", list_set, subr3, (SCM list, SCM k, SCM obj))
{
  register long x;
  SCM l = list;

  if (!CONSP(list)) error_bad_list(list);

  x = STk_integer_value(k);
  if (x >= 0) {
    for ( ; x > 0; x--) {
      if (NULLP(l) || !CONSP(l)) goto Error;
      l = CDR(l);
    }
    if (CONSP(l)) {
      if (BOXED_INFO(l) & CONS_CONST) error_const_cell(list);
      CAR(l) = obj;
      return STk_void;
    }
  Error:
    error_too_short(list);
  }
  error_not_exact_positive(k);
  return STk_void; /* never reached */
}


/*
<doc R57RS memq memv member
 * (memq obj list)
 * (memv obj list)
 * (member obj list)
 * (member obj list compare)
 *
 * These procedures return the first sublist of list whose car is |obj|,
 * where the sublists of list are the non-empty lists returned by
 * |(list-tail list k)| for |k| less than the length of list.
 * If |obj| does not occur in |list|, then |#f| (not the empty list) is
 * returned. |Memq| uses |eq?| to compare obj with the elements of list,
 * while |memv| uses |eqv?| and |member| uses |compare|, if given, and
 * |equal?| otherwise.
 *
 * @lisp
 *    (memq 'a '(a b c))              =>  (a b c)
 *    (memq 'b '(a b c))              =>  (b c)
 *    (memq 'a '(b c d))              =>  #f
 *    (memq (list 'a) '(b (a) c))     =>  #f
 *    (member (list 'a)
 *            '(b (a) c))             =>  ((a) c)
 *    (member "B"
 *            ’("a" "b" "c")
 *            string-ci=?)            => ("b" "c")
 *    (memv 101 '(100 101 102))       =>  (101 102)
 * @end lisp
 *
 * NOTE: As in R7RS, the |member| function accepts also a
 * comparison function.
doc>
 */

#define PTR_EQ(x, y)            ((x) == (y))
#define PTR_EQV(x, y)           (STk_eqv((x), (y)) != STk_false)
#define PTR_EQUAL(x, y)         (STk_equal((x), (y)) != STk_false)
#define PTR_CMPGEN(x, y)        (STk_C_apply(cmp, 2, (x), (y)) != STk_false)


#define LMEMBER(compare)                                        \
do{                                                             \
  register SCM ptr;                                             \
                                                                \
  if (!CONSP(list) && !NULLP(list)) error_bad_list(list);       \
                                                                \
  for (ptr=list; !NULLP(ptr); ) {                               \
    if (CONSP(ptr)) {                                           \
      if (compare(obj, CAR(ptr))) return ptr;                   \
    }                                                           \
    else                                                        \
      break; /* end of a dotted list */                         \
    if ((ptr=CDR(ptr)) == list) error_circular_list(ptr);       \
  }                                                             \
  return STk_false;                                             \
}while(0)


DEFINE_PRIMITIVE("memq", memq, subr2, (SCM obj, SCM list))
{
    LMEMBER(PTR_EQ);
}

DEFINE_PRIMITIVE("memv", memv, subr2, (SCM obj, SCM list))
{
    LMEMBER(PTR_EQV);
}

DEFINE_PRIMITIVE("member", member, subr23, (SCM obj, SCM list, SCM cmp))
{
  if (cmp) {
    if (STk_procedurep(cmp) != STk_true)
      error_bad_comparison_function(cmp);

    LMEMBER(PTR_CMPGEN);
  } else {
    LMEMBER(PTR_EQUAL);
  }
}


/*
<doc R57RS assq assv assoc
 * (assq obj alist)
 * (assv obj alist)
 * (assoc obj alist)
 * (assoc obj alist compare)
 *
 * |Alist| (for "association list") must be a list of pairs. These procedures
 * find the first pair in |alist| whose car field is |obj|, and returns that
 * pair. If no pair in |alist| has |obj| as its car, then |#f| (not the empty
 * list) is returned. |Assq| uses |eq?| to compare |obj| with the car fields
 * of the pairs in |alist|, while |assv| uses |eqv?| and |assoc| uses |equal?|.
 *
 * @lisp
 *    (define e '((a 1) (b 2) (c 3)))
 *    (assq 'a e)                =>  (a 1)
 *    (assq 'b e)                =>  (b 2)
 *    (assq 'd e)                =>  #f
 *    (assq (list 'a) '(((a)) ((b)) ((c))))
 *                               =>  #f
 *    (assoc (list 'a) '(((a)) ((b)) ((c))))
 *                               => ((a))
 *    (assoc 2.0 '((1 1) (2 4) (3 9)) =)
 *                               => (2 4)
 *    (assv 5 '((2 3) (5 7) (11 13)))
 *                               =>  (5 7)
 * @end lisp
 *
 * IMPORTANT: Although they are ordinarily used as predicates,
 * |memq|, |memv|, |member|, |assq|, |assv|, and |assoc| do not have question
 * marks in their names because they return useful values rather than just
 * |#t| or #|f|.
 *
 * NOTE: As in R7RS, the |assoc| function accepts also a
 * comparison function.
doc>
 */

#define LASSOC(compare)                                         \
do{                                                             \
  register SCM l,tmp;                                           \
                                                                \
  for(l=alist; CONSP(l); ) {                                    \
    tmp = CAR(l);                                               \
    if (CONSP(tmp) && compare(obj, CAR(tmp))) return tmp;       \
    if ((l=CDR(l)) == alist) break;                             \
  }                                                             \
  if (NULLP(l)) return(STk_false);                              \
  STk_error("improper list ~W", alist);                         \
  return STk_void; /* never reached */                          \
}while(0)

DEFINE_PRIMITIVE("assq", assq, subr2, (SCM obj, SCM alist))
{
  LASSOC(PTR_EQ);
}

DEFINE_PRIMITIVE("assv", assv, subr2, (SCM obj, SCM alist))
{
  LASSOC(PTR_EQV);
}

DEFINE_PRIMITIVE("assoc", assoc, subr23, (SCM obj, SCM alist, SCM cmp))
{
  if (cmp) {
    if (STk_procedurep(cmp) != STk_true)
      error_bad_comparison_function(cmp);

    LASSOC(PTR_CMPGEN);
 } else {
   LASSOC(PTR_EQUAL);
 }
}


/*
<doc circular-list?
 * (circular-list? obj)
 *
 * Returns |#t| if |obj| is a circular list, and |#f| otherwise.
 *
 * @lisp
 * (define L (list 1 2 3 4))
 * (set-cdr! (cdddr L) (cdr L))
 * (circular-list? L)               => #t
 * (circular-list? 10)              => #f
 * (circular-list? '#0=(a b . #0#) )=> #t
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("circular-list?", circular_listp, subr1, (SCM l))
{
  int len;
  SCM x = list_type_and_length(l, &len);

  return MAKE_BOOLEAN (x && CONSP(x));
}

/*
<doc EXT list-deep-copy list-copy
 * (list-copy obj)
 * (list-deep-copy obj)
 *
 * These procedures copy trees of pairs. |list-copy| will copy the
 * pairs, but wil not recurse into them for copying; but |list-deep-copy|
 * will recurse into the structure, so a new tree or association list will
 * be returned.
 *
 * |List-deep-copy| is particularly useful to copy association lists when
 * one needs the new list items to *not* be |eq?| to the old ones.
 *
 * Note that |list-deep-copy| will only recurse into lists, not vectors,
 * structs and other data structures.
 *
 * @lisp
 * (define X (cons 1 2))
 * (define y (list X 10))
 * (eq? y (list-copy y))                  => #f
 * (eq? (car y) (car (list-copy y)))      => #t
 * (eq? (car y) (car (list-deep-copy y))) => #f
 *
 * (define X '#(1 2))
 * (define y (list X 10))
 * (eq? y (list-copy y))                  => #f
 * (eq? (car y) (car (list-copy y)))      => #t
 * (eq? (car y) (car (list-deep-copy y))) => #t ; <= didn't recurse into vector
 * @end lisp
 *
 * NOTE: {{rseven}} defines the |list-copy| primitive, but it states that calling
 * it on a circular list is an error. This is not the case in {{stklos}}.
doc>
 */
static SCM list_copy(SCM l, int deep)
{
  if (!CONSP(l)) return l;     // non list values and '() too

  int len;
  SCM last = list_type_and_length(l, &len);
  SCM cycle_start = last;
  int cycle = CONSP(last);

  /*
    1. Allocate the new list.

    2. Copy the elements from the old to the new list.

    3. Adjust the last CDR:

       3.i) If the last CDR of the old list is NIL (proper list), or if
            it is NOT a CONS pair (list ending with non-nil, but still
            finite), make the last CDR the same.

       3.ii) If there was a cycle, then make the last CDR of the new
             list point to the marked cell.
  */

    /* 1. Allocate the list and copy the CARs */
    SCM ptr, ptr_res, res = STk_C_make_list(len, STk_void);
    SCM cycle_start_in_new_list = STk_nil;
    ptr = l;
    ptr_res = res;

    if (cycle && l == cycle_start) len--;

    /* 2. Copy. */
    for (int ctr = 0; ctr < len-1; ctr++) {
      CAR(ptr_res) = (deep) ? list_copy(CAR(ptr), 1) : CAR(ptr);
      /* Mark where the cycle start is in the copy: */
      if (cycle && ptr == cycle_start)
        cycle_start_in_new_list = ptr_res;

      ptr_res = CDR(ptr_res);
      ptr = CDR(ptr);
    }

    /* 3. Adjust last CDR: */
    CAR(ptr_res) = (deep) ? list_copy(CAR(ptr), 1) : CAR(ptr);

    if (!cycle && (CDR(ptr) == STk_nil || /* No cycle */
                   !CONSP(CDR(ptr))))     /* Improper list */
      CDR(ptr_res) = CDR(ptr);

    else if (cycle && l == cycle_start) /* Cycle */
      CDR(ptr_res) = res;

    else if (cycle && CDR(ptr) == cycle_start) /* Cycle */
      CDR(ptr_res) = cycle_start_in_new_list;

    else {
      STk_error("impossible error"); /* ??? */
    }
    return res;
}


DEFINE_PRIMITIVE("list-copy", list_copy, subr1, (SCM l))
{
  return list_copy(l,0);
}


DEFINE_PRIMITIVE("list-deep-copy", list_deep_copy, subr1, (SCM l))
{
  return list_copy(l,1);
}


/***
 *
 * Non standard functions
 *
 ***/


/*
<doc EXT pair-mutable?
 * (pair-mutable? obj)
 *
 * Returns |#t| if |obj| is a mutable pair, otherwise returns |#f|.
 * @lisp
 * (pair-mutable? '(1 . 2))    => #f
 * (pair-mutable? (cons 1 2))  => #t
 * (pair-mutable? 12)          => #f
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("pair-mutable?", pair_mutable, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(CONSP(obj) && !(BOXED_INFO(obj) & CONS_CONST));
}


/*
<doc EXT list*
 * (list* obj ...)
 *
 * |list*| is like |list| except that the last argument to |list*| is
 * used as the ,(emph "cdr") of the last pair constructed.
 * @lisp
 *    (list* 1 2 3)        => (1 2 . 3)
 *    (list* 1 2 3 '(4 5)) => (1 2 3 4 5)
 *    (list*)              => ()
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("list*", list_star, vsubr, (int argc, SCM *argv))
{
  register SCM *tmp, l;

  if (argc == 0) return STk_nil;

  tmp = argv-argc+1;
  l   = *tmp;

  for (++tmp; tmp <= argv; tmp++)
    l = STk_cons(*tmp, l);

  return l;
}



/*
<doc EXT last-pair
 * (last-pair list)
 *
 * Returns the last pair of |list|.
 * @lisp
 * (last-pair '(1 2 3))   => (3)
 * (last-pair '(1 2 . 3)) => (2 . 3)
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("last-pair", last_pair, subr1, (SCM l))
{
  int len;
  SCM x = list_type_and_length(l, &len);

  if (!x || !len) error_bad_list(l);
  if (CONSP(x)) error_circular_list(l);

  while(--len)
    l = CDR(l);

  return l;
}

/*
<doc EXT filter filter!
 * (filter  pred list)
 * (filter! pred list)
 *
 * |Filter| returns all the elements of |list| that satisfy predicate
 * |pred|. The |list| is not disordered: elements that appear in the
 * result list occur in the same order as they occur in the argument
 * list. |Filter!| does the same job as |filter| by physically
 * modifying its |list| argument
 * @lisp
 * (filter even? '(0 7 8 8 43 -4)) => (0 8 8 -4)
 * (let* ((l1 (list 0 7 8 8 43 -4))
 *        (l2 (filter! even? l1)))
 *    (list l1 l2))                => ((0 8 8 -4) (0 8 8 -4))
 * @end lisp
 * An error is signaled if |list| is a constant list.
doc>
 */
DEFINE_PRIMITIVE("filter", filter, subr2, (SCM pred, SCM list))
{
  register SCM ptr, l;
  SCM result;
  int len;
  SCM x = list_type_and_length(list, &len);

  if (!x) error_bad_list(list);
  if (CONSP(x)) error_circular_list(list);
  if (!NULLP(x)) error_improper_list(list);

  if (STk_procedurep(pred) != STk_true) error_bad_proc(pred);

  for (ptr=l=list, result=STk_nil; !NULLP(l); ) {

    if (STk_C_apply(pred, 1, CAR(l)) != STk_false) {
      if (NULLP(result)) {
        NEWCELL(result, cons);
        ptr = result;
      }
      else {
        NEWCELL(CDR(ptr), cons);
        ptr = CDR(ptr);
      }
      CAR(ptr) = CAR(l);
      CDR(ptr) = STk_nil;
    }
    l=CDR(l);
 }
  return result;
}


DEFINE_PRIMITIVE("filter!", dfilter, subr2, (SCM pred, SCM list))
{
  SCM previous, l;
  int len;
  SCM x = list_type_and_length(list, &len);

  if (!x) error_bad_list(list);
  if (CONSP(x)) error_circular_list(list);
  if (!NULLP(x)) error_improper_list(list);

  if (STk_procedurep(pred) != STk_true) error_bad_proc(pred);

  for (previous=STk_nil, l=list; !NULLP(l); ) {
    if (BOXED_INFO(l) & CONS_CONST) error_const_cell(l);

    if (STk_C_apply(pred, 1, CAR(l)) == STk_false) {
      if (previous == STk_nil)
        list = CDR(list);
      else
        CDR(previous) = CDR(l);
    } else {
      previous = l;
    }
    l = CDR(l);
  }
  return list;
}

/*
<doc EXT append!
 * (append! list ...)
 *
 * Returns a list consisting of the elements of the first list
 * followed by the elements of the other lists.
 * Contrarily to |append|, the parameter lists (except the last one) are
 * physically modified: their last pair is changed to the value of the next
 * list in the |append!| formal parameter list.
 * @lisp
 * (let* ((l1 (list 1 2))
 *        (l2 (list 3))
 *        (l3 (list 4 5))
 *        (l4 (append! l1 l2 l3)))
 *   (list l1 l2 l3 l4))  => ((1 2 3 4 5) (3 4 5) (4 5) (1 2 3 4 5))
 * @end lisp
 * An error is signaled if one of the given lists is a constant list.
doc>
*/

SCM STk_dappend2(SCM l1, SCM l2)
{
  register SCM tmp;

  if (NULLP(l1)) return l2;

  for (tmp = l1; ; tmp = CDR(tmp)) {
    if (BOXED_INFO(tmp) & CONS_CONST) error_const_cell(tmp);
    if (NULLP(CDR(tmp))) {
      CDR(tmp) = l2;
      return l1;
    }
  }
  STk_error("bad pair ~S", tmp);
  return STk_void; /* never reached */
}


DEFINE_PRIMITIVE("append!", dappend, vsubr, (int argc, SCM* argv))
{
  switch (argc) {
    case 0:  return STk_nil;
    case 1:  return *argv;
    case 2:  return STk_dappend2(argv[0], argv[-1]);
    default: return STk_dappend2(argv[0], STk_dappend(argc-1, argv-1));
  }
}

/*
<doc EXT reverse!
 * (reverse! list)
 *
 * Returns a list consisting of the elements of |list| in reverse order.
 * Contrarily to |reverse|, the returned value is not newly allocated but
 * computed "in place".
 *
 * @lisp
 * (let ((l '(a b c)))
 *   (list (reverse! l) l))        =>  ((c b a) (a))
 * (reverse! '(a constant list))   =>  error
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("reverse!", dreverse, subr1, (SCM l))
{
  SCM tmp, p, prev;

  for(p=l, prev=STk_nil; !NULLP(p); prev=p, p=tmp) {
    if (!CONSP(p)) error_bad_list(l);
    if (BOXED_INFO(p) & CONS_CONST) error_const_cell(p);
    tmp = CDR(p);
    CDR(p) = prev;
  }
  return prev;
}

/*
 *
 * Utility procedure equivalent to (delete! obj list eq?)
 *
 */
SCM STk_dremq(SCM obj, SCM list)
{
  SCM previous, l, start=list;

  for (previous=STk_nil, l=list; !NULLP(l); ) {
    if (!CONSP(l)) error_bad_list(list);
    if (BOXED_INFO(l) & CONS_CONST) error_const_cell(l);

    if (obj == CAR(l)) {
      if (previous == STk_nil)
        list = CDR(list);
      else
        CDR(previous) = CDR(l);
    } else {
      previous = l;
    }
    if ((l=CDR(l)) == start) error_circular_list(list);
  }
  return list;
}

/*
 *
 * Fast version of assq; for internal use only (alist must be well-formed)
 *
 */
SCM STk_int_assq(SCM obj, SCM alist)
{
  register SCM l;

  for(l=alist; !NULLP(l); l = CDR(l)) {
    if (CAR(CAR(l)) == obj) return CAR(l);
  }
  return STk_false;
}



/* ======================================================================
 *
 * Extended pairs
 *
 * ======================================================================
 */

struct econs_obj {              /* Use a mapping wchich is identical to cons */
  stk_header header;
  SCM car;
  SCM cdr;
  char *file;
  int line;
  int pos;
};

#define ECONS_FILE(p)   (((struct econs_obj *) (p))->file)
#define ECONS_LINE(p)   (((struct econs_obj *) (p))->line)
#define ECONS_POS(p)    (((struct econs_obj *) (p))->pos)
#define ECONSP(obj)     (CONSP(obj) && (BOXED_INFO(obj) & CONS_ECONS))


SCM STk_econs(SCM car, SCM cdr, char *file, int line, int pos)
{
  SCM z;

  NEWCELL_WITH_LEN(z, cons, sizeof(struct econs_obj));
  CAR(z)        = car;
  CDR(z)        = cdr;
  ECONS_FILE(z) = file;
  ECONS_LINE(z) = line;
  ECONS_POS(z)  = pos;
  BOXED_INFO(z) |= CONS_ECONS;

  return z;
}

DEFINE_PRIMITIVE("%epair?", epairp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(ECONSP(obj));
}

DEFINE_PRIMITIVE("%epair-file", epair_file, subr1, (SCM obj))
{
  if (ECONSP(obj)) return STk_Cstring2string(ECONS_FILE(obj));
  error_wrong_type(obj);
  return STk_void; /* never reached */
}


DEFINE_PRIMITIVE("%epair-line", epair_line, subr1, (SCM obj))
{
  if (ECONSP(obj)) return MAKE_INT(ECONS_LINE(obj));
  error_wrong_type(obj);
  return STk_void; /* never reached */
}


DEFINE_PRIMITIVE("%epair-position", epair_position, subr1, (SCM obj))
{
  if (ECONSP(obj)) return MAKE_INT(ECONS_POS(obj));
  error_wrong_type(obj);
  return STk_void; /* never reached */
}

int STk_init_list(void)
{
  ADD_PRIMITIVE(pairp);
  ADD_PRIMITIVE(cons);
  ADD_PRIMITIVE(car);
  ADD_PRIMITIVE(cdr);
  ADD_PRIMITIVE(setcar);
  ADD_PRIMITIVE(setcdr);
  ADD_PRIMITIVE(cxr);
  ADD_PRIMITIVE(nullp);
  ADD_PRIMITIVE(listp);
  ADD_PRIMITIVE(make_list);
  ADD_PRIMITIVE(list);
  ADD_PRIMITIVE(list_length);
  ADD_PRIMITIVE(append);
  ADD_PRIMITIVE(reverse);
  ADD_PRIMITIVE(list_tail);
  ADD_PRIMITIVE(list_ref);
  ADD_PRIMITIVE(list_set);
  ADD_PRIMITIVE(memq);
  ADD_PRIMITIVE(memv);
  ADD_PRIMITIVE(member);
  ADD_PRIMITIVE(assq);
  ADD_PRIMITIVE(assv);
  ADD_PRIMITIVE(assoc);
  ADD_PRIMITIVE(circular_listp);
  ADD_PRIMITIVE(list_copy);
  ADD_PRIMITIVE(list_deep_copy);

  ADD_PRIMITIVE(pair_mutable);
  ADD_PRIMITIVE(list_star);
  ADD_PRIMITIVE(last_pair);
  ADD_PRIMITIVE(filter);
  ADD_PRIMITIVE(dfilter);
  ADD_PRIMITIVE(dappend);
  ADD_PRIMITIVE(dreverse);

  ADD_PRIMITIVE(epairp);
  ADD_PRIMITIVE(epair_file);
  ADD_PRIMITIVE(epair_line);
  ADD_PRIMITIVE(epair_position);

  return TRUE;
}
