/*
 * 101.c   -- Implementation of SRFI-101
 *
 * Copyright © 2021 Jerônimo Pellegrini <j_p@aleph0.info>
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
 *           Author: Jerônimo Pellegrini [j_p@aleph0.info]
 *    Creation date: 27-Jun-2022 21:15
 */

#include <stklos.h>
#include "rlist-incl.c"

/*
  This is an implementation of the data structure described in

  Okasaki, Chris. "Purely Functional Random-Access Lists"
  Functional Programming Languages and Computer Architecture,
  June 1995, pages 86-95.

  The "random access" lists are called 'rlists' in this file.
  They are sequences (linked lists) of full binary trees.

  As an example, the list (A B C D E F G H I J K) is represented
  as

  [1] ......> [3] ...............> [7]
   |           |                    |
  "A"         "B"                  "E"
              / \                 /   \
            "C" "D"            "F"     "I"
                               / \     / \
                             "G" "H" "J" "K"

  Each node of the upper linked list stores the size of the
  tree that it holds: [1] -> [3] -> [7]; and that size is
  always  (2^k) - 1, because the trees are always full.
*/

static int tc_rlist;

/***************************************************************

  RLIST_TREE
  ==========

  An rlist is represented as a linked list of trees (hence a
  forest). The rlist_tree structure below represents one
  tree (a simple binary tree - data, left, right).

  In an rlist, all trees are complete, so either left and
  right do point to other nodes, or they are both STk_nil.

***************************************************************/

struct rlist_tree_obj {
  SCM data;
  SCM left;
  SCM right;
};

typedef struct rlist_tree_obj* TREE;

#define TREE_DATA(x)    (((struct rlist_tree_obj *) x)->data)
#define TREE_LEFT(x)    (((struct rlist_tree_obj *) x)->left)
#define TREE_RIGHT(x)   (((struct rlist_tree_obj *) x)->right)



/***************************************************************

  RLIST
  =====

  This is the linked list of trees that represent and rtree.

**************************************************************/

struct rlist_obj {
  stk_header header;
  long size;
  TREE first;
  SCM  rest;
};

#define RLISTP(x) (BOXED_TYPE_EQ((x), tc_rlist))

#define TREE_SIZE(x) (((struct rlist_obj *) x)->size)
#define FIRST(x)     (((struct rlist_obj *) x)->first)
#define REST(x)      (((struct rlist_obj *) x)->rest)



/***************************************************************

  TYPE CHECKING
  =============

  For Scheme-visible functions.

**************************************************************/

static void
check_rlist(SCM x) {
  if (!(NULLP(x)||RLISTP(x))) STk_error("bad rlist ~S", x);
}
static void
check_integer(SCM x) {
  if (!INTP(x)) STk_error("bad integer ~S", x);
}
static void
check_procedure(SCM x) {
  if (!STk_procedurep(x)) STk_error("bad procedure ~S", x);
}


/***************************************************************

  TREES
  =====

**************************************************************/


/* Builds a tree from left, right and data. */
static inline TREE
STk_rlist_make_tree(SCM data, TREE left, TREE right) {
  TREE t = STk_must_malloc(sizeof(struct rlist_tree_obj));
  TREE_LEFT(t) = left;
  TREE_RIGHT(t) = right;
  TREE_DATA(t) = data;
  return t;
}


/* Returns the idx-th element of tree t.
   The tree size is NOT stored in the tree in order to save space, so
   it must be kept outside and passed here. */
static inline SCM
srfi101_tree_ref(TREE t, long tree_size, long idx) {
  if (idx == 0) return TREE_DATA(t);
  if (idx <= tree_size/2) return srfi101_tree_ref(TREE_LEFT(t), tree_size/2, idx-1);
  return srfi101_tree_ref(TREE_RIGHT(t), tree_size/2, idx - tree_size/2-1);
}


/* Prints the tree t, prepending spaces according to 'level',
   sending output to 'port'.  */
static void
STk_debug_rtree(TREE t, int level, SCM port) {
  for(int i=0; i < (level*4); i++)
    STk_putc(' ', port);

  STk_print(TREE_DATA(t), port, DSP_MODE);
  STk_putc('\n', port);
  if (TREE_LEFT(t) != STk_nil)   STk_debug_rtree(TREE_LEFT(t),  level+1, port);
  if (TREE_RIGHT(t) != STk_nil)  STk_debug_rtree(TREE_RIGHT(t), level+1, port);
}


/* Searches for idx in a single tree (NOT the full forest).
   - if proc is NULL, just creates a new subtree with obj in the
   idx-th position
   - if proc is not NULL, creates a new subtree with proc(obj) in
   the idx-th position, and will set 'old' to the old value
   at that position
*/
static inline SCM
srfi101_tree_update(TREE t, long tree_size, long idx, SCM proc, SCM obj, SCM *old) {
  if (idx == 0) {
    if (proc) {
      *old = TREE_DATA(t);
      SCM new = STk_C_apply(proc,1,*old);
      return STk_rlist_make_tree(new, TREE_LEFT(t), TREE_RIGHT(t));
    }
    else {
      return STk_rlist_make_tree(obj, TREE_LEFT(t), TREE_RIGHT(t));
    }
  }
  long new_size = tree_size/2;
  return (idx <= new_size)
    ? STk_rlist_make_tree(TREE_DATA(t),
                          srfi101_tree_update(TREE_LEFT(t),
                                              new_size,
                                              idx - 1,
                                              proc, obj, old),
                          TREE_RIGHT(t))
    : STk_rlist_make_tree(TREE_DATA(t),
                          TREE_LEFT(t),
                          srfi101_tree_update(TREE_RIGHT(t),
                                              new_size,
                                              idx - new_size - 1,
                                              proc, obj, old));
}


/***************************************************************

  RLISTS / FORESTS
  ================

**************************************************************/


/* for rlist Scheme objects -- verifies if its tree has a
   single node.

   COMPLEXITY: O(1)

   NOTE:  even if there are other trees in the forest, if THIS
   tree only has one node, it IS a leaf!
*/
static int
STk_rlist_leafp(SCM list) {
  return
    RLISTP(list) &&
    list != STk_nil &&
    TREE_LEFT (FIRST(list)) == STk_nil &&
    TREE_RIGHT(FIRST(list)) == STk_nil;
}


/* Adds a tree to a forest, in a similar way to what CONS
   does to a list.

   COMPLEXITY: O(1)

   CAUTION: type cheking should be done BEFORE calling this function!
   a MUST be a tree. */
static SCM
rforest_cons (long size, TREE a, SCM b) {
  SCM c;
  NEWCELL_WITH_LEN(c, rlist, sizeof(struct rlist_obj));
  TREE_SIZE(c) = size;
  FIRST(c) = a;
  REST(c)  = b;
  return c;
}


/* Returns -1 for improper lists and the length for proper lists

   COMPLEXITY: O(log(n))   n = size of the rlist

   CAUTION: type cheking should be done BEFORE calling this function!
   x MUST be an rlist. */
static inline long
srfi101_len(SCM x) {
  long len = 0;
  SCM ptr;
  for(ptr = x; RLISTP(ptr); ptr = REST(ptr))
    len += TREE_SIZE(ptr);
  return NULLP(ptr)? len : -1;
}

/* Updates the rlist at position idx.
   - Does NOT change the original rlist, ir rather builds a copy, possibly sharing
   tail with the original.
   - If proc=NULL, then 'e' is put in the specified position (idx).
   - If e=NULL, then proc must be a procedure. Then 'e' will be ignored,
   and the element at position idx will be updated by applying proc on
   it.
   - If idx is out of bounds, STk_false is returned.   */
static inline SCM
rlist_update(SCM list, long idx, SCM proc, SCM e, SCM *old) {
  if (list == STk_nil ||
      (!RLISTP(list)))
    return STk_false; /* Out of bounds */

  if (idx < TREE_SIZE(list))
    return rforest_cons(TREE_SIZE(list),
                        srfi101_tree_update(FIRST(list),
                                            TREE_SIZE(list),
                                            idx,
                                            proc, e, old),
                        REST(list));
  else {
    SCM next = rlist_update(REST(list),
                            idx - TREE_SIZE(list),
                            proc, e, old);

    /* Out of bounds: */
    if (next == STk_false) return STk_false;

    return rforest_cons(TREE_SIZE(list),
                        FIRST(list),
                        next);
  }
}

/***************************************************************

  SCHEME API
  ==========

**************************************************************/


/* Prints rlist. This will print each tree in the sequence, along with
   its size, and also the tail if the list is improper. */
DEFINE_PRIMITIVE("%debug-rlist", srfi101_debug_rlist, subr1, (SCM rlist))
{
  check_rlist(rlist);

  SCM port = STk_stderr;

  if (rlist == STk_nil) {
    STk_print(rlist, port, DSP_MODE);
    return STk_void;
  }

  while (RLISTP(rlist)) {
    STk_puts("\n====TREE========\nsize: ", port);
    STk_putc('_',port);
    STk_print( MAKE_INT(TREE_SIZE(rlist)), port, DSP_MODE);
    STk_putc('_',port);
    STk_putc('\n',port);
    STk_debug_rtree(FIRST(rlist), 0, port);
    STk_puts("\n====END=========\n", port);
    rlist = REST(rlist);
  }
  if (!NULLP(rlist)) {
    STk_puts("\n=improper tail==\n", port);
    STk_print(rlist, port, DSP_MODE);
    STk_puts("\n====END=========\n", port);
  }
  return STk_void;
}



/*
<DOC ext srfi101:car
 * (srfi101:car rlist)
 *
 * Returns the car of |rlist| (SRFI-101 purely functional random access list).
 *
 * @lisp
 * (define x (rlist 10 20 30 40)) => 10
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:car", srfi101_car, subr1, (SCM list))
{
  check_rlist(list);
  if (NULLP(list)) STk_error("null rlist has no head");
  return TREE_DATA(FIRST(list));
}



/*
<DOC EXT srfi101:cdr
 * (srfi101:cdr rlist)
 *
 * Returns the cdr of |rlist| (SRFI-101 purely functional random access list).
 *
 * @lisp
 * (define x (rlist 10 20 30 40)) => (20 30 40)
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:cdr", srfi101_cdr, subr1, (SCM list))
{
  check_rlist(list);
  if (NULLP(list)) STk_error("null rlist has no tail");

  /* Leaf: */
  if (STk_rlist_leafp(list)) return REST(list);

  /* Neither null nor leaf, so we split the first tree
     in two equal-sized ones, leaving the root (car)
     out. */
  long size = TREE_SIZE(list)/2;
  return rforest_cons(size,
                      TREE_LEFT (FIRST(list)),
                      rforest_cons(size,
                                   TREE_RIGHT(FIRST(list)),
                                   REST(list)));
}



/*
<DOC EXT srfi101:cons
 * (cons obj1 obj2)
 *
 * Returns a newly allocated rpair whose |rcar| is |obj1| and whose |rcdr| is |obj2|.
 * The pair is guaranteed to be different (in the sense of |eqv?|) from every
 * existing object.  This operation takes O(1) time.
 *
 * @lisp
 *     (srfi101:cons 'a '())           =>  #,(<rlist> a)
 *     (srfi101:cons '(a) '(b c d))    =>  #,(<rlist> (a) b c d)
 *     (srfi101:cons "a" '(b c))       =>  #,(<rlist> "a" b c)
 *     (srfi101:cons 'a 3)             =>  #,(<rlist> a . 3)
 *     (srfi101:cons '(a b) 'c)        =>  #,(<rlist> (a b) . c)
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:cons", srfi101_cons, subr2, (SCM a, SCM list))
{
  TREE t;

  if ( ( !NULLP(list) )     &&
       RLISTP(list)         &&
       RLISTP(REST(list))   && /* at least two trees */
       TREE_SIZE(list) == TREE_SIZE(REST(list))) {      /* two first trees with same size */

    t = STk_rlist_make_tree(a,
                            FIRST(list),
                            FIRST(REST(list)));
    return rforest_cons(1 + TREE_SIZE(list) + TREE_SIZE(REST(list)),
                        t,
                        REST(REST(list)));
  } else {
    /* All other cases */
    t = STk_rlist_make_tree(a, STk_nil, STk_nil);
    return rforest_cons(1, t, list);
  }
}





/* EXTRA: list-copy */

static SCM
srfi101_copy(SCM x) {
  return (RLISTP(x))
    ? STk_srfi101_cons(STk_srfi101_car(x),
                       srfi101_copy(STk_srfi101_cdr(x)))
    : x;
}
DEFINE_PRIMITIVE("srfi101:list-copy", srfi_101_list_copy, subr1, (SCM x))
{
  check_rlist(x);
  return srfi101_copy(x);
}




/*
<DOC EXT srfi101:pair?
 * (pair? obj)
 *
 * Returns true if |obj| is an rlist pair, and otherwise returns false.
 * This operation takes O(1) time.
 *
 * @lisp
 * (pair? '(a . b))                               => #t
 * (pair? '(a b c))                               => #t
 * (pair? '())                                    => #f
 * (pair? '#(a b))                                => #f
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:pair?", srfi101_pairp, subr1, (SCM x))
{
  return MAKE_BOOLEAN(RLISTP(x));
}




/*
<DOC EXT srfi101:list?
 * (srfi101:list? obj)
 *
 * Returns true if |obj| is a list, false otherwise. By definition, all lists
 * are chains of pairs that have finite length and are terminated by
 * the empty list. This operation takes time bounded by O(log(n)),
 * where n is the number of pairs in the chain forming the potential
 * list.
 *
 * @lisp
 * (list? '(a b c))                               =>  #t
 * (list? '())                                    =>  #t
 * (list? '(a . b))                               =>  #f
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:list?", srfi101_listp, subr1, (SCM x))
{
  while (RLISTP(x)) x = REST(x);
  return MAKE_BOOLEAN(x == STk_nil);
}




/*
<DOC EXT srfi101:list
 * (srfi101:list obj1 ...)
 *
 * Returns a newly allocated rlist of its arguments.  This operation
 * takes time bounded by O(n), where n is the number of arguments.
 *
 * @lisp
 * (list 'a (+ 3 4) 'c)                        =>  (a 7 c)
 * (list)                                      =>  ()
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:list", srfi101_list, vsubr, (int argc, SCM *things))
{
  register SCM *tmp, l = STk_nil;

  for (tmp = things-argc+1; tmp <= things; tmp++)
    l = STk_srfi101_cons(*tmp, l);

  return l;
}



/* FIXME: should be O(log(k)), but is O(k) */
/*
<DOC EXT srfi101:make-list
 * (srfi101:make-list k)
 * (srfi101:make-list k fill)
 *
 * Returns a newly allocated rlist of |k| elements. If a second argument
 * is given, then each element is initialized to |fill| .  Otherwise the
 * initial contents of each element is unspecified. This operation takes
 * time and space bounded by O(log(k)).
 *
 * @lisp
 * (make-list 5 0)                                ⇒  (0 0 0 0 0)
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:make-list", srfi101_make_list, subr12, (SCM k, SCM fill))
{
  SCM result = STk_nil;
  check_integer(k);
  if (!fill) fill = STk_void;
  for(int i=0; i<INT_VAL(k); i++)
    result =STk_srfi101_cons(fill, result);
  return result;
}




/*
<DOC EXT srfi101:length
 * (srfi101:length rlist)
 *
 * Returns the length of |list|. This operation takes time bounded by
 * O(log(n)), where n is the length of the list.
 *
 * @lisp
 * (srfi101:length (srfi101:list a b c))             => 3
 * (srfi101:length (srfi101:list a
 *                               (srfi101:list b)
 *                               (srfi101:list c)))  => 3
 * (srfi101:length '())                              => 0
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:length", srfi101_length, subr1, (SCM x))
{
  check_rlist(x);
  long len = srfi101_len(x);
  if (len == -1) STk_error("attempt to calculate length of improper list ~S", x);
  return MAKE_INT(len);
}




/*
<DOC srfi101:length<=?
 * (srfi101:length<=? obj k)
 *
 * Returns true if |obj| is a chain of at least |k| pairs and false
 * otherwise. This operation takes time bounded by O(log(min(k,n))),
 * where n is the length of the chain of pairs.
 *
 * @lisp
 * (length<=? 'not-a-list 0)                      =>  #t
 * (length<=? '(a . b) 0)                         =>  #t
 * (length<=? '(a . b) 1)                         =>  #t
 * (length<=? '(a . b) 2)                         =>  #f
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:length<=?", srfi101_length_le, subr2, (SCM list, SCM k))
{
  check_integer(k);
  long bound = INT_VAL(k);

  if (bound == 0) return STk_true;

  check_rlist(list);

  long len = 0;
  for(SCM ptr = list; RLISTP(ptr); ptr = REST(ptr)) {
    len += TREE_SIZE(ptr);
    if (len >= bound) return STk_true;
  }

  return STk_false;
}




/* Appends exactly two rlists
   NO type checking. */
static inline SCM
srfi_101_append2(SCM a, SCM b) {
  /* Case 1. a is an rlist with one single element */
  if (STk_srfi101_cdr(a) == STk_nil)
    return STk_srfi101_cons(STk_srfi101_car(a), b);

  /* Case 2. a is an improper pair (ERROR) */
  if (!RLISTP(STk_srfi101_cdr(a)))
    STk_error("cannot append to end of improper rlist ~S", a);

  /* Case 3. a is a proper list with more than one element */
  return STk_srfi101_cons(STk_srfi101_car(a),
                          srfi_101_append2(STk_srfi101_cdr(a),
                                           b));
}

/* Appends several rlists.
   NO type checking. */
static inline SCM
srfi_101_append(int argc, SCM* argv) {
  if (argc == 0) return STk_nil;
  if (argc == 1) return *argv;

  SCM app_rest = srfi_101_append(argc-1, argv-1);

  /*
    We have
    *argv    = first list
    app_rest = other lists, already appended

    The structure of append_rest does not change.
    The elements of *argv are consed in front of
    append_rest, one by one, in reverse order.
  */

  return srfi_101_append2(*argv, app_rest);
}

/*
<DOC EXT srfi101:append
 * (srfi101:append list1 list2 ... obj)
 *
 * Returns a chain of pairs consisting of the elements of the first
 * list followed by the elements of the other lists, with |obj| as the
 * |cdr| of the final pair. An improper list results if |obj| is not a
 * list. This operation takes time bounded by O(log(n)), where n is
 * the total number of elements in the given lists.
 *
 * @lisp
 * (append '(x) '(y))                           =>  (x y)
 * (append '(a) '(b c d))                       =>  (a b c d)
 * (append '(a (b)) '((c)))                     =>  (a (b) (c))
 * (append '(a b) '(c . d))                     =>  (a b c . d)
 * (append '() 'a)                              =>  a
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:append", srfi101_append, vsubr, (int argc, SCM* argv))
{
  while (argc && NULLP(*argv)) { argv--; argc--; }

  SCM *ptr = argv;
  for (int i=0; i < argc - 1; i++)
    check_rlist(*ptr--);

  return srfi_101_append(argc, argv);
}




/*
<DOC EXT srfi101:reverse
 * (reverse rlist)
 *
 * Returns a newly allocated list consisting of the element of list in
 * reverse order. This operation takes time bounded by O(n) where n
 * is the length of the list.
 *
 * @lisp
 * (srfi101:reverse '(a b c))                =>  (c b a)
 * (srfi101:reverse '(a (b c) 'd '(e (f))))  =>  ((e (f)) d (b c) a)
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:reverse", srfi101_reverse, subr1, (SCM list))
{
  SCM result = STk_nil;
  SCM ptr = list;

  while(RLISTP(ptr)) {
    result = STk_srfi101_cons(STk_srfi101_car(ptr), result);
    ptr = STk_srfi101_cdr(ptr);
    if (!(NULLP(ptr) || RLISTP(ptr)))
      STk_error("cannot reverse improper rlist ~S", list);
  }

  return result;
}




/*
<DOC EXT srfi101:list-ref
 * (list-ref pair k)
 *
 * |Pair| must be a chain of pairs whose count is at least |k| + 1. The
 * |list-ref| procedure returns the |k|th element of pair. This operation
 * takes time bounded by O(min(k,log(n))), where n is the length
 * of the chain of pairs.
 *
 * @lisp
 * (list-ref '(a b c d) 2)      => c
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:list-ref", srfi101_list_ref, subr2, (SCM list, SCM k))
{
  check_integer(k);
  check_rlist(list);
  long idx = INT_VAL(k);

  if (TREE_SIZE(list) == 0) STk_error("empty rlist");
  if (idx < 0)              STk_error("index %d out of bounds", idx);

  /* Find the correct tree and call srfi101_tree_ref on it: */
  while (RLISTP(list)) {
    if (idx >= TREE_SIZE(list)) {
      idx -= TREE_SIZE(list);
      list = REST(list);
    } else return srfi101_tree_ref(FIRST(list), TREE_SIZE(list), idx);
  }
  STk_error("index %d out of bounds", INT_VAL(k)); /* don't report idx, it was changed! */
  return STk_void; /* Never reached. */
}





/*
<DOC EXT list-set
 * (list-set pair k obj)
 *
 * |Pair| must be a chain of pairs whose count is at least |k| + 1. The
 * |list-set| procedure returns the chain of pairs obtained by replacing
 * the kth element with obj. This operation takes time bounded by
 * O(min(k,log(n))), where n is the length of the chain of pairs.
 *
 * @lisp
 * (list-set '(a b c d) 2 'x)    => (a b x d)
 * @end lisp
DOC>
*/
static inline void check_rlist_and_size(SCM list)
{
  // HACK: This function permits to circumvent an issue with GCC 15.2
  // (with option -O2 or -O3). If we place those lines in the srfi101_list_set
  // function, GCC signals an error (and only in this function, whereas this
  // is used at everal places elsewhere) :-<.
  // This is all the more strange given that the function is inline and that
  // this code is normally inserted into srfi101_list_set.
  check_rlist(list);
  if (TREE_SIZE(list) == 0)      STk_error("empty rlist");
}


DEFINE_PRIMITIVE("srfi101:list-set", srfi101_list_set, subr3, (SCM list, SCM k, SCM obj))
{
  check_rlist_and_size(list);
  check_integer(k);
  long idx = INT_VAL(k);

  if (idx < 0) STk_error("index %d out of bounds", idx);

  /* The following function already checks for idx past the
     end of the list: */
  SCM result = rlist_update(list, idx, NULL, obj, NULL);
  if (result == STk_false) STk_error("index %d out of bounds", idx);
  return result;
}





/*
<DOC EXT list-ref/update
 * (list-ref/update pair k proc)
 *
 * Returns the same results as:
 * @lisp
 * (values (list-ref pair k)
 *         (list-set pair k (proc (list-ref pair k))))
 * @end lisp
 *
 * but it may be implemented more efficiently.
 *
 * @lisp
 * (list-ref/update '(7 8 9 10) 2 -)
 *        => 9
 *           (7 8 -9 10)
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:list-ref/update", srfi101_list_ref_update, subr3,
                                            (SCM list, SCM k, SCM proc))
{
  check_integer(k);
  check_rlist(list);
  check_procedure(proc);
  long idx = INT_VAL(k);
  if (TREE_SIZE(list) == 0)      STk_error("empty rlist");
  if (idx < 0) STk_error("index %d out of bounds", idx);

  /* 'old' will be set to the old value at psition 'k'.
     The procedure already checks for idx past the end of
     the rlist. */
  SCM old;
  SCM result = rlist_update(list, idx, proc, NULL, &old);
  if (result == STk_false) STk_error("index %d out of bounds", idx);
  return STk_n_values(2, old, result);
}





/*
  Helper to the iterate function

  proc        = procedure to be applied
  lists       = the rlists
  arity       = arity of proc, AND quantity of rlists passed
  output size = # of elements in the output rlist (when mapping)
  map         = if non-zero, the results are accumulated in a new rlist
*/
static inline SCM
iterate_aux(SCM proc, struct cons_obj *args, SCM *lists, int arity, int output_size, int map) {
  if (output_size == 0) return STk_nil;
  SCM res;

  /* Copy one element from each list into 'args': */
  for (int i=0; i < arity; i++) {
    CAR(&args[i]) = STk_srfi101_car(lists[i]);
  }

  /* Do the real work! :) */
  res = STk_C_apply_list(proc, (SCM)&args[0]);
  /* One step forward in each rlist: */
  for (int i=0; i < arity; i++)
    lists[i] = STk_srfi101_cdr(lists[i]);

  if (map) return STk_srfi101_cons(res,
                                   iterate_aux(proc,args,lists,arity,output_size-1,map));
  else return iterate_aux(proc,args,lists,arity,output_size-1,map);
}

/*
  Iterates on rlists, applying a procedure. This is used by map and for-each.
*/
static inline SCM
srfi101_iterate(SCM proc, int map, SCM *rlists, long arity) {

  SCM *ptr = rlists;
  check_rlist(*ptr);
  long len         = srfi101_len(*ptr);
  if (len == -1) STk_error ("improper rlist not allowed ~S", *ptr);
  long output_size = len;
  ptr--;
  for (int i=1; i < arity; i++) {
    check_rlist(*ptr);
    len = srfi101_len(*ptr);
    if (len == -1) STk_error ("improper rlist not allowed ~S", *ptr);
    if (len < output_size)
      output_size = len;

    ptr--;
  }

  /* NOTE: we do NOT check the arity of proc, since
     1. it will be checked anyway when it is applied
     2. it could be variable */

  SCM *lists = STk_must_malloc(arity * sizeof(SCM));

  ptr = rlists;
  for (int i=0; i < arity; i++)
    lists[i] = *ptr--;

  /*
    arity       = # of arguments to proc, AND # of rlists
    output_size = # of elements to process
    lists       = C array of rlists (those passed as argument),
    so we can access them by index in a verry short time
  */


  /* We will allocate a vector od SCM, ONCE, and then turn each of its cells
   * into CONS cells. This will be the argument list for proc.
   *
   * args will be a C array of cons cells, each pointing to the next.
   *
   * +------------+-------------+         +------------+
   * |  _____ ___ |   _____ ___ |         |  _____ ___ |
   * | | car | ----->| car | ------> ...  | | car | -------> nil
   * |  ----- --- |   ----- --- |         |  ----- --- |
   * +------------+-------------+         +------------+
   *    args[0]      args[1]         ...     args[n-1]
   *
   * In each iteration, we will copy the i-th element of each vector in vecs
   * onto its place in the list, and call STk_C_apply_list.
   *
   * I can't think of a more efficient way to do this.
   */

  /*
    arity       = # of arguments to proc, AND # of rlists
    output_size = # of elements to process
    lists       = C array of rlists (those passed as argument),
    so we can access them by index in a verry short time
    SIZE of lists is ARITY!
    args        = Scheme array AND list, of size arity, that will be
    passed to apply
  */


  struct cons_obj *args = STk_must_malloc(arity * sizeof(struct cons_obj));
  /* Adust CDR pointers */
  int i;
  for (i=0; i<arity-1; i++) {
    /* We do the same as NEWCELL would do after allocating each
       cons cell: */
    BOXED_TYPE(&args[i]) = tc_cons;
    BOXED_INFO(&args[i]) = 0;

    /* And set cdr: */
    CDR(&args[i]) = &args[i+1];
  }
  CAR(&args[arity-1]) = MAKE_INT((unsigned long) -999);
  CDR(&args[arity-1]) = STk_nil;

  return iterate_aux(proc, args, lists, arity, output_size, map);
}

/*
<DOC EXT srfi101:map
 * (srfi101:map proc list1 list2 ...)
 *
 * The lists should all have the same length. |Proc| should accept as many
 * arguments as there are lists and return a single value.
 *
 * The |map| procedure applies |proc| element-wise to the elements of the
 * lists and returns a list of the results, in order. |Proc| is always
 * called in the same dynamic environment as map itself. The order in
 * which |proc| is applied to the elements of the lists is unspecified.
 *
 * @lisp
 * (srfi101:map cadr (srfi101:list '(a b)
 *                                 '(d e)
 *                                 '(g h))) => (b e h)
 *
 * (srfi101:map (lambda (n) (expt n n))
 *              (srfi101:list 1 2 3 4 5))
 *                                         => (1 4 27 256 3125)
 *
 * (srfi101:map + (srfi101:list 1 2 3)
 *                (srfi101:list 4 5 6))    => (5 7 9)
 *
 * (let ((count 0))
 *   (srfi101:map (lambda (ignored)
 *          (set! count (+ count 1))
 *          count)
 *        (srfi101:list a b)))             => (1 2) or (2 1)
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:map", srfi101_map, vsubr, (int argc, SCM *argv))
{
  if (argc < 2) STk_error("at least two arguments needed, %d given", argc);

  SCM proc = *argv--;
  check_procedure(proc);

  return srfi101_iterate(proc, 1, argv, argc-1);
}

/*
<DOC EXT srfi101:for-each
 * (for-each proc list1 list2 ...)
 *
 * The lists should all have the same length. |Proc| should accept as
 * many arguments as there are lists.
 *
 * The |for|-each procedure applies |proc| element-wise to the elements of
 * the lists for its side effects, in order from the first element to the
 * last. |Proc| is always called in the same dynamic environment as
 * |for-each| itself. The return values of |for-each| are unspecified.
 *
 * @lisp
 * (let ((v (make-vector 5)))
 *  (srfi101:for-each (lambda (i)
 *                      (vector-set! v i (* i i)))
 *                    (srfi101:list 0 1 2 3 4))
 *  v)                                           =>  #(0 1 4 9 16)
 *
 * (srfi101:for-each (lambda (x) x)
 *                   (srfi101:list 1 2 3 4))     =>  unspecified
 * (srfi101:for-each even? '())                  =>  unspecified
 * @end lisp
DOC>
*/
DEFINE_PRIMITIVE("srfi101:for-each", srfi101_for_each, vsubr, (int argc, SCM *argv))
{
  if (argc < 2) STk_error("at least two arguments needed, %d given", argc);

  SCM proc = *argv--;
  check_procedure(proc);

  srfi101_iterate(proc, 0, argv, argc-1);
  return STk_void;
}


/***************************************************************

  THE RLIST TYPE: print, equal and extended type descriptor
  ===============

***************************************************************/


static void print_rlist(SCM rlist, SCM port, int mode)
{
  check_rlist(rlist);
  STk_puts("#,(<rlist> ", port);
  while (RLISTP(rlist)) {
    STk_print(STk_srfi101_car(rlist), port, mode);
    rlist = STk_srfi101_cdr(rlist);
    if (rlist != STk_nil) STk_putc(' ', port);
  }

  /* If the end is not nil, we print the dotted representation
     of an improper list: */
  if (rlist != STk_nil) {
    STk_puts(". ", port);
    STk_print(rlist, port, mode);
  }
  STk_putc(')', port);
}


static SCM test_equal_rlist(SCM x, SCM y)
{
  if (NULLP(x) && NULLP(y)) return STk_true;
  if (NULLP(x))             return STk_false;
  if (NULLP(y))             return STk_false;

  /* Sure we have at least one tree now. */

  /* Different sizes --> false! */
  SCM ptr_x = x;
  SCM ptr_y = y;
  while (RLISTP(ptr_x) && RLISTP(ptr_y)) {

    if (TREE_SIZE(ptr_x) != TREE_SIZE(ptr_y))
      return STk_false;

    ptr_x=REST(ptr_x);
    ptr_y=REST(ptr_y);
  }

  /* Now return false on:
     1. Different number of trees;
     2. Improper lists with different last element */
  if (STk_equal(ptr_x, ptr_y) == STk_false) return STk_false;

  /* We have two rlists with the same sizes, same
     structure, AND same ending element (nil for proper
     lists, or the same element for improper lists).
     So we just need to alk through the proper part
     of both lists comparing each element. */
  while ((RLISTP(x) && RLISTP(y))) {
    if (STk_equal(STk_srfi101_car(x),
                  STk_srfi101_car(y)) == STk_false)
      return STk_false;
    x = STk_srfi101_cdr(x);
    y = STk_srfi101_cdr(y);
  }
  return STk_true;
}

static struct extended_type_descr xtype_rlist = {
  .name  = "rlist",
  .print = print_rlist,
  .equal = test_equal_rlist
};

/***************************************************************

  MODULE
  ======

***************************************************************/

MODULE_ENTRY_START("scheme/rlist")
{
  SCM module =  STk_create_module(STk_intern("scheme/rlist"));

  /* Create a new type for rlist */
  tc_rlist = STk_new_user_type(&xtype_rlist);

  ADD_PRIMITIVE_IN_MODULE(srfi101_debug_rlist, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_101_list_copy, module);

  ADD_PRIMITIVE_IN_MODULE(srfi101_cons,            module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_pairp,           module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_listp,           module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_list,            module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_make_list,       module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_length,          module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_length_le,       module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_car,             module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_cdr,             module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_list_ref,        module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_list_set,        module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_list_ref_update, module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_append,          module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_reverse,         module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_map,             module);
  ADD_PRIMITIVE_IN_MODULE(srfi101_for_each,        module);

  /* DO NOT export the symbols we have just defined! They will
     be renamed before they are actually exported. */
  /* STk_export_all_symbols(module); */

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
