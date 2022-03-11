/*
 * itrie.c   -- Tries with fixnum keys --- Implementation of
 * SRFI-217: Integer Sets and SRFI-224: Integer Mappings
 *
 * Copyright © 2021 Jerônimo Pellegrini <j_p@aleph0.info>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 *    Creation date: 02-Jan-2022 18:41
 * Last file update:  4-Feb-2022 10:28 (eg)
 */


#include "stklos.h"
#include "itrie-incl.c"

/*
  New extended types for Patricia trees:

   - iset represents sets of fixnums
   - fxmap represents mappings from fixnums to arbitrary types

   These will be filled later, at the end of this file.
*/
static int tc_iset;
static int tc_fxmap;


/*
  PATRICIA TREES - implementation notes
  =====================================

  This data structure can be used to implement BOTH integer sets (as
  in SRFI 217) and integer mappings (as in SRFI 224), without wasting
  memory and with minimum speed overhead.

  This is based on the paper by Okasaki & Gill, "Fast Mergeable
  Integer Maps" and also on the reference implementations of the
  two SRFIs by Wolfgang Corcoran-Mathe, with several changes.


  ISETs: sets of integers
  -----------------------

  When used to represent integer sets, the value is a bitmap
  representing integers. So, the structure of leaves has the
  following layout:

    stk_header   long      long
  +------------+---------+-------+
  | header     | prefix  | bitmap |
  +------------+---------+-------+

  The prefix holds the first bits of the number, as one would use in
  2's complement. The bitmap is NEITHER 2's complement of anything NOR
  the usual powers-of-two representation of integers: each bit in it
  represents one number, starting from zero:

  0001   -> 0
  0010   -> 1

  And different bits can be used to represent a set of integers:

  0101   -> { 0, 2 }
  1011   -> { 0, 1, 3 }

  An example of leaf:

    stk_header   long      long
  +------------+---------+-------+
  | header     | 1010    | 1001  |
  +------------+---------+-------+

  This represents

  - the prefix, 1011_0000
  - each number represented in the bitmap represents a number when
    summed with the prefix:

  1010_0000 + 0 = 1010_0000
  1010_0000 + 3 = 1010_0011

  NOTE: since we're using plain C 'long ints' to store bitmaps, we get some
  more compression than if we were using Scheme fixnums -- we get up to 64
  numbers per leaf when 'long int" is 64-bit wide!


  FXMAPS: mappings from fixnums to Scheme values
  ----------------------------------------------

  When used to represent integer mappings, the value cell is used
  for the SCM value.

    stk_header   long      SCM
  +------------+---------+-------+
  | header     | prefix  | value |
  +------------+---------+-------+

  For leaves, the prefix *is* the key.

  The information as to wether the trie is of one or the other kind
  is stored in the datatype header (and hence is availabe in each
  trie node).

  Tries can also be made constant/non-mutable by setting one bit in
  their header.


  TRIE NODES
  ==========

  Each node (leaf or branch) of a trie is of type tc_fxmap or tc_iset.
  In its header (the stk_header type that all boxed Scheme types have),
  the field cell_info is used to store information about the trie
  node, in five different bits:

  bit : #defined constant : meaning
  ----+-------------------+------------------------------
    0 : TRIE_CONST        : 0=mutable    1=constant
    1 : TRIE_LEAF         : 0=branch     1=leaf
    2 : TRIE_EMPTY        : 0=non-empty  1=empty
  ----+-------------------+------------------------------

  When bit 1 is set, no insertions or deletions can be made on
  the trie represented by that node:

  - if it's a leaf, its key and values cannot be changed

  - if it's a branch, its left and right branches are also
    non-mutable, and mutating operations will not be done
    on them

  Bit 2 is used to represent empty tries.


  LAYOUT OF THE EMPTY TRIE
  ========================

  The empty trie has the TRIE_EMPTY bit set, and no value:

    stk_header
  +------------+
  | header     |   ----> info about emptyness is in the header
  +------------+

  We do NOT have a single empty trie for the whole program (which
  is different from the empty list). This is the same case as the
  empty vector:

  (eq? (list)   (list))   => #t
  (eq? (vector) (vector)) => #f
  (eq? (iset)  (iset))    => #f


  LAYOUT OF BRANCHES
  ==================

  Branches always have the same layout.

    stk_header   long     long            SCM    SCM
  +------------+--------+---------------+------+-------+
  | header     | prefix | branching bit | left | right |
  +------------+--------+---------------+------+-------+

  Not that both LEFT and RIGHT are necessarily present,
  always.


*/

#define TRIE_CONST     (1 << 0)
#define TRIE_LEAF      (1 << 1)
#define TRIE_EMPTY     (1 << 2)


struct trie_branch_obj {
    stk_header header;
    long prefix;
    long branch_bit;
    SCM  left;
    SCM  right;
};

struct trie_empty_obj {
    stk_header header;
};

struct trie_leaf_iset_obj {
    stk_header header;
    long prefix;
    long bitmap;
};

struct trie_leaf_fxmap_obj {
    stk_header header;
    long prefix;
    SCM  value;
};


/* Accessor macros for trie nodes: */

/* common for all nodes: */
#define ISETP(p)            (BOXED_TYPE_EQ((p), tc_iset))
#define FXMAPP(p)           (BOXED_TYPE_EQ((p), tc_fxmap))
#define TRIEP(p)            (ISETP(p) || FXMAPP(p))

#define TRIE_CONSTP(p)       (BOXED_INFO(p) & TRIE_CONST)
#define TRIE_LEAFP(p)        (BOXED_INFO(p) & TRIE_LEAF)
#define TRIE_BRANCHP(p)      (!(TRIE_LEAFP(p)))
#define TRIE_EMPTYP(p)       (BOXED_INFO(p) & TRIE_EMPTY)

/* BRANCHES AND LEAVES! */
#define TRIE_PREFIX(p)       (((struct trie_branch_obj *)   (p))->prefix)

/* branches: */
#define TRIE_BRANCHBIT(p)    (((struct trie_branch_obj *)   (p))->branch_bit)
#define TRIE_LEFT(p)         (((struct trie_branch_obj *)   (p))->left)
#define TRIE_RIGHT(p)        (((struct trie_branch_obj *)   (p))->right)

/* leaves: */
#define TRIE_BITMAP(p)       (((struct trie_leaf_iset_obj *)  (p))->bitmap)
#define TRIE_KEY(p)          (((struct trie_leaf_fxmap_obj *)  (p))->prefix) /* ??? */
#define TRIE_VALUE(p)        (((struct trie_leaf_fxmap_obj *)  (p))->value)


/* Used to select "min" or "max" of a trie: */
#define TRIE_MIN  (-1)
#define TRIE_MAX  (-2)

/*
  merge_bitmaps is syntactic sugar for bitwise IOR, XOR and AND.
  Used when merging two tries: we pass one of the macro symbols below
  and use one single merge procedure for union, xor and intersection.
*/

#define TRIE_NOP    0
#define TRIE_IOR    1
#define TRIE_XOR    2
#define TRIE_AND    3
#define TRIE_UPDATE 4
#define TRIE_ADJUST 5
#define TRIE_UNION  6

/* When merging two branches, the merge function calls the insert function,
   and passes a "decide" procedure that is used to decide what to do with duplicate
   keys. But "merge" on branches "a,b" may need to call insert "a into b" or
   "b into a", depending on a or b being leaves. So when a and b are swapped,
   the arguments to "decide" must be swapped too, and this macro is used to
   tell which situation it is. */
#define NO_SWAP 0
#define SWAP    1


/***********************************/
/***                             ***/
/***      BEGIN: BIT TRICKS      ***/
/***                             ***/
/***********************************/

/*
  Returns the INDEX of the first bit set in n.

  This is the same as SRFI 143 'fxfirst-set-bit`, but that one works
  in SCM objects, and this one works on unsigned longs:
  - this one doesn't check types
  - this one works with a integer width of sizeof(long)*8, while
    the SRFI 143 one works with the width of fixnums (INT_LENGTH)

     n  n(bin)     trie_first_set_bit(n)
    ------------------------------------
     0              -1
     4   (100)       2
     7   (111)       0
    48 (10000)       4
 */
static inline long
trie_first_set_bit(unsigned long n)
{
    for (int i=0; i < (int) sizeof(long)*8; i++, n = n >> 1)
      if ( n & 1 )
          return (long) i;
    return -1UL;
}

/*
  Returns the INDEX of the last bit set in n.
  The symetric of trie_first_set_bit.
*/
static inline long
trie_last_set_bit(unsigned long n)
{
    for (int i=0; i < (int) sizeof(long)*8; i++, n = n >> 1)
        if ( ! (n >> 1)  )
          return (long) i;
    return -1UL;
}


/*
   Returns a long integer in which the lowest bit of x
   is set:

   x  (as bin)      trie_lowestbit   (as bin)
   --+----------+------------------+---------
   6       110             4            100
   7       111             1              1
   8 001011000             8           1000

   We use a simple property of 2's complement,
*/
static inline long
trie_lowestbit (long x) {
    return x & (-x);
}

/*
   trie_mask sets the mask bit (branching bit) and
   and every bit BELOW it to ZERO.

   In the table below, the branching bit is between
   underscores, for clarity.

   k    (bin)          m                 trie_mask
   -----------------+-----------------+-----------------
   55     110_1_11     4    000_1_00      48    110_0_00
   217 0110_1_1001    16 0000_1_0000     192 0110_0_0000
*/
static inline long
trie_mask(long k, long m) {
    return (m == LONG_MIN)
        ? 0
        : k & ( (~ (m-1)) ^ m);
}


/*
  Verifies if a key, when masked, matches a prefix.

  'mask' is the branching bit, so trie_mask(key,mask)
   is all zeroes after the branching bit.

   key  prefix mask   trie_match_prefix

 */
static inline int
trie_match_prefix(long key, long prefix, long mask) {
    return (trie_mask(key, mask) == prefix);
}


/*
  Returns 1 if the bit given in mask is set in key,
  and 0 otherwise.
 */
static inline int
trie_zerobit(long key, long mask) {
    return (!(key & mask));
}


/*
  Returns the highest bit set in x.

  m is an initial guess, which should be LESS than
  the real answer. If in doubt, use m=1.

   x           m  trie_highestbit(x,m)
  -------------------------------
  48 110000    2    32   100000
  48 110000   16    32   100000
  48 110000   38    16    10000 <= WRONG, because m=38 was too high
  48 110000   64     0        0 <= WRONG, because m=64 was too high

  20  10100    2    16    10000
  20  10100    8    16    10000
  20  10100   16    16    10000
*/
static inline long
trie_highestbit(long x, long m) {
    /* zero bits below m */
    long xx = x & (~(m-1));

    while (1) {
        m = trie_lowestbit(xx);
        /* m only has one bit set, so if xx == m, then this is
           the last bit, from left to right, and we found the bit we
           wanted: */
        if (xx == m) return m;
        /* unset, in xx, the bit represented by m:  */
        xx = xx - m;
    }
    STk_error ("The impossible happened! trie_highestbit got past the loop.");
    return 0;
}

/*
  'MAX' for long integers, which we use later.
 */
static inline long
MAX(long a, long b) {
    return a < b ? b : a;
}


/*
  This function takes the prefix and branching bit of
  two tries and return the branching bit of a new
  branch that would have those two as leaves.

  - if the XOR of the prefixes is negative, then they have
    different signs (2's complement property), so we return
    the minimum long integer, which is the negative number
    represented as '1' followed by all zeros.

  - if the signs agree, then return the highest bit of
    their XOR, p1 ^ p0.
    The initial guess is the largest of
    [ 1, ( 2 * largest of m0, m1 ) ]
*/
static inline long
trie_branch_bit(long p0, long m0, long p1, long m1) {
    return ((p0 ^ p1) < 0) /* opposite signs*/
        ? LONG_MIN
        : trie_highestbit(p0 ^ p1, MAX(1, 2 * MAX(m0, m1)));
}

/*
  Size of the bitmap is one long integer;
  Suffix mask and prefix mask are obtained in a simple way.
*/
#define BITMAP_SIZE ( sizeof(long)*8 )
#define SUFFIX_MASK ( BITMAP_SIZE - 1)
#define PREFIX_MASK ( ~ SUFFIX_MASK  )

/* Extracts the suffix of a fixnum. */
static inline long
trie_suffix(long x) {
    return (long) (x & SUFFIX_MASK);
}

/* Extracts the preffix of a fixnum. */
static inline long
trie_prefix(long x) {
    return x & PREFIX_MASK;
}

static inline int
trie_branchbit_higher(long m1, long m2) {
    return ((m1 ^ m2) < 0) /* different signs? */
        ? (m1 < 0)
        : (m1 > m2);
}

/*
  Returns the bitmap that represents a fixnum.

  trie_fixnum2bitmap ( 0 ) -> 1  (0001)
  trie_fixnum2bitmap ( 1 ) -> 2  (0010)
  trie_fixnum2bitmap ( 2 ) -> 4  (0100)
  ...
 */
static inline  long
trie_fixnum2bitmap(long x) {
    return (long)1 << trie_suffix(x);
}


static inline long
trie_bitmap_delete(long b, long k) {
    return b & (~ trie_fixnum2bitmap(k));
}

static inline long
trie_bitmap_delete_min(long b) {
    return b & (~ trie_lowestbit (b));
}

static inline long
trie_bitmap_delete_max(long b) {
    return b & (~ trie_highestbit(b, trie_lowestbit (b)));
}



/*
  Count the number of ones in a long integer.
  bit_count is defined in fixnum.c
 */
static inline unsigned int
trie_bit_count(unsigned long n) {
    return bit_count(n);
}

/*
  Given a prefix and a bitmap, return the key.
  FIXME: works for a single bit in the bitmap only!
 */
static inline long
STk_pm2key(long prefix, long bitmap) {
    return (prefix | trie_first_set_bit(bitmap));
}




static inline long
merge_bitmaps(long b1, long b2, int merge_op) {
    switch(merge_op) {
    case TRIE_IOR: return b1 | b2;
    case TRIE_XOR: return b1 ^ b2;
    case TRIE_AND: return b1 & b2;
    }
    return 0; /* never reached */
}

/*********************************/
/***                           ***/
/***      END: BIT TRICKS      ***/
/***                           ***/
/*********************************/



/****************************************/
/***                                  ***/
/***      BEGIN: DEBUGGING TRIES      ***/
/***                                  ***/
/****************************************/

/*
  Returns a C string representing a number in binary.
  if full is nonzero, then leading zeros are printed. If not,
  only the relevant bits are printed.
 */
char *
trie_int2bin(unsigned long n, int full) {
    size_t si = full
        ? sizeof(unsigned long)*8
      : (size_t) trie_first_set_bit(n);
    char *res = STk_must_malloc(si+1);

    for (int i=si-1; i>=0; i--, n = n >> 1)
        res[i] = (n & 1) ? '1' : '0';
    res[si] = 0;
    return res;
}

/*
  Returns a SCHEME string representing a number in binary.
  if full is nonzero, then leading zeros are printed. If not,
  only the relevant bits are printed.
*/
SCM trie_int2binSCM (unsigned long v, int full) {
    return STk_Cstring2string(trie_int2bin(v,full));
}

/* debstep is the number of spaces used for indentation when debug-printing
   a trie */
static const unsigned int debstep = 4;

/* Prints a trie, with all its internal data. Extremely useful
   when debugging! */
void
trie_debug(SCM trie, unsigned int depth) {
    unsigned int i;

    for (i=0; i < depth; i++)
        STk_putc(' ', STk_stderr);

    if (!trie) {
        STk_puts("Internal error: void node!", STk_stderr);
        return;
    }

    if (TRIE_EMPTYP(trie)) {
        STk_puts(".\n", STk_stderr);

    } else if (TRIE_LEAFP(trie)) {

        STk_puts("Lf: [ m: ", STk_stderr);
        if (TRIE_CONSTP(trie)) STk_putc('N', STk_stderr); else STk_putc('Y', STk_stderr);
        STk_puts(" k: ", STk_stderr);
        STk_print(MAKE_INT(TRIE_KEY(trie)), STk_stderr, DSP_MODE);
        STk_puts(" p: ", STk_stderr);
        STk_print(trie_int2binSCM(TRIE_PREFIX(trie),1),STk_stderr, DSP_MODE);
        if (ISETP(trie)) {
            STk_puts(" b: ", STk_stderr);
            //STk_print(MAKE_INT(TRIE_BITMAP(trie)), STk_stderr, DSP_MODE);
            STk_print(trie_int2binSCM(TRIE_BITMAP(trie),1),STk_stderr, DSP_MODE);
        }
        if (FXMAPP(trie)) {
            STk_puts(" v: ", STk_stderr);
            STk_print(TRIE_VALUE(trie), STk_stderr, DSP_MODE);
        }
        STk_puts(" ]\n", STk_stderr);

    } else {

        STk_puts("Br: [ m: ", STk_stderr);
        if (TRIE_CONSTP(trie)) STk_putc('N', STk_stderr); else STk_putc('Y', STk_stderr);
        STk_puts(" p: ", STk_stderr);
        STk_print(MAKE_INT(TRIE_PREFIX(trie)), STk_stderr, DSP_MODE);
        STk_puts(" bb: ", STk_stderr);
        STk_print(MAKE_INT(TRIE_BRANCHBIT(trie)), STk_stderr, DSP_MODE);
        STk_puts(" ]\n", STk_stderr);

        for (i=0; i < depth; i++)
            STk_putc(' ', STk_stderr);
        STk_puts("L:\n", STk_stderr);

        trie_debug(TRIE_LEFT(trie), depth + debstep);

        for (i=0; i < depth; i++)
            STk_putc(' ', STk_stderr);
        STk_puts("R:\n", STk_stderr);

        trie_debug(TRIE_RIGHT(trie), depth + debstep);
    }
}

DEFINE_PRIMITIVE("%trie-debug", trie_debug, subr1, (SCM trie))
{
    if (!TRIEP(trie)) STk_error ("bad trie ~S", trie);
    trie_debug(trie,0);
    return STk_void;
}

/*
  Determines the height of a trie.
*/
long
trie_height_aux(SCM trie) {
    /* We choose -1 for the empty tree height... */
    if (TRIE_EMPTYP(trie)) return -1;
    if (TRIE_LEAFP(trie))  return 0;
    return 1 + MAX(trie_height_aux(TRIE_LEFT(trie)),
                   trie_height_aux(TRIE_RIGHT(trie)));
}

/*
<doc EXT fxmapping-height
 * (fxmapping-height trie)
 *
 * Returns the height of the internal trie of an fxmap. The
 * expected running time of searches and insertions is
 * proportional to this value.
doc>
*/
DEFINE_PRIMITIVE("fxmapping-height", trie_fxmap_height, subr1, (SCM trie))
{
    if (!FXMAPP(trie)) STk_error("bad fxmapping ~S", trie);
    return MAKE_INT(trie_height_aux(trie));
}

/*
<doc EXT iset-height
 * (iset-height trie)
 *
 * Returns the height of the internal trie of an iset. The
 * expected running time of searches and insertions is
 * proportional to this value.
doc>
*/
DEFINE_PRIMITIVE("iset-height", trie_iset_height, subr1, (SCM trie))
{
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);
    return MAKE_INT(trie_height_aux(trie));
}

/**************************************/
/***                                ***/
/***      END: DEBUGGING TRIES      ***/
/***                                ***/
/**************************************/



/**************************************/
/***                                ***/
/***    BEGIN: CORE OPERATIONS      ***/
/***                                ***/
/**************************************/

/*
  A direct translation from Scheme of the usual "flatten".
  Works with improper lists (and we will use it on alists):

  If x is ( (1 . a) (2 . b) ), then trie_flatten(x) will be
  (1 a 2 b).

  This is used internally here, AND by the trie reader for
  isets and fxmappings.

  USED BY THE PRINT PROCEDURE, which flattens the alist after
  converting an fxpapping.
 */
SCM
trie_flatten(SCM x) {
    if (NULLP(x))  return x;
    if (!CONSP(x)) return LIST1(x);
    return STk_append2 (trie_flatten(CAR(x)),
                        trie_flatten(CDR(x)));
}

/*
  LEAF: makes leaves

  If value is NULL, an iset leaf is created with prefix + bitmap;
  if value is not NULL, an fxmap leaf is created with key=prefix + value.
*/
SCM
trie_make_leaf(long prefix, long bitmap, SCM value) {
    SCM trie;

    if (value) {
        NEWCELL_WITH_LEN(trie, fxmap, sizeof(struct trie_leaf_fxmap_obj));
        TRIE_VALUE(trie)  = value;
    } else {
        NEWCELL_WITH_LEN(trie, iset, sizeof(struct trie_leaf_iset_obj));
        TRIE_BITMAP(trie) = bitmap;
    }

    if (value == NULL && bitmap == 0)
        BOXED_INFO(trie) |= TRIE_EMPTY;
    else {
        BOXED_INFO(trie)  |= TRIE_LEAF;
        TRIE_PREFIX(trie)  = prefix;
    }

    return trie;
}

/*
  BRANCH: makes branches

  Just a basic constructor. No special tricks used.
 */
SCM
trie_make_branch(long prefix, long branch_bit, SCM left, SCM right, int is_fxmap) {
    SCM trie;

    if (TRIE_EMPTYP(left))  return right;
    if (TRIE_EMPTYP(right)) return left;

    if (is_fxmap) {
        NEWCELL_WITH_LEN(trie, fxmap, sizeof(struct trie_branch_obj));
    } else {
        NEWCELL_WITH_LEN(trie, iset, sizeof(struct trie_branch_obj));
    }

    TRIE_PREFIX(trie)    = prefix;
    TRIE_BRANCHBIT(trie) = branch_bit;
    TRIE_LEFT(trie)      = left;
    TRIE_RIGHT(trie)     = right;

    return trie;
}


/*
  JOIN:

  This takes two DISJOINT tries and join them.

  t0, t1: two tries
  p0, p1: longest common prefixes for t0, t1
  m0, m1: branching bits of t0, t1

  If t0 is a leaf, then p0 is its key (same for t1, of course).
 */
SCM
trie_join(long p0, long m0, SCM t0, long p1, long m1, SCM t1) {

    /* m is the first bit in which p0 and p1 disagree -- the
       branching bit for the new branch! */
    long m = trie_branch_bit(p0, m0, p1, m1);

    int fxmap = FXMAPP(t0);

    /* trie_zerobit(p0,m) decides if p0 has the mth-bit zero'ed.  If
       it does, and **that is the first disagreeing bit**, then it means
       t0 should be to the left of t1! Otherwise, t0 goes to the right. */
    SCM trie = trie_zerobit(p0, m)
        ? trie_make_branch(trie_mask(p0, m), m, t0, t1, fxmap)
        : trie_make_branch(trie_mask(p0, m), m, t1, t0, fxmap);

    /* trie_debug(trie, 0);*/

    return trie;
}

/*
  INSERT:
  =======

  Inserts a new element into trie and returns a NEW trie.

  ARGUMENTS:
  ----------

  - trie: the trie in which we'll be inserting
  - prefix (=key for fxmaps)
  - bitmap (must be 0 for fxmaps)
  - value  (must be NULL for isets)
  - proc   (ignored for isets): called when deciding which value to use if duplicate keys are
                                found in an fxmap
  - swap: swap arguments of proc? (v1 <-> v2)
  - merge_op: the operation used when inserting
    * For ISETs, it is used to specify the operations on bitmaps.
      For FXMAPs, it specifies how duplicate keys are managed:
      + TRIE_NOP: unused
      + TRIE_IOR:
        . trie_aux uses when inserting into a leaf
        . trie_map_for_each uses to create leaves from bitmaps AND to merge branches
        . trie_partition_aux uses to insert into the two partitions
        . trie_list_trie_aux uses to insert into the new structure
        . iset-adjoin uses when inserting
        . %iset-union, %fxmapping-union
      + TRIE_XOR: used by iset-xor
      + TRIE_AND: unused
      + TRIE_XOR: used by fxmalling-xor (duplicates are deleted)
      + TRIE_UPDATE: if proc is given, runs  (proc k v-new v-old) to get the new
                     value; otherwise, force the update with the new one.
      + TRIE_ADJUST: always run (proc k v-old) to get the new value (there is no
                     "new" value, see fxmapping-adjust)
      + TRIE_UNION: used by %fxmapping-union

  WHO CALLS THIS FUNCTION:
  ------------------------

  This is particularly relevant when inserting into an fxmapping, because
  when a duplicate key is being inserted we need to decide which value it
  will be associated to: the old one, the new one, or the result of a
  procedure.

  Internal:

  - trie_aux, which in turn is called by:
    * fxmapping                   [ old values are preserved ]
    * alist->fxmapping            [ old values are preserved ]
    * alist->fxmapping/combinator [ when dup: (proc k v2 v1) ]
    * iset
    * list->iset

  - trie_merge, via:
    * iset-xor
    * iset-union
    * fxmapping-xor      [ no conflicts, dup keys are deleted ]
    * %fxmapping-union   [ old values are preserved ]
    * del_min_max_aux    [ no conflicts, nodes are deleted ]
    * trie_map_for_each

  - trie_map_for_each
    * %iset-map  [ to collect results of map into a new trie ]

  - trie_partition_aux, via:
    * iset-partition
    * iset-filter
    * fxmapping-partition [ inserts into the partitions,   no conflicts ]
    * fxmapping-filter    [ inserts into the filtered set, no conflicts ]

  - trie_list_trie_aux
    * list->iset
    * alist->fxmapping/combinator [ when dup: (proc k v2 v1) ]
    * alist->fxmapping            [ old values are preserved ]

  Scheme primitives:

  - fxmapping-adjoin            [ old values are preserved ]
  - fxmapping-set               [ new values override old  ]
  - fxmapping-adjust            [ k? -> (proc k v)   no k? -> same trie ]
  - fxmapping-adjoin/combinator [ when dup: (proc k v2 v1) ]
  - iset-adjoin

*/
SCM
trie_insert_aux(SCM trie, long prefix, unsigned long bitmap, SCM value, SCM proc, int swap, int merge_op) {

    long key = 0;
    if FXMAPP(trie) key = prefix;

    if (TRIE_EMPTYP(trie))
        /* fxmapping-adjust wants us to *only* adjust, but not insert
           associations: */
        return (merge_op == TRIE_ADJUST)
            ? trie
            : trie_make_leaf(prefix,bitmap,value);

    if (TRIE_LEAFP(trie)) {

        if (FXMAPP(trie) && (TRIE_KEY(trie) == key)) {
            SCM val;
            switch (merge_op) {
            case TRIE_UPDATE:
                val = proc     /* was proc set? */
                    ? (swap    /* swap args? */
                       ? STk_C_apply(proc,3,key,TRIE_VALUE(trie),value)
                       : STk_C_apply(proc,3,key,value,TRIE_VALUE(trie)))
                    : value;
                break;
            case TRIE_ADJUST:
                val = STk_C_apply(proc,2,MAKE_INT(key),TRIE_VALUE(trie));
                break;
            case TRIE_XOR:
                val = NULL; /* with bitmap=0, will result in empty tree */
                break;
            case TRIE_UNION:
                /* Union is performed from right to left, so we invert the logic: */
                val = value;
                break;
            default:
                return trie; /* keep old value */
            }
            return trie_make_leaf(key, 0, val);
        }

        if (ISETP(trie) & (prefix == TRIE_PREFIX(trie)) )
            return trie_make_leaf(prefix, merge_bitmaps(TRIE_BITMAP(trie), bitmap,merge_op), NULL);

        /* make another leaf and join */
        return trie_join(prefix,
                             0, /* m_0 = 0 for leaf  */
                             trie_make_leaf(prefix, bitmap, value),

                             TRIE_PREFIX(trie),
                             0, /* m_1 = 0 for leaf  */
                             trie);
    }

    /* this is a branch. we see if our key matches the prefix; if it does,
       we insert into this branch: */
    if (trie_match_prefix(prefix,
                          TRIE_PREFIX(trie),
                          TRIE_BRANCHBIT(trie)))
        /* insert on left or right: */
        if (trie_zerobit(prefix, TRIE_BRANCHBIT(trie)))
            return trie_make_branch(TRIE_PREFIX(trie),
                                    TRIE_BRANCHBIT(trie),
                                    trie_insert_aux(TRIE_LEFT(trie),prefix,bitmap,value,
                                                    proc,NO_SWAP,merge_op),
                                    TRIE_RIGHT(trie),
                                    FXMAPP(trie));
        else
            return trie_make_branch(TRIE_PREFIX(trie),
                                    TRIE_BRANCHBIT(trie),
                                    TRIE_LEFT(trie),
                                    trie_insert_aux(TRIE_RIGHT(trie),prefix,bitmap,value,
                                                    proc,NO_SWAP,merge_op),
                                    FXMAPP(trie));
    else
        /* ok, so our prefix didn't match the branch's prefix. we just
           create a leaf and join it with the branch. */
        return trie_join(prefix, 0, trie_make_leaf(prefix, bitmap, value),
                             TRIE_PREFIX(trie), TRIE_BRANCHBIT(trie), trie);

    STk_error ("What happened?");
    return MAKE_INT((unsigned long) -1);
}


/*
  LOOKUP:

  Looks up the value of key in trie.

  When key is in trie:
  - For fxmaps, return the mapped value
  - For isets, return #t

  When key is not in trie:
  - if def is NULL, an error is signaled
  - if def is not NULL, it is returned
 */
SCM
trie_lookup_aux(long key, SCM trie, SCM def) {
    if (!TRIE_EMPTYP(trie)) {
        if (TRIE_LEAFP(trie)) {

            if (FXMAPP(trie) && (key == TRIE_KEY(trie))) {
                /* FXMAP, match! */
                return TRIE_VALUE(trie);

            } else if ( (trie_prefix(key) == TRIE_PREFIX(trie)) &&
                        (trie_fixnum2bitmap(key) & TRIE_BITMAP(trie)) ) {
                /* ISET, match! */
                return MAKE_INT(key);
            }


        } else {
            /* branch, recurse! */
            if (trie_match_prefix(key, TRIE_PREFIX(trie), TRIE_BRANCHBIT(trie)))
                return trie_zerobit(key, TRIE_BRANCHBIT(trie))
                    ? trie_lookup_aux(key, TRIE_LEFT(trie),  def)
                    : trie_lookup_aux(key, TRIE_RIGHT(trie), def);
        }
    }
    if (!def) STk_error("key not found ~S", MAKE_INT(key));
    return def;
}

/*
  Returns an empty trie.
 */
SCM
trie_make_empty(int is_fxmap) {
    SCM trie;
    if (is_fxmap) {
        NEWCELL_WITH_LEN(trie, fxmap, sizeof(struct trie_empty_obj));
    } else {
        NEWCELL_WITH_LEN(trie, iset, sizeof(struct trie_empty_obj));
    }

    BOXED_INFO(trie) |= TRIE_EMPTY;
    return trie;
}

/*
  Builds a new trie from a variable number of arguments.
 */
SCM
trie_aux(int constant, int is_fxmap, int argc, SCM *argv) {
    if (is_fxmap && (argc%2)) STk_error("odd number of arguments to fxmapping");

    SCM trie = trie_make_empty(is_fxmap);

    if (constant)   BOXED_INFO(trie) |= TRIE_CONST;

    if (argc == 0) return trie;

    SCM *ptr = argv;

    /* When the trie has values associated with keys, we walk the
       argument list TWO at a time; when not, we do that ONE at a time. */
    int step = 1 + is_fxmap;

    for (int i=argc;   i>0;   i -= step , ptr -= step) {
        if (!INTP(*ptr)) STk_error("bad integer ~S", *ptr);
        trie = is_fxmap
            ? trie_insert_aux(trie, INT_VAL(*ptr), 0, *(ptr-1), NULL,NO_SWAP,TRIE_NOP)
            : trie_insert_aux(trie,
                              trie_prefix(INT_VAL(*ptr)),
                              trie_fixnum2bitmap(INT_VAL(*ptr)),
                              NULL, NULL,NO_SWAP,TRIE_IOR);
        if (constant)   BOXED_INFO(trie) |= TRIE_CONST;
    }
    /* Not empty anymore: */
    BOXED_INFO(trie) &= (~TRIE_EMPTY);
    return trie;
}

/*
  Deletes from a trie.
  Not used directly, but internally by other functions.
 */
SCM
trie_delete1(SCM trie, long prefix, long bitmap) {
    if (TRIE_EMPTYP(trie)) return trie_make_empty(FXMAPP(trie));
    long p = TRIE_PREFIX(trie);
    long b = TRIE_BITMAP(trie);

    if (TRIE_LEAFP(trie)) {
        if (FXMAPP(trie)) {
            return (prefix == TRIE_KEY(trie))
                ? trie_make_empty(FXMAPP(trie))
                : trie;
        } else if (p == prefix) {
            /* iset, a leaf can have several values */
            b = (~bitmap) & b;
            return trie_make_leaf(p, b, NULL);
        }
    }

    long m = TRIE_BRANCHBIT(trie);
    SCM  l = TRIE_LEFT(trie);
    SCM  r = TRIE_RIGHT(trie);


    /* It's a branch! */
    if (trie_match_prefix(prefix, p, m)) {

        return (trie_zerobit(prefix, m))
            ? trie_make_branch(p, m,
                                   trie_delete1(l, prefix, bitmap), r,
                                   FXMAPP(trie))
            : trie_make_branch(p, m,
                                   l, trie_delete1(r, prefix, bitmap),
                                   FXMAPP(trie));
    } else {
        return trie;
    }
    return STk_false; /* Never reached */
}

/*
  Set difference, s \ t.
 */
SCM
trie_difference_aux(SCM s, SCM t) {
    if (TRIE_EMPTYP(s)) return trie_make_empty(FXMAPP(s));
    if (TRIE_EMPTYP(t)) return s;
    if (TRIE_LEAFP(t))  return trie_delete1(s, TRIE_PREFIX(t),TRIE_BITMAP(t));
    if (TRIE_LEAFP(s)) {
        /* take the key from s.
           if the key is not in t, return s;
           if the key is in t, nothing is left in s, return empty.  */
        if (FXMAPP(s)) {
            /*
              Hack: we need to pass something as the last parameter that would
              NOT possibly be a SCM value that the user can pass, and NOT NULL.
              Create a variable and pass its address (since SCM is "void *").
            */
            char* __ref = "__ref";
            return (trie_lookup_aux(TRIE_KEY(s), t, __ref) == __ref)
                ? s
                : trie_make_empty(FXMAPP(s));
        } else {

            long p = TRIE_PREFIX(s);
            long q;
            long m;
            SCM  l;
            SCM  r;
            while (1) {

                if (TRIE_EMPTYP(t)) return s;

                q = TRIE_PREFIX(t);
                m = TRIE_BITMAP(t);
                if (TRIE_LEAFP(t)) {
                    if (p == q) return trie_delete1(s, q, m);
                    else        return s;
                }

                l = TRIE_LEFT(t);
                r = TRIE_RIGHT(t);
                if (trie_match_prefix(p,q,m))
                    if (trie_zerobit(p,m))  t = l;
                    else                    t = r;
                /* prefixes don't match, return s as it is: */
                else return s;
            }
        }
    }

    /* Both t and s are branches! */

    long p = TRIE_PREFIX(s);
    long q = TRIE_PREFIX(t);
    long m = TRIE_BRANCHBIT(s);
    long n = TRIE_BRANCHBIT(t);
    SCM sl = TRIE_LEFT(s);
    SCM sr = TRIE_RIGHT(s);
    SCM tl = TRIE_LEFT(t);
    SCM tr = TRIE_RIGHT(t);

    /* same prefix, same branching bit, just recurse: */
    if ((m == n) && (p == q))
        return trie_make_branch(p, m,
                                    trie_difference_aux(sl, tl),
                                    trie_difference_aux(sr, tr),
                                    FXMAPP(s));

    if (trie_branchbit_higher(m,n) &&
        trie_match_prefix(q,p,m))
        return (trie_zerobit(q,m))
            ? trie_make_branch(p,m, trie_difference_aux(sl, t), sr,FXMAPP(s))
            : trie_make_branch(p,m, sl, trie_difference_aux(sr, t),FXMAPP(s));

    if (trie_branchbit_higher(n,m) &&
        trie_match_prefix(p,q,n))
        return (trie_zerobit(p,n))
            ? trie_difference_aux(s, tl)
            : trie_difference_aux(s, tr);

    return s;
}

/*
  Merge two tries.

  - "decide" is the procedure that should be used to obtain the new value when there
    are duplicate keys in a mapping
  - "merge_op" is the merge operation (see the comment before trie_insert_aux for a
    detailed explanation)
 */
SCM
trie_merge(SCM decide, int merge_op, SCM a, SCM b) {
    if (TRIE_EMPTYP(a)) return b;
    if (TRIE_EMPTYP(b)) return a;

    long p = TRIE_PREFIX(a);
    long q = TRIE_PREFIX(b);
    int is_fxmap = FXMAPP(a);

    if (TRIE_LEAFP(a)) {
        if (is_fxmap) return trie_insert_aux(b, p, 0,     TRIE_VALUE(a), decide, NO_SWAP, merge_op);
        else          return trie_insert_aux(b, p, TRIE_BITMAP(a), NULL, decide, NO_SWAP, merge_op);
    }
    if (TRIE_LEAFP(b)) {
        /* We are changing the order of a and b, so we pass SWAP to the insert
           procedure. When "decide" is called, its arguments will be swapped too. */
        if (is_fxmap) return trie_insert_aux(a, q, 0,     TRIE_VALUE(b), decide, SWAP, merge_op);
        else          return trie_insert_aux(a, q, TRIE_BITMAP(b), NULL, decide, SWAP, merge_op);
    }

    long m = TRIE_BRANCHBIT(a);
    long n = TRIE_BRANCHBIT(b);
    SCM left_a  = TRIE_LEFT(a);
    SCM right_a = TRIE_RIGHT(a);
    SCM left_b  = TRIE_LEFT(b);
    SCM right_b = TRIE_RIGHT(b);

    if (m == n && p == q)
        return trie_make_branch(p, m,
                                trie_merge(decide, merge_op, left_a, left_b),
                                trie_merge(decide, merge_op, right_a, right_b),
                                is_fxmap);

    if (trie_branchbit_higher(m,n) &&
        trie_match_prefix(q,p,m))
        if (trie_zerobit(q,m))
            return trie_make_branch(p,m,
                                        trie_merge(decide, merge_op, left_a, b),
                                        right_a,
                                        is_fxmap);
        else
            return trie_make_branch(p,m,
                                        left_a,
                                        trie_merge(decide, merge_op, right_a, b),
                                        is_fxmap);
    else if (trie_branchbit_higher(n,m) &&
             trie_match_prefix(p,q,n))
        if (trie_zerobit(p,n))
            return trie_make_branch(q,n,
                                    trie_merge(decide, merge_op, a,left_b),
                                    right_b,
                                    is_fxmap);
        else
            return trie_make_branch(q,n,
                                    left_b,
                                    trie_merge(decide,merge_op,a,right_b),
                                    is_fxmap);
    else return trie_join(p,m,a, q,n,b);
    return STk_void; /* never reached */
}


/*
  Map/for-each implementation. Applies proc on all values of trie, in
  ascending key order.

  MAP: when collect is nonzero, it collects the result in a new trie;
  FOR-EACH: when collect == 0, it only applies proc, but returns void.
 */
SCM
trie_map_for_each(SCM trie, SCM collect, SCM proc) {
    if (TRIE_EMPTYP(trie)) return (collect == STk_true)
                               ? trie_make_empty(FXMAPP(trie))
                               : STk_void;

    if (TRIE_LEAFP(trie)) {
        if (FXMAPP(trie)) {
            SCM res = STk_C_apply(proc, 2,
                                  MAKE_INT(TRIE_KEY(trie)),
                                  TRIE_VALUE(trie));

            SCM x = trie_make_leaf(TRIE_KEY(trie), 0, res);
            return (collect == STk_true)?  x : STk_void;
        } else {
            /* run through the leaf bitmap and process each number */
            SCM t = trie_make_empty(0); /* 0 == not an fxmap, but an iset */
            long p = TRIE_PREFIX(trie);
            long b = TRIE_BITMAP(trie);
            while (b) {
                SCM res = STk_C_apply(proc, 1, MAKE_INT(STk_pm2key(p, b)));
                if ((collect==STk_true) && (!INTP(res)))
                    STk_error ("mapping procedure produced non-fixnum ~S", res);

                long x  = INT_VAL(res);
                long q  = trie_prefix(x);
                long bm = trie_fixnum2bitmap(x);
                t = trie_insert_aux(t, q, bm, NULL, NULL, NO_SWAP, TRIE_IOR);
                b = b & (~trie_lowestbit(b));
            }
            return t;
        }
    }

    /* It's a branch! */
    if (collect == STk_true) {
        SCM left;
        SCM right;
        if (TRIE_BRANCHBIT(trie) < 0) {
            right = trie_map_for_each(TRIE_RIGHT(trie), collect, proc);
            left  = trie_map_for_each(TRIE_LEFT(trie),  collect, proc);
        } else {
            left  = trie_map_for_each(TRIE_LEFT(trie),  collect, proc);
            right = trie_map_for_each(TRIE_RIGHT(trie), collect, proc);
        }
        return trie_merge(NULL, TRIE_IOR, left, right);
    }

    /* No result needed, it's a for-each operation. Just recurse and return
       void: */

    if (TRIE_BRANCHBIT(trie) < 0) {
        trie_map_for_each(TRIE_RIGHT(trie), collect, proc);
        trie_map_for_each(TRIE_LEFT(trie),  collect, proc);
    } else {
        trie_map_for_each(TRIE_LEFT(trie),  collect, proc);
        trie_map_for_each(TRIE_RIGHT(trie), collect, proc);
    }
    return STk_void;
}


/*
  Counts the number of elements in a trie.
*/
long
trie_count_aux(SCM trie) {
    if (TRIE_EMPTYP(trie)) return 0;
    if (TRIE_LEAFP (trie)) return FXMAPP(trie)
                               ? 1
                               : trie_bit_count(TRIE_BITMAP(trie));
    return
        trie_count_aux(TRIE_LEFT(trie)) +
        trie_count_aux(TRIE_RIGHT(trie));
}

/*
  Returns the minimum or maximum element of a trie, or #f if
  the trie is empty.
 */
SCM
trie_min_max_aux(SCM trie, int want_max) {
    if (TRIE_EMPTYP(trie)) return STk_false;

    while (1) {
        if (TRIE_LEAFP(trie)) {
            long p = TRIE_PREFIX(trie);
            long b = TRIE_BITMAP(trie);

            if (FXMAPP(trie)) return STk_n_values(2, MAKE_INT(TRIE_KEY(trie)), TRIE_VALUE(trie));
            else {
                return want_max
                    ? MAKE_INT(STk_pm2key(p, trie_highestbit(b, 1)))
                    : MAKE_INT(STk_pm2key(p, b));
            }
        }

        /* It's a branch! */

        /* If the branching bit is <0, we swap the logic and go to the right for
           minimum or left for maximum, because INTERNALLY, negatives go to the right
           of positives in the tree. */
        if (want_max)
            trie = (TRIE_BRANCHBIT(trie) < 0)
                ? TRIE_LEFT(trie)
                : TRIE_RIGHT(trie);
        else /* min */
            trie = (TRIE_BRANCHBIT(trie) < 0)
                ? TRIE_RIGHT(trie)
                : TRIE_LEFT(trie);
    }
    STk_error("trie_min_max internal error");
    return STk_void; /* never reached */
}


/*
  Delete min or max from a trie.

  Returns a PAIR,
  CAR = the element deleted
  CDR = the new trie, without it
 */
SCM
trie_del_min_max_aux(SCM trie, int want_max) {
    long p = TRIE_PREFIX(trie);

    if (TRIE_LEAFP(trie)) {
        long b = TRIE_BITMAP(trie);

        /* If it's an FXMAP and we're looking for MIN or MAX, then we'll only find
           ONE leaf. Ever. And that is exactly the one we want to delete, so
           we just return an empty tree. */
        if (FXMAPP(trie)) return  STk_cons(STk_cons(MAKE_INT(TRIE_KEY(trie)),
                                                    TRIE_VALUE(trie)),
                                           trie_make_empty(FXMAPP(trie)));

        /* It's an ISET, and we use the bitmap-deleting functions: */
        return want_max
            ? STk_cons(MAKE_INT(STk_pm2key(p,trie_highestbit(b,1))),
                       trie_make_leaf(p,
                                          trie_bitmap_delete_max(b),
                                          NULL))
            : STk_cons(MAKE_INT(STk_pm2key(p,b)),
                       trie_make_leaf(p,
                                          trie_bitmap_delete_min(b),
                                          NULL));
    }

    /* It's a branch! */

    SCM l = TRIE_LEFT(trie);
    SCM r = TRIE_RIGHT(trie);
    long bb = TRIE_BRANCHBIT(trie);

    SCM pair, left, right, deleted;

    /* If the branching bit is <0, we swap the logic and go to the right for
       minimum or left for maximum, because INTERNALLY, negatives go to the right
       of positives in the tree. */
    if (want_max)
        if (bb < 0) {
            right   = r;
            pair    = trie_del_min_max_aux(l, want_max);
            left    = CDR(pair);
        } else {
            left    = l;
            pair    = trie_del_min_max_aux(r, want_max);
            right   = CDR(pair);
        }
    else /* min */
        if (bb < 0) {
            left    = l;
            pair    = trie_del_min_max_aux(r, want_max);
            right   = CDR(pair);
        } else {
            right   = r;
            pair    = trie_del_min_max_aux(l, want_max);
            left    = CDR(pair);
        }

    deleted = CAR(pair);

    /* We pass NULL for "decide" and "TRIE_NOP" for merge_op when we call trie_merge
       because we know that the two tries are distincts, so these won't be used! */
    return STk_cons(deleted,
                    trie_merge(NULL, TRIE_NOP, left, right));

    STk_error("trie_min_max internal error");
    return STk_void; /* never reached */
}

/*
  Deletes min or max from a trie. This is just a wrapper that checks for
  empty tries before calling the function that does the actual work.
 */
SCM
trie_del_min_max(SCM trie, int want_max) {
    if (TRIE_EMPTYP(trie)) {
        if FXMAPP(trie) STk_error("cannot delete from empty fxmapping");
        else            STk_error("cannot delete from empty iset");
    }
    return trie_del_min_max_aux(trie,want_max);
}

/*
  Turns isets into lists and fxmappings into alists.
 */
SCM trie_list_aux(SCM trie, SCM tail) {
    if (TRIE_EMPTYP(trie)) return STk_nil;
    if (TRIE_LEAFP(trie))
        if (FXMAPP(trie))
            return STk_cons( STk_cons(MAKE_INT(TRIE_KEY(trie)), TRIE_VALUE(trie)), tail );
        else {
            /* run through the bitmap to get all numbers, from RIGHT to LEFT,
               because we're consing, and the list will be in reverse order.

              FIXME: shouldn't we just do left->right and the reverse at the
              end? trie_highest_bit has time O(n),and we're calling it
              for every set bit. */
            long  p = TRIE_PREFIX(trie);
            long  b = TRIE_BITMAP(trie);
            long val;
            long hi;
            while (b) {
                hi = trie_highestbit(b, 1);
                val = trie_last_set_bit(hi);
                tail = STk_cons( MAKE_INT( p + val ), tail );
                b = b ^ hi;
            }
            return tail;
        }
    else return (TRIE_BRANCHBIT(trie)<0)
             ? trie_list_aux(TRIE_RIGHT(trie), trie_list_aux(TRIE_LEFT (trie), tail))
             : trie_list_aux(TRIE_LEFT (trie), trie_list_aux(TRIE_RIGHT(trie), tail));
}

/*
  Implementation of fold.
  When from_right is not zero, fold is done from right to left.
 */
SCM
trie_fold(SCM proc, SCM nil, SCM trie, int from_right)
{
    if (TRIE_EMPTYP(trie)) return nil;
    SCM last = nil;
    if (TRIE_LEAFP(trie))  {
        if (FXMAPP(trie))
            return STk_C_apply(proc, 3, MAKE_INT(TRIE_KEY(trie)), TRIE_VALUE(trie), last);
        else {
            long p = TRIE_PREFIX(trie);
            long b = TRIE_BITMAP(trie);
            long hi;
            long cur;
            while (b) {
                if (from_right) {
                    hi = trie_highestbit(b, 1);
                    cur = p + trie_last_set_bit(hi);
                } else
                    cur = STk_pm2key(p, b);
                last = STk_C_apply(proc, 2, MAKE_INT(cur), last);
                b = from_right
                    ? b ^ hi
                    : b & (~trie_lowestbit(b));
            }
            return last;
        }

    }
    /* branch */
    int r = (TRIE_BRANCHBIT(trie)<0) ? from_right : !from_right;

    if (r) {
        last = trie_fold(proc, nil,  TRIE_LEFT(trie),  from_right);
        return trie_fold(proc, last, TRIE_RIGHT(trie), from_right);
    } else {
        last = trie_fold(proc, nil,  TRIE_RIGHT(trie), from_right);
        return trie_fold(proc, last, TRIE_LEFT(trie),  from_right);
    }
}

/*
  Partitions the trie into two others.
  Used as a building block for delete, filter and partition.

  - sets is a pair of tries: ( A, B )
    A (car) = those for which pred results in #t
    B (cdr) = those for which pred results in #f

    Pass STk_nil for both when calling - that is, when this
    function is called by others, "sets" should be
    CONS(STk_nil, STk_nil).

  - partition is a flag: when 0, the set B is not build.
    this is useful when we use this function as primitive
    for iset-filter and fxmapping-filter.
 */
void
trie_partition_aux(SCM pred, SCM trie, SCM sets, int partition) {
    /* If the result sets do not yet exist, create them: */
    if (CAR(sets) == STk_false) {
        CAR(sets) = trie_make_empty(FXMAPP(trie));
        if (partition)
            CDR(sets) = trie_make_empty(FXMAPP(trie));
    }

    /* No elements, just return! */
    if (TRIE_EMPTYP(trie)) return;

    if (TRIE_LEAFP(trie)) {
        if (FXMAPP(trie)) {
            if (STk_C_apply(pred, 2,
                            MAKE_INT(TRIE_KEY(trie)),
                            TRIE_VALUE(trie)) == STk_true)

                CAR(sets) = trie_insert_aux(CAR(sets),
                                                TRIE_KEY(trie),
                                                0,
                                                TRIE_VALUE(trie),NULL,NO_SWAP,TRIE_NOP);
            else if (partition)
                CDR(sets) = trie_insert_aux(CDR(sets),
                                                TRIE_KEY(trie),
                                                0,
                                                TRIE_VALUE(trie),NULL,NO_SWAP,TRIE_NOP);
        } else {
            /* run through the bitmap to get all numbers, from RIGHT to LEFT */
            long  p = TRIE_PREFIX(trie);
            long  b = TRIE_BITMAP(trie);
            while (b) {
                long val = STk_pm2key(p, b);
                int success = (STk_C_apply(pred, 1, MAKE_INT(val)) == STk_true);
                if (success)
                    CAR(sets) = trie_insert_aux(CAR(sets),
                                                    p,
                                                    trie_lowestbit(b),
                                                    NULL,NULL,NO_SWAP,TRIE_IOR);
                if ((!success) && partition)
                    CDR(sets) = trie_insert_aux(CDR(sets),
                                                    p,
                                                    trie_lowestbit(b),
                                                    NULL,NULL,NO_SWAP,TRIE_IOR);
                b = b & (~trie_lowestbit(b));
            }
        }
        return;


    } else {
        /* branch -- just recurse! */
        trie_partition_aux(pred,TRIE_LEFT(trie),  sets, partition);
        trie_partition_aux(pred,TRIE_RIGHT(trie), sets, partition);
    }
}


/*
  Compares two leaves for set-theoretic inclusion.

  Returns:
   -1 if s is subset of t
    0 if s is equal to t
   +1 if s is superset of t
   +2 if s and t are disjoint

  s and t must be leaves
  compare is a procedure used to compare values for *equality* in fxmappings.
 */
long
trie_compare_leaves(SCM s, SCM t, SCM compare) {
    if (FXMAPP(s)) {
        return ( (TRIE_KEY(s) == TRIE_KEY(t)) &&
                 (STk_C_apply(compare,2,TRIE_VALUE(s),TRIE_VALUE(t)) == STk_true) )
            ? 0  /* equal    */
            : 2; /* disjoint */
    }
    /* ISET: */
    long p  = TRIE_PREFIX(s);
    long q  = TRIE_PREFIX(t);
    long ba = TRIE_BITMAP(s);
    long bb = TRIE_BITMAP(t);
    if ( (p == q) && (ba == bb) ) return 0;           /* s == t */
    if ( (p == q) && ((ba & (~bb)) == 0) ) return -1; /* s in t */
    if ( (p == q) && ((bb & (~ba)) == 0) ) return +1; /* t in s */
    /* Now, either we have different prefixes, and the leafs are
       disjoint sets, or we have the same prefix, but the bitmaps
       are disjoint. Return +2. */
    return +2;
}



/* Forward declaration, because we have MUTUALLY RECURSIVE
   functions! */
/* FIXME: NOT TAIL RECURSIVE */
long trie_compare(SCM s, SCM t, SCM compare);

/*
  Compares a leaf and a branch for set-theoretic inclusion.

  Returns:
   -1 if leaf is subset of branch
    0 if leaf is equal to branch
   +1 if leaf is superset of branch
   +2 if leaf and branch are disjoint

  compare is a procedure used to compare values for *equality* in fxmappings.
*/
long
trie_compare_leaf_branch(SCM leaf, SCM branch, SCM compare) {
    long p = TRIE_PREFIX(leaf);
    long q = TRIE_PREFIX(branch);
    long m = TRIE_BRANCHBIT(branch);
    if (trie_match_prefix(p,q,m))
        return trie_zerobit(p,m)
            ? trie_compare(leaf,TRIE_LEFT(branch),  compare)
            : trie_compare(leaf,TRIE_RIGHT(branch), compare);
    return +2; /* Different prefixes, disjoint sets! */
}

/*
  Compares two tries for set-theoretic inclusion.

  Returns
   -1 if s is subset of t
    0 if s is equal to t
   +1 if s is superset of t
   +2 if s and t are disjoint

   compare is a procedure used to compare values for *equality* in fxmappings.
 */
long
trie_compare(SCM s, SCM t, SCM compare) {
    /* two empty sets are equal: */
    if ((STk_eqv(s,t) == STk_true) ||
        (TRIE_EMPTYP(s) && TRIE_EMPTYP(t)))
        return 0;

    /* empty is subset of whatever other: */
    if (TRIE_EMPTYP(s)) return -1;
    if (TRIE_EMPTYP(t)) return +1;

    /* two leaves, just compare them: */
    if (TRIE_LEAFP(s) && TRIE_LEAFP(t)) return trie_compare_leaves(s, t, compare);

    /* one leaf and one branch: */
    if (TRIE_LEAFP(s))
        switch(trie_compare_leaf_branch(s,t,compare)) {
            /* if the result was = or <, then we have <.
               (a leaf cannot be "equal" to a branch) */
        case  0:
        case -1: return -1;

            /* +1 shouldn't happen, as a leaf cannot be
               a superset of a branch. */
        case +1:
        case +2: return +2;
        }

    /* same as before, but swapping s <-> t and +1 <-> -1 */
    if (TRIE_LEAFP(t))
        switch (trie_compare_leaf_branch(t,s,compare)) {
        case  0:
        case +1: return +1;
        case -1:
        case +2: return +2;
        }

    /* we have two branches: */
    long p = TRIE_PREFIX(s);
    long q = TRIE_PREFIX(t);
    long m = TRIE_BRANCHBIT(s);
    long n = TRIE_BRANCHBIT(t);

    /* FIXME: calling trie_compare in non-tail position (can we do better?) */

    if (trie_branchbit_higher(m,n)) {
        /* s is at least larger than t. maybe superset */
        if (trie_match_prefix(q,p,m)) {
            /* re go down the larger trie, s. if we find a subtrie of S which is
               either EQUAL or a SUPERSET of t, then we answer SUPERSET. */
            long sub = (trie_zerobit(q,m))
                ? trie_compare(TRIE_LEFT(s),t,compare)
                : trie_compare(TRIE_RIGHT(s),t,compare);
            if (sub == 0 || sub == +1) return +1;
        }
        return +2;
    }

    if (trie_branchbit_higher(n, m)) {
        /* s is at smaller larger than t. maybe subset */
        if (trie_match_prefix(p,q,n)) {
            /* re go down the larger trie, t. if we find a subtrie of s which is
               either EQUAL or a SUBSET of t, then we answer SUBSET. */
            long sub = (trie_zerobit(p,n))
                ? trie_compare(s, TRIE_LEFT(t),compare)
                : trie_compare(s, TRIE_RIGHT(t),compare);
            if (sub == 0 || sub == -1) return -1;
        }
        return +2;
    }

    if (p == q) {
        long l = trie_compare(TRIE_LEFT(s),TRIE_LEFT(t),compare);
        long r = trie_compare(TRIE_RIGHT(s),TRIE_RIGHT(t),compare);
        if (l == r) return r;
        if (l == 0) return r;
        if (r == 0) return l;
        return +2;
    }
    return +3;
}


/*
  Turns lists into isets and alists into fxmappings.
*/
SCM
trie_list_trie_aux (SCM list, int is_fxmap, SCM combine) {
    long k, b = 0;
    SCM  v = NULL;

    SCM trie = trie_make_empty(is_fxmap);

    if (NULLP(list)) return trie;

    while (!NULLP(list)) {
        if (!CONSP(list)) STk_error ("bad list ~S", list);

        if (is_fxmap) {
            if (!CONSP(CAR(list))) STk_error ("alist member not a cons pair ~S", CAR(list));
            if (!INTP(CAR(CAR(list)))) STk_error("bad integer ~S", CAR(list));
            k = INT_VAL(CAR(CAR(list)));
            v = CDR(CAR(list));
            trie = combine
                ? trie_insert_aux(trie, k, b, v, combine, NO_SWAP, TRIE_UPDATE)
                : trie_insert_aux(trie, k, b, v, combine, NO_SWAP, TRIE_NOP);
        } else {
            if (!INTP(CAR(list))) STk_error("bad integer ~S", CAR(list));
            k = trie_prefix(INT_VAL(CAR(list)));
            b = trie_fixnum2bitmap(INT_VAL(CAR(list)));
            trie = trie_insert_aux(trie, k, b, v, combine, NO_SWAP, TRIE_IOR);
        }
        list = CDR(list);
    }
    BOXED_INFO(trie) &= (~TRIE_EMPTY);

    return trie;
}


/*
  Copies a trie into a new one. The new trie is always mutable.
*/
SCM
trie_copy (SCM oldtrie) {
    int is_fxmap = FXMAPP(oldtrie);

    if (TRIE_EMPTYP(oldtrie)) {
        SCM trie;
        if (is_fxmap) {
            NEWCELL_WITH_LEN(trie, fxmap, sizeof(struct trie_empty_obj));
        } else {
            NEWCELL_WITH_LEN(trie, iset, sizeof(struct trie_empty_obj));
        }
        BOXED_INFO(trie) |= TRIE_EMPTY;
        BOXED_INFO(trie) &= (~TRIE_CONST);
        return trie;
    }

    if (TRIE_LEAFP(oldtrie))
        return trie_make_leaf(TRIE_PREFIX(oldtrie),
                                  is_fxmap ? 0 : TRIE_BITMAP(oldtrie),
                                  is_fxmap ? TRIE_VALUE(oldtrie) : NULL);

    /* branch */
    return trie_make_branch (TRIE_PREFIX(oldtrie),
                                 TRIE_BRANCHBIT(oldtrie),
                                 trie_copy(TRIE_LEFT(oldtrie)),
                                 trie_copy(TRIE_RIGHT(oldtrie)),
                                 is_fxmap);
}


/*
  Intersection of two leaves.
  If "existence" is non-zero, then only the existence of
  a non-void intersection is reported (as #t or #f).
  Otherwise, the trie with the intersection is returned.
 */
SCM
trie_leaf_inter(SCM proc, SCM s, SCM t, int existence) {

    if (FXMAPP(s)) {
        long sk = TRIE_KEY(s);
        long tk = TRIE_KEY(t);
        if (sk == tk)
            return existence
                ? STk_true
                : (proc     /* for fxmapping-intersection/combinator: */
                   ? trie_make_leaf(sk, 0,
                                    STk_C_apply(proc,3,
                                                MAKE_INT(sk),
                                                TRIE_VALUE(s),
                                                TRIE_VALUE(t)))
                   : s);

        else
            return existence
                ? STk_false
                : trie_make_empty(1);
    }

    long p =  TRIE_PREFIX(s);
    long q =  TRIE_PREFIX(t);
    long bs = TRIE_BITMAP(s);
    long bt = TRIE_BITMAP(t);

    /* different prefixes, no intersection: */
    if (p != q) return existence
                    ? STk_false
                    : trie_make_empty(FXMAPP(s));

    /* if bitmaps have intersection, build a leaf with it: */
    if (bs & bt) return existence
                     ? STk_true
                     : trie_make_leaf(p, bs & bt, NULL);

    /* otherwise, it's empty! */
    return existence
        ? STk_false
        : trie_make_empty(FXMAPP(s));
}


/* leaf and branch */
SCM
trie_inter_leaf_branch(SCM proc, SCM s, SCM t, int existence) {
    long p =  TRIE_PREFIX(s);
    long q =  TRIE_PREFIX(t);
    while (1) {
        if (TRIE_EMPTYP(t)) return existence
                                ? STk_false
                                : trie_make_empty(FXMAPP(s));

        if (TRIE_LEAFP(t)) return trie_leaf_inter(proc,s,t,existence);

        long n  = TRIE_BRANCHBIT(t);
        if (trie_match_prefix(p,q,n))
            if (trie_zerobit(p,n)) t = TRIE_LEFT(t);
            else                   t = TRIE_RIGHT(t);
        else return existence
                 ? STk_false
                 : trie_make_empty(FXMAPP(s));
    }
}

/*
  Intersection of two tries.
  If "existence" is non-zero, then only the existence of
  a non-void intersection is reported (as #t or #f).
  Otherwise, the trie with the intersection is returned.
 */
SCM
trie_intersection (SCM proc, SCM s, SCM t, int existence) {
    /* When comparing associations, only the keys are compared. In
       case of duplicate keys, associations in the result fxmapping
       are drawn from the first fxmapping in which they appear. */

    /* empty inter empty is empty. */
    if (TRIE_EMPTYP(s) || TRIE_EMPTYP(t))
        return existence
            ? STk_false
            : trie_make_empty(FXMAPP(s));

    /* two leaves */
    if (TRIE_LEAFP(s) && TRIE_LEAFP(t)) return trie_leaf_inter(proc,s,t,existence);


    /* leaf and branch */
    if (TRIE_LEAFP(s)) return trie_inter_leaf_branch(proc,s,t,existence);
    if (TRIE_LEAFP(t)) return trie_inter_leaf_branch(proc,t,s,existence);

    /* two branches */
    long p = TRIE_PREFIX(s);
    long q = TRIE_PREFIX(t);
    long m = TRIE_BRANCHBIT(s);
    long n = TRIE_BRANCHBIT(t);
    SCM sl = TRIE_LEFT(s);
    SCM sr = TRIE_RIGHT(s);
    SCM tl = TRIE_LEFT(t);
    SCM tr = TRIE_RIGHT(t);

    if (trie_branchbit_higher(m,n)) {
        if (trie_match_prefix(q,p,m)) {
            return (trie_zerobit(q,m))
                ? trie_intersection(proc,sl,t,existence)
                : trie_intersection(proc,sr,t,existence);
        } else return existence
                   ? STk_false
                   : trie_make_empty(FXMAPP(s));
    }

    if (trie_branchbit_higher(n,m)) {
        if (trie_match_prefix(p,q,n)) {
            return (trie_zerobit(p,n))
                ? trie_intersection(proc,s,tl,existence)
                : trie_intersection(proc,s,tr,existence);
        } else return existence
                   ? STk_false
                   : trie_make_empty(FXMAPP(s));
    }

    if (p == q) return existence
                    ? MAKE_BOOLEAN((trie_intersection(proc,sl,tl,existence) == STk_true) &&
                                   (trie_intersection(proc,sr,tr,existence) == STk_true))
                    : trie_make_branch(p,m,
                                       trie_intersection(proc,sl,tl,existence),
                                       trie_intersection(proc,sr,tr,existence),
                                       FXMAPP(s));
    STk_error("trie intersection: internal error");
    return STk_nil; /* never reached */
}

/**************************************/
/***                                ***/
/***     END: CORE OPERATIONS       ***/
/***                                ***/
/**************************************/



/***************************/
/***                     ***/
/***     BEGIN: API      ***/
/***                     ***/
/***************************/


/***
 *** CONSTRUCTORS
 ***/

/*
  iset             C
  iset-unfold      Scheme
  make-range-iset  Scheme
*/

/*
  fxmapping                     C
  fxmapping-unfold              Scheme
  fxmapping-accumulate          Scheme
  alist->fxmapping              C
  alist->fxmapping/combinator   C
 */

/*
<doc EXT fxmapping constant-fxmapping
 * (fxmapping k1 v1 k2 v2 ... kn vn)
 * (constant-fxmapping k1 v1 k2 v2 ... kn vn)
 *
 * Builds a fixnum map containing the integer keys |k1|, |k2|, ..., |kn|
 * with respective associated values |v1|, |v2|, ... |vn|.
 *
 * It is an error if any of the keys is not an integer, or if the
 * number of arguments is not even.
doc>
 */
DEFINE_PRIMITIVE("fxmapping", trie_fxmap, vsubr, (int argc, SCM *argv))
{
    /* 0=mutable, 1=is_fxmap */
    return trie_aux(0, 1,  argc, argv);
}

DEFINE_PRIMITIVE("constant-fxmapping", trie_constant_fxmap, vsubr, (int argc, SCM *argv))
{
    /* 1=constant, 1=is_fxmap */
    return trie_aux(1, 1,  argc, argv);
}

/*
<doc EXT iset constant-iset
 * (iset n1 n2 ... nk)
 * (constant-iset n1 n2 ... nk)
 *
 * Builds a fixnum set containing the fixnums  |n1|, |n2|, ..., |nk|.
 *
 * It is an error if any of the keys is not an integer.
doc>
 */

DEFINE_PRIMITIVE("iset", trie_iset, vsubr, (int argc, SCM *argv))
{
    /* 0=mutable, 1=NO_values */
    return trie_aux(0, 0,  argc, argv);
}

DEFINE_PRIMITIVE("constant-iset", trie_constant_iset, vsubr, (int argc, SCM *argv))
{
    /* 1=constant, 1=NO_values */
    return trie_aux(1, 0,  argc, argv);
}

/*
<doc EXT alist->fxmapping
 * (alist->fxmapping alist) → fxmapping
 *
 * Returns a newly allocated fxmapping containing the associations of
 * alist. It is an error if the car of any pair in alist is not a
 * fixnum. If an integer k appears as the key of multiple associations in
 * alist (i.e. as the car of multiple pairs), then the first association
 * for k is preferred.
 *
 * @lisp
 * (fxmapping->alist
 *   (alist->fxmapping '((1 . b) (0 . a) (2 . c))))
 *  => ((0 . a) (1 . b) (2 . c))
 *
 * (fxmapping->alist
 *   (alist->fxmapping '((-10 . "yar") (-10 . "worf"))))
 *  => ((-10 . "yar"))
 * @end lisp
doc>
*/

/*
<doc EXT iset-unfold
 * (iset-unfold stop? mapper successor seed)
 *
 * Create a newly allocated iset as if by iset. If the result of
 * applying the predicate |stop?| to |seed| is true, return the
 * iset. Otherwise, apply the procedure |mapper| to |seed|. The
 * value that |mapper| returns is added to the iset. Then get a
 * new seed by applying the procedure |successor| to |seed|, and
 * repeat this algorithm.
 *
 * @lisp
 * (iset->list (iset-unfold (lambda (n) (> n 64))
 *                          values
 *                          (lambda (n) (* n 2))
 *                          2))
 * => (2 4 8 16 32 64)
 * @end lisp
doc>
*/

/*
<doc EXT make-range-iset
 * (make-range-iset start end [step])
 *
 * Returns a newly allocated iset specified by an inclusive lower bound
 * |start|, an exclusive upper bound |e|nd, and a step value (default 1), all
 * of which are exact integers. This constructor produces an iset
 * containing the sequence
 *
 * |start, (+ start step), (+ start (* 2 step)), ..., (+ start (* n step))|,

 * where |n| is the greatest integer such that |(+ start (* n step)) < end|
 * if |step| is positive, or such that |(+ start (* n step)) > end| if |step|
 * is negative. It is an error if |step| is zero.
 *
 * @lisp
 * (iset->list (make-range-iset 25 30))    => (25 26 27 28 29)
 * (iset->list (make-range-iset -10 10 6)) => (-10 -4 2 8)
 * @end lisp
doc>
*/

/***
 ***   PREDICATES
 ***/

/*
  iset?           C
  iset-contains?  C
  iset-empty?     C
  iset-disjoint?  C
*/

/*
  Not essential, here just forcompleteness and may be useful in
  debugging this implementation. %itrie checks if an object is
  either an fxmap or an iset.
 */
DEFINE_PRIMITIVE("%itrie?", triep, subr1, (SCM obj)) {
    return MAKE_BOOLEAN(TRIEP(obj));
}


/*
<doc EXT SRFI-224 fxmapping?
 * (fxmapping? obj)
 *
 * Returns |#t| is |obj| is an fxmapping object and |#f| otherwise.
doc>
 */
DEFINE_PRIMITIVE("fxmapping?", fxmapp, subr1, (SCM obj)) {
    return MAKE_BOOLEAN(FXMAPP(obj));
}


/*
<doc EXT SRFI-217 iset?
 * (iset? obj)
 *
 * Returns |#t| is |obj| is an iset and |#f| otherwise.
doc>
 */
DEFINE_PRIMITIVE("iset?", isetp, subr1, (SCM obj)) {
    return MAKE_BOOLEAN(ISETP(obj));
}


/*
<doc EXT SRFI-224 fxmapping-empty?
 * (fxmapping-empty? obj)
 *
 * Returns |#t| is |obj| is an empty fxmapping and |#f| if it
 * is an fxmapping containing at least one key.
 * If |obj| is not an fxmapping object, an error is sginaled.
doc>
 */
DEFINE_PRIMITIVE("fxmapping-empty?", trie_fxmap_empty_p, subr1, (SCM obj)) {
    if (!FXMAPP(obj)) STk_error("bad fxmapping ~S", obj);
    return MAKE_BOOLEAN(TRIE_EMPTYP(obj));
}

/*
<doc EXT SRFI-217 iset-empty?
 * (iset-empty? obj)
 *
 * Returns |#t| is |obj| is an empty iset and |#f| if it
 * is an iset containing at least one key.
 * If |obj| is not an iset object, an error is sginaled.
doc>
 */
DEFINE_PRIMITIVE("iset-empty?", trie_iset_empty_p, subr1, (SCM obj)) {
    if (!ISETP(obj)) STk_error("bad iset ~S", obj);
    return MAKE_BOOLEAN(TRIE_EMPTYP(obj));
}


/*
<doc EXT fxmapping-mutable?
 * (fxmapping-mutable? obj)
 *
 * Returns |#t| is |obj| is a mutable fxmapping and |#f| otherwise.
doc>
 */
DEFINE_PRIMITIVE("fxmapping-mutable?", trie_fxmap_mutable_p, subr1, (SCM obj)) {
    if (!FXMAPP(obj)) STk_error("bad fxmapping ~S", obj);
    return MAKE_BOOLEAN(!TRIE_CONSTP(obj));
}

/*
<doc EXT iset-mutable?
 * (iset-mutable? obj)
 *
 * Returns |#t| is |obj| is a mutable iset and |#f| otherwise.
doc>
 */
DEFINE_PRIMITIVE("iset-mutable?", trie_iset_mutable_p, subr1, (SCM obj)) {
    if (!ISETP(obj)) STk_error("bad iset ~S", obj);
    return MAKE_BOOLEAN(!TRIE_CONSTP(obj));
}

/*
<doc EXT fxmapping-contains?
 * (fxmapping-contains? map element)
 *
 * Returns true if |map| contains an association for |element|, and false otherwise.
doc>
 */

/*
<doc EXT iset-contains?
 * (iset-contains? set element)
 *
 * Returns true if |set| contains |element|, and false otherwise.
doc>
 */
DEFINE_PRIMITIVE("iset-contains?", trie_iset_contains, subr2, (SCM trie, SCM key))
{
    if (!ISETP(trie)) STk_error ("bad iset ~S", trie);
    if (BIGNUMP(key)) STk_error ("integer is not a fixnum: ~S", key);
    if (!(INTP(key))) STk_error ("bad integer ~S", key);
    return MAKE_BOOLEAN(trie_lookup_aux(INT_VAL(key), trie, STk_false) != STk_false);
}

/*
<doc EXT iset-disjoint?
 * (iset-disjoint? iset1 iset2)
 *
 * Returns |#t| if |iset1| and |iset2| have no elements in common and |#f| otherwise.
 *
 * @lisp
 * (iset-disjoint? (iset 1 3 5) (iset 0 2 4)) => #t
 * (iset-disjoint? (iset 1 3 5) (iset 2 3 4)) => #f
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("iset-disjoint?",trie_iset_disj, subr2, (SCM s, SCM t))
{
    if (!ISETP(s)) STk_error("bad iset ~S", s);
    if (!ISETP(t)) STk_error("bad iset ~S", t);
    return MAKE_BOOLEAN(trie_intersection(NULL,s,t,1) == STk_false);
}

DEFINE_PRIMITIVE("fxmapping-disjoint?",trie_fxmap_disj, subr2, (SCM s, SCM t))
{
    if (!FXMAPP(s)) STk_error("bad fxmapping ~S", s);
    if (!FXMAPP(t)) STk_error("bad fxmapping ~S", t);
    return MAKE_BOOLEAN(trie_intersection(NULL,s,t,1) == STk_false);
}

/***
 ***   ACCESSORS
 ***/

/*
  iset-member  C
  iset-min     C
  iset-max     C
*/

/*
  fxmapping-ref          Scheme
  fxmapping-ref/default  C
  fxmapping-min          C
  fxmapping-max          C
*/



/*
<doc EXT fxmapping-ref/default
 * (fxmapping-ref/default map k obj)
 *
 * If an association |(k, v)| occurs in |map|, returns |v|. Otherwise, returns |obj|.
 *
 * @lisp
 * (fxmapping-ref/default (fxmapping 36864 'zap) 36864 #f) => zap
 * (fxmapping-ref/default (fxmapping 0 'a) 36864 #f) => #f
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("fxmapping-ref/default", trie_fxmap_refdef, subr23, (SCM trie, SCM key, SCM def))
{
    if (!FXMAPP(trie)) STk_error ("bad fxmapping ~S", trie);
    if (!(INTP(key))) STk_error ("bad integer ~S", key);
    if (BIGNUMP(key)) STk_error ("integer is not a fixnum: ~S", key);
    return trie_lookup_aux(INT_VAL(key), trie, def);
}

/*
<doc EXT iset-member
 * (iset-member element set default)
 *
 * Returns the element of |set| that is equal to |element|. If |element| is not a member
 * of |set|, then |default| is returned.
doc>
 */
DEFINE_PRIMITIVE("iset-member", trie_iset_member, subr3, (SCM trie, SCM key, SCM def))
{
    if (!ISETP(trie)) STk_error ("bad iset ~S", trie);
    if (BIGNUMP(key)) STk_error ("integer is not a fixnum: ~S", key);
    if (!(INTP(key))) STk_error ("bad integer ~S", key);
    return trie_lookup_aux(INT_VAL(key), trie, def);
}


/*
<doc EXT iset-min iset-max
 * (iset-min set)
 * (iset-max set)
 *
 * Returns the smallest or largest integer in |set|, or |#f| if there is none.
 *
 * @lisp
 * (iset-min (iset 2 3 5 7 11)) => 2
 * (iset-max (iset 2 3 5 7 11)) => 11
 * (iset-max (iset))            => #f
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("fxmapping-min", trie_fxmap_min, subr1, (SCM trie))
{
    if (!FXMAPP(trie)) STk_error("bad fxmapping ~S", trie);
    return trie_min_max_aux(trie, 0);
}

DEFINE_PRIMITIVE("iset-min", trie_iset_min, subr1, (SCM trie))
{
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);
    return trie_min_max_aux(trie, 0);
}


DEFINE_PRIMITIVE("fxmapping-max", trie_fxmap_max, subr1, (SCM trie))
{
    if (!FXMAPP(trie)) STk_error("bad fxmapping ~S", trie);
    return trie_min_max_aux(trie, 1);
}

DEFINE_PRIMITIVE("iset-max", trie_iset_max, subr1, (SCM trie))
{
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);
    return trie_min_max_aux(trie, 1);
}

/***
 *** UPDATERS
 ***/

/*
  iset-adjoin        C
  iset-adjoin!       Scheme
  iset-delete        Scheme
  iset-delete!       Scheme
  iset-delete-all    Scheme
  iset-delete-all!   Scheme
  iset-search        Scheme
  iset-search!       Scheme
  iset-delete-min    C
  iset-delete-min!   Scheme
  iset-delete-max    C
  iset-delete-max!   Scheme
*/

/*
<doc EXT iset-adjoin iset-adjoin!
 * (iset-adjoin set element1 element2 ...)
 * (iset-adjoin! set element1 element2 ...)
 *
 * The |iset-adjoin| procedure returns a newly allocated iset that contains
 * all the values of |set|, and in addition each element unless it is
 * already equal to one of the existing or newly added members.
 *
 * @lisp
 * (iset->list (iset-adjoin (iset 1 3 5) 0)) => (0 1 3 5)
 * @end lisp
 *
 * The |iset-adjoin!| procedure is the linear update version of
 * |iset-adjoin|. In STklos, it is an alias to |iset-adjoin|.
doc>
*/

/*
<doc EXT  fxmapping-adjoin
 * (fxmapping-adjoin fxmap k1 obj1 k2 ...)
 *
 * Returns a fxmapping containing all of the associations of |fxmap| as
 * well as the associations |(k1, obj1)|, |(k2, obj2)|, ... The number of
 * key/value arguments must be even.
 *
 * If any of the keys already have associations in |fxmap|, the old
 * associations are preserved.
 *
 * @lisp
 * (fxmapping->alist (fxmapping-adjoin (fxmapping 1 'b) 0 'a))
 *  => ((0 . a) (1 . b))
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("fxmapping-adjoin", trie_fxmap_adjoin, vsubr, (int argc, SCM *argv))
{
    if (argc<1) STk_error ("at least one argument needed, none given");
    if (!FXMAPP(*argv)) STk_error("bad fxmapping ~S", *argv);

    argc--;

    if (argc % 2) STk_error ("Odd number (~S) of key+values for insertion in fxmapping ~S",
                             MAKE_INT(argc), *argv);

    SCM trie = *argv--;

    for (;  argc > 0; argc-=2, argv-=2) {
        if (!INTP(*argv)) STk_error("bad integer ~S", *argv);
        trie = trie_insert_aux(trie, INT_VAL(*argv), 0, *(argv-1), NULL, NO_SWAP, TRIE_NOP);
    }
    return trie;
}

DEFINE_PRIMITIVE("fxmapping-set", trie_fxmap_set, vsubr, (int argc, SCM *argv))
{
    if (argc<1) STk_error ("at least one argument needed, none given");
    if (!FXMAPP(*argv)) STk_error("bad fxmapping ~S", *argv);

    argc--;

    if (argc % 2) STk_error ("Odd number (~S) of key+values for insertion in fxmapping ~S",
                             MAKE_INT(argc), *argv);

    SCM trie = *argv--;

    for (;  argc > 0; argc-=2, argv-=2) {
        if (!INTP(*argv)) STk_error("bad integer ~S", *argv);
        trie = trie_insert_aux(trie, INT_VAL(*argv), 0, *(argv-1), NULL,
                               NO_SWAP, TRIE_UPDATE);
    }
    return trie;
}

DEFINE_PRIMITIVE("fxmapping-adjust", trie_fxmap_adjust, subr3, (SCM trie, SCM key, SCM proc))
{
    if (!FXMAPP(trie)) STk_error("bad fxmapping ~S", trie);
    if (!INTP(key)) STk_error("bad integer ~S", key);
    if (STk_procedurep(proc)==STk_false) STk_error("bad procedure ~S", proc);
    return trie_insert_aux(trie, INT_VAL(key), 0, NULL, proc, NO_SWAP, TRIE_ADJUST);
}


DEFINE_PRIMITIVE("fxmapping-adjoin/combinator", trie_fxmap_adjoin_comb, vsubr, (int argc, SCM *argv))
{
    if (argc<2) STk_error ("at least two arguments needed");
    if (!FXMAPP(*argv)) STk_error("bad fxmapping ~S", *argv);
    SCM trie = *argv--;
    argc--;

    if (STk_procedurep(*argv)==STk_false)
        STk_error("bad procedure ~S", *argv);

    SCM proc = *argv--;
    argc--;

    if (argc % 2) STk_error ("Odd number (~S) of key+values for insertion in fxmapping ~S",
                             MAKE_INT(argc), trie);

    for (;  argc > 0; argc-=2, argv-=2) {
        if (!INTP(*argv)) STk_error("bad integer ~S", *argv);
        trie = trie_insert_aux(trie, INT_VAL(*argv), 0, *(argv-1), proc, NO_SWAP, TRIE_UPDATE);
    }
    return trie;
}


DEFINE_PRIMITIVE("iset-adjoin", trie_iset_adjoin, vsubr, (int argc, SCM *argv))
{
    if (argc<1) STk_error ("at least one argument needed, none given");
    SCM trie = *argv--;
    argc--;
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);

    for (;  argc > 0; argc--, argv--) {
        if (!INTP(*argv)) STk_error("bad integer ~S", *argv);
        long k = INT_VAL(*argv);
        trie = trie_insert_aux(trie, trie_prefix(k),
                               trie_fixnum2bitmap(k), NULL,  NULL, NO_SWAP, TRIE_IOR);
    }
    return trie;
}


/*
<doc EXT  iset-delete iset-delete! iset-delete-all iset-delete-all!
 * (iset-delete set element1 element2 ...)
 * (iset-delete! set element1 element2 ...)
 * (iset-delete-all set element-list)
 * (iset-delete-all! set element-list)
 *
 * The |iset-delete| procedure returns a newly allocated iset containing
 * all the values of |set| except for any that are equal to one or more of
 * the elements. Any element that is not equal to some member of the |set|
 * is ignored.
 *
 * The |iset-delete!| procedure is the same as |iset-delete|.
 * is permitted to mutate and return the iset argument rather than
 * allocating a new iset -- but in STklos, it doesn't.
 *
 * The |iset-delete-all| and |iset-delete-all!| procedures are the same as
 * |iset-delete| and |iset-delete!|, except that they accept a single
 * argument which is a list of elements to be deleted.
 *
 * @lisp
 * (iset->list (iset-delete (iset 1 3 5) 3)) => (1 5)
 * (iset->list (iset-delete-all (iset 2 3 5 7 11)
 *                              '(3 4 5)))   => (2 7 11)
 * @end lisp
doc>
*/

/*
<doc EXT iset-delete-min iset-delete-min! iset-delete-max iset-delete-max!
 * (iset-delete-min set)
 * (iset-delete-min! set)
 * (iset-delete-max set)
 * (iset-delete-max! set)
 *
 * Returns two values: the smallest/largest integer n in |set| and a
 * newly-allocated iset that contains all elements of |set| except for
 * n. It is an error if iset is empty.
 *
 * The |iset-delete-min!| and |iset-delete-max!| procedures are the same as
 * |iset-delete-min| and |iset-delete-max|, respectively, except that they
 * are permitted to mutate and return the |set| argument instead of
 * allocating a new iset. In STklos, they do not.
 *
 * @lisp
 * (let-values (((n set) (iset-delete-min (iset 2 3 5 7 11))))
 *   (list n (iset->list set)))
 *   => (2 (3 5 7 11))
 * (let-values (((n set) (iset-delete-max (iset 2 3 5 7 11))))
 *   (list n (iset->list set)))
 *   => (11 (2 3 5 7))
 * @end lisp
doc>
*/


DEFINE_PRIMITIVE("fxmapping-delete-min", trie_fxmap_delete_min, subr1, (SCM trie)) {
    if (!FXMAPP(trie)) STk_error ("bad fxmapping ~S", trie);
    return CDR(trie_del_min_max(trie, 0));
}

DEFINE_PRIMITIVE("iset-delete-min", trie_iset_delete_min, subr1, (SCM trie)) {
    if (!ISETP(trie)) STk_error ("bad iset ~S", trie);
    SCM res = trie_del_min_max(trie, 0);
    return STk_n_values(2,CAR(res),CDR(res));
}

DEFINE_PRIMITIVE("fxmapping-delete-max", trie_fxmap_delete_max, subr1, (SCM trie)) {
    if (!FXMAPP(trie)) STk_error ("bad fxmapping ~S", trie);
    return CDR(trie_del_min_max(trie, 1));
}

DEFINE_PRIMITIVE("iset-delete-max", trie_iset_delete_max, subr1, (SCM trie)) {
    if (!ISETP(trie)) STk_error ("bad iset ~S", trie);
    SCM res = trie_del_min_max(trie, 1);
    return STk_n_values(2,CAR(res),CDR(res));
}

DEFINE_PRIMITIVE("fxmapping-pop-min", trie_fxmap_pop_min, subr1, (SCM trie)) {
    if (!FXMAPP(trie)) STk_error ("bad fxmapping ~S", trie);
    SCM res = trie_del_min_max(trie, 0);
    return STk_n_values(3,CAR(CAR(res)),CDR(CAR(res)),CDR(res));
}

DEFINE_PRIMITIVE("fxmapping-pop-max", trie_fxmap_pop_max, subr1, (SCM trie)) {
    if (!FXMAPP(trie)) STk_error ("bad fxmapping ~S", trie);
    SCM res = trie_del_min_max(trie, 1);
    return STk_n_values(3, CAR(CAR(res)), CDR(CAR(res)), CDR(res));
}



/*
<doc EXT iset-search iset-search!
 * (iset-search set element failure success)
 * (iset-search! iset element failure success)
 *
 * |Set| is searched from lowest to highest value for |element|. If it
 * is not found, then the |failure| procedure is tail-called with two
 * continuation arguments, |insert| and |ignore|, and is expected to
 * tail-call one of them. If |element| is found, then the |success| procedure
 * is tail-called with the matching element of |set| and two
 * continuations, |update| and |remove|, and is expected to tail-call one of
 * them.
 *
 * The effects of the continuations are as follows (where |obj| is any
 * Scheme object):
 *
 * Invoking |(insert obj)| causes |element| to be inserted into iset.
 *
 * Invoking |(ignore obj)| causes |set| to remain unchanged.
 *
 * Invoking |(update new-element obj)| causes |new-element| to be
 * inserted into |set| in place of |element|.
 *
 * Invoking |(remove obj)| causes the matching element of |set| to
 * be removed from it.
 *
 * In all cases, two values are returned: an iset and |obj|.
 *
 * The |iset-search!| procedure is the same as |iset-search|, except that it
 * is permitted to mutate and return the iset argument rather than
 * allocating a new iset. In STklos, it does not.
doc>
*/

/***
 *** THE WHOLE ISET
 ***/

/*
  iset-size    C
  iset-find    Scheme
  iset-count   Scheme
  iset-any?    Scheme
  iset-every?  Scheme
*/
/*
<doc EXT fxmapping-size
 * (fxmapping-size trie)
 *
 * Returns the number of key/value pairs in an fxmap.
doc>
*/
DEFINE_PRIMITIVE("fxmapping-size", trie_fxmap_size, subr1, (SCM trie))
{
    if (!FXMAPP(trie)) STk_error("bad fxmapping ~S", trie);
    return MAKE_INT(trie_count_aux(trie));
}

/*
<doc EXT iset-size
 * (iset-size set)
 *
 * Returns the number of fixnums in |set|.
doc>
*/
DEFINE_PRIMITIVE("iset-size", trie_iset_size, subr1, (SCM trie))
{
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);
    return MAKE_INT(trie_count_aux(trie));
}

/*
<doc EXT iset-find
 * (iset-find predicate set failure)
 *
 * Returns the smallest element of |set| that satisfies predicate, or
 * the result of invoking |failure| with no arguments if there is none.
 *
 * @lisp
 * (iset-find positive? (iset -1 1) (lambda () #f))  => 1
 * (iset-find zero?     (iset -1 1) (lambda () #f))  => #f
 * @end lisp
doc>
*/

/*
<doc EXT iset-count
 * (iset-count pred? set)
 *
 * Returns the number of elements of |set| that satisfy
 * |pred?| as an exact integer.
 *
 * @lisp
 * (iset-count odd? (iset 10 2 1 -3 9 4 3))  => 4
 * @end lisp
doc>
*/

/*
<doc EXT iset-any?
 * (iset-any? pred? set)
 *
 * Returns true if at least one of the elements of |set| satisfies
 * |pred?|. Note that this differs from the SRFI 1 analogue because
 * it does not return an element of the iset.
 *
 * @lisp
 * (iset-any odd? (iset 10 2 -3 4))    => #t
 * (iset-any odd? (iset 10 2 -8 4 0))  => #f
 * @end lisp
doc>
*/

/*
<doc EXT iset-every?
 * (iset-every? predicate iset)
 *
 * Returns |#t| if every element of |set| satisfies |predicate|, or |#f|
 * otherwise. Note that this differs from the SRFI 1 analogue because it
 * does not return an element of the iset.
 *
 * @lisp
 * (iset-every? (lambda (x) (< x 5)) (iset -2 -1 1 2)) => #t
 * (iset-every? positive? (iset -2 -1 1 2))            => #f
 * @end lisp
doc>
*/

/***
 *** MAPPING AND FOLDING
 ***/

/*
  iset-map         C + Scheme
  iset-for-each    C + Scheme
  iset-fold        C
  iset-fold-right  C
  iset-filter      C
  iset-filter!     Scheme
  iset-remove      Scheme
  iset-remove!     Scheme
  iset-partition   C
  iset-partition!  Scheme
*/

/*
<doc EXT iset-map
 * (iset-map proc set)
 *
 * Applies |proc| to each element of |set| in arbitrary order and returns a
 * newly allocated iset, created as if by iset, which contains the
 * results of the applications. It is an error if |proc| returns a value
 * that is not an exact integer.
 *
 * @lisp
 * (iset-map (lambda (x) (* 10 x)) (iset 1 11 21))
 *      => (iset 10 110 210)
 * (iset-map (lambda (x) (quotient x 2))
 *          (iset 1 2 3 4 5))
 *  => (iset 0 1 2)
 * @end lisp
doc>
*/

/*
<doc EXT iset-for-each
 * (iset-for-each proc set)
 *
 * Applies |proc| to |set| in increasing numerical order, discarding the
 * returned values. Returns an unspecified result.
 *
 * @lisp
 * (let ((sum 0))
 *   (iset-for-each (lambda (x) (set! sum (+ sum x)))
 *                  (iset 2 3 5 7 11))
 *   sum)
 * => 28
 * @end lisp
doc>
*/

DEFINE_PRIMITIVE("%iset-map", trie_iset_map, subr3, (SCM proc, SCM trie, SCM collect))
{
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);
    if (STk_procedurep(proc) == STk_false) STk_error("bad procedure ~S", proc);
    return trie_map_for_each(trie,collect,proc);
}

DEFINE_PRIMITIVE("%fxmapping-map", trie_fxmap_map, subr3, (SCM proc, SCM trie, SCM collect))
{
    if (!FXMAPP(trie)) STk_error("bad fxmapping ~S", trie);
    if (STk_procedurep(proc) == STk_false) STk_error("bad procedure ~S", proc);
    return trie_map_for_each(trie,collect,proc);
}

/*
<doc EXT iset-fold iset-fold-right
 * (iset-fold proc nil set)
 * (iset-fold-right proc nil set)
 *
 * Invokes |proc| on each member of |set| in increasing/decreasing numerical
 * order, passing the result of the previous invocation as a second
 * argument. For the first invocation, |nil| is used as the second
 * argument. Returns the result of the last invocation, or |nil| if there
 * was no invocation.
 *
 * @lisp
 * (iset-fold + 0 (iset 2 3 5 7 11))            => 28
 * (iset-fold cons '() (iset 2 3 5 7 11))       => (11 7 5 3 2)
 * (iset-fold-right cons '() (iset 2 3 5 7 11)) => (2 3 5 7 11)
 * @end lisp
doc>
*/


DEFINE_PRIMITIVE("iset-fold", trie_iset_fold, subr3, (SCM proc, SCM nil, SCM trie)) {
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);
    return trie_fold(proc, nil, trie, 0);
}

DEFINE_PRIMITIVE("fxmapping-fold", trie_fxmap_fold, subr3, (SCM proc, SCM nil, SCM trie)) {
    if (!FXMAPP(trie)) STk_error("bad fxmapping ~S", trie);
    return trie_fold(proc, nil, trie, 0);
}

DEFINE_PRIMITIVE("iset-fold-right", trie_iset_fold_right, subr3, (SCM proc, SCM nil, SCM trie)) {
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);
    return trie_fold(proc, nil, trie, 1);
}

DEFINE_PRIMITIVE("fxmapping-fold-right", trie_fxmap_fold_right, subr3, (SCM proc, SCM nil, SCM trie)) {
    if (!FXMAPP(trie)) STk_error("bad fxmapping ~S", trie);
    return trie_fold(proc, nil, trie, 1);
}

/*
<doc EXT iset-filter iset-filter!
 * (iset-filter predicate set)
 * (iset-filter! predicate set)
 *
 * Returns a newly allocated iset containing just the elements
 * of |set| that satisfy |predicate|.
 *
 * @lisp
 * (iset->list (iset-filter (lambda (x) (< x 6)) (iset 2 3 5 7 11)))
 *  => (2 3 5)
 * @end lisp
 *
 * |iset-filter!| is allowed to modify |set|, but in STklos it does not.
doc>
*/


DEFINE_PRIMITIVE("iset-filter", trie_iset_filter, subr2, (SCM pred, SCM trie)) {
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);
    if (STk_procedurep(pred) == STk_false) STk_error("bad procedure ~S", pred);
    SCM l = STk_cons(STk_false, STk_false);
    trie_partition_aux(pred, trie, l, 0);
    return CAR(l);
}

DEFINE_PRIMITIVE("fxmapping-filter", trie_fxmap_filter, subr2, (SCM pred, SCM trie)) {
    if (!FXMAPP(trie))    STk_error("bad fxmapping ~S", trie);
    if (STk_procedurep(pred) == STk_false) STk_error("bad procedure ~S", pred);
    SCM l = STk_cons(STk_false, STk_false);
    trie_partition_aux(pred, trie, l, 0);
    return CAR(l);
}

/*
<doc EXT iset-remove iset-remove!
 * (iset-remove predicate set)
 * (iset-remove! predicate set)
 *
 * Returns a newly allocated |set| containing just the elements of |set|
 * that do not satisfy |predicate|.
 *
 * @lisp
 * (iset->list (iset-remove (lambda (x) (< x 6)) (iset 2 3 5 7 11)))
 *  => (7 11)
 * @end lisp
 *
 * |Iset-remove!| is allowed to modify |set|, but in STklos it does not.
doc>
*/

/*
<doc EXT iset-partition iset-partition!
 * (iset-partition predicate set)
 * (iset-partition! predicate set)
 *
 * Returns two values: a newly allocated iset that contains just the
 * elements of |set| that satisfy |predicate| and another newly allocated
 * iset that contains just the elements of |set| that do not satisfy
 * |predicate|.
 *
 * @lisp
 * (let-values (((low high) (iset-partition (lambda (x) (< x 6))
 *                                          (iset 2 3 5 7 11))))
 *   (list (iset->list low) (iset->list high)))
 *  => ((2 3 5) (7 11))
 * @end lisp
doc>
*/

DEFINE_PRIMITIVE("iset-partition", trie_iset_partition, subr2, (SCM pred, SCM trie)) {
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);
    if (STk_procedurep(pred) == STk_false) STk_error("bad procedure ~S", pred);
    SCM l = STk_cons(STk_false, STk_false);
    trie_partition_aux(pred, trie, l, 1);
    return STk_n_values(2, CAR(l), CDR(l));
}

DEFINE_PRIMITIVE("fxmapping-partition", trie_fxmap_partition, subr2, (SCM pred, SCM trie)) {
    if (!FXMAPP(trie)) STk_error("bad fxmapping ~S", trie);
    if (STk_procedurep(pred) == STk_false) STk_error("bad procedure ~S", pred);
    SCM l = STk_cons(STk_false, STk_false);
    trie_partition_aux(pred, trie, l, 1);
    return STk_n_values(2, CAR(l), CDR(l));
}

DEFINE_PRIMITIVE("%trie-compare", trie_compare, subr23, (SCM s, SCM t, SCM compare))
{
    if (!TRIEP(s))                 STk_error("bad iset or fxmapping ~S", s);
    if (!TRIEP(t))                 STk_error("bad iset or fxmapping ~S", t);
    if ((ISETP(s) && FXMAPP(t)) ||
        (ISETP(t) && FXMAPP(s)))   STk_error("cannot compare iset with fxmapping: ~S and ~S");
    if (compare && (STk_procedurep(compare)==STk_false))
                                   STk_error("bad procedure ~S", compare);
    if (ISETP(s) && compare)       STk_error ("cannot use compare procedure when comparing isets");
    if (FXMAPP(s) && (!compare))   STk_error ("need compare procedure when comparing fxmappings");
    return MAKE_INT(trie_compare(s,t,compare));
}

/***
 *** COPYING AND CONVERSION
 ***/

/*
  iset-copy    C
  iset->list   C
  list->iset   C
  list->iset!  Scheme
*/

/*
  fxmapping->alist                 C
  fxmapping->decreasing-alist      Scheme
  fxmapping-keys                   Scheme
  fxmapping-values                 Scheme
  fxmapping->generator             Scheme
  fxmapping->decreasing-generator  Scheme
*/

DEFINE_PRIMITIVE("fxmapping-copy", trie_fxmap_copy, subr1, (SCM trie)) {
    if (!FXMAPP(trie)) STk_error("bad fxmapping ~S", trie);
    return trie_copy(trie);
}

/*
<doc EXT iset-copy
 * (iset-copy set)
 *
 * Returns a newly allocated iset containing the elements of |set|.
doc>
*/

/*
<doc EXT iset->list
 * (iset->list set)
 *
 * Returns a newly allocated list containing the members of
 * |set| in increasing numerical order.
 *
 * @lisp
 * (iset->list (iset 2 3 5 7 11)) => (2 3 5 7 11)
 * @end lisp
doc>
*/

/*
<doc EXT list->iset
 * (list->iset list)
 * (list->iset! set list)
 *
 * Returns a newly allocated iset, created as if by iset, that contains
 * the elements of |list|. Duplicate elements are omitted.
 *
 * @lisp
 * (list->iset '(-3 -1 0 2)) = (iset -3 -1 0 2)
 * @end lisp
 *
 * |list->iset!| may mutate |set| rather than allocating a new iset,
 * but in STklos it does not.
 *
 * @lisp
 * (iset->list (list->iset! (iset 2 3 5) '(-3 -1 0))) ⇒ (-3 -1 0 2 3 5)
 * @end lisp
doc>
*/




DEFINE_PRIMITIVE("iset-copy", trie_iset_copy, subr1, (SCM trie)) {
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);
    return trie_copy(trie);
}

DEFINE_PRIMITIVE("fxmapping->alist", trie_fxmap_alist, subr1, (SCM trie))
{
    if (!FXMAPP(trie)) STk_error("bad fxmapping ~S", trie);
    return trie_list_aux(trie,STk_nil);
}

DEFINE_PRIMITIVE("iset->list", trie_iset_list, subr1, (SCM trie))
{
    if (!ISETP(trie)) STk_error("bad iset ~S", trie);
    return trie_list_aux(trie,STk_nil);
}

DEFINE_PRIMITIVE("list->iset", trie_list_iset, subr1, (SCM list))
{
    if (!(NULLP(list) || CONSP(list))) STk_error("bad list ~S", list);
    return trie_list_trie_aux(list,0,NULL);
}

DEFINE_PRIMITIVE("alist->fxmapping", trie_list_fxmap, subr1, (SCM list))
{
    if (!(NULLP(list) || CONSP(list))) STk_error("bad list ~S", list);
    return trie_list_trie_aux(list,1,NULL);
}

DEFINE_PRIMITIVE("alist->fxmapping/combinator", trie_list_fxmap_comb, subr2, (SCM combinator, SCM list))
{
    if (!STk_procedurep(combinator)) STk_error("bad procedure ~S", combinator);
    if (!(NULLP(list) || CONSP(list))) STk_error("bad list ~S", list);
    return trie_list_trie_aux(list,1,combinator);
}



/*
<doc EXT fxmapping-keys
 * (fxmapping-keys fxmap)
 *
 * Returns the keys of |fxmap| as a list in ascending numerical order.
 *
 * @lisp
 * (fxmapping-keys (fxmapping 137 'a -24 'b -5072 'c))
 *  => (-5072 -24 137)
 * @end lisp
doc>
*/

/*
<doc EXT fxmapping-values
 * (fxmapping-values fxmap)
 *
 * Returns the values of |fxmap| as a list in ascending numerical order of
 * key. That is, if |(k1, v1), ..., (kn, vn)| are the associations of |fxmap|
 * ordered so that |k1 <= … <= kn|, then |(fxmapping-values fxmap)| produces
 * the list |(v1 ... vn)|.
 *
 * @lisp
 * (fxmapping-values (fxmapping 0 "picard" 1 "riker" 2 "troi"))
 *  => ("picard" "riker" "troi")
 * @end lisp
doc>
*/

/***
 *** SUBSETS
 ***/

/*
  iset=?  C + Scheme
  iset<?  C + Scheme
  iset>?  C + Scheme
  iset<=? C + Scheme
  iset>=? C + Scheme
*/


/*
<doc EXT  iset=? iset<? iset>? iset<=? iet>=?
 * (iset=? iset1 iset2 iset3 ...)
 * (iset<? iset1 iset2 iset3 ...)
 * (iset>? iset1 iset2 iset3 ...)
 * (iset<=? iset1 iset2 iset3 ...)
 * (iset>=? iset1 iset2 iset3 ...)
 *
 * These procedures return true when each set is
 * equal (|iset=?|) or a proper subset (|iset<?|), a proper
 * superset (|iset>?|), a subset (|iset<=?|) or a superset
 * (|iset>=?|) of the next one.
 *
 * @lisp
 * (iset=? (iset 1 2 3) (iset 3 1 2))           => #t
 * (iset<? (iset 3 1 2) (iset 4 2 1 3))         => #t
 * (iset>=? (iset 3 0 1) (iset 0 1) (iset 0 1)) => #t
 * @end lisp
doc>
*/


/***
 *** SET THEORY OPERATIONS
 ***/

/*
  iset-union          C + Scheme
  iset-intersection   C + Scheme
  iset-difference     C + Scheme
  iset-xor            C
  iset-union!         Scheme
  iset-intersection!  Scheme
  iset-difference!    Scheme
  iset-xor!           Scheme
*/

/*
  fxmapping-union                    C + Scheme
  fxmapping-intersection             C + Scheme
  fxmapping-difference               C + Scheme
  fxmapping-xor                      C
  fxmapping-union/combinator         C + Scheme
  fxmapping-intersection/combinator
*/

/*
<doc EXT iset-union iset-intersection iset-difference iset-xor iset-union! iset-intersection! iset-difference! iset-xor!
 * (iset-union iset1 iset2 iset3 ...)
 * (iset-intersection iset1 iset2 iset3 ...)
 * (iset-difference iset1 iset2 iset3 ...)
 * (iset-xor iset1 iset2)
 * (iset-union! iset1 iset2 iset3 ...)
 * (iset-intersection! iset1 iset2 iset3 ...)
 * (iset-difference! iset1 iset2 iset3 ...)
 * (iset-xor! iset1 iset2)
 *
 * Return a newly allocated iset that is the union, intersection,
 * asymmetric difference, or symmetric difference of the
 * isets. Asymmetric difference is extended to more than two isets by
 * taking the difference between the first iset and the union of the
 * others. Symmetric difference is not extended beyond two
 * isets. Elements in the result iset are drawn from the first iset in
 * which they appear.
 *
 * @lisp
 * (iset->list (iset-union (iset 0 1 3) (iset 0 2 4))) => (0 1 2 3 4)
 * (iset->list (iset-intersection (iset 0 1 3 4) (iset 0 2 4))) => (0 4)
 * (iset->list (iset-difference (iset 0 1 3 4) (iset 0 2) (iset 0 4))) => (1 3)
 * (iset->list (iset-xor (iset 0 1 3) (iset 0 2 4))) => (1 2 3 4)
 * @end lisp
 *
 * The procedures whose name end in |!| are linear update procedures.
 * The specification says they may or may not alter their argument. In
 * STklos they do not: in fact, they are aliases to the pure functional
 * versions.
doc>
*/

/*
<doc EXT fxmapping-union fxmapping-intersection fxmapping-difference fxmapping-xor
 * (fxmapping-union fxmap1 fxmap2 fxmap3 ...)
 * (fxmapping-intersection fxmap1 fxmap2 fxmap3 ...)
 * (fxmapping-difference fxmap1 fxmap2 fxmap3 ...)
 * (fxmapping-xor fxmap1 fxmap2)
 *
 * Return a fxmapping whose set of associations is the union,
 * intersection, asymmetric difference, or symmetric difference of the
 * sets of associations of the fxmaps. Asymmetric difference is extended
 * to more than two fxmappings by taking the difference between the first
 * fxmapping and the union of the others. Symmetric difference is not
 * extended beyond two fxmappings. When comparing associations, only the
 * keys are compared. In case of duplicate keys, associations in the
 * result fxmapping are drawn from the first fxmapping in which they
 * appear.
 *
 * @lisp
 * (fxmapping->alist (fxmapping-union (fxmapping 0 'a 2 'c)
 *                                   (fxmapping 1 'b 3 'd)))
 *    => ((0 . a) (1 . b) (2 . c) (3 . d))
 *
 *  (fxmapping->alist
 *   (fxmapping-intersection (fxmapping 0 'a 2 'c)
 *                           (fxmapping 1 'b 2 'c 3 'd)
 *                           (fxmapping 2 'c 4 'e)))
 *     => ((2 . c))
 *
 * (fxmapping->alist
 *  (fxmapping-difference (fxmapping 0 'a 1 'b 2 'c)
 *                        (fxmapping 2 "worf")
 *                        (fxmapping 1 "data")))
 *     => ((0 . a))
 * @end lisp
doc>
*/

DEFINE_PRIMITIVE("%iset-union", trie_iset_union, subr3, (SCM proc,SCM a, SCM b))
{
    if (!ISETP(a)) STk_error("bad iset ~S", a);
    if (!ISETP(b)) STk_error("bad iset ~S", b);
    if (STk_procedurep(proc) == STk_false) STk_error("bad procedure ~S", proc);
    return trie_merge(proc,TRIE_IOR,a,b);
}


DEFINE_PRIMITIVE("%fxmapping-union", trie_fxmap_union, subr3, (SCM proc,SCM a, SCM b))
{
    if (!FXMAPP(a)) STk_error("bad fxmapping ~S", a);
    if (!FXMAPP(b)) STk_error("bad fxmapping ~S", b);
    if (proc == STk_false) return trie_merge(NULL,TRIE_UNION,a,b);

    if (STk_procedurep(proc) == STk_false) STk_error("bad procedure ~S", proc);
    return trie_merge(proc,TRIE_UPDATE,a,b);
}

DEFINE_PRIMITIVE("iset-xor", trie_iset_xor, subr2, (SCM a, SCM b))
{
    if (!ISETP(a)) STk_error("bad iset ~S", a);
    if (!ISETP(b)) STk_error("bad iset ~S", b);
    return trie_merge(NULL,TRIE_XOR,a,b);
}


DEFINE_PRIMITIVE("fxmapping-xor", trie_fxmap_xor, subr2, (SCM a, SCM b))
{
    if (!FXMAPP(a)) STk_error("bad fxmapping ~S", a);
    if (!FXMAPP(b)) STk_error("bad fxmapping ~S", b);
    return trie_merge(NULL,TRIE_XOR,a,b);
}



DEFINE_PRIMITIVE("%iset-difference",trie_iset_difference, subr2, (SCM s, SCM t))
{
    if (!ISETP(s)) STk_error("bad iset ~S", s);
    if (!ISETP(t)) STk_error("bad iset ~S", t);
    return trie_difference_aux(s, t);
}

DEFINE_PRIMITIVE("%fxmapping-difference",trie_fxmap_difference, subr2, (SCM s, SCM t))
{
    if (!FXMAPP(s)) STk_error("bad fxmapping ~S", s);
    if (!FXMAPP(t)) STk_error("bad fxmapping ~S", t);
    return trie_difference_aux(s, t);
}

DEFINE_PRIMITIVE("%iset-intersection",trie_iset_inter, subr2, (SCM s, SCM t))
{
    if (!ISETP(s)) STk_error("bad iset ~S", s);
    if (!ISETP(t)) STk_error("bad iset ~S", t);
    return trie_intersection(NULL,s,t,0);
}

DEFINE_PRIMITIVE("%fxmapping-intersection",trie_fxmap_inter, subr3, (SCM proc, SCM s, SCM t))
{
    if (!FXMAPP(s)) STk_error("bad fxmapping ~S", s);
    if (!FXMAPP(t)) STk_error("bad fxmapping ~S", t);
    if (proc == STk_false) proc = NULL;
    return trie_intersection(proc,s,t,0);
}



/***
 *** INTERVALS AND RANGES
 ***/

/*
  iset-open-interval        Scheme
  iset-closed-interval      Scheme
  iset-open-closed-interval Scheme
  iset-closed-open-interval Scheme
  isubset=                  Scheme
  isubset<                  Scheme
  isubset<=                 Scheme
  isubset>                  Scheme
  isubset>=                 Scheme
*/

/*
<doc EXT iset-open-interval iset-closed-interval iset-open-closed-interval iset-closed-open-interval
 * (iset-open-interval set low high)
 * (iset-closed-interval set low high)
 * (iset-open-closed-interval set low high)
 * (iset-closed-open-interval set low high)
 *
 * Procedures that return a subset of |set| contained in the interval from
 * |low| to |high|. The interval may be open, closed, open below and closed
 * above, or open above and closed below.
 *
 * @lisp
 * (iset->list (iset-open-interval (iset 2 3 5 7 11) 2 7))        => (3 5)
 * (iset->list (iset-closed-interval (iset 2 3 5 7 11) 2 7))      => (2 3 5 7)
 * (iset->list (iset-open-closed-interval (iset 2 3 5 7 11) 2 7)) => (3 5 7)
 * (iset->list (iset-closed-open-interval (iset 2 3 5 7 11) 2 7)) => (2 3 5)
 * @end lisp
doc>
*/

/*
<doc EXT isubset= isubset< isubset<= isubset> isubset>=
 * (isubset= set k)
 * (isubset< set k)
 * (isubset<= set k)
 * (isubset> set k)
 * (isubset>= set k)
 *
 * Procedures that return an integer set containing the elements of |set|
 * that are equal to, less than, less than or equal to, greater than, or
 * greater than or equal to |k|. Note that the result of |isubset=| contains
 * at most one element.
 *
 * @lisp
 * (iset->list (isubset= (iset 2 3 5 7 11) 7))  => (7)
 * (iset->list (isubset< (iset 2 3 5 7 11) 7))  => (2 3 5)
 * (iset->list (isubset>= (iset 2 3 5 7 11) 7)) => (7 11)
 * @end lisp
doc>
*/

/***
 *** EXTRA
 ***/

DEFINE_PRIMITIVE("%trie/list-flatten", trie_list_flatten, subr1, (SCM l))
{
    if (!CONSP(l)) STk_error ("bad list ~S", l); /* should never happen */
    return trie_flatten(l);
}

/*************************/
/***                   ***/
/***     END: API      ***/
/***                   ***/
/*************************/



/*===========================================================================* \
 *
 *  Trie extended type definition
 *
\*===========================================================================*/



/*
  In order to print, we turn the iset or fxmap into a list
  (or alist), then cons the appropriate symbol, `<iset>`
  or `<fxmapping>`.
  But we need the `trie_flatten` function...

  FIXME: we should be able to do this with for-each. Can we
         set the print function in Scheme, and not C?
*/
static void print_trie(SCM trie, SCM port, int mode) {
    SCM lst = trie_list_aux(trie, STk_nil);
    STk_puts("#,", port);
    if (FXMAPP(trie))
        lst = trie_flatten(STk_cons (STk_intern("<fxmapping>"), lst));
    else
        lst = STk_cons (STk_intern("<iset>"), lst);
    STk_print(lst, port, mode);
}

static SCM test_equal_trie(SCM x, SCM y) {
    return trie_compare(x,y,STk_equal)
        ? STk_false
        : STk_true;
}

static struct extended_type_descr xtype_fxmap = {
  .name  = "fxmapping",
  .print = print_trie,
  .equal = test_equal_trie
};

static struct extended_type_descr xtype_iset = {
  .name  = "iset",
  .print = print_trie,
  .equal = test_equal_trie
};


/*===========================================================================*\
 *
 *  Module for ITRIE
 *
\*===========================================================================*/

MODULE_ENTRY_START("stklos/itrie")
{
  SCM module =  STk_create_module(STk_intern("stklos/itrie"));

  /* Create new types for tries */
  tc_fxmap = STk_new_user_type(&xtype_fxmap);
  tc_iset = STk_new_user_type(&xtype_iset);

  ADD_PRIMITIVE_IN_MODULE(triep,                 module);
  ADD_PRIMITIVE_IN_MODULE(fxmapp,                module);
  ADD_PRIMITIVE_IN_MODULE(isetp,                 module);

  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_empty_p,    module);
  ADD_PRIMITIVE_IN_MODULE(trie_iset_empty_p,     module);

  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_mutable_p,  module);
  ADD_PRIMITIVE_IN_MODULE(trie_iset_mutable_p,   module);

  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_refdef,     module);
  ADD_PRIMITIVE_IN_MODULE(trie_iset_member,      module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_contains,    module);

  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_size,       module);
  ADD_PRIMITIVE_IN_MODULE(trie_iset_size,        module);

  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_height,     module);
  ADD_PRIMITIVE_IN_MODULE(trie_iset_height,      module);

  ADD_PRIMITIVE_IN_MODULE(trie_fxmap,            module);
  ADD_PRIMITIVE_IN_MODULE(trie_iset,             module);

  ADD_PRIMITIVE_IN_MODULE(trie_constant_fxmap,   module);
  ADD_PRIMITIVE_IN_MODULE(trie_constant_iset,    module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_adjoin,      module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_set,        module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_adjoin,     module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_adjoin_comb, module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_adjust,     module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_delete_min,  module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_delete_min, module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_delete_max,  module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_delete_max, module);

  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_pop_min,    module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_pop_max,    module);

  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_min,        module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_max,        module);
  ADD_PRIMITIVE_IN_MODULE(trie_iset_min,         module);
  ADD_PRIMITIVE_IN_MODULE(trie_iset_max,         module);

  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_alist,      module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_alist,      module);
  ADD_PRIMITIVE_IN_MODULE(trie_iset_list,        module);

  ADD_PRIMITIVE_IN_MODULE(trie_list_fxmap,       module);
  ADD_PRIMITIVE_IN_MODULE(trie_list_fxmap_comb,  module);
  ADD_PRIMITIVE_IN_MODULE(trie_list_iset,        module);

  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_copy,       module);
  ADD_PRIMITIVE_IN_MODULE(trie_iset_copy,        module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_partition,   module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_partition,  module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_filter,      module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_filter,     module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_union,       module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_union,      module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_xor,         module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_xor,        module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_difference,  module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_difference, module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_inter,       module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_inter,      module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_disj,        module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_disj,       module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_map,         module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_map,        module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_fold,        module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_fold,       module);

  ADD_PRIMITIVE_IN_MODULE(trie_iset_fold_right,  module);
  ADD_PRIMITIVE_IN_MODULE(trie_fxmap_fold_right, module);

  ADD_PRIMITIVE_IN_MODULE(trie_debug,            module);
  ADD_PRIMITIVE_IN_MODULE(trie_list_flatten,     module);

  ADD_PRIMITIVE_IN_MODULE(trie_compare,          module);

  STk_export_all_symbols(module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
