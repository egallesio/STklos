/*
 * 178.c   -- Support for SRFI-178 (Bitvector library)
 *
 * Copyright © 2023 Jeronimo Pellegrini <j_p@aleph0.info>
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
 *           Author: Jeronimo Pellegrini
 *    Creation date: 19-Aug-2023 06:12
 */

#include <gmp.h>
#include "stklos.h"
#include "178-incl.c"

/*
  Bitvectors are represented exactly as bit sequences. The bitvector_obj
  structure has a 'data' field that is of the type `uint_fast8_t`, so
  it will be the fastest possible representation of a sequence of 8 bits.

  Obviously, if a bitvector's size is not multiple of the size of this
  cell, then the last element will be used only partially.
 */

static int tc_bitvector;

struct bitvector_obj {
  stk_header header;
  long size;         // number of cells in memory
  long length;       // number of stored bits
  uint_fast8_t data[1];
};

#define BITS_PER_CELL (CHAR_BIT * sizeof(uint_fast8_t))

#define BVECTORP(p)            (BOXED_TYPE_EQ((p), tc_bitvector))
#define BVECTOR_SIZE(p)        (((struct bitvector_obj *) (p))->size)
#define BVECTOR_LENGTH(p)      (((struct bitvector_obj *) (p))->length)
#define BVECTOR_DATA(p)        (((struct bitvector_obj *) (p))->data)

static inline void check_bitvector(SCM obj) {
  if (!BVECTORP(obj)) STk_error("bad bitvector ~S", obj);
}
static inline void check_cons(SCM obj) {
  if (!CONSP(obj)) STk_error("bad list ~S", obj);
}
static inline void check_integer(SCM obj, char *msg) {
  if (!INTP(obj)) STk_error("bad integer ~S for %d", obj);
}
static inline int get_bit(SCM bit) {
  if (bit == MAKE_INT(1) || bit == STk_true) return 1;
  if (bit == MAKE_INT(0) || bit == STk_false) return 0;
  STk_error("bad bit ~S", bit);
}

/***
    Internal utilities
 ***/

/* get_index checks an index for a bitvector, and returns it as a long int. */
static inline
long get_index(SCM bvec, SCM index) {
  check_integer(index, "index");
  long i = INT_VAL(index);
  if (i < 0) STk_error("negative index ~S", index);
  if (i > BVECTOR_LENGTH(bvec)) STk_error("index too large ~S", index);
  return i;
}

/*
  control_index:

  Verifies start and end indices.
  Arguments are   bvec, [start, [end]]
   - If indices are NULL, the defaults are 0 and end of the bitvector
   - start <= end, positive integers
   - *pstart is set to the start index.
   - If *pend is NULL, it is not set.
 */
static inline
void control_start_end_idx(SCM bvec,
                           SCM start, SCM end,
                           long *pstart, long *pend) {
  if (!start) start = MAKE_INT(0);
  else check_integer(start, "start index");

  if (!end)   end   = MAKE_INT(BVECTOR_LENGTH(bvec));
  else check_integer(end, "end index");

  int Cstart = INT_VAL(start);
  int Cend   = INT_VAL(end);

  if (Cstart < 0) STk_error("negative start index ~S", start);
  if (Cend   < 0) STk_error("negative end index ~S", end);
  if (Cstart > BVECTOR_LENGTH(bvec)) STk_error("start index out of range ~S", start);
  if (Cend   > BVECTOR_LENGTH(bvec)) STk_error("end index out of range ~S", end);
  if (Cstart > INT_VAL(end)) STk_error("start index ~S > end index ~S", start, end);

  *pstart = Cstart;
  if (pend) *pend   = Cend;
}

/*
  control_index:

  Verifies start and end indices.
  Arguments are   bvec, [start, [end]]
   - If absent (argc not large enough), the defaults are 0 and end of
     the bitvector
   - start <= end, positive integers
   - *pstart is set to the start index.
   - If *pend is NULL, it is not set.
 */
static inline
void control_index(SCM bvec,
                   int argc, SCM *argv,
                   long *pstart, long *pend) {

  SCM start = (argc > 0) ? *argv-- : NULL;
  SCM end   = (argc > 1) ? *argv-- : NULL;

  control_start_end_idx(bvec, start, end, pstart, pend);
}

static inline
int bvector_ref(SCM bv, long n) {
  long q = n / BITS_PER_CELL;
  long r = n % BITS_PER_CELL;
  return (BVECTOR_DATA(bv)[q] & (1 << r)) == 0
    ? 0
    : 1;
}

static inline
void bvector_set(SCM bv, long n, int bit) {
  long q = n / BITS_PER_CELL;
  long r = n % BITS_PER_CELL;
  BVECTOR_DATA(bv)[q] &= ~(1   << r);  /* set to 0 */
  BVECTOR_DATA(bv)[q] ^=  (bit << r);  /* 0 ^ bit = bit */
}

static inline
void bvector_swap(SCM bv, long i, long j) {
    int tmp_i = bvector_ref(bv,i);
    bvector_set(bv,i,bvector_ref(bv,j));
    bvector_set(bv,j,tmp_i);
}

static inline
void bvcopy(SCM dest, long dest_pos,
       SCM from, long from_pos, long k,
       int reverse) {
    long step = reverse? -1 : +1;
    long f_pos = reverse? (from_pos + k - 1) : from_pos;
    for(long i=0; i < k; i++, f_pos += step)
      if (bvector_ref(from, f_pos))
          bvector_set(dest, dest_pos+i, 1);
}

/* Counts a bit run starting at index start. If to_left is non-zero,
   goes towards the left; otherwise, towards the right. */
static inline
long count_run(int bit, SCM bvec, long start, int to_left) {
  long count = 0;
  long step = to_left ? -1 : +1;
  for (long pos = start; pos >= 0 && pos < BVECTOR_LENGTH(bvec); pos+=step)
    if (bvector_ref(bvec, pos) != bit)
      return count;
    else count++;
  return count;
}

SCM STk_make_bvect(long len, SCM init) {
  register long i;
  SCM  z;
  long size = len / BITS_PER_CELL;

  if (len % BITS_PER_CELL) size++;

  NEWCELL_WITH_LEN(z, bitvector, sizeof(struct bitvector_obj) + (size-1)* sizeof(uint_fast8_t));
  BVECTOR_SIZE(z)   = size;
  BVECTOR_LENGTH(z) = len;

  if (init && (init == MAKE_INT(1) || init == STk_true))
    memset(&BVECTOR_DATA(z)[0],0xff,size);

  return z;
}

/***
    Bit Conversion
 ***/

DEFINE_PRIMITIVE("bit->integer", bit2int, subr1, (SCM bit)) {
  return MAKE_INT(get_bit(bit));
}

DEFINE_PRIMITIVE("bit->boolean", bit2bool, subr1, (SCM bit)) {
  return MAKE_BOOLEAN(get_bit(bit));
}

/***
    Constructors
 ***/

DEFINE_PRIMITIVE("make-bitvector", make_bitvector, subr12, (SCM len, SCM bit)) {
  check_integer(len, "length");
  long Clen = INT_VAL(len);
  if (Clen<0) STk_error("negative bitvector size ~S", len);
  int b = bit ? get_bit(bit) : 0;

  return STk_make_bvect(Clen, MAKE_INT(b));
}

/* (bitvector ...)                        => in Scheme */
/* (SCM f, SCM length, SCM seed ...)      => in Scheme */
/* (bitvector-unfold-right f length seed) => in Scheme */


DEFINE_PRIMITIVE("bitvector-copy", bitvector_copy, vsubr, (int argc, SCM *argv)) {
  /* (bitvector-copy bvec [start [end]]) */
  if (argc == 0) return STk_true;
  SCM bvec = *argv--;
  argc--;
  check_bitvector(bvec);
  long start, end;
  control_index(bvec, argc, argv, &start, &end);

  SCM new_bvec = STk_make_bvect(end-start, 0);
  bvcopy(new_bvec, 0,
         bvec, start, end-start,
         0);
  return new_bvec;
}

DEFINE_PRIMITIVE("bitvector-reverse-copy", bitvector_rev_copy, vsubr, (int argc, SCM *argv)) {
  /* (bitvector-reverse-copy bvec [start [end]]) */
  if (argc == 0) return STk_true;
  SCM bvec = *argv--;
  argc--;
  check_bitvector(bvec);

  long start, end;
  control_index(bvec, argc, argv, &start, &end);

  SCM new_bvec = STk_make_bvect(end-start, 0);
  bvcopy(new_bvec, 0,
         bvec, start, end-start,
         1);
  return new_bvec;
}


DEFINE_PRIMITIVE("bitvector-append", bitvector_append, vsubr, (int argc, SCM *argv)) {
  long size = 0;
  for (int i=0; i<argc; i++) {
    check_bitvector(argv[-i]);
    size += BVECTOR_LENGTH(argv[-i]);
  }
  SCM bv = STk_make_bvect(size, 0);
  long pos_new = 0;
  for (int i=0; i<argc; i++)
    for (long pos_each=0; pos_each < BVECTOR_LENGTH(argv[-i]); pos_each++, pos_new++)
      bvector_set(bv, pos_new, bvector_ref(argv[-i], pos_each));
  return bv;
}

/* (bitvector-concatenate list-of-bitvectors) => in Scheme */

DEFINE_PRIMITIVE("bitvector-append-subbitvectors", bitvector_append_sub, vsubr, (int argc, SCM *argv)) {
//(bitvector-append-subbitvectors [bvec start end] ...)
  long size = 0;
  if (argc % 3) STk_error("bad argument list - not a multiple of three");

  long start, end;
  for (int i=0; i<argc; i+=3) {
    check_bitvector(argv[-i]);
    control_start_end_idx(argv[-i], argv[-i-1], argv[-i-2], &start, &end);
    size += end - start;
  }
  SCM bv = STk_make_bvect(size, 0);
  long pos_new = 0;

  for (int i=0; i<argc; i+=3)
    for (long pos_each=INT_VAL(argv[-i-1]); /* start for i-th bitvector*/
         pos_each < INT_VAL(argv[-i-2]);   /* end for i-th bitvector*/
         pos_each++, pos_new++)
      bvector_set(bv, pos_new, bvector_ref(argv[-i], pos_each));
  return bv;
}

/***
    PREDICATES
 ***/

DEFINE_PRIMITIVE("bitvector?", bitvectorp, subr1, (SCM obj)) {
    return MAKE_BOOLEAN(BVECTORP(obj));
}

DEFINE_PRIMITIVE("bitvector-empty?", bitvector_empty, subr1, (SCM bvec)) {
  check_bitvector(bvec);
  return BVECTOR_LENGTH(bvec) == 0
    ? STk_true
    : STk_false;
}

static SCM test_equal_bitvector(SCM x, SCM y) {
    if (BVECTOR_LENGTH(x) != BVECTOR_LENGTH(y)) return STk_false;
    /* Test the zero case here, because otherwise we'd end up calculating
       "BVECTOR_SIZE(x)-1" below, and accessing negative indices! */
    if (BVECTOR_LENGTH(x) == 0) return STk_true;

    /* We first test the "full" initial bytes, then compara the
       incomplete byte a bit at a time. */
    for (long i=0; i < BVECTOR_SIZE(x)-1; i++)
      if (BVECTOR_DATA(x)[i] != BVECTOR_DATA(y)[i]) return STk_false;
    for (long i = BITS_PER_CELL*(BVECTOR_SIZE(x)-1); i < BVECTOR_LENGTH(x); i++)
      if (bvector_ref(x,i) != bvector_ref(y,i)) return STk_false;

    return STk_true;
}


DEFINE_PRIMITIVE("bitvector=?", bitvector_equal, vsubr, (int argc, SCM *argv)) {
  if (argc == 0) return STk_true;
  SCM first = *argv--;
  check_bitvector(first);
  for (int i=1; i < argc; i++, argv--) {
    check_bitvector(*argv);
    if (test_equal_bitvector(first, *argv) == STk_false) return STk_false;
  }

  return STk_true;
}


/***
    SELECTORS
 ***/

DEFINE_PRIMITIVE("bitvector-ref/int", bitvector_ref_int, subr2, (SCM bvec, SCM i)) {
  check_bitvector(bvec);
  check_integer(i, " index");
  return MAKE_INT(bvector_ref(bvec,get_index(bvec,i)));
}

DEFINE_PRIMITIVE("bitvector-ref/bool", bitvector_ref_bool, subr2, (SCM bvec, SCM i)) {
  check_bitvector(bvec);
  check_integer(i, " index");
  return bvector_ref(bvec,get_index(bvec,i))
    ? STk_true
    : STk_false;
}


DEFINE_PRIMITIVE("bitvector-length", bitvector_length, subr1, (SCM bvec)) {
  check_bitvector(bvec);
  return MAKE_INT(BVECTOR_LENGTH(bvec));
}

/***
    ITERATION
 ***/

DEFINE_PRIMITIVE("bitvector-take", bitvector_take, subr2, (SCM bvec, SCM n)) {
  check_bitvector(bvec);

  check_integer(n, "quantity");
  long num = INT_VAL(n);
  if (num < 0) STk_error("negative quantity ~S", n);
  if (num > BVECTOR_LENGTH(bvec)) STk_error("amount ~S larger than bitvector", n);
  SCM bv = STk_make_bvect(num, MAKE_INT(0));
  bvcopy(bv,0,        /* dest */
         bvec,0,num,  /* source */
         0);          /* don't reverse */
  return bv;
}

DEFINE_PRIMITIVE("bitvector-take-right", bitvector_take_right, subr2, (SCM bvec, SCM n)) {
  check_bitvector(bvec);

  check_integer(n, "quantity");
  long num = INT_VAL(n);
  if (num < 0) STk_error("negative quantity ~S", n);
  if (num > BVECTOR_LENGTH(bvec)) STk_error("amount ~S larger than bitvector", n);
  SCM bv = STk_make_bvect(num, MAKE_INT(0));
  bvcopy(bv, 0,                                /* dest */
         bvec,BVECTOR_LENGTH(bvec) - num, num, /* source */
         0);                                   /* don't reverse */
  return bv;
}

DEFINE_PRIMITIVE("bitvector-drop", bitvector_drop, subr2, (SCM bvec, SCM n)) {
  check_bitvector(bvec);

  check_integer(n, "quantity");
  long num = INT_VAL(n);
  if (num < 0) STk_error("negative quantity ~S", n);
  if (num > BVECTOR_LENGTH(bvec)) STk_error("amount ~S larger than bitvector", n);
  long size = BVECTOR_LENGTH(bvec)-num;
  SCM bv = STk_make_bvect(size, MAKE_INT(0));
  bvcopy(bv,0,          /* dest */
         bvec,num,size, /* source */
         0);            /* don't reverse */
  return bv;
}


DEFINE_PRIMITIVE("bitvector-drop-right", bitvector_drop_right, subr2, (SCM bvec, SCM n)) {
  check_bitvector(bvec);

  check_integer(n, "quantity");
  long num = INT_VAL(n);
  if (num < 0) STk_error("negative quantity ~S", n);
  if (num > BVECTOR_LENGTH(bvec)) STk_error("amount ~S larger than bitvector", n);
  long size = BVECTOR_LENGTH(bvec)-num;
  SCM bv = STk_make_bvect(size, MAKE_INT(0));
  bvcopy(bv,0,          /* dest */
         bvec,0,size,   /* source */
         0);            /* don't reverse */
  return bv;
}

DEFINE_PRIMITIVE("bitvector-segment", bitvector_segment, subr2, (SCM bvec, SCM n)) {
  check_bitvector(bvec);

  check_integer(n, "segment size");
  long segsize = INT_VAL(n);
  if (segsize <= 0) STk_error("bad segment size ~S", n);
  long q = BVECTOR_LENGTH(bvec) / segsize;
  long r = BVECTOR_LENGTH(bvec) % segsize;

  SCM res = STk_nil;
  SCM this_bv;

  for (long i=0; i < q; i++) {
      this_bv = STk_make_bvect(segsize,0);
      bvcopy(this_bv, 0,                /* dest */
             bvec, i*segsize, segsize,  /* source */
             0);                        /* don't reverse */
      res = STk_cons(this_bv,res);
  }

  if (r) {
      this_bv = STk_make_bvect(r,0);
      bvcopy(this_bv, 0, /* dest */
             bvec, q, r, /* source */
             0);         /* don't reverse */
      res = STk_cons(this_bv,res);
  }
  return STk_reverse(res); /* FIXME: it's possible to do this without reversing */
}

/* (bitvector-fold/int kons knil bvec1 bvec2 ...)        => in Scheme */
/* (bitvector-fold/bool kons knil bvec1 bvec2 ...)       => in Scheme */
/* (bitvector-fold-right/int kons knil bvec1 bvec2 ...)  => in Scheme */
/* (bitvector-fold-right/bool kons knil bvec1 bvec2 ...) => in Scheme */
/* (bitvector-map/int f bvec1 bvec2 ...)                 => in Scheme */
/* (bitvector-map/bool f bvec1 bvec2 ...)                => in Scheme */
/* (bitvector-map!/int f bvec1 bvec2 ...)                => in Scheme */
/* (bitvector-map!/bool f bvec1 bvec2 ...)               => in Scheme */
/* (bitvector-map->list/int f bvec1 bvec2 ...)           => in Scheme */
/* (bitvector-map->list/bool f bvec1 bvec2 ...)          => in Scheme */
/* (bitvector-for-each/int f bvec1 bvec2 ...)            => in Scheme */
/* (bitvector-for-each/bool f bvec1 bvec2 ...)           => in Scheme */


/***
    Prefixes, suffixes, trimming, padding
***/

/* Checks the size of the common prefix (or suffix) of
   the two bitvectors. */
static inline
long bvprefix_size(SCM bv1, SCM bv2, int from_end) {
    long len1 = BVECTOR_LENGTH(bv1) ;
    long len2 = BVECTOR_LENGTH(bv2) ;
    long len  = len1 < len2 ? len1 : len2;
    long size = 0;
    long pos1 = from_end ? len1 - 1 : 0;
    long pos2 = from_end ? len2 - 1 : 0;
    long step = from_end ? -1 : +1;
    for (long i=0; i < len; i++) {
        if (bvector_ref(bv1, pos1) != bvector_ref(bv2, pos2))
            return size;
        size++;
        pos1 += step;
        pos2 += step;
    }
    return size;
}

DEFINE_PRIMITIVE("bitvector-prefix-length", bitvector_pref_len, subr2, (SCM bvec1, SCM bvec2)) {
  check_bitvector(bvec1);
  check_bitvector(bvec2);

  return MAKE_INT(bvprefix_size(bvec1, bvec2, 0));
}

DEFINE_PRIMITIVE("bitvector-suffix-length", bitvector_suf_len, subr2, (SCM bvec1, SCM bvec2)) {
  check_bitvector(bvec1);
  check_bitvector(bvec2);

  return MAKE_INT(bvprefix_size(bvec1, bvec2, 1));
}

DEFINE_PRIMITIVE("bitvector-prefix?", bitvector_pref, subr2, (SCM bvec1, SCM bvec2)) {
  check_bitvector(bvec1);
  check_bitvector(bvec2);

  return MAKE_BOOLEAN (bvprefix_size(bvec1, bvec2, 0) == BVECTOR_LENGTH(bvec1));
}

DEFINE_PRIMITIVE("bitvector-suffix?", bitvector_suf, subr2, (SCM bvec1, SCM bvec2)) {
  check_bitvector(bvec1);
  check_bitvector(bvec2);

  return MAKE_BOOLEAN (bvprefix_size(bvec1, bvec2, 1) == BVECTOR_LENGTH(bvec1));
}

/* Pads a bitvector with a specific bit. */
static inline
SCM pad(SCM bv, int bit, long len, int from_right) {
  if (len < BVECTOR_LENGTH(bv)) len = BVECTOR_LENGTH(bv);
  SCM new_bvec = STk_make_bvect(len, 0);

  long padlen = len - BVECTOR_LENGTH(bv);
  long pad_start_pos = from_right? BVECTOR_LENGTH(bv) : 0;
  long pad_copy_pos = from_right? 0 : padlen;

  for(long i = 0; i < padlen; i++)
      bvector_set(new_bvec, pad_start_pos+i, bit);

  for(long i = 0; i < BVECTOR_LENGTH(bv); i++)
      bvector_set(new_bvec, i + pad_copy_pos, bvector_ref(bv, i));

  return new_bvec;
}

DEFINE_PRIMITIVE("bitvector-pad", bitvector_pad, subr3, (SCM bit, SCM bvec, SCM len)) {
  check_bitvector(bvec);
  check_integer(len, "length");
  return pad(bvec, get_bit(bit), INT_VAL(len), 0);
}
DEFINE_PRIMITIVE("bitvector-pad-right", bitvector_pad_right, subr3, (SCM bit, SCM bvec, SCM len)) {
  check_bitvector(bvec);
  check_integer(len, "length");
  return pad(bvec, get_bit(bit), INT_VAL(len), 1);
}

DEFINE_PRIMITIVE("bitvector-trim", bitvector_trim, subr2, (SCM bit, SCM bvec)) {
  check_bitvector(bvec);
  long k = count_run(get_bit(bit), bvec, 0, 0);
  SCM bv = STk_make_bvect(BVECTOR_LENGTH(bvec)-k, MAKE_INT(0));
  bvcopy(bv,0,                           /* dest */
         bvec,k,BVECTOR_LENGTH(bvec)-k,  /* source */
         0);                             /* don't reverse */
  return bv;
}

DEFINE_PRIMITIVE("bitvector-trim-right", bitvector_trim_right, subr2, (SCM bit, SCM bvec)) {
  check_bitvector(bvec);
  long k = count_run(get_bit(bit), bvec, BVECTOR_LENGTH(bvec)-1 , 1);
  SCM bv = STk_make_bvect(BVECTOR_LENGTH(bvec)-k, MAKE_INT(0));
  bvcopy(bv,0,                           /* dest */
         bvec,0,BVECTOR_LENGTH(bvec)-k,  /* source */
         0);                             /* don't reverse */
  return bv;
}

DEFINE_PRIMITIVE("bitvector-trim-both", bitvector_trim_both, subr2, (SCM bit, SCM bvec)) {
    return STk_bitvector_trim(bit, STk_bitvector_trim_right(bit, bvec));
}


/***
    Mutators
***/


DEFINE_PRIMITIVE("bitvector-set!", bitvector_set, subr3, (SCM bvec, SCM i, SCM bit)) {
  check_bitvector(bvec);
  check_integer(i, "index");
  bvector_set(bvec, get_index(bvec,i), get_bit(bit));
  return STk_void;
}

DEFINE_PRIMITIVE("bitvector-swap!", bitvector_swap, subr3, (SCM bvec, SCM i, SCM j)) {
  check_bitvector(bvec);
  bvector_swap(bvec,get_index(bvec,i),get_index(bvec,j));
  return STk_void;
}


DEFINE_PRIMITIVE("bitvector-reverse!", bitvector_nrev, vsubr, (int argc, SCM *argv)) {
/* (bitvector-reverse! bvec [start [end]]) -> unspecified */
  if (argc < 1) STk_error("at least one argument needed");
  if (argc > 3) STk_error("at most three arguments allowed");
  SCM bvec = *argv--;
  argc--;
  check_bitvector(bvec);

  long start, end;
  control_index(bvec, argc, argv, &start, &end);

  for (long i=0; i < (end-start)/2; i++)
    bvector_swap(bvec, start+i, end-i-1);

  return STk_void;
}

DEFINE_PRIMITIVE("bitvector-copy!", bitvector_ncopy, vsubr, (int argc, SCM *argv)) {
/* (bitvector-copy! to at from [start [end]]) -> unspecified */
  if (argc < 3) STk_error("at least three arguments needed");
  if (argc > 5) STk_error("at most five arguments allowed");

  SCM to   = *argv--;
  SCM at   = *argv--;
  SCM from = *argv--;
  argc -= 3;
  check_bitvector(to);
  check_bitvector(from);

  check_integer(at, "index");
  long from_start, from_end;
  control_index(from, argc, argv, &from_start, &from_end);
  long size = from_end - from_start;
  long Cat = INT_VAL(at);
  if (size + Cat >= BVECTOR_LENGTH(to))
    STk_error("cannot copy past end of bitvector: pos=%d vector end=%d, size=%d",
              Cat, BVECTOR_LENGTH(to), size);
  bvcopy(to, Cat,
         from, from_start, size,
         0);
  return STk_void;
}


DEFINE_PRIMITIVE("bitvector-reverse-copy!", bitvector_rev_ncopy, vsubr, (int argc, SCM *argv)) {
/* (bitvector-reverse-copy! to at from [start [end]]) -> unspecified */
  if (argc < 3) STk_error("at least three arguments needed");
  if (argc > 5) STk_error("at most five arguments allowed");

  SCM to   = *argv--;
  SCM at   = *argv--;
  SCM from = *argv--;
  argc -= 3;
  check_bitvector(to);
  check_bitvector(from);

  check_integer(at, "index");
  long from_start, from_end;
  control_index(from, argc, argv, &from_start, &from_end);
  long size = from_end - from_start;
  long Cat = INT_VAL(at);
  if (size + Cat >= BVECTOR_LENGTH(to))
    STk_error("cannot copy past end of bitvector: pos=%d vector end=%d, size=%d",
              Cat, BVECTOR_LENGTH(to), size);
  bvcopy(to, Cat,
         from, from_start, size,
         1);
  return STk_void;
}


/***
    Conversion
***/

/* Turns a bitvector into a list.
   If need_bool is non-zero, writes booleans into the list, otherwise
   writes integers. */
static inline
SCM bv2list(SCM bv, long start, long end, int need_bool, int reverse) {
  SCM L = STk_nil;
  long step = reverse? +1 : -1;
  long max = end-start;
  if (reverse) {
    long tmpstart = start;
    start         = end - 1;
    end           = tmpstart + 1;
  }
  long i = end-1;
  for (long c=0; c < max; c++, i+= step) {
    if (need_bool)
      L = STk_cons(bvector_ref(bv,i) ? STk_true : STk_false,L);
    else
      L = STk_cons(MAKE_INT(bvector_ref(bv,i)),L);
  }
  return L;
}

DEFINE_PRIMITIVE("bitvector->list/int", bitvec2list_int, vsubr, (int argc, SCM *argv)) {
  if (argc < 1) STk_error("at least one argument needed");
  if (argc > 3) STk_error("at most three arguments allowed");
  SCM bvec = *argv--; argc--;
  check_bitvector(bvec);

  long start, end;
  control_index(bvec, argc, argv, &start, &end);
  return bv2list(bvec, start, end, 0, 0);
}

DEFINE_PRIMITIVE("bitvector->list/bool", bitvec2list_bool, vsubr, (int argc, SCM *argv)) {
  if (argc < 1) STk_error("at least one argument needed");
  if (argc > 3) STk_error("at most three arguments allowed");
  SCM bvec = *argv--; argc--;
  check_bitvector(bvec);

  long start, end;
  control_index(bvec, argc, argv, &start, &end);
  return bv2list(bvec, start, end, 1, 0);
}

DEFINE_PRIMITIVE("reverse-bitvector->list/int", rev_bitvec2list_int, vsubr, (int argc, SCM *argv)) {
  if (argc < 1) STk_error("at least one argument needed");
  if (argc > 3) STk_error("at most three arguments allowed");
  SCM bvec = *argv--; argc--;
  check_bitvector(bvec);

  long start, end;
  control_index(bvec, argc, argv, &start, &end);
  return bv2list(bvec, start, end, 0, 1);
}

DEFINE_PRIMITIVE("reverse-bitvector->list/bool", rev_bitvec2list_bool, vsubr, (int argc, SCM *argv)) {
  if (argc < 1) STk_error("at least one argument needed");
  if (argc > 3) STk_error("at most three arguments allowed");
  SCM bvec = *argv--; argc--;
  check_bitvector(bvec);

  long start, end;
  control_index(bvec, argc, argv, &start, &end);
  return bv2list(bvec, start, end, 1, 1);
}

/* Transforms a list into a bitvector. */
static inline
SCM lst2bv(SCM lst, int reverse) {
    long pos = 0;
    long len = STk_int_length(lst);
    if (len < 0) STk_error("improper list ~S", lst);
    SCM bvec = STk_make_bvect(len, STk_false);

    long step = 1;
    if (reverse) {
        step = -1;
        pos = len-1;
        len = 0;
    }
    for (SCM ptr = lst; !NULLP(ptr); ptr = CDR(ptr), pos+=step)
      bvector_set(bvec, pos, get_bit(CAR(ptr)));

    return bvec;
}

DEFINE_PRIMITIVE("list->bitvector", list2bitvec, subr1, (SCM lst)) {
    if (NULLP(lst)) return STk_make_bvect(0,STk_false);
    check_cons(lst);
    return lst2bv(lst,0);
}

DEFINE_PRIMITIVE("reverse-list->bitvector", revlist2bitvec, subr1, (SCM lst)) {
    if (NULLP(lst)) return STk_make_bvect(0,STk_false);
    check_cons(lst);
    return lst2bv(lst,1);
}

/* Transforms a bitvector into a vector. */
static inline
SCM bv2vect(SCM bv, long start, long end, int want_bool, int rev) {
  SCM vec = STk_makevect(end-start, want_bool ? STk_false : MAKE_INT(0));
  SCM one = want_bool ? STk_true : MAKE_INT(1);
  long step  = rev ? -1 :+1;
  long pos = rev ? end - start - 1 : 0;
  for (long i=start; i < end; i++, pos += step)
    if (bvector_ref(bv, i))
      STk_vector_set(vec, MAKE_INT(pos), one);
  return vec;
}

DEFINE_PRIMITIVE("bitvector->vector/int", bitvector2vec_int, vsubr, (int argc, SCM *argv)) {
/* (bitvector->vector/int bvec [start [end]]) -> vector of integers */
  if (argc < 1) STk_error("at least one argument needed");
  if (argc > 3) STk_error("at most three arguments allowed");
  SCM bvec = *argv--; argc--;
  check_bitvector(bvec);
  long start, end;
  control_index(bvec, argc, argv, &start, &end);
  return bv2vect(bvec, start, end, 0, 0);
}

DEFINE_PRIMITIVE("bitvector->vector/bool", bitvector2vec_bool, vsubr, (int argc, SCM *argv)) {
/* (bitvector->vector/bool bvec [start [end]]) -> vector of booleans */
  if (argc < 1) STk_error("at least one argument needed");
  if (argc > 3) STk_error("at most three arguments allowed");
  SCM bvec = *argv--; argc--;
  check_bitvector(bvec);

  long start, end;
  control_index(bvec, argc, argv, &start, &end);
  return bv2vect(bvec, start, end, 1, 0);
}

DEFINE_PRIMITIVE("reverse-bitvector->vector/int", bitvector2vec_revint, vsubr, (int argc, SCM *argv)) {
/* (reverse-bitvector->vector/int bvec [start [end]]) -> vector of integers */
  if (argc < 1) STk_error("at least one argument needed");
  if (argc > 3) STk_error("at most three arguments allowed");
  SCM bvec = *argv--; argc--;
  check_bitvector(bvec);

  long start, end;
  control_index(bvec, argc, argv, &start, &end);
  return bv2vect(bvec, start, end, 0, 1);
}

DEFINE_PRIMITIVE("reverse-bitvector->vector/bool", bitvector2vec_revbool, vsubr, (int argc, SCM *argv)) {
/* (reverse-bitvector->vector/bool bvec [start [end]]) -> vector of booleans */
  if (argc < 1) STk_error("at least one argument needed");
  if (argc > 3) STk_error("at most three arguments allowed");
  SCM bvec = *argv--; argc--;
  check_bitvector(bvec);

  long start, end;
  control_index(bvec, argc, argv, &start, &end);
  return bv2vect(bvec, start, end, 1, 1);
}

/* Transforms a vector into a bitvector. */
static inline
SCM vec2bv(SCM vec, long start, long end, int rev) {
    SCM bv = STk_make_bvect(end - start, 0);
    long step = rev ? -1 : +1;
    long pos = rev ? end - 1 : start;

    for (long i = 0; i < end-start; i++, pos += step)
      if (get_bit(VECTOR_DATA(vec)[pos]))
        bvector_set(bv,i,1);

    return bv;
}

DEFINE_PRIMITIVE("vector->bitvector", vector2bitvector, vsubr, (int argc, SCM *argv)) {
/* (vector->bitvector vec [start [end]]) -> bitvector */
  if (argc < 1) STk_error("at least one argument needed");
  if (argc > 3) STk_error("at most three arguments allowed");
  SCM vec = *argv--; argc--;
  if (!VECTORP(vec)) STk_error("bad vector ~S", vec);

  SCM start = MAKE_INT(0);
  SCM end   = MAKE_INT(VECTOR_SIZE(vec));

  if (argc > 0) {
    start = *argv--;
    check_integer(start, "start index");
  }
  if (argc > 1) {
    end = *argv--;
    check_integer(end, "end index");
  }
  long Cstart = INT_VAL(start);
  long Cend   = INT_VAL(end);

  if (Cstart < 0) STk_error("negative start index ~S", start);
  if (Cend   < 0) STk_error("negative end index ~S", end);
  if (Cstart > VECTOR_SIZE(vec)) STk_error("start index out of range ~S", start);
  if (Cend   > VECTOR_SIZE(vec)) STk_error("end index out of range ~S", end);
  if (Cstart > INT_VAL(end)) STk_error("start index ~S > end index ~S", start, end);

  return vec2bv(vec, Cstart, Cend, 0);
}

DEFINE_PRIMITIVE("reverse-vector->bitvector", vector2bitvector_rev, vsubr, (int argc, SCM *argv)) {
/* (reverse-vector->bitvector vec [start [end]]) -> bitvector */
  if (argc < 1) STk_error("at least one argument needed");
  if (argc > 3) STk_error("at most three arguments allowed");
  SCM vec = *argv--; argc--;
  if (!VECTORP(vec)) STk_error("bad vector ~S", vec);

  SCM start = MAKE_INT(0);
  SCM end   = MAKE_INT(VECTOR_SIZE(vec));

  if (argc > 0) {
    start = *argv--;
    check_integer(start, "start index");
  }
  if (argc > 1) {
    end = *argv--;
    check_integer(end, "end index");
  }
  long Cstart = INT_VAL(start);
  long Cend   = INT_VAL(end);

  if (Cstart < 0) STk_error("negative start index ~S", start);
  if (Cend   < 0) STk_error("negative end index ~S", end);
  if (Cstart > VECTOR_SIZE(vec)) STk_error("start index out of range ~S", start);
  if (Cend   > VECTOR_SIZE(vec)) STk_error("end index out of range ~S", end);
  if (Cstart > INT_VAL(end)) STk_error("start index ~S > end index ~S", start, end);

  return vec2bv(vec, Cstart, Cend, 1);
}

static char* bv2string(SCM bv) {
  long i, n = BVECTOR_LENGTH(bv);
  char *out = STk_must_malloc_atomic(n+3);
  out[0] = '#';
  out[1] = '*';
  for (i = 0; i < n; i++)
    if (bvector_ref(bv, i))
      out[i+2] = '1';
    else
      out[i+2] = '0';
  out[n+2] = '\0';
  return out;
}

DEFINE_PRIMITIVE("bitvector->string", bv2str, subr1, (SCM bvec)) {
  check_bitvector(bvec);
  return STk_Cstring2string(bv2string(bvec));
}

DEFINE_PRIMITIVE("string->bitvector", str2bv, subr1, (SCM str)) {
  if (!STRINGP(str)) STk_error("bad string ~S", str);
  long i = 0;
  if (STRING_LENGTH(str) < 2) return STk_false;
  char *s = STRING_CHARS(str);
  if (s[0] != '#' | s[1] != '*') return STk_false;
  SCM bv = STk_make_bvect(STRING_LENGTH(str)-2, MAKE_INT(0));
  for (i=2; i<STRING_LENGTH(str); i++) {
    if (s[i] == '1') bvector_set(bv,i-2,1);
    else if (s[i] != '0') return STk_false;
  }
  return bv;
}

/*******************************/
/*** START: BIGNUMS required ***/
/*******************************/

/* We had to copy over some parts of number.c into here, in order to
   make bitvector->integer work. */

struct bignum_obj {
  stk_header header;
  mpz_t val;
};

#define BIGNUM_VAL(p)   (((struct bignum_obj *) (p))->val)
#define BIGNUM_FITS_INTEGER(_bn) (mpz_cmp_si((_bn), INT_MIN_VAL) >= 0 &&        \
                                  mpz_cmp_si((_bn), INT_MAX_VAL) <= 0)
static inline SCM bignum2integer(mpz_t n)
{
  return MAKE_INT(mpz_get_si(n));
}
static inline SCM bignum2scheme_bignum(mpz_t n)
{
  SCM z;

  NEWCELL(z, bignum);
  mpz_init_set(BIGNUM_VAL(z), n);
  return z;
}
static inline SCM bignum2number(mpz_t n)  /* => int or bignum */
{
  return (BIGNUM_FITS_INTEGER(n)) ? bignum2integer(n): bignum2scheme_bignum(n);
}

/* bitvector->integer needs integer-length: */
EXTERN_PRIMITIVE("integer-length", integer_length, subr1, (SCM z));

DEFINE_PRIMITIVE("bitvector->integer", bitvec2int, subr1, (SCM bvec)) {
  check_bitvector(bvec);
  mpz_t n;
  mpz_t tmp;
  mpz_init(tmp);
  mpz_init_set_ui(n, 0);
  for (long i=0; i < BVECTOR_LENGTH(bvec); i++)
      if (bvector_ref(bvec, i)) {
          mpz_ui_pow_ui(tmp,2,i);
          mpz_add(n,n,tmp);
      }
  mpz_clear(tmp);
  return bignum2number(n);
}

/*****************************/
/*** END: BIGNUMS required ***/
/*****************************/


SCM long2bitvector(long n, long L) {
    SCM bv = STk_make_bvect(L, 0);
    for (long i=0; i<L; i++, n /= 2)
        bvector_set(bv, i, n % 2);
    return bv;
}
SCM mpz2bitvector(mpz_t n, long L) {
    SCM bv = STk_make_bvect(L, 0);
    mpz_t q;
    mpz_init(q);
    /* mpz_tdiv_q_2exp(n,n,1) is division by 2 */
    for (long i=0; i<L; i++, mpz_tdiv_q_2exp(n,n,1)) {
        mpz_tdiv_r_2exp(q, n, 1);       /* remainder */
        bvector_set(bv, i, mpz_sgn(q)); /* remainder is either 0 or 1 */
    }
    return bv;
}

DEFINE_PRIMITIVE("integer->bitvector", int2bitvector, subr12, (SCM n, SCM len)) {
/* (integer->bitvector integer [ len ]) */
  if (!(INTP(n) || BIGNUMP(n))) STk_error("bad integer ~S", len);
  long L;
  if (len) {
    check_integer(len, "length");
      L = INT_VAL(len);
      if (L < 0) STk_error("negative size ~S", L);
  } else
      L = INT_VAL(STk_integer_length(n));
  if (INTP(n)) return long2bitvector(INT_VAL(n), L);
  return mpz2bitvector(BIGNUM_VAL(n), L);
}

/***
    Generators
***/

/* (make-bitvector/int-generator bitvector)  => In Scheme */
/* (make-bitvector/bool-generator bitvector) => In Scheme */
/* (make-bitvector-accumulator)              => In Scheme */


/***
    Basic operations
***/

DEFINE_PRIMITIVE("bitvector-not", bitvector_not, subr1, (SCM bvec)) {
  check_bitvector(bvec);
  SCM new = STk_make_bvect(BVECTOR_LENGTH(bvec), 0);
  for (long i=0; i < BVECTOR_SIZE(bvec); i++)
    BVECTOR_DATA(new)[i] = ~BVECTOR_DATA(bvec)[i];
  return new;
}

DEFINE_PRIMITIVE("bitvector-not!", bitvector_nnot, subr1, (SCM bvec)) {
  check_bitvector(bvec);
  for (long i=0; i < BVECTOR_SIZE(bvec); i++)
    BVECTOR_DATA(bvec)[i] = ~BVECTOR_DATA(bvec)[i];
  return bvec;
}

#define AND   1
#define IOR   2
#define XOR   3
#define NAND  4
#define NOR   5
#define ANDC1 6
#define ANDC2 7
#define ORC1  8
#define ORC2  9
#define NXOR  10

static inline SCM
iterate_op(SCM bv1, SCM bv2, int op, int destruc) {
    long size = BVECTOR_SIZE(bv1);
    if (bv2 && size != BVECTOR_SIZE(bv2))
      STk_error("cannot operate on bitvectors of different sizes %d and %d",
                size, BVECTOR_SIZE(bv2));
    SCM bv = destruc
        ? bv1
        :  STk_make_bvect(BVECTOR_LENGTH(bv1), 0);
    for (long i = 0; i < size; i++)
        switch(op) {
        case AND:   BVECTOR_DATA(bv)[i] = BVECTOR_DATA(bv1)[i] & BVECTOR_DATA(bv2)[i];    break;
        case IOR:   BVECTOR_DATA(bv)[i] = BVECTOR_DATA(bv1)[i] | BVECTOR_DATA(bv2)[i];    break;
        case XOR:   BVECTOR_DATA(bv)[i] = BVECTOR_DATA(bv1)[i] ^ BVECTOR_DATA(bv2)[i];    break;
        case NAND:  BVECTOR_DATA(bv)[i] = ~(BVECTOR_DATA(bv1)[i] & BVECTOR_DATA(bv2)[i]); break;
        case NOR:   BVECTOR_DATA(bv)[i] = ~(BVECTOR_DATA(bv1)[i] | BVECTOR_DATA(bv2)[i]); break;
        case ANDC1: BVECTOR_DATA(bv)[i] = ~(BVECTOR_DATA(bv1)[i]) & BVECTOR_DATA(bv2)[i]; break;
        case ANDC2: BVECTOR_DATA(bv)[i] = BVECTOR_DATA(bv1)[i] & ~(BVECTOR_DATA(bv2)[i]); break;
        case ORC1:  BVECTOR_DATA(bv)[i] = ~(BVECTOR_DATA(bv1)[i]) | BVECTOR_DATA(bv2)[i]; break;
        case ORC2:  BVECTOR_DATA(bv)[i] = BVECTOR_DATA(bv1)[i] | ~(BVECTOR_DATA(bv2)[i]); break;
        case NXOR:  BVECTOR_DATA(bv)[i] = ~(BVECTOR_DATA(bv1)[i] ^ BVECTOR_DATA(bv2)[i]); break;
        default: STk_error("PANIC: internal error -- unknown operation on bitvector");
        }
    return destruc ? STk_void : bv;
}


DEFINE_PRIMITIVE("bitvector-and", bitvector_and, vsubr, (int argc, SCM *argv)) {
  if (argc < 2) STk_error("at least 2 arguments needed");
  SCM b1 = *argv--; argc--;
  SCM b2 = *argv--; argc--;
  check_bitvector(b1);
  check_bitvector(b2);

  SCM res = iterate_op(b1, b2, AND, 0);
  SCM next;
  for (int i = 0; i < argc; i++) {
      next = *argv--;
      iterate_op(res, next, AND, 1);
  }
  return res;
}


DEFINE_PRIMITIVE("bitvector-and!", bitvector_dand, vsubr, (int argc, SCM *argv)) {
  if (argc < 2) STk_error("at least 2 arguments needed");
  SCM b1 = *argv--; argc--;
  SCM b2 = *argv--; argc--;
  check_bitvector(b1);
  check_bitvector(b2);

  iterate_op(b1, b2, AND, 1);
  SCM next;
  for (int i = 0; i < argc; i++) {
      next = *argv--;
      iterate_op(b1, next, AND, 1);
  }
  return STk_void;
}

DEFINE_PRIMITIVE("bitvector-ior", bitvector_ior, vsubr, (int argc, SCM *argv)) {
  if (argc < 2) STk_error("at least 2 arguments needed");
  SCM b1 = *argv--; argc--;
  SCM b2 = *argv--; argc--;
  check_bitvector(b1);
  check_bitvector(b2);

  SCM res = iterate_op(b1, b2, IOR, 0);
  SCM next;
  for (int i = 0; i < argc; i++) {
      next = *argv--;
      iterate_op(res, next, IOR, 1);
  }
  return res;
}


DEFINE_PRIMITIVE("bitvector-ior!", bitvector_dior, vsubr, (int argc, SCM *argv)) {
  if (argc < 2) STk_error("at least 2 arguments needed");
  SCM b1 = *argv--; argc--;
  SCM b2 = *argv--; argc--;
  check_bitvector(b1);
  check_bitvector(b2);

  iterate_op(b1, b2, IOR, 1);
  SCM next;
  for (int i = 0; i < argc; i++) {
      next = *argv--;
      iterate_op(b1, next, IOR, 1);
  }
  return STk_void;
}

DEFINE_PRIMITIVE("bitvector-xor", bitvector_xor, vsubr, (int argc, SCM *argv)) {
  if (argc < 2) STk_error("at least 2 arguments needed");
  SCM b1 = *argv--; argc--;
  SCM b2 = *argv--; argc--;
  check_bitvector(b1);
  check_bitvector(b2);

  SCM res = iterate_op(b1, b2, XOR, 0);
  SCM next;
  for (int i = 0; i < argc; i++) {
      next = *argv--;
      iterate_op(res, next, XOR, 1);
  }
  return res;
}


DEFINE_PRIMITIVE("bitvector-xor!", bitvector_dxor, vsubr, (int argc, SCM *argv)) {
  if (argc < 2) STk_error("at least 2 arguments needed");
  SCM b1 = *argv--; argc--;
  SCM b2 = *argv--; argc--;
  check_bitvector(b1);
  check_bitvector(b2);

  iterate_op(b1, b2, XOR, 1);
  SCM next;
  for (int i = 0; i < argc; i++) {
      next = *argv--;
      iterate_op(b1, next, XOR, 1);
  }
  return STk_void;
}

DEFINE_PRIMITIVE("bitvector-eqv", bitvector_nxor, vsubr, (int argc, SCM *argv)) {
  if (argc < 2) STk_error("at least 2 arguments needed");
  SCM b1 = *argv--; argc--;
  SCM b2 = *argv--; argc--;
  check_bitvector(b1);
  check_bitvector(b2);

  SCM res = iterate_op(b1, b2, NXOR, 0);
  SCM next;
  for (int i = 0; i < argc; i++) {
      next = *argv--;
      iterate_op(res, next, NXOR, 1);
  }
  return res;
}


DEFINE_PRIMITIVE("bitvector-eqv!", bitvector_dnxor, vsubr, (int argc, SCM *argv)) {
  if (argc < 2) STk_error("at least 2 arguments needed");
  SCM b1 = *argv--; argc--;
  SCM b2 = *argv--; argc--;
  check_bitvector(b1);
  check_bitvector(b2);

  iterate_op(b1, b2, NXOR, 1);
  SCM next;
  for (int i = 0; i < argc; i++) {
      next = *argv--;
      iterate_op(b1, next, NXOR, 1);
  }
  return STk_void;
}

DEFINE_PRIMITIVE("bitvector-nand", bitvector_nand, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, NAND, 0);
}

DEFINE_PRIMITIVE("bitvector-nand!", bitvector_nnand, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, NAND, 1);
}

DEFINE_PRIMITIVE("bitvector-nor", bitvector_nor, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, NOR, 0);
}

DEFINE_PRIMITIVE("bitvector-nor!", bitvector_nnor, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, NOR, 1);
}

DEFINE_PRIMITIVE("bitvector-andc1", bitvector_andc1, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, ANDC1, 0);
}

DEFINE_PRIMITIVE("bitvector-andc1!", bitvector_nandc1, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, ANDC1, 1);
}

DEFINE_PRIMITIVE("bitvector-andc2", bitvector_andc2, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, ANDC2, 0);
}

DEFINE_PRIMITIVE("bitvector-andc2!", bitvector_nandc2, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, ANDC2, 1);
}

DEFINE_PRIMITIVE("bitvector-orc1", bitvector_orc1, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, ORC1, 0);
}

DEFINE_PRIMITIVE("bitvector-orc1!", bitvector_norc1, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, ORC1, 1);
}

DEFINE_PRIMITIVE("bitvector-orc2", bitvector_orc2, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, ORC2, 0);
}

DEFINE_PRIMITIVE("bitvector-orc2!", bitvector_norc2, subr2, (SCM b1, SCM b2)) {
  check_bitvector(b1);
  check_bitvector(b2);
  return iterate_op(b1, b2, ORC2, 1);
}


/***
    Quasi-integer operations
***/


DEFINE_PRIMITIVE("bitvector-logical-shift", bitvector_log_shift, subr3,
                 (SCM bvec, SCM count, SCM bit)) {
  check_bitvector(bvec);
  int wanted_bit = get_bit(bit);

  check_integer(count, "count");
  long Ccount = INT_VAL(count);

  SCM bv = STk_make_bvect(BVECTOR_LENGTH(bvec),0);

  /* Move existing bits, 'count' positions to the appropriate side */
  long abs_count = labs(Ccount);
  long copy_size  = BVECTOR_LENGTH(bvec) - abs_count;
  long start_src  = Ccount < 0 ?         0 : abs_count;
  long start_dest = Ccount < 0 ? abs_count : 0;
  bvcopy(bv, start_dest,
         bvec, start_src, copy_size,
         0);

  /* Set the bit on 'count' positions in the destination, starting
     at the appropriate index */
  start_dest = Ccount < 0 ? 0 : copy_size;
  for (long i=start_dest; i < start_dest + abs_count; i++)
    bvector_set(bv,i,wanted_bit);

  return bv;
}

DEFINE_PRIMITIVE("bitvector-count", bitvector_count, subr2, (SCM bit, SCM bvec)) {
  check_bitvector(bvec);
  int wanted_bit = get_bit(bit);

  long count = 0;
  for (long i=0; i<BVECTOR_LENGTH(bvec); i++)
      if (bvector_ref(bvec,i) == wanted_bit) count++;
  return MAKE_INT(count);
}


DEFINE_PRIMITIVE("bitvector-count-run", bitvector_count_run, subr3, (SCM bit, SCM bvec, SCM i)) {
/* (bitvector-count-run bit bvec i) */
  check_bitvector(bvec);
  return MAKE_INT(count_run(get_bit(bit), bvec, get_index(bvec,i), 0));
}

DEFINE_PRIMITIVE("bitvector-if", bitvector_if, subr3, (SCM bvif, SCM bvthen, SCM bvelse)) {
/* (bitvector-if if-bitvector then-bitvector else-bitvector) */
  check_bitvector(bvif);
  check_bitvector(bvthen);
  check_bitvector(bvelse);
  if (BVECTOR_LENGTH(bvif) != BVECTOR_LENGTH(bvthen) ||
      BVECTOR_LENGTH(bvif) != BVECTOR_LENGTH(bvelse))
    STk_error("bitvector lengths are not the same: ~S, ~S, ~S",
              BVECTOR_LENGTH(bvif),
              BVECTOR_LENGTH(bvthen),
              BVECTOR_LENGTH(bvelse));
  SCM bv = STk_make_bvect(BVECTOR_LENGTH(bvif), 0);
  for (long i=0; i<BVECTOR_LENGTH(bvif); i++)
    bvector_set(bv, i,
                bvector_ref(bvif, i)
                ? bvector_ref(bvthen, i)
                : bvector_ref(bvelse, i));
  return bv;
}

DEFINE_PRIMITIVE("bitvector-first-bit", bitvector_first_bit, subr2, (SCM bit, SCM bvec)) {
  check_bitvector(bvec);
  int Cbit = get_bit(bit);

  for (long i = 0; i < BVECTOR_LENGTH(bvec); i++)
      if (bvector_ref(bvec, i) == Cbit) return MAKE_INT(i);

  return MAKE_INT(-1);
}


/***
    Bit field operations
***/

DEFINE_PRIMITIVE("bitvector-field-any?", bitvector_field_any, subr3, (SCM bvec, SCM start, SCM end)) {
/* (bitvector-field-any? bvec start end) */
  long Cstart, Cend;
  check_bitvector(bvec);
  control_start_end_idx(bvec, start, end, &Cstart, &Cend);

  /* FIXME: this could be faster by comparing entire bytes, except possibly on the
     start and end bytes... */
  for (long i = Cstart; i < Cend; i++)
      if (bvector_ref(bvec, i))
          return STk_true;

  return STk_false;
}

DEFINE_PRIMITIVE("bitvector-field-every?", bitvector_field_every, subr3, (SCM bvec, SCM start, SCM end)) {
/* (bitvector-field-every? bvec start end) */
  long Cstart, Cend;
  check_bitvector(bvec);
  control_start_end_idx(bvec, start, end, &Cstart, &Cend);

  /* FIXME: this could be faster by comparing entire bytes, except possibly on the
     start and end bytes... */
  for (long i = Cstart; i < Cend; i++)
      if (!bvector_ref(bvec, i))
          return STk_false;

  return STk_true;
}

#define CLEAR         1
#define SET           2
#define REPLACE       3
#define REPLACE_SAME  4
#define SAME          5
#define FLIP          6

static inline SCM
field_op(SCM bv1, SCM bv2, long start, long end, int op, int destruc) {
    SCM bv;
    if (destruc) bv=bv1;
    else {
        bv = STk_make_bvect(BVECTOR_LENGTH(bv1), 0);
        bvcopy(bv, 0,
               bv1, 0, start,
               0);
        bvcopy(bv, end,
               bv1, end, BVECTOR_LENGTH(bv1),
               0);
    }

    for (long i = start; i < end; i++)
        switch(op) {
        case CLEAR:        bvector_set(bv, i, 0);                        break;
        case SET:          bvector_set(bv, i, 1);                        break;
        case REPLACE:      bvector_set(bv, i, bvector_ref(bv2,i-start)); break;
        case REPLACE_SAME: bvector_set(bv, i, bvector_ref(bv2,i));       break;
        case FLIP:         bvector_set(bv, i, !bvector_ref(bv1,i));      break;
        default: STk_error("PANIC: internal error -- unknown operation on bitvector");
        }
    return bv;
}

DEFINE_PRIMITIVE("bitvector-field-clear", bitvector_field_clear, subr3, (SCM bvec, SCM start, SCM end)) {
/* (bitvector-field-clear bvec start end) */
  long Cstart, Cend;
  check_bitvector(bvec);
  control_start_end_idx(bvec, start, end, &Cstart, &Cend);
  return field_op(bvec, NULL, Cstart, Cend, CLEAR, 0);
}

DEFINE_PRIMITIVE("bitvector-field-clear!", bitvector_field_dclear, subr3, (SCM bvec, SCM start, SCM end)) {
/* (bitvector-field-clear! bvec start end) */
  long Cstart, Cend;
  check_bitvector(bvec);
  control_start_end_idx(bvec, start, end, &Cstart, &Cend);
  return field_op(bvec, NULL, Cstart, Cend, CLEAR, 1);
}

DEFINE_PRIMITIVE("bitvector-field-set", bitvector_field_set, subr3, (SCM bvec, SCM start, SCM end)) {
/* (bitvector-field-set bvec start end) */
  long Cstart, Cend;
  check_bitvector(bvec);
  control_start_end_idx(bvec, start, end, &Cstart, &Cend);
  return field_op(bvec, NULL, Cstart, Cend, SET, 0);
}

DEFINE_PRIMITIVE("bitvector-field-set!", bitvector_field_dset, subr3, (SCM bvec, SCM start, SCM end)) {
/* (bitvector-field-set! bvec start end) */
  long Cstart, Cend;
  check_bitvector(bvec);
  control_start_end_idx(bvec, start, end, &Cstart, &Cend);
  return field_op(bvec, NULL, Cstart, Cend, SET, 1);
}

DEFINE_PRIMITIVE("bitvector-field-replace", bitvector_field_replace, subr4,
                 (SCM dest, SCM source, SCM start, SCM end)) {
/* (bitvector-field-replace bvec start end) */
  check_bitvector(dest);
  check_bitvector(source);
  long Cstart, Cend;
  control_start_end_idx(dest, start, end, &Cstart, &Cend);
  if (Cend - Cstart > BVECTOR_LENGTH(source)) STk_error("source bitvector not long enough: ~S", source);
  return field_op(dest, source, Cstart, Cend, REPLACE, 0);
}

DEFINE_PRIMITIVE("bitvector-field-replace!", bitvector_field_dreplace, subr4,
                 (SCM dest, SCM source, SCM start, SCM end)) {
/* (bitvector-field-replace! bvec start end) */
  check_bitvector(dest);
  check_bitvector(source);
  long Cstart, Cend;
  control_start_end_idx(dest, start, end, &Cstart, &Cend);
  if (Cend - Cstart > BVECTOR_LENGTH(source)) STk_error("source bitvector not long enough: ~S", source);
  return field_op(dest, source, Cstart, Cend, REPLACE, 1);
}

DEFINE_PRIMITIVE("bitvector-field-replace-same", bitvector_field_replace_same, subr4,
                 (SCM dest, SCM source, SCM start, SCM end)) {
/* (bitvector-field-replace dest source start end) */
  check_bitvector(dest);
  check_bitvector(source);
  long Cstart, Cend;
  control_start_end_idx(dest, start, end, &Cstart, &Cend);
  if (Cend > BVECTOR_LENGTH(source)) STk_error("source bitvector not long enough: ~S", source);
  return field_op(dest, source, Cstart, Cend, REPLACE_SAME, 0);
}

DEFINE_PRIMITIVE("bitvector-field-replace-same!", bitvector_field_dreplace_same, subr4,
                 (SCM dest, SCM source, SCM start, SCM end)) {
/* (bitvector-field-replace! dest source start end) */
  check_bitvector(dest);
  check_bitvector(source);
  long Cstart, Cend;
  control_start_end_idx(dest, start, end, &Cstart, &Cend);
  if (Cend > BVECTOR_LENGTH(source)) STk_error("source bitvector not long enough: ~S", source);
  return field_op(dest, source, Cstart, Cend, REPLACE_SAME, 1);
}

DEFINE_PRIMITIVE("bitvector-field-rotate", bitvector_field_rotate, subr4,
                 (SCM bvec, SCM count, SCM start, SCM end)) {
/* (bitvector-field-rotate bvec count start end) */
  long Cstart, Cend;
  check_bitvector(bvec);
  control_start_end_idx(bvec, start, end, &Cstart, &Cend);

  check_integer(count, "count");
  long Ccount = INT_VAL(count);

  SCM bv = STk_make_bvect(BVECTOR_LENGTH(bvec), 0);
  /* Copy the part that will not rotate: */
  bvcopy(bv, 0,
         bvec, 0, Cstart,
         0);
  bvcopy(bv, Cend,
         bvec, Cend, BVECTOR_LENGTH(bv) - Cend,
         0);
  long size = Cend - Cstart;
  long real_count = Ccount % size;
  long ref = real_count < 0 ? Cend - 1 : Cstart;

  if (real_count < 0) {
    real_count = - real_count;
    for(long i=0; i < size; i++) {
      bvector_set(bv, (Cend - 1) - i,
                  bvector_ref(bvec, (Cend - 1) - (i + real_count) % size));
    }
  } else
    for(long i=0; i < size; i++)
      bvector_set(bv, Cstart + i,
                  bvector_ref(bvec, Cstart + (i + real_count) % size));

  return bv;
}

DEFINE_PRIMITIVE("bitvector-field-flip", bitvector_field_flip, subr3, (SCM bvec, SCM start, SCM end)) {
/* (bitvector-field-flip bvec start end) */
  long Cstart, Cend;
  check_bitvector(bvec);
  control_start_end_idx(bvec, start, end, &Cstart, &Cend);
  return field_op(bvec, NULL, Cstart, Cend, FLIP, 0);
}

DEFINE_PRIMITIVE("bitvector-field-flip!", bitvector_field_dflip, subr3, (SCM bvec, SCM start, SCM end)) {
/* (bitvector-field-flip! bvec start end) */
  long Cstart, Cend;
  check_bitvector(bvec);
  control_start_end_idx(bvec, start, end, &Cstart, &Cend);
  return field_op(bvec, NULL, Cstart, Cend, FLIP, 1);
}

static void print_bitvector(SCM vect, SCM port, int mode)
{
  STk_puts(bv2string(vect), port);
}


static struct extended_type_descr xtype_bitvector = {
  .name  = "bitvector",
  .print = print_bitvector,
  .equal = test_equal_bitvector
};

/*===========================================================================*\
 *
 *  Module for SRFI-178
 *
\*===========================================================================*/

MODULE_ENTRY_START("srfi/178")
{
  SCM module =  STk_create_module(STk_intern("srfi/178"));

  /* Create a new type for arrays */
  tc_bitvector = STk_new_user_type(&xtype_bitvector);

  ADD_PRIMITIVE_IN_MODULE(bit2int, module);
  ADD_PRIMITIVE_IN_MODULE(bit2bool, module);

  ADD_PRIMITIVE_IN_MODULE(make_bitvector, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_copy, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_rev_copy, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_append, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_append_sub, module);
  ADD_PRIMITIVE_IN_MODULE(bitvectorp, module);

  ADD_PRIMITIVE_IN_MODULE(bitvector_empty,module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_equal,module);

  ADD_PRIMITIVE_IN_MODULE(bitvector_ref_int, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_ref_bool, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_length, module);

  ADD_PRIMITIVE_IN_MODULE(bitvector_take, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_take_right, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_drop, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_drop_right, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_segment, module);

  ADD_PRIMITIVE_IN_MODULE(bitvector_pref_len, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_suf_len, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_pref, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_suf, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_pad, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_pad_right, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_trim, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_trim_right, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_trim_both, module);

  ADD_PRIMITIVE_IN_MODULE(bitvector_set, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_swap, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_nrev, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_ncopy, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_rev_ncopy, module);

  ADD_PRIMITIVE_IN_MODULE(bitvec2list_int, module);
  ADD_PRIMITIVE_IN_MODULE(bitvec2list_bool, module);
  ADD_PRIMITIVE_IN_MODULE(rev_bitvec2list_int, module);
  ADD_PRIMITIVE_IN_MODULE(rev_bitvec2list_bool, module);
  ADD_PRIMITIVE_IN_MODULE(list2bitvec, module);
  ADD_PRIMITIVE_IN_MODULE(revlist2bitvec, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector2vec_int, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector2vec_bool, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector2vec_revint, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector2vec_revbool, module);
  ADD_PRIMITIVE_IN_MODULE(vector2bitvector, module);
  ADD_PRIMITIVE_IN_MODULE(vector2bitvector_rev, module);

  ADD_PRIMITIVE_IN_MODULE(bv2str, module);
  ADD_PRIMITIVE_IN_MODULE(str2bv, module);

  ADD_PRIMITIVE_IN_MODULE(bitvec2int, module);
  ADD_PRIMITIVE_IN_MODULE(int2bitvector, module);

  ADD_PRIMITIVE_IN_MODULE(bitvector_and, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_dand, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_ior, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_dior, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_xor, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_dxor, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_nxor, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_dnxor, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_and, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_dand, module);

  ADD_PRIMITIVE_IN_MODULE(bitvector_not, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_nnot, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_nand, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_nnand, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_nor, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_nnor, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_andc1, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_nandc1, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_andc2, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_nandc2, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_orc1, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_norc1, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_orc2, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_norc2, module);


  ADD_PRIMITIVE_IN_MODULE(bitvector_log_shift, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_count, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_count_run, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_if, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_first_bit, module);

  ADD_PRIMITIVE_IN_MODULE(bitvector_field_any, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_every, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_clear, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_dclear, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_set, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_dset, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_replace, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_dreplace, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_replace_same, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_dreplace_same, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_rotate, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_flip, module);
  ADD_PRIMITIVE_IN_MODULE(bitvector_field_dflip, module);

  STk_export_all_symbols(module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
