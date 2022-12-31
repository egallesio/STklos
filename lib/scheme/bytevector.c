/*
 * bytevector.c   -- Implementation of R7RS-Large bytevectors
 *
 * Copyright © 2022 Jerônimo Pellegrini <j_p@aleph0.info>
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
 *    Creation date: 07-Jul-2022 22:40
 * Last file update: 31-Dec-2022 09:33 (eg)
 */

#include <gmp.h>
#include <sys/param.h>
#include <stklos.h>
#include <pendian.h>
#include "bytevector-incl.c"

/* We define a type for endianness, just for convenience. */
typedef enum { end_little=0, end_big } endianness_t;
static endianness_t  native_endianness;
static SCM symb_little, symb_big;

/* We'll need to handle bignums at some point. */
struct bignum_obj {
  stk_header header;
  mpz_t val;
};

#define BIGNUM_VAL(p)   (((struct bignum_obj *) (p))->val)

#define LONG_FITS_INTEGER(_l) \
  (((int64_t)INT_MIN_VAL) <= (_l) && \
   (_l) <= ((int64_t) INT_MAX_VAL))

#define ULONG_FITS_INTEGER(_l)  ((_l) <= ((uint64_t) INT_MAX_VAL))

/* Reals... */
static inline SCM double2real(double x)
{
  SCM z;

  NEWCELL(z, real);
  REAL_VAL(z) = x;
  return z;
}

/* Some utilities */

static inline void
check_integer(SCM x) {
  if (!INTP(x)) STk_error("bad integer ~S", x);
}
static inline void
check_string(SCM x) {
  if (!STRINGP(x)) STk_error("bad string ~S", x);
}
static inline void
check_bytevector(SCM v)
{
  if (!BYTEVECTORP(v))
    STk_error("bad bytevector ~s", v);
}


static inline void
bad_arguments(int low, int hi, int given) {
    STk_error ("expected between %d and %d arguments, but got %d", low, hi, given);
}

static inline void
bytevector_init_native_endianness() {
  unsigned int i = 1;
  char *c = (char*) &i;

  if ( (int) *c )
      native_endianness = end_little;
  else
      native_endianness = end_big;
}

static inline endianness_t
get_endianness(SCM endianness) {
  if (!endianness) return end_big;
  if (!SYMBOLP(endianness)) STk_error ("bad symbol ~S", endianness);
  if (endianness == symb_little)
      return end_little;
  else if (endianness == symb_big)
      return end_big;
  else STk_error("bad endianness symbol ~S", endianness);
  return end_little; /* Never reached */
}



/*
<doc EXT native-endianness
 * (native-endianness)
 *
 * Returns the endianness symbol of the underlying machine.
doc>
*/
DEFINE_PRIMITIVE("native-endianness", native_endianness, subr0, ())
{
    if (native_endianness == end_little)
        return symb_little;
    else
        return symb_big;
}


/*
<doc EXT bytevector=?
 * (bytevector=? bytevector1 bytevector2 )
 *
 * Returns true if |bytevector1| and |bytevector2| are equal—that
 * is, if they have the same length and equal bytes at all valid
 * indices. It returns false otherwise.
doc>
*/
DEFINE_PRIMITIVE("bytevector=?", bytevector_equal, subr2,
                 (SCM a, SCM b))
{
  check_bytevector(a);
  check_bytevector(b);
  return MAKE_BOOLEAN(STk_uvector_equal(a,b));
}

/*
<doc EXT bytevector-fill!
 * (bytevector-fill! bytevector fill )
 *
 * The |fill| argument is as in the description of the make-bytevector
 * procedure. The |bytevector-fill!|  procedure stores |fill| in every
 * element of bytevector and returns unspecified values. Analogous to
 * |vector-fill!|.
doc>
*/
DEFINE_PRIMITIVE("bytevector-fill!", bytevector_fill, subr2,
                 (SCM b, SCM x))
{
  check_bytevector(b);
  check_integer(x);

  long vali = INT_VAL(x);

  if (vali >= 256 || vali < -128)
      STk_error("value ~S is out of bounds or incorrect for a bytevector", x);

  if (vali < 0)
      for (long i=0; i<UVECTOR_SIZE(b); i++)
          ((char *) UVECTOR_DATA(b))[i] = (char) vali;
  else
      for (long i=0; i<UVECTOR_SIZE(b); i++)
          ((uint8_t *) UVECTOR_DATA(b))[i] = (uint8_t) vali;;

  return STk_void;
}

/*
<doc EXT bytevector-u8-ref bytevector-s8-ref
 * (bytevector-u8-ref bytevector k)
 * (bytevector-s8-ref bytevector k)
 *
 * The |bytevector-u8-ref| procedure returns the byte at index |k| of
 * |bytevector|, as an octet.
 * The |bytevector-s8-ref| procedure returns the byte at index |k| of
 * |bytevector|, as a signed byte.
 *
 * @lisp
 * (let ((b1 (make-bytevector 16 -127))
 *       (b2 (make-bytevector 16 255)))
 *   (list
 *     (bytevector-s8-ref b1 0)
 *     (bytevector-u8-ref b1 0)
 *     (bytevector-s8-ref b2 0)
 *     (bytevector-u8-ref b2 0)))
 * => (-127 129 -1 255)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("bytevector-s8-ref", bytevector_s8_ref, subr2,
                 (SCM b, SCM i))
{
  check_bytevector(b);
  check_integer(i);

  return MAKE_INT(((char *) UVECTOR_DATA(b))[INT_VAL(i)]);
}

/*
<doc EXT bytevector-u8-set! bytevector-s8-set!
 * (bytevector-u8-set! bytevector k octet)
 * (bytevector-s8-set! bytevector k byte)
 *
 * |K| must be a valid index of bytevector.
 * The |bytevector-u8-set!| procedure stores octet in element
 * |k| of |bytevector|.
 * The |bytevector-s8-set!| procedure stores the two’s-complement
 * representation of |byte| in element |k| of |bytevector|.
 * Both procedures return unspecified values.
 *
 * @lisp
 * (let ((b (make-bytevector 16 -127)))
 *   (bytevector-s8-set! b 0 -126)
 *   (bytevector-u8-set! b 1 246)
 *   (list
 *     (bytevector-s8-ref b 0)
 *     (bytevector-u8-ref b 0)
 *     (bytevector-s8-ref b 1)
 *     (bytevector-u8-ref b 1)))
 *   => (-126 130 -10 246)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("bytevector-s8-set!", bytevector_s8_set, subr3,
                 (SCM b, SCM i, SCM byte))
{
  check_integer(i);
  long  vali = STk_integer_value(byte);
  if (-128 <= vali && vali < +128)
      ((char *) UVECTOR_DATA(b))[INT_VAL(i)] = (char) vali;
  else STk_error("value ~S is out of bounds or incorrect for a bytevector", byte);
  return STk_void;
}

/******
       INT
*******/

SCM
bytevector_uint_ref_aux(SCM b, endianness_t end, size_t idx, size_t size, int sig) {
  if (size <= sizeof(long)) {

    char *ptr; /* points into the bytevector */

    /* Only one of these will be used, one for signed and one for unsigned result.
       We'll make an uint8_t point to the right one, and build the number. */
    long          res  = 0L;
    unsigned long ures = 0L;

    uint8_t *tmp; /* points into the *result long integer!*  */
    if (sig)  tmp = (uint8_t*) &res;
    else      tmp = (uint8_t*) &ures;

    if (end == end_little) ptr = &(((char *) UVECTOR_DATA(b))[idx]);
    else                   ptr = &(((char *) UVECTOR_DATA(b))[idx]) + size - 1;
    for (size_t i=0; i < size; i++) {
      *tmp = *ptr;
      tmp++;
      if (end == end_little) ptr++;
      else                   ptr--;
    }

    /* If the requested 'size' was less than sizeof(long), then
       we filled part of the number. If we also have a negative number,
       we need to finish filling the most significative part with ones. */
    if (sig && (*(tmp-1) >= 128)) {
    memset(tmp, 0xff, sizeof(long)-size);
    }

    /* We have built the long integer result as if the machine was little endian.
       If it is big endian, we swap everything: */
    switch (sizeof(long)) {
    case 4:
    res  = le32toh(res);
    ures = le32toh(ures);
    break;
    case 8:
    res  = le64toh(res);
    ures = le64toh(ures);
    break;
    default: STk_error("STklos does not support long integers with %d bytes", sizeof(long));
    }

    return sig
    ? STk_long2integer(res)
    : STk_ulong2integer(ures);

  } else {
      /*** BIGNUMS ***/

      if (sig &&
      ( (  (end == end_big) &&
           (((char *) UVECTOR_DATA(b))[idx]) < 0)
        ||
        (  (end == end_little) &&
           (((char *) UVECTOR_DATA(b))[idx+size-1]) < 0))) {
      /***
          Negtive case: we do this:
          - copy the bits to tmp
          - invert all bits
          - import into GMP
          - add one
          I have tried to not use the tmp buffer, and inverting the bits
          after they were in a GMP number, but that didn't work for some
          reason.
      ***/

      char *tmp = STk_must_malloc(size);
      memcpy(tmp,&(((char *) UVECTOR_DATA(b))[idx]),size);

      /* Negate the bits */
      for (size_t i=0; i<size; i++)
          tmp[i] = ~tmp[i];

      /* import into a GMP number: */
      SCM z;
      mpz_t num, num2;
      mpz_init(num);
      mpz_init(num2);
      int e = (end == end_little) ? -1 : +1;
      mpz_import (num,                                 /* destination             */
              size,                                /* word count              */
              e,                                   /* order (endianness)      */
              1,                                   /* word size               */
              e,                                   /* endianness within words */
              0,                                   /* nails (skipped bits)    */
              tmp);                                /* from                    */

      /* Add one, to finish the conversion from 2's complement: */
      mpz_add_ui(num2,num,1);
      mpz_neg(num,num2);

      NEWCELL(z, bignum);
      mpz_set(BIGNUM_VAL(z), num);
      return z;

      } else {
      /***
          Positive case: the GMP already has a function for that!
      ***/
      SCM z;
      mpz_t num;
      mpz_init(num);
      int e = (end == end_little) ? -1 : +1;
      mpz_import (num,                                 /* destination             */
              size,                                /* word count              */
              e,                                   /* order (endianness)      */
              1,                                   /* word size               */
              e,                                   /* endianness within words */
              0,                                   /* nails (skipped bits)    */
              &(((char *) UVECTOR_DATA(b))[idx])); /* from                    */

      NEWCELL(z, bignum);
      mpz_set(BIGNUM_VAL(z), num);
      return z;
      }
  }
}

/*
<doc EXT bytevector-uint-ref bytevector-sint-ref bytevector-uint-set! bytevector-sint-set!
 * (bytevector-uint-ref bytevector k endianness size)
 * (bytevector-sint-ref bytevector k endianness size)
 * (bytevector-uint-set! bytevector k n endianness size)
 * (bytevector-sint-set! bytevector k n endianness size)
 *
 * The |bytevector-uint-ref| procedure retrieves the exact
 * integer object corresponding to the unsigned representation
 * of size |size| and specified by |endianness| at indices
 * |k , ... , k + size − 1|.
 *
 * The |bytevector-sint-ref| procedure retrieves the exact
 * integer object corresponding to the two’s-complement
 * representation of size |size| and specified by |endianness|
 * at indices |k , ... , k + size − 1|.
 *
 * For |bytevector-uint-set!|, |n| must be an exact integer
 * object in the interval |{0, ... , 256^size − 1}|.
 *
 * The |bytevector-uint-set!| procedure stores the unsigned
 * representation of size |size| and specified by |endianness|
 * into |bytevector| at indices |k , ... , k + size − 1|.
 *
 * For bytevector-sint-set!, n must be an exact integer
 * object in the interval |{−256^size /2, ... , 256^size /2 − 1}|.
 * |Bytevector-sint-set!| stores the two’s-complement
 * representation of size |size| and specified by |endianness|
 * into |bytevector| at indices |k , ... , k + size − 1|.
 *
 * The |...-set!| procedures return unspecified values.
 *
 * @lisp
 * (define b (make-bytevector 16 -127))
 *
 * (bytevector-uint-set! b 0 (- (expt 2 128) 3)
 *                       (endianness little) 16)
 * (bytevector-uint-ref b 0 (endianness little) 16)
 *  => #xfffffffffffffffffffffffffffffffd
 *
 * (bytevector-sint-ref b 0 (endianness little) 16)
 *  => -3
 *
 * (bytevector->u8-list b)
 *  => (253 255 255 255 255 255 255 255
 *      255 255 255 255 255 255 255 255)
 *
 * (bytevector-uint-set! b 0 (- (expt 2 128) 3)
 *                       (endianness big) 16)
 * (bytevector-uint-ref b 0 (endianness big) 16)
 *  => #xfffffffffffffffffffffffffffffffd
 *
 * (bytevector-sint-ref b 0 (endianness big) 16)
 *  =>-3
 *
 * (bytevector->u8-list b)
 *  => (255 255 255 255 255 255 255 255
 *      255 255 255 255 255 255 255 253))
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("bytevector-uint-ref", bytevector_uint_ref, subr4,
                 (SCM b, SCM i, SCM endianness, SCM s))
{
  check_bytevector(b);
  check_integer(i);
  check_integer(s);

  long idx = INT_VAL(i);
  long size = INT_VAL(s);

  if (idx < 0)
      STk_error("negative index %d", idx);
  if (size < 0)
      STk_error("negative size %d", size);
  if (idx + size > UVECTOR_SIZE(b))
      STk_error("index %d plus size %d out of bounds for bytevector of length %d",
                idx, size, UVECTOR_SIZE(b));

  endianness_t end = get_endianness(endianness);

  return bytevector_uint_ref_aux(b, end, idx, size, 0);
}


DEFINE_PRIMITIVE("bytevector-sint-ref", bytevector_sint_ref, subr4,
                 (SCM b, SCM i, SCM endianness, SCM s))
{
  check_bytevector(b);
  check_integer(i);
  check_integer(s);

  long idx = INT_VAL(i);
  long size = INT_VAL(s);

  if (idx < 0)
      STk_error("negative index %d", idx);
  if (size < 0)
      STk_error("negative size %d", size);
  if (idx + size > UVECTOR_SIZE(b))
      STk_error("index %d plus size %d out of bounds for bytevector of length %d",
                idx, size, UVECTOR_SIZE(b));

  endianness_t end = get_endianness(endianness);

  return bytevector_uint_ref_aux(b, end, idx, size, 1);
}



DEFINE_PRIMITIVE("bytevector-uint-set!", bytevector_uint_set, subr5,
                 (SCM b, SCM i, SCM n, SCM endianness, SCM s))
{
  check_bytevector(b);
  check_integer(i);
  check_integer(s);
  if (!(INTP(n)||BIGNUMP(n))) STk_error("bad integer ~S", n);

  long idx  = INT_VAL(i);
  long size = INT_VAL(s);

  endianness_t end = get_endianness(endianness);

  if (idx < 0)
      STk_error("negative index %d", idx);
  if (size < 0)
      STk_error("negative size %d", size);
  if (idx + size > UVECTOR_SIZE(b))
      STk_error("index %d plus size %d out of bounds for bytevector of length %d",
                idx, size, UVECTOR_SIZE(b));

  if (INTP(n)) {
      long j;
      long val  = INT_VAL(n);

      /* This is the uint version, so no negatives: */
      if (val < 0)
          STk_error("value ~S is not unsigned", n);

      /* The value must fit the 'size' bytes, which means it should
         be less than (256)^size.  The bounds are explicit in the
         spec. */
      if ((unsigned long) val >= ((unsigned long) 1 << (size * 8)))
          STk_error("value %d does not fit in %d bytes", val, size);

      char *ptr;
      if (end == end_little) ptr = &(((char *) UVECTOR_DATA(b))[idx]);
      else                   ptr = &(((char *) UVECTOR_DATA(b))[idx]) + size - 1;
      for (j=0; j < size; j++) {
          *ptr = (char) (val & 0xff);
          val = val >> 8;
          if (end == end_little) ptr++;
          else                   ptr--;
      }
  } else {
      /* n is a BIGNUM */
      int e = (end == end_little) ? -1 : +1;
      size_t count;
      void *ptr = mpz_export (NULL,               /* destination             */
                              &count,             /* bytes written           */
                              e,                  /* endianness              */
                              1,                  /* word size               */
                              e,                  /* endianness within words */
                              0,                  /* nails                   */
                              BIGNUM_VAL(n));     /* from */
      if ((long)count > size)
          STk_error("bignum ~S does not fit in ~S bytes", n, size);

      if (end == end_little) {
          memcpy(&(((char *) UVECTOR_DATA(b))[idx]),
                 ptr,
                 count);
          memset(&(((char *) UVECTOR_DATA(b))[idx+count]),
                 0,
                 size - count);
          /* do not free ptr, GMP is using libgc already */
      } else {
          memcpy(&(((char *) UVECTOR_DATA(b))[idx+size-count]),
                 ptr,
                 count);
          memset(&(((char *) UVECTOR_DATA(b))[idx]),
                 0,
                 size - count);
          /* do not free ptr, GMP is using libgc already */
      }
  }
  return STk_void;
}



/******
       INT16
*******/

DEFINE_PRIMITIVE("bytevector-u16-ref", bytevector_u16_ref, subr3,
                 (SCM b, SCM i, SCM endianness))
{
  check_bytevector(b);
  check_integer(i);

  unsigned long idx = INT_VAL(i);

  uint16_t *z = (uint16_t *) &(((char *) UVECTOR_DATA(b))[idx++]);
  if (endianness == symb_little)
      return MAKE_INT(le16toh (*z));
  else if (endianness == symb_big)
      return MAKE_INT(be16toh (*z));
  else STk_error("bad endianness symbol ~S", endianness);
  return STk_void; /* Never reached */
}



DEFINE_PRIMITIVE("bytevector-s16-ref", bytevector_s16_ref, subr3,
                 (SCM b, SCM i, SCM endianness))
{
  check_bytevector(b);
  check_integer(i);

  unsigned long idx = INT_VAL(i);

  uint16_t *z = (uint16_t *) &(((char *) UVECTOR_DATA(b))[idx++]);
  if (endianness == symb_little)
      return MAKE_INT((int16_t)le16toh (*z));
  else if (endianness == symb_big)
      return MAKE_INT((int16_t)be16toh (*z));
  else STk_error("bad endianness symbol ~S", endianness);
  return STk_void; /* Never reached */
}




DEFINE_PRIMITIVE("bytevector-u16-set!", bytevector_u16_set, subr4,
                 (SCM b, SCM i, SCM val, SCM endianness))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);
  unsigned long vali = STk_integer_value(val);

  if (vali < +65536) {
      uint16_t *z = (uint16_t *) &(((char *) UVECTOR_DATA(b))[idx++]);

      if (endianness == symb_little) {
          *z = htole16(vali);
      } else if (endianness == symb_big) {
          *z = htobe16(vali);
      } else STk_error("bad endianness symbol ~S", endianness);
  } else STk_error("value ~S is out of bounds or incorrect for a bytevector", val);
  return STk_void;
}

DEFINE_PRIMITIVE("bytevector-s16-set!", bytevector_s16_set, subr4,
                 (SCM b, SCM i, SCM byte, SCM endianness))
{
  check_integer(i);
  unsigned long idx = INT_VAL(i);
  long vali = STk_integer_value(byte);

  if (vali < +32768 && vali >= -32768) {
      int16_t *z = (int16_t *) &(((char *) UVECTOR_DATA(b))[idx++]);

      if (endianness == symb_little) {
          *z = (int16_t) htole16((uint16_t) vali);
      } else if (endianness == symb_big) {
          *z = (int16_t) htobe16((uint16_t) vali);
      } else STk_error("bad endianness symbol ~S", endianness);
  } else STk_error("value ~S is out of bounds or incorrect for a bytevector", byte);
  return STk_void;
}




DEFINE_PRIMITIVE("bytevector-u16-native-ref", bytevector_u16_native_ref, subr2,
                 (SCM b, SCM i))
{
  check_bytevector(b);
  check_integer(i);

  unsigned long idx = INT_VAL(i);

  uint16_t *z = (uint16_t *) &(((char *) UVECTOR_DATA(b))[idx++]);
  return MAKE_INT(*z);
}


DEFINE_PRIMITIVE("bytevector-s16-native-ref", bytevector_s16_native_ref, subr2,
                 (SCM b, SCM i))
{
  check_bytevector(b);
  check_integer(i);

  long idx = INT_VAL(i);

  int16_t *z = (int16_t *) &(((char *) UVECTOR_DATA(b))[idx++]);
  return MAKE_INT(*z);
}


DEFINE_PRIMITIVE("bytevector-u16-native-set!", bytevector_u16_native_set, subr3,
                 (SCM b, SCM i, SCM val))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);
  unsigned long vali = STk_integer_value(val);

  if (vali < +65536) {
    uint16_t *z = (uint16_t *) &(((char *) UVECTOR_DATA(b))[idx++]);
    *z = vali;
  } else STk_error("value ~S is out of bounds or incorrect for a bytevector", val);
  return STk_void;
}

DEFINE_PRIMITIVE("bytevector-s16-native-set!", bytevector_s16_native_set, subr3,
                 (SCM b, SCM i, SCM val))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);
  long vali = STk_integer_value(val);

  if (vali > -32768 && vali < +32767) {
    int16_t *z = (int16_t *) &(((char *) UVECTOR_DATA(b))[idx++]);
    *z = (int16_t)  vali;
  } else STk_error("value ~S is out of bounds or incorrect for a bytevector", val);
  return STk_void;
}

/******
       INT32
*******/

DEFINE_PRIMITIVE("bytevector-u32-ref", bytevector_u32_ref, subr3,
                 (SCM b, SCM i, SCM endianness))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);

  uint32_t z = *((uint32_t *) &(((char *) UVECTOR_DATA(b))[idx++]));

  if (endianness == symb_little)
      z = (uint32_t) le32toh (z);
  else if (endianness == symb_big)
      z = (uint32_t) be32toh (z);
  else STk_error("bad endianness symbol ~S", endianness);

  return MAKE_INT((unsigned long)z);
}



DEFINE_PRIMITIVE("bytevector-s32-ref", bytevector_s32_ref, subr3,
                 (SCM b, SCM i, SCM endianness))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);

  uint32_t z = *( (uint32_t *) &(((char *) UVECTOR_DATA(b))[idx++]) );

  if (endianness == symb_little)
      return MAKE_INT((int32_t)le32toh (z));
  else if (endianness == symb_big)
      return MAKE_INT((int32_t)be32toh (z));
  else STk_error("bad endianness symbol ~S", endianness);

  return STk_void; /* Never reached */
}




DEFINE_PRIMITIVE("bytevector-u32-set!", bytevector_u32_set, subr4,
                 (SCM b, SCM i, SCM val, SCM endianness))
{
  check_integer(i);
  unsigned long idx = INT_VAL(i);
  unsigned long vali = STk_integer_value(val);

  if (vali < +4294967296) {
      uint32_t *z = (uint32_t *) &(((char *) UVECTOR_DATA(b))[idx++]);

      if (endianness == symb_little) {
          *z = htole32(vali);
      } else if (endianness == symb_big) {
          *z = htobe32(vali);
      } else STk_error("bad endianness symbol ~S", endianness);
  } else STk_error("value ~S is out of bounds or incorrect for a bytevector", val);
  return STk_void;
}

DEFINE_PRIMITIVE("bytevector-s32-set!", bytevector_s32_set, subr4,
                 (SCM b, SCM i, SCM byte, SCM endianness))
{
  check_integer(i);
  unsigned long idx = INT_VAL(i);
  long vali = STk_integer_value(byte);

  if (vali <= +2147483647 && vali >= -2147483648) {
      int32_t *z = (int32_t *) &(((char *) UVECTOR_DATA(b))[idx++]);

      if (endianness == symb_little) {
          *z = (int32_t) htole32((uint32_t) vali);
      } else if (endianness == symb_big) {
          *z = (int32_t) htobe32((uint32_t) vali);
      } else STk_error("bad endianness symbol ~S", endianness);
  } else STk_error("value ~S is out of bounds or incorrect for a bytevector", byte);
  return STk_void;
}





DEFINE_PRIMITIVE("bytevector-u32-native-ref", bytevector_u32_native_ref, subr2,
                 (SCM b, SCM i))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);

  uint32_t z = *((uint32_t *) &(((char *) UVECTOR_DATA(b))[idx]));

  return MAKE_INT((unsigned long) z);
}

DEFINE_PRIMITIVE("bytevector-s32-native-ref", bytevector_s32_native_ref, subr2,
                 (SCM b, SCM i))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);

  int32_t z = *((int32_t *) &(((char *) UVECTOR_DATA(b))[idx]) );

  return MAKE_INT((int32_t) z);
}

DEFINE_PRIMITIVE("bytevector-u32-native-set!", bytevector_u32_native_set, subr3,
                 (SCM b, SCM i, SCM val))
{
  check_integer(i);
  unsigned long idx = INT_VAL(i);
  unsigned long vali = STk_integer_value(val);

  if (vali < +4294967296) {
      uint32_t *z = (uint32_t *) &(((char *) UVECTOR_DATA(b))[idx++]);
      *z = vali;
  } else STk_error("value ~S is out of bounds or incorrect for a bytevector", val);

  return STk_void;
}


DEFINE_PRIMITIVE("bytevector-s32-native-set!", bytevector_s32_native_set, subr3,
                 (SCM b, SCM i, SCM byte))
{
  check_integer(i);
  unsigned long idx = INT_VAL(i);
  long vali = STk_integer_value(byte);

  if (vali <= +2147483647 && vali >= -2147483648) {
      int32_t *z = (int32_t *) &(((char *) UVECTOR_DATA(b))[idx++]);
      *z = (int32_t) vali;
  } else STk_error("value ~S is out of bounds or incorrect for a bytevector", byte);

  return STk_void;
}


/******
       INT64
*******/

DEFINE_PRIMITIVE("bytevector-u64-ref", bytevector_u64_ref, subr3,
                 (SCM b, SCM i, SCM endianness))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);

  uint64_t z = * ((uint64_t *) &(((char *) UVECTOR_DATA(b))[idx++]) );
  if (endianness == symb_little)
      z = le64toh (z);
  else if (endianness == symb_big)
      z = be64toh (z);
  else STk_error("bad endianness symbol ~S", endianness);

  return (ULONG_FITS_INTEGER(z))
      ? MAKE_INT(z)
      : STk_ulong2integer(z);
}



DEFINE_PRIMITIVE("bytevector-s64-ref", bytevector_s64_ref, subr3,
                 (SCM b, SCM i, SCM endianness))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);

  uint64_t z = * ((uint64_t *) &(((char *) UVECTOR_DATA(b))[idx++]) );
  if (endianness == symb_little)
      z =  le64toh (z);
  else if (endianness == symb_big)
      z =  be64toh (z);
  else STk_error("bad endianness symbol ~S", endianness);

  int64_t *w = ((int64_t *) &z);
  return (LONG_FITS_INTEGER(*w))
    ? MAKE_INT((long) *w)
    : STk_long2integer((long) *w);
}




DEFINE_PRIMITIVE("bytevector-u64-set!", bytevector_u64_set, subr4,
                 (SCM b, SCM i, SCM val, SCM endianness))
{
  check_integer(i);
  unsigned long idx = INT_VAL(i);
  unsigned long vali = STk_integer_value(val);

  if (vali < +65536) {
      uint64_t *z = (uint64_t *) &(((char *) UVECTOR_DATA(b))[idx++]);

      if (endianness == symb_little) {
          *z = htole64(vali);
      } else if (endianness == symb_big) {
          *z = htobe64(vali);
      } else STk_error("bad endianness symbol ~S", endianness);
  } else STk_error("value ~S is out of bounds or incorrect for a bytevector", val);
  return STk_void;
}

DEFINE_PRIMITIVE("bytevector-s64-set!", bytevector_s64_set, subr4,
                 (SCM b, SCM i, SCM byte, SCM endianness))
{
  check_integer(i);
  long idx = INT_VAL(i);
  long vali = STk_integer_value(byte);

  if (vali < +64768 && vali >= -64768) {
      int64_t *z = (int64_t *) &(((char *) UVECTOR_DATA(b))[idx++]);

      if (endianness == symb_little) {
          *z = (int64_t) htole64((uint64_t) vali);
      } else if (endianness == symb_big) {
          *z = (int64_t) htobe64((uint64_t) vali);
      } else STk_error("bad endianness symbol ~S", endianness);
  } else STk_error("value ~S is out of bounds or incorrect for a bytevector", byte);
  return STk_void;
}


DEFINE_PRIMITIVE("bytevector-u64-native-ref", bytevector_u64_native_ref, subr2,
                 (SCM b, SCM i))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);

  uint64_t z = * ((uint64_t *) &(((char *) UVECTOR_DATA(b))[idx++]) );

  return (ULONG_FITS_INTEGER(z))
      ? MAKE_INT(z)
      : STk_ulong2integer(z);
}

DEFINE_PRIMITIVE("bytevector-s64-native-ref", bytevector_s64_native_ref, subr2,
                 (SCM b, SCM i))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);

  int64_t z = * ((int64_t *) &(((char *) UVECTOR_DATA(b))[idx++]) );

  return (LONG_FITS_INTEGER(z))
      ? MAKE_INT((long) z)
      : STk_long2integer((long) z);
}


DEFINE_PRIMITIVE("bytevector-u64-native-set!", bytevector_u64_native_set, subr3,
                 (SCM b, SCM i, SCM val))
{
  check_integer(i);
  unsigned long idx = INT_VAL(i);
  unsigned long vali = STk_integer_value(val);

  uint64_t *z = (uint64_t *) &(((char *) UVECTOR_DATA(b))[idx++]);
  *z = vali;

  return STk_void;
}

DEFINE_PRIMITIVE("bytevector-s64-native-set!", bytevector_s64_native_set, subr3,
                 (SCM b, SCM i, SCM val))
{
  check_integer(i);
  unsigned long idx = INT_VAL(i);
  unsigned long vali = STk_integer_value(val);

  int64_t *z = (int64_t *) &(((char *) UVECTOR_DATA(b))[idx++]);
  *z = (int64_t) vali;

  return STk_void;
}


/******
       IEEE-754
*******/

uint32_t
ieee_4_ref(SCM b, unsigned int idx, endianness_t end) {
  uint32_t *z = (uint32_t *) &(((char *) UVECTOR_DATA(b))[idx]);
  uint32_t w = 0;
  if (end == end_little)
    w = le32toh (*z);
  else if (end == end_big)
    w = be32toh (*z);
  else
    STk_error("wrong endianness!");
  return w;
}

uint64_t
ieee_8_ref(SCM b, unsigned int idx, endianness_t end) {
  uint64_t *z = (uint64_t *) &(((char *) UVECTOR_DATA(b))[idx]);
  uint64_t w = 0;
  if (end == end_little)
      w = le64toh (*z);
  else if (end == end_big)
      w = be64toh (*z);
  else
    STk_error("wrong endianness!");
 return w;
}

void
ieee_4_set(uint32_t *place, endianness_t end,  void *val) {
  if (end == end_little)
      *place = htole32( *((uint32_t*) val));
  else if (end == end_big)
      *place = htobe32( *((uint32_t*) val));
}

void
ieee_8_set(uint64_t *place, endianness_t end,  void *val) {
  if (end == end_little)
      *place = htole64( *((uint64_t*) val));
  else if (end == end_big)
      *place = htobe64( *((uint64_t*) val));
}


DEFINE_PRIMITIVE("bytevector-ieee-single-ref", bytevector_ieee_single_ref, subr3,
                 (SCM b, SCM i, SCM endianness))
{
  check_bytevector(b);
  endianness_t end = end_little;
  if (endianness == symb_little)
      end = end_little;
  else if (endianness == symb_big)
      end = end_big;
  else STk_error("bad endianness symbol ~S", endianness);

  check_integer(i);
  unsigned long idx = INT_VAL(i);

  switch (sizeof(float)) {
  case 4: {
    union { uint32_t w4; float f; } tmp;
    tmp.w4 = ieee_4_ref(b, idx, end);
    return double2real(tmp.f);
  }
  case 8: {
    union { uint64_t w8; float f; } tmp;
    tmp.w8 = ieee_8_ref(b, idx, end);
    return double2real(tmp.f);
  }
  default:
      STk_error("floats of %d bytes are not supported in STklos", sizeof(float));
  }
  return STk_false; /* Never reached */
}

DEFINE_PRIMITIVE("bytevector-ieee-single-native-ref", bytevector_ieee_single_native_ref, subr2,
                 (SCM b, SCM i))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);

  switch (sizeof(float)) {
  case 4: {
    union { uint32_t w4; float f; } tmp;
    tmp.w4 = ieee_4_ref(b, idx, native_endianness);
    return double2real(tmp.f);
  }
  case 8: {
    union { uint64_t w8; float f; } tmp;
    tmp.w8 = ieee_8_ref(b, idx, native_endianness);
    return double2real(tmp.f);

  }
  default:
      STk_error("floats of %d bytes are not supported in STklos", sizeof(float));
  }
  return STk_false; /* Never reached */
}


DEFINE_PRIMITIVE("bytevector-ieee-double-ref", bytevector_ieee_double_ref, subr3,
                 (SCM b, SCM i, SCM endianness))
{
  check_bytevector(b);
  endianness_t end = end_little;
  if (endianness == symb_little)
      end = end_little;
  else if (endianness == symb_big)
      end = end_big;
  else STk_error("bad endianness symbol ~S", endianness);

  check_integer(i);
  unsigned long idx = INT_VAL(i);

  switch (sizeof(double)) {
  case 8: {
    union { uint64_t w8; double d;} tmp;
    tmp.w8 = ieee_8_ref(b, idx, end);
    return double2real(tmp.d);
  }
  default:
      STk_error("doubles of %d bytes are not supported in STklos", sizeof(double));
  }
  return STk_false; /* Never reached */
}

DEFINE_PRIMITIVE("bytevector-ieee-double-native-ref", bytevector_ieee_double_native_ref, subr2,
                 (SCM b, SCM i))
{
  check_bytevector(b);
  check_integer(i);
  unsigned long idx = INT_VAL(i);

  switch (sizeof(double)) {
  case 8: {
    union { uint64_t w8; double d;} tmp;
    tmp.w8 = ieee_8_ref(b, idx, native_endianness);
    return double2real(tmp.d);
  }
  default:
      STk_error("doubles of %d bytes are not supported in STklos", sizeof(double));
  }
  return STk_false; /* Never reached */
}



DEFINE_PRIMITIVE("bytevector-ieee-single-set!", bytevector_ieee_single_set, subr4,
                 (SCM b, SCM i, SCM val, SCM endianness))
{
  check_integer(i);

  endianness_t end = end_little;
  if (endianness == symb_little)
      end = end_little;
  else if (endianness == symb_big)
      end = end_big;
  else STk_error("bad endianness symbol ~S", endianness);

  unsigned long idx = INT_VAL(i);
  float valf = REAL_VAL(val);

  switch (sizeof(float)) {
  case 4: {
    uint32_t *z4 = (uint32_t *) &(((char *) UVECTOR_DATA(b))[idx]);
    ieee_4_set(z4, end, &valf);
    break;
  }
  case 8: {
    uint64_t *z8 = (uint64_t *) &(((char *) UVECTOR_DATA(b))[idx]);
    ieee_8_set(z8, end, &valf);
    break;
  }
  default:
      STk_error("floats of %d bytes are not supported in STklos", sizeof(float));
  }
  return STk_void;
}

DEFINE_PRIMITIVE("bytevector-ieee-single-native-set!", bytevector_ieee_single_native_set, subr3,
                 (SCM b, SCM i, SCM val))
{
  check_integer(i);

  unsigned long idx = INT_VAL(i);
  float valf = REAL_VAL(val);

  switch (sizeof(float)) {
  case 4: {
    uint32_t *z4 = (uint32_t *) &(((char *) UVECTOR_DATA(b))[idx]);
    ieee_4_set(z4, native_endianness, &valf);
    break;
  }
  case 8: {
    uint64_t *z8 = (uint64_t *) &(((char *) UVECTOR_DATA(b))[idx]);
    ieee_8_set(z8, native_endianness, &valf);
    break;
  }
  default:
      STk_error("floats of %d bytes are not supported in STklos", sizeof(float));
  }
  return STk_void;
}

DEFINE_PRIMITIVE("bytevector-ieee-double-set!", bytevector_ieee_double_set, subr4,
                 (SCM b, SCM i, SCM val, SCM endianness))
{
  check_integer(i);

  endianness_t end = 0;
  if (endianness == symb_little)
      end = end_little;
  else if (endianness == symb_big)
      end = end_big;
  else STk_error("bad endianness symbol ~S", endianness);

  unsigned long   idx  = INT_VAL(i);
  double valf = REAL_VAL(val);

  switch (sizeof(double)) {
  case 8: {
    uint64_t *z = (uint64_t *) &(((char *) UVECTOR_DATA(b))[idx]);
    ieee_8_set(z, end, &valf);
    break;
  }
  default:
      STk_error("doubles of %d bytes are not supported in STklos", sizeof(double));
  }
  return STk_void;
}

DEFINE_PRIMITIVE("bytevector-ieee-double-native-set!", bytevector_ieee_double_native_set, subr3,
                 (SCM b, SCM i, SCM val))
{
  check_integer(i);

  unsigned long   idx  = INT_VAL(i);
  double valf = REAL_VAL(val);

  switch (sizeof(double)) {
  case 8: {
    uint64_t *z = (uint64_t *) &(((char *) UVECTOR_DATA(b))[idx]);
    ieee_8_set(z, native_endianness, &valf);
    break;
  }
  default:
      STk_error("doubles of %d bytes are not supported in STklos", sizeof(double));
  }
  return STk_void;
}

/******
       STRINGS
*******/

endianness_t
get_bom_16(SCM vec, endianness_t default_end, int *bom_bytes) {
    *bom_bytes = 0;

    if (UVECTOR_SIZE(vec) < 2) return default_end;

    uint8_t a = ((uint8_t *)UVECTOR_DATA(vec))[0];
    uint8_t b = ((uint8_t *)UVECTOR_DATA(vec))[1];

    if (a == 0xff && b == 0xfe) {
        *bom_bytes = 2;
        return end_little;
    }
    if (a == 0xfe && b == 0xff) {
        *bom_bytes = 2;
        return end_big;
    }

    return default_end;
}



DEFINE_PRIMITIVE("utf16->string", utf162string, subr23, (SCM vec, SCM end, SCM bbom))
{
    if (bbom != NULL && !BOOLEANP(bbom)) STk_error ("bad boolean ~S", bbom);

    int bom = (bbom == STk_true)? 0 : 1;
    int bom_bytes = 0;

    check_bytevector(vec);
    unsigned long len = UVECTOR_SIZE(vec);


    endianness_t endianness = get_endianness(end);
    if (bom) endianness = get_bom_16(vec, endianness, &bom_bytes);

    uint16_t w1, w2;

    /* compute number of characters */
    unsigned long size = 0;
    for (unsigned long i=bom_bytes; i<len;) {
        if (endianness == end_little) {
            w1  = ((uint8_t *) UVECTOR_DATA(vec))[i++];
            w1 |= ((uint8_t *) UVECTOR_DATA(vec))[i++] << 8;
        } else {
            w1  = ((uint8_t *) UVECTOR_DATA(vec))[i++] << 8;
            w1 |= ((uint8_t *) UVECTOR_DATA(vec))[i++];
        }

        if (w1 >= 0xd800 &&
            w1 <= 0xdfff) {
          if (i == len-1)
            STk_error("bad UTF16 encoding (bytevector ~S ends in half byte pair)", vec);
          size++;
          i+=2; /* w2 */
        } else
          size++;
    }
    SCM s = STk_makestring(size,NULL);

    uint32_t u;
    unsigned long idx_v = bom_bytes;
    for(unsigned long idx_s=0; idx_s < size; idx_s++) {
        if (endianness == end_little) {
            w1  = ((uint8_t *) UVECTOR_DATA(vec))[idx_v++];
            w1 |= ((uint8_t *) UVECTOR_DATA(vec))[idx_v++] << 8;
        } else {
            w1  = ((uint8_t *) UVECTOR_DATA(vec))[idx_v++] << 8;
            w1 |= ((uint8_t *) UVECTOR_DATA(vec))[idx_v++];
        }

        if (w1 < 0xd800 || w1 > 0xdfff)
          STk_string_set(s, MAKE_INT(idx_s), MAKE_CHARACTER(w1));
        else {
          if (endianness == end_little) {
            w2  = ((uint8_t *) UVECTOR_DATA(vec))[idx_v++];
            w2 |= (uint16_t) (((uint8_t *) UVECTOR_DATA(vec))[idx_v++]) << 8;
          } else {
            w2  = (uint16_t) (((uint8_t *) UVECTOR_DATA(vec))[idx_v++]) << 8;
            w2 |= ((uint8_t *) UVECTOR_DATA(vec))[idx_v++];
          }

          u = (((uint32_t) (w2 & 0x3ff)) |
               ((uint32_t) ((w1 & 0x3ff)<< 10)));

          u += 0x10000;
          STk_string_set(s, MAKE_INT(idx_s), MAKE_CHARACTER(u));
        }
    }
    return s;
}


DEFINE_PRIMITIVE("string->utf16", string2utf16, vsubr, (int argc, SCM *argv))
{
    if (argc < 1 || argc > 3) bad_arguments(2, 3, argc);
    SCM str = *argv--;
    SCM end  = (argc > 1) ? *argv-- : NULL;
    SCM bbom = (argc > 2) ? *argv-- : NULL;

    check_string(str);
    if (bbom != NULL && !BOOLEANP(bbom)) STk_error ("bad boolean ~S", bbom);

    int bom = (bbom == STk_true)? 1 : 0;

    if (STRING_LENGTH(str) == 0) return STk_make_C_bytevector(0);

    endianness_t endianness = get_endianness(end);

    unsigned long len = STRING_LENGTH(str);
    SCM ch;
    long val;
    unsigned long dst_len = 0;

    for (unsigned long i=0; i<len; i++) {
        ch = STk_string_ref(str,MAKE_INT(i));
        val = CHARACTER_VAL(ch);
        dst_len += 2;
    if (val > 0x10FFFF) STk_error("character with value ~S outside of Unicode range",
                                      MAKE_INT(val));
        if (val >= 0x10000) dst_len += 2;
    }

    SCM dest = STk_make_C_bytevector(dst_len + 2 * bom);

    unsigned long dest_idx = 0;

    if (bom) {
    if (endianness == end_little) {
        ((uint8_t *) UVECTOR_DATA(dest))[dest_idx++] = 0xff;
        ((uint8_t *) UVECTOR_DATA(dest))[dest_idx++] = 0xfe;
    } else {
        ((uint8_t *) UVECTOR_DATA(dest))[dest_idx++] = 0xfe;
        ((uint8_t *) UVECTOR_DATA(dest))[dest_idx++] = 0xff;
    }
    }

    for (unsigned long i=0; i<len; i++) {

        SCM ch = STk_string_ref(str,MAKE_INT(i));
        unsigned long val = CHARACTER_VAL(ch);

        if (val < 0x10000) {
            uint16_t e = (endianness == end_big)
                ? htobe16 ((uint16_t) val)
                : htole16 ((uint16_t) val);

            uint8_t* c = (uint8_t* ) &e;
            ((uint8_t *) UVECTOR_DATA(dest))[dest_idx++] = *c++;
            ((uint8_t *) UVECTOR_DATA(dest))[dest_idx++] = *c;

        } else {

            val -= 0x10000;

            uint16_t w1;
            uint16_t w2;

            w1 = 0xd800 | (uint16_t) ((val >> 10) & 0x3ff);   /*  select 10 higher-order bits */
            w2 = 0xdc00 | (uint16_t) (val & 0x3ff);           /*  0b1111111111,
                                                                  select 10 lower-order bits  */
            if (endianness == end_little) {
                w1 = htole16(w1);
                w2 = htole16(w2);
            } else {
                /* big */
                w1 = htobe16(w1);
                w2 = htobe16(w2);
            }

            uint8_t *c = (uint8_t *) &w1;
            ((uint8_t *) UVECTOR_DATA(dest))[dest_idx++] = *c++;
            ((uint8_t *) UVECTOR_DATA(dest))[dest_idx++] = *c++;
            c = (uint8_t *) &w2;
            ((uint8_t *) UVECTOR_DATA(dest))[dest_idx++] = *c++;
            ((uint8_t *) UVECTOR_DATA(dest))[dest_idx++] = *c;
        }
    }
    return dest;
}





/* UTF32 */


/**/
endianness_t
get_bom_32(SCM vec, endianness_t default_end, int *bom_bytes) {
  *bom_bytes = 0;

  if (UVECTOR_SIZE(vec) < 4) return default_end;

  uint8_t a = ((uint8_t *)UVECTOR_DATA(vec))[0];
  uint8_t b = ((uint8_t *)UVECTOR_DATA(vec))[1];
  uint8_t c = ((uint8_t *)UVECTOR_DATA(vec))[2];
  uint8_t d = ((uint8_t *)UVECTOR_DATA(vec))[3];

  if (a == 0xff && b == 0xfe &&
      c == 0x00 && d == 0x00) {
      *bom_bytes = 4;
      return end_little;
  }
  if (a == 0x00 && b == 0x00 &&
      c == 0xfe && d == 0xff) {
      *bom_bytes = 4;
      return end_big;
  }

  return default_end;
}

DEFINE_PRIMITIVE("utf32->string", utf322string, subr23, (SCM vec, SCM end, SCM bbom))
{
    if (bbom != NULL && !BOOLEANP(bbom)) STk_error ("bad boolean ~S", bbom);

    int bom = (bbom == STk_true)? 0 : 1;
    int bom_bytes = 0;

    check_bytevector(vec);
    unsigned long len = UVECTOR_SIZE(vec);

    endianness_t endianness = get_endianness(end);
    if (bom) endianness = get_bom_32(vec, endianness, &bom_bytes);

    if (len%4) STk_error("bad bytevector length %d for UTF32 string", len);

    SCM str = STk_makestring((len - bom_bytes)/4, NULL);

    uint32_t *p;
    uint32_t z;
    unsigned long j = 0;
    for (unsigned long i = bom_bytes; i < len; i += 4, j++) {
        p = (uint32_t *)  &(((uint8_t *) UVECTOR_DATA(vec))[i]);
        z = *p;
        if (endianness == end_big)
            z = be32toh(z);
        else
            z = le32toh(z);
        /* Now z is the exact integer reprsenting the character we want */
        STk_string_set(str, MAKE_INT(j), MAKE_CHARACTER(z));
    }
    return str;
}

DEFINE_PRIMITIVE("string->utf32", string2utf32, vsubr, (int argc, SCM *argv))
{
    if (argc < 1 || argc > 3) bad_arguments(2, 3, argc);
    SCM str = *argv--;
    SCM end  = (argc > 1) ? *argv-- : NULL;
    SCM bbom = (argc > 2) ? *argv-- : NULL;

    check_string(str);

    if (bbom != NULL && !BOOLEANP(bbom)) STk_error ("bad boolean ~S", bbom);
    int bom = (bbom == STk_true)? 1 : 0;

    endianness_t endianness = get_endianness(end);

    unsigned long len = STRING_LENGTH(str);
    SCM dest = STk_make_C_bytevector((len+bom)*4); /* 4 bytes = one uint32_t */

    uint32_t z;

    unsigned long j=0; /* index for the destination uvector */

    if (bom) {
    if (endianness == end_little) {
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = 0xff;
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = 0xfe;
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = 0x00;
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = 0x00;
    } else {
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = 0x00;
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = 0x00;
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = 0xfe;
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = 0xff;
    }
    }

    for (unsigned long i=0; i<len; i++) {
        z = CHARACTER_VAL(STk_string_ref(str,MAKE_INT(i)));
        if (endianness == end_big)
            z = htobe32(z);
        else
            z = htole32(z);
        uint8_t *c = (uint8_t *) &z;
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = *c++;
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = *c++;
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = *c++;
        ((uint8_t *) UVECTOR_DATA(dest))[j++] = *c++;
    }
    return dest;
}



MODULE_ENTRY_START("scheme/bytevector")
{
    SCM module =  STk_create_module(STk_intern("scheme/bytevector"));

    bytevector_init_native_endianness();
    symb_little = STk_intern("little");
    symb_big    = STk_intern("big");

    ADD_PRIMITIVE_IN_MODULE(native_endianness,   module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_equal,    module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_fill,     module);

    ADD_PRIMITIVE_IN_MODULE(bytevector_uint_ref, module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_sint_ref, module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_uint_set, module);

    ADD_PRIMITIVE_IN_MODULE(bytevector_s8_ref,   module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s8_set,   module);

    ADD_PRIMITIVE_IN_MODULE(bytevector_u16_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s16_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_u16_set,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s16_set,  module);

    ADD_PRIMITIVE_IN_MODULE(bytevector_u16_native_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s16_native_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_u16_native_set,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s16_native_set,  module);

    ADD_PRIMITIVE_IN_MODULE(bytevector_u32_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s32_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_u32_set,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s32_set,  module);

    ADD_PRIMITIVE_IN_MODULE(bytevector_u32_native_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s32_native_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_u32_native_set,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s32_native_set,  module);

    ADD_PRIMITIVE_IN_MODULE(bytevector_u64_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s64_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_u64_set,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s64_set,  module);

    ADD_PRIMITIVE_IN_MODULE(bytevector_u64_native_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s64_native_ref,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_u64_native_set,  module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_s64_native_set,  module);

    ADD_PRIMITIVE_IN_MODULE(bytevector_ieee_single_ref, module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_ieee_single_set, module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_ieee_double_ref, module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_ieee_double_set, module);

    ADD_PRIMITIVE_IN_MODULE(bytevector_ieee_single_native_ref, module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_ieee_single_native_set, module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_ieee_double_native_ref, module);
    ADD_PRIMITIVE_IN_MODULE(bytevector_ieee_double_native_set, module);

    ADD_PRIMITIVE_IN_MODULE(string2utf16, module);
    ADD_PRIMITIVE_IN_MODULE(utf162string, module);

    ADD_PRIMITIVE_IN_MODULE(string2utf32, module);
    ADD_PRIMITIVE_IN_MODULE(utf322string, module);

   /* Export all the symbols we have just defined */
    STk_export_all_symbols(module);


    /* Execute Scheme code */
    STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
