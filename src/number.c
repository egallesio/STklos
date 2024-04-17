/*                                                      -*- coding: utf-8 -*-
 *
 * n u m b e r . c      -- Numbers management
 *
 * Copyright © 1993-2024 Erick Gallesio <eg@stklos.net>
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
 *
 *           Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: 12-May-1993 10:34
 */


#include <math.h>
#include <float.h>
#include <ctype.h>
#include <locale.h>
#include "stklos.h"

#undef sinc
#if defined(__linux__) && defined(__alpha__)
#  include <signal.h>
#endif


static int use_srfi_169 = 1; /* do we allow the use of underscores in numbers? */

/* Real precision */
static int real_precision = REAL_FORMAT_SIZE;
static unsigned int log10_maxint;

#define FINITE_REALP(n) isfinite(REAL_VAL(n))

/* Complex i: will be used as a constant when computing some functions. */
static SCM complex_i;

/* rational_epsilon: an exact (rational) epsilon which should
 * be small enough to work as an epsilon for doubles. */
static SCM rational_epsilon;

/* Forward declarations */
static void integer_division(SCM x, SCM y, SCM *quotient, SCM* remainder);


/* Declaration of bignums. This is done here instead of stklos.h to avoid
 * to expose the file "gmp.h" in "stklos.h" which is the interface users
 * see to access all the system (note that we can also use our version which
 * can be different of the one which is sytem installed, and resolve conflits
 * could be hard).
 */
#include <gmp.h>

struct bignum_obj {
  stk_header header;
  mpz_t val;
};

#define BIGNUM_VAL(p)   (((struct bignum_obj *) (p))->val)


/*==============================================================================*/

//#define MY_PI           3.1415926535897932384626433832795029L  /* pi */
#define MY_PI      3.1415926535897932384626433832795028841971693993751058209749445923078164062862090L

#define BIGNUM_FITS_INTEGER(_bn) (mpz_cmp_si((_bn), INT_MIN_VAL) >= 0 &&        \
                                  mpz_cmp_si((_bn), INT_MAX_VAL) <= 0)
#define LONG_FITS_INTEGER(_l)    (INT_MIN_VAL <= (_l) && (_l) <= INT_MAX_VAL)
#define TYPEOF(n)                (INTP(n)? tc_integer: STYPE(n))

#define IS_INFP(x)              (REALP(x) && isinf(REAL_VAL(x)))

#define MINUS_INF "-inf.0"
#define PLUS_INF  "+inf.0"
#define MINUS_NaN "-nan.0"
#define PLUS_NaN  "+nan.0"


/* Special IEEE values */
static double plus_inf, minus_inf;
double STk_NaN;

/**** forward declarations ****/
static type_cell convert(SCM *px, SCM *py);

static int zerop(SCM n);
static int negativep(SCM n);
static int positivep(SCM n);
static int isexactp(SCM z);
static SCM gcd2(SCM n1, SCM n2);

EXTERN_PRIMITIVE("make-rectangular", make_rectangular, subr2, (SCM r, SCM i));
EXTERN_PRIMITIVE("real-part", real_part, subr1, (SCM z));
EXTERN_PRIMITIVE("angle", angle, subr1, (SCM z));
EXTERN_PRIMITIVE("sqrt", sqrt, subr1, (SCM z));
EXTERN_PRIMITIVE("exact->inexact", ex2inex, subr1, (SCM z));
EXTERN_PRIMITIVE("inexact->exact", inex2ex, subr1, (SCM z));


#define add2 STk_add2
#define mul2 STk_mul2
#define div2 STk_div2
#define sub2 STk_sub2
#define absolute STk_abs
#define exact2inexact STk_ex2inex
#define inexact2exact STk_inex2ex


static SCM int_quotient(SCM x, SCM y);
static SCM my_cos(SCM z);
static SCM my_sin(SCM z);
static SCM STk_complexp(SCM x);


/******************************************************************************
 *
 * Utilities
 *
 ******************************************************************************/
static void error_bad_number(SCM n)
{
  STk_error("~S is a bad number", n);
}

static void error_not_a_real_number(SCM n)
{
  if (COMPLEXP(n))
    STk_error("~S is not a real number", n);
  else
    error_bad_number(n);
}

static void error_out_of_range(SCM x)
{
  STk_error("argument out of range ~s", x);
}

static void error_at_least_1(void)
{
  STk_error("expects at least one argument");
}

static void error_cannot_operate(char *operation, SCM o1, SCM o2)
{
  STk_error("cannot perform %s on ~S and ~S", operation, o1, o2);
}

static void error_divide_by_0(SCM n)
{
  STk_error("cannot divide ~S by 0", n);
}

static void error_incorrect_radix(SCM r)
{
  STk_error("base must be 2, 8, 10 or 16. It was ~S", r);
}

static void error_not_an_integer(SCM n)
{
  STk_error("exact or inexact integer required, got ~s", n);
}

static void error_not_an_exact_integer(SCM n)
{
  STk_error("exact integer required, got ~s", n);
}

union binary64 {
  uint64_t u;
  double   d;
};

static const uint64_t sign_mask    = (uint64_t) 1 << 63;
static const uint64_t quiet_mask   = (uint64_t) 1 << 51;
static const uint64_t payload_mask = ((uint64_t) 1 << 50) - 1;

static double make_nan(int neg, int quiet, unsigned long pay)
{
  union binary64 t;

  /* Beware:
   *   quiet NAN is       0x7ff8000000000000
   *   signaling NaN is   0x7ffZxxxxxxxxxxxx   where Z is 0xxx (bit 51 is 0)
   * BUT
   *   +inf.0          is 0x7ff0000000000000
   * Consequently, clearing bit 51 is not sufficient (if the payload is 0, a
   * signaling dille be seen as a positive infinity).
   * So, to make a signaling NaN, we clear the bit 51 and set the bit 50
   * ==> the payload can use only 50 bits
   */
  t.u = (quiet)? 0x7ff8000000000000U : 0x7ff4000000000000U;
  if (neg)   t.u |= sign_mask;
  t.u |= pay;

  return t.d;
}

int STk_isnan(SCM z) {
  switch (TYPEOF(z)) {
  case tc_complex:  return (REALP(COMPLEX_REAL(z)) && isnan(REAL_VAL(COMPLEX_REAL(z)))) ||
                           (REALP(COMPLEX_IMAG(z)) && isnan(REAL_VAL(COMPLEX_IMAG(z))));
  case tc_real:     return isnan(REAL_VAL(z));
  case tc_rational:
  case tc_bignum:
  case tc_integer:  return 0;
  default:          error_bad_number(z); return 0;
  }
}


double STk_dbl_true_min(void) /* return (or compute) DBL_TRUE_MIN */
{
  /*
   This function is used here in order to calculate a rational epsilon (for
   square roots) and also in the (scheme flonum) library.

   Some platforms may not have DBL_TRUE_MIN defined (at this time, OpenBSD
   doesn't), so we calculate DBL_TRUE_MIN. dbl_truemin is also used in
   lib/scheme/flonum.c

   DBL_MIN is the least NORMAL positive number represented in IEEE format.
   DBL_TRUE_MIN is the least SUBNORMAL positive number: the one that, when
   divided by 2, is equal to zero.
   Some platforms may not have DBL_TRUE_MIN defined (at this time, OpenBSD
   doesn't), so we calculate DBL_TRUE_MIN.

   Remark I: Using IEEE 754, the representations of DBL_MIN and DBL_TRUE_MIN
   are as follows.

   DBL_MIN:
   [ 0 | 00000000001 | 0000000000000000000000000000000000000000000000000000 ]
   Signal = 0, Exponent = 1, Mantissa = 0.

   DBL_MIN / 2.0:
   [ 0 | 00000000000 | 1000000000000000000000000000000000000000000000000000 ]
   Signal = 0, Exponent = 0, Mantissa = 2^52.

   DBL_MIN / 4.0:
   [ 0 | 00000000000 | 0100000000000000000000000000000000000000000000000000 ]
   Signal = 0, Exponent = 0, Mantissa = 2^51.

   DBL_TRUE_MIN:
   [ 0 | 00000000000 | 0000000000000000000000000000000000000000000000000001 ]
   Signal = 0, Exponent = 0, Mantissa = 1.

   Each time we divide DBL_MIN by 2.0, we do a right shift on the number.
   Eventually, it will become zero.

   Note that the first time that DBL_MIN is divided by zero already results in
   a subnormal number (the exponent becomes zero) -- because DBL_MIN is
   indeed the least *normal* number.

   Remark II: if we were to assume that numbers are always represented using
   IEEE format, we could just take positive zero, set its first bit, and
   that would be the same as DBL_TRUE_MIN. But we'll be more careful and
   calculate it, dividing DBL_MIN by 2 successfully until it is zero.

   -- jpellegrini
  */
#ifdef DBL_TRUE_MIN
  return DBL_TRUE_MIN;
#else
  double x = DBL_MIN;
  double res = x;
  while (1) {
    if (x == 0.0) return res;
    res = x;
    x = x / 2.0;
  }
#endif
}

/*
<doc EXT real-precision
 * (real-precision)
 * (real-precision value)
 *
 * This parameter object allows changing the default precision used
 * to print real numbers.
 *
 * By precision when printing a number we mean the number of significant
 * digits -- that is, excluding the leading and trailing zeros in
 * decimal representation. (This is exactly the same as the number
 * for the `g` specifier for `printf` in the C language).
 *
 * @lisp
 * (real-precision)         => 15
 * (define f 0.123456789)
 * (display f)              => 0.123456789
 * (real-precision 3)
 * (display f)              => 0.123
 * (display   1.123456789)  => 1.12
 * (display  12.123456789)  => 12.1
 * (display 123.123456789)  => 123.0
 * @end lisp
 * In the last example, only three significant digits were printed (123),
 * and the zero only marks this number as inexact.
 *
 * If the number won't fit using the usual decimal format, it will be
 * printed in scientific notation, but still using the specified number
 * of significant digits:
 * @lisp
 * (display     1234.123456789) => 1.23e+03
 * (display    12345.123456789) => 1.23e+04
 * (display 12345678.123456789) => 1.23e+07
 * @end lisp
 * Repeating the three examples above with precision equal to one results
 * in the following.
 * @lisp
 * (real-precision 1)
 * (display     1234.123456789) => 1e+03
 * (display    12345.123456789) => 1e+04
 * (display 12345678.123456789) => 1e+07
 * @end lisp
 * If the number is only printed up to its n-th digit, then the printed nth
 * digit will be n rounded up or down, according to the digit that comes
 * after it.
 * @lisp
 * (real-precision 4)
 * (display 12.123456789) => 12.12  ;; "123..." rounded to "12"
 * (display 12.987654321) => 12.99  ;; "987..." rounded to "99"
 * @end lisp
doc>
*/
static SCM real_precision_conv(SCM value)
{
  long precision = STk_integer_value(value);

  if (precision <= 0 || precision > 50)
    STk_error("real precision must be an integer in ]0 50]. It was ~S",
              value);
  real_precision = (int) precision;
  return value;
}


/*
<doc EXT accept-srfi-169-numbers
 * (accept-srfi-169-numbers)
 * (accept-srfi-169-numbers value)
 *
 * This parameter object permits to change the behavior of the reader
 * with underscores in numbers. Numbers with underscores are defined
 * in ,(link-srfi 169). By default, this variable is true, meaning that
 * underscores are accepted in numbers.
 *
 * @lisp
 * (accept-srfi-169-numbers)        => #t
 * (symbol? '1_000_000)             => #f
 * (number? '1_000_000)             => #t
 * (accept-srfi-169-numbers #f)
 * (symbol? '1_000_000)             => #t
 * (number? '1_000_000)             => #f
 * @end lisp
doc>
*/
static SCM srfi_169_conv(SCM value)
{
  use_srfi_169 = (value != STk_false);
  return MAKE_BOOLEAN(use_srfi_169);
}

/******************************************************************************
 *
 * Constructor Functions
 *
 ******************************************************************************/
static inline SCM Cmake_complex(SCM r, SCM i)
{
  SCM z;

  NEWCELL(z, complex);
  COMPLEX_REAL(z) = r;
  COMPLEX_IMAG(z) = i;
  return z;
}

static inline SCM make_complex(SCM r, SCM i)
{
  return (isexactp(i) && zerop(i)) ? r : Cmake_complex(r, i);
}

static inline SCM make_polar(SCM a, SCM m)
{
  return make_complex(mul2(a, my_cos(m)), mul2(a, my_sin(m)));
}


static inline SCM Cmake_rational(SCM n, SCM d)
{
  SCM z;

  NEWCELL(z, rational);
  RATIONAL_NUM(z) = n;
  RATIONAL_DEN(z) = d;
  return z;
}


static SCM make_rational(SCM n, SCM d)
{
  SCM gcd;

  if (zerop(d))
    STk_error("cannot make rational with null denominator");

  /* Always keep sign in the numerator */
  if (negativep(d)) {
    n = mul2(n, MAKE_INT((unsigned long) -1));
    d = mul2(d, MAKE_INT((unsigned long) -1));
  }

  /* Simplify rational */
  gcd = gcd2(n, d);
  if (gcd != MAKE_INT(1)) {
    if (d == gcd) return int_quotient(n, gcd);
    n = int_quotient(n, gcd);
    d = int_quotient(d, gcd);
  }

  /* Make rational if denominator is not 1 (if n is small, it is not a bignum) */
  if (d ==  MAKE_INT(1))
    return n;
  else
    return Cmake_rational(n, d);
}


/******************************************************************************
 *
 * Types declaration
 *
 ******************************************************************************/
static void print_bignum(SCM n, SCM port, int _UNUSED(mode))
{
  char *s;

  s = STk_must_malloc_atomic(mpz_sizeinbase(BIGNUM_VAL(n), 10) + 2);
  mpz_get_str(s, 10, BIGNUM_VAL(n));
  STk_puts(s, port);
  STk_free(s);
}


static void print_rational(SCM n, SCM port, int mode)
{
  STk_print(RATIONAL_NUM(n), port, mode);
  STk_putc('/', port);
  STk_print(RATIONAL_DEN(n), port, mode);
}

static void print_complex(SCM n, SCM port, int mode)
{
  SCM imag = COMPLEX_IMAG(n);

  STk_print(COMPLEX_REAL(n), port, mode);
  /* About the next IF condition:
     NaNs and infinities do have a signbit and they satisfy REALP, so,
     specifically for NaNs:

     1) +nan.0 passes the first line in the condition (sign bit is zero).
        But for the extra plus to be printed, the other two lines
        require:
          i) Either zero (it's not), OR:
         ii) Positive AND not infinity -- but NaNs so not satisfy the
             STklos C predicate positivep. :)
        Then the extra + is not printed, and the +nan.0 is printed.

     2) -nan.0 fails the first line, because its sign bit is 1 (negated = 0).
        Then the + is not printed, and the -nan.0 is printed.                 */
  if ((!((REALP(imag) && signbit(REAL_VAL(imag))))) &&
      (zerop(imag) ||
       (positivep(imag) && (!(REALP(imag) && isinf(REAL_VAL(imag)))))))

    STk_putc('+', port);
  STk_print(imag, port, mode);
  STk_putc('i', port);
}



static struct extended_type_descr xtype_bignum = {
  .name  = "bignum",
  .print = print_bignum
};

static struct extended_type_descr xtype_complex = {
  .name  = "complex",
  .print = print_complex
};

static struct extended_type_descr xtype_rational = {
  .name  = "rational",
  .print = print_rational
};


/******************************************************************************
 *
 * Conversion Functions
 *
 ******************************************************************************/

static inline SCM long2scheme_bignum(long x)
{
  SCM z;

  NEWCELL(z, bignum);
  mpz_init_set_si(BIGNUM_VAL(z), x);
  return z;
}


static inline SCM long2integer(long x)
{
  return (INT_MIN_VAL<=x && x<=INT_MAX_VAL) ?  MAKE_INT(x): long2scheme_bignum(x);
}


static inline SCM double2real(double x)
{
  SCM z;

  if (isnan(x) && signbit(x)) /* convert -nan.0 to +nan.0 */ x = STk_NaN;
  NEWCELL(z, real);
  REAL_VAL(z) = x;
  return z;
}

static inline SCM bignum2integer(mpz_t n)
{
  return MAKE_INT(mpz_get_si(n));
}


static inline double bignum2double(mpz_t n)
{
    /* If the result does not fit a double, we return (+/-)inf.0
       We don't use the result of mpz_get_d because it does not
       guarantee that an inf will be returned in this case.

       The most positive and most negative flonums are (+/-) DBL_MAX,
       so we only call the GMP function mpz_cmpabs_d once (no need to
       test for both > +DBL_MAX and < -DBL_MAX).                 */
    if (mpz_cmpabs_d((n), +DBL_MAX) > 0)
        return (mpz_sgn(n)>0)
            ? plus_inf
            : minus_inf;

    /* A very large integer may not be representable as a float.

       mpz_get_d always "rounds towards zero" -- that is, it returns
       the closest float to n ***that is between 0 and n***, but
       R7RS requires 'inexact' to return the *closest* number.
       So we need to adapt.

       Suppose there are two representable integers around n, but n
       itself is not representable. Call those integers 'below' and
       'above':

           0                        below       n  above
        ---|--------------------------|---------|----|-----
                                      v
                                   returned
                                    by GMP

        As the figure shows, even if there is an integer 'above'
        that is closer to n, the number 'below' (closer to 0) will be
        returned.  Note that the whole picture could be reflected
        around zero if n is negative, so "above" actually means
        "farthest from zero" but not "largest":

           above  n       below                        0
        -----|----|---------|--------------------------|---
                                                       |
                                                neg <--+--> pos

        We of course can be sure that there is no integer between
        'below' and 'above'.

        So we do the following:

        1. Get the next integer representable as double with nextafter (the
           one starting from below, but *away from zero*). We call this
           one 'above'
        2. Convert both below and above back into bignums (!), as 'zbelow'
           and 'zabove'
        3. Measure (using the GMP) the distances ABS(zabove-n) and ABS(n-zbelow)
           and if n is closer to zabove, we return ceil(above) or floor(above),
           depending on the sign. If it's closer to below, we return below.     */
    double below = mpz_get_d(n);

    /* Use ceil or floor, since we want the next *integer* representable as
       double -- and that's exactly what ceil and floor do! */
    double above = (below > 0)
        ? ceil(nextafter(below, plus_inf))
        : floor(nextafter(below, minus_inf));

    /* So, if going further we get to infinity, we return 'below'. This
       is our interpretation: "infinity" is always farther to 'n' than
       'below'. (And the GMP may crash without this, if we try to
       initialize a number with an infinite double!)
       'Below' is guaranteed to NOT be infinite (it was the first
       thing we did in this function!)                              */
    if (isinf(above)) return below;

    /* the *_set_d functions in GMP are *exact*, so no precision is lost here: */
    mpz_t zbelow, zabove;
    mpz_init_set_d(zabove, above);
    mpz_init_set_d(zbelow, below);

    /* zabove <- distance(zabove, n)
       zbelow <- distance(n, zbelow)   */
    mpz_sub(zabove, zabove, n);
    mpz_sub(zbelow, n, zbelow);

    /* First store res, then clear zbelow amd zabove, and THEN
       return! */
    double res = (mpz_cmpabs(zabove, zbelow) >= 0)
        ? below
        : above;

    mpz_clear(zbelow);
    mpz_clear(zabove);

    return res;
}


static inline double scheme_bignum2double(SCM b)
{
  return bignum2double(BIGNUM_VAL(b));
}


static inline SCM scheme_bignum2real(SCM bn)
{
  return double2real(scheme_bignum2double(bn));
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


static SCM double2integer(double n)     /* small or big depending on n's size */
{
  /* Try first to convert n to a long */
  if (((double) INT_MIN_VAL <= n) && (n <= (double) INT_MAX_VAL))
    return MAKE_INT((long) n);
  else {
    /* n doesn't fit in a long => build a bignum. */
    mpz_t r;

    mpz_init_set_d(r, n);
    return bignum2number(r);
  }
}

static SCM double2rational(double d)
{
  double fraction, i;
  SCM int_part, res;
  int negative = 0;

  if (d < 0.0) { negative = 1; d = -d; }
  fraction = modf(d, &i);
  int_part = double2integer(i);

  if (!fraction) {
    res = negative ? sub2(MAKE_INT(0), int_part) : int_part;
  } else {

#ifdef __MINI_GMP_H__
    /* BEGIN code for compiling WITH MINI GMP (*no* rationals!) */
    SCM num, den;
    num = MAKE_INT(0);
    den = MAKE_INT(1);

    while (fraction) {
      num      = mul2(num, MAKE_INT(2));
      den      = mul2(den, MAKE_INT(2));
      fraction = modf(ldexp(fraction, 1), &i);
      if (i)
        num = add2(num, MAKE_INT(1));
    }
    res = add2(int_part, div2(num, den));
    if (negative)
      res = sub2(MAKE_INT(0), res);
#else
    /* BEGIN code for compiling WITH FULL GMP (*with* rationals!) */

    /* We just create a rational from a double using the GMP, then
       get back the numerator and denominator. According to the
       GMP documentation, this conversion is exact -- there is no
       rounding. */
    mpz_t num, den;
    mpq_t q;
    mpz_init(num);
    mpz_init(den);
    mpq_init(q);
    mpq_set_d(q, d);

    mpq_get_num(num, q);
    mpq_get_den(den, q);
    if (negative) mpz_neg(num,num);
    res = Cmake_rational(bignum2number(num), bignum2number(den));
    mpq_clear(q);
    mpz_clear(num);
    mpz_clear(den);
#endif /* __MINI_GMP_H__ */

  }
  return res;
}




/* The following code is an adaptation of code stolen in mini-gmp   */
/* (mpq_get_d function)                                             */
static double bigrational2double(mpz_t num, mpz_t den) {
  #ifndef GMP_LIMB_BITS
     #define GMP_LIMB_BITS (sizeof(mp_limb_t) * CHAR_BIT)
  #endif
  #define GMP_LIMB_HIGHBIT ((mp_limb_t) 1 << (GMP_LIMB_BITS - 1))

  mp_bitcnt_t ne, de, ee;
  mpz_t z;
  double B, ret;

  ne = mpz_sizeinbase(num, 2);
  de = mpz_sizeinbase(den, 2);

  ee = CHAR_BIT * sizeof (double);
  if (de == 1 || ne > de + ee)
    ee = 0;
  else
    ee = (ee + de - ne) / GMP_LIMB_BITS + 1;

  mpz_init (z);
  mpz_mul_2exp (z, num, ee * GMP_LIMB_BITS);
  mpz_tdiv_q (z, z, den);
  ret = mpz_get_d (z);
  mpz_clear (z);

  B = 4.0 * (double) (GMP_LIMB_HIGHBIT >> 1);
  for (B = 1 / B; ee != 0; --ee)
    ret *= B;
  return ret;
}

static double rational2double(SCM r)
{
  SCM num = RATIONAL_NUM(r);
  SCM den = RATIONAL_DEN(r);

  switch (convert(&num, &den)) {
    case tc_integer: return ((double) INT_VAL(num)) / ((double) INT_VAL(den));
    case tc_bignum:  return bigrational2double(BIGNUM_VAL(num), BIGNUM_VAL(den));
    default:         STk_panic("bad rational ~S", r);
  }
  return 0.0; /* never reached */
}

static inline SCM rational2real(SCM r)
{
  return double2real(rational2double(r));
}

static inline SCM real2integer(SCM r)
{
  double v = REAL_VAL(r);

  if (floor(v) != v) {
    /* This is not an inexact integer (weak test) */
    STk_error("bad number (~s) in an integer division", r);
  }
  return double2integer(v);
}

void STk_double2Cstr(char *buffer, size_t bufflen, double n)
{
  snprintf(buffer, bufflen, "%.*g", real_precision, n);
  if (strchr(buffer, '.') == NULL && strchr(buffer, 'e') == NULL)
    strncat(buffer, ".0", bufflen);
  /* Treat special cases of +nan.0 and +inf.0 */
  if (isalpha(buffer[0])) {
    if (strcmp(buffer, "inf.0") == 0) snprintf(buffer, bufflen, "+inf.0");
    if (strcmp(buffer, "nan.0") == 0) snprintf(buffer, bufflen,  "+nan.0");
  }
}


/* Convert a number to a C-string. Result must be freed if != from buffer */
static char *number2Cstr(SCM n, long base, char buffer[], size_t bufflen)
{
  char *s = buffer;

  switch (TYPEOF(n)) {
    case tc_integer:
      {
        long tmp, val = INT_VAL(n);
        int u;

        if (val < 0) {
          val  = -val;
          *s++ = '-';
        }
        /* Find how much digit we need */
        for (s++, tmp=val; tmp >= base; tmp /= base) s++;

        *s = '\0'; tmp = val;
        do {
          u = tmp % base;
          *(--s) = u + ((u < 10) ? '0' : 'a'-10);
          tmp   /= base;
        }
        while (tmp);
        return buffer;
      }
    case tc_bignum:
      s = STk_must_malloc_atomic(mpz_sizeinbase(BIGNUM_VAL(n), base) + 2);
      s = mpz_get_str(s, base, BIGNUM_VAL(n));
      return s;
    case tc_rational:
      {
        char *left, *right, *res, tmp[100];
        size_t len;

        left  = number2Cstr(RATIONAL_NUM(n), base, buffer, bufflen);
        right = number2Cstr(RATIONAL_DEN(n), base, tmp, sizeof(tmp));
        len   = strlen(left) + strlen(right) + 2;
        res   = STk_must_malloc_atomic(len);
        snprintf(res, len, "%s/%s", left, right);
        if (right!=tmp) STk_free(right); /*buffer will event. be deallocated by caller*/
        return res;
      }
    case tc_complex:
      {
        /* We print the real and imaginary parts in left (real) and
           right (imaginary), and then glue them together in res. tmp
           is just a temporary buffer for the imaginary part. */
        char *left, *right, *res, tmp[100];
        size_t len;

        left  = number2Cstr(COMPLEX_REAL(n), base, buffer, bufflen);
        right = number2Cstr(COMPLEX_IMAG(n), base, tmp, sizeof(tmp));
        len   = strlen(left) + strlen(right) + 3;
        res   = STk_must_malloc_atomic(len);
        /* If the imaginary part is negative, infinite, or nan, then its representation
           will already have the sign (-2.5, +nan.0, -inf.0 etc), so we don't add the
           sign before the imaginary part. */
        snprintf(res, len, "%s%s%si", left, (isdigit(*right) ? "+" : ""), right);
        if (right!=tmp) STk_free(right); /* buffer will event. be deallocated by caller */
        return res;
      }
    case tc_real:
      if (base != 10) STk_error("base must be 10 for this number", n);
      STk_double2Cstr(buffer, bufflen, REAL_VAL(n));
      return buffer;

    default: return STk_void; /* never reached (for the gcc static analyzer)  */
  }
}


/*===== The general conversion routine ==== */

static type_cell convert(SCM *px, SCM *py)
{
  SCM x = *px;
  SCM y = *py;

  if (TYPEOF(x)==TYPEOF(y)) return(TYPEOF(x)); /* avoid testing on current cases */
  switch (TYPEOF(x)) {
    case tc_complex:
            switch (TYPEOF(y)) {
              case tc_complex: /*already done */
              case tc_real:
              case tc_rational:
              case tc_bignum:
              case tc_integer:  *py = Cmake_complex(y, MAKE_INT(0)); break;
              default:          error_bad_number(y);                 break;
            }
            break;
    case tc_real:
            switch (TYPEOF(y)) {
              case tc_complex:  *px = Cmake_complex(x, MAKE_INT(0));    break;
              case tc_real:     /* already done */                      break;
              case tc_rational: *py = rational2real(y);                 break;
              case tc_bignum:   *py = scheme_bignum2real(y);            break;
              case tc_integer:  *py = double2real((double) INT_VAL(y)); break;
              default:          error_bad_number(y);                    break;
            }
            break;
    case tc_rational:
            switch (TYPEOF(y)) {
              case tc_complex:  *px = Cmake_complex(x, MAKE_INT(0));   break;
              case tc_real:     *px = rational2real(x);                break;
              case tc_rational: /* already done */                     break;
              case tc_bignum:   /* no break */
              case tc_integer:  *py = Cmake_rational(y , MAKE_INT(1)); break;
              default:          error_bad_number(y);                   break;
            }
            break;
    case tc_bignum:
            switch (TYPEOF(y)) {
              case tc_complex:  *px = Cmake_complex(x, MAKE_INT(0));    break;
              case tc_real:     *px = scheme_bignum2real(x);            break;
              case tc_rational: *px = Cmake_rational(x , MAKE_INT(1));  break;
              case tc_bignum:   /* already done */                      break;
              case tc_integer:  *py = long2scheme_bignum(INT_VAL(y));   break;
              default:          error_bad_number(y);                    break;
            }
            break;
    case tc_integer:
            switch (TYPEOF(y)) {
              case tc_complex:  *px = Cmake_complex(x, MAKE_INT(0));    break;
              case tc_real:     *px = double2real((double) INT_VAL(x)); break;
              case tc_rational: *px = Cmake_rational(x,  MAKE_INT(1));  break;
              case tc_bignum:   *px = long2scheme_bignum(INT_VAL(x));   break;
              case tc_integer:  /* already done */                      break;
              default:          error_bad_number(y);                    break;
            }
            break;
    default: error_bad_number(x);
  }
  return TYPEOF(*px);
}


long STk_integer_value(SCM x) /* Returns LONG_MIN if not representable as long */
{
  if (INTP(x)) return INT_VAL(x);
  if (BIGNUMP(x)) {
    mpz_t *v = &BIGNUM_VAL(x);
    if (mpz_cmp_si(*v, LONG_MIN) > 0 && mpz_cmp_si(*v, LONG_MAX) <= 0)
      return  mpz_get_si(*v);
  }
  return LONG_MIN;
}

unsigned long STk_uinteger_value(SCM x) /* Returns ULONG_MAX if not an ulong */
{
  if (INTP(x) && ((long)x > 0)) return INT_VAL(x); /* sign(INTEGER_VAL(x))==sign(x) */
  if (BIGNUMP(x)) {
    mpz_t *v = &BIGNUM_VAL(x);
    if (mpz_cmp_ui(*v, 0) >= 0 && mpz_cmp_ui(*v, ULONG_MAX) < 0)
      return mpz_get_ui(*v);
  }
  return ULONG_MAX;
}


SCM STk_long2integer(long n)
{
  return long2integer(n);
}


SCM STk_ulong2integer(unsigned long n)
{
  if (n <= INT_MAX_VAL) {  /* n  >= 0 since it is an ulong */
    return MAKE_INT(n);
  }
  else {
    SCM z;

    NEWCELL(z, bignum);
    mpz_init_set_ui(BIGNUM_VAL(z), n);
    return z;
  }
}


long STk_integer2int32(SCM n, int *overflow)
{
  *overflow = 0;

  if (INTP(n)) {
#if (LONG_MAX == INT32_MAX)       /* longs are on 32 bits */
    return INT_VAL(n);
#else                             /* longs are more than 32 bits (probably 64) */
    long val = INT_VAL(n);

    if ((- INT32_MAX - 1) <= val && val <= INT32_MAX)
      return val;
    else {
      *overflow = 1;
      return 0;
    }
#endif
  }
  if (BIGNUMP(n)) {
    mpz_t *v = &BIGNUM_VAL(n);
    if (mpz_cmp_si(*v, (- INT32_MAX - 1)) >= 0 && mpz_cmp_si(*v, INT32_MAX) <= 0)
      return mpz_get_si(*v);
  }
  *overflow = 1;
  return 0;
}


unsigned long STk_integer2uint32(SCM n, int *overflow)
{
  *overflow = 0;

  if (INTP(n)) {
    long val = INT_VAL(n);

    if (val >= 0) {
#if (ULONG_MAX == UINT32_MAX)   /* unsigned longs are on 32 bits */
      return (unsigned long) INT_VAL(n);
#else                           /* longs are more than 32 bits (probably 64) */
      if (val <= UINT32_MAX)
        return val;
      else {
        *overflow = 1;
        return 0;
      }
#endif
    }
  }
  if (BIGNUMP(n)) {
    mpz_t *v = &BIGNUM_VAL(n);
    if (mpz_cmp_ui(*v, 0) >= 0 && mpz_cmp_ui(*v, UINT32_MAX) <= 0)
      return mpz_get_ui(*v);
  }
  *overflow = 1;
  return 0;
}



SCM STk_double2real(double d)
{
  return double2real(d); /* use the inlined function */
}


double STk_number2double(SCM n) /* returns NaN if not convertible */
{
  switch (TYPEOF(n)) {
    case tc_real:     return REAL_VAL(n);
    case tc_rational: return REAL_VAL(rational2real(n));
    case tc_bignum:   return REAL_VAL(scheme_bignum2real(n));
    case tc_integer:  return (double) INT_VAL(n);
    default:          return STk_NaN;
  }
}



/******************************************************************************
 *
 * Utilities
 *
 ******************************************************************************/

static long do_compare(SCM x, SCM y)
{
  double d1=0, d2=0;

  switch (TYPEOF(x)) {
    case tc_real:
            switch (TYPEOF(y)) {
              case tc_complex:  goto general_diff;
              case tc_real:     d1 = REAL_VAL(x); d2 = REAL_VAL(y);
                                goto double_diff;
              case tc_rational:
              case tc_bignum:   goto general_diff;
              case tc_integer:  d1 = REAL_VAL(x); d2 =  INT_VAL(y);
                                goto double_diff;
              default:          break;
            }
            break;
    case tc_integer:
            switch (TYPEOF(y)) {
              case tc_complex:  goto general_diff;
              case tc_real:     d1 = INT_VAL(x); d2 = REAL_VAL(y);
                                goto double_diff;
              case tc_rational:
              case tc_bignum:   goto general_diff;
              case tc_integer:  return (INT_VAL(x) - INT_VAL(y));
              default:          break;
            }
            break;
    case tc_complex:
    case tc_rational:
    case tc_bignum:
            switch (TYPEOF(y)) {
              case tc_complex:
              case tc_real:
              case tc_rational:
              case tc_bignum:
              case tc_integer:  goto general_diff;
              default:          break;
            }
            break;
    default:
            break;
  }
  /* if we are here, it s that x and y cannot be compared */
  STk_error("comparison between ~S and ~S impossible", x,  y);
double_diff:
  if (isnan(d1) && isnan(d2))
    return 0;
  return (d1 == d2) ? 0 : ((d1 < d2)?  -1 : 1);
general_diff:
  {
    SCM d = sub2(x, y);

    if (zerop(d)) return 0;
    /* complex numbers cannot be compared => return always 1 */
    return COMPLEXP(d) ? 1 : (negativep(d) ? -1: 1);
  }
}


static SCM int_quotient(SCM x, SCM y)
/* Specialized version for rationals.
   Accepts only integer or bignums as params, and not inexacts as
   int_divide. Also, *only* computes and returns the quotient,
   not the remainder. */
{
  mpz_t q;
  SCM res;

  if (INTP(x) && INTP(y))
    return MAKE_INT(INT_VAL(x)/INT_VAL(y));

  mpz_init(q);

  if (INTP(x)) /* && BIGNUMP(y), of course! */
    return MAKE_INT(0); /* int / BIGNUM = 0... */

  if (INTP(y)) /* && BIGNUMP(x), of course! */
    /* The sign is in the numerator, so it's OK to use the '_ui' variant
       from the GMP.  */
    mpz_tdiv_q_ui(q, BIGNUM_VAL(x), INT_VAL(y));
  else /* Here x and y are both bignums */
    mpz_tdiv_q(q, BIGNUM_VAL(x), BIGNUM_VAL(y));

  res = bignum2number(q);

  mpz_clear(q);

  return res;
}

static int digitp(char c, long base)
{
  c = ('0' <= c && c <= '9') ? c - '0':
      ('a' <= c && c <= 'f') ? c - 'a' + 10:
      ('A' <= c && c <= 'F') ? c - 'A' + 10:
      (c == '#')             ? 0           :
      100;
  return (c < base);
}


/******************************************************************************
 *
 * Number parser
 *
 ******************************************************************************/

static SCM compute_exact_real(char *s, char *p1, char *p2, char *p3, char *p4)
{
  SCM int_part, fract_part, exp_part;
  mpz_t tmp;

  mpz_init(tmp);
  int_part   = MAKE_INT(0);
  fract_part = MAKE_INT(0);
  exp_part   = MAKE_INT(1);

  /* Representation of the given number (number is '\0' terminated)
   *
   *        +xxxxxxxxxxxxxxxxx.yyyyyyyyyyyyyE+zzzzz
   *        ^                 ^^            ^^
   *        |                 ||            ||
   *        +-str          p1-++-p2      p3-++-p4
   */

  /* patch the given string so that splitting the various parts of the number
   * is easy
   */
  if (p1) *p1 = '\0';
  if (p3) *p3 = '\0';

  if (p1) {             /* compute integer part */
    if (mpz_init_set_str(tmp, s, 10L) < 0) { mpz_clear(tmp); return STk_false; }
    int_part = bignum2number(tmp);
  }

  if (p3 > p2) {        /* compute decimal part as a rational 0.12 => 6/5 */
    SCM num, den;

    if (mpz_init_set_str(tmp, p2, 10L) < 0) { mpz_clear(tmp); return STk_false; }
    num = bignum2number(tmp);

    mpz_ui_pow_ui(tmp, 10UL, strlen(p2));
    den = bignum2number(tmp);

    fract_part = make_rational(num, den);
  }

  if (p4) {             /* compute exposant as a rational 3 => 1000, -3 => 1/1000 */
    long expo;

    expo = atoi(p4);
    if (expo > 0) {
      mpz_ui_pow_ui(tmp, 10UL, expo);
      exp_part = bignum2number(tmp);
    } else {
      mpz_ui_pow_ui(tmp, 10UL, -expo);
      exp_part = div2(MAKE_INT(1), bignum2number(tmp));
    }
  }

  mpz_clear(tmp);
  /* now return (int_part + fract_part) * exp_part */
  return mul2(add2(int_part, fract_part), exp_part);
}


static int could_be_a_srfi_169_number(char *str, long base) // SRFI-169 syntax OK?
{
  char prev = *str;

  if (prev == '_')
    // a number cannot start with a '_'
    return 0;

  for (char *p=str+1; *p; p++) {
    char next= *(p+1); /* could be the final '\0' */
    if (*p == '_') {
      // a '_' must be surrounded by digit (or the '#' for R5RS)
      // note: we use base 16 here just for verification, control will be done later
      if (!digitp(prev, base) || !digitp(next, base))
        return 0;
    }
    prev = *p;
  }
  return 1;
}

static void clean_srfi_169_number(char *str)  // Suppress '_' of a correct SRFI-169
{
  char *q = str;

  for (char *p = str; *p; p++) {
    if (*p != '_') {
      if (p != q) *q = *p;
      q++;
    }
  }
  *q = '\0';
}

static int contain_weird_chars(char *str) // has characters that'll be patched later?
{
  for (char *p=str; *p; p++) {
    switch (*p) {
    case '#':
    case 's': case 'S': case 'f': case 'F':
    case 'd': case 'D': case 'l': case 'L': return 1;
    }
  }
  return 0;
}

static SCM read_integer_or_real(char *str, long base, char exact_flag, char **end)
{
  int adigit=0, isint=1;
  char saved_char = '\0', *original_str = str;
  char *p, *p1, *p2, *p3, *p4;
  SCM res;

  /* if first char cannot start a number, return #f (not a number => symbol) */
  if (!digitp(*str, base) && *str != '-' && *str != '+' && *str != '.')
    return STk_false;

  /* If str contains certain characters, they will be patched later, to passed a correct
   * string to  standard conversion functions. For instance, "1_2#s3" will be modified
   * in place to "120e3" before to be sent to the standard strtod function. The problem
   * is that we can think that we are on a number an see later that it's a symbol (all non
   * numbers are symbols in STklos). For instance "1_2#s3XY" is a symbol and,
   * if we don't care, it will be read as symbol "120e3XY".
   * Consequently, we need to work on a copy of str, when analyzing numbers. Since
   * allocations for each number slow down significantly the reader, we'll work on
   * minimizing the number of string duplications.
   */

  /* suppress eventually '_' of SRFI_169 numbers */
  if (use_srfi_169 && strchr(str, '_')) {
    if (could_be_a_srfi_169_number(str, base)) {
      str = STk_strdup(str);
      clean_srfi_169_number(str);
    } else
      /* we have '_' and it it's not a valid srfi-169 number => it's a symbol */
      return STk_false;
  }

  /* if number contains weird exponent notation or '#' characters, duplicate */
  if (contain_weird_chars(str) && str == original_str) {
     /* Note: if str != original_str, we have already duplicated the string since it
      * contains at least an underscore */
    str = STk_strdup(str);
  }

  p = str; // str can be the original string or a copy of it, if we'll patch it

  /* See function compute_exact_real for the meaning of p1..p4 pointers */
  p1 = p2 = p3 = p4 = NULL;

  if (*p == '-' || *p == '+') p+=1;
  if (*p == '#') return STk_false;

  while(digitp(*p, base)) { p+=1; adigit=1; if (*p == '#') isint = 0; }

  if (adigit) p1 = p;           /* p1 = end of integral part */

  if (*p=='.') {
    isint = 0; p += 1;
    p2 = p;
    while(digitp(*p, base)) { p+=1; adigit=1; }
    p3 = p;
  }

  if (!adigit) return STk_false;

  if (*p && strchr("eEsSfFdDlL", *p)) {
    isint = 0;
    p += 1;
    p4 = p;
    if (*p == '-' || *p == '+') p+=1;
    if (!digitp(*p, base)) return STk_false;
    p+=1;
    while (digitp(*p, base)) p+=1;
  }
  if (*p) {
    /* Patch the end of the number with a '\0' (will be restored on exit) */
    saved_char = *p;
    *p = '\0';
  }


  if (isint) {
    /* We are sure to have an integer. Read it as a bignum and see if we can
     * convert it in smallnum after that. Small numbers (those with few
     * digits expressed in base 10) are not read as bignums.
     * This optimisation is easily missed (e.g. 000000000000000001 will be
     * read as a bignum), but it avoids allocation for current numbers
     * represented in a usual form.
     */
    mpz_t n;

    if (*str == '+') str+=1; /* mpz_init_set_str doesn't recognize +xyz !!! */
    if (strlen(str) <= log10_maxint && base == 10) {
      long num = atol(str);

      res = (exact_flag == 'i') ? double2real((double) num): MAKE_INT(num);
    }
    else {
      if (mpz_init_set_str(n, str, base) < 0) {
        /* Bad syntax for a bignum */
        res = STk_false;
      } else if (BIGNUM_FITS_INTEGER(n)) {
        /* Can be represented as a short integer */
        long num = mpz_get_si(n);

        res = (exact_flag == 'i') ? double2real((double) num): MAKE_INT(num);
      } else {
        /* It's a bignum */
        res = bignum2scheme_bignum(n);
        if (exact_flag == 'i') res = scheme_bignum2real(res);
      }
      mpz_clear(n);
    }
  } else {
    /* Number is a float */
    if (base == 10) {
      /* Replace sharp signs by 0 */
      for(p=str; *p; p++)
        switch (*p) {
          case '#': *p = '0'; break;
          case 's': case 'S': case 'f': case 'F':
          case 'd': case 'D': case 'l': case 'L': *p = 'e';
        }
      if (exact_flag == 'e') {
        res = compute_exact_real(str, p1, p2, p3, p4);
      } else {
        res = double2real(strtod(str, &p));
      }
    }
    else
      res = STk_false;
  }

  if (saved_char) *p = saved_char;  /* character which ended the number */
  *end = p;                         /* position of last analysed character */
  return res;
}


static SCM read_rational_den(SCM num, char *str, long base, char exact_flag, char **end)
{
  SCM den;

  den = read_integer_or_real(str, base, exact_flag, end);
  if (den == STk_false) return STk_false;

  if ((TYPEOF(num) == tc_integer || TYPEOF(num) == tc_bignum) &&
      (TYPEOF(den) == tc_integer || TYPEOF(den) == tc_bignum))
    return make_rational(num, den);
  else if (exact_flag=='i')
    /* We're sure we got here with either fixnums, bignums or reals, so
       div2 will always work. */
    return div2(num,den);

  STk_error("cannot make rational with ~S and ~S", num, den);

  return STk_false;             /* never reached */
}

/* ----------------------------------------------------------------------
 * STk_Cstr2number: Read a number from a C string
 * ---------------------------------------------------------------------- */

/* STk_Cstr2simple_number will read from str a non-complex number.
   The function STk_Cstr2number, which reads complexes, uses
   this one to read the two parts of the number, */
static SCM Cstr2simple_number(char *str, char *exact, long *base, char **end)
{
  int i, radix;
  char *p = str;
  SCM num = STk_false;

  if ((*str == '-' || *str == '+') && isalpha(str[1])) {
    /* Treat special inf "+values.0" -inf.0 , "+nan.0", "-nan.0"
     * NOTE: R7RS says that -nan.0 is synonym to +nan.0 */
    if      (strncmp(str, MINUS_INF,6)==0) num = double2real(minus_inf);
    else if (strncmp(str, PLUS_INF,6)==0)  num = double2real(plus_inf);
    else if (strncmp(str, MINUS_NaN,6)==0) num = double2real(STk_NaN);
    else if (strncmp(str, PLUS_NaN,6)==0)  num = double2real(STk_NaN);

    if (num != STk_false) { /* Did we actually read an inf or nan? */
      *end = str + 6;
      return num;
    }
  }

  /* Should we read in a different basis or exactness? */
  radix = 0;
  for (i = 0; i < 2 && *p == '#'; i++) { /* two loops laps to permit #i#xff -> 255.0 */
    p += 1;
    switch (*p++) {
      case 'e': if (*exact == ' ')  { *exact = 'e'; break;} else return STk_false;
      case 'i': if (*exact == ' ')  { *exact = 'i'; break;} else return STk_false;
      case 'b': if (!radix) {*base = 2;  radix = 1; break;} else return STk_false;
      case 'o': if (!radix) {*base = 8;  radix = 1; break;} else return STk_false;
      case 'd': if (!radix) {*base = 10; radix = 1; break;} else return STk_false;
      case 'x': if (!radix) {*base = 16; radix = 1; break;} else return STk_false;
      default:  return STk_false;
    }
    str += 2;
  }

  /* If the user tries to make us read a complex with two different
     bases, #xa+#o10i it will fail (as it should), because trying to
     read +#o10 below fails. */
  num = read_integer_or_real(p, *base, *exact, &p);
  if (num == STk_false) return STk_false;

  if (*p == '/')
    num = read_rational_den(num, p+1, *base, *exact, &p);

  /* Store in end where the number ends. */
  *end = p;
  return num;
}



/* Reads a number from str in the specified base. */
SCM STk_Cstr2number(char *str, long base)
{
  if (strcmp(str, "+i")==0) return make_complex(MAKE_INT(0), MAKE_INT(+1UL));
  if (strcmp(str, "-i")==0) return make_complex(MAKE_INT(0), MAKE_INT(-1UL));
  else {
    char *end, *end2 = "";
    char exact = ' ';
    SCM a, b;

    /* First part of the number */
    a = Cstr2simple_number(str, &exact, &base, &end);
    if (a == STk_false) return STk_false; /* Not even the first part was good */

    /* Second part of the number; the possibilities now are:
       i)   Non-complex;
       ii)  +bi, -bi;
       iii) a+bi, a-bi;
       iv)  a@b;
       v)   not a number */
    switch (*end) {
      case '\0':
        return a;
      case 'i':
        if (*str == '+' || *str == '-')
          if (*(end+1) == '\0') return make_complex(MAKE_INT(0),a);
        break;
      case '+':
      case '-':
        if (strcmp(end, "+i")==0) return make_complex(a,MAKE_INT(+1UL));
        if (strcmp(end, "-i")==0) return make_complex(a,MAKE_INT(-1UL));
        b = Cstr2simple_number(end, &exact, &base, &end2);
        if (*end2 == 'i' && *(end2+1) == '\0') return make_complex(a,b);
        break;
      case '@':
        /* end+1, because we want to skip the '@' sign: */
        b = Cstr2simple_number(end+1, &exact, &base, &end2);
        if (*end2 == '\0') return make_polar(a,b);
        break;
    }
    return STk_false;
  }
}

/******************************************************************************
 *
 *                      Scheme primitives and utilities
 *
 ******************************************************************************/


/*
<doc number? complex? real? rational? integer?
 * (number? obj)
 * (complex? obj)
 * (real? obj)
 * (rational? obj)
 * (integer? obj)
 *
 * These numerical type predicates can be applied to any kind of
 * argument, including non-numbers. They return |#t| if the object is of
 * the named type, and otherwise they return |#f|. In general, if a type
 * predicate is true for a number then all higher type predicates are
 * also true for that number. Consequently, if a type predicate is
 * false of a number, then all lower type predicates are also false of
 * that number.
 *
 * If |z| is an inexact complex number, then |(real? z)| is true if and
 * only if |(zero? (imag-part z))| is true. If |x| is an inexact real
 * number, then |(integer? x)| is true if and only if
 * |(and (finite? x) (= x (round x)))|
 *
 *
 * @lisp
 *   (complex? 3+4i)         =>  #t
 *   (complex? 3)            =>  #t
 *   (real? 3)               =>  #t
 *   (real? -2.5+0.0i)       =>  #t
 *   (real? #e1e10)          =>  #t
 *   (rational? 6/10)        =>  #t
 *   (rational? 6/3)         =>  #t
 *   (integer? 3+0i)         =>  #t
 *   (integer? 3.0)          =>  #t
 *   (integer? 3.2)          =>  #f
 *   (integer? 8/4)          =>  #t
 *   (integer? "no")         =>  #f
 *   (complex? +inf.0)       =>  #t
 *   (real? -inf.0)          =>  #t
 *   (rational? +inf.0)      =>  #f
 *   (integer? -inf.0)       =>  #f
 * @end lisp
 *
doc>
 */
DEFINE_PRIMITIVE("number?", numberp, subr1, (SCM x))
{
  switch (TYPEOF (x)) {
    case tc_complex:
    case tc_real:
    case tc_rational:
    case tc_bignum:
    case tc_integer: return STk_true;
    default:         return STk_false;
  }
}


DEFINE_PRIMITIVE("complex?", complexp, subr1, (SCM x))
{
  return STk_numberp(x);
}


DEFINE_PRIMITIVE("real?", realp, subr1, (SCM x))
{
  switch (TYPEOF(x)) {
    case tc_complex:
      // a+0i is real; a+0.0i is not
      // return MAKE_BOOLEAN(STk_eq(COMPLEX_IMAG(x), MAKE_INT(0)) == STk_true);
      // Useless: if it's a complex, IMAG_PART is not #e0
      return STk_false;
    case tc_real:
    case tc_rational:
    case tc_bignum:
    case tc_integer: return STk_true;
    default:         return STk_false;
  }
}


DEFINE_PRIMITIVE("rational?", rationalp, subr1, (SCM x))
{
  switch (TYPEOF(x)) {
    case tc_real:    return MAKE_BOOLEAN(FINITE_REALP(x));
    case tc_rational:
    case tc_bignum:
    case tc_integer: return STk_true;
    default:         return STk_false;
  }
}


/*
<doc EXT bignum?
 * (bignum? x)
 *
 * This predicates returns |#t| if |x| is an integer number too large to be
 * represented with a native integer.
 * @lisp
 * (bignum? (expt 2 300))     => |#t|   ;; (very likely)
 * (bignum? 12)               => |#f|
 * (bignum? "no")             => |#f|
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("bignum?", bignump, subr1, (SCM x))
{
  return MAKE_BOOLEAN(BIGNUMP(x));
}


DEFINE_PRIMITIVE("integer?", integerp, subr1, (SCM x))
{
  switch (TYPEOF(x)){
    case tc_real:    {
                       double val = REAL_VAL(x);
                       return ((val == minus_inf) || (val == plus_inf)) ?
                                 STk_false:
                                 MAKE_BOOLEAN(floor(val) == val);
                     }
    case tc_bignum:
    case tc_integer: return STk_true;
    default:         return STk_false;
  }
}


/*
<doc  exact? inexact?
 * (exact? z)
 * (inexact? z)
 *
 * These numerical predicates provide tests for the exactness of a
 * quantity. For any Scheme number, precisely one of these predicates
 * is true.
doc>
 */

static int isexactp(SCM z)
{
  switch (TYPEOF(z)) {
    case tc_complex:  return isexactp(COMPLEX_REAL(z)) && isexactp(COMPLEX_IMAG(z));
    case tc_real:     return FALSE;
    case tc_rational:
    case tc_bignum:
    case tc_integer:  return TRUE;
    default:          error_bad_number(z);
  }
  return FALSE; /* never reached */
}


DEFINE_PRIMITIVE("exact?", exactp, subr1, (SCM z))
{
  return MAKE_BOOLEAN(isexactp(z));
}


DEFINE_PRIMITIVE("inexact?", inexactp, subr1, (SCM z))
{
  return MAKE_BOOLEAN(!isexactp(z));
}

/*
<doc EXT integer-length
 * (integer-length n)
 *
 * |Integer-length| returns the necessary number of bits to represent |n|
 * in 2's complement, assuming a leading 1 bit when |n| is negative. When
 * |n| is zero, the procedure returns zero.
 * This procedure works for any type of integer (fixnums and bignums).
 *
 * @lisp
 * (integer-length -3)            => 2
 * (integer-length -2)            => 1
 * (integer-length -1)            => 0
 * (integer-length 0)             => 0
 * (integer-length 1)             => 1
 * (integer-length 2)             => 2
 * (integer-length 3)             => 2
 * (integer-length (expt 2 5000)) => 5001
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("integer-length", integer_length, subr1, (SCM z))
{
   switch (TYPEOF(z)) {
    case tc_integer:{
      long n = INT_VAL(z);
      if (n == -1 || n == 0) return MAKE_INT(0);
      if (n>0)  return MAKE_INT( (long) log2( (float) n) + 1 ); /* n >  0 */
      return MAKE_INT( (long) log2( (float) labs(n+1) ) + 1 );  /* n < -1 */
    }
    case tc_bignum:  return MAKE_INT(mpz_sizeinbase(BIGNUM_VAL(z),2));

    default: STk_error ("bad integer ~S", z);
  }
  return STk_void; /* Never reached */
}

/*
<doc  = < > <= >=
 * (= z1 z2 z3 ...)
 * (< x1 x2 x3 ...)
 * (> x1 x2 x3 ...)
 * (<= x1 x2 x3 ...)
 * (>= x1 x2 x3 ...)
 *
 * These procedures return |#t| if their arguments are (respectively):
 * equal, monotonically increasing, monotonically decreasing,
 * monotonically nondecreasing, or monotonically nonincreasing, and
 * |#f| otherwise. If any of the arguments are +nan.0, all the predicates
 * return |#f|.
 * @lisp
 * (= +inf.0 +inf.0)           =>  #t
 * (= -inf.0 +inf.0)           =>  #f
 * (= -inf.0 -inf.0)           =>  #t
 * @end lisp
 *
 * For any finite real number x:
 *
 * @lisp
 * (< -inf.0 x +inf.0)         =>  #t
 * (> +inf.0 x -inf.0)         =>  #t
 * @end lisp
doc>
 */
#define COMPARE_NUM(_sname_, _cname_, _max_type_, _operator_)               \
    DEFINE_PRIMITIVE(_sname_, _cname_, vsubr, (int argc, SCM *argv))        \
    {                                                                       \
      SCM previous;                                                         \
                                                                            \
      if (argc == 0) error_at_least_1();                                    \
      if (_max_type_(*argv) == STk_false) error_not_a_real_number(*argv);   \
                                                                            \
      for (previous = *argv--; --argc; previous = *argv--) {                \
        if (_max_type_(*argv) == STk_false) error_bad_number(*argv);        \
        if (STk_isnan(*argv)) return STk_false;                             \
        if (do_compare(previous, *argv) _operator_ 0) return STk_false;     \
      }                                                                     \
      return STk_true;                                                      \
    }


#define COMPARE_NUM2(_prim_, _max_type_, _operator_)                        \
    long STk_##_prim_##2(SCM o1, SCM o2)                                    \
    {                                                                       \
      if (_max_type_(o1) == STk_false) error_not_a_real_number(o1);         \
      if (_max_type_(o2) == STk_false) error_not_a_real_number(o2);         \
      if (STk_isnan(o1) || STk_isnan(o2)) return 0;                         \
      return do_compare(o1, o2) _operator_ 0;                               \
    }


COMPARE_NUM("=",  numeq, STk_complexp, !=)
COMPARE_NUM("<",  numlt, STk_realp,    >=)
COMPARE_NUM(">",  numgt, STk_realp,    <=)
COMPARE_NUM("<=", numle, STk_realp,    >)
COMPARE_NUM(">=", numge, STk_realp,    <)


/* Version with only two parameters (used by runtime) STk_numeq2, STk_numgt2 ... */
COMPARE_NUM2(numeq,   STk_complexp, ==)
COMPARE_NUM2(numlt,   STk_realp,    <)
COMPARE_NUM2(numgt,   STk_realp,    >)
COMPARE_NUM2(numle,   STk_realp,    <=)
COMPARE_NUM2(numge,   STk_realp,    >=)


/*
<doc finite? infinite?  zero? positive? negative? odd? even?
 * (finite? z)
 * (infinite? z)
 * (zero? z)
 * (positive? x)
 * (negative? x)
 * (odd? n)
 * (even? n)
 *
 * These numerical predicates test a number for a particular property,
 * returning |#t| or |#f|.
 * @lisp
 * (positive? +inf.0)          ==>  #t
 * (negative? -inf.0)          ==>  #t
 * (finite? -inf.0)            ==>  #f
 * (infinite? +inf.0)          ==>  #t
 * @end lisp
doc>
 */

int STk_real_isoddp(SCM n)   /* n MUST be a real */
{
  SCM r = 0;

  integer_division(n, MAKE_INT(2), NULL, &r);
  /* We are sure here that r is a real */
  return (fpclassify(REAL_VAL(r)) != FP_ZERO);
}

static inline int number_parity(SCM n)
{
  /* result -1 (odd), 0 (non integer), +1 (even). Error if n is not a number. */
  switch (TYPEOF(n)) {
    case tc_integer:  return (INT_VAL(n) & 1)? -1: +1;
    case tc_bignum:   return mpz_odd_p(BIGNUM_VAL(n))? -1: +1;
    case tc_real:     {
                        double x = REAL_VAL(n);

                        if ((x == minus_inf) || (x == plus_inf) || (x != round(x)))
                          return 0;
                        else
                          return STk_real_isoddp(n) ? -1: +1;
                      }
    case tc_rational:
    case tc_complex:  return 0;
    default:          error_bad_number(n);
   }
  return 0;  /* for the compiler */
}

static int zerop(SCM n)
{
  switch (TYPEOF(n)) {
    case tc_integer:  return (INT_VAL(n) == 0);
    case tc_real:     return (fpclassify(REAL_VAL(n)) == FP_ZERO);
    case tc_bignum:   return (mpz_sgn(BIGNUM_VAL(n)) == 0);
    case tc_complex:  return zerop(COMPLEX_REAL(n)) && zerop(COMPLEX_IMAG(n));
    case tc_rational: return zerop(RATIONAL_NUM(n));
    default:          error_bad_number(n);
  }
  return FALSE; /* never reached */
}

static int positivep(SCM n)
{
  switch (TYPEOF(n)) {
    case tc_integer:  return (INT_VAL(n) > 0);
    case tc_real:     return (REAL_VAL(n) > 0.0);
    case tc_bignum:   return (mpz_sgn(BIGNUM_VAL(n)) > 0);
    case tc_rational: return positivep(RATIONAL_NUM(n));
    default:          error_not_a_real_number(n);
  }
  return FALSE; /* never reached */
}


static int negativep(SCM n)
{
  switch (TYPEOF(n)) {
    case tc_integer:  return (INT_VAL(n) < 0);
    case tc_real:     return (REAL_VAL(n) < 0.0);
    case tc_bignum:   return (mpz_sgn(BIGNUM_VAL(n)) < 0);
    case tc_rational: return negativep(RATIONAL_NUM(n));
    default:          error_not_a_real_number(n);
  }
  return FALSE; /* never reached */
}


static int finitep(SCM n)
{
  switch (TYPEOF(n)) {
    case tc_real:     return (FINITE_REALP(n));
    case tc_rational:
    case tc_bignum:
    case tc_integer:  return TRUE;
    case tc_complex:  return (finitep(COMPLEX_REAL(n)) &&
                              finitep(COMPLEX_IMAG(n)));
    default:          error_bad_number(n);
  }
  return FALSE; /* never reached */
}

static int infinitep(SCM n)
{
  switch (TYPEOF(n)) {
    case tc_real:     return (isinf(REAL_VAL(n)));
    case tc_rational:
    case tc_bignum:
    case tc_integer:  return FALSE;
    case tc_complex:  return (infinitep(COMPLEX_REAL(n)) ||
                              infinitep(COMPLEX_IMAG(n)));
    default:          error_bad_number(n);
  }
  return FALSE; /* never reached */
}

DEFINE_PRIMITIVE("finite?", finitep, subr1, (SCM n))
{
  return MAKE_BOOLEAN(finitep(n));
}


DEFINE_PRIMITIVE("infinite?", infinitep, subr1, (SCM n))
{
  return MAKE_BOOLEAN(infinitep(n));
}


DEFINE_PRIMITIVE("zero?", zerop, subr1, (SCM n))
{
  return MAKE_BOOLEAN(zerop(n));
}


DEFINE_PRIMITIVE("positive?", positivep, subr1, (SCM n))
{
  return MAKE_BOOLEAN(positivep(n));
}


DEFINE_PRIMITIVE("negative?", negativep, subr1, (SCM n))
{
  return MAKE_BOOLEAN(negativep(n));
}


DEFINE_PRIMITIVE("odd?", oddp, subr1, (SCM n))
{
  return MAKE_BOOLEAN(number_parity(n) < 0);
}


DEFINE_PRIMITIVE("even?", evenp, subr1, (SCM n))
{
  return MAKE_BOOLEAN(number_parity(n) >0);
}


/*
<doc R7RS nan?
 * (nan? z)
 *
 * The |nan?| procedure returns #t on |+nan.0|, and on complex
 * numbers if their real or imaginary parts or both are |+nan.0|.
 * Otherwise it returns #f.
 *
 * @lisp
 * (nan? +nan.0)          =>  #t
 * (nan? 32)              =>  #f
 * (nan? +nan.0+5.0i)     =>  #t
 * (nan? 1+2i)            =>  #f
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("nan?", nanp, subr1, (SCM z))
{
  return MAKE_BOOLEAN(STk_isnan(z));
}



/*
<doc  max min
 * (max x1 x2 ...)
 * (min x1 x2 ...)
 *
 * These procedures return the maximum or minimum of their arguments.
 *
 * @lisp
 * (max 3 4)              =>  4    ; exact
 * (max 3.9 4)            =>  4.0  ; inexact
 * @end lisp
 * For any real number x:
 * @lisp
 * (max +inf.0 x)         =>  +inf.0
 * (min -inf.0 x)         =>  -inf.0
 * @end lisp
 *
 * NOTE: If any argument is inexact, then the result will also be
 * inexact
doc>
 */
DEFINE_PRIMITIVE("max", max, vsubr, (int argc, SCM *argv))
{
  SCM res;
  int exactp;

  if (argc == 0) error_at_least_1();
  if (argc == 1) {
    if (STk_realp(*argv) == STk_true) return *argv;
    error_not_a_real_number(*argv);
  }

  exactp = isexactp(*argv);

  for (res = *argv--; --argc; argv--) {
    /* See that the argument is a correct number */
    if (STk_realp(*argv) == STk_false) error_not_a_real_number(*argv);

    /* determine if result should be exact or not */
    if (!isexactp(*argv)) exactp = 0;

    /* compute max */
    if (do_compare(res, *argv) < 0) res = *argv;
  }
  return (!exactp && isexactp(res)) ? exact2inexact(res) : res;
}


DEFINE_PRIMITIVE("min", min, vsubr, (int argc, SCM *argv))
{
  SCM res;
  int exactp;

  if (argc == 0) error_at_least_1();
  if (argc == 1) {
    if (STk_realp(*argv) == STk_true) return *argv;
    error_not_a_real_number(*argv);
  }

  exactp = isexactp(*argv);

  for (res = *argv--; --argc; argv--) {
    /* See that the argument is a correct number */
    if (STk_realp(*argv) == STk_false) error_not_a_real_number(*argv);

    /* determine if result should be exact or not */
    if (!isexactp(*argv)) exactp = 0;

    /* compute max */
    if (do_compare(res, *argv) > 0) res = *argv;
  }
  return (!exactp && isexactp(res)) ? exact2inexact(res) : res;
}


/*
<doc + *
 * (+ z1 ...)
 * (* z1 ...)
 *
 * These procedures return the sum or product of their arguments.
 * @lisp
 * (+ 3 4)                 =>  7
 * (+ 3)                   =>  3
 * (+)                     =>  0
 * (+ +inf.0 +inf.0)       =>  +inf.0
 * (+ +inf.0 -inf.0)       =>  +nan.0
 * (* 4)                   =>  4
 * (*)                     =>  1
 * (* 5 +inf.0)            =>  +inf.0
 * (* -5 +inf.0)           =>  -inf.0
 * (* +inf.0 +inf.0)       =>  +inf.0
 * (* +inf.0 -inf.0)       =>  -inf.0
 * (* 0 +inf.0)            =>  +nan.0
 * @end lisp
 * NOTE: For any finite number z:
 * @lisp
 *       (+ +inf.0 z)      =>  +inf.0
 *       (+ -inf.0 z)      =>  -inf.0
 * @end lisp
doc>
 */
SCM STk_add2(SCM o1, SCM o2)
{
  switch (convert(&o1, &o2)) {
    case tc_bignum:
        {
          mpz_t add;

          mpz_init(add);
          mpz_add(add, BIGNUM_VAL(o1), BIGNUM_VAL(o2));

          o1 = bignum2number(add);
          mpz_clear(add);
          break;
        }
      case tc_integer:
        {
          long add =  (long) INT_VAL(o1) + INT_VAL(o2);

          if (LONG_FITS_INTEGER(add))
            o1 = MAKE_INT(add);
          else
            o1 = long2scheme_bignum(add);
          break;
        }
      case tc_real:
        {
          o1 = double2real(REAL_VAL(o1) + REAL_VAL(o2));
          break;
        }
      case tc_complex:
        {
          o1 = make_complex(add2(COMPLEX_REAL(o1), COMPLEX_REAL(o2)),
                            add2(COMPLEX_IMAG(o1), COMPLEX_IMAG(o2)));
          break;
        }
      case tc_rational:
        {
          SCM num1, num2, den;

          den  = mul2(RATIONAL_DEN(o1), RATIONAL_DEN(o2));
          num1 = mul2(RATIONAL_NUM(o1), RATIONAL_DEN(o2));
          num2 = mul2(RATIONAL_NUM(o2), RATIONAL_DEN(o1));

          o1 = make_rational(add2(num1, num2), den);
          break;
        }
      default: error_cannot_operate("addition", o1, o2);
  }
  return o1;
}

DEFINE_PRIMITIVE("+", plus, vsubr, (int argc, SCM *argv))
{
  SCM res;

  if (argc == 0) return MAKE_INT(0);
  if (argc == 1) return add2(MAKE_INT(0), *argv);

  for (res = *argv--; --argc; argv--)
    res = add2(res, *argv);

  return res;
}


/***
 *** multiplication
 ***/
SCM STk_mul2(SCM o1, SCM o2)
{
  switch (convert(&o1, &o2)) {
    case tc_bignum:
      mult_bignum:
      {
        mpz_t prod;

        mpz_init(prod);
        mpz_mul(prod, BIGNUM_VAL(o1), BIGNUM_VAL(o2));

        o1 = bignum2number(prod);
        mpz_clear(prod);
        break;
      }
    case tc_integer:
      {
        long int i1 = INT_VAL(o1);
        long int i2 = INT_VAL(o2);

        o1 = MAKE_INT(i1*i2);
        if (i1 != 0 && (INT_VAL(o1) / i1) != i2) {
          o1 = long2scheme_bignum(i1);
          o2 = long2scheme_bignum(i2);
          goto mult_bignum;
        }
        break;
      }
      case tc_real:
        {
          o1 = double2real(REAL_VAL(o1) * REAL_VAL(o2));
          break;
        }
      case tc_complex:
        {
          SCM r1 = COMPLEX_REAL(o1);
          SCM i1 = COMPLEX_IMAG(o1);
          SCM r2 = COMPLEX_REAL(o2);
          SCM i2 = COMPLEX_IMAG(o2);

          /* Take care to complex numbers with a real or imaginary part which
           * is an infinite, when thy are multiplied by a non complex. In this
           * case, normal computation of the result will multiply an infinity
           * with zero, bringing a NaN in the result. In this case, we have
           * made a useless conversion (but should be pretty rare).
           */
          if ((i1 == MAKE_INT(0)) && (IS_INFP(r2) || IS_INFP(i2))) {
            // o1 was not a complex and o2 contains an infinite
            o1 = make_complex(mul2(r1,COMPLEX_REAL(o2)),
                              mul2(r1,COMPLEX_IMAG(o2)));
          } else if ((i2 == MAKE_INT(0)) && (IS_INFP(r1) || IS_INFP(i1))) {
            // o2 was not a complex and o1 contains an infinite
            o1 = make_complex(mul2(r2, COMPLEX_REAL(o1)),
                              mul2(r2,COMPLEX_IMAG(o1)));
          } else {
            // Normal case
            o1 = make_complex(sub2(mul2(r1,r2), mul2(i1, i2)),
                              add2(mul2(r1,i2), mul2(r2, i1)));
          }
          break;
        }
      case tc_rational:
        {
          o1 = make_rational(mul2(RATIONAL_NUM(o1), RATIONAL_NUM(o2)),
                             mul2(RATIONAL_DEN(o1), RATIONAL_DEN(o2)));
          break;
        }
      default: error_cannot_operate("multiplication", o1, o2);
  }
  return o1;
}

DEFINE_PRIMITIVE("*", multiplication, vsubr, (int argc, SCM *argv))
{
  SCM res;

  if (argc == 0) return MAKE_INT(1);
  if (argc == 1) return mul2(MAKE_INT(1), *argv);

  for (res = *argv--; --argc; argv--)
    res = mul2(res, *argv);

  return res;
}

/*
<doc   - /
 * (- z)
 * (- z1 z2)
 * (/ z)
 * (/ z1 z2 ...)
 *
 * With two or more arguments, these procedures return the difference or quotient
 * of their arguments, associating to the left. With one argument, however,
 * they return the additive or multiplicative inverse of their argument.
 *
 * @lisp
 * (- 3 4)                 =>  -1
 * (- 3 4 5)               =>  -6
 * (- 3)                   =>  -3
 * (- +inf.0 +inf.0)       => +nan.0
 * (/ 3 4 5)               =>  3/20
 * (/ 3)                   =>  1/3
 * (/ 0.0)                 => +inf.0
 * (/ -0.0)                => -inf.0
 * (- 0.0)                 => -0.0
 * (/ 0)                   => error (division by 0)
 * @end lisp
doc>
 */
SCM STk_sub2(SCM o1, SCM o2)
{
  /* Special case:
     (- 0.0) is calculated as (- 0 0.0) in turn should result in -0.0. */
  if (INTP(o1)  && INT_VAL(o1)==0 &&
      REALP(o2) && fpclassify(REAL_VAL(o2)) == FP_ZERO)
    return double2real(-REAL_VAL(o2));

  switch (convert(&o1, &o2)) {
    case tc_bignum:
      {
        mpz_t sub;

        mpz_init(sub);
        mpz_sub(sub, BIGNUM_VAL(o1), BIGNUM_VAL(o2));

        o1 = bignum2number(sub),
        mpz_clear(sub);
        break;
      }
    case tc_integer:
      {
        long sub = (long) INT_VAL(o1) - INT_VAL(o2);
        if (LONG_FITS_INTEGER(sub))
          o1 = MAKE_INT(sub);
        else
          o1 = long2scheme_bignum(sub);
        break;
      }
      case tc_real:
        {
          o1 = double2real(REAL_VAL(o1) - REAL_VAL(o2));
          break;
        }
      case tc_complex:
        {
          o1 = make_complex(sub2(COMPLEX_REAL(o1), COMPLEX_REAL(o2)),
                            sub2(COMPLEX_IMAG(o1), COMPLEX_IMAG(o2)));
          break;
        }
      case tc_rational:
        {
          SCM num1, num2, den;

          den  = mul2(RATIONAL_DEN(o1), RATIONAL_DEN(o2));
          num1 = mul2(RATIONAL_NUM(o1), RATIONAL_DEN(o2));
          num2 = mul2(RATIONAL_NUM(o2), RATIONAL_DEN(o1));

          o1 = make_rational(sub2(num1, num2), den);
          break;
        }
      default: error_cannot_operate("subtraction", o1, o2);
  }
  return o1;
}


DEFINE_PRIMITIVE("-", difference, vsubr, (int argc, SCM *argv))
{
  SCM res;

  if (argc == 0) error_at_least_1();
  if (argc == 1) return sub2(MAKE_INT(0), *argv);

  for (res = *argv-- ; --argc; argv--)
    res = sub2(res, *argv);
  return res;
}


/***
 ***   Division
 ***/
SCM STk_div2(SCM o1, SCM o2)
{
  switch (convert(&o1, &o2)) {
    case tc_bignum:
    case tc_integer:
      o1 = make_rational(o1, o2);
      break;

    case tc_real:
      {
        double r2 = REAL_VAL(o2);

        if (r2 != 1.0)
          o1 = double2real(REAL_VAL(o1) / r2);
        break;
      }

    case tc_rational:
      o1 =  make_rational(mul2(RATIONAL_NUM(o1), RATIONAL_DEN(o2)),
                          mul2(RATIONAL_DEN(o1), RATIONAL_NUM(o2)));
      break;

    case tc_complex:          /* See comment in STk_mul2 */
      if (IS_INFP(COMPLEX_REAL(o2)) || IS_INFP(COMPLEX_IMAG(o2))) {
        // o2 contains an infinite => result is 0.0+0.0i
        // FIXME: in fact, result can also be -0.0+0.0i, 0.0-0i
        // or -0.0-0.0i, but I don't know the rule
        o1 = make_complex(double2real(0.0), double2real(0.0));
      } else if ((COMPLEX_IMAG(o2) == MAKE_INT(0)) &&
                 (IS_INFP(COMPLEX_REAL(o1)) || IS_INFP(COMPLEX_IMAG(o1)))) {
        // o1 is a complex and o2 is not
        SCM r2 = COMPLEX_REAL(o2);
        o1 = make_complex(div2(COMPLEX_REAL(o1), r2),
                          div2(COMPLEX_IMAG(o1), r2));
      } else {
        SCM tmp, new_r, new_i;

        tmp   = add2(mul2(COMPLEX_REAL(o2), COMPLEX_REAL(o2)),
                     mul2(COMPLEX_IMAG(o2), COMPLEX_IMAG(o2)));
        new_r = div2(add2(mul2(COMPLEX_REAL(o1), COMPLEX_REAL(o2)),
                          mul2(COMPLEX_IMAG(o1), COMPLEX_IMAG(o2))),
                     tmp);
        new_i = div2(sub2(mul2(COMPLEX_IMAG(o1), COMPLEX_REAL(o2)),
                          mul2(COMPLEX_REAL(o1), COMPLEX_IMAG(o2))),
                     tmp);
        o1 = make_complex(new_r, new_i);
      }
      break;

    default: error_cannot_operate("division", o1, o2);
  }
  return o1;
}

DEFINE_PRIMITIVE("/", division, vsubr, (int argc, SCM *argv))
{
  SCM res;

  if (argc == 0) error_at_least_1();
  if (argc == 1) return div2(MAKE_INT(1), *argv);

  for (res = *argv--; --argc; argv--)
    res = div2(res, *argv);
  return res;
}


/*
<doc  abs
 * (abs z)
 *
 * |Abs| returns the absolute value of its argument.
 * @lisp
 * (abs -7)                =>  7
 * (abs -inf.0)            => +inf.0
 * (abs -3+4i)             => 5
 * (abs -3.0-4i)           => 5.0
 * @end lisp
 *
 * NOTE: {{stklos}} extends the {{rseven}} |abs| function, by allowing its
 * argument to be a complex number. In this case, |abs| returns the
 * _magnitude_ of its argument.
doc>
 */
DEFINE_PRIMITIVE("abs", abs, subr1, (SCM x))
{
  switch (TYPEOF(x)) {
    case tc_integer:  if (INT_VAL(x) == INT_MIN_VAL)
                        return long2scheme_bignum(-INT_VAL(x));
                      return (INT_VAL(x) < 0) ? MAKE_INT(-INT_VAL(x)) : x;
    case tc_bignum:   if (mpz_sgn(BIGNUM_VAL(x)) < 0) {
                        mpz_t tmp;

                        mpz_init(tmp);
                        mpz_neg(tmp, BIGNUM_VAL(x));
                        x = bignum2scheme_bignum(tmp);
                        mpz_clear(tmp);
                      }
                      return x;
    case tc_real:     return (REAL_VAL(x) < 0.0) ? double2real(-REAL_VAL(x)) : x;
    case tc_rational: return make_rational(absolute(RATIONAL_NUM(x)),
                                           RATIONAL_DEN(x));
    case tc_complex:  {
                        SCM r = COMPLEX_REAL(x);
                        SCM i = COMPLEX_IMAG(x);
                        return STk_sqrt(add2(mul2(r, r), mul2(i, i)));
                      }
    default:          error_not_a_real_number(x);
  }
  return STk_void;      /* never reached */
}


/*
<doc  quotient remainder modulo
 * (quotient n1 n2)
 * (remainder n1 n2)
 * (modulo n1 n2)
 *
 * These procedures implement number-theoretic (integer) division. n2 should
 * be non-zero. All three procedures return integers.
 * @l
 * If |n1/n2| is an integer:
 *
 * @lisp
 * (quotient n1 n2)   => n1/n2
 * (remainder n1 n2)  => 0
 * (modulo n1 n2)     => 0
 * @end lisp
 *
 * If n1/n2 is not an integer:
 *
 * @lisp
 * (quotient n1 n2)   => nq
 * (remainder n1 n2)  => nr
 * (modulo n1 n2)     => nm
 * @end lisp
 *
 * where |nq| is |n1/n2| rounded towards zero, 0 < abs(nr) < abs(n2),
 * 0 < abs(nm) < abs(n2), |nr| and |nm| differ from n1 by a multiple of n2,
 * |nr| has the same sign as n1, and |nm| has the same sign as n2.
 * @l
 * From this we can conclude that for integers |n1| and |n2| with |n2| not
 * equal to 0,
 * @lisp
 *  (= n1 (+ (* n2 (quotient n1 n2))
 *           (remainder n1 n2)))   =>  #t
 * @end lisp
 * provided all numbers involved in that computation are exact.
 *
 * @lisp
 * (modulo 13 4)           =>  1
 * (remainder 13 4)        =>  1
 *
 * (modulo -13 4)          =>  3
 * (remainder -13 4)       =>  -1
 *
 * (modulo 13 -4)          =>  -3
 * (remainder 13 -4)       =>  1
 *
 * (modulo -13 -4)         =>  -1
 * (remainder -13 -4)      =>  -1
 *
 * (remainder -13 -4.0)    =>  -1.0  ; inexact
 * @end lisp
doc>
 */
static void int_divide(SCM x, SCM y, SCM *quotient, SCM* remainder, int exact)
{
#ifdef STK_DEBUG
  if (!quotient && !remainder)
    STk_panic("integer_division called with no quotient nor reminder pointers");
#endif
  /* Here, x and y can only be integer or bignum (not real) */

  /* NOTE about the remainder: the GMP accepts 'unsigned integers' and
     also returns 'unsigned integers' for remainders. We can safely use
     'long' for these remainders, AND these will always fit a fixnum,
     because we'll only receive them when we passed fixnums as arguments,
     and the remainder won't be larger. */

  long quo;              /* temp var for long quotient */
  long rem = 0;          /* temp var for long remainder */
  mpz_t q;
  mpz_t r;
  int big_q = 0;         /* Is the result quotient a bignum? */
  int big_r = 0;         /* Is the result remainder a bignum? */

  /* The following sequence of "if"s is written for clarity. Even though
     it may look like we call INTP more times than necessary, the compiler
     will later optimize the logic here. */

  /* FIXNUM - FIXNUM */
  if (INTP(x) && INTP(y)) { // NOTE: at least one of x or y was originally inexact
    long int i1 = INT_VAL(x);
    long int i2 = INT_VAL(y);

    if (quotient)  quo = i1 / i2;
    if (remainder) rem = i1 % i2;

  /* FIXNUM - BIGNUM */
  } else if (INTP(x)) {
      /* STklos never stores fixnums as if they were bignums, so
         we're sure that the quotient for INT/BIG = zero. */
      if (quotient)  quo = 0;
      if (remainder) rem = INT_VAL(x);

  /* BIGNUM - FIXNUM */
  } else  if (INTP(y)) {
      if (quotient) mpz_init(q);
      /* The GMP only returns unsigned remainders, so we need to keep track of the
         sign of x. The easiest way to put back the sign is to initialize rem with
         it, and multiply by whatever the GMP returns.
         Also, we need 'labs' so the GMP will get the expected ulong. */
      long xsign = mpz_sgn(BIGNUM_VAL(x));
      if (!quotient) rem = xsign * (long) mpz_tdiv_ui(BIGNUM_VAL(x), labs(INT_VAL(y)));
      else rem = xsign * (long) mpz_tdiv_q_ui(q, BIGNUM_VAL(x),
                                              labs(INT_VAL(y))); /* rem may or may not be used */
      if (quotient && (INT_VAL(y) < 0)) mpz_neg(q,q); /* Put back the sign of y */
      big_q = 1;

  /* BIGNUM - BIGNUM */
  } else {
    if (quotient)  mpz_init(q);
    if (remainder) mpz_init(r);
    /* mpz_cmpabs seems fast enough to not make much of a difference,
       so we can call it here -- and it makes the dividing SMALL / BIG
       and N / N cases much faster (I wonder why the GMP doesn't do
       this internally): */
    int cmp = mpz_cmpabs(BIGNUM_VAL(x), BIGNUM_VAL(y));
    if (cmp < 0) {
      /* SMALL / BIG -> quotient 0, remainder x */
      if (quotient)  quo = 0;
      if (remainder) mpz_set(r, BIGNUM_VAL(x));
      big_r = 1;
    } else if (cmp == 0) {
      /* (+-)N / (+-)N -> quotient (signX * signY), remainder 0 */
      long xsign = mpz_sgn(BIGNUM_VAL(x));
      long ysign = mpz_sgn(BIGNUM_VAL(x));
      if (quotient)  quo = xsign * ysign;
      if (remainder) rem = 0;
    } else {
      /* BIG / SMALL -> call a division GMP function */
      if (!quotient)        mpz_tdiv_r(r, BIGNUM_VAL(x), BIGNUM_VAL(y));
      else if (!remainder)  mpz_tdiv_q(q, BIGNUM_VAL(x), BIGNUM_VAL(y));
      else                  mpz_tdiv_qr(q, r, BIGNUM_VAL(x), BIGNUM_VAL(y));
      big_q = big_r = 1;
    }
  }
  /*** END OF CASES ***/

  /* Check exactness and set the result values */
  if (exact) {
      if (quotient)  *quotient  = big_q ? bignum2number(q) : long2integer(quo);
      if (remainder) *remainder = big_r ? bignum2number(r) : long2integer(rem);
  } else {
      if (quotient)  *quotient  = big_q ? double2real(bignum2double(q)) :
                                          double2real((double) quo);
      if (remainder) *remainder = big_r ? double2real(bignum2double(r)) :
                                          double2real((double) rem);
  }
  if (quotient  && big_q) mpz_clear(q);
  if (remainder && big_r) mpz_clear(r);
}

static void integer_division(SCM x, SCM y, SCM *quotient, SCM* remainder)
{
  int exact = 1;

  if (!INTP(x) && !BIGNUMP(x) && !REALP(x)) error_bad_number(x);
  if (!INTP(y) && !BIGNUMP(y) && !REALP(y)) error_bad_number(y);
  if (zerop(y))                             error_divide_by_0(x);

  if (INTP(x) && INTP(y)) {
    /* Fast path for the division of two fixnums */
    long int i1 = INT_VAL(x);
    long int i2 = INT_VAL(y);
    if (quotient)  *quotient  = long2integer(i1 / i2);
    if (remainder) *remainder = long2integer(i1 % i2);
  } else {
    /* General case */
    if (REALP(x)) { x = real2integer(x); exact = 0; }
    if (REALP(y)) { y = real2integer(y); exact = 0; }
    int_divide(x, y, quotient, remainder, exact);
  }
}

DEFINE_PRIMITIVE("quotient", quotient, subr2, (SCM n1, SCM n2))
{
  SCM q = 0;

  integer_division(n1, n2, &q, NULL);
  return q;
}


DEFINE_PRIMITIVE("remainder", remainder, subr2, (SCM n1, SCM n2))
{
  SCM r = 0;

  integer_division(n1, n2, NULL, &r);
  return r;
}


DEFINE_PRIMITIVE("modulo", modulo, subr2, (SCM n1, SCM n2))
{
  SCM r = 0;

  integer_division(n1, n2, NULL, &r);
  if (negativep(n1) != negativep(n2) && !zerop(r))
     /*kerch@parc.xerox.com*/
    r = add2(r, n2);
  return r;
}


/*
<doc  gcd lcm
 * (gcd n1 ...)
 * (lcm n1 ...)
 *
 * These procedures return the greatest common divisor or least common
 * multiple of their arguments. The result is always non-negative.
 *
 * @lisp
 * (gcd 32 -36)            =>  4
 * (gcd)                   =>  0
 * (lcm 32 -36)            =>  288
 * (lcm 32.0 -36)          =>  288.0  ; inexact
 * (lcm)                   =>  1
 * @end lisp
doc>
 */

static SCM gcd2_fixnum(SCM n1, SCM n2) /* special version for fixnums */
{
  long l1 = INT_VAL(n1);
  long l2 = INT_VAL(n2);

  if (l1 < 0) l1 = -l1;
  if (l2 < 0) l2 = -l2;

  while (l2) {
    long tmp = l1;
    l1 = l2;
    l2 = tmp % l2;
  }
  return MAKE_INT(l1);
}



static SCM gcd2(SCM n1, SCM n2)
{
  int exactp = 1;

  if (STk_integerp(n1) == STk_false) error_not_an_integer(n1);
  if (STk_integerp(n2) == STk_false) error_not_an_integer(n2);

  if (REALP(n1)) {
    n1 = inexact2exact(n1);
    exactp = 0;
  }
  if (REALP(n2)) {
    n2 = inexact2exact(n2);
    exactp = 0;
  }

  /* In the specific case we have bignums, GMP is absolutely faster than
   * doing it ourselves. So try to use specialized GMP functions in this
   * case (and use a simple algorithm with C longs, otherwise)
   */
  if (INTP(n1) && INTP(n2)) {
    SCM res = gcd2_fixnum(n1, n2);
    return exactp ? res: exact2inexact(res);
  }
  else {
    /* COMPUTE THE GCD WITH AT LEAST ONE BIGNUM
     * Three cases:
     * - fixnum - bignum
     * - bignum - fixnum
     * - bignum - bignum
     */
    mpz_t r;
    mpz_init_set_si(r,0);

    if (BIGNUMP(n1) && INTP(n2)) /* n1:BIG n2:FIX */
      /* GMP requires an unsigned long for the second arg
         (there's no "si" version for this function) -- so
         we need to call labs(). */
      mpz_gcd_ui(r, BIGNUM_VAL(n1), labs(INT_VAL(n2)));
    else if (INTP(n1) && BIGNUMP(n2)) /* n1:FIX n2:BIG */
      mpz_gcd_ui(r, BIGNUM_VAL(n2), labs(INT_VAL(n1)));
    else if (BIGNUMP(n1) && BIGNUMP(n2)) /*  n1:BIG n2:BIG */
      mpz_gcd(r, BIGNUM_VAL(n1), BIGNUM_VAL(n2));

    /* NOTE: we are sure to not have here a NaN or an infinity since
     * at most r is equal to n1 or n2, which has been accepted by
     * predicate integer? when entering this function
     */
    SCM res = (exactp) ? bignum2number(r): double2real(bignum2double(r));
    mpz_clear(r);
    return res;
  }
}


DEFINE_PRIMITIVE("gcd", gcd, vsubr, (int argc, SCM *argv))
{
  SCM res;

  if (argc == 0) return MAKE_INT(0);
  if (argc == 1) return absolute(gcd2(*argv, MAKE_INT(0)));

  for (res = *argv--; --argc; argv--)
    res = gcd2(res, *argv);

  return absolute(res);
}

DEFINE_PRIMITIVE("lcm", lcm, vsubr, (int argc, SCM *argv))
{
  SCM res, gcd;

  if (argc == 0) return MAKE_INT(1);
  if (STk_numberp(*argv) == STk_false) error_bad_number(*argv);

  for (res = *argv--; --argc; argv--) {
    gcd = gcd2(res, *argv);
    res = mul2(res,div2(*argv, gcd));
  }
  return absolute(res);
}

/*
<doc  numerator denominator
 * (numerator q)
 * (denominator q)
 *
 * These procedures return the numerator or denominator of their argument; the
 * result is computed as if the argument was represented as a fraction in
 * lowest terms. The denominator is always positive. The denominator of
 * 0 is defined to be 1.
 * @lisp
 * (numerator (/ 6 4))  =>  3
 * (denominator (/ 6 4))  =>  2
 * (denominator
 * (exact->inexact (/ 6 4))) => 2.0
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("numerator", numerator, subr1, (SCM q))
{
  switch (TYPEOF(q)) {
    case tc_real:     return
                        exact2inexact(STk_numerator(inexact2exact(q)));
    case tc_rational: return RATIONAL_NUM(q);
    case tc_bignum:
    case tc_integer:  return q;
    default:          error_bad_number(q);
  }
  return STk_void; /* never reached */
}

DEFINE_PRIMITIVE("denominator", denominator, subr1, (SCM q))
{
  switch (TYPEOF(q)) {
    case tc_real:     return exact2inexact(STk_denominator(inexact2exact(q)));
    case tc_rational: return RATIONAL_DEN(q);
    case tc_bignum:
    case tc_integer:  return MAKE_INT(1);
    default:          error_bad_number(q);
  }
  return STk_void; /* never reached */
}

/*
<doc  floor ceiling truncate round
 * (floor x)
 * (ceiling x)
 * (truncate x)
 * (round x)
 *
 * These procedures return integers. |Floor| returns the largest integer not
 * larger than |x|. |Ceiling| returns the smallest integer not smaller than |x|.
 * |Truncate| returns the integer closest to |x| whose absolute value is not
 * larger than the absolute value of |x|. |Round| returns the closest integer
 * to |x|, rounding to even when |x| is halfway between two integers.
 * @l
 * IMPORTANT: |Round| rounds to even for consistency with the default
 * rounding mode specified by the IEEE floating point standard.
 * @l
 * NOTE: If the argument to one of these procedures is inexact, then the
 * result will also be inexact. If an exact value is needed, the result should
 * be passed to the |inexact->exact| procedure.
 *
 * @lisp
 *
 * (floor -4.3)          =>  -5.0
 * (ceiling -4.3)        =>  -4.0
 * (truncate -4.3)       =>  -4.0
 * (round -4.3)          =>  -4.0
 *
 * (floor 3.5)           =>  3.0
 * (ceiling 3.5)         =>  4.0
 * (truncate 3.5)        =>  3.0
 * (round 3.5)           =>  4.0  ; inexact
 *
 * (round 7/2)           =>  4    ; exact
 * (round 7)             =>  7
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("floor", floor, subr1, (SCM x))
{
  switch (TYPEOF(x)) {
    case tc_real:     return double2real(floor(REAL_VAL(x)));
    case tc_rational: {
                        SCM tmp;

                        tmp = negativep(RATIONAL_NUM(x)) ?
                                 sub2(RATIONAL_NUM(x),
                                      sub2(RATIONAL_DEN(x), MAKE_INT(1))):
                                 RATIONAL_NUM(x);
                        return int_quotient(tmp, RATIONAL_DEN(x));
                      }
    case tc_bignum:
    case tc_integer:  return x;
    default:          error_not_a_real_number(x);
  }
  return STk_void; /* never reached */
}

DEFINE_PRIMITIVE("ceiling", ceiling, subr1, (SCM x))
{
  switch (TYPEOF(x)) {
    case tc_real:     return double2real(ceil(REAL_VAL(x)));
    case tc_rational: {
                        SCM tmp;

                        tmp = negativep(RATIONAL_NUM(x))?
                                RATIONAL_NUM(x) :
                                add2(RATIONAL_NUM(x),
                                     sub2(RATIONAL_DEN(x), MAKE_INT(1)));
                        return int_quotient(tmp, RATIONAL_DEN(x));
                      }
    case tc_bignum:
    case tc_integer:  return x;
    default:          error_not_a_real_number(x);
  }
  return STk_void; /* never reached */
}


DEFINE_PRIMITIVE("truncate", truncate, subr1, (SCM x))
{
  switch (TYPEOF(x)) {
    case tc_real:     {
                        double d = REAL_VAL(x);
                        return double2real(d < 0.0 ? ceil(d): floor(d));
                      }
    case tc_rational: return int_quotient(RATIONAL_NUM(x), RATIONAL_DEN(x));
    case tc_bignum:
    case tc_integer:  return x;
    default:          error_not_a_real_number(x);
  }
  return STk_void; /* never reached */
}


DEFINE_PRIMITIVE("round", round, subr1, (SCM x))
{
  switch (TYPEOF(x)) {
    case tc_real:     {
                        double res, d= REAL_VAL(x) + 0.5;

                        res = floor(d);
                        if (d == res && d/2 != floor(d/2)) res -= 1;
                        return double2real(res);
                      }
    case tc_rational: {
                        SCM q, r;
                        integer_division(RATIONAL_NUM(x), RATIONAL_DEN(x), &q, &r);
                        /* abs(r) is between 0 and denom-1,
                           so we can compare if r/2 <= denom. Or
                           if 2 <= 2denom. */
                        if (STk_numge2(mul2(absolute(r),MAKE_INT(2)),
                                       RATIONAL_DEN(x)))
                            return negativep(RATIONAL_NUM(x))
                                ? sub2(q, MAKE_INT(1))
                                : add2(q, MAKE_INT(1));
                        else
                            return q;
                      }
    case tc_bignum:
    case tc_integer:  return x;
    default:          error_not_a_real_number(x);
  }
  return STk_void; /* never reached */
}

/* ============== TRANSCENDENTALS */


#define transcendental(name)                            \
  DEFINE_PRIMITIVE(#name, name, subr1, (SCM z))         \
  {                                                     \
     return my_##name(z);                               \
  }


/*
<doc  exp log sin cos tan asin acos atan
 * (exp z)
 * (log z)
 * (log z b)
 * (sin z)
 * (cos z)
 * (tan z)
 * (asin z)
 * (acos z)
 * (atan z)
 * (atan y x)
 *
 * These procedures compute the usual transcendental functions. |Log| computes the
 * natural logarithm of z (not the base ten logarithm). |Asin|, |acos|,
 * and |atan| compute arcsine, arccosine, and  arctangent, respectively.
 * The two-argument variant of |log| computes the logarithm of x in base b as
 * @lisp
 * (/ (log x) (log b))
 * @end lisp
 * The two-argument variant of |atan| computes
 * @lisp
 * (angle (make-rectangular x y))
 * @end lisp
 *
 * When it is possible these procedures produce a real result from a real
 * argument.
doc>
 */
static SCM my_exp(SCM z)
{
  switch (TYPEOF(z)) {
    case tc_integer:  if (z == MAKE_INT(0)) return MAKE_INT(1);
                      return double2real(exp(INT_VAL(z)));
    case tc_bignum:   return double2real(exp(scheme_bignum2double(z)));
    case tc_rational: return double2real(exp(rational2double(z)));
    case tc_real:     return double2real(exp(REAL_VAL(z)));
    case tc_complex:  return make_polar(my_exp(COMPLEX_REAL(z)),
                                        COMPLEX_IMAG(z));
    default:          error_bad_number(z);
  }
   return STk_void; /* never reached */
}


/*****************************************************************************
  The code for calculating logarithms of bignums uses mpz_get_d_2exp,
  which is available in the large GMP but not in the mini GMP. So, if
  we are using the mini GMP, we provide limited support for logs of
  bignums (if the bignum doesn't fit a double, +inf.0 will be
  returned).

   For that, we compile conditionally here
 *****************************************************************************/

#ifdef __MINI_GMP_H__ /* BEGIN code for compiling WITH MINI GMP */

static inline double my_bignum_log(SCM z) {
  /* Will return +inf.0, except for the bignums that fit
     a double: */
  return log(scheme_bignum2double(z));
}
static inline double my_bignum_rational_log(SCM z) {
  /* Will return +inf.0, except for the bignums that fit
     a double: */
  return log(rational2double(z));
}

#else  /* BEGIN code for compiling WITH FULL GMP */

static double my_bignum_log(SCM z) {
  /* The GMP function mpz_get_d_2exp is similar to the C library
     function frexp: it returns a float D between 0.5 and 1.0,
     and sets an exponent E. Then,
     N = D * 2^E
     is the (possibly truncated) value of the original number.
     Then calculating the log is simple:

     log(N) = log(D*2^E)
            = log(D) + log(2^E)
            = log(D) + log(2) * E                               */
  long   expo;
  double d = mpz_get_d_2exp(&expo, BIGNUM_VAL(z));
  return log(d) + log(2) * (double) expo;
}


static SCM my_log(SCM z); /* Forward declaration needed, because my_bignum_rational_log
                             and my_log are mutually recursive */
static double my_bignum_rational_log(SCM z) {
  double log_num, log_den;

  SCM num = RATIONAL_NUM(z);
  SCM den = RATIONAL_DEN(z);

  /* For both the numerator and denominator:

     - If it is a bignum AND fits a double, then just
       convert to double and take the log.
     - If it is a bignum and does NOT fit a double,
       use my_bignum_log.
     - It may be that only one of numerator or denominator
       is a bignum. Then the other won't be, and we just
       take the log of it with my_log.                     */
  if (BIGNUMP(num))
    if (BIGNUM_FITS_INTEGER(BIGNUM_VAL(num)))
      log_num = log(scheme_bignum2double(num));
    else
      log_num = my_bignum_log(num);
  else
    log_num = STk_number2double(my_log(num));

  if (BIGNUMP(den))
    if (BIGNUM_FITS_INTEGER(BIGNUM_VAL(den)))
      log_den = log(scheme_bignum2double(den));
    else
      log_den = my_bignum_log(den);
  else
    log_den = STk_number2double(my_log(den));

  /* Now use log(a/b) = log(a) - log(b): */
  return log_num - log_den;
}
#endif /* __MINI_GMP_H__ */

static SCM my_log(SCM z)
{
  if (!COMPLEXP(z) && negativep(z) && finitep(z))
    return make_complex(my_log(sub2(MAKE_INT(0), z)), double2real(MY_PI));


  switch (TYPEOF(z)) {
    case tc_integer:  if (z == MAKE_INT(0)) STk_error("value is not defined for 0");
                      if (z == MAKE_INT(1)) return MAKE_INT(0);
                      return double2real(log((double) INT_VAL(z)));
    case tc_bignum:   if (BIGNUM_FITS_INTEGER(BIGNUM_VAL(z)))
                        return double2real(log(scheme_bignum2double(z)));
                      else
                        return double2real(my_bignum_log(z));
    case tc_rational: if (!BIGNUMP(RATIONAL_NUM(z)) &&
                          !BIGNUMP(RATIONAL_DEN(z)))
                        return double2real(log(rational2double(z)));
                      else
                        return double2real(my_bignum_rational_log(z));
    case tc_real:     if ( (REAL_VAL(z) == 0.0) && signbit(REAL_VAL(z)) )
                          return make_complex(double2real(minus_inf), double2real(MY_PI));
                      else if ( isinf(REAL_VAL(z)) && signbit(REAL_VAL(z)) )
                          return make_complex(double2real(plus_inf),  double2real(MY_PI));
                      else
                          return double2real(log(REAL_VAL(z)));
    case tc_complex:  return make_complex(my_log(absolute(z)), STk_angle(z));
    default:          error_bad_number(z);
  }
  return STk_void; /* never reached */
}


static SCM my_cos(SCM z)
{
  switch (TYPEOF(z)) {
    case tc_integer:  if (z == MAKE_INT(0)) return MAKE_INT(1);
                      return double2real(cos(INT_VAL(z)));
    case tc_bignum:   return double2real(cos(scheme_bignum2double(z)));
    case tc_rational: return double2real(cos(rational2double(z)));
    case tc_real:     return double2real(cos(REAL_VAL(z)));
    case tc_complex:  return
                        div2(add2(my_exp(make_complex(sub2(MAKE_INT(0),
                                                           COMPLEX_IMAG(z)),
                                                      COMPLEX_REAL(z))),
                                  my_exp(make_complex(COMPLEX_IMAG(z),
                                                      sub2(MAKE_INT(0),
                                                           COMPLEX_REAL(z))))),
                             MAKE_INT(2));
    default:          error_bad_number(z);
  }
  return STk_void; /* never reached */
}


static SCM my_sin(SCM z)
{
  switch (TYPEOF(z)) {
    case tc_integer:  if (z == MAKE_INT(0)) return MAKE_INT(0);
                      return double2real(sin(INT_VAL(z)));
    case tc_bignum:   return double2real(sin(scheme_bignum2double(z)));
    case tc_rational: return double2real(sin(rational2double(z)));
    case tc_real:     return double2real(sin(REAL_VAL(z)));
    case tc_complex:  return
                        div2(sub2(my_exp(make_complex(sub2(MAKE_INT(0),
                                                           COMPLEX_IMAG(z)),
                                                      COMPLEX_REAL(z))),
                                  my_exp(make_complex(COMPLEX_IMAG(z),
                                                      sub2(MAKE_INT(0),
                                                           COMPLEX_REAL(z))))),
                             Cmake_complex(MAKE_INT(0), MAKE_INT(2)));
    default:          error_bad_number(z);
  }
  return STk_void; /* never reached */
}

static SCM my_tan(SCM z)
{
  switch (TYPEOF(z)) {
    case tc_integer:  if (z == MAKE_INT(0)) return MAKE_INT(0);
                      return double2real(tan(INT_VAL(z)));
    case tc_bignum:   return double2real(tan(scheme_bignum2double(z)));
    case tc_rational: return double2real(tan(rational2double(z)));
    case tc_real:     return double2real(tan(REAL_VAL(z)));
    case tc_complex:  {
                        SCM a = my_exp(make_complex(sub2(MAKE_INT(0),
                                                         COMPLEX_IMAG(z)),
                                                    COMPLEX_REAL(z)));
                        SCM b = my_exp(make_complex(COMPLEX_IMAG(z),
                                                    sub2(MAKE_INT(0),
                                                         COMPLEX_REAL(z))));
                        SCM c;

                        c = div2(sub2(a, b), add2(a,b));
                        return COMPLEXP(c) ?
                                  make_complex(COMPLEX_IMAG(c),
                                               sub2(MAKE_INT(0), COMPLEX_REAL(c))):
                                  make_complex(MAKE_INT(0),
                                               sub2(MAKE_INT(0), c));
                      }
    default:          error_bad_number(z);
  }
  return STk_void; /* never reached */
}


static SCM asin_complex(SCM z)
{
  return mul2(Cmake_complex(MAKE_INT(0), MAKE_INT(-1UL)),                  /* -i */
              my_log(add2(mul2(Cmake_complex(MAKE_INT(0), MAKE_INT(+1UL)), /* +i */
                               z),
                          STk_sqrt(sub2(MAKE_INT(1),
                                        mul2(z, z))))));
}

static SCM asin_real(double d)
{
  if (d < -1)
    return sub2(MAKE_INT(0), asin_real(-d));
  if (d > 1)
    return mul2(Cmake_complex(MAKE_INT(0), MAKE_INT(-1UL)),
                my_log(add2(mul2(Cmake_complex(MAKE_INT(0), MAKE_INT(1UL)),
                                 double2real(d)),
                            STk_sqrt(double2real(1 - d*d)))));
  return double2real(asin(d));
}


static SCM my_asin(SCM z)
{
  switch (TYPEOF(z)) {
    case tc_integer:  if (z == MAKE_INT(0)) return MAKE_INT(0);
                      return asin_real(INT_VAL(z));
    case tc_bignum:   return asin_real(scheme_bignum2double(z));
    case tc_rational: return asin_real(rational2double(z));
    case tc_real:     return asin_real(REAL_VAL(z));
    case tc_complex:  {
                        SCM imag = COMPLEX_IMAG(z);

                        if ((positivep(imag)) ||
                            (REALP(imag)&&(imag==0) && negativep(COMPLEX_REAL(z))))
                          return sub2(MAKE_INT(0),
                                      asin_complex(sub2(MAKE_INT(0), z)));
                        return asin_complex(z);
                      }
    default:          error_bad_number(z);
  }
  return STk_void; /* never reached */
}


static inline SCM acos_complex(SCM z)
{
  return mul2(Cmake_complex(MAKE_INT(0), MAKE_INT(-1UL)),
              my_log(add2(z,
                          mul2(Cmake_complex(MAKE_INT(0), MAKE_INT(1UL)),
                               STk_sqrt(sub2(MAKE_INT(1UL),
                                             mul2(z, z)))))));
}

static SCM acos_real(double d)
{
  return (-1 <= d && d <= 1) ? double2real(acos(d)) : acos_complex(double2real(d));
}



static SCM my_acos(SCM z)
{
  switch (TYPEOF(z)) {
   case tc_integer:   if (z == MAKE_INT(1)) return MAKE_INT(0);
                      return acos_real(INT_VAL(z));
    case tc_bignum:   return acos_real(scheme_bignum2double(z));
    case tc_rational: return acos_real(rational2double(z));
    case tc_real:     return acos_real(REAL_VAL(z));
    case tc_complex:  return acos_complex(z);
    default:          error_bad_number(z);
  }
  return STk_void; /* never reached */
}


static SCM my_atan(SCM z)
{
  switch (TYPEOF(z)) {
    case tc_integer:  if (z == MAKE_INT(0)) return MAKE_INT(0);
                      return double2real(atan(INT_VAL(z)));
    case tc_bignum:   return double2real(atan(scheme_bignum2double(z)));
    case tc_rational: return double2real(atan(rational2double(z)));
    case tc_real:     return double2real(atan(REAL_VAL(z)));
    case tc_complex:  {
                        SCM i = COMPLEX_REAL(z);
                        SCM r = COMPLEX_IMAG(z);
                        SCM a;

                        if ((r == MAKE_INT(1)) && (zerop(i)))
                          error_out_of_range(z);
                        a = STk_make_rectangular(sub2(MAKE_INT(0), r), i);
                        return div2(sub2(my_log(add2(a, MAKE_INT(1))),
                                         my_log(sub2(MAKE_INT(1), a))),
                                    Cmake_complex(MAKE_INT(0), MAKE_INT(2)));
                      }
    default:          error_bad_number(z);
  }
  return STk_void; /* never reached */
}

static SCM my_atan2(SCM y, SCM x)
{
  if (STk_realp(y) == STk_false) error_bad_number(y);
  if (STk_realp(x) == STk_false) error_bad_number(x);
  if (x == MAKE_INT(0))
    /* For (atan y 0), if y is y is inexact zero, the result is either
       -pi/2 or +pi/2, according to R7RS (there is a table in the standard
       with several specific cases, including these). */
    if (REALP(y)) {
      double yval = REAL_VAL(y);
      if (yval ==  0.0)
        return signbit(yval)
            ? double2real( - MY_PI / 2)
            : double2real( + MY_PI / 2);
    }
    /* Angle for 0+0i is not defined: */
    else if (y == MAKE_INT(0)) return double2real(STk_NaN);
  return double2real(atan2(REAL_VAL(exact2inexact(STk_real_part(y))),
                           REAL_VAL(exact2inexact(STk_real_part(x)))));
}

transcendental(exp)
transcendental(sin)
transcendental(cos)
transcendental(tan)
transcendental(asin)
transcendental(acos)


/* ========== HYPERBOLIC */

/*
<doc EXT sinh asinh cosh acosh tanh atanh
 * (sinh z)
 * (cosh z)
 * (tanh z)
 * (asinh z)
 * (acosh z)
 * (atanh z)
 *
 * These procedures compute the hyperbolic trigonometric functions.
 * @lisp
 * (sinh 1)     => 1.1752011936438
 * (sinh 0+1i)  => 0.0+0.841470984807897i
 * (cosh 1)     => 1.54308063481524
 * (cosh 0+1i)  => 0.54030230586814
 * (tanh 1)     => 0.761594155955765
 * (tanh 0+1i)  => 0.0+1.5574077246549i
 * (asinh 1)    => 0.881373587019543
 * (asinh 0+1i) => 0+1.5707963267949i
 * (acosh 0)    => 0+1.5707963267949i
 * (acosh 0+1i) => 1.23340311751122+1.5707963267949i
 * (atanh 1)    => error
 * (atanh 0+1i) => 0.0+0.785398163397448i
 * @end lisp
 *
 * In general, |(asinh (sinh x))| and similar compositions should be
 * equal to |x|, except for inexactness due to the internal floating
 * point number approximation for real numbers.
 * @lisp
 * (sinh (asinh 0+1i)) => 0.0+1.0i
 * (cosh (acosh 0+1i)) => 8.65956056235493e-17+1.0i
 * (tanh (atanh 0+1i)) => 0.0+1.0i
 * @end lisp
 *
 * These functions will always return an exact result for the following
 * arguments:
 * @lisp
 * (sinh 0.0)     => 0
 * (cosh 0.0)     => 1
 * (tanh 0.0)     => 0
 * (asinh 0.0)    => 0
 * (acosh 1.0)    => 0
 * (atanh 0.0)    => 0
 * @end lisp
doc>
*/

static SCM my_cosh(SCM z)
{
  switch (TYPEOF(z)) {
  /* 1. fastest for reals/fixnums is to use C 'cosh'.

     2. cosh(z) = [  1 + exp(-2x)  ] /  2exp(-x)
                = [ exp(x) + exp(-x) ] / 2
        faster for the rest.

     3. cosh(z) = cos(i z),
        but it's always slow. (not used) */
  case tc_real:     if (fpclassify(REAL_VAL(z)) == FP_ZERO) return MAKE_INT(1);
                    return double2real(cosh(REAL_VAL(z)));
  case tc_integer:  if (INT_VAL(z) == 0) return MAKE_INT(1);
                    return double2real(cosh(INT_VAL(z)));
  case tc_complex:
  case tc_bignum:
  case tc_rational: {
      SCM ez = my_exp(z);
      SCM inv_ez = div2 (MAKE_INT(1), ez);
      return div2(add2(ez,inv_ez),
                  double2real(2.0));
  }
  default:          error_bad_number(z);
  }
  return STk_void; // for the compiler
}

static SCM my_sinh(SCM z)
{
  /* 1. fastest for reals/fixnums is to use C 'sinh'.

     2. cosh(z) = [  1 - exp(-2x)  ] /  2exp(-x)
                = [ exp(x) - exp(-x) ] / 2
        (faster for all but reals/fixnums)

     3. sinh(z) = -i sin(i z),
        but it is almost always faster to use exponentials
        (not used) */
  switch (TYPEOF(z)) {
  case tc_real:     if (fpclassify(REAL_VAL(z)) == FP_ZERO) return MAKE_INT(0);
                    return double2real(sinh(REAL_VAL(z)));
  case tc_integer:  if (INT_VAL(z) == 0) return MAKE_INT(0);
                    return double2real(sinh(INT_VAL(z)));
  case tc_complex:
  case tc_bignum:
  case tc_rational: {
      SCM ez = my_exp(z);
      SCM inv_ez = div2 (MAKE_INT(1), ez);
      return div2(sub2(ez,inv_ez),
                  double2real(2.0));
  }
  default:          error_bad_number(z);
  }
  return STk_void; // for the compiler
}


static SCM my_tanh(SCM z)
{
  /* 1. fastest for reals/fixnums is to use C 'tanh'.

     2. tanh(z) = [  exp(2x) - 1  ] /  [ exp(2x) + 1  ]
        (faster for all but reals/fixnums)

     3. tanh(z) = -i tan(i z)
        but this is always slower than using exponentials...
        (not used) */
  switch (TYPEOF(z)) {
  case tc_real:     if (fpclassify(REAL_VAL(z)) == FP_ZERO) return MAKE_INT(0);
                    return double2real(tanh(REAL_VAL(z)));
  case tc_integer:  if (INT_VAL(z) == 0) return MAKE_INT(0);
                    return double2real(tanh(INT_VAL(z)));
  case tc_complex:
  case tc_bignum:
  case tc_rational: {
      SCM ez = my_exp(z);
      SCM inv_ez = div2 (MAKE_INT(1), ez);
      return div2(sub2 (ez, inv_ez),
                  add2 (ez, inv_ez));
  }
  default:          error_bad_number(z);
  }
  return STk_void; // for the compiler
}


/* asinh is defined for all real numbers, so we can safely use
   the C function "asinh". */
static SCM my_asinh(SCM z) {
  /* asinh(z) = ln (z + SQRT(z^2 + 1)) */
  switch (TYPEOF(z)) {
  case tc_real:     if (fpclassify(REAL_VAL(z)) == FP_ZERO) return MAKE_INT(0);
                    return double2real(asinh(REAL_VAL(z)));
  case tc_integer:  if (INT_VAL(z) == 0) return MAKE_INT(0);
                    return double2real(asinh(INT_VAL(z)));
  case tc_complex:
  case tc_bignum:
  case tc_rational: return my_log(add2(z, STk_sqrt(add2(mul2(z,z), MAKE_INT(1)))));
  default:          error_bad_number(z);
  }
  return STk_void; // for the compiler
}


/* acosh_aux computes acosh of *non-complex* z (zz), using fast C
   math but reverting to my_log, add2 and STk_sqrt for values less
   than +1, which will produce a NaN from the C library. */
static inline SCM
acosh_aux(SCM z, double zz) {
    /* acosh(+inf) = acosh(-inf) = +inf */
    if (isinf(zz)) return double2real(plus_inf);
    double r = zz*zz - 1;
    if (!isinf(r) && r >= 0) { /* can be too large for a double if
                                  zz is too large; can be negative if
                                  zz is in (0,+1). */
        double zzz = sqrt(r) + zz;
        if (!isinf(zzz)) /* did it overflow when we summed zz? */
            return double2real(log(zzz));
    }
    return my_log(add2(z, STk_sqrt(sub2(mul2(z,z), MAKE_INT(1)))));
}

static SCM my_acosh(SCM z) {
  /* acosh(z) = ln (z + SQRT(z^2 - 1)) */
  switch (TYPEOF(z)) {
  case tc_real:     {
      if (fpclassify(REAL_VAL(z)-1.0) == FP_ZERO) return MAKE_INT(0);
      return acosh_aux(z, REAL_VAL(z));
  }
  case tc_integer:  {
      if (INT_VAL(z) == 1) return MAKE_INT(0);
      return acosh_aux(z, (double)INT_VAL(z));
  }
  case tc_complex:
  case tc_bignum:
  case tc_rational: return my_log(add2(z, STk_sqrt(sub2(mul2(z,z), MAKE_INT(1)))));
  default:          error_bad_number(z);
  }
  return STk_void; // for the compiler
}


/*
  atanh_aux computes
  (1/2) [ ln (numer) - ln (denom) ],
  which is the value of atanh(z) when
  numer = 1+z
  denom = 1-z
  This avoids NaNs when z is outside (-1,+1).
*/
static inline SCM
atanh_aux(double numer, double denom) {
      if (numer > 0.0 && denom > 0)
          return double2real((log(numer) -
                              log(denom)) / 2.0);
      SCM l = sub2(my_log(double2real(numer)),
                   my_log(double2real(denom)));
      if (REALP(l)) return double2real(REAL_VAL(l)/2.0);
      return div2(l, double2real(2.0));
}

static SCM my_atanh(SCM z) {
  /* When z=1 or z=-1:
     Chez, Gambit and most Common Lisp implementations signal an error, because
     the argument is out of range.
     Gauche, Guile, Kawa return +inf.0 or -inf.0.
     We do the same as Chez and Gambit */

  /* We do not use atanh from C for values outside the interval (-1,1)
     even if the argument is native double or long, because the C
     implementation doesn't handle complex numbers, and the value returned
     can be a NaN (but Scheme implementations will define atanh for all
     numbers). */

  /* atanh(z) = (1/2) ln [ (1+z) / (1-z) ]
              = (1/2) [ ln (1+z) - ln (1-z) ] */
  switch (TYPEOF(z)) {
  case tc_real:    {
      double zz = REAL_VAL(z);
      if (zz == -1.0 || zz == +1.0)
        error_out_of_range(z);
      if (fpclassify(zz) == FP_ZERO) return MAKE_INT(0);
      /* atanh(inf) is -(i.pi)/2 */
      if (isinf(zz)) return Cmake_complex(double2real(0.0),
                                          double2real(MY_PI/(signbit(zz)
                                                             ? 2.0L
                                                             : -2.0L)));
      return atanh_aux(1.0 + zz, 1.0 - zz);
  }
  case tc_integer:  {
      long zz = INT_VAL(z);
      if (zz == -1 || zz == +1)
        error_out_of_range(z);
      if (zz == 0) return MAKE_INT(0);
      return atanh_aux(1.0 + zz, 1.0 - zz);
  }
  case tc_complex:
  case tc_bignum:
  case tc_rational: {
      SCM numer = add2(MAKE_INT(1),z);
      SCM denom = sub2(MAKE_INT(1),z);
      if (zerop(numer) || zerop(denom))
        error_out_of_range(z);
      /* Too slow to use div2 here, since my_log will return
         inexact, except when log returns zero or a complex.
         Also, log(a)-log(b) is twice as fast as log(a/b)
         when working with bignums!
         However, zero will never happen, since numer = denom+2,
         and log(x) is never log(x-2), so we don't check for
         zero. */
      SCM l = sub2(my_log(numer), my_log(denom));
      if (REALP(l)) return double2real(REAL_VAL(l)/2.0);
      return div2(l, double2real(2.0));
  }
  default:          error_bad_number(z);
  }
  return STk_void; // for the compiler
}


transcendental(cosh)
transcendental(sinh)
transcendental(tanh)
transcendental(acosh)
transcendental(asinh)
transcendental(atanh)



/*=============================================================================*/

static inline int power_of_2_p(long x) {
  /* Find if x is a small power of two.
     By "small" power of two we mean k up to 5 */
  for (int i=1; i <= 5; i++)
    if (x == (1 << i)) {
      return 1;
    }
  return 0;
}

SCM my_log2(SCM x, SCM b) {
  /* my_log2 has fast path for taking logs of fixnums, bignums and
     exact rationals in base two.  It uses a simple trick to extend
     this to "base which is power of two". */

  if (b == MAKE_INT(1)) STk_error("cannot take log in base 1");
  long base = INT_VAL(b);

  if (INTP(b)) {
    /* Fast path for base two. When the number is negative, we compute
       the exact log, and make a complex number with an exact real part,
       and an inexact imaginary part (equal to pi/log(b) ).    */

    if (power_of_2_p(base)) {
      switch (TYPEOF(x)) {
      case tc_integer: {
        unsigned long pwr = base;

        long xx = INT_VAL(x);
        if (power_of_2_p(xx) && base > 2)
          return div2(my_log2(x,MAKE_INT(2)), my_log2(b,MAKE_INT(2)));

        int pos = (xx > 0);

        /* Explicitly check for +-1, so we give an exact result in these cases: */
        if (xx == 1)  return MAKE_INT(0);
        if (xx == -1) return Cmake_complex(MAKE_INT(0),double2real(MY_PI/log(base)));

        xx = labs(xx);
        if (xx == 0) STk_error("cannot take log of zero");
        /* Linear search for the wanted power... */
        for (unsigned long i=1; i < INT_LENGTH; i++, pwr *= base) {
          if (xx == (long) pwr)
            /* If the number is negative, we return the same
               result, but with an imaginary part equal to
               PI/log(base). */
            return pos
              ? MAKE_INT(i)
              : Cmake_complex(MAKE_INT(i),double2real(MY_PI/log(base)));
        }
        break;
      }
      case tc_bignum: {
        if (base <= 62) { /* This is a GMP limitation */
          mpz_t *xx = &BIGNUM_VAL(x);
          mpz_t r;
          mpz_init(r);
          int sgn = mpz_sgn(*xx);
          /* mpz_sizeinbase returns log(xx) in base two plus one, if it's
             exact: */
          unsigned long s = mpz_sizeinbase(*xx,base);
          if (s != 1) {
            mpz_ui_pow_ui(r, base, (s-1));
            /* Now, is s-1 the exact log of xx in base 2 ? */
            if (!mpz_cmpabs(r,*xx)) {
              /* If the number is negative, we return the same
               result, but with an imaginary part equal to
               PI/log(base). */
              mpz_clear(r);
              return (sgn>0)
                ? MAKE_INT(s-1)
              : Cmake_complex(MAKE_INT(s-1),double2real(MY_PI/log(base)));
            }
          }
          mpz_clear(r);
          /* If not, do the floating-point work after the switch... */
        }
        break;
      }
      case tc_rational:
        /* Do log(a/b) = log(a)/log(b).
           This allows us to give exact answers to (log 1/32 2),
           for example! */
        return sub2(my_log2(RATIONAL_NUM(x),b),
                    my_log2(RATIONAL_DEN(x),b));
      }
    }
  }
  return div2(my_log(x),my_log(b));
}

DEFINE_PRIMITIVE("log", log, subr12, (SCM x, SCM b))
{
    return (b)? my_log2(x,b) : my_log(x);
}


DEFINE_PRIMITIVE("atan", atan, subr12, (SCM y, SCM x))
{
  return (x)? my_atan2(y, x) : my_atan(y);
}

/*=============================================================================*/

/*
<doc R7RS square
 * (square z)
 *
 * Returns the square of |z|. This is equivalent to |(* z z)|.
 *
 * @lisp
 * (square 42)     => 1764
 * (square 2.0)    => 4.0
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("square", square, subr1, (SCM z))
{
  if (STk_numberp(z) == STk_false) error_bad_number(z);
  return STk_mul2(z, z);
}

/*
<doc sqrt
 * (sqrt z)
 *
 * Returns the principal square root of |z|. The result will have either
 * positive real part, or zero real part and non-negative imaginary part.
doc>
 */
static SCM my_sqrt_exact(SCM z) {
  if (zerop(z))     return MAKE_INT(0);
  if (negativep(z)) return Cmake_complex(MAKE_INT(0),
                                         my_sqrt_exact(mul2(MAKE_INT(-1UL), z)));

  if (INTP(z)) {
    long   i = INT_VAL(z);
    double d = (double) sqrt((double) i);

    return ((int) d * (int) d == i)? MAKE_INT((int) d) : double2real(d);

  } else { /* This is a bignum */

    mpz_t z0;
    mpz_init(z0);

    if (mpz_perfect_square_p(BIGNUM_VAL(z))) {
      /* We're lucky! It's a perfect square, and the GMP
         will compute the exact result. */
      mpz_sqrt(z0,BIGNUM_VAL(z));
      return bignum2number(z0);
    }

    /* Does it fit a double? If so, use plain C sqrt. It's not exact
       anyway, since we checked above with the result from
       mpz_sqrtrem... */
    double r = bignum2double(BIGNUM_VAL(z));
    if (isfinite(r)) return double2real(sqrt(r));

    mpz_sqrt(z0,BIGNUM_VAL(z));
    SCM x0 = bignum2number(z0);
    /* If x0 does not fit a double, we don't need to waste time with
       an approximation. Return infinity. */
    r = bignum2double(BIGNUM_VAL(x0));
    if (!isfinite(r)) return double2real(plus_inf);

    /* Ok, we tried everything. There's only the slow path now! */
    SCM x = x0;
    SCM x_new = x0;
    SCM err = x0;

    /* Approximate the square root... Essentially, Newton's method,
       but coded using STklos' internal sub2, div2, mul2, abs
       functions.  */
    while(STk_numgt2(err, rational_epsilon) > 0 &&
          isfinite(REAL_VAL(exact2inexact(x_new)))) {
        x_new = sub2(x, div2(sub2(mul2(x, x), z),
                             mul2(x, MAKE_INT(2))));
        err = STk_abs(sub2(x_new, x));
        x = x_new;
    }
    /* Return inexact, because if we got here, the square of this
       result will not equal to z (it's an approximation, so it would
       be strange to give an "exact" result that is not "exactly" the
       result of the operation). But for floating-point, it is
       acceptable to offer an approximation.  */
    return exact2inexact(x_new);
  }
}

static inline SCM my_sqrt_complex(SCM z)
{
  SCM aa, bb;
  SCM a = COMPLEX_REAL(z);
  SCM b = COMPLEX_IMAG(z);

  /* Given a, b we will compute A, B such that the square root of
     the complex number a+bi is

     sqrt(a+bi) = A+Bi

     The algorithm:

     if a < 0:
     B := sqrt( (|z|-a) / 2) * sign(b)
     A := b / (2*B).

     if a >= 0:
     A := sqrt( (|z|+a) / 2)
     if A != 0:
     B := (b / (2*A))
     else:
     B = 0.0                        */

  if (negativep(a) ||
      (REALP(a) && signbit(REAL_VAL(a)))) { /* negativep(-0.0) won't work... */
    /* a < 0 */
    bb = STk_sqrt(div2(sub2(absolute(z),a), MAKE_INT(2)));

    if (negativep(b) || (REALP(b) && signbit(REAL_VAL(b))))
      bb = mul2(bb,MAKE_INT((unsigned long) -1));

    aa = div2(b,mul2(bb, MAKE_INT(2)));
  } else {
    /* a >= 0 */
    aa = STk_sqrt(div2(add2(a, absolute(z)), MAKE_INT(2)));
    bb = zerop(aa) ? double2real(0.0): div2(b,mul2(aa,MAKE_INT(2)));
  }
  return make_complex(aa, bb);
}

DEFINE_PRIMITIVE("sqrt", sqrt, subr1, (SCM z))
{
  switch (TYPEOF(z)) {
    case tc_integer:
    case tc_bignum:   return my_sqrt_exact(z);
    case tc_rational: return div2(my_sqrt_exact(RATIONAL_NUM(z)),
                                  my_sqrt_exact(RATIONAL_DEN(z)));
    case tc_real:     if (REAL_VAL(z) < 0 && FINITE_REALP(z))
                        return Cmake_complex(MAKE_INT(0),
                                             double2real(sqrt(-REAL_VAL(z))));
                      return double2real(sqrt(REAL_VAL(z)));
    case tc_complex:  return my_sqrt_complex(z);
    default:          error_bad_number(z);
  }
  return STk_void; /* never reached */
}


/*
<doc expt
 * (expt z1 z2)
 *
 * Returns |z1| raised to the power |z2|.
 * @l
 * NOTE: |0,(sup "z")| is 1 if |z = 0| and |0| otherwise.
doc>
 */

static inline SCM exact_exponent_expt(SCM x, SCM y)
{
  mpz_t res;
  SCM scm_res;

  /* y is already known to be exact; so if it is zero,
     return exact one. */
  if (zerop(y)) return MAKE_INT(1);

  if (zerop(x) || (x == MAKE_INT(1))) return x;

  if (TYPEOF(y) == tc_bignum)
    STk_error("exponent too big: ~S", y);

  switch (TYPEOF(x)) {
    case tc_integer:
      mpz_init(res);
      /* Take the sign of the base. The GMP computation will be done
         without it. */
      long sign = (INT_VAL(x) < 0) ? -1 : +1;

      mpz_ui_pow_ui(res, (unsigned long) (sign*INT_VAL(x)), (unsigned long) INT_VAL(y));

      /* Put back the sign, if needed (that is, if sign (of x) < 0 and the exponent is odd): */
      if (sign<0 && ((INT_VAL(y)) & 1UL)) mpz_neg(res,res);
      scm_res = bignum2number(res);
      mpz_clear(res);
      return scm_res;
    case tc_bignum:
      mpz_init(res);
      mpz_pow_ui(res, BIGNUM_VAL(x), INT_VAL(y));
      scm_res = bignum2number(res);
      mpz_clear(res);
      return scm_res;
    case tc_rational:
      return make_rational(exact_exponent_expt(RATIONAL_NUM(x), y),
                           exact_exponent_expt(RATIONAL_DEN(x), y));
    default: {
      SCM nx, ny, val = MAKE_INT(1);

      while (y != MAKE_INT(1)) {
        nx = mul2(x, x);
        ny = int_quotient(y, MAKE_INT(2));
        if (STk_evenp(y) == STk_false) val = mul2(x, val);
        x = nx;
        y = ny;
      }
      return mul2(val, x);
    }
  }
}

static SCM my_expt(SCM x, SCM y)
{
  /* y is >= 0 */
  switch (TYPEOF(y)) {
    case tc_integer:
    case tc_bignum:
      return exact_exponent_expt(x, y);
    case tc_rational:
    case tc_real:
      {
        if (zerop(y)) return double2real(1.0);
        if (zerop(x)) return (x==MAKE_INT(0)) ? x : double2real(0.0);
        if (REALP(y)) {
          if (REALP(x) && !negativep(x)) {
            /* real ^ real, see if we can use pow: */
            double r = pow(REAL_VAL(x),REAL_VAL(y));
            if (!isinf(r) || /* no overflow, return r */
                (!FINITE_REALP(x)) || !FINITE_REALP(y)) /* not overflow, one arg. was inf! */
              return double2real(r);
          }
          if (! (REAL_VAL(y) - floor(REAL_VAL(y))))
            /* It represents an integer precisely! Turn the exponent into
               an exact number and call exact_exponent_expt: */
            return exact2inexact(exact_exponent_expt(x, (inexact2exact(y))));
          /* Either r overflowed, or y didn't represent an integer perfectly.
             Fall through to use STklos' arithmetic version of
             exp(log(x) * y)                                                  */
        }
      }
      /* FALLTHROUGH */
    case tc_complex:
      if (zerop(x)) {
        /* R7RS: The value of 0^z is 1 if (zero? z), 0 if (real-part z) is positive,
           and an error otherwise. Similarly for 0.0^z, with inexact results.*/
        if (positivep(COMPLEX_REAL(y))) {
          return isexactp(x) ? MAKE_INT(0) : double2real(0.0);
        }
        else
          STk_error("power of zero to a complex exponent with negative real part ~S", y);
      }
      else
        return my_exp(mul2(my_log(x),y));
      /* FALLTHROUGH */
    default:
      error_cannot_operate("expt", x, y);
  }
  return STk_void; /* never reached */
}


DEFINE_PRIMITIVE("expt", expt, subr2, (SCM x, SCM y))
{
  if (!COMPLEXP(y) && negativep(y))
    return div2(MAKE_INT(1),
                my_expt(x, sub2(MAKE_INT(0), y)));
  return my_expt(x, y);
}

/*
<doc  make-rectangular make-polar real-part imag-part magnitude angle
 * (make-rectangular x1 x2)
 * (make-polar x3 x)
 * (real-part z)
 * (imag-part z)
 * (magnitude z)
 * (angle z)
 *
 * If x1, x2, x3, and x4 are real numbers and z is a complex number such that
 * @l
 * |z = x1 + x2.i = x3 . e^i.x4^)|
 * @l
 * Then
 * @lisp
 * (make-rectangular x1 x2)       => z
 * (make-polar x3 x4)             => z
 * (real-part z)                  => x1
 * (imag-part z)                  => x2
 * (magnitude z)                  => abs(x3)
 * (angle z)                      => xa
 * @end lisp
 * where
 * |-π < xa <= π| with |xa = x4 + 2πn|
 * for some integer n.
 * @lisp
 * (angle +inf.0)                 => 0.0
 * (angle -inf.0)                 => 3.14159265358979
 * @end lisp
 * @l
 * NOTE: |Magnitude| is the same as |abs| for a real argument.
doc>
 */

DEFINE_PRIMITIVE("magnitude", magnitude, subr1, (SCM z))
{
  return absolute(z);
}

DEFINE_PRIMITIVE("angle", angle, subr1, (SCM z))
{
  switch (TYPEOF(z)) {
    case tc_integer:
    case tc_bignum:
    case tc_rational: /* The angle for 0+0i is undefined. It would, strictly speaking,
                         be atan(0/0), and we won't divide by zero.
                         Of all other implementations, it seems that only Chez does
                         trigger an error here, but it is the correct thing to do, at
                         least for exact zero... */
                      if (z == MAKE_INT(0)) STk_error("not defined for exact zero");
                      else return positivep(z) ? MAKE_INT(0) : double2real(MY_PI);
    case tc_real:     return double2real(positivep(z) ? 0.0 : MY_PI);
    case tc_complex:  return my_atan2(COMPLEX_IMAG(z), COMPLEX_REAL(z));
    default:          error_bad_number(z);
  }
  return STk_void; /* never reached */
}



DEFINE_PRIMITIVE("real-part", real_part, subr1, (SCM z))
{
  switch (TYPEOF(z)) {
    case tc_complex: return COMPLEX_REAL(z);
    case tc_real:
    case tc_rational:
    case tc_bignum:
    case tc_integer: return z;
    default:         error_bad_number(z);
  }
  return STk_void; /* never reached */
}

DEFINE_PRIMITIVE("imag-part", imag_part, subr1, (SCM z))
{
  switch (TYPEOF(z)) {
    case tc_complex: return COMPLEX_IMAG(z);
    case tc_real:
    case tc_rational:
    case tc_bignum:
    case tc_integer: return MAKE_INT(0);
    default:         error_bad_number(z);
  }
  return STk_void; /* never reached */
}


DEFINE_PRIMITIVE("make-rectangular", make_rectangular, subr2, (SCM r, SCM i))
{
  if (STk_realp(r) == STk_false) error_not_a_real_number(r);
  if (STk_realp(i) == STk_false) error_not_a_real_number(i);
  return make_complex(r, i);
}


DEFINE_PRIMITIVE("make-polar", make_polar, subr2, (SCM a, SCM m))
{
  if (STk_realp(a) == STk_false) error_not_a_real_number(a);
  if (STk_realp(m) == STk_false) error_not_a_real_number(m);

  return make_polar(a, m);
}


/*
<doc exact->inexact inexact->exact
 * (exact->inexact z)
 * (inexact->exact z)
 *
 * |Exact->inexact| returns an inexact representation of z.
 * The value returned is the inexact number that is numerically closest to
 * the argument.
 * |Inexact->exact| returns an exact representation of z.
 * The value returned is the exact number that is numerically closest to
 * the argument.
doc>
*/
DEFINE_PRIMITIVE("exact->inexact", ex2inex, subr1, (SCM z))
{
  switch (TYPEOF(z)) {
    case tc_complex:  if (REALP(COMPLEX_REAL(z)) && REALP(COMPLEX_IMAG(z)))
                        return z;
                      else return Cmake_complex(exact2inexact(COMPLEX_REAL(z)),
                                                exact2inexact(COMPLEX_IMAG(z)));
    case tc_real:     return z;
    case tc_rational: return rational2real(z);
    case tc_bignum:   return scheme_bignum2real(z);
    case tc_integer:  return double2real((double) INT_VAL(z));
    default:          error_bad_number(z); return STk_void;
  }
}

DEFINE_PRIMITIVE("inexact->exact", inex2ex, subr1, (SCM z))
{
  switch (TYPEOF(z)) {
    case tc_complex:  if (REALP(COMPLEX_REAL(z)) || REALP(COMPLEX_IMAG(z)))
                        return make_complex(inexact2exact(COMPLEX_REAL(z)),
                                            inexact2exact(COMPLEX_IMAG(z)));
                      else return z;
    case tc_real:     {
                        register double x = REAL_VAL(z);
                        if (isinf(x) || isnan(x))
                          STk_error("Cannot make infinity/nan ~S exact", z);
                        return double2rational(x);
                      }
    case tc_rational:
    case tc_bignum:
    case tc_integer:  return z;
    default:          error_bad_number(z); return STk_void;
  }
}

/*
<doc  number->string
 * (number->string z)
 * (number->string z radix)
 *
 * |Radix| must be an exact integer, either 2, 8, 10, or 16. If omitted, radix
 * defaults to 10. The procedure |number->string| takes a number and a radix
 * and returns as a string an external representation of the given number in
 * the given radix such that
 * @lisp
 * (let ((number number)
 *       (radix radix))
 *   (eqv? number
 *        (string->number (number->string number radix) radix)))
 * @end lisp
 * is true. It is an error if no possible result makes this expression true.
 * @l
 * If |z| is inexact, the radix is 10, and the above expression can be
 * satisfied by a result that contains a decimal point, then the result
 * contains a decimal point and is expressed using the minimum number of digits
 * (exclusive of exponent and trailing zeroes) needed to make the above expression
 * true; otherwise the format of the result is unspecified.
 * @l
 * The result returned by |number->string| never contains an explicit radix
 * prefix.
 * @l
 * NOTE: The error case can occur only when |z| is not a complex number or
 * is a complex number with a non-rational real or imaginary part.
 * @l
 * IMPORTANT: If |z| is an inexact number represented using flonums, and
 * the radix is 10, then the above expression is normally satisfied by a result
 * containing a decimal point. The unspecified case allows for infinities,
 * NaNs, and non-flonum representations.
doc>
 */

DEFINE_PRIMITIVE("number->string", number2string, subr12, (SCM n, SCM base))
{
  long b = (base)? STk_integer_value(base) : 10L;
  char *s, buffer[100];
  SCM z;

  if (!NUMBERP(n))                            error_bad_number(n);
  if (b != 2 && b != 8 && b != 10 && b != 16) error_incorrect_radix(base);

  s = number2Cstr(n, b, buffer, sizeof(buffer));
  z = STk_makestring(strlen(s), s);
  if (s != buffer) STk_free(s);
  return z;
}

/*
<doc  string->number
 * (string->number string)
 * (string->number string radix)
 *
 * Returns a number of the maximally precise representation expressed by the
 * given |string|. |Radix| must be an exact integer, either 2, 8, 10, or 16.
 * If supplied, |radix| is a default radix that may be overridden by an explicit
 * radix prefix in |string| (e.g. ,(code "\"#o177\"")). If |radix| is not
 *  supplied, then
 * the default radix is 10. If |string| is not a syntactically valid notation
 * for a number, then |string->number| returns |#f|.
 * @lisp
 * (string->number "100")        =>  100
 * (string->number "100" 16)     =>  256
 * (string->number "1e2")        =>  100.0
 * (string->number "15##")       =>  1500.0
 * (string->number "+inf.0")     =>  +inf.0
 * (string->number "-inf.0")     =>  -inf.0
 * @end lisp
 *
doc>
 */

DEFINE_PRIMITIVE("string->number", string2number, subr12, (SCM str, SCM base))
{
  long b = (base)? STk_integer_value(base) : 10L;

  if (!STRINGP(str))                          STk_error("bad string ~S", str);
  if (b != 2 && b != 8 && b != 10 && b != 16) error_incorrect_radix(base);

  return STk_Cstr2number(STRING_CHARS(str), b);
}


/*
<doc EXT decode-float
 * (decode-float n)
 *
 * |decode-float| returns three exact integers: |significand|, |exponent|
 * and |sign| (where |-1 \<= sign \<= 1|). The values returned by
 * |decode-float| satisfy:
 * @lisp
 * n = (* sign significand (expt 2 exponent))
 * @end lisp
 * Here is an example of |decode-float| usage.
 * @lisp
 * (receive l (decode-float -1.234) l)
 *                     => (5557441940175192 -52 -1)
 * (exact->inexact (* -1
 *                     5557441940175192
 *                     (expt 2 -52)))
 *                     => -1.234
 * @end lisp
doc>
*/
static SCM decode(SCM num)
{
  /* Decodes floating-point numbers. As portable as it was possible to make,
     and using no arithmetic on Scheme numbers. */

  double d = REAL_VAL(num);

  /* Special cases */
  if (isnan(d)) return STk_n_values(3, STk_false, MAKE_INT(0), MAKE_INT(0));
  if (isinf(d)) return STk_n_values(3, STk_true, MAKE_INT(0), MAKE_INT(0));

  SCM exponent;
  SCM significand;
  SCM sign = MAKE_INT( (signbit(d)) ? -1 : +1 );

  if (signbit(d)) d = -d;

  if (d == 0.0) {
    exponent = MAKE_INT(0);
    significand = MAKE_INT(0);
  } else {
    int e = 1;
    /* We'll obtain the exponent. There are two cases:

       1. NORMAL: we calculate the exponent. This is the same that ECL does
       (and which only works for normal numbers).

       frexp will return a double DD (which we ignore) such that

       d = DD * 2^e

       We know that 1/2 <= DD < 1, so the computed 'e' is unique. This 'e'
       is the one we need.

       2. SUBNORMAL: the exponent is fixed in DBL_MIN_EXP. */
    if (isnormal(d)) frexp(d,&e);
    else             e = DBL_MIN_EXP;


    /* We subtract DBL_MANT_DIG from the exponent (the C macro does not
       take this into account, so we need to compensate). */
    e -= DBL_MANT_DIG;

    /* To obtain the significand, we only need to "undo" the operation
       d = significand * 2^(exponent).
       Which is the same as calculating
       d * 2(-exponent).
       Which, then, is the same as calculating
       ldexp(d,-e).                                                       */
    significand = double2integer(ldexp(d, -e));
    exponent = MAKE_INT((unsigned long) e);
  }
  return STk_n_values(3, significand, exponent, sign);
}

/*
<doc EXT float-max-significand float-min-exponent float-max-exponent
 * (float-max-significand)
 * (float-min-exponent)
 * (float-max-exponent)
 *
 * These procedures return the maximum significand value and the
 * minimum and maximum values for the exponent when calling
 * the |encode-float| procedure.
doc>
 */
DEFINE_PRIMITIVE("float-max-significand", float_max_signif, subr0, ())
{
  return STk_ulong2integer((unsigned long) pow(FLT_RADIX, DBL_MANT_DIG)
                           -1);
}

DEFINE_PRIMITIVE("float-min-exponent", float_min_exp, subr0, ())
{
  return MAKE_INT((unsigned long)(DBL_MIN_EXP - DBL_MANT_DIG));
}

DEFINE_PRIMITIVE("float-max-exponent", float_max_exp, subr0, ())
{
  return MAKE_INT(DBL_MAX_EXP - DBL_MANT_DIG);
}

DEFINE_PRIMITIVE("decode-float", decode_float, subr1, (SCM n))
{
  if (!NUMBERP(n) || COMPLEXP(n)) error_not_a_real_number(n);
  if (EXACTP(n)) n = exact2inexact(n);
  return decode(n);
}

/*
<doc EXT encode-float
 * (encode-float significand exponent sign)
 *
 * |encode-float| does the inverse work of |decode-float|: it accepts three
 * numbers, |significand|, |exponent| and |sign|, and returns the floating
 * point number represented by them.
 *
 * When |significand| is `#f`, a NaN will be returned.
 * When |significand| is `#t`, positive or negative infinity is returned,
 * depending on the value of |sign|.
 *
 * Otherwise, the number returned is
 * @lisp
 * n = (* sign significand (expt 2 exponent))
 * @end lisp
 *
 * Both |significand| and |exponent| must be within their proper ranges:
 *
 * 0 < |significand| \<= |float-max-significand|, and
 *
 * |float-min-exponent| \<= |exponent| \<=  |float-max-exponent|.
 *
 *
 * @lisp
 * (encode-float (#t 0  1)) => +inf.0
 * (encode-float (#t 0 -1)) => -inf.0
 * (encode-float (#f 0  1)) => +nan.0
 *
 * (decode-float -0.01)
 * => 5764607523034235
 * => -59
 * => -1
 * (inexact (encode-float 5764607523034235 -59 -1)) => -0.01
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("encode-float", encode_float, subr3, (SCM significand, SCM exponent,
                                                       SCM sign))
{
  if (STk_integerp(exponent) == STk_false)  error_not_an_integer(exponent);
  if (STk_integerp(sign) == STk_false)      error_not_an_integer(sign);
  int g = INT_VAL(inexact2exact(sign));

  /* #f => NaN,
     #t => inf  */
  if (significand == STk_false) return double2real(make_nan(0,0,0));
  if (significand == STk_true)  return (g >= 0)
                                  ? double2real(plus_inf)
                                  : double2real(minus_inf);

  /* Significand */
  if (STk_integerp(significand) == STk_false) error_not_an_integer(significand);
  SCM max_signif = STk_ulong2integer((unsigned long) pow(FLT_RADIX, DBL_MANT_DIG)-1);
  if (negativep(significand)) STk_error("negative significand ~S", significand);
  if (STk_numgt2(significand, max_signif))
    STk_error("significand ~S above maximum ~S", significand, max_signif);

  /* Exponent */
  long e = INT_VAL(inexact2exact(exponent));
  if (e < DBL_MIN_EXP - DBL_MANT_DIG)
    STk_error("exponent ~S below minimum ~S",
              exponent,
              MAKE_INT((unsigned long) DBL_MIN_EXP));
  if (e > DBL_MAX_EXP - DBL_MANT_DIG)
    STk_error("exponent ~S above maximum ~S",
              exponent,
              MAKE_INT(DBL_MAX_EXP));

  /* Done! */
  SCM res = STk_mul2(sign, significand);
  return STk_mul2(res, STk_expt (MAKE_INT(2), exponent));
}

/*
 *
 * Logical operations
 *
 */

static inline SCM bignum_logop(SCM n1, SCM n2,
                               void (*op)(mpz_t, const mpz_t, const mpz_t))
{
  mpz_t r;
  SCM res;

  mpz_init(r);
  op(r, BIGNUM_VAL(n1), BIGNUM_VAL(n2));
  res = bignum2number(r);
  mpz_clear(r);
  return res;
}

#define LOGICAL_OP(sname, name, op, opfct)                         \
DEFINE_PRIMITIVE(sname, name, subr2, (SCM n1, SCM n2))             \
{                                                                  \
  if (!INTP(n1) && !BIGNUMP(n1))  error_not_an_exact_integer(n1);  \
  if (!INTP(n2) && !BIGNUMP(n2))  error_not_an_exact_integer(n2);  \
                                                                   \
  if (INTP(n1) && INTP(n2))                                        \
    return MAKE_INT(INT_VAL(n1) op INT_VAL(n2));                   \
                                                                   \
  if (INTP(n1)) n1 = long2scheme_bignum(INT_VAL(n1));              \
  if (INTP(n2)) n2 = long2scheme_bignum(INT_VAL(n2));              \
  return bignum_logop(n1, n2, opfct);                              \
}

LOGICAL_OP("%bit-and", bit_and, &, mpz_and)
LOGICAL_OP("%bit-or",  bit_or,  |, mpz_ior)
LOGICAL_OP("%bit-xor", bit_xor, ^, mpz_xor)


/*
 *
 * Allocation functions for Bignums (i.e. use GC)
 *
 */
static void * allocate_function(size_t sz)
{
  void *ptr = STk_must_malloc_atomic(sz);

  if (ptr)
    memset(ptr, 0, sz);
  return ptr;
}

static void * reallocate_function(void *ptr, size_t _UNUSED(old), size_t new)
{
  return STk_must_realloc(ptr, new);
}

static void deallocate_function(void * ptr, size_t _UNUSED(sz))
{
  STk_free(ptr);
}

/*
 * SRFI 208: NaN procedures
 */

static void verify_NaN(SCM n) {
  if ((TYPEOF(n) != tc_real) || !isnan(REAL_VAL(n)))
    STk_error("bad NaN value: ~S", n);
}



DEFINE_PRIMITIVE("%make-nan", make_nan, subr3, (SCM neg, SCM quiet, SCM payload))
{
  SCM z;

  if (!INTP(payload) || ((uint64_t) INT_VAL(payload) > payload_mask))
    STk_error("bad payload ~S", payload);

  /* Do not call STk_double2real since it converts -nan.0 to +nan.0 */
  NEWCELL(z, real);
  REAL_VAL(z) = make_nan(neg != STk_false, quiet != STk_false, INT_VAL(payload));
  return z;
}



/*
<doc EXT nan-negative?
 * (nan-negative? nan)
 *
 * returns |#t| if the sign bit of |nan| is set and |#f| otherwise.
doc>
*/
DEFINE_PRIMITIVE("nan-negative?", nan_negativep, subr1, (SCM nan)) {
  union binary64 tmp;

  verify_NaN(nan);
  tmp.d = REAL_VAL(nan);
  return MAKE_BOOLEAN((tmp.u & sign_mask) != 0);
}


/*
<doc EXT nan-quiet?
 * (nan-quiet? nan)
 *
 * returns |#t| if |nan| is a quiet NaN.
doc>
*/
DEFINE_PRIMITIVE("nan-quiet?", nan_quietp, subr1, (SCM nan)) {
  union binary64 tmp;

  verify_NaN(nan);
  tmp.d = REAL_VAL(nan);
  return MAKE_BOOLEAN((tmp.u & quiet_mask)!= 0);
}


/*
<doc EXT nan-payload
 * (nan-payload nan)
 *
 * returns  the payload bits of |nan| as a positive exact integer.
doc>
*/
DEFINE_PRIMITIVE("nan-payload", nan_payload, subr1, (SCM nan)) {
  union binary64 tmp;

  verify_NaN(nan);
  tmp.d = REAL_VAL(nan);
  return MAKE_INT(tmp.u & payload_mask);
}


/*
<doc EXT nan=?
 * (nan=? nan1 nan2)
 *
 * Returns |#t| if |nan1| and |nan2| have the same sign, quiet bit,
 * and payload; and |#f| otherwise.
doc>
*/
DEFINE_PRIMITIVE("nan=?", nan_equalp, subr2, (SCM n1, SCM n2)) {
  union binary64 tmp1, tmp2;

  verify_NaN(n1);
  verify_NaN(n2);
  tmp1.d = REAL_VAL(n1);
  tmp2.d = REAL_VAL(n2);
  return MAKE_BOOLEAN(tmp1.u ==tmp2.u);
}


DEFINE_PRIMITIVE("%stklos-has-gmp?", has_gmp, subr0, ())
{
#ifdef  __MINI_GMP_H__
  return STk_false;
#else
  return STk_true;
#endif
}


/*
 *
 * Initialization
 *
 */
int STk_init_number(void)
{
  /* For systems without these constants, we can do:
     plus_inf  = 1.0 / 0.0;
     minus_inf = -plus_inf;
     STk_NaN   = strtod("NAN", NULL);
  */

  /* initialize  special IEEE 754 values */
  plus_inf  = HUGE_VAL;
  minus_inf = -HUGE_VAL;
  STk_NaN   = strtod("NAN", NULL); // FIXME: use make_nan(0, 1, 0)?

  /* Other useful "constants" */
  complex_i        = make_complex(MAKE_INT(0),MAKE_INT(1));
  rational_epsilon = div2(inexact2exact(double2real(STk_dbl_true_min())),
                          MAKE_INT(2));

  /* Force the LC_NUMERIC locale to "C", since Scheme definition
     imposes that decimal numbers use a '.'
  */
  setlocale(LC_NUMERIC, "C");

  /* Compute the log10 of INT_MAX_VAL to avoid to build a bignum for small int */
  {
    char buffer[100];
    snprintf(buffer, sizeof(buffer), "%ld", INT_MAX_VAL);

    log10_maxint = strlen(buffer)-1;
  }

  /* Register bignum allocation functions */
  mp_set_memory_functions(allocate_function,
                          reallocate_function,
                          deallocate_function);

  /* register the extended types for numbers */
  DEFINE_XTYPE(bignum,   &xtype_bignum);
  DEFINE_XTYPE(rational, &xtype_rational);
  DEFINE_XTYPE(complex,  &xtype_complex);

  /* Add new primitives */
  ADD_PRIMITIVE(nanp);
  ADD_PRIMITIVE(numberp);
  ADD_PRIMITIVE(complexp);
  ADD_PRIMITIVE(realp);
  ADD_PRIMITIVE(rationalp);
  ADD_PRIMITIVE(bignump);
  ADD_PRIMITIVE(integerp);
  ADD_PRIMITIVE(exactp);
  ADD_PRIMITIVE(inexactp);
  ADD_PRIMITIVE(integer_length);

  ADD_PRIMITIVE(numeq);
  ADD_PRIMITIVE(numlt);
  ADD_PRIMITIVE(numgt);
  ADD_PRIMITIVE(numle);
  ADD_PRIMITIVE(numge);

  ADD_PRIMITIVE(finitep);
  ADD_PRIMITIVE(infinitep);
  ADD_PRIMITIVE(zerop);
  ADD_PRIMITIVE(positivep);
  ADD_PRIMITIVE(negativep);
  ADD_PRIMITIVE(oddp);
  ADD_PRIMITIVE(evenp);

  ADD_PRIMITIVE(max);
  ADD_PRIMITIVE(min);

  ADD_PRIMITIVE(plus);
  ADD_PRIMITIVE(multiplication);
  ADD_PRIMITIVE(difference);
  ADD_PRIMITIVE(division);

  ADD_PRIMITIVE(abs);

  ADD_PRIMITIVE(quotient);
  ADD_PRIMITIVE(remainder);
  ADD_PRIMITIVE(modulo);
  ADD_PRIMITIVE(gcd);
  ADD_PRIMITIVE(lcm);
  ADD_PRIMITIVE(numerator);
  ADD_PRIMITIVE(denominator);

  ADD_PRIMITIVE(floor);
  ADD_PRIMITIVE(ceiling);
  ADD_PRIMITIVE(truncate);
  ADD_PRIMITIVE(round);

  ADD_PRIMITIVE(exp);
  ADD_PRIMITIVE(log);
  ADD_PRIMITIVE(cos);
  ADD_PRIMITIVE(sin);
  ADD_PRIMITIVE(tan);

  ADD_PRIMITIVE(asin);
  ADD_PRIMITIVE(acos);
  ADD_PRIMITIVE(atan);

  ADD_PRIMITIVE(cosh);
  ADD_PRIMITIVE(sinh);
  ADD_PRIMITIVE(tanh);
  ADD_PRIMITIVE(acosh);
  ADD_PRIMITIVE(asinh);
  ADD_PRIMITIVE(atanh);

  ADD_PRIMITIVE(square);
  ADD_PRIMITIVE(sqrt);
  ADD_PRIMITIVE(expt);


  ADD_PRIMITIVE(make_rectangular);
  ADD_PRIMITIVE(make_polar);
  ADD_PRIMITIVE(real_part);
  ADD_PRIMITIVE(imag_part);
  ADD_PRIMITIVE(magnitude);
  ADD_PRIMITIVE(angle);

  ADD_PRIMITIVE(ex2inex);
  ADD_PRIMITIVE(inex2ex);

  ADD_PRIMITIVE(number2string);
  ADD_PRIMITIVE(string2number);

  ADD_PRIMITIVE(decode_float);
  ADD_PRIMITIVE(encode_float);
  ADD_PRIMITIVE(float_max_signif);
  ADD_PRIMITIVE(float_min_exp);
  ADD_PRIMITIVE(float_max_exp);

  ADD_PRIMITIVE(bit_and);
  ADD_PRIMITIVE(bit_or);
  ADD_PRIMITIVE(bit_xor);


  /* SRFI 208: NaN procedures */
  ADD_PRIMITIVE(make_nan);
  ADD_PRIMITIVE(nan_negativep);
  ADD_PRIMITIVE(nan_quietp);
  ADD_PRIMITIVE(nan_payload);
  ADD_PRIMITIVE(nan_equalp);

  ADD_PRIMITIVE(has_gmp);

  /* Add parameter for float numbers precision */
  STk_make_C_parameter("real-precision",
                       MAKE_INT(real_precision),
                       real_precision_conv,
                       STk_STklos_module);

   /* Add parameter for allowing underscore in numbers */
   STk_make_C_parameter("accept-srfi-169-numbers",
                       MAKE_BOOLEAN(use_srfi_169),
                       srfi_169_conv,
                       STk_STklos_module);
  return TRUE;
}
