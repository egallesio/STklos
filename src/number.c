/*                                                      -*- coding: utf-8 -*-
 *
 * n u m b e r . c      -- Numbers management
 *
 * Copyright © 1993-2021 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 * Last file update: 21-May-2021 18:15 (eg)
 */


/* workaround for bad optimisations done in glibc2.1. Thanks to  Andreas Jaeger
 * <aj@suse.de> for it
 */
#ifndef __NO_MATH_INLINES
#  define __NO_MATH_INLINES
#endif

#include <math.h>
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

#define FINITE_REALP(n) ((REAL_VAL(n) != minus_inf) && (REAL_VAL(n) != plus_inf))



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

#define MY_PI           3.1415926535897932384626433832795029L  /* pi */

#define BIGNUM_FITS_INTEGER(_bn) (mpz_cmp_si((_bn), INT_MIN_VAL) >= 0 &&        \
                                  mpz_cmp_si((_bn), INT_MAX_VAL) <= 0)
#define LONG_FITS_INTEGER(_l)    (INT_MIN_VAL <= (_l) && (_l) <= INT_MAX_VAL)
#define TYPEOF(n)                (INTP(n)? tc_integer: STYPE(n))


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
static SCM gcd2(SCM o1, SCM o2);

EXTERN_PRIMITIVE("make-rectangular", make_rectangular, subr2, (SCM r, SCM i));
EXTERN_PRIMITIVE("real-part", real_part, subr1, (SCM z));
EXTERN_PRIMITIVE("magnitude", magnitude, subr1, (SCM z));
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


static SCM int_quotient(SCM o1, SCM o2);
static SCM my_cos(SCM z);
static SCM my_sin(SCM z);



/******************************************************************************
 *
 * Utilities
 *
 ******************************************************************************/
static void error_bad_number(SCM n)
{
  STk_error("~S is a bad number", n);
}

static void error_at_least_1(void)
{
  STk_error("expects at least one argument");
}

static void error_not_on_a_complex(SCM n)
{
  STk_error("cannot be computed on the complex number ~S", n);
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
   * signaling dille be seen as a positive infinity.
   * So, to make a signaling NaN, we clear the bit 51 and set the bit 50
   * ==> the payload can use only 50 bits
   */
  t.u = (quiet)? 0x7ff8000000000000U : 0x7ff4000000000000U;
  if (neg)   t.u |= sign_mask;
  t.u |= pay;

  return t.d;
}


/*
<doc EXT real-precision
 * (real-precision)
 * (real-precision value)
 *
 * This parameter object permits to change the default precision used
 * to print real numbers.
 * @lisp
 * (real-precision)        => 15
 * (define f 0.123456789)
 * (display f)             => 0.123456789
 * (real-precision 3)
 * (display f)             => 0.123
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
static Inline SCM Cmake_complex(SCM r, SCM i)
{
  SCM z;

  NEWCELL(z, complex);
  COMPLEX_REAL(z) = r;
  COMPLEX_IMAG(z) = i;
  return z;
}

static Inline SCM make_complex(SCM r, SCM i)
{
  return (zerop(i)) ? r : Cmake_complex(r, i);
}

static Inline SCM make_polar(SCM a, SCM m)
{
  return make_complex(mul2(a, my_cos(m)), mul2(a, my_sin(m)));
}


static Inline SCM Cmake_rational(SCM n, SCM d)
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
  if (positivep(imag))
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

static Inline SCM long2scheme_bignum(long x)
{
  SCM z;

  NEWCELL(z, bignum);
  mpz_init_set_si(BIGNUM_VAL(z), x);
  return z;
}


static Inline SCM long2integer(long x)
{
  return (INT_MIN_VAL<=x && x<=INT_MAX_VAL) ?  MAKE_INT(x): long2scheme_bignum(x);
}


static Inline SCM double2real(double x)
{
  SCM z;

  NEWCELL(z, real);
  REAL_VAL(z) = x;
  return z;
}

static SCM double2integer(double n)     /* small or big depending of n's size */
{
  unsigned int i, j;
  size_t size = 20;
  char *tmp = NULL;
  SCM z;

  /* Try first to convert n to a long */
  if (((double) INT_MIN_VAL <= n) && (n <= (double) INT_MAX_VAL))
    return MAKE_INT((long) n);

  /* n doesn't fit in a long => build a bignum. THIS IS VERY INEFFICIENT */
  tmp = STk_must_malloc(size);
  i = 0;
  if (n < 0.0) { tmp[i++] = '-'; n = -n; }
  do {
    if (i >= size) tmp = STk_must_realloc(tmp, size *= 2);
    tmp[i++] = (int) fmod(n, (double) 10) + '0';
    n = floor(n / 10.0);
  }
  while (n > 0.0);
  tmp[i] = 0;

  /* Reverse the content of string tmp */
  for (i=i-1, j=(tmp[0]=='-'); i > j; i--, j++) {
    char c = tmp[i];
    tmp[i] = tmp[j];
    tmp[j] = c;
  }

  /* tmp contains a textual representation of n. Convert it to a bignum */
  z = STk_Cstr2number(tmp, 10L);
  if (tmp) STk_free(tmp);
  return z;
}

static SCM double2rational(double d)
{
  double fraction, i;
  SCM int_part, num, den, res;
  int negative = 0;

  if (d < 0.0) { negative = 1; d = -d; }
  fraction = modf(d, &i);
  int_part = double2integer(i);

  if (!fraction) {
    res = int_part;
  } else {
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
  }

  return negative? mul2(res, MAKE_INT((unsigned long) -1)): res;
}

static Inline SCM bignum2scheme_bignum(mpz_t n)
{
  SCM z;

  NEWCELL(z, bignum);
  mpz_init_set(BIGNUM_VAL(z), n);
  return z;
}

static Inline SCM bignum2integer(mpz_t n)
{
  return MAKE_INT(mpz_get_si(n));
}

static Inline SCM bignum2number(mpz_t n)  /* => int or bignum */
{
  return (BIGNUM_FITS_INTEGER(n)) ? bignum2integer(n): bignum2scheme_bignum(n);
}

static Inline double bignum2double(mpz_t n)
{
  /* I do not use the function mpz_get_d since it gives an unspecified value
   * when converting a number which is +inf or -inf
   */
 char *s = STk_must_malloc_atomic(mpz_sizeinbase(n, 10) + 2);
 return atof(mpz_get_str(s, 10, n));
}


static Inline double scheme_bignum2double(SCM b)
{
  return bignum2double(BIGNUM_VAL(b));
}


static Inline SCM scheme_bignum2real(SCM bn)
{
  return double2real(scheme_bignum2double(bn));
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

static Inline SCM rational2real(SCM r)
{
  return double2real(rational2double(r));
}

static Inline SCM real2integer(SCM r)
{
  double v = REAL_VAL(r);

  if (floor(v) != v) {
    /* This is not an inexact integer (weak test) */
    STk_error("bad number (~s) in an integer division", r);
  }
  return double2integer(v);
}

void STk_double2Cstr(char *buffer, double n)
{
  sprintf(buffer, "%.*g", real_precision, n);
  if (strchr(buffer, '.') == NULL && strchr(buffer, 'e') == NULL)
    strcat(buffer, ".0");
  /* Treat special cases of +nan.0 and +inf.0 */
  if (isalpha(buffer[0])) {
    if (strcmp(buffer, "inf.0") == 0) strcpy(buffer, "+inf.0");
    if (strcmp(buffer, "nan.0") == 0) strcpy(buffer, "+nan.0");
  }
}

/* Convert a number to a C-string. Result must be freed if != from buffer */
static char *number2Cstr(SCM n, long base, char buffer[])
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
      s = STk_must_malloc(mpz_sizeinbase(BIGNUM_VAL(n), base) + 2);
      s = mpz_get_str(s, base, BIGNUM_VAL(n));
      return s;
    case tc_rational:
      {
        char *s1, *s2, *s3, tmp[100];

        s1 = number2Cstr(RATIONAL_NUM(n), base, buffer);
        s2 = number2Cstr(RATIONAL_DEN(n), base, tmp);
        s3 = STk_must_malloc(strlen(s1) + strlen(s2) + 2);
        sprintf(s3, "%s/%s", s1, s2);
        if (s2!=tmp) STk_free(s2); /*buffer will event. be deallocated by caller*/
        return s3;
      }
    case tc_complex:
      {
        char *s1, *s2, *s3, tmp[100];

        s1 = number2Cstr(COMPLEX_REAL(n), base, buffer);
        s2 = number2Cstr(COMPLEX_IMAG(n), base, tmp);
        s3 = STk_must_malloc(strlen(s1) + strlen(s2) + 3);
        sprintf(s3, "%s%s%si", s1, ((*s2 == '-') ? "": "+"), s2);
        if (s2!=tmp) STk_free(s2); /*buffer will event. be deallocated by caller*/
        return s3;
      }
    case tc_real:
      if (base != 10) STk_error("base must be 10 for this number", n);
      STk_double2Cstr(buffer, REAL_VAL(n));
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
              case tc_real:    /*already done */ ;                      break;
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
              case tc_rational: /*already done */ ;                    break;
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

    if ((- INT32_MAX - 1) <= val && val < INT32_MAX)
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
      if (val < UINT32_MAX)
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
/* Specialized version for rationals. Accepts only integer or bignums as params */
{
  mpz_t q, r;

  if (INTP(x)) {
    if (INTP(y))
      return MAKE_INT(INT_VAL(x)/INT_VAL(y));
    else
      x = long2scheme_bignum(INT_VAL(x));
  } else {
    if (INTP(y))
      y = long2scheme_bignum(INT_VAL(y));
  }
  /* Here x and y are both bignum */
  mpz_init(q); mpz_init(r);
  mpz_tdiv_qr(q, r, BIGNUM_VAL(x), BIGNUM_VAL(y));
  return bignum2number(q);
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
    if (mpz_init_set_str(tmp, s, 10L) < 0) return STk_false;
    int_part = bignum2number(tmp);
  }

  if (p3 > p2) {        /* compute decimal part as a rational 0.12 => 6/5 */
    SCM num, den;

    if (mpz_init_set_str(tmp, p2, 10L) < 0) return STk_false;
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

  /* now return (int_part + fract_part) * exp_part */
  return mul2(add2(int_part, fract_part), exp_part);
}


/*
 * SRFI-169.
 * remove_underscores will remove all underscores from a number represented
 * as string, while also checking wether the string conforms to SRFI-169
 * (no double underscores, no leading or trailing underscores, and no
 * underscore close to anything that is not a digit).
 */
static int remove_underscores(char *str, char *end, long base) {
  char *q;
  int just_saw_one = 0;
  for (char *p=str; p<end-1; p++)
    if (*p=='_') {

      /* SRFI-169: no double underscores */
      if (just_saw_one) return 0;
      just_saw_one = 1;

      if ((p>str) && (! digitp(*(p-1),base))) return 0; /* SRFI-169: no '_' adjacent to dot. */
      if (!digitp(*(p+1),base))               return 0; /* SRFI-169: no '_' adjacent to dot. */

      for (q=p; q<end; q++) {
        *q=*(q+1);
      }
      p--;
      end = q;
    } else
      just_saw_one = 0;

  if (*(end-1)=='_') return 0;  /* SRFI-169 forbids trailing '_' */
  return 1;
}


static SCM read_integer_or_real(char *str, long base, char exact_flag, char **end)
{
  int adigit=0, isint=1;
  char saved_char = '\0', *p = str, *p1, *p2, *p3, *p4;
  SCM res;

  /* see function compute_exact_real for the meaning of these pointers */
  p1 = p2 = p3 = p4 = NULL;

  if (*p == '-' || *p == '+') p+=1;
  if (*p == '#') return STk_false;
  if (*p == '_') return STk_false; /* SRFI-169 forbids _ in leading position. */

  /* the  ( || *p=='_' ) in the rest of this function implements SRFI-169. */
  while(digitp(*p, base) || *p=='_') { p+=1; adigit=1; if (*p == '#') isint = 0; }

  if (adigit) p1 = p;           /* p1 = end of integral part */

  if (*p=='.') {
    isint = 0; p += 1;
    p2 = p;
    while(digitp(*p, base) || *p=='_') { p+=1; adigit=1; }
    p3 = p;
  }

  if (!adigit) return STk_false;

  if (*p && strchr("eEsSfFdDlL", *p)) {
    isint = 0;
    p += 1;
    p4 = p;
    if (*p == '-' || *p == '+') p+=1;
    if (!(digitp(*p, base)|| *p=='_')) return STk_false;
    p+=1;
    while (digitp(*p, base)|| *p=='_') p+=1;
  }
  if (*p) {
    /* Patch the end of the number with a '\0' (will be restored on exit) */
    saved_char = *p;
    *p = '\0';
  }

  /* SRFI-169: we have already accepted the number with underscores, now
   *  remove_underscores will validate their positions and remove them
   */
  if (strchr(str, '_')) {
    if (!use_srfi_169) return STk_false;
    if (!remove_underscores(str,p,base))
      return STk_false;
  }

  if (isint) {
    /* We are sure to have an integer. Read it as a bignum and see if we can
     * convert it in smallnum after that. Small numbers (those with few
     * digits expressed in base 10) are not read as bignums.
     * This optimisation is easily missed (e.g. 000000000000000001 will be
     * read as a bignum), but it avoids allocation for current numbers
     * represented in an usual form.
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


static SCM read_rational(SCM num, char *str, long base, char exact_flag, char **end)
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
    return  (div2(num,den));

  STk_error("cannot make rational with ~S and ~S", num, den);

  return STk_false;             /* never reached */
}

SCM STk_Cstr2number(char *str, long base)
{
  int i, exact, radix, polar, is_signed;
  char *p = str;
  SCM num1, num2;

  is_signed = 0;

  if ((*str == '-' || *str == '+')) {
    is_signed = 1;

    if (isalpha(str[1])) {
      /* Treat special values "+inf.0" -inf.0 and "NaN" as well as +i and -i */
      if (strcmp(str, MINUS_INF)==0) return double2real(minus_inf);
      if (strcmp(str, PLUS_INF)==0)  return double2real(plus_inf);
      if (strcmp(str, MINUS_NaN)==0) return double2real(make_nan(1,0,0));
      if (strcmp(str, PLUS_NaN)==0)  return double2real(make_nan(0,0,0));
      if (strcmp(str, "+i")==0)      return make_complex(MAKE_INT(0), MAKE_INT(+1UL));
      if (strcmp(str, "-i")==0)      return make_complex(MAKE_INT(0), MAKE_INT(-1UL));
    }
  }

  exact = ' ', radix = 0;
  for (i = 0; i < 2; i++) {
    if (*p == '#') {
      p += 1;
      switch (*p++) {
        case 'e': if (exact == ' ') { exact = 'e'; break; }  else return STk_false;
        case 'i': if (exact == ' ') { exact = 'i'; break; }  else return STk_false;
        case 'b': if (!radix) {base = 2;  radix = 1; break;} else return STk_false;
        case 'o': if (!radix) {base = 8;  radix = 1; break;} else return STk_false;
        case 'd': if (!radix) {base = 10; radix = 1; break;} else return STk_false;
        case 'x': if (!radix) {base = 16; radix = 1; break;} else return STk_false;
        default:  return STk_false;
      }
      str += 2;
    }
    if (*p != '#') break;
  }

  num1 = read_integer_or_real(p, base, exact, &p);
  if (num1 == STk_false) return STk_false;

  if (*p == '/')
    num1 = read_rational(num1, p+1, base, exact, &p);

  if ((*p == '+') || (*p == '-') || (*p == '@')) {
    /* Start to read a complex number */
    if (*p == '+' && p[1] == 'i') {
      p   += 2;
      num1 = make_complex(num1, MAKE_INT(1UL));   /* special case ...+i */
    }
    else if (*p == '-' && p[1] == 'i') {
      p    += 2;
      num1  = make_complex(num1, MAKE_INT(-1UL)); /* special case ...-i */
    }
    else {                                      /* general case ....[+-@]... */
      polar = (*p == '@') ? (p++,1) : 0;

      num2 = read_integer_or_real(p, base, exact, &p);
      if (num2 == STk_false) return STk_false;

      if (*p == '/') {
        /* Second member of complex number is a rational */
        num2 = read_rational(num2, p+1, base, exact, &p);
        if (num2 == STk_false) return STk_false;
      }

      if (polar) {
        num1 = make_polar(num1, num2);
      } else {
        if (*p == 'i') {
          num1 = make_complex(num1, num2);
          p += 1;
        }
      }
    }
  } else if (*p == 'i' && is_signed) {
    /* We had a number of the form '{+|-}...i' */
    p   += 1;
    num1 = make_complex(MAKE_INT(0), num1);
  }

  return (*p) ? STk_false : num1;
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
 * predicate is true of a number then all higher type predicates are
 * also true of that number. Consequently, if a type predicate is
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
    case tc_complex: return MAKE_BOOLEAN(zerop(COMPLEX_IMAG(x)));
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
 * (bignum? (expt 2 300))     => |#t|   (very likely)
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
<doc  = < > <= >=
 * (= z1 z2 z3 ...)
 * (< x1 x2 x3 ...)
 * (> x1 x2 x3 ...)
 * (<= x1 x2 x3 ...)
 * (>= x1 x2 x3 ...)
 *
 * These procedures return |#t| if their arguments are (respectively):
 * equal, monotonically increasing, monotonically decreasing,
 * monotonically nondecreasing, or monotonically nonincreasing.
 * @lisp
 * (= +inf.0 +inf.0)           =>  #t
 * (= -inf.0 +inf.0)           =>  #f
 * (= -inf.0 -inf.0)           =>  #t
 * @l
 * For any finite real number x:
 * @l
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
      if (_max_type_(*argv) == STk_false) error_bad_number(*argv);          \
                                                                            \
      for (previous = *argv--; --argc; previous = *argv--) {                \
        if (_max_type_(*argv) == STk_false) error_bad_number(*argv);        \
        if (do_compare(previous, *argv) _operator_ 0) return STk_false;     \
      }                                                                     \
      return STk_true;                                                      \
    }


#define COMPARE_NUM2(_prim_, _max_type_, _operator_)                        \
    long STk_##_prim_##2(SCM o1, SCM o2)                                    \
    {                                                                       \
      if (_max_type_(o1) == STk_false) error_bad_number(o1);                \
      if (_max_type_(o2) == STk_false) error_bad_number(o2);                \
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
static Inline int is_oddp(SCM n)
{
  switch (TYPEOF(n)) {
    case tc_integer: return INT_VAL(n) & 1;
    case tc_bignum:  return mpz_odd_p(BIGNUM_VAL(n));
    default:         error_bad_number(n);
  }
  return FALSE; /* never reached */
}

static int zerop(SCM n)
{
  switch (TYPEOF(n)) {
    case tc_integer:  return (INT_VAL(n) == 0);
    case tc_real:     return (REAL_VAL(n) == 0.0);
    case tc_bignum:   return (mpz_cmp_si(BIGNUM_VAL(n), 0L) == 0);
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
    case tc_bignum:   return (mpz_cmp_si(BIGNUM_VAL(n), 0L) > 0);
    case tc_rational: return positivep(RATIONAL_NUM(n));
    case tc_complex:  error_not_on_a_complex(n); break;
    default:          error_bad_number(n);
  }
  return FALSE; /* never reached */
}


static int negativep(SCM n)
{
  switch (TYPEOF(n)) {
    case tc_integer:  return (INT_VAL(n) < 0);
    case tc_real:     return (REAL_VAL(n) < 0.0);
    case tc_bignum:   return (mpz_cmp_si(BIGNUM_VAL(n), 0L) < 0);
    case tc_rational: return negativep(RATIONAL_NUM(n));
    case tc_complex:  error_not_on_a_complex(n); break;
    default:          error_bad_number(n);
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


DEFINE_PRIMITIVE("finite?", finitep, subr1, (SCM n))
{
  return MAKE_BOOLEAN(finitep(n));
}


DEFINE_PRIMITIVE("infinite?", infinitep, subr1, (SCM n))
{
  return MAKE_BOOLEAN(!finitep(n));
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
  return MAKE_BOOLEAN(is_oddp(n));
}


DEFINE_PRIMITIVE("even?", evenp, subr1, (SCM n))
{
  return MAKE_BOOLEAN(!is_oddp(n));
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
  switch (TYPEOF(z)) {
    case tc_complex: return MAKE_BOOLEAN(STk_nanp(COMPLEX_REAL(z)) == STk_true ||
                                         STk_nanp(COMPLEX_IMAG(z)) == STk_true);
    case tc_real:     return MAKE_BOOLEAN(isnan(REAL_VAL(z)));
    case tc_rational:
    case tc_bignum:
    case tc_integer:  return STk_false;
    default:          error_bad_number(z); return STk_void;
  }
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
 * ,(bold "Note:") If any argument is inexact, then the result will also be
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
    error_bad_number(*argv);
  }

  exactp = isexactp(*argv);

  for (res = *argv--; --argc; argv--) {
    /* See that the argument is a correct number */
    if (STk_realp(*argv) == STk_false) error_bad_number(*argv);

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
    error_bad_number(*argv);
  }

  exactp = isexactp(*argv);

  for (res = *argv--; --argc; argv--) {
    /* See that the argument is a correct number */
    if (STk_realp(*argv) == STk_false) error_bad_number(*argv);

    /* determine if result should be exact or not */
    if (!isexactp(*argv)) exactp = 0;

    /* compute max */
    if (do_compare(res, *argv) > 0) res = *argv;
  }
  return (!exactp && isexactp(res)) ? exact2inexact(res) : res;
}


/*
<doc    + *
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
 * ,(bold "Note:") For any finite number z:
 * @lisp
 * (+ +inf.0 z)            =>  +inf.0
 * (+ -inf.0 z)            =>  -inf.0
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

          o1 = make_complex(sub2(mul2(r1,r2), mul2(i1, i2)),
                            add2(mul2(r1,i2), mul2(r2, i1)));
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
 * (/ 0)                   => error (division by 0)
 * @end lisp
doc>
 */
SCM STk_sub2(SCM o1, SCM o2)
{
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
    case tc_complex:
      {
        SCM tmp, new_r, new_i;

        if (!zerop(o1)) {
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
      }
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
 * (abs x)
 *
 * |Abs| returns the absolute value of its argument.
 * @lisp
 * (abs -7)                =>  7
 * (abs -inf.0)            => +inf.0
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("abs", abs, subr1, (SCM x))
{
  switch (TYPEOF(x)) {
    case tc_integer:  return (INT_VAL(x) < 0) ? MAKE_INT(-INT_VAL(x)) : x;
    case tc_bignum:   if (mpz_cmp_ui(BIGNUM_VAL(x), 0L) < 0) {
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
    default:          error_bad_number(x);
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
static void integer_division(SCM x, SCM y, SCM *quotient, SCM* remainder)
{
  mpz_t q, r;
  int exact = 1;

  if (!INTP(x) && !BIGNUMP(x) && !REALP(x)) error_bad_number(x);
  if (!INTP(y) && !BIGNUMP(y) && !REALP(y)) error_bad_number(y);
  if (zerop(y))                             error_divide_by_0(x);

  if (REALP(x)) { x = real2integer(x); exact = 0; }
  if (REALP(y)) { y = real2integer(y); exact = 0; }

  /* Here, x and y can only be integer or bignum (not real) */
  if (INTP(x)) {
    if (INTP(y)) {
      long int i1 = INT_VAL(x);
      long int i2 = INT_VAL(y);

      if (exact) {
        *quotient  = MAKE_INT(i1 / i2);
        *remainder = MAKE_INT(i1 % i2);
      } else {
        *quotient  = double2real((double) (i1 / i2));
        *remainder = double2real((double) (i1 % i2));
      }
      return;
    }
    else
      x = long2scheme_bignum(INT_VAL(x));
  } else {
    /* x is a bignum */
    if (INTP(y))
      y = long2scheme_bignum(INT_VAL(y));
  }

  /* Here x and y are both bignum */
  mpz_init(q); mpz_init(r);
  mpz_tdiv_qr(q,r,BIGNUM_VAL(x),BIGNUM_VAL(y));
  if (exact) {
    *quotient  = bignum2number(q);
    *remainder = bignum2number(r);
  } else {
    /*     *quotient  = double2real(mpz_get_d(q)); */
    /*     *remainder = double2real(mpz_get_d(r)); */
    *quotient  = double2real(bignum2double(q));
    *remainder = double2real(bignum2double(r));
  }
  mpz_clear(q); mpz_clear(r);                          /* //FIXME: TESTER */
}

DEFINE_PRIMITIVE("quotient", quotient, subr2, (SCM n1, SCM n2))
{
  SCM q, r;

  integer_division(n1, n2, &q, &r);
  return q;
}


DEFINE_PRIMITIVE("remainder", remainder, subr2, (SCM n1, SCM n2))
{
  SCM q, r;

  integer_division(n1, n2, &q, &r);
  return r;
}


DEFINE_PRIMITIVE("modulo", modulo, subr2, (SCM n1, SCM n2))
{
  SCM q, r;

  integer_division(n1, n2, &q, &r);
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
static SCM gcd2(SCM n1, SCM n2)
{
  return zerop(n2) ? n1 : gcd2(n2, STk_modulo(n1, n2));
}

DEFINE_PRIMITIVE("gcd", gcd, vsubr, (int argc, SCM *argv))
{
  SCM res;

  if (argc == 0) return MAKE_INT(0);
  if (argc == 1) return gcd2(*argv, MAKE_INT(0));

  for (res = *argv--; --argc; argv--)
    res = absolute(gcd2(res, *argv));

  return res;
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
 * ,(bold "Rationale:") |Round| rounds to even for consistency with the default
 * rounding mode specified by the IEEE floating point standard.
 * @l
 * ,(bold "Note:") If the argument to one of these procedures is inexact, then the
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
                        return STk_quotient(tmp, RATIONAL_DEN(x));
                      }
    case tc_bignum:
    case tc_integer:  return x;
    default:          error_bad_number(x);
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
                        return STk_quotient(tmp, RATIONAL_DEN(x));
                      }
    case tc_bignum:
    case tc_integer:  return x;
    default:          error_bad_number(x);
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
    case tc_rational: return STk_quotient(RATIONAL_NUM(x), RATIONAL_DEN(x));
    case tc_bignum:
    case tc_integer:  return x;
    default:          error_bad_number(x);
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
                        SCM tmp;

                        if (RATIONAL_DEN(x) == MAKE_INT(2)) {
                          tmp = (negativep(RATIONAL_NUM(x))) ?
                                   sub2(RATIONAL_NUM(x), MAKE_INT(1)):
                                   add2(RATIONAL_NUM(x), MAKE_INT(1));
                          return mul2(STk_quotient(tmp, MAKE_INT(4)), MAKE_INT(2));
                        }
                        tmp = make_rational(add2(mul2(RATIONAL_NUM(x), MAKE_INT(2)),
                                                 RATIONAL_DEN(x)),
                                            mul2(RATIONAL_DEN(x), MAKE_INT(2)));
                        return STk_floor(tmp);
                      }
    case tc_bignum:
    case tc_integer:  return x;
    default:          error_bad_number(x);
  }
  return STk_void; /* never reached */
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


static SCM my_log(SCM z)
{
  if (!COMPLEXP(z) && negativep(z) && finitep(z))
    return make_complex(my_log(sub2(MAKE_INT(0), z)), double2real(MY_PI));


  switch (TYPEOF(z)) {
    case tc_integer:  if (z == MAKE_INT(0)) STk_error("value is not defined for 0");
                      if (z == MAKE_INT(1)) return MAKE_INT(0);
                      return double2real(log((double) INT_VAL(z)));
    case tc_bignum:   return double2real(log(scheme_bignum2double(z)));
    case tc_rational: return double2real(log(rational2double(z)));
    case tc_real:     return double2real(log(REAL_VAL(z)));
    case tc_complex:  return make_complex(my_log(STk_magnitude(z)),
                                          STk_angle(z));
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


static Inline SCM acos_complex(SCM z)
{
  return mul2(Cmake_complex(MAKE_INT(0), MAKE_INT(-1UL)),
              my_log(add2(z,
                          mul2(Cmake_complex(MAKE_INT(0), MAKE_INT(1UL)),
                               STk_sqrt(sub2(MAKE_INT(1UL),
                                             mul2(z, z)))))));
}

static SCM acos_real(double d)
{
  return (-1 < d && d < 1) ? double2real(acos(d)) : acos_complex(double2real(d));
}



static SCM my_acos(SCM z)
{
  switch (TYPEOF(z)) {
    case tc_integer:  if (z == MAKE_INT(0)) return div2(double2real(MY_PI),
                                                        MAKE_INT(2));
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
                          STk_error("value ~S is out of range", z);
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
  return double2real(atan2(REAL_VAL(exact2inexact(STk_real_part(y))),
                           REAL_VAL(exact2inexact(STk_real_part(x)))));
}


/*=============================================================================*/

#define transcendental(name)                            \
  DEFINE_PRIMITIVE(#name, name, subr1, (SCM z))         \
  {                                                     \
     return my_##name(z);                               \
  }

transcendental(exp)
transcendental(sin)
transcendental(cos)
transcendental(tan)
transcendental(asin)
transcendental(acos)

DEFINE_PRIMITIVE("log", log, subr12, (SCM x, SCM b))
{
    return (b)? div2(my_log(x),my_log(b)) : my_log(x);
}


DEFINE_PRIMITIVE("atan", atan, subr12, (SCM y, SCM x))
{
  return (x)? my_atan2(y, x) : my_atan(y);
}

/*=============================================================================*/

/*
<doc sqrt
 * (sqrt z)
 *
 * Returns the principal square root of |z|. The result will have either
 * positive real part, or zero real part and non-negative imaginary part.
doc>
 */

static SCM my_sqrt_exact(SCM x)
{
  if (zerop(x))     return MAKE_INT(0);
  if (negativep(x)) return Cmake_complex(MAKE_INT(0),
                                         my_sqrt_exact(mul2(MAKE_INT(-1UL), x)));
  if (INTP(x)) {
    int    i = INT_VAL(x);
    double d = (double) sqrt((double) i);

    return ((int) d * (int) d == i)? MAKE_INT((int) d) : double2real(d);
  } else { /* This is a bignum */
    mpz_t root, tmp;
    SCM res;

    mpz_init(root);
    mpz_sqrt(root, BIGNUM_VAL(x));

    mpz_init(tmp);
    mpz_mul(tmp, root, root);
    res = (mpz_cmp(tmp, BIGNUM_VAL(x))==0) ? bignum2number(root) :
                                             STk_sqrt(scheme_bignum2real(x));
    mpz_clear(root); mpz_clear(tmp);
    return res;
  }
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
    case tc_complex:  return make_polar(STk_sqrt(STk_magnitude(z)),
                                        div2(STk_angle(z), MAKE_INT(2)));
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
 * ,(bold "Note:") |0,(sup "z")| is 1 if |z = 0| and |0| otherwise.
doc>
 */

static Inline SCM exact_expt(SCM x, SCM y)
{
  SCM nx, ny, res = MAKE_INT(1);

  if (zerop(y)) return MAKE_INT(1);
  if (zerop(x) || (x == MAKE_INT(1))) return x;

  while (y != MAKE_INT(1)) {
    nx = mul2(x, x);
    ny = STk_quotient(y, MAKE_INT(2));
    if (STk_evenp(y) == STk_false) res = mul2(x, res);
    x = nx;
    y = ny;
  }

  return mul2(res, x);
}

static SCM my_expt(SCM x, SCM y)
{
  /* y is >= 0 */
  switch (TYPEOF(y)) {
    case tc_integer:
    case tc_bignum:   return exact_expt(x, y);
    case tc_rational:
    case tc_real:     if (zerop(y)) return double2real(1.0);
                      if (zerop(x)) return (x==MAKE_INT(0)) ? x : double2real(0.0);
                      /* FALLTHROUGH */
    case tc_complex:  return my_exp(mul2(my_log(x),y));
    default:          error_cannot_operate("expt", x, y);
  }
  return STk_void; /* never reached */
}


DEFINE_PRIMITIVE("expt", expt, subr2, (SCM x, SCM y))
{
  if (negativep(y)) return div2(MAKE_INT(1),
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
 * |z = x1 + x2.i = x3 . e,(sup "i.x4")|
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
 * |-,(symbol "pi") < xa <= ,(symbol "pi")| with |xa = x4 + 2,(symbol "pi")n|
 * for some integer n.
 * @lisp
 * (angle +inf.0)                 => 0.0
 * (angle -inf.0)                 => 3.14159265358979
 * @end lisp
 * @l
 * ,(bold "Note:") |Magnitude| is the same as |abs| for a real argument.
doc>
 */

DEFINE_PRIMITIVE("magnitude", magnitude, subr1, (SCM z))
{
  switch (TYPEOF(z)) {
    case tc_integer:
    case tc_bignum:
    case tc_rational:
    case tc_real:     return absolute(z);
    case tc_complex: {
                        SCM r = COMPLEX_REAL(z);
                        SCM i = COMPLEX_IMAG(z);

                        return STk_sqrt(add2(mul2(r, r), mul2(i, i)));
                      }
    default:          error_bad_number(z);
  }
  return STk_void; /* never reached */
}

DEFINE_PRIMITIVE("angle", angle, subr1, (SCM z))
{
  switch (TYPEOF(z)) {
    case tc_integer:
    case tc_bignum:
    case tc_rational: return positivep(z) ? MAKE_INT(0) : double2real(MY_PI);
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
  if (STk_realp(r) == STk_false) error_bad_number(r);
  if (STk_realp(i) == STk_false) error_bad_number(i);
  return make_complex(r, i);
}


DEFINE_PRIMITIVE("make-polar", make_polar, subr2, (SCM a, SCM m))
{
  if (STk_realp(a) == STk_false) error_bad_number(a);
  if (STk_realp(m) == STk_false) error_bad_number(m);

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
                        return Cmake_complex(inexact2exact(COMPLEX_REAL(z)),
                                             inexact2exact(COMPLEX_IMAG(z)));
                      else return z;
    case tc_real:     return double2rational(REAL_VAL(z));
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
 * ,(bold "Note:") The error case can occur only when |z| is not a complex number or
 * is a complex number with a non-rational real or imaginary part.
 * @l
 * ,(bold "Rationale:") If |z| is an inexact number represented using flonums, and
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

  s = number2Cstr(n, b, buffer);
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
 * The decode_flonum function is taken from Shiro Kawai Gauche
 * implementation
 *   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
 *
 * Decompose flonum D into an integer mantissa F and exponent E, where
 *   -1022 <= E <= 1023,
 *    0 <= abs(F) < 2^53
 *    D = F * 2^(E - 53)
 * Some special cases:
 *    F = 0, E = 0 if D = 0.0 or -0.0
 *    F = #t if D is infinity (positive or negative)
 *    F = #f if D is NaN.
 * If D is normalized number, F >= 2^52.
 *
 * Cf. IEEE 754 Reference
 * http://babbage.cs.qc.edu/courses/cs341/IEEE-754references.html
 */
union ieee_double {
    double d;
    struct {
#ifdef WORDS_BIGENDIAN
#if SIZEOF_LONG >= 8
        unsigned int sign:1;
        unsigned int exp:11;
        unsigned long mant:52;
#else  /*SIZEOF_LONG < 8*/
        unsigned int sign:1;
        unsigned int exp:11;
        unsigned long mant0:20;
        unsigned long mant1:32;
#endif /*SIZEOF_LONG < 8*/
#else  /*!WORDS_BIGENDIAN*/
#if SIZEOF_LONG >= 8
        unsigned long mant:52;
        unsigned int  exp:11;
        unsigned int  sign:1;
#else  /*SIZEOF_LONG < 8*/
        unsigned long mant1:32;
        unsigned long mant0:20;
        unsigned int  exp:11;
        unsigned int  sign:1;
#endif /*SIZEOF_LONG < 8*/
#endif /*!WORDS_BIGENDIAN*/
    } components;
};

static SCM decode_flonum(double d, int *exp, int *sign)
{
  union ieee_double dd;
  SCM f;

  dd.d = d;

  /* Check exceptional cases */
  if (dd.components.exp == 0x7ff) {
    *exp = 0;
    if (
#if SIZEOF_LONG >= 8
        dd.components.mant == 0
#else
        dd.components.mant0 == 0 && dd.components.mant1 == 0
#endif
        ) {
      return STk_true;  /* infinity */
    } else {
      return STk_false; /* NaN */
    }
  }

  *exp  = (dd.components.exp? dd.components.exp - 0x3ff - 52 : -0x3fe - 52);
  *sign = (dd.components.sign? -1 : 1);

#if SIZEOF_LONG >= 8
  {
    unsigned long lf = dd.components.mant;
    if (dd.components.exp > 0) {
      lf += (1L<<52);     /* hidden bit */
    }
    f = STk_ulong2integer(lf);
  }
#else
  {
    unsigned long v0 = dd.components.mant0;
    unsigned long v1 = dd.components.mant1;

    if (dd.components.exp > 0) {
      v0 += (1L<<20); /* hidden bit */
    }
    f = add2(mul2(STk_ulong2integer(v0),
                  STk_mul2(MAKE_INT(1<<16), MAKE_INT(1<<16))), /* (expt 2 32) */
             STk_ulong2integer(v1));
  }
#endif
  return f;
}

/*
<doc EXT decode-float
 * (decode-float n)
 *
 * |decode-float| returns three exact integers: |significand|, |exponent|
 * and |sign| (where |-1 <= sign <= 1|). The values returned by |decode-float|
 * satisfy:
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
DEFINE_PRIMITIVE("decode-float", decode_float, subr1, (SCM n))
{
  SCM tmp;
  int exp, sign=0;

  if (!NUMBERP(n)) error_bad_number(n);
  if (COMPLEXP(n)) STk_error("real number expected. It was ~S", n);
  if (EXACTP(n)) n = exact2inexact(n);

  tmp = decode_flonum(REAL_VAL(n), &exp, &sign);
  return STk_n_values(3,
                      tmp,
                      MAKE_INT(exp),
                      MAKE_INT(sign));
}


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
 * SRFI 28: NaN procedures
 */

static void verify_NaN(SCM n) {
  if ((TYPEOF(n) != tc_real) || !isnan(REAL_VAL(n)))
    STk_error("bad NaN value: ~S", n);
}



DEFINE_PRIMITIVE("%make-nan", make_nan, subr3, (SCM neg, SCM quiet, SCM payload)) {
  if (!INTP(payload) || ((uint64_t) INT_VAL(payload) > payload_mask))
    STk_error("bad payload ~S", payload);
  return double2real(make_nan(neg != STk_false,
                              quiet != STk_false,
                              INT_VAL(payload)));
}


/*
<doc EXT nan-negative?
 * (nan-negative? nan)
 *
 * returns #t if the sign bit of |nan| is set and #f otherwise.
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
 * returns #t  if |nan| is a quiet NaN.
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
 * Returns #t if |nan1| and |nan2| have the same sign, quiet bit,
 * and payload; and #f otherwise.
*/
DEFINE_PRIMITIVE("nan=?", nan_equalp, subr2, (SCM n1, SCM n2)) {
  union binary64 tmp1, tmp2;

  verify_NaN(n1);
  verify_NaN(n2);
  tmp1.d = REAL_VAL(n1);
  tmp2.d = REAL_VAL(n2);
  return MAKE_BOOLEAN(tmp1.u ==tmp2.u);
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
  STk_NaN   =  strtod("NAN", NULL);

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

  /* register the extended types types for numbers */
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

  /* SRFI 208: NaN procedures */
  ADD_PRIMITIVE(make_nan);
  ADD_PRIMITIVE(nan_negativep);
  ADD_PRIMITIVE(nan_quietp);
  ADD_PRIMITIVE(nan_payload);
  ADD_PRIMITIVE(nan_equalp);

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
