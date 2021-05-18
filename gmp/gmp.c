/*
 * gmp.c	-- The GMP wrapper
 *
 * Copyright © 2009 Erick Gallesio - Polytech'Nice-Sophia <eg@unice.fr>
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
 *    Creation date: 12-Oct-2009 19:27 (eg)
 * Last file update: 28-Oct-2009 15:21 (eg)
 */

#include <stdio.h>
#include <ctype.h>

#include "gmp.h"

#define MAXBUF		100   /* size for temporary string buffer */

/*
  MPI doesn't work correctly with native 64 bits, as advertised. As a
  consequence, the wrapper functions in gmp.c are 32 bits and use a
  different strategy in presence of values which cannot be represented
  with 32 bits (conversion to/from string). This is slow, but it works.

  We know that we have a 64 bits value when the value is ouside the
  range [MIN32_SI, MAX32_SI]. In this case, we pass by the decimal
  representation of the number in a string
 */

#define MAX32_SI	2147483647
#define MIN32_SI	(-MAX32_SI + -1)
#define MAX32_UI	4294967295UL

#define bit64_si(si)	((si) < MIN32_SI || (si) > MAX32_SI)
#define bit64_ui(si)	((ui) > MAX32_UI)



#ifdef STK_DEBUG
void mpz_trace_bignum(char *msg, mpz_t bn)
{
  char *buffer = alloca(mp_radix_size(bn, 10) + 10);

  mp_toradix(bn, (unsigned char *)buffer, 10);
  fprintf(stderr, "%s[%s] ", msg, buffer);
}
#endif


static void lowerstring(char *str)
{
  while (*str) {
    *str = tolower(*str);
    str++;
  }
}

/* ----------------------------------------------------------------------
 * Memory functions
 * ---------------------------------------------------------------------- */
void mp_set_memory_functions(void *(*allocate) (size_t),
			       void *(*reallocate) (void *, size_t, size_t),
			       void (*deallocate) (void *, size_t))
{
  _gmp_alloc = allocate;
  _gmp_free  = deallocate;
}


/* ----------------------------------------------------------------------
 * Initialisations
 * ---------------------------------------------------------------------- */

#ifndef GMP_USE_MACROS
void mpz_init(mpz_t bn)
{
  mp_init(bn);
}

void mpz_init_set(mpz_t bn1, mpz_t bn2)
{
  mp_init(bn1);
  mp_copy(bn2, bn1);
}
#endif


void mpz_init_set_si(mpz_t bn, signed long int si)
{
  if (bit64_si(si)) {
    char buffer[MAXBUF];

    snprintf(buffer, MAXBUF, "%ld", si);
    mpz_init_set_str(bn, buffer, 10L);
  } else {
    mp_init(bn);
    mp_set_int(bn, (long) si);
  }
}


void mpz_init_set_ui(mpz_t bn, unsigned long int ui)
{
  char buffer[MAXBUF];

  snprintf(buffer, MAXBUF, "%lu", ui);
  mpz_init_set_str(bn, buffer, 10L);
}


int mpz_init_set_str(mpz_t bn, char *s, long base)
{
  mp_init(bn);
  return (mp_read_radix(bn, (unsigned char *) s, (int) base) == MP_YES)? 0 : -1;
}


/* ----------------------------------------------------------------------
 * Free
 * ---------------------------------------------------------------------- */
#ifndef GMP_USE_MACROS
void mpz_clear(mpz_t bn)
{
  mp_clear(bn);
}
#endif


/* ----------------------------------------------------------------------
 * Getters
 * ---------------------------------------------------------------------- */
signed long int mpz_get_si(mpz_t bn)
{
  char buffer[MAXBUF];

  mp_toradix(bn, (unsigned char *)buffer, 10);
  return strtol(buffer, NULL, 10);
}

unsigned long int mpz_get_ui(mpz_t bn)
{
  char buffer[MAXBUF];

  mp_toradix(bn, (unsigned char *)buffer, 10);
  return strtoul(buffer, NULL,10);
}


char *mpz_get_str(char *str, int base, mpz_t bn)
{
  if (!str)
    str = _gmp_alloc(mp_radix_size(bn, base) + 2);

  mp_toradix(bn, (unsigned char *)str, base);
  lowerstring(str);

  return str;
}


/* ----------------------------------------------------------------------
 * Comparison
 * ---------------------------------------------------------------------- */
#ifndef GMP_USE_MACROS
int mpz_cmp(mpz_t a, mpz_t b)
{
  return mp_cmp(a, b);
}

int mpz_sgn(mpz_t a)
{
  return mp_cmp_z(a);
}
#endif


int mpz_cmp_si(mpz_t bn, signed long si)
{
  if (bit64_si(si)) {
    char buffer[MAXBUF];
    mpz_t tmp;
    int res;

    snprintf(buffer, MAXBUF, "%ld", si);
    mpz_init_set_str(tmp, buffer, 10);
    res = mp_cmp(bn, tmp);
    mp_clear(tmp);

    return res;
  } else {
    return mp_cmp_int(bn, si);
  }
}

int mpz_cmp_ui(mpz_t bn, unsigned long int ui)
{
  if (ui > (unsigned long int) MAX32_SI) {
    char buffer[MAXBUF];
    mpz_t tmp;
    int res;

    snprintf(buffer, MAXBUF, "%lu", ui);
    mpz_init_set_str(tmp, buffer, 10);
    res = mp_cmp(bn, tmp);
    mp_clear(tmp);

    return res;
  } else {
    /* this is a small unsigned value, we can use signed comparison */
    return mp_cmp_int(bn, (signed long int) ui);
  }
}

/* ----------------------------------------------------------------------
 * Operations
 * ---------------------------------------------------------------------- */
#ifndef GMP_USE_MACROS
void mpz_add(mpz_t res, mpz_t a, mpz_t b)
{
  mp_add(a, b, res);
}

void mpz_sub(mpz_t res, mpz_t a, mpz_t b)
{
  mp_sub(a, b, res);
}


void mpz_mul(mpz_t res, mpz_t a, mpz_t b)
{
  mp_mul(a, b, res);
}


void mpz_tdiv_qr(mpz_t q, mpz_t r, mpz_t a, mpz_t b)
{
  mp_div(a, b, q, r);
}

void mpz_neg(mpz_t res, mpz_t a)
{
  mp_neg(a, res);
}

void mpz_sqrt(mpz_t res, mpz_t bn)
{
  mp_sqrt(bn, res);
}
#endif

void mpz_ui_pow_ui(mpz_t bn, unsigned long int base, unsigned long int exp)
{
  mpz_t base_bn;
  mpz_t exp_bn;

  /* convert base and exp to bignums */
  mpz_init_set_ui(exp_bn, exp);
  mpz_init_set_ui(base_bn, base);

  /* compute result */
  mp_expt(base_bn, exp_bn, bn);
}


/* ----------------------------------------------------------------------
 * Misc
 * ---------------------------------------------------------------------- */
#ifndef GMP_USE_MACROS
size_t mpz_sizeinbase(mpz_t bn, int base)
{
  return mp_radix_size(bn, base);
}

int mpz_odd_p(mpz_t bn)
{
  return mp_isodd(bn);
}
#endif




