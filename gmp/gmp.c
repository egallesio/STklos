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
 * Last file update: 25-Oct-2009 23:06 (eg)
 */

#include <stdio.h>
#include <ctype.h>

#include "gmp.h"


#if 0
static void trace_bignum(mpz_t bn)
{
  char *buffer = alloca(mp_radix_size(bn, 10) + 10);
  
  mp_toradix(bn, (unsigned char *)buffer, 10);
  fprintf(stderr, " [%s] ", buffer);
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

void mpz_init_set_si(mpz_t bn, signed long int si)
{
  mp_init(bn); 
  mp_set_int(bn, (long) si);
}
#endif

int mpz_init_set_str(mpz_t bn, char *s, long base)
{
  mp_init(bn);
  return (mp_read_radix(bn, (unsigned char *) s, (int) base) == MP_YES)? 0 : -1;
}

void mpz_init_set_ui(mpz_t bn, unsigned long int ui)
{
  char buffer[100];

  snprintf(buffer, 100, "%ld", ui);
  mpz_init_set_str(bn, buffer, 10L);
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
  char buffer[100];
  
  mp_toradix(bn, (unsigned char *)buffer, 10);
  return atol(buffer);
}

unsigned long int mpz_get_ui(mpz_t bn)
{
  char buffer[100];
   
  mp_toradix(bn, (unsigned char *)buffer, 10);
  return atof(buffer);
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
int mpz_cmp_si(mpz_t bn, long v)
{
  return mp_cmp_int(bn, v);
}

int mpz_cmp(mpz_t a, mpz_t b)
{
  return mp_cmp(a, b);
}

int mpz_sgn(mpz_t a)
{
  return mp_cmp_z(a);
}
#endif


int mpz_cmp_ui(mpz_t bn, unsigned long int ui)
{
  char buffer[100];
  mpz_t tmp;
  int res;

  snprintf(buffer, 100, "%ld", ui);
  mpz_init_set_str(tmp, buffer, 10);
  res = mp_cmp(bn, tmp);
  mp_clear(tmp);
  
  return res;
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




