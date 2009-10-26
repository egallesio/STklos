/*
 * gmp.h	-- A look like GMP library
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
 *    Creation date: 12-Oct-2009 19:29 (eg)
 * Last file update: 26-Oct-2009 17:57 (eg)
 */


#ifndef _GMPLITE_H
#  define _GMPLITE_H

# define GMP_USE_MACROS 1

#include <stdlib.h>
#include "mpi.h"


typedef mp_int mpz_t[1];

extern void *(*_gmp_alloc)(size_t);
extern void (*_gmp_free)(void*, size_t);

/* ----------------------------------------------------------------------
 * Memory functions
 * ---------------------------------------------------------------------- */
void mp_set_memory_functions(void *(*allocate) (size_t),
			     void *(*reallocate) (void *, size_t, size_t),
			     void (*deallocate) (void *, size_t));

/* ----------------------------------------------------------------------
 * Initialisations
 * ---------------------------------------------------------------------- */

#ifndef GMP_USE_MACROS
void mpz_init(mpz_t bn);
void mpz_init_set(mpz_t bn1, mpz_t bn2);
void mpz_init_set_si(mpz_t bn, signed long int si);
#else
#  define mpz_init(bn)  		mp_init(bn);
#  define mpz_init_set(bn1, bn2) 	{ mp_init(bn1); mp_copy(bn2, bn1); }
#  define mpz_init_set_si(bn, si)   	{ mp_init(bn); mp_set_int(bn, (long) si); }
#endif


int mpz_init_set_str(mpz_t bn, char *s, long base);
void mpz_init_set_ui(mpz_t bn, unsigned long int ui);

/* ----------------------------------------------------------------------
 * Free
 * ---------------------------------------------------------------------- */
#ifndef GMP_USE_MACROS
void mpz_clear(mpz_t bn);
#else 
#  define  mpz_clear(bn) 	mp_clear(bn)
#endif

/* ----------------------------------------------------------------------
 * Getters
 * ---------------------------------------------------------------------- */
signed long int mpz_get_si(mpz_t bn);
unsigned long int mpz_get_ui(mpz_t bn);
char *mpz_get_str(char *str, int base, mpz_t bn);

/* ----------------------------------------------------------------------
 * Comparison
 * ---------------------------------------------------------------------- */
#ifndef GMP_USE_MACROS
int mpz_cmp_si(mpz_t bn, long v);
int mpz_cmp(mpz_t a, mpz_t b);
int mpz_sgn(mpz_t a)
#else 
#  define mpz_cmp_si(bn, v)	mp_cmp_int(bn, v)
#  define mpz_cmp(a, b)	mp_cmp(a, b)
#  define mpz_sgn(a)		mp_cmp_z(a)
#endif
int mpz_cmp_ui(mpz_t bn, unsigned long int ui);

/* ----------------------------------------------------------------------
 * Operations 
 * ---------------------------------------------------------------------- */
#ifndef GMP_USE_MACROS
void mpz_add(mpz_t res, mpz_t a, mpz_t b);
void mpz_sub(mpz_t res, mpz_t a, mpz_t b);
void mpz_mul(mpz_t res, mpz_t a, mpz_t b);
void mpz_tdiv_qr(mpz_t q, mpz_t r, mpz_t a, mpz_t b);
void mpz_neg(mpz_t res, mpz_t a);
void mpz_sqrt(mpz_t res, mpz_t a);
#else
#  define mpz_add(res, a, b)	   mp_add(a, b, res)
#  define mpz_sub(res, a, b)	   mp_sub(a, b, res)
#  define mpz_mul(res, a, b)	   mp_mul(a, b, res)
#  define mpz_tdiv_qr(q, r, a, b)   mp_div(a, b, q, r);
#  define mpz_neg(res, a)	   mp_neg(a, res);
#  define mpz_sqrt(res, a)	   mp_sqrt(a, res);
#endif
void mpz_ui_pow_ui(mpz_t bn, unsigned long int base, unsigned long int exp);


/* ----------------------------------------------------------------------
 * Misc 
 * ---------------------------------------------------------------------- */
#ifndef GMP_USE_MACROS
size_t mpz_sizeinbase(mpz_t bn, int base);
int mpz_odd_p(mpz_t bn);
#else
#  define  mpz_sizeinbase(bn, base)  mp_radix_size(bn, base)
#  define  mpz_odd_p(bn)	    mp_isodd(bn)
#endif
#endif /* _GMPLITE_H */
