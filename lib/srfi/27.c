/*
 * srfi-27.c   -- Implementation of SRFI-27: Sources of Random Numbers
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
 *    Creation date: 03-May-2021 15:22
 */

#include "stklos.h"
#include "struct.h"

#include <gmp.h>
#include <math.h>
#include <fcntl.h>
#include <string.h>

#include "27-incl.c"


/********
 * A CHUNK OF BIGNUM.C WAS COPIED HERE
 *
 * Do we separate these in bignum.h? Or leave it as is?
 ********/

struct bignum_obj {
  stk_header header;
  mpz_t val;
};

#define BIGNUM_VAL(p)   (((struct bignum_obj *) (p))->val)

static inline SCM bignum2scheme_bignum(mpz_t n)
{
  SCM z;

  NEWCELL(z, bignum);
  mpz_init_set(BIGNUM_VAL(z), n);
  return z;
}

static inline SCM double2real(double x)
{
  SCM z;

  NEWCELL(z, real);
  REAL_VAL(z) = x;
  return z;
}

/********
 * END OF COPY FROM BIGNUM.C
 ********/


static int tc_state_mt;



/********************************************\
 *                                          *
 *      MERSENNE TWISTER IMPLEMENTATION     *
 *                                          *
\********************************************/


/* Mersenne Twister implementation (64 bits)

   Slightly adapted for STklos (made state an argument to the functions)


   Original Copyright notice follows:

   A C-program for MT19937-64 (2014/2/23 version).
   Coded by Takuji Nishimura and Makoto Matsumoto.

   This is a 64-bit version of Mersenne Twister pseudorandom number
   generator.

   Before using, initialize the state by using init_genrand64(seed)
   or init_by_array64(init_key, key_length).

   Copyright (C) 2004, 2014, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote
        products derived from this software without specific prior written
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   References:
   T. Nishimura, ``Tables of 64-bit Mersenne Twisters''
     ACM Transactions on Modeling and
     Computer Simulation 10. (2000) 348--357.
   M. Matsumoto and T. Nishimura,
     ``Mersenne Twister: a 623-dimensionally equidistributed
       uniform pseudorandom number generator''
     ACM Transactions on Modeling and
     Computer Simulation 8. (Jan. 1998) 3--30.

   Any feedback is very welcome.
   http://www.math.hiroshima-u.ac.jp/~m-mat/MT/emt.html
   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove spaces)
*/

#include "include/mt64.h"

#define NN 312
#define MM 156
#define MATRIX_A UINT64_C(0xB5026F5AA96619E9)
#define UM UINT64_C(0xFFFFFFFF80000000) /* Most significant 33 bits */
#define LM UINT64_C(0x7FFFFFFF) /* Least significant 31 bits */


/* initializes mt[NN] with a seed */
void init_genrand64(state_mt *state, uint64_t seed)
{
    state->mt[0] = seed;
    for (state->mti=1; state->mti<NN; state->mti++)
        state->mt[state->mti] =  (UINT64_C(6364136223846793005) *
                                  (state->mt[state->mti-1] ^ (state->mt[state->mti-1] >> 62)) +
                                  state->mti);
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
void init_by_array64(state_mt *state,
                     uint64_t init_key[],
                     uint64_t key_length)
{
    unsigned int i, j;
    uint64_t k;
    init_genrand64(state, UINT64_C(19650218));
    i=1; j=0;
    k = (NN>key_length ? NN : key_length);
    for (; k; k--) {
        state->mt[i] = (state->mt[i] ^ ((state->mt[i-1] ^ (state->mt[i-1] >> 62)) * UINT64_C(3935559000370003845)))
          + init_key[j] + j; /* non linear */
        i++; j++;
        if (i>=NN) { state->mt[0] = state->mt[NN-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=NN-1; k; k--) {
        state->mt[i] = (state->mt[i] ^ ((state->mt[i-1] ^ (state->mt[i-1] >> 62)) * UINT64_C(2862933555777941757)))
          - i; /* non linear */
        i++;
        if (i>=NN) { state->mt[0] = state->mt[NN-1]; i=1; }
    }

    state->mt[0] = UINT64_C(1) << 63; /* MSB is 1; assuring non-zero initial array */
}

/* generates a random number on [0, 2^64-1]-interval */
uint64_t genrand64_int64(state_mt *state)
{
    int i;
    uint64_t x;
    static uint64_t mag01[2]={UINT64_C(0), MATRIX_A};

    if (state->mti >= NN) { /* generate NN words at one time */

        /* if init_genrand64() has not been called, */
        /* a default initial seed is used     * /
        if (mti == NN+1)
            init_genrand64(UINT64_C(5489)); */

        for (i=0;i<NN-MM;i++) {
            x = (state->mt[i]&UM)|(state->mt[i+1]&LM);
            state->mt[i] = state->mt[i+MM] ^ (x>>1) ^ mag01[(int)(x&UINT64_C(1))];
        }
        for (;i<NN-1;i++) {
            x = (state->mt[i]&UM)|(state->mt[i+1]&LM);
            state->mt[i] = state->mt[i+(MM-NN)] ^ (x>>1) ^ mag01[(int)(x&UINT64_C(1))];
        }
        x = (state->mt[NN-1]&UM)|(state->mt[0]&LM);
        state->mt[NN-1] = state->mt[MM-1] ^ (x>>1) ^ mag01[(int)(x&UINT64_C(1))];

        state->mti = 0;
    }

    x = state->mt[state->mti++];

    x ^= (x >> 29) & UINT64_C(0x5555555555555555);
    x ^= (x << 17) & UINT64_C(0x71D67FFFEDA60000);
    x ^= (x << 37) & UINT64_C(0xFFF7EEE000000000);
    x ^= (x >> 43);

    return x;
}

/* generates a random number on [0, 2^63-1]-interval */
int64_t genrand64_int63(state_mt *state)
{
    return (int64_t)(genrand64_int64(state) >> 1);
}

/* generates a random number on [0,1]-real-interval */
double genrand64_real1(state_mt *state)
{
    return (genrand64_int64(state) >> 11) * (1.0/9007199254740991.0);
}

/* generates a random number on [0,1)-real-interval */
double genrand64_real2(state_mt *state)
{
    return (genrand64_int64(state) >> 11) * (1.0/9007199254740992.0);
}

/* generates a random number on (0,1)-real-interval */
double genrand64_real3(state_mt *state)
{
    return ((genrand64_int64(state) >> 12) + 0.5) * (1.0/4503599627370496.0);
}

/********************************************\
 *                                          *
 *  END OF MERSENNE TWISTER IMPLEMENTATION  *
 *                                          *
\********************************************/



/*
 * RANDOM STATE
 *
 */

#define NN 312
#define MM 156

/*
  If called with no arguments, %make-random-state-mt will initialize a new random
  state, with a FIXED seed (if one wants a different, unpredictable seed, use the
  random-source-randomize! procedure).

  If called with two arguments, they should be an integer and a vector of non-negative
  integers.
 */
DEFINE_PRIMITIVE("%make-random-state-mt", srfi_27_make_random_state_mt, vsubr, (int argc, SCM *argv))
{
    SCM st;

    NEWCELL_ATOMIC(st, state_mt,
                   sizeof(state_mt) +
                   sizeof(uint64_t) * NN);

    state_mt *s = (state_mt *) st;
    if (argc == 0) {
        s->mti = NN+1;
        init_genrand64(s, UINT64_C(5489));
    } else if (argc == 2) {
        if (!INTP(*argv)) STk_error("bad integer ~S", *argv);

        int _mti =  INT_VAL(*argv--);
        if (!VECTORP(*argv)) STk_error("bad vector ~S", *argv);

        SCM _mt  = *argv;

        if (VECTOR_SIZE(_mt) != NN)
            STk_error("bad size ~S for Mersenne Twister state vector",
                      MAKE_INT(VECTOR_SIZE(_mt)));

        STATE_MT_MTI(s) = _mti;

        SCM x;
        for (int i=0; i<NN; i++) {
            x = VECTOR_DATA(_mt)[i];
            if (BIGNUMP(x))
                STATE_MT_MT(s)[i] = mpz_get_ui(BIGNUM_VAL(x));
            else if (INTP(x))
                STATE_MT_MT(s)[i] = INT_VAL(x);
            else
                STk_error("bad integer ~S in Mersenne Twister state vector", x);
        }
    } else
        STk_error("expects either zero or two arguments");

    return st;
}

/* %random-state-copy-mt will create a new copy of a random state. */
DEFINE_PRIMITIVE("%random-state-copy-mt",srfi_27_random_state_copy_mt,subr1,(SCM state))
{
    SCM st;

    size_t size = sizeof(state_mt) +
                  sizeof(uint64_t) * (NN - 1);

    NEWCELL_ATOMIC(st, state_mt, size);

    memcpy(st,state,size);
    return st;
}


void print_random_state_mt (SCM state, SCM port, int _UNUSED(mode))
{
    /* The format is   #,(<random-state-mt> i v1 v2 v3 ...) */
    char buf[50];

    STk_puts("#,(<random-state-mt> ",port);
    snprintf(buf, sizeof(buf), "%d", STATE_MT_MTI(state));
    STk_puts(buf, port);

    for (int i=0; i < NN; i++) {
      snprintf(buf, sizeof(buf), " %lu", (unsigned long) STATE_MT_MT(state)[i]);
        STk_puts(buf, port);
    }

    STk_putc(')',port);
}

/* Just for completeness, the user will be able to compare
   random states, although not required by the SRFI. */
static SCM test_equal_random_state_mt(SCM x, SCM y)
{
    if (STATE_MT_MTI(x) != STATE_MT_MTI(y)) return STk_false;
    for (int i=0; i<NN; i++)
        if (STATE_MT_MT(x) != STATE_MT_MT(y)) return STk_false;
    return STk_true;
}

static struct extended_type_descr xtype_state_mt = {
    .name  = "random-state-mt",
    .print = print_random_state_mt,
    .equal = test_equal_random_state_mt
};


/*
 * RANDOM SOURCE
 *
 */

DEFINE_PRIMITIVE("%random-source-randomize-mt!", srfi_27_random_source_randomize_mt, subr1, (SCM st))
{

    /* FIXME: does /dev/random exist in all supported platforms? */
    uint64_t r;
    int res, randsrc = open("/dev/random", O_RDONLY);
    res = read(randsrc, &r, sizeof(uint64_t));       // res is unused. Nedded if _FORTIFY_SOURCE=2
    close(randsrc);

    STATE_MT_MTI(st) = NN+1;
    init_genrand64((state_mt *)st, r);

    (void) res;
    return STk_void;
}

DEFINE_PRIMITIVE("%random-source-pseudo-randomize-mt!",
                 srfi_27_random_source_pseudo_randomize_mt,
                 subr2,
                 (SCM st, SCM vec))
{
    uint64_t *init = STk_must_malloc_atomic(sizeof(uint64_t)*VECTOR_SIZE(vec));
    for(int i=0; i<VECTOR_SIZE(vec); i++)
            init[i] = INT_VAL(VECTOR_DATA(vec)[i]);
    init_by_array64((state_mt *)st, init, VECTOR_SIZE(vec));

    STATE_MT_MTI(st) = NN + 1;

    return STk_void;
}

DEFINE_PRIMITIVE("%random-integer-from-source-mt", srfi_27_rnd_int_src_mt, subr2, (SCM state, SCM n))
{
    if (BIGNUMP(n)) {
        if (mpz_sgn(BIGNUM_VAL(n)) < 1) STk_error("range bound ~S for generating random integer is <=0", n);
        size_t size = mpz_sizeinbase(BIGNUM_VAL(n),2)/64;

        mpz_t x;
        mpz_init(x);

        /*
         * Generate a number, zero the bits we know we won't use, and check
         * if it's within bounds. If not, try again. This does not add bias
         * (see Victor Shoup, "A Computational Introduction to Number Theory
         * and Algebra", section 7.4, "Generating a random number from a
         * given interval")
         */
        uint64_t *buf = STk_must_malloc_atomic(size*sizeof(uint64_t));
        do {
            for (size_t i=0; i<size; i++)
                buf[i] = genrand64_int64((state_mt *)state);
            mpz_import(x,size,1,8,0,0,buf);
        } while (mpz_cmp(x,BIGNUM_VAL(n)) >= 0); /* while x >= limit */

        return bignum2scheme_bignum(x);

    } else if (INTP(n)) {
        long limit = INT_VAL(n);
        if (limit <=0) STk_error("range bound ~S for generating random integer is <=0", n);

        unsigned size = ceil(log2(limit))+1;
        uint64_t x;
        do {
            /* Clear the bits we don't want: */
            x = (genrand64_int64(state) << (64-size)) >> (64-size);
        } while (x >= (size_t) limit);
        return (MAKE_INT(x));
    } else
         STk_error("bad integer ~S", n);
    return STk_false; /* never reached */
}

DEFINE_PRIMITIVE("%random-real-from-source-mt", srfi_27_rnd_real_src_mt, subr1, (SCM state))
{
    /* we generate a 64-bit real, since STklos does not support higher
       precision for floating point anyway. this is very fast!
       Note: double is 64-bit on all devices I checked, even those with
       32-bit word size. */
    double res = genrand64_real3((state_mt *) state);

    return double2real(res);
}


/*===========================================================================*\
 *
 *  Module for SRFI-27
 *
\*===========================================================================*/

extern SCM find_module(SCM name, int create);

MODULE_ENTRY_START("srfi/27")
{
  SCM module =  STk_create_module(STk_intern("srfi/27"));

  tc_state_mt = STk_new_user_type(&xtype_state_mt);

  ADD_PRIMITIVE_IN_MODULE(srfi_27_make_random_state_mt, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_27_random_state_copy_mt, module);

  ADD_PRIMITIVE_IN_MODULE(srfi_27_random_source_randomize_mt, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_27_random_source_pseudo_randomize_mt, module);

  ADD_PRIMITIVE_IN_MODULE(srfi_27_rnd_int_src_mt,module);
  ADD_PRIMITIVE_IN_MODULE(srfi_27_rnd_real_src_mt,module);

  STk_export_all_symbols(module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
