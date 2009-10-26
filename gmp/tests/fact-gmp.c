/*
  fact.c

  Compute factorial of input integer

  by Michael J. Fromberger <sting@linguist.dartmouth.edu>
  Copyright (C) 1999 Michael J. Fromberger, All Rights Reserved
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gmp.h"

static void trace_bignum(char* msg, mpz_t bn)
{
  char *buffer = alloca(mp_radix_size(bn, 10) + 10);
  
  mp_toradix(bn, (unsigned char *)buffer, 10);
  printf("%s {%s} ", msg, buffer);
  fflush(stdout);
}



int mp_fact(mp_int *a, mp_int *b);

int main(int argc, char *argv[])
{
  mpz_t  a;
  int  res;

  if(argc < 2) {
    fprintf(stderr, "Usage: %s <number>\n", argv[0]);
    return 1;
  }

  mpz_init(a);
  mpz_init_set_str(a, argv[1], 10);

  if((res = mp_fact(a, a)) < 0) {
    fprintf(stderr, "%s: error in fact(%s)\n", argv[0], argv[1]);
    mpz_clear(a);
    return 1;
  }

  trace_bignum("a = ", a); 
  {
    char  *buf = mpz_get_str(NULL, 10, a);
    
    printf("======%p\n", buf);
    puts(buf);
    free(buf);
  }

  mpz_clear(a);
  return 0;
}

int mp_fact(mpz_t a, mpz_t b)
{
  mpz_t   ix, s, one;
  int    res = 0;

  if(mpz_sgn(a) < 0)
    return -1;

  mpz_init_set_si(one, 1UL);

  mp_init(s);
  mp_add(s, one, s);   /* s = 1  */

  mp_init(ix);
  mp_add(ix, one, ix); /* ix = 1 */
  
  for(/*  */; mpz_cmp(ix, a) <= 0; mpz_add(ix, one, ix)) {
    mp_mul(s, ix, s);
    //trace_bignum("a = ", a); trace_bignum("ix = ", ix); printf("\n");
    trace_bignum("fact(", ix); trace_bignum(") = ", s); printf("\n");
    
  }
  
  mpz_init_set(b, s);
  mpz_clear(ix);
  mpz_clear(s);
  mpz_clear(one);

  return 0;
}
