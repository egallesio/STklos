/*
 * srfi-25.c   -- Implementation of SRFI-25: Multi-dimensional Array Primitives
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
 *    Creation date: 28-Mar-2021 18:41
 * Last file update:  7-Jun-2021 11:41 (eg)
 */

#include "stklos.h"
#include <math.h>
#include "srfi-25-incl.c"

/*
  ------------------------------------------------------------------------------
  ----
  ----                                 A R R A Y . C
  ----
  ------------------------------------------------------------------------------
*/

static int tc_array;


struct array_obj {
  stk_header header;
  int shared;                /* does this array share data with another? */
  int *orig_share_count;     /* pointer to original array share counter */
#ifndef THREADS_NONE
  MUT_FIELD(share_cnt_lock); /* lock for share counter */
  MUT_FIELD(*share_cnt_lock_addr); /* pointer to mutex - ours or of original array's */
#endif
  long size;                 /* size of data */
  long length;               /* # of elements */
  int  rank;                 /* # of dimensons */
  long offset;               /* offset from zero, to be added when calculaing index */
  long *shape;               /* pairs of bounds for each dimenson */
  long *multipliers;         /* size of each dimension stride */
  SCM  *data_ptr;            /* pointer to data */
};

#define ARRAYP(p)            (BOXED_TYPE_EQ((p), tc_array))
#define ARRAY_SHARED(p)      (((struct array_obj *) (p))->shared)
#define ARRAY_SHARE_COUNT(p) (((struct array_obj *) (p))->orig_share_count)
#define ARRAY_LOCK(p)        (*(((struct array_obj *) (p))->share_cnt_lock_addr))
#define ARRAY_SIZE(p)        (((struct array_obj *) (p))->size)
#define ARRAY_LENGTH(p)      (((struct array_obj *) (p))->length)
#define ARRAY_RANK(p)        (((struct array_obj *) (p))->rank)
#define ARRAY_OFFSET(p)      (((struct array_obj *) (p))->offset)
#define ARRAY_SHAPE(p)       (((struct array_obj *) (p))->shape)
#define ARRAY_MULTS(p)       (((struct array_obj *) (p))->multipliers)
#define ARRAY_DATA(p)        (((struct array_obj *) (p))->data_ptr)

#ifdef THREADS_NONE
#  define ARRAY_MUTEX(p)
#  define ARRAY_MUTEX_SIZE 1
#  define ARRAY_MUTEX_PTR_SIZE (sizeof(int))
#else
#  define ARRAY_MUTEX(p) (((struct array_obj *) (p))->share_cnt_lock)
#  define ARRAY_MUTEX_SIZE (sizeof(pthread_mutex_t))
#  define ARRAY_MUTEX_PTR_SIZE (sizeof(pthread_mutex_t*))
#endif



EXTERN_PRIMITIVE("list-set!", list_set, subr3, (SCM list, SCM k, SCM obj));

/*

 *
 * ARRAY STRUCTURE
 *

  DATA_PTR: pointer to data. We cannot do as it is done with vectors, and implement
            this as "SCM data[1]", then allocate more than the necessary size, because
            shared arrays do not have theyr data vector with them. But we
            do something similar (we allocate more than necessary when the array
            is not shared).

  SHARED: when share-array is applied to array A in order to create array B,
          *both* A and B are marked as "shared". But they're different.

          - an array created with array or make-array is original. it contains
            its own data vector, and has a share-counter that is always >=0.
            this counter keeps track of how many others point to this array's
            data.

          - an array created with share-array is a share-array. it contains a pointer
            to another array's data vector. its share-counter is always -1, and
            when this array is garbage-collected, its finalizer decreases the
            original array's counter.

  RANK: is the number of dimensions specified in the shape, even if there
        are empty dimensions!

  MULTIPLIERS: Suppose arrays always begin at index zero. Then, when an index
               (i j k) is used in array-ref or array-set!, it is translated into

  multipliers[0] * i +
  multipliers[1] * j +
  multipliers[2] * k


  OFFSET: When an index (i j k) is used in array-ref or array-set!, it is translated
          as the sum

  multipliers[0] * ( -(array-start 0) + i )   +
  multipliers[1] * ( -(array-start 1) + j )   +
  multipliers[2] * ( -(array-start 2) + k )

  Factoring out the constants, we have:

  ---
  multipliers[0] * i +
  multipliers[1] * j +
  multipliers[2] * k +

  -(multipliers[0] * (array-start 0) +
    multipliers[1] * (array-start 1) +
    multipliers[2] * (array-start 2))
  ---
  The last term is the offset, fixed for each array.


  *
  * MACROS
  *

 ARRAYP(p)
 ARRAY_SHARED(p)
 ARRAY_SHARE_COUNT(p)
 ARRAY_SIZE(p)
 ARRAY_LENGTH(p)
 ARRAY_RANK(p)
 ARRAY_OFFSET(p)
 ARRAY_SHAPE(p)
 ARRAY_MULTS(p)
 ARRAY_DATA(p)

 *
 * empty arrays and default values
 *

 The SRFI requires support for empty arrays (or at least
 it is implied by the reference implementation and mailing
 list archives). But the reference implementation is not
 consistent, but I have adopter the same behavior:

 * arrays are created with a default value, so
   (make-array (shape)) has a default value of #void

   (array-ref (make-array (shape)))  =>  #void

 * from the reference implementation, from Kawa and Gauche,
   (shape) is an array of rank 2, but there are no indices that
   can be used on it, so referencing (shape) is an error:

  (array-ref (shape) i j) => out of bounds, no matter what i, j are!

* from the reference implementation:

  (define a (make-array (shape) -2))  ; empty array, default value -2
  (array-ref a) => -2

  (define b (make-array (shape 1 1) -2))  ; empty array, not referenceable:
  (array-ref b) => error

};


 *
 * efficiency
 *

 I have tried as much as possible to make a fast
 implementation -- however, there are lots of verifications
 necessary for each element access. For example,

 * the element position may be specifed as arguments, as
   elements in a vector, or as elements in an array.
  > This requires one more "if"...
 * arrays may be empty, and may or not have a default value.
   More checks.
   This also requires us to store a single element, even
   when the "data" vector is supposed to have length zero...

 */


EXTERN_PRIMITIVE("array-ref", srfi_25_array_ref,vsubr, (int argc, SCM *argv));


/* WARNING: following function assumes array was already type checked. */
int array_zero_basedp(SCM array)
{
    for (int i = 0; i < ARRAY_RANK(array); i++)
        if (ARRAY_SHAPE(array)[2*i] != 0) return 0;
    return 1;
}

/*******************/
/* INDEX VERIFIERS */
/*******************/

static inline
void check_index_arguments(SCM array, int nargs, SCM *args)
{
    if (ARRAY_RANK(array) != nargs)
        STk_error("wrong number of indices ~S, should be ~S",
                  MAKE_INT(nargs),
                  MAKE_INT(ARRAY_RANK(array)));
    for (; nargs; nargs--)
        if (!INTP(*args)) STk_error("bad integer ~S used as index", *args--);
}

static inline
void check_index_vector(SCM vec, SCM array)
{
    if (ARRAY_RANK(array) != VECTOR_SIZE(vec)) STk_error("wrong number of indices");
    for (int i=0; i<VECTOR_SIZE(vec); i++)
        if (!INTP(VECTOR_DATA(vec)[i])) STk_error("bad integer ~S used in index vector ~S",
                                                  VECTOR_DATA(vec)[i],
                                                  vec);
}


/*
  checks if index_arr contains valid indices for array.
 */
static inline
void check_index_array(SCM index_arr, SCM array)
{
    if (ARRAY_RANK(index_arr)!=1)        STk_error("Index array must be of rank 1");
    if (!array_zero_basedp(index_arr))   STk_error("Index array ~S is not zero-based", index_arr);
    if (ARRAY_RANK(array) != ARRAY_LENGTH(index_arr))
        STk_error("wrong number of indices ~S, should be ~S",
                  MAKE_INT(ARRAY_LENGTH(index_arr)),
                  MAKE_INT(ARRAY_RANK(array)));

    /* FIXME: optimize. we should only use array-ref if the array is shared */
    SCM args[2];
    for (int i=0; i<ARRAY_LENGTH(index_arr); i++) {
        args[1] = index_arr;
        args[0] = MAKE_INT(i);
        if (!INTP  (STk_srfi_25_array_ref(2, &args[1])) )
            STk_error("bad integer ~S used in index vector ~S",
                      STk_srfi_25_array_ref(2, &args[1]),
                      index_arr);
    }
}

/*
  checks if single index given_index is valid for array's dimension "dim".
 */
static inline
void check_array_dim_bounds(SCM array, int given_index, int dim)
{
    if (given_index <  ARRAY_SHAPE(array)[2*dim])
        STk_error("array index ~S < ~S for dimension ~S",
                  MAKE_INT(given_index),
                  MAKE_INT(ARRAY_SHAPE(array)[2*dim]),
                  MAKE_INT(dim));
    if(given_index >= ARRAY_SHAPE(array)[2*dim+1])
        STk_error("array index ~S >= ~S for dimension ~S",
                  MAKE_INT(given_index),
                  MAKE_INT(ARRAY_SHAPE(array)[2*dim+1]),
                  MAKE_INT(dim));
}


/*****************/
/* INDEX GETTERS */
/*****************/

static inline long
raw_get_index_from_C_args(long *mults, long offset, int argc, long *argv)
{
    register long index = offset;
    for(register int i=0; i < argc; i++)
        index +=  mults[i] * (*argv++); /* the index given by user for dimension i */
    return index;
}


static inline long
get_index_from_args(SCM array, int argc, SCM *argv)
{
    register long index = ARRAY_OFFSET(array);
    register long dim_idx;
    for(register int i=0; i < argc; i++) {
        dim_idx = INT_VAL(*argv--); /* the index given by user for dimension i */
        check_array_dim_bounds(array,  dim_idx,  i);
        index +=  ARRAY_MULTS(array)[i] * dim_idx;
    }
    return index;
}

static inline long
get_index_from_vector (SCM array, SCM vec)
{
    register long index = ARRAY_OFFSET(array);
    register long dim_idx;
    for(register int i=0; i<VECTOR_SIZE(vec); i++) {
        dim_idx = INT_VAL(VECTOR_DATA(vec)[i]); /* the index given by user for dimension i */
        check_array_dim_bounds(array, dim_idx, i);
        index +=  ARRAY_MULTS(array)[i] * dim_idx;
    }
    return index;
}

static inline long
get_index_from_array (SCM array, SCM idx_arr)
{
    register long index = ARRAY_OFFSET(array);
    register int dim_idx;
    if (ARRAY_SHARED(idx_arr) == -1) { /* it's a copy! can't directly acess ARRAY_DATA */
        SCM arg;
        for(register long i=0; i < ARRAY_LENGTH(idx_arr); i++) {
            arg = MAKE_INT(i);
            long dim_idx_idx = get_index_from_args(idx_arr, 1, &arg);
            dim_idx = INT_VAL(ARRAY_DATA(idx_arr)[dim_idx_idx]);
            check_array_dim_bounds(array, dim_idx, i);
            index +=  ARRAY_MULTS(array)[i] * dim_idx;
        }
    }  else
        for(register long i=0; i < ARRAY_LENGTH(idx_arr); i++) {
            dim_idx = INT_VAL(ARRAY_DATA(idx_arr)[i]); /* the index given by user for dimension i */
            check_array_dim_bounds(array, dim_idx, i);
            index +=  ARRAY_MULTS(array)[i] * dim_idx;
        }
    return index;
}

/**************/
/* AREF, ASET */
/**************/

/* AREF1 and ASET1are fast array-referencers/setters for internal use.
   They are macros so as to avoid the use of an index vector.
   The index, however, needs to be a lvalue (register variables won't work).
   Indices are NOT checked (we suppose that is done before using these. */

#define AREF1(array, i) ( ARRAY_DATA(array) [ raw_get_index_from_C_args(ARRAY_MULTS(array),ARRAY_OFFSET(array),1,&i) ] )

#define ASET1(array, i, val) { ARRAY_DATA(array) [ raw_get_index_from_C_args(ARRAY_MULTS(array),ARRAY_OFFSET(array),1,&i) ] = val; }


/******************/
/* ARRAY BUILDERS */
/******************/


/* returns the size of an array (the number of elements in it),
   given its rank and shape.
   - if rank is zero, size is zero;
   - if a dimension has no possible index, for example as the
     second dimension in (shape 0 3 5 5  0 3), the size
     will be zero. */
static inline long
get_array_size(int rank, long *shape)
{
    if (rank == 0) return 0;
    long size = 1;
    for (int i=0; i<rank; i++)
        size *= (shape[i*2+1] - shape[i*2]);

    return size;
}


SCM STk_make_array(int rank, long *shape, SCM init)
{
  /* size = number of elements, not number of bytes! */
  long size = get_array_size(rank, shape);

  /* Not explicit in SRFI-25, but the tests and reference implementation
     expect you to be able to make an empty array WITH AN INIT VALUE,
     which IS TO BE RETURNED by (array-ref a), and set
     by (array-set! a new-value). So we need to keep at least one cell
     in the array.  */
  long elements_size = size ? size : 1;

  /* shape is rank * 2 * sizeof(long),
     multipliers is rank *  sizeof(long).
     add them to size of data[]           */
  long metadata_size = elements_size + rank * 3;

  register long i;

  SCM a;
  NEWCELL_WITH_LEN(a, array,
                   sizeof(struct array_obj) +
                   sizeof(int) +                  /* shared */
                   sizeof(int*) +                 /* orig_share_count */
                   ARRAY_MUTEX_SIZE +             /* share_cnt_lock */
                   ARRAY_MUTEX_PTR_SIZE +         /* share_cnt_lock_addr */
                   sizeof(long) +                 /* size */
                   sizeof(long) +                 /* length */
                   sizeof(int) +                  /* rank */
                   sizeof(long) +                 /* offset */
                   sizeof(long*) +                /* shape */
                   sizeof(long*) +                /* multipliers */
                   sizeof(SCM*) +                 /* data_ptr */
                   elements_size * sizeof(SCM) +  /* data (elements) */
                   metadata_size * sizeof(long)); /* meta-data (multipliers & shape) */

  struct array_obj *s = (struct array_obj *) a;

  s->shared           = 0;
  s->orig_share_count = &s->shared;
  s->size             = size;
  s->length           = size;
  s->rank             = rank;
  s->offset           = 0L;     /* will be updated, see below */

#ifndef THREADS_NONE
  /* we are a fresh array (not a copy) , so we point to our own lock: */
  s->share_cnt_lock_addr = &(s->share_cnt_lock);
  MUT_INIT(s->share_cnt_lock);
#endif

  /* data begins right after the data pointer: */
  s->data_ptr = (SCM *)(&(s->data_ptr)) + 1;

  /* then multipliers, then shape! */
  s->multipliers = (long*) (s->data_ptr + elements_size);
  s->shape = s->multipliers + rank;

  /* copy shape array */
  for(i=0; i<rank*2; i++)
      s->shape[i] = shape[i];

  /* the size of each stride:
     multipliers[j] is the size of stride for j-th dimension */
  if (rank) {
      long m = size;
      long d;
      for(i=0; i<rank; i++)  {
          d =  (shape[i*2+1] - shape[i*2]);
          if (d) {
              m /= d;
              s->multipliers[i] = m;
              s->offset -= m * shape[i*2];
          } else {
              /* we found an empty dimension! there are *no* sequence
                 of indices that can address any element here, so we
                 should treat this as an empty array, but tests from
                 the reference implementation require that
                 (= (array-rank (shape -1 -1)) 2)... So we don't change
                 the rank, but do change the size!
                 Regarding multipliers: we do calculate them, but
                 won't ever be used anyway... */
              s->length = 0;
          }
      }
  }

  /* an array created with an empty shape has one element. */
  if (rank == 0) s->length = 1;

  if (init) {
      register SCM *tmp = ARRAY_DATA(a);
      for(i=0; i < elements_size; i++) *tmp++ = init;
  }

  return a;
}

DEFINE_PRIMITIVE("array?", srfi_25_arrayp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(ARRAYP(obj));
}

static int
long * shapetoCshape(SCM shape)
{
    int len = ARRAY_LENGTH(shape);

    if (len%2) STk_error("bad array shape ~S", shape);

    /* shape could be a shared array, so we can't blindly traverse its data array. */
    long *cshape = STk_must_malloc_atomic(len * sizeof(long));
    long index;
    SCM arg[2];

    for (int i = 0; i < len/2; i++) {
        for (int j = 0; j < 2; j++) {
            arg[1]=MAKE_INT(i);
            arg[0]=MAKE_INT(j);
            index = get_index_from_args(shape,2,&arg[1]);
            cshape[2*i+j] = INT_VAL(ARRAY_DATA(shape)[index]);
        }
    }
    return cshape;
}


/*
<doc EXT shape?
 * (shape? obj)
 *
 * Checks if |obj| is an array shape. SRFI-25 dictates that a
 * shape is an ordinary array, with rank two and shape |(0 r 0 2)|,
 * where |r| is the rank of the array that the shape describes.
 * So, any array of shape |(0 r 0 2| is a shape, for any non-negative
 * integer |r|.
doc>
*/
DEFINE_PRIMITIVE("shape?", srfi_25_shapep, subr1, (SCM obj))
{
    if (!ARRAYP(obj))               return STk_false;
    if (ARRAY_RANK(obj) != 2)       return STk_false;
    if (ARRAY_SHAPE(obj)[0] != 0)   return STk_false;
    if (ARRAY_SHAPE(obj)[2] != 0)   return STk_false;
    if (ARRAY_SHAPE(obj)[3] != 2)   return STk_false;
    return STk_true;
}


DEFINE_PRIMITIVE("make-array",srfi_25_make_array,subr12,(SCM shape, SCM obj))
{
    if (!obj) obj = STk_void;
    if (STk_srfi_25_shapep(shape) == STk_false) STk_error("bad array shape ~S",shape);
    long *cshape = shapetoCshape(shape);
    long len = ARRAY_LENGTH(shape);

    return STk_make_array(len/2, cshape, obj);
}

DEFINE_PRIMITIVE("shape",srfi_25_shape,vsubr,(int argc, SCM *argv))
{
  if (argc % 2) STk_error("odd number of arguments (~S) given for shape", argc);

  /* shape of a shape is 0 d 0 2 */
  long *meta_shape = STk_must_malloc_atomic(4*sizeof(long));
  meta_shape[0]=0L;
  meta_shape[1]=argc/2L;
  meta_shape[2]=0L;
  meta_shape[3]=2L; /* two elements (start/end) */

  SCM s = STk_make_array(2, meta_shape, NULL);
  for (int i=0; i < argc; i++)
      ARRAY_DATA(s)[i] = *argv--;

  for (int i=0; i < argc/2; i++)
      if (INT_VAL(ARRAY_DATA(s)[2*i])  >  INT_VAL(ARRAY_DATA(s)[2*i+1])) {
          STk_error("shape has upper bound below lower bound");
      }

  return s;
}


DEFINE_PRIMITIVE("array", srfi_25_array, vsubr, (int argc, SCM *argv))
{
    if (argc < 1) STk_error("not enough arguments");

    SCM shape = *argv--;
    if (STk_srfi_25_shapep(shape) == STk_false) STk_error("bad array shape ~S",shape);

    long *cshape = shapetoCshape(shape);
    long len = ARRAY_LENGTH(shape);

    /* The empty array may or may not have a single init element */
    if (len > 0) {
        if (get_array_size(len/2,cshape) != (argc-1))
            STk_error("shape does not match argument count");

    } else if (argc > 2) /* The empty array case: one or zero inits */
            STk_error("shape does not match argument count");

    SCM a = STk_make_array(len/2, cshape, NULL);

    /* array, empty or not, with init. use it. */
    for(int i = 0; i < argc-1; i++)
        ARRAY_DATA(a)[i] = *argv--;

    /* empty array *without* init; use NULL (should this be #void?) */
    if ((len == 0) && (argc == 1))
        ARRAY_DATA(a)[0] = NULL;

    return a;
}

DEFINE_PRIMITIVE("array-rank", srfi_25_array_rank, subr1, (SCM obj))
{
  if (!ARRAYP(obj)) STk_error("bad array ~S", obj);
  return MAKE_INT(ARRAY_RANK(obj));
}

DEFINE_PRIMITIVE("array-start", srfi_25_array_start,subr2, (SCM array, SCM k))
{
    if(!ARRAYP(array))STk_error("bad array ~S", array);
    if(!INTP(k)) STk_error("bad integer ~S", k);
    long dim = INT_VAL(k);
    if (dim >= ARRAY_RANK(array)) STk_error("array dimension ~S too high",k);
    if (dim < 0L)  STk_error("negative array dimension ~S", k);
    return MAKE_INT((ARRAY_SHAPE(array)[2*dim]));
}

DEFINE_PRIMITIVE("array-end", srfi_25_array_end,subr2, (SCM array, SCM k))
{
    if(!ARRAYP(array))STk_error("bad array ~S", array);
    if(!INTP(k)) STk_error("bad integer ~S", k);
    long dim = INT_VAL(k);
    if (dim >= ARRAY_RANK(array)) STk_error("array dimension ~S too high",k);
    if (dim < 0L)  STk_error("negative array dimension ~S", k);
    return MAKE_INT((ARRAY_SHAPE(array)[2*dim+1]));
}



DEFINE_PRIMITIVE("array-ref", srfi_25_array_ref,vsubr, (int argc, SCM *argv))
{
    if (argc<1) STk_error("not enough arguments");

    /* we have at least the first argument, the array: */
    SCM array = *argv--;
    if(!ARRAYP(array)) STk_error("bad array ~S", array);

    /* The empty array. rank == 0,  argc == 1. */
    if (ARRAY_RANK(array) == 0 && argc == 1) {
        if (!ARRAY_DATA(array)[0]) STk_error("array ~S has no default value", array);
        return ARRAY_DATA(array)[0];
    }

    /* SRFI-25 says indices can be split into args, (array-set! A i j k value)
       OR they can be in a vector, (array-set! A #(i j k) value),
       OR an array,  (array-set! A #1A((0 3)(i j k)) value),.
       So we need to check all these cases here.    */

    register long index = 0;

    if (INTP(*argv)) {
        check_index_arguments(array, argc-1, argv);
        index = get_index_from_args(array, argc-1, argv);

    } else if (VECTORP(*argv)) {
        if (VECTOR_SIZE(*argv)==0) {
            if (!ARRAY_DATA(array)[0]) STk_error("array ~S has no default value", array);
            return ARRAY_DATA(array)[0];
        }
        check_index_vector(*argv, array);
        index = get_index_from_vector(array, *argv);

    } else if (ARRAYP(*argv)) {
        check_index_array(*argv,array);

        if (ARRAY_RANK(array) == 0)  {
            if (!ARRAY_DATA(array)[0]) STk_error("array ~S has no default value", array);
            return  ARRAY_DATA(array)[0];
        }
        index = get_index_from_array(array, *argv);

    } else STk_error("Index must be vector, array or sequence of integers");

    return ARRAY_DATA(array)[index];
}



DEFINE_PRIMITIVE("array-set!", srfi_25_array_set,vsubr, (int argc, SCM *argv))
{
    if (argc<2) STk_error("not enough arguments");

    /* we have at least the first argument, the array: */
    SCM array = *argv--;
    if(!ARRAYP(array)) STk_error("bad array ~S", array);

    /* The empty array. rank == 0, argc == 2. */
    if (ARRAY_RANK(array) == 0 && argc == 2) {
        ARRAY_DATA(array)[0] = *argv;
        return STk_void;
    }

    /* SRFI-25 says indices can be split into args, (array-set! A i j k value)
       OR they can be in a vector, (array-set! A #(i j k) value),
       OR an array,  (array-set! A #1A((0 3)(i j k)) value),.
       So we need to check all these cases here.    */

    register long index = 0;

    if (INTP(*argv)) {
        check_index_arguments(array, argc-2, argv);
        index = get_index_from_args(array, argc-2, argv);
        argv -= (argc-2) ; /* skip indices */
    } else if (VECTORP(*argv)) {
        if (VECTOR_SIZE(*argv)==0) {
            argv--;
            ARRAY_DATA(array)[0] = *argv;
            return STk_void;
        }
        check_index_vector(*argv,array);
        index = get_index_from_vector(array, *argv);
        argv--; /* skip the vector with indices */

    } else if (ARRAYP(*argv)) {
        check_index_array(*argv,array);

        if (ARRAY_RANK(array) == 0) {
            argv--;
            ARRAY_DATA(array)[0] = *argv;
            return STk_void;
        }
        index = get_index_from_array(array, *argv);
        argv--; /* skip the array with indices */

    } else STk_error("Index must be vector, array or sequence of integers");

    ARRAY_DATA(array)[index] = *argv;

    return STk_void;
}


/***************/
/* SHARE-ARRAY */
/***************/

SCM *get_coefficients (SCM proc, int p)
{
    /* although coefs is a pointer to SCM, the SCM objects should be
       integers only, and neither VECTOR nor INT contain pointers,
       so we use STk_must_malloc_atomic here. */
    SCM *coefs = STk_must_malloc_atomic((p+1) * sizeof(SCM));
    SCM args = STk_vector2list(STk_makevect(p, MAKE_INT(0)));
    SCM zero = MAKE_INT(0);
    SCM one  = MAKE_INT(1);
    int i, j;

    /*
      The affine map mentioned in the SRFI is a linear map plus a
      constant -- the linear map is identified by running proc
      on the canonical basis, so we identify the map coefficients.

      # GIVEN:

      new array indices      r1, r2, ..., rp
                multipliers  n1, n2, ..., np
                offset       f

      old array indices      x1, x2, ..., xq
                multipliers  m1, m2, ..., mq
                offset       o

      # THEN:

      * MAP COEFFICIENTS from indices r_ to indices x_ :

      x_1 = a_11 r1 + a_12 r2 + ... a_1p rp   + k_1
      x_2 = a_21 r1 + a_22 r2 + ... a_2p rp   + k_2
      x_3 = a_31 r1 + a_32 r2 + ... a_3p rp   + k_3
      ...
      x_q = a_q1 r1 + a_q2 r2 + ... a_qp rp   + k_q

      * MAP CONSTANTS are

      k_1 k_2 ... k_q

      * MULTIPLIERS for the new array are:

      n1 = a_11 m1 + a_21 m2 + ... + a_q1 mq
      n2 = a_12 m1 + a_22 m2 + ... + a_q2 mq
      ...
      np = a_1p m1 + a_2p m2 + ... + a_qp mq

      * OFFSET for the new array is:

      f = o + m1 k1 + m2 k2 + ... + mq kq
    */

    /*

      (proc r1=0 ... ri=1 ... rp=0) retruns Q values:

      a_i1 + k_1,   a_i2 + k_2,   ... ,  a_iq + k_q

     */


    /* k_1 ... k_q  q = rank of old array */
    SCM k = STk_values2vector(STk_C_apply_list(proc, args),NULL);
    int q = VECTOR_SIZE(k);

    /* coefs[i] is a vector with the a_j,i coefficients --
       the coefficient a_ji in the affine map */
    for (i=0; i < p; i++) {

        STk_list_set(args, MAKE_INT(i), one);

        coefs[i] = STk_values2vector(STk_C_apply_list(proc, args),NULL);

        for (j=0; j < q; j++)
            VECTOR_DATA(coefs[i])[j] = MAKE_INT(INT_VAL(VECTOR_DATA(coefs[i])[j]) -
                                                INT_VAL(VECTOR_DATA(k)[j]));

        STk_list_set(args, MAKE_INT(i), zero);
    }
    coefs[p] = k;

    /* COEFS:

       0 -> a_10 ... a_q0
       1 -> a_11 ... a_q1
       2 -> a_12 ... a_q2
       .
       .
       .
       p -> k_1 k_2 ... k_q
     */

    return coefs;
}

/*
  Given a procedure PROC and the number of arguments (or "array rank"):
  - p of the new array
  - q of the old array
  this function creates a string that displays the affine map that
  PROC implements for translating shapes.

  We try as much as possible to be memory-efficient, so we calculate the space
  needed for the string.
 */
char *get_affine_map(SCM proc, int p, int q)
{
    SCM *c = get_coefficients(proc,p);

    double dtotal = 0;
    long size;
    for (int i=0; i<p; i++)
        for (int j=0; j<q; j++) {
            size = INT_VAL(VECTOR_DATA(c[i])[j]);
            dtotal += size < 2
                ? 2
                : ceil(log10((double) size)) + 1;
        }

    SCM er = STk_makestring(6,"given");

    long written = 0;
    long total = (long) dtotal * 6 + 1;

    char *buf = STk_must_malloc_atomic(total);
    char *ptr = buf;

    long val;
    char *sign;
    int nonzero;
    for(long j=0; j<q; j++) {
        nonzero = 0;

        written = snprintf(ptr, total-(ptr-buf),"x_%ld ->", j);
        if (written < 0) return er;
        ptr+=written;

        for (long i=0; i<p; i++) {
            val = INT_VAL(VECTOR_DATA(c[i])[j]);
            if  (val != 0) {

                if (i == 0 && val > 0) sign = "";
                else sign = (val < 0) ? "- " : "+ ";

                written = snprintf(ptr,total-(ptr-buf)," %s%ldy_%ld",sign,labs(INT_VAL(VECTOR_DATA(c[i])[j])),i);
                if (written < 0) return er;
                ptr += written;
                nonzero=1;
            }
        }

        /* the constant */
        val = INT_VAL(VECTOR_DATA(c[p])[j]);
        if (val || !nonzero) {

            if (!nonzero && !val) sign = "";
            else sign = (val < 0) ? "- " : "+ ";

            written = snprintf(ptr, total - (ptr-buf), " %s%ld", sign, labs(val));
            if (written < 0) return er;
            ptr += written;
        }
        if (j != q-1) written = snprintf(ptr, total - (ptr-buf),"; ");
        if (written < 0) return er;
        ptr += written;
    }
    return buf;
}

/*
  cvec2string returns a newly allocated string containing
  a display of the values in vec:

  vec[0]=10;
  vec[1]=20;
  vec[2]=30;
  cvec2string(3, vec) => "(10 20 30)"

  The amount of memory needed is calculated precisely.
 */
char *cvec2string(int n, long *vec)
{
    double dtotal = 0;
    double size;

    /* compute the number of digits we will print: */
    for (int i=0; i<n; i++) {
        size = vec[i] < 2
            /* always add one for a space after the number */
            ? 2
            : ceil(log10((double)(vec[i]))) + 1;
        dtotal += size;
    }

    long total = (long) dtotal;
    char *s = STk_must_malloc_atomic(total+3);
    char *p = s;
    *p = '(';
    p++;
    int written = 1;

    for(int i=0;  i<n;  i++,p++) {
        written = snprintf(p,total-(s-p), "%ld", vec[i]);
        p += written;
        if (i==n-1)  *p = ')';
        else         *p = ' ';
    }
    *p = '\0';

    return s;
}

/* Verifies is cshape is compatible with the shape of array, which
   has an underlying store of size elements.

   This function will run all possible indices within cshape, generating
   a long integer that would be an index to the data vector of array. If
   this number is negative or >= size of the underlying vector, then we
   return zero (false). Otherwise, we return one.
 */
void check_array_shape_compatible(int new_rank, long *new_shape,
                                  int old_rank, long *old_shape,
                                  SCM proc,
                                  long offset, long *mults,
                                  long size)
{
    long index;
    long *idx = STk_must_malloc_atomic(new_rank*sizeof(long));

    if (new_rank == 0) return; /* always compatible with any shape */

    for (int i=0; i<new_rank; i++) {
        if (new_shape[2*i] == new_shape[2*i+1]) return; /* no usable indices, compatible with anything */
        idx[i] = new_shape[2*i]; /* lower bound */
    }

    int updated = 1;
    char *msg   = "Shape %s does not map to shape %s under mapping %s. "
                  "Index %s for the new array goes out of bounds in the "
                  "original array.";

    while(updated) {
        updated = 0;

        index = raw_get_index_from_C_args(mults, offset,new_rank,&idx[0]);

        if ( index < 0 || (index >= size) ) {
            char *ns = cvec2string(2*new_rank,new_shape);
            char *os = cvec2string(2*old_rank,old_shape);
            char *map = get_affine_map(proc,new_rank, old_rank);
            char *id  = cvec2string(new_rank,idx);
            char *buf = STk_must_malloc_atomic(strlen(msg)+
                                               strlen(ns)+
                                               strlen(os)+
                                               strlen(map)+
                                               strlen(id)+1);

            sprintf(buf,msg, ns, os, map, id);

            STk_error(buf);
        }

        /* update idx vector */
        for(int d = new_rank - 1; d >= 0; d--) {
            if (idx[d] < new_shape[d*2+1] - 1) { /* we can increase */
                idx[d]++;
                for(int i = d+1; i < new_rank; i++) {
                    idx[i] = new_shape[2*i];
                }
                updated=1;
                break;
            }
        }
    }
}

/* Increases the share counter of an array. */
static void shared_array_inc_count(SCM array)
{
    struct array_obj *a = (struct array_obj *) array;
    MUT_LOCK(a->share_cnt_lock);
    (*(a->orig_share_count))++;
    MUT_UNLOCK(a->share_cnt_lock);
};

/* Decreases the sare counter of an array. This is registered as a finalizer
   only for arrays that were build with share-array. */
static void shared_array_dec_count(SCM array,  void _UNUSED(*client_data))
{
    struct array_obj *a = (struct array_obj *) array;
    MUT_LOCK(a->share_cnt_lock);
    // fprintf(stderr,"DECREMENTED\n");
    (*(a->orig_share_count))--;
    MUT_UNLOCK(a->share_cnt_lock);
};


EXTERN_PRIMITIVE("array-shape",srfi_25_array_shape,subr1,(SCM array));  /* will be used in share-array */

DEFINE_PRIMITIVE("share-array", srfi_25_share_array, subr3, (SCM old_array, SCM new_shape, SCM proc))
{
    if (!ARRAYP(old_array)) STk_error("bad array ~S", old_array);
    if (STk_srfi_25_shapep(new_shape)==STk_false) STk_error("bad arrayp ~S", new_shape);
    if (!CLOSUREP(proc)) STk_error("bad procedure ~S", proc);

    int p = ARRAY_LENGTH(new_shape)/2; /* rank of new */
    int q = ARRAY_RANK(old_array);     /* rank of old */
    int i, j;

    /* shape of the new vector. we store shapes as C arrays, not as Scheme arrays,
       so we need to recover each value... */
    long *cshape= shapetoCshape(new_shape);

    /* the number of elements that will be accessible from this array. */
    long elements_size = get_array_size(p,cshape);

    SCM a;
    NEWCELL_WITH_LEN(a, array,
                   sizeof(struct array_obj) +
                   sizeof(int) +                  /* shared */
                   sizeof(int*) +                 /* orig_share_count */
                   ARRAY_MUTEX_SIZE +             /* share_cnt_lock */
                   ARRAY_MUTEX_PTR_SIZE +         /* share_cnt_lock_addr */
                   sizeof(long) +                 /* size */
                   sizeof(long) +                 /* length */
                   sizeof(int) +                  /* rank */
                   sizeof(long) +                 /* offset */
                   sizeof(long*) +                /* shape */
                   sizeof(long*) +                /* multipliers */
                   sizeof(SCM*));                 /* data_ptr */
                   /* don't include data elements, we'll use the othre array's */


    /* multipliers */
    long *old_mult = ARRAY_MULTS(old_array);

    SCM *coefs = get_coefficients(proc, p);

    long offset = ARRAY_OFFSET(old_array);
    for (i=0; i<q; i++)
        offset += old_mult[i] * INT_VAL(VECTOR_DATA(coefs[p])[i]);

    long *new_mult = STk_must_malloc_atomic(p * sizeof(long));
    for (i=0; i<p; i++) {
        new_mult[i] = 0L;
        for (j=0; j<q; j++)
            new_mult[i] += old_mult[j] * INT_VAL(VECTOR_DATA(coefs[i])[j]);
    }

    check_array_shape_compatible(p, cshape,
                                 q, ARRAY_SHAPE(old_array),
                                 proc, offset, new_mult, ARRAY_SIZE(old_array));

    struct array_obj *s = (struct array_obj *) a;

    s->shared           = -1;
    if (*ARRAY_SHARE_COUNT(old_array) >= 0) /* original has the data */
        s->orig_share_count = &ARRAY_SHARED(old_array);
    else
        s->orig_share_count = ARRAY_SHARE_COUNT(old_array); /* original was already a copy, has pointer */
    s->size             = ARRAY_SIZE(old_array);            /* we share the data object! */
    s->length           = elements_size;                    /* number of elements  usable in THIS array */
    s->rank             = p;
    s->offset           = offset;
    s->shape            = cshape;
    s->multipliers      = new_mult;
    s->data_ptr         = ARRAY_DATA(old_array);

#ifndef THREADS_NONE
    s->share_cnt_lock   = ARRAY_LOCK(old_array);
    /* get the address of the lock from the old array, and don't initialize ours. */
    s->share_cnt_lock_addr = &(ARRAY_LOCK(old_array));
#endif

    /* mark old array as shared by one more array, OR of the original
       array (the right thing will be done) */
    shared_array_inc_count(old_array);

    STk_register_finalizer(a, shared_array_dec_count);

    return a;
}

/*===========================================================================*\
 *
 *  EXTRA
 *
\*===========================================================================*/


/*
<doc EXT shared-array?
 * (shared-array? array)
 *
 * Will return ,(code "#t") when the array has its data shared with other
 * arrays, and ,(code "#f") otherwise.
doc>
 */
DEFINE_PRIMITIVE("shared-array?",srfi_25_shared_arrayp,subr1,(SCM array))
{
  if (!ARRAYP(array)) STk_error("bad array ~S", array);
  return MAKE_BOOLEAN(ARRAY_SHARED(array));
}

/*
<doc EXT array-share-count
 * (array-share-count array)
 *
 * Returns the number of arrays that were built sharing |array|'s
 * elements through |(share-array array shape proc)|, and that were not
 * yet garbage collected.
 * Note that it may take a long time for an object to be garbage
 * collected automatically. It is possible to force a garbage
 * collection pass by calling |(gc)|, but even that does not guarantee that
 * a specific object will be collected.
doc>
 */
DEFINE_PRIMITIVE("array-share-count",srfi_25_array_share_count,subr1,(SCM array))
{
  if (!ARRAYP(array)) STk_error("bad array ~S", array);
  return MAKE_INT(ARRAY_SHARED(array));
}


/*
<doc EXT array-shape
 * (array-shape array)
 *
 * Returns the shape of |array|.
doc>
 */
DEFINE_PRIMITIVE("array-shape",srfi_25_array_shape,subr1,(SCM array))
{
    if (!ARRAYP(array)) STk_error("bad array ~S", array);

    long *ashape = ARRAY_SHAPE(array);
    int dim = 2 * ARRAY_RANK(array);

    long *meta_shape = STk_must_malloc_atomic(4*sizeof(long));
    meta_shape[0]=0L;
    meta_shape[1]=ARRAY_RANK(array);
    meta_shape[2]=0L;
    meta_shape[3]=2L;

    SCM shape = STk_make_array(2,meta_shape,NULL);

    for (int i = 0; i < dim; i++)
        ARRAY_DATA(shape)[i] = MAKE_INT(ashape[i]);

    return shape;
}

/*
<doc EXT array-size
 * (array-size array)
 *
 * Returns the number of elements in |array|.
doc>
 */
DEFINE_PRIMITIVE("array-size",srfi_25_array_size,subr1,(SCM array))
{
    if (!ARRAYP(array)) STk_error("bad array ~S", array);
    long size = ARRAY_LENGTH(array);
    return MAKE_INT( size);/*? size : 1); / * empty arrays always have the default value */
}


/*
<doc EXT array-copy+share
 * (array-copy+share array)
 *
 * Returns a copy of |array|.
 * If array does not have its own internal data, but was built using
 * share-array, then the new array will be similar -- it will be a copy of
 * array, sharing the elements in the same way.
doc>
 */
DEFINE_PRIMITIVE("array-copy+share",srfi_25_array_copy_share,subr1,(SCM old_array))
{
    if (!ARRAYP(old_array)) STk_error("bad array ~S", old_array);
    SCM new_array;

    long total_size =
        sizeof(struct array_obj) +
        sizeof(int) +                  /* shared */
        sizeof(int*) +                 /* orig_share_count */
#ifndef THREAD_NONE
        ARRAY_MUTEX_SIZE +             /* share_cnt_lock */
#endif
        sizeof(long) +                 /* size */
        sizeof(long) +                 /* length */
        sizeof(int) +                  /* rank */
        sizeof(long) +                 /* offset */
        sizeof(long*) +                /* shape */
        sizeof(long*) +                /* multipliers */
        sizeof(SCM*) +                 /* data_ptr */
        ARRAY_SIZE(old_array);

    NEWCELL_WITH_LEN(new_array, array, total_size);

    struct array_obj *n = (struct array_obj *) new_array;
    struct array_obj *o = (struct array_obj *) old_array;

    memcpy(n,o, total_size);

    MUT_INIT(n->share_cnt_lock);

    /* If old_array was not share-created, then we copied its data vector,
       and nobody points to the new copy. We created a fresh, non-shared array. */
    if (*ARRAY_SHARE_COUNT(old_array) >= 0)  *ARRAY_SHARE_COUNT(new_array)=0;

    /* If old_array was share-created, then we created a new copy of its pointer to the
       original! :)
       We then increase the original array's share-counter. */
    if (*ARRAY_SHARE_COUNT(old_array) == -1) shared_array_inc_count(old_array);

    return new_array;
}


/*
<doc EXT shape-for-each
 * (shape-for-each shape proc [index-object])
 *
 * This procedure will apply proc to all valid sequences of
 * indices in |shape|, in row-major order.
 *
 * If |index-object| is not provided, then |proc| must accept
 * as many arguments as the number of dimensions that the shape
 * describes.
 * @lisp
 * (shape-for-each (shape 1 3 10 12)
 *                 (lambda (x y)
 *                   (format #t "\[~a ~a\]~%" x y)))
 * \[1 10\]
 * \[1 11\]
 * \[2 10\]
 * \[2 11\]
 * @end lisp
 * If |index-object| is provided, it is used as a place to store the
 * indices, so proc must accept either a vector or an array (this is to
 * avoid pushing and popping too many values when calling |proc|).
 * |index-object|, when present, must be aither a vector or array.
 * @lisp
 * (let ((vec (make-vector 2 #f)))
 *   (shape-for-each (shape 1 3 10 12)
 *                   (lambda (o)
 *                     (format #t "\[~a ~a\]~%"
 *                     (vector-ref o 0)
 *                     (vector-ref o 1)))
 *                   vec))
 * \[1 10\]
 * \[1 11\]
 * \[2 10\]
 * \[2 11\]
 *
 * (let ((arr (make-array (shape 0 2))))
 *   (shape-for-each (shape 1 3 10 12)
 *                   (lambda (o)
 *                     (format #t "\[~a ~a\]~%"
 *                     (array-ref o 0)
 *                     (array-ref o 1)))
 *                   arr))
 * \[1 10\]
 * \[1 11\]
 * \[2 10\]
 * \[2 11\]
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("shape-for-each", srfi_25_shape_for_each, vsubr, (int argc, SCM *argv))
{
    if (argc < 2 || argc > 3) STk_error ("either 2 or 3 arguments needed");

    SCM shape = *argv--;
    SCM proc  = *argv;

    if (!ARRAYP(shape))  STk_error("shape ~S is not an array", shape);
    if (!CLOSUREP(proc)) STk_error("bad procedure ~S", proc);

    long * cshape = shapetoCshape(shape);

    int rank = ARRAY_LENGTH(shape)/2;
    SCM idx; // the index vector -- may or may not be used.
    int updated = 1;
    register int i, d, dim;

    if (argc == 3) {
        argv--;
        SCM obj = *argv;

        if (VECTORP(obj)) {
            /*****************/
            /** VECTOR CASE **/
            /*****************/
            idx = obj;

            /* initialize idx with the lowest index for each dimension */
            for (dim = 0; dim < rank; dim++)
                VECTOR_DATA(idx)[dim] = MAKE_INT(cshape[dim*2]);

            updated = 1;
            while(updated) {
                updated = 0;

                STk_C_apply(proc,1,idx);

                /* update idx vector */
                for(d = rank - 1; d >= 0; d--)
                    if (INT_VAL(VECTOR_DATA(idx)[d]) < cshape[d*2+1] - 1) { /* we can increase */
                        VECTOR_DATA(idx)[d] = MAKE_INT(INT_VAL(VECTOR_DATA(idx)[d])+1);
                        for(i = d+1; i < rank; i++) {
                            VECTOR_DATA(idx)[i] = MAKE_INT(cshape[i*2]);
                        }
                        updated=1;
                        break;
                    }
            }
            return STk_void;


        } else if (ARRAYP(obj)) {
            /****************/
            /** ARRAY CASE **/
            /****************/
            idx = obj;

            /* initialize idx with the lowest index for each dimension.
               since we're working on a n array, we need to first get the index into its
               internal data. */
            long i, d, dim;
            for (dim = 0; dim < rank; dim++)
                ASET1(idx, dim, MAKE_INT(cshape[dim*2]));

            updated = 1;
            while(updated) {
                updated = 0;

                STk_C_apply(proc,1,idx);

                /* update idx vector */
                for(d = rank - 1; d >= 0; d--)

                    if (INT_VAL(AREF1(idx, d)) < cshape[d*2+1] - 1) { /* we can increase */
                        ASET1(idx, d, MAKE_INT(INT_VAL(AREF1(idx, d))+1));
                        for(i = d+1; i < rank; i++)
                            ASET1(idx, i, MAKE_INT(cshape[i*2]));

                        updated=1;
                        break;
                    }
            }
            return STk_void;

        } else
            STk_error("index-object ~S is neither array nor vector");
    } else {
        /***************/
        /** ARGS CASE **/
        /***************/
        if (rank != CLOSURE_ARITY(proc) && CLOSURE_ARITY(proc) >=0)
            STk_error("length of shape (~S) is different fromm procedure arity (~S)",
                      MAKE_INT(rank),
                      MAKE_INT(CLOSURE_ARITY(proc)));

        SCM idx = STk_makevect(rank,NULL);

        /* initialize idx with the lowest index for each dimension */
        for (dim = 0; dim < rank; dim++)
            VECTOR_DATA(idx)[dim] = MAKE_INT(cshape[2*dim]);

        while(updated) {
            updated = 0;

            STk_C_apply_list(proc, STk_vector2list(idx));

            /* update idx vector */
            for(d = rank - 1; d >= 0; d--)
                if (INT_VAL(VECTOR_DATA(idx)[d]) < cshape[d*2+1] - 1) { /* we can increase */
                    VECTOR_DATA(idx)[d] = MAKE_INT(INT_VAL(VECTOR_DATA(idx)[d])+1);
                    for(i = d+1; i < rank; i++) {
                        VECTOR_DATA(idx)[i] = MAKE_INT(cshape[i*2]);
                    }
                    updated=1;
                    break;
                }
        }
    }
    return STk_void;
}


/*===========================================================================* \
 *
 *  Array extended type definition
 *
\*===========================================================================*/


static void print_array(SCM array, SCM port, int mode)
{
   /* This should probably use shape-for-each, but that function expects us
      to pass it a Scheme procedure to be called in each step, so I left this
      as a hard-coded loop (actually I first wrote this function then used its
      loop as a template for shape-for-each). -- jpellegrini */
   int i;
   int rank = ARRAY_RANK(array);

   char buffer[100];
   STk_puts("#,(<array> (",port);

   /* write the array shape */
   for (int i =0; i< rank; i++) {
     sprintf(buffer,"%ld %ld",
             ARRAY_SHAPE(array)[2*i],    /* lower bound */
             ARRAY_SHAPE(array)[2*i+1]); /* upper bound */
     STk_puts(buffer, port);
     if (i != rank-1 ) STk_putc(' ', port);
   }
   STk_putc(')', port);

   /* now, the tricky part. we could easily just run through ARRAY_DATA(array),
      printing the elements and including ")(" to separate strides, however
      this would not work for shared arrays, because they may share the same
      ARRAY_DATA from another array, and *NOT* even use all elements there!
      we actually need to use array-ref, so we build an index vector that is
      updated after each iteration.
      for example, if the shape is (2 4 1 3), the index vector will be
      updated sequentially as
      #(2 1)
      #(2 2)
      #(3 1)
      #(3 2)
      and used as argument to array-ref. */

   SCM idx = STk_makevect(rank,NULL);
   int dim;

   /* initialize idx with the lowest values for all indices */
   for (dim = 0; dim < rank; dim++)
       VECTOR_DATA(idx)[dim] = MAKE_INT(ARRAY_SHAPE(array)[dim*2]);

   SCM args[2];
   args[1]=array;
   args[0]=idx;

   /* check for empty arrays */
   int empty = 0;
   for (int d=0; d<rank; d++)
       if (ARRAY_SHAPE(array)[d*2] == ARRAY_SHAPE(array)[d*2+1]) {
           empty = 1;
           break;
       }

   if ( (rank==0) || empty) {
       /* Empty arrays may have a default value. We show it here. */
       if (ARRAY_DATA(array)[0]) {
           STk_putc(' ',port);
           STk_print(ARRAY_DATA(array)[0], port, mode);
       }
   } else {
       STk_puts(" ", port);
       int updated = 1;
       while(updated) {
           updated = 0;

           /* now we print one element: */
           SCM x = STk_srfi_25_array_ref(2, &args[1]);
           if (x)
               STk_print(STk_srfi_25_array_ref(2, &args[1]), port, mode);
           else
               STk_error("array element is NULL, not a Scheme value -- this should not have happened!");


           /* update idx vector */
           for(int d = rank - 1; d >= 0; d--)
               if (INT_VAL(VECTOR_DATA(idx)[d]) < ARRAY_SHAPE(array)[d*2+1] - 1) { /* we can increase */
                   VECTOR_DATA(idx)[d] = MAKE_INT(INT_VAL(VECTOR_DATA(idx)[d])+1);
                   for(i = d+1; i < rank; i++) {
                       VECTOR_DATA(idx)[i] = MAKE_INT(ARRAY_SHAPE(array)[i*2]);
                   }
                   updated=1;
                   STk_putc(' ', port);
                   break;
               }
       }
   }
   STk_putc(')', port);
}


static SCM test_equal_array(SCM x, SCM y)
{
  long ix, iy, lx, ly, rx, ry, i;
  SCM *dx, *dy;
  long *sx, *sy;
  SCM data_x, data_y;

  lx = ARRAY_LENGTH(x); ly = ARRAY_LENGTH(y);
  rx = ARRAY_RANK(x); ry = ARRAY_RANK(y);
  if (lx == ly && rx == ry) {
    dx = ARRAY_DATA(x);
    dy = ARRAY_DATA(y);
    sx = ARRAY_SHAPE(x);
    sy = ARRAY_SHAPE(y);

    for (i=0; i < rx;  i++)
      if (sx[i]!= sy[i]) return STk_false;

    /* check for empty arrays */
   int empty = 0;
   for (int d=0; d<rx; d++)
       if (sx[d*2] == sx[d*2+1]) {
           empty = 1;
           break;
    }

    if ( (rx==0) || empty) {
        /* if none of them have elements, they're equal! */
        if (!dx[0] && !dy[0]) return STk_true;

       /* empty arrays may have a default value. we compare it here. */
        if (dx[0] && dy[0])
            if (STk_equal(dx[0], dy[0]) == STk_true) return STk_true;
    } else {

        /* idx is the vector that will be used as index for the two arrays: */
        SCM idx = STk_makevect(rx,NULL);

        /* initialize idx with the lowest values for all indices */
        for (int dim = 0; dim < rx; dim++)
            VECTOR_DATA(idx)[dim] = MAKE_INT(sx[dim*2]);

        int updated = 1;
        while(updated) {
            updated = 0;

            /* now we compare two elements at the same index: */
            ix = get_index_from_vector(x, idx);
            iy = get_index_from_vector(y, idx);
            data_x = dx[ix];
            data_y = dy[iy];

            if (STk_equal(data_x, data_y) == STk_false) return STk_false;

            /* update idx vector */
            for(int d = rx - 1; d >= 0; d--)
                if (INT_VAL(VECTOR_DATA(idx)[d]) < sx[d*2+1] - 1) { /* we can increase */
                    VECTOR_DATA(idx)[d] = MAKE_INT(INT_VAL(VECTOR_DATA(idx)[d])+1);
                    for(i = d+1; i < rx; i++) {
                        VECTOR_DATA(idx)[i] = MAKE_INT(sx[i*2]);
    }
                    updated=1;
                    break;
                }
        }
    return STk_true;
    }
  }
  return STk_false;
}



static struct extended_type_descr xtype_array = {
  .name  = "array",
  .print = print_array,
  .equal = test_equal_array
};


/*===========================================================================*\
 *
 *  Module for SRFI-25
 *
\*===========================================================================*/

MODULE_ENTRY_START("srfi-25")
{
  SCM module =  STk_create_module(STk_intern("SRFI-25"));

  /* Create a new type for arrays */
  tc_array = STk_new_user_type(&xtype_array);


  ADD_PRIMITIVE_IN_MODULE(srfi_25_arrayp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_shapep, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_make_array, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_shape, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_array, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_array_rank, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_array_start, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_array_end, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_array_ref, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_array_set, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_share_array, module);

  /* EXTRA procedures: */
  ADD_PRIMITIVE_IN_MODULE(srfi_25_shared_arrayp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_array_share_count, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_array_shape, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_array_size, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_array_copy_share, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_25_shape_for_each, module);

  STk_export_all_symbols(module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
