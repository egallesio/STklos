/*
 * 160/base.c --  SRFI-160: Homogeneous numeric vector libraries
 *                          (base sublibrary)
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
 *    Creation date: 17-Jun-2022 09:10
 * Last file update: 23-Jun-2022 07:26 (jpellegrini)
 */

#include <float.h>
#include <math.h>
#include "stklos.h"
#include "base-incl.c"

/***
    UTILITIES FOR SRFI-160
 ***/

#define UVECTOR_C64  10
#define UVECTOR_C128 11

extern SCM get_u64_max();
extern SCM get_s64_min();
extern SCM get_s64_max();
extern SCM makeuvect(int type, int len, SCM init);
extern int vector_element_size(int type);
    
EXTERN_PRIMITIVE("list?", listp, subr1, (SCM x));
EXTERN_PRIMITIVE("length", list_length, subr1, (SCM l));

/*
 * We use exact->inexact to cast complex vectors in c64 and c128 types
 * (possibly resultingin +inf.0 or -inf.0)
 */
EXTERN_PRIMITIVE("exact->inexact", ex2inex, subr1, (SCM z));

/*
 * We use nanp in the TAG? predicates.
 */
EXTERN_PRIMITIVE("nan?", nanp, subr1, (SCM z));

static char *type_vector(int tip)
{
  switch (tip) {
    case UVECT_S8:   return "s8";
    case UVECT_U8:   return "u8";
    case UVECT_S16:  return "s16";
    case UVECT_U16:  return "u16";
    case UVECT_S32:  return "s32";
    case UVECT_U32:  return "u32";
    case UVECT_S64:  return "s64";
    case UVECT_U64:  return "u64";
    case UVECT_F32:  return "f32";
    case UVECT_F64:  return "f64";
    case UVECT_C64:  return "c64";
    case UVECT_C128: return "c128";
    default:        return ""; /* never reached */
  }
}

/*********************
 * Arguments checking
 *********************/

static inline
void check_argc(int argc, int n) {
    if (argc !=n) STk_error("Exactly ~A arguments required, got ~A",
			    MAKE_INT(n), MAKE_INT(argc));
}

static inline
void check_int(SCM n) {
    if (!INTP(n)) STk_error("bad integer ~S", n);
}

static inline
void check_uvector(SCM v) {
    if (!UVECTORP(v)) STk_error("bad uvector ~S", v);
}


static inline
void check_indices(SCM v, long i) {
    if (i < 0)               STk_error ("negative index ~A", MAKE_INT(i));
    if (i > UVECTOR_SIZE(v)) STk_error ("index ~A out of bounds", MAKE_INT(i));
}

static inline
void check_type(SCM v, int type) {
    if (type < 0 || type > UVECT_C128)
	STk_error("bad uvector type", MAKE_INT(type));
    if(UVECTOR_TYPE(v) != type)
	STk_error("expected ~Avector, got ~Avector",
		  STk_Cstring2string(type_vector(type)),
		  STk_Cstring2string(type_vector(UVECTOR_TYPE(v))));
}

static inline
void check_procedure(SCM proc) {
    if (! (STk_procedurep(proc) ))
	STk_error("bad procedure ~S", proc);
}

static inline
void check_boolean(SCM proc) {
    if (! (BOOLEANP(proc) ))
	STk_error("bad boolean ~S", proc);
}


/******************
 * Other utilities
 ******************/

SCM
resize_uvector(SCM w, long len, int stride) {
    w = STk_must_realloc(w, sizeof(struct uvector_obj) + stride*len - 1);
    UVECTOR_SIZE(w) = len;
    return w;
}

/*********
  UVECTOR_GET and UVECTOR_COPY - macros around memcpy.
  
  We use memcpy and not uvector_set+uvector-ref because
  using uvector_set+uvector-ref is around 2.2x slower:

  (define v (u64vector-unfold
              (lambda (x y) (values x 0))
              6000000
              0))

  (time (begin (u64vector-remove odd? v) #void))

  This runs in around 600ms with memcpy, and 1400ms with
  the C API.

  Vector-fill! becomes even faster, since using STk_uvector_put
  would check the type of the same element 6000000 times (goes
  from 1800ms to 31ms !

  CAUTION: we use memcpy and not memmove here because it's the
  most commonly needed case in the code, but remember that if
  the chunks overlap, memmove should be used.
*/

#define UVECTOR_GET(dstaddr,v,i,stride) do{                       \
    memcpy((dstaddr),&UVECTOR_DATA(v)[(i) * (stride)], (stride)); \
    }while(0)

#define UVECTOR_SET(v,i,srcaddr,stride) do{                        \
    memcpy(&UVECTOR_DATA(v)[(i) * (stride)], (srcaddr), (stride)); \
    }while(0)

#define UVECTOR_COPY_ELT(to,i,from,j,stride) do{ \
    memcpy(&UVECTOR_DATA(to)[(i) * (stride)],    \
           &UVECTOR_DATA(from)[(j) * (stride)],  \
           (stride));			         \
    }while(0)


/* This function does the copying work for %uvector-copy
 * after the arguments are already checked and adjusted: */
SCM
STk_uvector_copy_contents(int type,
			  SCM to, long to_start, long to_end,
			  SCM from, long from_start, long from_end,
			  SCM reverse, long stride) {
    if (reverse == STk_true &&
	to == from &&
	(from_end >= to_start ||
	 to_end <= from_start ||
	 (from_end == to_end && from_start == to_start))) {
	/* THE CHUNKS OVERLAP *_AND_* WE SHOULD REVERSE!
	   The SRFI spec requires us to
	   do as if we had copied the vector before proceeding, so...
	   We do copy it! (Calling -copy! or -reverse-copy! with
	   overlapping chunks shouldn't be common, and not good
	   practice in my opinion, so even if there is some small
	   room for improvement, let's do the simpler thing here. */
	SCM new = makeuvect(type, from_end - from_start, NULL);
	/* Copying a whole bunch, so not using UVECTOR_COPY_ELT */
	memcpy(UVECTOR_DATA(new),
	       &UVECTOR_DATA(from)[from_start * stride],
	       stride * UVECTOR_SIZE(new));
	return STk_uvector_copy_contents(type,
					 to, to_start, to_end,
					 new, 0, UVECTOR_SIZE(new),
					 reverse, stride);
    }

    if (reverse == STk_true) {
	/* Guaranteed to not overlap (see above), so we can
	   use memcpy. */
	from_end -= 1;
	while (from_end >= from_start) {
	    UVECTOR_COPY_ELT(to, to_start, from, from_end, stride);
	    to_start += 1;
	    from_end -= 1;
	}
    } else {
	/* - May overlap, need to use memmove
	   - Copying a whole bunch
	   So not using UVECTOR_COPY_ELT... */
	memmove(&UVECTOR_DATA(to)[to_start * stride],
		&UVECTOR_DATA(from)[from_start * stride],
		stride * (from_end - from_start));
    }
    return to;
}

/**********************************
 *
 * SCHEME-VISIBLE PRIMITIVES
 *
 **********************************/

/*
 * %uvector-copy is a versatile procedure that can do different things
 * while copying uvector chunks:
 *
 *  (%uvector-copy type
 *                 to at
 *                 from s e
 *                 reverse)
 *
 * Basically, copies the elements of the UVECTOR `from` with indices
 *  s <= idx < e.
 *
 * - `type` is the uvector type
 * - If `reverse` is #t, the elements are copied in reverse order
 * - If `to` is NOT a uvector, then a new uvector is allocator
 * - If `to` IS a uvector, then the elements are copied into it,
 *   beginning at index `at`
 * - Care is taken to not overwrite the uvector itself when copying,
 *   as per the comment in SRFI 133's 'vector-reverse-copy!' (the
 *   procedures of SRFI 133 are mentioned in SRFI 160, and we're
 *   supposed to mimic their behavior).
 *
 * This is used to implement:
 * - vector-take
 * - vector-copy
 * - vector-reverse-copy
 * - vector-copy!
 * - vector-reverse-copy!
 */
DEFINE_PRIMITIVE("%uvector-copy", srfi_160_uvector_copy, vsubr, (int argc, SCM *argv))
{
    check_argc(argc, 7);
    SCM type    = *argv--;
    SCM to      = *argv--;
    SCM at      = *argv--;
    SCM from    = *argv--;
    SCM s       = *argv--;
    SCM e       = *argv--;
    SCM reverse = *argv--;

    /* Basic argument checking: */

    check_int(type);
    check_uvector(from);
    check_int(s);
    check_int(e);
    check_indices(from,INT_VAL(s));
    check_type(from,INT_VAL(type));

    /* Get correct bounds and stride: */
    
    long from_start = INT_VAL(s);
    long from_end   = INT_VAL(e);

    if (from_end < from_start)
	STk_error("end index ~A is smaller than start index ~A", s, e);
    
    long to_start = 0;
    long to_end   = 0;  /* Won't be used*/
    
    long stride = vector_element_size(UVECTOR_TYPE(from));


    /* Check if we have a destinatio to mutate, then do some more bounds
       computation and argument checking: */
    if (UVECTORP(to)) {

	check_int(at);
	check_indices(to,INT_VAL(at));

	to_start = INT_VAL(at);
	to_end   = INT_VAL(at) + (from_end - from_start);

	check_type(to, INT_VAL(type));
	
	if( (UVECTOR_SIZE(to) - INT_VAL(at)) < (from_end - from_start) )
	    STk_error("target vector not large enough for specified chunk: need ~A cells, ~A available",
		      UVECTOR_SIZE(to) - INT_VAL(at),
		      from_end - from_start);
    }
    else
	to = makeuvect(INT_VAL(type), from_end - from_start, NULL);

    /* Do the real work! */
    return STk_uvector_copy_contents(INT_VAL(type),
				     to, to_start, to_end,
				     from, from_start, from_end,
				     reverse, stride);
}


/*
 * %uvector-unfold is the basic unfolding procedure for uvectors.
 * Its arguments are:
 *
 * - type (the uvector type)
 * - vec (the uvector)
 * - f (the function used to generate values)
 * - end (either end in unfold! or the length of the uvector to be created
 *        in unfold).
 * - seed, (similar to knil in unfold)
 * - right (boolean: #t if unfolding is to be done from right to left) 
 */
DEFINE_PRIMITIVE("%uvector-unfold", uvector_unfold, vsubr, (int argc, SCM *argv))
{
    if (argc < 6 || argc > 7)
	STk_error("wrong number of arguments ~A", argc);
    SCM sstart = MAKE_INT(0);
    
    SCM type = *argv--;
    SCM f = *argv--;
    SCM vec = *argv--;
    if (argc == 7) { sstart =  *argv--; check_int(sstart); }
    SCM eend = *argv--;
    SCM seed = *argv--;
    SCM right = *argv--;

    check_int(type);
    check_int(eend);
    check_procedure(f);
    
    long start = INT_VAL(sstart);
    long end   = INT_VAL(eend);

    SCM to = (UVECTORP(vec))
	? vec
	: makeuvect(INT_VAL(type), end, NULL);

    long stride = vector_element_size(UVECTOR_TYPE(to));

    /* the srfi says that f will return two VALUES, so we need a vector to
       hold them... */
    SCM res = STk_makevect(2, NULL);

    long idx, step;
    if (right == STk_true) {
	idx = end-1;
	step  = -1;
    } else {
	idx = start;
	step  = +1;
    }
	
    for (long i = 0;
	 i < end - start;
	 i++, idx += step) {

	VECTOR_DATA(res)[0] = STk_false;
	VECTOR_DATA(res)[1] = STk_false;
	STk_values2vector ( STk_C_apply(f,2,MAKE_INT(idx),seed),
			    res );
        STk_uvector_put(to, idx, VECTOR_DATA(res)[0]);
	seed = VECTOR_DATA(res)[1];
    }
    return to;
}


/*
 * %uvector-append-subvectors implements concatenate, append and
 * append-subvectors for uvectors.
 *
 * - t (the uvector type)
 * - bounds (#t for the -subvectors variant, where the bounds are intercalated
 *   with the vectors, and #f when the list only contains uvectors)
 * - vecs (the list of vectors)
 *
 * All three procedures are easily implemented:
 * (define (u64vector-append . vecs)
 *   (%uvector-append-subvectors 7 #f vecs))
 *
 * (define (u64vector-concatenate vecs)
 *   (%uvector-append-subvectors 7 #f vecs))
 *
 * (define (u64vector-append-subvectors . vecs)
 *   (%uvector-append-subvectors 7 #t vecs))
 *
 */
DEFINE_PRIMITIVE("%uvector-append-subvectors", uvector_append_subs, subr3, (SCM t, SCM bounds, SCM vecs))
{
    check_int(t);
    int type = INT_VAL(t);
    check_boolean(bounds);
    if (NULLP(vecs))       return makeuvect(type, 0, NULL);
    if (!CONSP(vecs))      STk_error("bad list ~S", vecs);

    /* Get the total size AND check types before we begin.
       We can't go operating yet, since we need to know the
       final vector size to allocate. */
    if (bounds == STk_true) {
	int l = STk_int_length(vecs);
	if (l % 3)
	    STk_error ("vector list of wrong length ~A (should be multiple of three, [ vec, start, end ] for each", l);
    }
    SCM ptr = vecs;
    SCM v;
    SCM ss, ee;
    long s, e;
    long len = 0;
    while (ptr != STk_nil) {
	v = CAR(ptr);
	check_type(v,type);
	if (bounds == STk_true) {
	    ss = CAR(CDR(ptr));
	    ee = CAR(CDR(CDR(ptr)));
	    check_int(ss);
	    check_int(ee);
	    s = INT_VAL(ss);
	    e = INT_VAL(ee);
	    check_indices(v, s);
	    check_indices(v, e);
	    /* No problem if e < s */
	} else {
	    s = 0;
	    e = UVECTOR_SIZE(v);
	}
	len += e - s;
	ptr = CDR(ptr);
	if (bounds == STk_true) ptr = CDR(CDR(ptr));
    }

    /* allocate the new vector */
    SCM res = makeuvect(type, len, NULL);
    SCM from;
    long to_start = 0;
    long to_end, from_start, from_end;
    long stride = vector_element_size(type);

    /* copy each chunk */
    for (; vecs != STk_nil ; vecs = CDR(vecs)) {
	from = CAR(vecs);
	if (bounds == STk_true) {
	    from_start = INT_VAL(CAR(CDR(vecs)));
	    from_end   = INT_VAL(CAR(CDR(CDR(vecs))));
	} else {
	    from_start = 0;
	    from_end   = UVECTOR_SIZE(from);
	}
	to_end = to_start + (from_end - from_start);
	STk_uvector_copy_contents(type,
				  res, to_start,  to_end,
				  from, from_start, from_end,
				  STk_false, stride);
	to_start = to_end;
	if (bounds == STk_true) vecs = CDR(CDR(vecs));
    }
    return res;
}

/*
 * PREDICATES
 */

DEFINE_PRIMITIVE("s8?", s8p, subr1, (SCM x))
{
    long v = STk_integer_value(x);
    return MAKE_BOOLEAN (-128 <= v && v < +128);
}

DEFINE_PRIMITIVE("u8?", u8p, subr1, (SCM x))
{
    long v = STk_integer_value(x);
    return MAKE_BOOLEAN (0 <= v && v < +256);
}

DEFINE_PRIMITIVE("s16?", s16p, subr1, (SCM x))
{
    long v = STk_integer_value(x);
    return MAKE_BOOLEAN (-32768 <= v && v < +32768);
}

DEFINE_PRIMITIVE("u16?", u16p, subr1, (SCM x))
{
    long v = STk_integer_value(x);
    return MAKE_BOOLEAN (0 <= v && v < +65536);
}

DEFINE_PRIMITIVE("s32?", s32p, subr1, (SCM x))
{
    int overflow;
    STk_integer2int32(x, &overflow);
    return MAKE_BOOLEAN (!overflow);
}

DEFINE_PRIMITIVE("u32?", u32p, subr1, (SCM x))
{
    int overflow;
    STk_integer2uint32(x, &overflow);
    return MAKE_BOOLEAN (!overflow);
}

DEFINE_PRIMITIVE("s64?", s64p, subr1, (SCM x))
{
    return MAKE_BOOLEAN ( (INTP(x) || BIGNUMP(x)) &&
			  (STk_numle2(get_s64_min(), x) && STk_numle2(x, get_s64_max())) );
}

DEFINE_PRIMITIVE("u64?", u64p, subr1, (SCM x))
{
    return MAKE_BOOLEAN ( (INTP(x) || BIGNUMP(x)) &&
			  (STk_numle2(MAKE_INT(0), x) && STk_numle2(x, get_u64_max())) );
}

DEFINE_PRIMITIVE("f32?", f32p, subr1, (SCM x))
{
    if (REALP(x) &&                      /* MUST be inexact */
	( (REAL_VAL(x) >= -FLT_MAX &&
	   REAL_VAL(x) <= +FLT_MAX) ||   /* 1. within bounds, OR */
	  STk_nanp(x) ||                 /* 2. NaN, OR */ 
	  !isfinite(REAL_VAL(x)) ))      /* 3. infinite */
	return STk_true;
    
    return STk_false;
}

DEFINE_PRIMITIVE("f64?", f64p, subr1, (SCM x))
{
    return MAKE_BOOLEAN(REALP(x));
}

DEFINE_PRIMITIVE("c64?", c64p, subr1, (SCM x))
{
    if ( (STk_f32p(x) == STk_true) ||
	 ( COMPLEXP(x) &&
	   STk_f32p(COMPLEX_REAL(x)) == STk_true &&
	   STk_f32p(COMPLEX_IMAG(x)) == STk_true))
	return STk_true;

    return STk_false;
}

DEFINE_PRIMITIVE("c128?", c128p, subr1, (SCM x))
{
    if ( (STk_f64p(x) == STk_true) ||
	 ( COMPLEXP(x) &&
	   STk_f64p(COMPLEX_REAL(x)) == STk_true &&
	   STk_f64p(COMPLEX_IMAG(x)) == STk_true))
	return STk_true;

    return STk_false;
}

DEFINE_PRIMITIVE("%uvector-empty?", uvector_emptyp, subr2,
		 (SCM tip, SCM vec))
{
    check_uvector(vec);
    check_type(vec, INT_VAL(tip));
    return MAKE_BOOLEAN(UVECTOR_SIZE(vec) == 0);
}

DEFINE_PRIMITIVE("%uvector=", u_vector_equal, subr2, (SCM t, SCM vecs))
{
    check_int(t);
    int type = INT_VAL(t);
    if (NULLP(vecs)) return STk_true;
    if (!STk_listp(vecs))
	STk_error("bad uvector list ~S", vecs);
	
    SCM vec2;
    SCM vec1 = CAR(vecs);
    check_uvector(vec1);
    check_type(vec1, type);
    vecs = CDR(vecs);
    while (vecs != STk_nil) {
	vec2 = CAR(vecs);
	check_uvector(vec2);
	check_type(vec2,type);
	if (UVECTOR_SIZE(vec2) != UVECTOR_SIZE(vec1))
	    return STk_false;
	if (!STk_uvector_equal(vec1, vec2))
	    return STk_false;
	vec1 = vec2;
	vecs = CDR(vecs);
    }
    return STk_true;
}

/***
 * SUPER-ITERATOR:
 *
 * %uvector-iterate.
 *
 * A single function that implements map, for-each, take, drop, fold, and more...
 * Since this function does a Scheme procedure call in each iteration, the impact
 * of having a " switch (operation) { "  at each operation too isn't large.
 * And it's worth it, since there are some interesting mechanism that would
 * be duplicated otherwise (the reversing of traversal for the -right procedures,
 * the application of proc etc).
 *
 * IMPORTANT: The operation tag is passed as first argument. See the #defines below,
 * =========  and also the code in the TAG.stk libraries.
 *
 * ARGUMENTS:
 *
 * - t    (uvector type)
 * - proc (a procedure)
 * - vecs (the list of vectors)
 * - mutate (if #t, map! will write on the first vector; if not, a new one
 *           is allocated)
 * - oper (the operation tag)
 * - from_right (#t if vectors should be traversed from right to left)
 * - seed (a seed -- "knil", when it makes sense)
 *
 *
 * The following procedures are implemented as minimal wrappers on top of
 * %uvector-iterate:
 *
 * - TAGvector-fold
 * - TAGvector-map
 * - TAGvector-for-eaach
 * - TAGvector-count
 * - TAGvector-cumulate
 * - TAGvector-index
 * - TAGvector-skip
 * - TAGvector-any
 * - TAGvector-every
 * - TAGvector-partition
 * - TAGvector-filter
 * - TAGvector-remove
 ***/

#define MAP       0
#define ANY       1
#define EVERY     2
#define INDEX     3
#define SKIP      4
#define COUNT     5
#define PARTITION 6
#define FILTER    7
#define REMOVE    8
#define FOR_EACH  9
#define FOLD      10
#define CUMULATE  11

DEFINE_PRIMITIVE("%uvector-iterate", uvector_iterate, vsubr, (int argc, SCM* argv))
{
    check_argc(argc,7);
    SCM t = *argv--;
    SCM proc = *argv--;
    SCM vecs = *argv--;
    SCM mutate = *argv--;
    SCM oper = *argv--;
    SCM from_right = *argv--;
    SCM seed = *argv--;
    
    check_int(t);
    int type = INT_VAL(t);
    if (type < 0 || type > UVECTOR_C128) STk_error ("bad uvector type descriptor ~A, out of bounds!", t);

    check_procedure(proc);

    /* Fixme: actually check if it's a list? */
    if (!STk_listp(vecs)) STk_error ("bad list ~S", vecs);

    check_boolean(mutate);
    
    for (SCM ptr = vecs; ptr != STk_nil; ptr = CDR(ptr)) {
	check_uvector(CAR(ptr));
	check_type(CAR(ptr), type);
    }

    long arity = INT_VAL(STk_list_length(vecs));
    
    /* vecs should have at least one element, so CAR and CDR are available: */
    long vec_len = UVECTOR_SIZE(CAR(vecs));
    for (SCM ptr = CDR(vecs); ptr != STk_nil; ptr = CDR(ptr)) {
	if (UVECTOR_SIZE(CAR(ptr)) < vec_len)
	    vec_len = UVECTOR_SIZE(CAR(ptr));
    }


    SCM w;
    /* If we're going to mutate the first vector (as in map!) then
       we set w to it. Otherwise (for map), we need a newly allocated
       vector. */
    if (mutate == STk_true)
	w = CAR(vecs);
    else
	w = makeuvect(type, vec_len, NULL);

    /* We will allocate a vector od SCM, ONCE, and then turn each of its cells
     * into CONS cells. This will be the argument list for proc.
     *
     * args will be a C array of cons cells, each pointing to the next.
     *
     * +------------+-------------+         +------------+
     * |  _____ ___ |   _____ ___ |         |  _____ ___ |
     * | | car | ----->| car | ------> ...  | | car | -------> nil
     * |  ----- --- |   ----- --- |         |  ----- --- |
     * +------------+-------------+         +------------+
     *    args[0]      args[1]         ...     args[n-1]
     *
     * In each iteration, we will copy the i-th element of each vector in vecs
     * onto its place in the list, and call STk_C_apply_list.
     *
     * I can't think of a more efficient way to do this.
     */

    struct cons_obj *args = STk_must_malloc(arity * sizeof(struct cons_obj));

	
    /* Adust CDR pointers */
    int i;
    for (i=0; i<arity-1; i++) {
	/* We do the same as NEWCELL would do after allocating each
	   cons cell: */
	BOXED_TYPE(&args[i]) = tc_cons;
	BOXED_INFO(&args[i]) = 0;

	/* And set cdr: */
	CDR(&args[i]) = &args[i+1];
    }
    CAR(&args[arity-1]) = MAKE_INT(- 999);
    CDR(&args[arity-1]) = STk_nil;

    int op = INT_VAL(oper);
    
    long counter = 0;
    long step = +1;
    long idx = 0;
    long stride = vector_element_size(UVECTOR_TYPE(CAR(vecs)));

    
    if (from_right == STk_true) {
	idx = vec_len - 1;
	step  = -1;
    }

    long left_idx  = 0;
    long right_idx = vec_len - 1;

    SCM res = STk_Cstring2string("WARNING! res returned unchanged from %uvector-iterate.");
    
    for(long i=0; i<vec_len; i++, idx += step) {
	SCM vecs_ptr = vecs;
	for (int j=0; j<arity; j++) {
	    res = CAR(vecs_ptr);
	    /* Using uvector_get here, because our UVECTOR_GET
	       macro didn't work in this case... */
	    CAR(&args[j]) = STk_uvector_get(res, idx); /* idx-th element of current vector */
	    vecs_ptr = CDR(vecs_ptr);                  /* next vector */
	}

	/* FIXME: we wouldn't need to CONS here, but it's just one cell... */
	if (op == FOLD || op == CUMULATE)
	    res = STk_C_apply_list(proc, STk_cons(seed,(SCM)&args[0]));
	else
	    res = STk_C_apply_list(proc, (SCM)&args[0]);

	/* NO case for FOR_EACH, since nothing should be done anyway! */
	switch (op) {
	case MAP:
	    STk_uvector_put(w, idx, res);
	    break;
	case ANY:
	    if (res != STk_false) return res;
	    break;
	case EVERY:
	    if (res == STk_false) return STk_false;
	    break;
	case INDEX:
	    if (res != STk_false) return MAKE_INT(idx);
	    break;
	case SKIP:
	    if (res == STk_false) return MAKE_INT(idx);
	    break;
	case COUNT:
	    if (res != STk_false) counter++;
	    break;
	case FOLD:
	    seed = res;
	    break;
	case CUMULATE:
	    seed = res;
	    STk_uvector_put(w, idx, seed);
	    break;

       /* In the following cases there is only ONE uvector
          being traversed, CAR(vecs), so we can access it
          directly. */
	case FILTER:
	    if (res != STk_false) {
                /* Slow:
                   uvector_put(w, left_idx,
                               uvector_ref(type, CAR(vecs), idx)); */
                /* Fast: */
                UVECTOR_COPY_ELT(w, left_idx, CAR(vecs), idx, stride);
		left_idx++;
	    }
	    break;
	case PARTITION:
	    if (res != STk_false) {
                UVECTOR_COPY_ELT(w, left_idx, CAR(vecs), idx, stride);
		left_idx++;
	    } else {
                UVECTOR_COPY_ELT(w, right_idx, CAR(vecs), idx, stride);
		right_idx--;
	    }
	    break;
	case REMOVE:
	    if (res == STk_false) {
                UVECTOR_COPY_ELT(w, left_idx, CAR(vecs), idx, stride);
		left_idx++;
	    }
	    break;
	}
    }
	
    switch (op) {
    case MAP:
	return (mutate == STk_true)
	    ? STk_void /* map! */
	    : w;       /* map  */
    case ANY:       return STk_false;
    case EVERY:     return (vec_len == 0)? STk_true : res;
    case INDEX:     return STk_false;
    case SKIP:      return STk_false;
    case COUNT:     return MAKE_INT(counter);
    case FOLD:      return seed;
    case CUMULATE:  return w;
    case FILTER:    return resize_uvector(w, left_idx, stride);
    case PARTITION:
	/* need to reverse the second part of the vector... */
	char buf[16];
	long max = UVECTOR_SIZE(w);
	right_idx++;
	for (long i=0; i < (max - right_idx)/2; i++) {
	    UVECTOR_GET(&buf[0], w,right_idx +i, stride);
	    UVECTOR_COPY_ELT(w, right_idx + i, w, max -i -1, stride);
	    UVECTOR_SET(w,max - i - 1, &buf[0], stride);
	}
	return w;	
    case REMOVE:    return resize_uvector(w, left_idx, stride);
    case FOR_EACH:  return STk_void;
    }
    return MAKE_INT(-1); /* just to check if we ever get here... */
}


/*
 * %uvector-segment:
 * This is TAGvector-segment, just with an extra first argument for the uvector type.
 */
DEFINE_PRIMITIVE("%uvector-segment", uvector_segment, subr3, (SCM t, SCM v, SCM nn))
{
    check_int(t);
    check_int(nn);
    check_uvector(v);
    int type   = INT_VAL(t);
    check_type(v,type);

    long n     = INT_VAL(nn);
    if (n < 1) STk_error("number of segments should be at least 1, got ~A", nn);
    long vlen  = UVECTOR_SIZE(v);
    long nlast = vlen % n;

    
    SCM res, tmp;
    long idx;
    int stride = vector_element_size(type);
    
    if (nlast == 0)  {
	res = STk_nil;
	idx = vlen - n;
    } else {
	idx = vlen - nlast;
	res = makeuvect(type, nlast, NULL);
	res = LIST1(STk_uvector_copy_contents(type,
					      res, 0, nlast,
					      v, idx, idx + nlast,
					      STk_false, stride));
	idx -=n;
    }
    
    while (idx >= 0) {
	tmp = makeuvect(type, n, NULL);
	tmp = STk_uvector_copy_contents(type,
					tmp, 0, n,
					v, idx, idx + n,
					STk_false, stride);

	res = STk_cons(tmp, res);
	idx -= n;
    }

    return res;
}

/*
 * %uvector-swap! is *exactly* TAG-vector-swap!
 */
DEFINE_PRIMITIVE("%uvector-swap!", uvector_swap, subr3,
		 (SCM vec, SCM i, SCM j))
{
    check_int(i);
    check_int(j);
    check_uvector(vec);
    check_indices(vec,INT_VAL(i));
    check_indices(vec,INT_VAL(j));

    char buf[16];
    int stride = vector_element_size(UVECTOR_TYPE(vec));
    int pos_i = INT_VAL(i);
    int pos_j = INT_VAL(j);
    /* No big difference from using the XOR method... */
    UVECTOR_GET(&buf[0], vec,pos_i, stride);
    UVECTOR_COPY_ELT(vec,pos_i, vec,pos_j, stride);
    UVECTOR_SET(vec,pos_j, &buf[0], stride);
    return STk_void;
}

/*
 * %uvector-fill! is *exactly* TAG-vector-fill!
 */
DEFINE_PRIMITIVE("%uvector-fill!", uvector_fill, vsubr, (int argc, SCM *argv))
{
  if (argc < 2 || argc >4)
      STk_error("wrong number of arguments ~A", argc);
  SCM vec = *argv--;
  check_uvector(vec);
  SCM fill = *argv--;
  SCM s, e;
  long start, end;
  if (argc > 2) {
      s = *argv--;
      check_int(s);
      start = INT_VAL(s);
  } else start = 0;
  if (argc > 3) {
      e = *argv--;
      check_int(e);
      end = INT_VAL(e);
  } else end = UVECTOR_SIZE(vec);
  
  check_indices(vec,start);
  check_indices(vec,end);

  int stride = vector_element_size(UVECTOR_TYPE(vec));

  /* Call STk_uvector_put ONCE only, then use UVECTOR_COPY_ELT
     to copy the first element onto the rest of the vector.
     This makes filling incredibly faster!

     Bonus:
     
     - STk_uvector_put will also check the fill argument and
       the vector type for us!  */
  
  STk_uvector_put(vec, start, fill);
  for (long i=start+1; i<end; i++)
      UVECTOR_COPY_ELT(vec,i,
		       vec,start,
		       stride);

  return STk_void;
}

		 


MODULE_ENTRY_START("srfi/160/base")
{
  SCM module =  STk_create_module(STk_intern("srfi/160/base"));

  ADD_PRIMITIVE_IN_MODULE(s8p,   module);
  ADD_PRIMITIVE_IN_MODULE(u8p,   module);
  ADD_PRIMITIVE_IN_MODULE(s16p,  module);
  ADD_PRIMITIVE_IN_MODULE(u16p,  module);
  ADD_PRIMITIVE_IN_MODULE(s32p,  module);
  ADD_PRIMITIVE_IN_MODULE(u32p,  module);
  ADD_PRIMITIVE_IN_MODULE(s64p,  module);
  ADD_PRIMITIVE_IN_MODULE(u64p,  module);
  ADD_PRIMITIVE_IN_MODULE(f32p,  module);
  ADD_PRIMITIVE_IN_MODULE(f64p,  module);
  ADD_PRIMITIVE_IN_MODULE(c64p,  module);
  ADD_PRIMITIVE_IN_MODULE(c128p, module);

  /* Export all the symbols we have just defined */
  STk_export_all_symbols(module);

  /********************************************************
   * The following are not exported, they will be used to *
   * make procedures exported by separate libraries --    *
   * as per the SRFI text.                                *
   ********************************************************/
  
  ADD_PRIMITIVE_IN_MODULE(srfi_160_uvector_copy,   module);
  ADD_PRIMITIVE_IN_MODULE(uvector_unfold, module);
  ADD_PRIMITIVE_IN_MODULE(uvector_append_subs, module);
  ADD_PRIMITIVE_IN_MODULE(u_vector_equal,     module);
  ADD_PRIMITIVE_IN_MODULE(uvector_emptyp,     module);
  ADD_PRIMITIVE_IN_MODULE(uvector_iterate,        module);
  ADD_PRIMITIVE_IN_MODULE(uvector_segment,    module);
  ADD_PRIMITIVE_IN_MODULE(uvector_swap,       module);
  ADD_PRIMITIVE_IN_MODULE(uvector_fill,       module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
