/*
 * vector.c   -- C part of SRFI-133: Vector Library (R7RS Compatible)
 *
 * Copyright © 2021 Jeronimo Pellegrini <j_p@aleph0.info>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 *           Author: Jeronimo Pellegrini [j_p@aleph0.info]
 *    Creation date: 09-Mar-2021 12:14 (jpellegrini)
 * Last file update: 11-Nov-2022 15:51 (eg)
 */


#include "stklos.h"
#include "vector-incl.c"

DEFINE_PRIMITIVE("check-index", srfi_133_check_index,subr3, (SCM vec,
                                                             SCM index,
                                                             SCM callee))
{
    int i = INT_VAL(index);
    if (i < 0) STk_error("index ~S too low for vector. callee: ~S", index, callee);
    if (i >= VECTOR_SIZE(vec)) STk_error("index ~S too high for vector. callee: ~S", index, callee);
    return index;
}

#define error_with_loc STk_error_with_location


static SCM _check_indices (SCM vec,
                           SCM start,
                           SCM start_name,
                           SCM end,
                           SCM end_name,
                           SCM callee)
{
    if (!VECTORP(vec)) error_with_loc(callee, "bad vector ~S",  vec);
    if (!INTP(start))  error_with_loc(callee, "bad integer ~S", start);
    if (!INTP(end))    error_with_loc(callee, "bad integer ~S", end);

    int cstart = INT_VAL(start);
    int cend   = INT_VAL(end);

    if (cstart < 0) error_with_loc(callee, "vector range out of bounds (~S < 0)", start_name);
    if (cstart > VECTOR_SIZE(vec)) error_with_loc(callee, "vector range out of bounds (~S > len)", start_name);
    if (cend > VECTOR_SIZE(vec)) error_with_loc(callee, "vector range out of bounds (~S > len)", end_name);
    if (cstart > cend) error_with_loc(callee, "vector range out of bounds: ~S > ~S", start_name, end_name);

    return STk_n_values(2,start,end);
}


DEFINE_PRIMITIVE("vector-parse-start+end",srfi_133_vector_parse_start_end,subr5,
                 (SCM vector,
                  SCM args,
                  SCM start_name,
                  SCM end_name,
                  SCM callee))
{
    if (!VECTORP(vector)) error_with_loc(callee, "bad vector ~S",  vector);
    int len = VECTOR_SIZE(vector);

    if (NULLP(args)) {
        return STk_n_values(2,MAKE_INT(0),MAKE_INT(len));
    } else if (NULLP(CDR(args))) {
        return _check_indices(vector,
                              CAR(args), start_name,
                              MAKE_INT(len), end_name,
                              callee);
    } else if (NULLP(CDR(CDR(args)))) {
        return _check_indices(vector,
                              CAR(args), start_name,
                              CAR(CDR(args)), end_name,
                              callee);
    } else error_with_loc(callee, "too many arguments. callee: ~S. extra args: ~S", callee, CDR(CDR(args)));

    return STk_void; /* never reached */
}

DEFINE_PRIMITIVE("%smallest-length",srfi_133__smallest_length,subr3,(SCM vecs,
                                                                     SCM default_length,
                                                                     SCM callee))
{
    if (NULLP(vecs)) return default_length;
    int min = INT_VAL(default_length);
    SCM vec;
    while(!NULLP(vecs)) {
        vec = CAR(vecs);
        if (!VECTORP(vec)) error_with_loc(callee, "~S: bad vector ~S", callee, vec);
        if (VECTOR_SIZE(vec) < min)
            min = VECTOR_SIZE(vec);
        vecs = CDR(vecs);
    }
    return MAKE_INT(min);
}



DEFINE_PRIMITIVE("%vector-copy!",srfi_133__nvector_copy,subr5,(SCM target,
                                                               SCM tstart,
                                                               SCM source,
                                                               SCM sstart,
                                                               SCM send))
{
    int ctstart = INT_VAL(tstart);
    int csstart = INT_VAL(sstart);
    int csend   = INT_VAL(send);

    while (csstart < csend) {
        VECTOR_DATA(target)[ctstart] = VECTOR_DATA(source)[csstart];
        ctstart++;
        csstart++;
    }
    return STk_void;
}

DEFINE_PRIMITIVE("%vector-reverse-copy!",srfi_133__nvector_reverse_copy,subr5,(SCM target,
                                                                               SCM tstart,
                                                                               SCM source,
                                                                               SCM sstart,
                                                                               SCM send))
{
    int ctstart = INT_VAL(tstart);
    int csstart = INT_VAL(sstart);
    int csend   = INT_VAL(send) - 1;

    /* R7RS specifies that, for vector-copy!,

       "The order in which elements are copied is unspecified, except
       that if the source and destination overlap, copying takes place
       as if the source is first copied into a temporary vector and
       then into the destination. This can be achieved without
       allocating storage by making sure to copy in the correct
       direction in such circumstances.",

       and that vector-reverse-copy! is "Like vector-copy!, but the
       elements appear in to in reverse order."

       So that restriction is also true for vector-reverse-copy!.

       Of course, we split this in two cases, overlapping and
       non-overlapping...

       --jpellegrini */

    int size = csend - csstart + 1;
    int ctend  = ctstart + size;

    if (target == source &&
        (csend >= ctstart || ctend <= csstart ||
         (csend == ctend && csstart == ctstart))) {
      SCM *v = VECTOR_DATA(target);

      // copy (in order) to destination
      for (int i = csend, j=ctend-1; j >= ctstart; i--, j--) {
        v[j] = v[i];
      }

      // reverse in place
      for (int i = ctstart, j = ctend-1; j > i; i++, j--) {
        SCM tmp = v[i]; v[i] = v[j]; v[j] = tmp;
      }
    } else {
      while (csend >= csstart) {
	    VECTOR_DATA(target)[ctstart] = VECTOR_DATA(source)[csend];
	    ctstart++;
	    csend--;
      }
    }
    return STk_void;
}

DEFINE_PRIMITIVE("%vector-reverse!",srfi_133__nvector_reverse,subr3,(SCM vec,
                                                                     SCM start,
                                                                     SCM end))
{
    int cstart = INT_VAL(start);
    int cend   = INT_VAL(end);
    int i      = cstart;
    int j      = cend - 1;
    SCM tmp;

    while( i < j ) {
        tmp                 = VECTOR_DATA(vec)[i];
        VECTOR_DATA(vec)[i] = VECTOR_DATA(vec)[j];
        VECTOR_DATA(vec)[j] = tmp;
        i++;
        j--;
    }
    return STk_void;
}


DEFINE_PRIMITIVE("%vector-fold1",srfi_133__vector_fold1, subr4, (SCM kons,
                                                                 SCM knil,
                                                                 SCM len,
                                                                 SCM vec))
{
    int i = 0;
    int clen=INT_VAL(len);
    while(1) {
        if (i == clen) return knil;
        knil=STk_C_apply(kons,2, knil, VECTOR_DATA(vec)[i]);
        i++;
    }
}

DEFINE_PRIMITIVE("%vector-fold2+",srfi_133__vector_fold2, subr4, (SCM kons,
                                                                  SCM knil,
                                                                  SCM len,
                                                                  SCM vecs))
{
    int i;
    int j;
    SCM arg;
    SCM vec;
    int n_vecs = STk_int_length(vecs);

    /* build a list with length len.
       each time f is applied, this will be the argument list.
       we only build it once, so we don't need to cons all the
       time (instead, we run through the list and set all cars
       in each iteration).
       The length of this will be the length of vecs
       PLUS ONE, because kons is appleid to (KNIL . ARGS) .  */
    SCM args = STk_nil;
    for (j=0; j < n_vecs+1 ; j++)
        args = STk_cons(STk_void, args);

    i = 0;
    while(i < INT_VAL(len)) {

        /* calculate the args list: */
        vec = vecs;
        arg = args;
        /* prepend KNIL to args (see the reference implementation */
        CAR(arg) = knil;
        arg=CDR(arg);
        for (j=0; j < n_vecs; j++) {
            CAR(arg) = VECTOR_DATA(CAR(vec))[i];
            arg = CDR(arg);
            vec = CDR(vec);
        }

        /* apply kons to knil, args and set knil: */
        knil = STk_C_apply_list(kons, args);
        i++;
    }
    return knil;
}

DEFINE_PRIMITIVE("%vector-map1!",srfi_133__nvector_map1, subr4, (SCM f,
                                                               SCM target,
                                                               SCM vec,
                                                               SCM len))
{
    int i=INT_VAL(len);
    while(1) {
        if (i == 0) return target;
        i--;
        VECTOR_DATA(target)[i] = STk_C_apply(f,1, VECTOR_DATA(vec)[i]);
    }
}

DEFINE_PRIMITIVE("%vector-map2+!",srfi_133__nvector_map2, subr4, (SCM f,
                                                                  SCM target,
                                                                  SCM vecs,
                                                                  SCM len))
{
    int i = INT_VAL(len);
    int j;
    SCM arg;
    SCM vec;
    int n_vecs = STk_int_length(vecs);

    /* build a list with length len.
       each time f is applied, this will be the argument list.
       we only build it once, so we don't need to cons all the
       time (instead, we run through the list and set all cars
       in each iteration).
       The length of this will be the length of vecs.  */
    SCM args = STk_nil;
    for (j=0; j < n_vecs ; j++)
        args = STk_cons(STk_void, args);

    while(i != 0) {
        i--;

        /* calculate the args list: */
        vec = vecs;
        arg = args;
        for (j=0; j < n_vecs; j++) {
            CAR(arg) = VECTOR_DATA(CAR(vec))[i];
            arg = CDR(arg);
            vec = CDR(vec);
        }

        /* apply f to args and and set i-th target element
           to the rsult: */
        VECTOR_DATA(target)[i] = STk_C_apply_list(f, args);
    }
    return target;
}


DEFINE_PRIMITIVE("vector-empty?",srfi_133_vector_emptyp,subr1, (SCM vec))
{
    if (!VECTORP(vec)) STk_error("bad vector ~S", vec);
    return (VECTOR_SIZE(vec) == 0)
        ? STk_true
        : STk_false;
}

MODULE_ENTRY_START("scheme/vector")
{
  SCM module =  STk_create_module(STk_intern("scheme/vector"));

  ADD_PRIMITIVE_IN_MODULE(srfi_133_check_index, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_133_vector_parse_start_end, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_133__smallest_length, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_133__nvector_copy, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_133__nvector_reverse_copy, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_133__nvector_reverse, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_133__vector_fold1, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_133__vector_fold2, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_133__nvector_map1, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_133__nvector_map2, module);

  ADD_PRIMITIVE_IN_MODULE(srfi_133_vector_emptyp, module);

  /* Export all the symbols we have just defined */
  STk_export_all_symbols(module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
