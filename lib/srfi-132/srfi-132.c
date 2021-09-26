/*
 * srfi-25.c   -- Implementation of SRFI-132: Sort Libraries
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
 *    Creation date: 08-Aug-2021 13:40
 * Last file update: 26-Sep-2021 18:54 (eg)
 */

#include <stklos.h>
#include "srfi-132-incl.c"

static void error_bad_list(SCM x)
{
  STk_error("bad list ~W", x);
}

static void error_bad_proc(SCM x)
{
  STk_error("bad procedure ~S", x);
}

static void error_bad_vector(SCM v)
{
  STk_error("bad vector ~s", v);
}


/*************************************
 *
 * Predicates
 *
 *************************************/


DEFINE_PRIMITIVE("list-sorted?", list_sorted, subr2, (SCM pred, SCM l))
{
    if (!(CONSP(l) || NULLP(l))) error_bad_list(l);
    if (STk_procedurep(pred) != STk_true) error_bad_proc(pred);
    if (NULLP(l)) return STk_true;

    SCM original_car; /* for checking circular lists */
    SCM car;
    SCM cdr;
    SCM cadr;

    car = CAR(l);
    original_car = car;
    if (NULLP(CDR(l))) return STk_true;
    if (!CONSP(CDR(l))) error_bad_list(l);
    cdr = CDR(l);
    cadr = CAR(cdr);

    while(1) {
        /* If cadr = original car, then (1) it's a circular list; and
           (2) we checked the whole list already, and got back to the original car.
           We should NOT compare these, as it would be comparing last to first;
           just retrun true! */
        if (cadr == original_car) return STk_true;

        if (STk_C_apply(pred, 2,car, cadr) == STk_false) return STk_false;

        if (NULLP(CDR(cdr))) return STk_true; /* end of list */

        /* move to next: */
        car = cadr;
        cdr = CDR(cdr);
        if (!CONSP(cdr)) error_bad_list(l);
        cadr = CAR(cdr);
    }
    return STk_void; /* never reached */
}

DEFINE_PRIMITIVE("vector-sorted?", vector_sorted, vsubr, (int argc, SCM *argv))
{
    if (argc < 2) STk_error ("requires at least 2 arguments");
    if (argc > 4) STk_error ("requires at most 4 arguments");
    SCM pred = *argv--;
    SCM v = *argv--;

    if (!(VECTORP(v))) error_bad_vector(v);
    if (STk_procedurep(pred) != STk_true) error_bad_proc(pred);

    SCM start;
    SCM end;
    long cstart, cend;

    if (argc>2) {
        start = *argv--;
        if (!INTP(start)) STk_error("bad integer");
        cstart = INT_VAL(start);
    } else cstart = 0;

    if (argc>3) {
        end = *argv--;
        if (!INTP(end)) STk_error("bad integer");
        cend = INT_VAL(end);
    } else cend = VECTOR_SIZE(v);

    for (long i = cstart; i < (cend - 1); i++)
        if (STk_C_apply(pred, 2, VECTOR_DATA(v)[i+1] , VECTOR_DATA(v)[i]) == STk_true)
            return STk_false;

    return STk_true;
}


/*************************************
 *
 * Merge procedures
 *
 *************************************/

DEFINE_PRIMITIVE("list-merge", srfi_132_list_merge, subr3, (SCM less, SCM A, SCM B))
{
    if (STk_procedurep(less) != STk_true) error_bad_proc(less);

    /* need to copy, cannot share storage with input:*/
    if (NULLP(A)) return STk_list_copy(B);
    if (NULLP(B)) return STk_list_copy(A);

    if (!(CONSP(A))) error_bad_list(A);
    if (!(CONSP(B))) error_bad_list(B);


    SCM res, one, two;
    if (STk_C_apply(less, 2, CAR(B), CAR(A)) == STk_true) {
        res = LIST1(CAR(B));
        one = CDR(B);
        two = A;
    } else {
        res = LIST1(CAR(A));
        one = CDR(A);
        two = B;
    }
    SCM cur = res;

    while (CONSP(one) && CONSP(two) ) {
        if ( STk_C_apply(less, 2, CAR(two), CAR(one)) == STk_true ) {
            CDR(cur) = LIST1(CAR(two));
            two = CDR(two);
        } else {
            CDR(cur) = LIST1(CAR(one));
            one = CDR(one);
        }
        cur = CDR(cur);
    }

    if (CONSP(two))
        CDR(cur) = STk_list_copy(two);
    if (CONSP(one))
        CDR(cur) = STk_list_copy(one);

    return res;
}

SCM
list_merge_aux(SCM less, SCM A, SCM B)
{
    if (NULLP(A)) return B;
    if (NULLP(B)) return A;

    if (!(CONSP(A))) error_bad_list(A);
    if (!(CONSP(B))) error_bad_list(B);

    /*
      cur   one
      |     |
      v     v
      a1 -> a2 -> a3 -> ...

      b1 -> b2 -> b3 -> ...
      ^
      |
      two

      cur points to the last element separated into the resulting list.
      one and two are the next two candidates.

      Works in linear time.
     */

    /* res points to the one with the smallest head (will be returned
       as result) */
    SCM res = (STk_C_apply(less, 2, CAR(B), CAR(A)) == STk_true) ? B : A;

    SCM cur = res;
    SCM one;
    SCM two;
    if (cur == A) {
        one = CDR(A);
        two = B;
    } else {
        one = A;
        two = CDR(B);
    }

    while (CONSP(one) && CONSP(two)) {

        if ( STk_C_apply(less, 2, CAR(two), CAR(one)) == STk_true ) {
            CDR(cur) = two;
            cur = two;
            two = CDR(two);
        } else {
            CDR(cur) = one;
            cur = one;
            one = CDR(one);
        }
    }

    if (CONSP(two))
        CDR(cur) = two;

    if (CONSP(one))
        CDR(cur) = one;

    return res;
}

DEFINE_PRIMITIVE("list-merge!", srfi_132_nlist_merge, subr3, (SCM less, SCM A, SCM B))
{
    if (STk_procedurep(less) != STk_true) error_bad_proc(less);
    return list_merge_aux(less, A, B);
}


/*
  returns the NUMBER of skipped items
  also copies that number from A to TO.
*/
long gallop(SCM pred, SCM to,
            SCM a, SCM b,
            long start,
            long starta, long enda,
            long startb, long endb)
{
     /*
      a[starta] <= b[cstartb] so now instead of proceeding linearly,
      use "galloping": search for the position where b[startb] would fit
      but proceed exponentially:

      a[starta]      <= b[startb]
      a[starta + 1]  <= b[startb]
      a[starta + 3]  <= b[startb]
      a[starta + 5]  <= b[startb]
      a[starta + 7]  <= b[startb]
      a[starta + 15] <= b[startb]
                       .
                       .
                       .
      The index growing is starta + (2^k)-1.

      As per the remarks on "galloping" in the CPython implementation of Timsort,
      https://github.com/python/cpython/blob/main/Objects/listsort.txt

      We use galloping for runs with more than 20 elements.
     */
     if ( (enda - starta) > 20) {
         long saved_starta = starta;
         long tmp = starta;
         long i = 2;
         while (starta == tmp && starta < enda) {
             tmp = starta + i - 1;
             if (tmp < enda &&
                 STk_C_apply(pred,2,
                             VECTOR_DATA(a)[tmp],
                             VECTOR_DATA(b)[startb]) == STk_true) {
                 starta = tmp;
                 i = i*2;
             }
         }

         long skipped = starta - saved_starta;

         /* TODO: binary search to find the EXACT place */


         /* copy the beginning of A, which only has elements lesser than
            all elements in B. */
         memcpy( &VECTOR_DATA(to)[start],
                 &VECTOR_DATA(a)[saved_starta],
                 skipped * sizeof(SCM));
         return skipped;
     }
     return 0;
}


void vector_merge_aux(SCM pred, SCM to, SCM v1, SCM v2,
                      long start,
                      long cstart1, long cend1,
                      long cstart2, long cend2)
{

    long skipped;
    if (cstart1 < cend1 &&
        cstart2 < cend2 &&
        STk_C_apply(pred,2,
                    VECTOR_DATA(v1)[cstart1],
                    VECTOR_DATA(v2)[cstart2]) == STk_true) {
        skipped = gallop(pred, to, v1, v2, start, cstart1, cend1, cstart2, cend2);
        cstart1 += skipped;
    } else {
        skipped = gallop(pred, to, v2, v1, start, cstart2, cend2, cstart1, cend1);
        cstart2 += skipped;
    }
    start += skipped;

     /* do the proper merge: */
     int i = start;
     while ((cstart1 < cend1) || (cstart2 < cend2)) {
         if ((cstart1 < cend1) && (cstart2 < cend2)) {

             if (STk_C_apply(pred,2,
                             VECTOR_DATA(v2)[cstart2],
                             VECTOR_DATA(v1)[cstart1]) == STk_true) {
                 VECTOR_DATA(to)[i] = VECTOR_DATA(v2)[cstart2];
                 cstart2++;
             } else {
                 VECTOR_DATA(to)[i] = VECTOR_DATA(v1)[cstart1];
                 cstart1++;
             }
             i++;
         } else break;
    }

    /* when there are elements left in one of the chunks, copy them */
     if (cstart1 < cend1) {
        memcpy(&VECTOR_DATA(to)[i],
               &VECTOR_DATA(v1)[cstart1],
               (cend1-cstart1) * sizeof(SCM));
     }
     if (cstart2 < cend2) {
         memcpy(&VECTOR_DATA(to)[i],
                &VECTOR_DATA(v2)[cstart2],
                (cend2-cstart2) * sizeof(SCM));
     }
}

void check_index(long size, long start, long end)
{
    if (start < 0) STk_error("negative index %d", start);
    if (end > size) STk_error("index too high: %d > %d", end, size);
}


int
vec_init_args(long *cstart1, long *cend1,
                   int argc, SCM *argv,
                   long size1) {
    SCM start1;
    SCM end1;
    int res = 0;

    if (argc>0) {
        start1 = *argv--;
        res++;
        if (!INTP(start1)) STk_error("bad integer for start index");
        *cstart1 = INT_VAL(start1);
    } else *cstart1 = 0;

    if (argc>1) {
        end1 = *argv--;
        res++;
        if (!INTP(end1)) STk_error("bad integer  for end index");
        *cend1 = INT_VAL(end1);
    } else *cend1 = size1;

    check_index(size1,*cstart1,*cend1);

    return res;
}

DEFINE_PRIMITIVE("vector-merge",vector_merge,vsubr, (int argc, SCM *argv))
{
    if (argc < 3) STk_error ("requires at least 3 arguments");
    if (argc > 7) STk_error ("requires at most 7 arguments");
    SCM pred = *argv--;
    SCM v1 = *argv--;
    SCM v2 = *argv--;

    if (!(VECTORP(v1))) error_bad_vector(v1);
    if (!(VECTORP(v2))) error_bad_vector(v2);
    if (STk_procedurep(pred) != STk_true) error_bad_proc(pred);

    long cstart1, cend1, cstart2, cend2;

    argc -= 3;
    int a;

    a = vec_init_args(&cstart1, &cend1,
                      argc, argv,
                      VECTOR_SIZE(v1));
    argc -= a;
    argv -= a;

    vec_init_args(&cstart2, &cend2,
                  argc, argv,
                  VECTOR_SIZE(v2));

    SCM v3 = STk_makevect ( (cend1 - cstart1) + (cend2 - cstart2), MAKE_INT(-1));
                            //(SCM) NULL );

    vector_merge_aux(pred,v3,v1,v2,0,cstart1,cend1,cstart2,cend2);

    return v3;
}

void check_overlap(SCM to,int start,int max, SCM v,int start1,int end1)
{
    if ( ((&VECTOR_DATA(v)[end1] >   &VECTOR_DATA(to)[start]) &&
          (&VECTOR_DATA(v)[start1] < &VECTOR_DATA(to)[max]))
         ||
         ((&VECTOR_DATA(to)[max] >   &VECTOR_DATA(v)[start1]) &&
          (&VECTOR_DATA(to)[start] < &VECTOR_DATA(v)[end1])) )
        STk_error("slices overlap with destination vector");
}


DEFINE_PRIMITIVE("vector-merge!",nvector_merge,vsubr, (int argc, SCM *argv))
{
    if (argc < 4) STk_error ("requires at least 4 arguments");
    if (argc > 9) STk_error ("requires at most 8 arguments");
    SCM pred = *argv--;
    SCM to = *argv--;
    SCM v1 = *argv--;
    SCM v2 = *argv--;
    argc -= 4;

    if (!(VECTORP(to))) error_bad_vector(to);
    if (!(VECTORP(v1))) error_bad_vector(v1);
    if (!(VECTORP(v2))) error_bad_vector(v2);
    if (STk_procedurep(pred) != STk_true) error_bad_proc(pred);

    SCM start;
    long cstart, cstart1, cend1, cstart2, cend2;

    if (argc>0) {
        start = *argv--;
        argc--;
        if (!INTP(start)) STk_error("bad integer ~S", start);
        cstart = INT_VAL(start);
    } else cstart = 0;

    int a = vec_init_args(&cstart1, &cend1,
                          argc, argv,
                          VECTOR_SIZE(v1));

    argc -= a;
    argv -= a;

    vec_init_args(&cstart2, &cend2,
                  argc, argv,
                  VECTOR_SIZE(v2));

    /* to_max is the maximum index on the destination vector.
       its current*/
    long to_max = cstart + (cend1 - cstart1) + (cend2 - cstart2);
    if (to_max > VECTOR_SIZE(to))
        STk_error("merged vector would exceed length of destination");

    check_overlap(to,cstart,to_max, v1,cstart1,cend1);
    check_overlap(to,cstart,to_max, v2,cstart2,cend2);

    if (to_max - cstart > 0)
        vector_merge_aux(pred,to,v1,v2,cstart,cstart1,cend1,cstart2,cend2);

    return STk_void;
}


void merge(SCM v, SCM aux,
           SCM less,
           long *runs,
           long i
           )
{
    memcpy( VECTOR_DATA(aux),
            &(VECTOR_DATA(v)[runs[i-2]]),
            (runs[i-1] - runs[i-2])* sizeof(SCM));

    vector_merge_aux(less,
                     v,                        /* result is in v */
                     aux, v,                   /* the two sections to merge */
                     runs[i-2],                /* where to merge in v */
                     0L,runs[i-1] - runs[i-2], /* one run (in aux) */
                     runs[i-1],runs[i]);       /* one run (in v) */
}

/*************************************
 *
 * Sort procedures
 *
 *************************************/

void insertion_sort(SCM *vec, SCM less, long start, long end)
{
    int r;
    int q = start + 1;
    SCM swap_aux;

    /* TODO: use binary insertion sort */
    while (q < end) {
        r = q;
        while (r > start &&
               STk_C_apply(less, 2, vec[r], vec[r-1] ) == STk_true) {
            swap_aux = vec[r];
            vec[r] = vec[r-1];
            vec[r-1] = swap_aux;
            r--;
        }
        q++;
    }
}

DEFINE_PRIMITIVE("list-stable-sort!",
                 srfi_132_nlist_stable_sort,
                 subr2,
                 (SCM less, SCM list))
{
    /* If someone is using this procedure and not list-stable-sort,
       it is likely to be due to memory constraints (not willing to
       create a full copy of the list). So we will sacrifice performance
       in order to avoid, as much as possible, allocating more memory.

       We will do something similar to timsort.

       1. Find a run.
       2. If it's too short, copy the next M list pointers it onto a vector,
          sort the vector using insertion sort, and we have a run of length
          M.
       3. Push the run (start pointer + size) onto a list. We will need to
          cons here, but it will be a very small list.
       4. Use the timsort criteria for merging the top runs.
       5. Go back to (1).
    */
    if (!(CONSP(list) || NULLP(list))) STk_error("bad list ~S", list);
    if (NULLP(list) || NULLP(CDR(list))) return list;

    SCM runs = STk_nil;
    SCM start = list;
    SCM end   = start;
    long run_len;
    SCM one, two;
    long size1, size2;
    SCM x, *tmp_vec;
    long len = STk_int_length(start);

    /* We need to go through all the list in order to calculate its length --
       and the elements may be scattered all around. However, doing that once
       doesn't seem to hurt in practice. */
    long min_run = len & 0x3f; /* 6 first bits */
    if ( (min_run >> 6) != 0)
        min_run++;
    if (min_run == 0)
        min_run++;

    tmp_vec = STk_must_malloc(min_run * sizeof(SCM));

    while (!NULLP(start) && !NULLP(end)) {
        if (!CONSP(end)) STk_error("improper list ~S", list);

        run_len = 1;
        end = start;

        while (!NULLP(CDR(end)) &&
               STk_C_apply(less, 2, CAR(end), CAR(CDR(end))) == STk_true) {
            run_len++;
            end = CDR(end);
        }
        /* end points to the LAST element in a run (NOT to the element after it) */


        /*
          if size of this run is less than min_run:
             extend the list so as to have min_run length,
             COPY it into vector (do not use vector2lis, don't cons)
             use insertion sort on the vector,
             COPY back into list, with set-car!
        */
        if (run_len < min_run) {
            SCM old_start = start;

            /* copy remaining unordered items on a vector */
            int j;
            for (j=0; j < min_run - run_len; j++) {
                tmp_vec[j] = CAR(start);
                if (NULLP(CDR(start))) { j++; break; } /* j should point to one AFTER the last */
                start = CDR(start);
            }

            /* sort tmp_vec using insertion sort */
            insertion_sort(tmp_vec, less, 0, j);

            /* copy back to the list */
            start = old_start;
            for (int i=0; i < j; i++) {
                CAR(start) = tmp_vec[i];
                start = CDR(start);
            }

            start = old_start;
            run_len = j;

        }

        runs = STk_cons(LIST2(start, MAKE_INT(run_len)), runs);
        start = CDR(end);
        CDR(end) = STk_nil;

        /* The following is the heard of timsort. Look at the runs stack and
           decide if it's time to merge. */
        /* BEGIN of the merging done after each run is queued. */

        int merged;
        do {

            merged = 0;
            if (!NULLP(CDR(runs)) &&                            /* at least three runs on the stack */
                !NULLP(CDR(CDR(runs))) &&
                !NULLP(CDR(CDR(CDR(runs)))) &&
                   INT_VAL(CAR(CDR( CAR(runs) )))               /*    C */
                +  INT_VAL(CAR(CDR( CAR(CDR(runs)) )))          /* +  B */
                >= INT_VAL(CAR(CDR( CAR(CDR(CDR(runs))) )))) {  /* >= A */

                if(   INT_VAL(CAR(CDR( CAR(CDR(CDR(runs))) )))  /*   A */
                   <  INT_VAL(CAR(CDR( CAR(runs) )))) {         /* < C */

                    merged = 1;
                    one = CAR(CDR(runs));       /* B */
                    two = CAR(CDR(CDR(runs)));  /* A */

                    size1 = INT_VAL(CAR(CDR(one)));
                    size2 = INT_VAL(CAR(CDR(two)));

                    x = list_merge_aux(less,
                                       CAR(two),
                                       CAR(one));

                    CDR(runs) = STk_cons(LIST2(x, MAKE_INT(size1 + size2)),
                                         CDR(CDR(CDR(runs))));  /* discard the two runs we merged */
                } else {
                    /* merge BC */
                    merged = 1;
                    one = CAR(runs);
                    two = CAR(CDR(runs));

                    size1 = INT_VAL(CAR(CDR(one)));
                    size2 = INT_VAL(CAR(CDR(two)));

                    x = list_merge_aux(less,
                                       CAR(two),
                                       CAR(one));

                    runs = STk_cons(LIST2(x, MAKE_INT(size1 + size2)),
                                    CDR(CDR(runs))); /* discard the two runs we merged */
                }
            }

            if (!NULLP(CDR(runs)) &&                        /* at least two runs on the stack */
                !NULLP(CDR(CDR(runs))) &&
                   INT_VAL(CAR(CDR( CAR(runs) )))           /*    C */
                >= INT_VAL(CAR(CDR( CAR(CDR(runs)) )))) {   /* >= B */

                merged = 1;
                one = CAR(runs);
                two = CAR(CDR(runs));

                size1 = INT_VAL(CAR(CDR(one)));
                size2 = INT_VAL(CAR(CDR(two)));

                x = list_merge_aux(less,
                                   CAR(two),
                                   CAR(one));

                runs = STk_cons(LIST2(x, MAKE_INT(size1 + size2)),
                                CDR(CDR(runs))); /* discard the two runs we merged */
            }
        } while (merged == 1);
        /* END of the merging done after each run is queued. */

    }

    /* merge all remaining runs on the stack, two at a time */
    while (!NULLP(CDR(runs))){

        one = CAR(runs);
        two = CAR(CDR(runs));

        size1 = INT_VAL(CAR(CDR(one)));
        size2 = INT_VAL(CAR(CDR(two)));

        x = list_merge_aux(less,
                           CAR(two),     /* pointer to first run  */
                           CAR(one));    /* pointer to second run */

        runs = STk_cons(LIST2(x, MAKE_INT(size1 + size2)),
                        CDR(CDR(runs))); /* discard the two runs we merged */
    }

    return CAR(CAR(runs));
}

void reverse_vector(SCM *vec, long start, long end)
{
    long a = start;
    long b = end - 1;
    SCM swap_aux;
    while (a<b) {
        swap_aux = vec[a];
        vec[a] = vec[b];
        vec[b] = swap_aux;
        a++;
        b--;
    }
}


DEFINE_PRIMITIVE("vector-stable-sort!",
                 srfi_132_nvector_stable_sort,
                 vsubr,
                 (int argc, SCM *argv))
{
    if (argc < 2) STk_error ("requires at least 2 arguments");
    if (argc > 4) STk_error ("requires at most 4 arguments");
    SCM less = *argv--;
    SCM v = *argv--;

    if (!(VECTORP(v))) error_bad_vector(v);
    if (STk_procedurep(less) != STk_true) error_bad_proc(less);

    SCM start;
    SCM end;
    long cstart, cend;

    if (argc>2) {
        start = *argv--;
        if (!INTP(start)) STk_error("bad integer ~S for start index", start);
        cstart = INT_VAL(start);
    } else cstart = 0;

    if (argc>3) {
        end = *argv--;
        if (!INTP(end)) STk_error("bad integer ~S for end index", end);
        cend = INT_VAL(end);
    } else cend = VECTOR_SIZE(v);

    long size = cend - cstart;

    /* Rule for computing minrun from timsort,
       https://github.com/python/cpython/blob/main/Objects/listsort.txt */
    long min_timsort_run = size & 0x3f; /* 6 first bits */
    if ( (min_timsort_run >> 6) != 0)
        min_timsort_run++;
    if (min_timsort_run == 0)
        min_timsort_run++;


    /* The following is an implementation of Timsort.
       We find "runs" (portions of the array that are already
       sorted), store their indices in a list (actually an array),
       then merge the list.
       If a run is too short, we take a larger portion of the array
       (which is not sorted) and perform insertion sort on it. */

    SCM aux = STk_makevect(size, STk_void);

    SCM *vec = &VECTOR_DATA(v)[0];

    /* "runs" is an array of long and not int because it stores
       the end indices of runs */
    long runs_size = 1 + (size+1) / min_timsort_run;
    long *runs = STk_must_malloc(sizeof(long) * runs_size);

    /*
      i  points to the beginning of the current run
      j  starts at i+1 and moves forward, and will end up one position AFTER the
         last position of the run
      k  is the sequential index of the run (1st run, 2nd run, etc).
     */

    long i = cstart;
    long j;
    long s;
    long k = 1; /* index of the current run */
    long run_end = i+1;

    runs[0]=cstart;


    while (run_end <= cend) {
        /* Find a run. Try forward, then backward. One of these will immediately fail,
           unless the user has been kind enough to call this procedure with `=` as a
           comparison predicate (which won't hurt anyway). */

        j = s = run_end;

        /* forward */
        while ( (j < cend) &&
                STk_C_apply(less, 2, vec[j-1], vec[j] ) == STk_true) {
            j++;
        }

        /* backward */
        while ( (s < cend) &&
                STk_C_apply(less, 2, vec[s], vec[s-1] ) == STk_true) {
            s++;
        }

        run_end = (s > j) ? s : j;

        /* un-reverse the reversed run, regardless of its size: */
        if (s > j) reverse_vector(vec, i, run_end);


        /* if the run is too short, create a long one with insertion sort
           (cannot be shell, mnust be a stable sort): */
        if ( (run_end - i) < min_timsort_run && run_end < cend) {

            if (i + min_timsort_run >= cend)
                run_end = cend;
            else
                run_end =  i + min_timsort_run;

            insertion_sort(vec, less, i, run_end);
        }


        /* include the end of the run into the list of runs to merge. */
        runs[k] = run_end;


        /* The following is the heard of timsort. Look at the runs stack and
           decide if it's time to merge. */
        /* BEGIN of the merging done after each run is queued. */
        long kk;
        do {
            kk = k;
            if (k > 3 &&
                   runs[k] - runs[k-1]    /*    C */
                +  runs[k-1] - runs[k-2]  /*  + B */
                >= runs[k-2] - runs[k-3]) /* >= A */
                {
                    if (  runs[k-2] - runs[k-3]   /*   A */
                          < runs[k] - runs[k-1])  /* < C */
                        {
                            /* merge AB */
                            merge(v, aux, less, runs, k-1);
                            runs[k-2] = runs[k-1];
                            runs[k-1] = runs[k];
                        k--;
                        } else
                        {
                            /* merge BC */
                            merge(v, aux, less, runs, k);
                            runs[k-1] = runs[k];
                            k--;
                        }
                }
            if (k > 2 &&
                    runs[k] - runs[k-1]    /*    C */
                 >= runs[k-1] - runs[k-2]) /* >= B */
                {
                    merge(v, aux, less, runs, k);
                    runs[k-1] = runs[k];
                    k--;
                }
        } while (kk != k); /* k was decreased, some merge was made */
        /* END of the merging done after each run is queued. */

        i = run_end;
        run_end++;
        k++;
    }

    /* if either j was already >= end when we started, or the vector
       was already sorted, then we return void. */
    if (k <= 1) return STk_void;

    /* merge all remaining runs on the stack, two at a time */
    for (i = k-1; i > 1; i--) {
        /* merge two runs */
        merge(v, aux, less, runs, i);
        runs[i-1] = runs[i];
    }

    return STk_void;
}

DEFINE_PRIMITIVE("vector-stable-sort",
                 srfi_132_vector_stable_sort,
                 vsubr,
                 (int argc, SCM *argv))
{
    SCM *original_argv = argv;

    if (argc < 2) STk_error ("requires at least 2 arguments");
    if (argc > 4) STk_error ("requires at most 4 arguments");

    argv--; /* the predicate */

    SCM v = *argv--;

    if (!(VECTORP(v))) error_bad_vector(v);

    SCM start;
    SCM end;
    long cstart, cend;

    if (argc>2) {
        start = *argv--;
        if (!INTP(start)) STk_error("bad integer ~S for start index", start);
        cstart = INT_VAL(start);
    } else cstart = 0;

    if (argc>3) {
        end = *argv--;
        if (!INTP(end)) STk_error("bad integer ~S for end index", end);
        cend = INT_VAL(end);
    } else cend = VECTOR_SIZE(v);

    long size = cend - cstart;

    SCM w = STk_makevect(size, NULL);

    if (size == 0) return w;

    memcpy(&VECTOR_DATA(w)[0],&VECTOR_DATA(v)[cstart], size * sizeof(SCM));

    /* TODO: we're unboxing-then-boxing-then-unboxing. */
    *(original_argv-1) = w;
    *(original_argv-2) = MAKE_INT(0);    /* new start */
    *(original_argv-3) = MAKE_INT(size); /* new end */
    /* TODO: indices will be unnecessarily checked again! */
    STk_srfi_132_nvector_stable_sort(argc, original_argv);

    return w;
}

/*************************************
 *
 * Deleting duplicate neighbors
 *
 *************************************/


/* Tried to make these efficient, and they should also work with
   improper lists - but the last CDR is ignored when comparing:
   (list-delete-neighbor-dups = '(2 2 3 2 . 2))
   => (2 3 2 . 2)
*/

DEFINE_PRIMITIVE("list-delete-neighbor-dups",
                 srfi_132_list_delete_neighbor_dups,
                 subr2,
                 (SCM eq, SCM lst))
{
    if (!(CONSP(lst) || NULLP(lst))) error_bad_list(lst);
    if (STk_procedurep(eq) != STk_true) error_bad_proc(eq);

    if (NULLP(lst) || !CONSP(CDR(lst))) return lst;

    SCM new = LIST1(CAR(lst));
    SCM ptr = new;
    lst = CDR(lst);

    while (  ( ! NULLP(CDR(lst))  ) &&
             (   CONSP(CDR(lst))  ) )
        if (STk_C_apply(eq, 2, CAR(lst), CAR(ptr)) == STk_true)
            lst = CDR(lst);
        else {
            CDR(ptr) = LIST1(CAR(lst));
            ptr = CDR(ptr);
            lst = CDR(lst);
        }

    if (STk_C_apply(eq, 2, CAR(lst), CAR(ptr)) == STk_false)
        CDR(ptr) = lst;
    else
        CDR(ptr) = CDR(lst);

    return new;
}


DEFINE_PRIMITIVE("list-delete-neighbor-dups!",
                 srfi_132_nlist_delete_neighbor_dups,
                 subr2,
                 (SCM eq, SCM lst))
{
    if (!(CONSP(lst) || NULLP(lst))) error_bad_list(lst);
    if (STk_procedurep(eq) != STk_true) error_bad_proc(eq);

    if (NULLP(lst)) return lst;

    SCM ptr = lst;
    while ( (! NULLP(CDR(ptr))) && CONSP(CDR(ptr)) )
        if (STk_C_apply(eq, 2, CAR(CDR(ptr)), CAR(ptr)) == STk_true)
            CDR(ptr)=CDR(CDR(ptr));
        else
            ptr = CDR(ptr);

    return lst;
}


/* Returns the number of duplicates */
long
srfi_132_vector_del_dups_aux(SCM v, SCM eq, long start, long end)
{
    long dups = 0;
    long i = start ;
    long j = start + 1;

    while (i < end-1 && j < end) {
        /* find next different */
        while (j < end &&
               STk_C_apply(eq, 2,
                           VECTOR_DATA(v)[i],
                           VECTOR_DATA(v)[j]) == STk_true) {
            j++;
            dups++;
        }

        /* advance i and copy next different */
        if ( j < end ) {
            i++;
            VECTOR_DATA(v)[i] = VECTOR_DATA(v)[j];
            j++;
        }
    }
    return dups;
}

DEFINE_PRIMITIVE("vector-delete-neighbor-dups!",
                 srfi_132_nvector_delete_neighbor_dups,
                 vsubr,
                 (int argc, SCM* argv))
{
    if (argc < 2) STk_error ("requires at least 2 arguments");
    if (argc > 4) STk_error ("requires at most 4 arguments");
    SCM eq = *argv--;
    SCM v = *argv--;

    if (!(VECTORP(v))) error_bad_vector(v);
    if (STk_procedurep(eq) != STk_true) error_bad_proc(eq);

    if (VECTOR_SIZE(v)<2) return MAKE_INT(VECTOR_SIZE(v));

    long cstart, cend;

    argc -= 2;
    vec_init_args(&cstart, &cend,
                  argc, argv,
                  VECTOR_SIZE(v));

    long dups = srfi_132_vector_del_dups_aux(v, eq, cstart, cend);

    return MAKE_INT(cend - dups);
}

DEFINE_PRIMITIVE("vector-delete-neighbor-dups",
                 srfi_132_vector_delete_neighbor_dups,
                 vsubr,
                 (int argc, SCM* argv))
{
    if (argc < 2) STk_error ("requires at least 2 arguments");
    if (argc > 4) STk_error ("requires at most 4 arguments");
    SCM eq = *argv--;
    SCM v = *argv--;

    if (!(VECTORP(v))) error_bad_vector(v);
    if (STk_procedurep(eq) != STk_true) error_bad_proc(eq);


    long cstart, cend;

    argc -= 2;
    vec_init_args(&cstart, &cend,
                  argc, argv,
                  VECTOR_SIZE(v));

    SCM w = STk_makevect(cend - cstart, NULL);

    memcpy (&VECTOR_DATA(w)[0],
            &VECTOR_DATA(v)[cstart],
            (cend-cstart) * sizeof(SCM));

    if (VECTOR_SIZE(v)<2) return w;

    long dups = srfi_132_vector_del_dups_aux(w, eq, 0, cend-cstart);

    /* Oh no! We can't realoc VECTOR_DATA, so we need to make yet
       another copy of the vector! */
    SCM u = STk_makevect(VECTOR_SIZE(w) - dups, NULL);
    memcpy (&VECTOR_DATA(u)[0],
            &VECTOR_DATA(w)[0],
            VECTOR_SIZE(u) * sizeof(SCM));
    return u;
}

MODULE_ENTRY_START("srfi-132")
{
    SCM module =  STk_create_module(STk_intern("SRFI-132"));

    ADD_PRIMITIVE_IN_MODULE(list_sorted,module);
    ADD_PRIMITIVE_IN_MODULE(vector_sorted,module);

    ADD_PRIMITIVE_IN_MODULE(srfi_132_nlist_stable_sort,module);
    ADD_PRIMITIVE_IN_MODULE(srfi_132_nvector_stable_sort,module);
    ADD_PRIMITIVE_IN_MODULE(srfi_132_vector_stable_sort,module);

    ADD_PRIMITIVE_IN_MODULE(srfi_132_list_merge, module);
    ADD_PRIMITIVE_IN_MODULE(srfi_132_nlist_merge, module);
    ADD_PRIMITIVE_IN_MODULE(vector_merge,module);
    ADD_PRIMITIVE_IN_MODULE(nvector_merge,module);

    ADD_PRIMITIVE_IN_MODULE(srfi_132_nlist_delete_neighbor_dups, module);
    ADD_PRIMITIVE_IN_MODULE(srfi_132_list_delete_neighbor_dups, module);
    ADD_PRIMITIVE_IN_MODULE(srfi_132_vector_delete_neighbor_dups, module);
    ADD_PRIMITIVE_IN_MODULE(srfi_132_nvector_delete_neighbor_dups, module);

    /* Export all the symbols we have just defined */
    STk_export_all_symbols(module);

   /* Execute Scheme code */
    STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
