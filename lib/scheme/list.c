/*
 *  list.c         -- Partial implementation of (scheme list) aka SRFI-1
 *
 *  Copyright © 2023-2025 Jeronimo Pellegrini - <j_p@aleph0.info>
 *
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
 *  USA.
 *
 *            Author: Jeronimo Pellegrini [j_p@aleph0.info]
 *     Creation date: 18-Apr-2023 14:21 (jpellegrini)
 *
 */

#include <stklos.h>

#include "list-incl.c"

static void error_bad_list(SCM l) {
  STk_error("bad list ~s", l);
}

static void error_circular_list(SCM l) {
  STk_error("circular list ~s", l);
}

static void error_bad_integer(SCM l) {
  STk_error("bad integer ~s", l);
}


static void error_negative_count(SCM v) {
  STk_error("negative count ~S", v);
}

static void error_count_too_big(SCM v) {
  STk_error("count (~s) is larger than list size", v);
}

static void error_const_cell(SCM v) {
  STk_error("changing the constant ~s is not allowed", v);
}

static inline void verify_count(SCM v) {
  if (!INTP(v))       error_bad_integer(v);
  if (INT_VAL(v) < 0) error_negative_count(v);
}


// ----------------------------------------------------------------------

/* cars_cdrs is the heart of the CAR/CDR extracting internal utilities
   for SRFI-1. THe parameters are:
   cars_final       = element to use after the last one in the CAR list.
                      If null, none is added
   do_cars, co_cdrs = which lists should be computed? cars, cdrs, or both?
   test             = if one of the lists is null, return NIL (test=1) or
                      error (test=0)                                        */

static SCM cars_cdrs(SCM lists, SCM cars_final, int do_cars, int do_cdrs, int test) {
  SCM cars = STk_nil;
  SCM cdrs = STk_nil;

  if (!(CONSP(lists) || NULLP(lists))) error_bad_list(lists);

  while(CONSP(lists)) {
    if (NULLP(CAR(lists))) {
      if (test) return (!do_cars || !do_cdrs)
                  ? STk_nil
                  : STk_n_values(2, STk_nil, STk_nil);
      else
        STk_error("empty list");
    }

    if (CONSP(CAR(lists))) {
      if (do_cars) cars = STk_cons(CAR(CAR(lists)), cars);
      if (do_cdrs) cdrs = STk_cons(CDR(CAR(lists)), cdrs);
      lists = CDR(lists);
    } else break;
  }
  if (!do_cars) return STk_dreverse(cdrs);
  if (cars_final) cars = STk_cons(cars_final, cars);
  if (!do_cdrs) return STk_dreverse(cars);
  return STk_n_values(2, STk_dreverse(cars), STk_dreverse(cdrs));
}

DEFINE_PRIMITIVE("%cars+", cars, subr2, (SCM lists, SCM fin)) {
  return cars_cdrs(lists, fin, 1, 0, 1);
}

DEFINE_PRIMITIVE("%cdrs", cdrs, subr1, (SCM lists)) {
  return cars_cdrs(lists, NULL, 0, 1, 1);
}

DEFINE_PRIMITIVE("%cars+cdrs", cars_cdrs, subr1, (SCM lists)) {
  return cars_cdrs(lists, NULL, 1, 1, 1);
}

DEFINE_PRIMITIVE("%cars+cdrs+", cars_cdrs_fin, subr2, (SCM lists, SCM fin)) {
  return cars_cdrs(lists, fin, 1, 1, 1);
}

DEFINE_PRIMITIVE("%cars+cdrs/notest", cars_cdrs_notest, subr1, (SCM lists)) {
  return cars_cdrs(lists, NULL, 1, 1, 0);
}

/*   We already have STk_list_type_and_length in list.c, which does most
     of the job, so it makes no sense to use the reference
     implementation of SRFI-1 for length+, circular-list? and
     dotted-list?.

     STk_list_type_and_length's return value depends on what the list is:
     1. PROPER LIST: return STk_nil.
     2. CYCLIC LIST: A CONS cell (this is where the cycle starts).
     3. FINITE IMPROPER LIST: The last CDR.
     4. NOT A LIST: NULL.  */

DEFINE_PRIMITIVE("length+", length_plus, subr1, (SCM list)) {
  int len;
  SCM res = STk_list_type_and_length(list, &len);

  if (res == NULL) error_bad_list(list);           /* not a list */
  if (res == STk_nil) return MAKE_INT(len);        /* proper */
  return STk_false;                                /* improper (dotted/circular) */
}

DEFINE_PRIMITIVE("dotted-list?", dotted_list, subr1, (SCM list)) {
  int len;
  SCM res = STk_list_type_and_length(list, &len);

  if (res == NULL) error_bad_list(list);    /* not a list */
  return MAKE_BOOLEAN(!CONSP(res)       &&  /* not circular */
                      !(res == STk_nil));   /* not proper */
}

DEFINE_PRIMITIVE("iota", iota, vsubr, (int argc, SCM *argv)) {
  /*  count [start step] */
  if (argc < 1) STk_error("at least one argument needed");
  SCM count, start, step;

  count = *argv--; argc--;
  if (!(INTP(count))) error_bad_integer(count);

  if (INT_VAL(count) == 0) return STk_nil;

  if (argc) {
    start = *argv--; argc--;
  } else {
    start = MAKE_INT(0);
  }
  if (argc) {
    step = *argv--; argc--;
  } else {
    step = MAKE_INT(1);
  }

  if (INT_VAL(count) < 0) error_negative_count(count);

  SCM list = STk_C_make_list(INT_VAL(count), start);
  SCM ptr  = CDR(list); /* CAR is already initialized by STk_C_make_list */

  for (long c = 1; c < INT_VAL(count); c++, ptr = CDR(ptr)) {
    start = STk_add2(start, step);
    CAR(ptr) = start;
  }
  return list;
}

DEFINE_PRIMITIVE("take", take, subr2, (SCM lis, SCM k)) {
  SCM ptr, res;

  verify_count(k);
  ptr = res = STk_C_make_list(INT_VAL(k), STk_false);

  for (int i = INT_VAL(k); i; i--) {
    if (!CONSP(lis)) error_count_too_big(k);
    CAR(ptr) = CAR(lis);
    lis = CDR(lis);
    ptr = CDR(ptr);
  }
  return res;
}

DEFINE_PRIMITIVE("take!", ntake, subr2, (SCM lis, SCM k)) {
  SCM ptr = lis;

  verify_count(k);
  if (k == MAKE_INT(0)) return STk_nil;

  for (int i = INT_VAL(k) - 1; i; i--) {
    if (!CONSP(ptr)) error_count_too_big(k);
    ptr = CDR(ptr);
  }
  if (!CONSP(ptr)) error_count_too_big(k);
  if (BOXED_INFO(ptr) & CONS_CONST) error_const_cell(lis);
  CDR(ptr) = STk_nil;

  return lis;
}

DEFINE_PRIMITIVE("drop", drop, subr2, (SCM lis, SCM k)) {
  SCM res = lis;

  verify_count(k);

  for (int i = INT_VAL(k); i; i--) {
    if (!CONSP(res)) error_count_too_big(k);
    res = CDR(res);
  }
  return res;
}

DEFINE_PRIMITIVE("take-right", take_right, subr2, (SCM lis, SCM k)) {
  int len;
  SCM res = STk_list_type_and_length(lis, &len);
  SCM ptr = lis;

  verify_count(k);

  if (CONSP(res))  error_circular_list(lis);
  if (res == NULL) error_bad_list(lis);

  for (int i = 0; i < len - INT_VAL(k); i++)
    ptr = CDR(ptr);
  return ptr;
}

DEFINE_PRIMITIVE("drop-right", drop_right, subr2, (SCM lis, SCM k)) {
  /* The spec says " If the argument is a list of non-zero length,
     drop-right is guaranteed to return a freshly-allocated list, even
     in the case where nothing is dropped, e.g. (drop-right lis
     0)." */
  int len;

  verify_count(k);

  SCM res = STk_list_type_and_length(lis, &len);

  if (CONSP(res))  error_circular_list(lis);
  if (res == NULL) error_bad_list(lis);

  if (INT_VAL(k) == len) return STk_nil;

  int size = len - INT_VAL(k);
  if (size < 0) error_count_too_big(k);

  res = STk_C_make_list(size, STk_false);
  SCM ptr = res;

  for (; size-1; size--) {
    CAR(ptr) = CAR(lis);
    lis = CDR(lis);
    ptr = CDR(ptr);
  }
  CAR(ptr) = CAR(lis);
  CDR(ptr) = STk_nil;

  return res;
}

DEFINE_PRIMITIVE("drop-right!", ndrop_right, subr2, (SCM lis, SCM k)) {
  int len;

  verify_count(k);

  SCM res = STk_list_type_and_length(lis, &len);

  if (CONSP(res))  error_bad_integer(lis);
  if (res == NULL) error_bad_list(lis);

  if (INT_VAL(k) == len) return STk_nil;

  int size = len - INT_VAL(k);
  if (size < 0) error_count_too_big(k);

  SCM ptr = lis;

  for (; size-1; size--)
    ptr = CDR(ptr);

  /* Just cut the tail setting the CDR to NIL: */
  if (BOXED_INFO(ptr) & CONS_CONST) error_const_cell(lis);
  CDR(ptr) = STk_nil;

  return lis;
}

DEFINE_PRIMITIVE("split-at", split_at, subr2, (SCM lis, SCM k)) {
  int len;
  SCM res = STk_list_type_and_length(lis, &len);

  verify_count(k);

  if (res == NULL) error_bad_list(lis);      /* not a list */
  if (res == STk_nil || !CONSP(res))         /* finite proper or dotted list */
    if (INT_VAL(k) > len) error_count_too_big(k);
                                             /* don't test k if lis is circular */

  SCM left     = STk_C_make_list(INT_VAL(k), STk_false);
  SCM left_ptr = left;

  for (long i=0; i < INT_VAL(k); i++) {
    CAR(left_ptr) = CAR(lis);
    left_ptr      = CDR(left_ptr);
    lis           = CDR(lis);
  }
  return STk_n_values(2, left, lis);
}

DEFINE_PRIMITIVE("split-at!", nsplit_at, subr2, (SCM lis, SCM k)) {
  int len;
  SCM res = STk_list_type_and_length(lis, &len);

  verify_count(k);

  if (res == NULL) error_bad_list(lis);      /* not a list */
  if (res == STk_nil || !CONSP(res))         /* finite proper or dotted list */
    if (INT_VAL(k) > len) error_count_too_big(k);
                                             /* don't test k if lis is circular */
  SCM prev = STk_nil;
  SCM ptr  = lis;

  for (long i=0; i < INT_VAL(k); i++) {
    prev = ptr;
    ptr  = CDR(ptr);
  }
  if (INT_VAL(k) > 0) {
    if (BOXED_INFO(prev) & CONS_CONST) error_const_cell(lis);
    CDR(prev) = STk_nil;
    return STk_n_values(2, lis, ptr);
  }
  return STk_n_values(2, STk_nil, lis);     /* k = 0 */
}


MODULE_ENTRY_START("scheme/list")
{
  SCM module =  STk_create_module(STk_intern("scheme/list"));

  ADD_PRIMITIVE_IN_MODULE(cars, module);
  ADD_PRIMITIVE_IN_MODULE(cdrs, module);
  ADD_PRIMITIVE_IN_MODULE(cars_cdrs, module);
  ADD_PRIMITIVE_IN_MODULE(cars_cdrs_fin, module);
  ADD_PRIMITIVE_IN_MODULE(cars_cdrs_notest, module);

  ADD_PRIMITIVE_IN_MODULE(length_plus, module);
  ADD_PRIMITIVE_IN_MODULE(dotted_list, module);
  ADD_PRIMITIVE_IN_MODULE(iota, module);

  ADD_PRIMITIVE_IN_MODULE(take, module);
  ADD_PRIMITIVE_IN_MODULE(drop, module);
  ADD_PRIMITIVE_IN_MODULE(ntake, module);
  ADD_PRIMITIVE_IN_MODULE(take_right, module);
  ADD_PRIMITIVE_IN_MODULE(drop_right, module);
  ADD_PRIMITIVE_IN_MODULE(ndrop_right, module);
  ADD_PRIMITIVE_IN_MODULE(split_at, module);
  ADD_PRIMITIVE_IN_MODULE(nsplit_at, module);

  STk_export_all_symbols(module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
