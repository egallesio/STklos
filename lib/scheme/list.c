/*
 *  list.c         -- Partial implementation of (scheme list) aka SRFI-1
 *
 *  Copyright © 2023 Jeronimo Pellegrini - <j_p@aleph0.info>
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

/* cars_cdrs is the heart of the CAR/CDR extracting internal utilities
   for SRFI-1. THe parameters are:
   cars_final       = element to use after the last one in the CAR list.
                      If null, none is added
   do_cars, co_cdrs = which lists should be computed? cars, cdrs, or both?
   test             = if one of the lists is null, return NIL (test=1) or
                      error (test=0)                                        */
static inline SCM cars_cdrs(SCM lists, SCM cars_final, int do_cars, int do_cdrs, int test) {
  SCM cars = STk_nil;
  SCM cdrs = STk_nil;

  if (!(CONSP(lists) || NULLP(lists)))
    STk_error("bad list ~S", lists);

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
  if (!do_cars) return STk_reverse(cdrs);
  if (cars_final) cars = STk_cons(cars_final, cars);
  if (!do_cdrs) return STk_reverse(cars);
  return STk_n_values(2, STk_reverse(cars), STk_reverse(cdrs));
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

MODULE_ENTRY_START("scheme/list")
{
  SCM module =  STk_create_module(STk_intern("scheme/list"));

  ADD_PRIMITIVE_IN_MODULE(cars, module);
  ADD_PRIMITIVE_IN_MODULE(cdrs, module);
  ADD_PRIMITIVE_IN_MODULE(cars_cdrs, module);
  ADD_PRIMITIVE_IN_MODULE(cars_cdrs_fin, module);
  ADD_PRIMITIVE_IN_MODULE(cars_cdrs_notest, module);

  STk_export_all_symbols(module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
