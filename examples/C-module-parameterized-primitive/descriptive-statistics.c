/*
 * descriptive-statistics.c  -- A simple module in C and Scheme with
 *                              a parameterizable primitive
 *
 * Copyright © 2025 Jeronimo Pellegrini <j_p@aleph0.info>
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
 *    Creation date: 25-Mar-2025 11:35 (jpellegrini)
 */


#include "stklos.h"

#include "descriptive-statistics-incl.c"

SCM STk_desc_stat_arithmetic;
SCM STk_desc_stat_geometric;
SCM STk_desc_stat_harmonic;

EXTERN_PRIMITIVE("sqrt", sqrt, subr1, (SCM z));

/*
  mean: last argument is a keyword that selects the type of mean:
  :arithmetic
  :geometric
  :harmonic
  The other two (exactly two) arguments are the numbers for which
  the mean will be computed.
*/
DEFINE_PRIMITIVE("mean", mean, vsubr, (int argc, SCM *argv))
{
  if (argc != 3) STk_error("exactly two arguments needed (~s given)", MAKE_INT(argc-1));
  if (argv[-2] == STk_desc_stat_arithmetic) {
    return STk_div2(STk_add2(argv[0], argv[-1]),
                    MAKE_INT(2));
  }
  if (argv[-2] == STk_desc_stat_geometric) {
    return STk_sqrt(STk_mul2(argv[0], argv[-1]));
  }
  if (argv[-2] == STk_desc_stat_harmonic) {
    return STk_div2(MAKE_INT(2),
                    STk_add2(STk_div2(MAKE_INT(1),argv[0]),
                             STk_div2(MAKE_INT(1),argv[-1])));
  }
  STk_error("bad mean selector ~s", argv[-2]);
  return STk_void; /* Never reached */
}

/*
<doc make-mean
 * (make-mean type)
 *
 * Returns a procedure that implements the requested mean type, for
 * two numbers. Type can be |:arithmetic|, |:geometric| or |:harmonic|.
doc>
*/
DEFINE_PRIMITIVE("make-mean", make_mean, subr1, (SCM type))
{
  if (!KEYWORDP(type)) STk_error("bad keyword ~s", type);
  SCM p;
  NEWCELL_WITH_LEN(p, param_vsubr, sizeof(struct pprimitive_obj));
  PRIMITIVE_NAME(p) = "mean";   /* Name for *this* instance being created. */
  PRIMITIVE_FUNC(p) = STk_mean; /* Because we used the mean primitive above. */
  PRIMITIVE_PARAM(p) = type;
  return p;
}

MODULE_ENTRY_START("descriptive-statistics")
{
  /* We first actually create the module: */
  SCM module =  STk_create_module(STk_intern("descriptive-statistics"));

  STk_desc_stat_arithmetic = STk_makekey("arithmetic");
  STk_desc_stat_geometric  = STk_makekey("geometric");
  STk_desc_stat_harmonic   = STk_makekey("harmonic");;

  /* Include Scheme primitives in the module: */
  ADD_PRIMITIVE_IN_MODULE(make_mean, module);

  /* Export all primitives included so far: */
  STk_export_all_symbols(module);

  /* Now, the primitives included after the call to
     STk_export_all_symbols will not be exported: */

  ADD_PRIMITIVE_IN_MODULE(mean, module);

  /* Execute Scheme code that was included as "a-incl.c",
     generated from A.stk */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

/*
  DEFINE_MODULE_INFO will generate the function
  STk_module_info, which is used when dynamically
  loading the module.
*/
DEFINE_MODULE_INFO
