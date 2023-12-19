/*
 * charset.c   -- Implementation of (scheme charset)
 *
 * Copyright © 2023 Erick Gallesio <eg@stklos.net>
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
  *           Author: Erick Gallesio [eg@stklos.net]
 *    Creation date: 14-Dec-2023 00:21
 */

#include <stklos.h>

#define BASE_CHAR_SET    // must be defined to grab the "all_table"
#include "../../src/utf8-tables.inc"
#include "charset-incl.c"


/*
 * Define primitives to access UTF8 characters tables. Tables are used here,
 * (rather than char.c, because some tables are big (all_table for instance is
 * a ~200Kb static table which only serves to define the char-set:all table of
 * (scheme charset). Hence, specific charset tables are only loaded when using
 * charsets (even if we don't use UTF8 at runtime :-<, which should be rare
 * today).
 */

static inline SCM make_char_list(utf8_char *tab, int len)
{
  SCM lst = STk_nil;

  for (int i = len-1; i >=0; i--) {
    lst = STk_cons(MAKE_CHARACTER(tab[i]), lst);
  }
  return lst;
}


DEFINE_PRIMITIVE("%all-list", all_list, subr0, (void))
{
  return make_char_list(all_table, all_table_length);
}

DEFINE_PRIMITIVE("%punctuations-list", punctuations_list, subr0, (void))
{
  return make_char_list(puncts_table, puncts_table_length);
}

DEFINE_PRIMITIVE("%symbols-list", symbols_list, subr0, (void))
{
  return make_char_list(symbols_table, symbols_table_length);
}

DEFINE_PRIMITIVE("%title-case-list", title_case_list, subr0, (void))
{
  return make_char_list(titlecase_table, titlecase_table_length);
}

DEFINE_PRIMITIVE("%blanks-list", blanks_list, subr0, (void))
{
  return make_char_list(blanks_table, blanks_table_length);
}

DEFINE_PRIMITIVE("%valid-char-code?", valid_char_code, subr1, (SCM val))
{
  int c;

  if (!INTP(val)) STk_error("bad character code value ~S", val);
  c = INT_VAL(val);

  return MAKE_BOOLEAN((STk_use_utf8)?
                      STk_valid_utf8_char_codep(c, all_table, all_table_length):
                      (c >= 0 && c < 256));
}


MODULE_ENTRY_START("scheme/charset")
{
  SCM module = STk_create_module(STk_intern("scheme/charset"));

  ADD_PRIMITIVE_IN_MODULE(all_list, module);
  ADD_PRIMITIVE_IN_MODULE(punctuations_list, module);
  ADD_PRIMITIVE_IN_MODULE(symbols_list, module);
  ADD_PRIMITIVE_IN_MODULE(title_case_list, module);
  ADD_PRIMITIVE_IN_MODULE(blanks_list, module);

  ADD_PRIMITIVE_IN_MODULE(valid_char_code, module);

  /* Export all the symbols we have just defined */
  STk_export_all_symbols(module);
  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
