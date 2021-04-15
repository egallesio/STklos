/*
 *
 * e x t e n d . c          -- All the stuff dealing with
 *                             extended types
 *
 * Copyright © 1995-2021 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *           Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: 15-Mar-1995 11:31
 * Last file update: 15-Apr-2021 19:15 (eg)
 */

#include "stklos.h"

/*
 * The array of extended type descriptors
 */
struct extended_type_descr *STk_xtypes[MAX_CELL_TYPES] = {NULL};
static int user_extended_type = tc_last_standard;


static void error_bad_symbol(SCM o) {  STk_error("bad symbol ~S", o); }
static void error_bad_user_type(SCM o) { STk_error("bad user type name ~S", o); }
static void error_bad_key(SCM o) { STk_error("bad procedure key name ~S", o); }


int STk_new_user_type(struct extended_type_descr *descr)
{
  MUT_DECL(lck);

  MUT_LOCK(lck);
  user_extended_type += 1;
  STk_xtypes[user_extended_type]= descr;
  MUT_UNLOCK(lck);

  return user_extended_type;
}


SCM STk_extended_eqv(SCM o1, SCM o2)
{
  /* Assert: o1 and o2 have the same type */
  struct extended_type_descr *descr = BOXED_XTYPE(o1);
  return XTYPE_EQV(descr) ? (XTYPE_EQV(descr) (o1, o2)) : STk_false;
}

SCM STk_extended_equal(SCM o1, SCM o2)
{
  /* Assert: o1 and o2 have the same type */
  struct extended_type_descr *descr = BOXED_XTYPE(o1);
  return XTYPE_EQUAL(descr) ? (XTYPE_EQUAL(descr) (o1, o2)) : STk_false;
}

SCM STk_extended_class_of(SCM o)
{
  return XTYPE_CLASS_OF(BOXED_XTYPE(o));
}

// ----------------------------------------------------------------------
//
// Scheme aceess to the extended type descriptor
//
// ----------------------------------------------------------------------
static struct extended_type_descr *search_descriptor(char *str) {
  for (int i = tc_last_standard+1; i <= user_extended_type; i++) {
    if (strcmp(str, (STk_xtypes[i])->name) == 0) return STk_xtypes[i];
  }
  return NULL;
}


DEFINE_PRIMITIVE("%user-type-name", user_type_name, subr1, (SCM o))
{
   return (HAS_EXTENDED_TYPEP(o))?
     STk_intern(XTYPE_NAME(BOXED_XTYPE(o))):
     STk_false;
}


DEFINE_PRIMITIVE("%user-type-proc", user_type_proc, subr2, (SCM name, SCM key))
{
  struct extended_type_descr *descr;

  if (!SYMBOLP(name)) error_bad_symbol(name);
  if (!SYMBOLP(key)) error_bad_symbol(key);

  descr = search_descriptor(SYMBOL_PNAME(name));
  if (!descr) error_bad_user_type(name);

  if (strcmp(SYMBOL_PNAME(key), "describe") == 0) {
    SCM result = XTYPE_DESCRIBE(descr);
    return (result) ? result: STk_false;
  }
  if (strcmp(SYMBOL_PNAME(key), "class-of") == 0) {
    SCM result = XTYPE_CLASS_OF(descr);
    return (result) ? result: STk_false;
  }

  error_bad_key(key);
  return STk_void;  /* for the compiler */
}


DEFINE_PRIMITIVE("%user-type-proc-set!", user_type_proc_set, subr3,
                 (SCM name, SCM key, SCM proc))
{
  struct extended_type_descr *descr;

  if (!SYMBOLP(name)) error_bad_symbol(name);
  if (!SYMBOLP(key)) error_bad_symbol(key);

  descr = search_descriptor(SYMBOL_PNAME(name));
  if (!descr) error_bad_user_type(name);

  if (strcmp(SYMBOL_PNAME(key), "describe") == 0) {
    XTYPE_DESCRIBE(descr) = proc;
    return STk_void;
  }
  if (strcmp(SYMBOL_PNAME(key), "class-of") == 0) {
    XTYPE_CLASS_OF(descr) = proc;
    return STk_void;
  }
  error_bad_key(key);
  return STk_void;  /* for the compiler */
}


int STk_init_extend(void)
{
  ADD_PRIMITIVE(user_type_name);
  ADD_PRIMITIVE(user_type_proc);
  ADD_PRIMITIVE(user_type_proc_set);

  return TRUE;
}
