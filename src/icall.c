/*
 * icall.c   -- Internal calls implementation
 *
 * Copyright © 2025 Erick Gallesio <eg@stklos.net>
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
 *    Creation date: 10-Aug-2025 16:09
 */

#include "stklos.h"
#include "hash.h"

/* Internal calls have the form #%xxx and are directly executed by the
 * function compile-%%xxx in the STklos compiler. For instance, the Scheme
 * `(if a b c)` is rewritten in `(%if a b c)` by the `if` macro (which can be
 * re-assigned later by the user). The `tc_icall` type is used for internal
 * calls.  It has two slots: the name of the icall and the function to call
 * in the compiler (a local cache). All the #%xxx icalls share the same
 * address, and the function pointer is set to `#f` when initialized (it will
 * be set! at the first use of the icall by the compiler).
 */

struct icall_obj {
  stk_header header;
  SCM name;
  SCM func;
};

#define ICALLP(p)         (BOXED_TYPE_EQ((p), tc_icall))
#define ICALL_NAME(p)     (((struct icall_obj *) (p))->name)
#define ICALL_FUNC(p)     (((struct icall_obj *) (p))->func)


static SCM icall_table;                  // table of all the internal calls


static void verify_internal_call(SCM obj)
{
  if (!ICALLP(obj)) STk_error("bad internal call ~S", obj);
}


SCM STk_get_icall(SCM name)   // get back an already existing internal call
{
  SCM z;

  if (!SYMBOLP(name)) STk_error("bad icall name ~S", name);
  z = STk_C_hash_get(icall_table, SYMBOL_PNAME(name));

  if (!z) {
    /* Build a new icall descriptor with a fake func field */
    NEWCELL(z, icall);
    ICALL_NAME(z) = name;
    ICALL_FUNC(z) = STk_false;

    /* Register the new icall in the global icall_table */
    STk_C_hash_set(icall_table, SYMBOL_PNAME(name), z);
  }
  return z;
}

DEFINE_PRIMITIVE("%icall?", icallp, subr1, (SCM x))
{
  return MAKE_BOOLEAN(ICALLP(x));
}


DEFINE_PRIMITIVE("%icall-name", icall_name, subr1, (SCM x))
{
  verify_internal_call(x);
  return ICALL_NAME(x);
}


DEFINE_PRIMITIVE("%icall-function", icall_func, subr1, (SCM x))
{
  verify_internal_call(x);
  return ICALL_FUNC(x);

}

DEFINE_PRIMITIVE("%icall-function-set!", icall_func_set, subr2, (SCM x, SCM v))
{
  verify_internal_call(x);
  if (STk_procedurep(v) == STk_false) STk_error("bad icall function ~S", v);
  ICALL_FUNC(x) = v;
  return STk_void;

}


// Define a new type for internal calls
static void print_icall(SCM icall, SCM port, int _UNUSED(mode))
{
  STk_puts("#%", port);
  STk_print(ICALL_NAME(icall), port, WRT_MODE);
}

static struct extended_type_descr xtype_icall = {
  .name = "icall",
  .print = print_icall
};


// ----------------------------------------------------------------------
int STk_init_icall(void)
{
  // Initialize  a table to always return a unique address for icall objetcts with
  // the same name.
  icall_table = STk_make_C_hash_table();;

  // Define a new type for internal calls
  DEFINE_XTYPE(icall, &xtype_icall);

  // Icall primitives
  ADD_PRIMITIVE(icallp);
  ADD_PRIMITIVE(icall_name);
  ADD_PRIMITIVE(icall_func);
  ADD_PRIMITIVE(icall_func_set);
  return 1;
}

/*  LocalWords:  icall icalls
 */
