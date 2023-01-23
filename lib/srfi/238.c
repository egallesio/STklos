/*
 * 238.c   -- Support for SRFI-238 (Codesets)
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
 *          Authors: Erick Gallesio
 *    Creation date: 22-Jan-2023 09:36
 * Last file update: 23-Jan-2023 13:38 (eg)
 */

#include "stklos.h"
#include "238-incl.c"

static int tc_codeset;
static SCM all_codesets= STk_nil; //FIXME: Use a mutex to acess it

//
// New Scheme type codeset
// NOTE: codes and messages are represented by A-lists.
// FIXME? Use hash-tables?
//
struct codeset_obj {
  stk_header header;
  SCM name;
  SCM symbols;
  SCM messages; // Unused for errno and signal */
  char *(*get_message)(SCM, SCM);
};

#define CODESETP(p)           (BOXED_TYPE_EQ((p), tc_codeset))
#define CODESET_NAME(p)       (((struct codeset_obj *) (p))->name)
#define CODESET_SYMBOLS(p)    (((struct codeset_obj *) (p))->symbols)
#define CODESET_GET_MSG(p)    (((struct codeset_obj *) (p))->get_message)
#define CODESET_MESSAGES(p)   (((struct codeset_obj *) (p))->messages)

static void print_codeset(SCM codeset, SCM port, int _UNUSED(mode))
{
  STk_fprintf(port, "#[codeset-object %s]", SYMBOL_PNAME(CODESET_NAME(codeset)));
}

static struct extended_type_descr xtype_codeset = {
  .name = "codeset-object",
  .print = print_codeset
};

//
// Utilities
//
static void error_bad_codeset_element(SCM obj) {
  STk_error("bad codeset element ~S", obj);
}

static char* general_get_message(SCM cs, SCM n) {
  SCM res = STk_assq(n, CODESET_MESSAGES(cs));
  return res == STk_false? res : CDR(res);
}

static char* errno_get_message(SCM _UNUSED(cs), SCM n) {
  return STk_Cstring2string(strerror(INT_VAL(n)));
}

static char* signal_get_message(SCM _UNUSED(cs), SCM n) {
  return STk_Cstring2string(strsignal(INT_VAL(n)));
}

static void register_codeset(SCM cs)    // FIXME: MUTEX
{
  all_codesets = STk_cons(STk_cons(CODESET_NAME(cs), cs), all_codesets);
}


static void make_C_codeset(SCM name, struct codeset_code *table,
                           char* (*get_message)(SCM, SCM))
{
  SCM z;
  SCM alist= STk_nil;

  NEWCELL(z, codeset);
  CODESET_NAME(z)     = name;
  CODESET_SYMBOLS(z)  = STk_nil;
  CODESET_GET_MSG(z)  = get_message;

  /* populate the symbol table */
  for (struct codeset_code *p=table; p->name; p++) {
    alist = STk_cons(STk_cons(MAKE_INT(p->code), STk_intern((char *) p->name)),
                     alist);
  }
  CODESET_SYMBOLS(z) = alist;

  register_codeset(z);
}

// ======================================================================
DEFINE_PRIMITIVE("codeset?", codesetp, subr1, (SCM obj))
{
  return CODESETP(obj) ?
    STk_true:
    MAKE_BOOLEAN(STk_assq(obj, all_codesets) != STk_false);
}


DEFINE_PRIMITIVE("%find-codeset", find_codeset, subr1, (SCM obj))
{
  if (CODESETP(obj))
    return obj;
  else {
    SCM res = STk_assq(obj, all_codesets);

    if (res == STk_false)
      STk_error("bad codset ~s", obj);
    return CDR(res);
  }
}


DEFINE_PRIMITIVE("%codeset-symbols", codeset_symbols, subr1, (SCM cs))
{
  if (!CODESETP(cs)) STk_error("bad codeset ~s", cs); // already verified in Scheme
  return CODESET_SYMBOLS(cs);
}


DEFINE_PRIMITIVE("%codeset-messages", codeset_messages, subr1, (SCM cs))
{
  if (!CODESETP(cs)) STk_error("bad codeset ~s", cs); // already verified in Scheme
  return CODESET_MESSAGES(cs);
}

DEFINE_PRIMITIVE("%codeset-message", codeset_message, subr2, (SCM cs, SCM number))
{
  if (!CODESETP(cs)) STk_error("bad codeset ~s", cs); // already verified in Scheme
  if (!INTP(number)) STk_error("bad number ~s", cs);  // already verified in Scheme
  return CODESET_GET_MSG(cs)(cs, number);
}


DEFINE_PRIMITIVE("%make-user-codeset", make_user_codeset, subr3,
                 (SCM name, SCM codes, SCM messages))
{
  SCM z;

  NEWCELL(z, codeset);
  CODESET_NAME(z) = name;
  CODESET_SYMBOLS(z)  = codes;
  CODESET_MESSAGES(z) = messages;
  CODESET_GET_MSG(z)  = general_get_message;
  register_codeset(z);

  return z;
}

// ======================================================================
MODULE_ENTRY_START("srfi/238")
{
  SCM module =  STk_create_module(STk_intern("srfi/238"));

  tc_codeset = STk_new_user_type(&xtype_codeset);

  ADD_PRIMITIVE_IN_MODULE(codesetp, module);
  ADD_PRIMITIVE_IN_MODULE(find_codeset, module);
  ADD_PRIMITIVE_IN_MODULE(codeset_symbols,module);
  ADD_PRIMITIVE_IN_MODULE(codeset_messages,module);
  ADD_PRIMITIVE_IN_MODULE(codeset_message,module);

  ADD_PRIMITIVE_IN_MODULE(make_user_codeset, module);

  /* Create the errno & signal codesets */
  make_C_codeset(STk_intern("errno"),  STk_errno_names,  errno_get_message);
  make_C_codeset(STk_intern("signal"), STk_signal_names, signal_get_message);



  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
