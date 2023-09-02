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
 *          Authors: Lassi Kortela & Erick Gallesio
 *    Creation date: 22-Jan-2023 09:36
 */

#include "stklos.h"
#include "238-incl.c"
#include <string.h>


/*
 * Notes on the implementation:
 *
 * - This implementation is based on a Lassi Kortela Pull Request.
 *
 * - Codes are sorted in numerical order internally, and returned in
 *   numerical order by `codeset-symbols`. Documentation tends to list
 *   them in this order so it is likely to be intuitive to users.
 *
 * - All messages of a system codeset are fetched the first time the user
 *   requests any message. Messages tend to come from APIs that are not
 *   thread-safe. Locking is simpler and faster if we fetch everything at
 *   once.
 *
 * - The messages are ket in an A-list,it could be changed to a hash-table
 *   if necessary.
 *
 * - A codeset can be defined in Scheme with the make-codeset primitive
 *
 */

MUT_DECL(srfi238_mutex);
static int tc_codeset;
static SCM all_codesets= STk_nil;
EXTERN_PRIMITIVE("sort", sort, subr2, (SCM obj, SCM test));


//
// New Scheme type codeset
//
struct codeset_obj {
  stk_header header;
  SCM name;
  SCM symbols;
  SCM messages;   // NULL if the cache is not built yet
  char *(*get_message)(int);
};

#define CODESETP(p)           (BOXED_TYPE_EQ((p), tc_codeset))
#define CODESET_NAME(p)       (((struct codeset_obj *) (p))->name)
#define CODESET_SYMBOLS(p)    (((struct codeset_obj *) (p))->symbols)
#define CODESET_MESSAGES(p)   (((struct codeset_obj *) (p))->messages)
#define CODESET_GET_MSG(p)    (((struct codeset_obj *) (p))->get_message)

static void print_codeset(SCM codeset, SCM port, int _UNUSED(mode))
{
  STk_fprintf(port, "#[codeset-object %s]", SYMBOL_PNAME(CODESET_NAME(codeset)));
}

static struct extended_type_descr xtype_codeset = {
  .name = "codeset-object",
  .print = print_codeset
};

/* ======================================================================
 *
 * Utilities
 *
 * ======================================================================
 */
static void error_bad_codeset(SCM obj) {
  STk_error("bad codeset ~S", obj);
}

static void register_codeset(SCM cs)
{
  MUT_LOCK(srfi238_mutex);
  all_codesets = STk_cons(STk_cons(CODESET_NAME(cs), cs), all_codesets);
  MUT_UNLOCK(srfi238_mutex);
}


// Build a codeset, given a correspondence table and a function to access
// the messages in C. This is used to build the errno and signal codesets.
//
// Since the C functions strerror and strsignal are not thread safe, we cache
// the messages in the codeset_object. Note that even if a reentrant version of
// strerror exists (strerror_r), this is not the case for strsignal. Furthermore,
// these function incur a non negligible time penalty.
// NOTE: the cache is built during the first access

static void make_C_codeset(SCM name, struct codeset_code *table,
                           char* (*get_message)(int), SCM comparator)
{
  SCM slist = STk_nil;   /* list of symbols */

  /* Build the symbol list */
  for (struct codeset_code *p=table; p->name; p++) {
    slist = STk_cons(STk_cons(MAKE_INT(p->code), STk_intern((char *) p->name)),
                     slist);
  }

  /* Build a codeset object and register it*/
  SCM z;
  NEWCELL(z, codeset);
  CODESET_NAME(z)     = name;
  CODESET_SYMBOLS(z)  = STk_sort(slist, comparator); // sort the list
  CODESET_MESSAGES(z) = NULL; // NULL: cache is not populated yet
  CODESET_GET_MSG(z)  = get_message;
  register_codeset(z);
}


/* ======================================================================
 *
 * Primitives
 *
 * ======================================================================
 */

/*
<doc EXT codeset-list
 * (codeset-list)
 *
 * Retuns a list of known codeset names.
 *
 * @lisp
 * (codeset-list) => (errno signal)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("codeset-list", codeset_list,
                 subr0, (void))
{
  SCM res = STk_nil;

  MUT_LOCK(srfi238_mutex);
  for (SCM l = all_codesets; !NULLP(l); l = CDR(l))
    res = STk_cons(CAR(CAR(l)), res);
  MUT_UNLOCK(srfi238_mutex);

  return res;
}


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

    if (res == STk_false) error_bad_codeset(obj);
    return CDR(res);
  }
}


DEFINE_PRIMITIVE("%codeset-symbols", codeset_symbols, subr1, (SCM cs))
{
  if (!CODESETP(cs)) error_bad_codeset(cs); // already verified in Scheme normally
  return CODESET_SYMBOLS(cs);
}


DEFINE_PRIMITIVE("%codeset-message", codeset_message, subr2, (SCM cs, SCM number))
{
  if (!CODESETP(cs)) STk_error("bad codeset ~s", cs); // already verified in Scheme
  if (!INTP(number)) STk_error("bad number ~s", cs);  // already verified in Scheme

  if (!CODESET_MESSAGES(cs)) {    // Build a cache of all messages at once.
    SCM mlist = STk_nil;

    MUT_LOCK(srfi238_mutex);
    for (SCM l = CODESET_SYMBOLS(cs); !NULLP(l); l = CDR(l)) {
      SCM code = CAR(CAR(l));

      if (STk_assq(code, mlist) == STk_false) {
      // Code isn't a duplicate.  Add it to mlist
        char *msg = CODESET_GET_MSG(cs)(INT_VAL(code));
        mlist = STk_cons(STk_cons(code, STk_Cstring2string(msg)), mlist);
      }
    }
    MUT_UNLOCK(srfi238_mutex);
    CODESET_MESSAGES(cs) = mlist;
  }

  SCM res = STk_assq(number, CODESET_MESSAGES(cs));
  return res == STk_false? res : CDR(res);
}


DEFINE_PRIMITIVE("%make-user-codeset", make_user_codeset, subr3,
                 (SCM name, SCM codes, SCM messages))
{
  SCM z;

  NEWCELL(z, codeset);
  CODESET_NAME(z) = name;
  CODESET_SYMBOLS(z)  = codes;
  CODESET_MESSAGES(z) = messages;
  CODESET_GET_MSG(z)  = NULL;
  register_codeset(z);

  return z;
}

static char *strsig(int sig)
{
  char *message = strsignal(sig);

#ifdef DARWIN
  if (message) {
    char *tail;
    if ((tail = strchr(message, ':'))) {
      /* MacOS repeats signal number in message, e.g. "Interrupt: 2" */
      *tail = '\0';
    }
  }
#endif
  return message;
}

DEFINE_PRIMITIVE("%create-system-codesets!", create_sys_codesets, subr1, (SCM comparator))
{
  make_C_codeset(STk_intern("errno"),  STk_errno_names,  strerror,  comparator);
  make_C_codeset(STk_intern("signal"), STk_signal_names, strsig,    comparator);
  return STk_void;
}



// ======================================================================
MODULE_ENTRY_START("srfi/238")
{
  SCM module =  STk_create_module(STk_intern("srfi/238"));

  tc_codeset = STk_new_user_type(&xtype_codeset);

  ADD_PRIMITIVE_IN_MODULE(codeset_list, module);
  ADD_PRIMITIVE_IN_MODULE(codesetp, module);
  ADD_PRIMITIVE_IN_MODULE(find_codeset, module);
  ADD_PRIMITIVE_IN_MODULE(codeset_symbols,module);
  ADD_PRIMITIVE_IN_MODULE(codeset_message,module);
  ADD_PRIMITIVE_IN_MODULE(make_user_codeset, module);
  ADD_PRIMITIVE_IN_MODULE(create_sys_codesets, module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
