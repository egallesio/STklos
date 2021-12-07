/*
 *
 * e n v . c                    -- Environment management
 *
 * Copyright © 1993-2021 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *           Author: Erick Gallesio [eg@unice.fr]
 *    Creation date: 23-Oct-1993 21:37
 * Last file update:  8-Nov-2021 18:27 (eg)
 */

#include "stklos.h"
#include "hash.h"
#include "vm.h"
#include "thread-common.h"

static void error_bad_module_name(SCM obj)
{
  STk_error("bad module name ~S", obj);
}

static void error_bad_module(SCM obj)
{
  STk_error("bad module ~S", obj);
}

static void error_not_exist(SCM obj)
{
  STk_error("module with name ~S does not exist", obj);
}

static void error_bad_list(SCM obj)
{
  STk_error("bad list ~S", obj);
}

static void error_unbound_variable(SCM symbol)
{
  STk_error("variable ~S unbound", symbol);
}

static void error_bad_symbol(SCM symbol)
{
  STk_error("bad symbol ~S", symbol);
}


/*===========================================================================*\
 *
 *                              M O D U L E S
 *
\*===========================================================================*/
struct module_obj {
  stk_header header;
  SCM name;                     /* module name */
  SCM exported_symbols;         /* symbols declared as exported */
  SCM imports;                  /* imported modules */
  int is_library;               /* 1 if module is a R7RS library; 0 otherwise */
  struct hash_table_obj hash;   /* The associated hash table */
};


#define MODULE_NAME(m)          (((struct module_obj *) (m))->name)
#define MODULE_EXPORTS(m)       (((struct module_obj *) (m))->exported_symbols)
#define MODULE_IMPORTS(m)       (((struct module_obj *) (m))->imports)
#define MODULE_IS_LIBRARY(m)    (((struct module_obj *) (m))->is_library)
#define MODULE_HASH_TABLE(m)    (((struct module_obj *) (m))->hash)

SCM STk_STklos_module;          /* The module whose name is STklos */
static SCM Scheme_module;       /* The module whose name is SCHEME */
static SCM all_modules;         /* List of all knowm modules */

static void print_module(SCM module, SCM port, int mode)
{
  STk_nputs(port, "#[module ", 9);
  STk_print(MODULE_NAME(module), port, mode);
  STk_putc(']', port);
}

static SCM make_simple_module(SCM name)
{
  register SCM z;

  NEWCELL(z, module);
  MODULE_NAME(z)        = name;
  MODULE_EXPORTS(z)     = STk_nil;
  MODULE_IMPORTS(z)     = STk_nil;
  MODULE_IS_LIBRARY(z)  = 0;
  STk_hashtable_init(&MODULE_HASH_TABLE(z), HASH_VAR_FLAG);
  return z;
}

static void register_module(SCM mod)
{
  MUT_DECL(lck);
  MUT_LOCK(lck);
  all_modules = STk_cons(mod, all_modules);
  MUT_UNLOCK(lck);
}

static SCM STk_makemodule(SCM name)
{
  register SCM z;

  NEWCELL(z, module);
  MODULE_NAME(z)        = name;
  MODULE_EXPORTS(z)     = STk_nil;
  MODULE_IMPORTS(z)     = (name == STk_void)? STk_nil : LIST1(STk_STklos_module);
  /* Initialize the associated hash table & store the module in the global list*/
  STk_hashtable_init(&MODULE_HASH_TABLE(z), HASH_VAR_FLAG);
  register_module(z);

  return z;
}


static SCM find_module(SCM name, int create)
{
  SCM tmp;

  if (name == STk_intern("STklos") || name == STk_intern("stklos"))
    return STk_STklos_module;

  for (tmp = all_modules; !NULLP(tmp); tmp = CDR(tmp)) {
    if (MODULE_NAME(CAR(tmp)) == name)
      return CAR(tmp);
  }
  /* module does not exists */
  return (create) ? STk_makemodule(name) : STk_void;
}

/*===========================================================================*\
 *
 * Module primitives
 *
\*===========================================================================*/

static SCM make_export_list(SCM symbols)
{ /* Transform list of symbols (s1 s2 ...) in ((s1 . s1) (s2 . s2) ...) */
  SCM res = STk_nil;

  for (SCM l=symbols; l != STk_nil; l=CDR(l)) {
    res = STk_cons(STk_cons(CAR(l), CAR(l)), res);
  }
  return res;
}


void STk_export_all_symbols(SCM module)
{
  if (!MODULEP(module)) error_bad_module(module);
  if (module != STk_STklos_module)
    MODULE_EXPORTS(module) = make_export_list(STk_hash_keys(&MODULE_HASH_TABLE(module)));
}

/* ==== Undocumented primitives ==== */

DEFINE_PRIMITIVE("%create-module", create_module, subr1, (SCM name))
{
  if (!SYMBOLP(name)) error_bad_module_name(name);
  return find_module(name, TRUE);
}

 DEFINE_PRIMITIVE("%make-library", make_library, subr0, (void))
{
  SCM z = make_simple_module(STk_void);

  MODULE_IS_LIBRARY(z) = 1;
  return z;
}

DEFINE_PRIMITIVE("%register-library-as-module", register_library,
                 subr2, (SCM m, SCM name))
{
  if (!MODULEP(m))    error_bad_module(m);
  if (!SYMBOLP(name)) error_bad_symbol(name);
  MODULE_NAME(m) = name;
  register_module(m);
  return STk_void;;
}




DEFINE_PRIMITIVE("%select-module", select_module, subr1, (SCM module))
{
  vm_thread_t *vm = STk_get_current_vm();

  if (!MODULEP(module)) error_bad_module(module);
  vm->current_module= module;
  return STk_void;
}

DEFINE_PRIMITIVE("%module-imports-set!", module_imports_set, subr2,
                 (SCM importer,SCM imported))
{
  if (!MODULEP(importer)) error_bad_module(importer);

  if (CONSP(imported)) {
    /* STklos modules implicitely import STklos, but R7RS library don't */
    if (MODULE_IS_LIBRARY(importer)) {
      MODULE_IMPORTS(importer) = imported;
    }
    else
      MODULE_IMPORTS(importer) = STk_dappend2(imported, LIST1(STk_STklos_module));
  }
  else if (NULLP(imported))
    MODULE_IMPORTS(importer) = STk_nil;
  else error_bad_list(imported);

  return STk_void;
}

DEFINE_PRIMITIVE("%module-exports-set!", module_exports_set, subr2,
                 (SCM exporter,SCM exported))
{
  if (!MODULEP(exporter)) error_bad_module(exporter);
  if (!CONSP(exported))   error_bad_list(exported);

  MODULE_EXPORTS(exporter) = exported;
  return STk_void;
}


/*
<doc EXT module?
 * (module? object)
 *
 * Returns |#t| if |object| is a module and |#f| otherwise.
 * @lisp
 * (module? (find-module 'ST\klos))  => #t
 * (module? 'ST\klos)                => #f
 * (module? 123 'no)                 => no
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("module?", modulep, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(MODULEP(obj));
}

/*
<doc EXT library?
 * (library? object)
 *
 * Returns |#t| if |object| is a module defined as a R7RS library and |#f| otherwise.
 * @lisp
 * (define-module a)
 * (define-library b)
 * 
 * (module? (find-module 'a))   => #t
 * (module? (find-module 'b))   => #t
 * (library? (find-module 'a))  => #f
 * (library? (find-module 'b))  => #t
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("library?", libraryp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(MODULEP(obj) && MODULE_IS_LIBRARY(obj));
}


/*
<doc EXT find-module
 * (find-module name)
 * (find-module name default)
 *
 * STklos modules are first class objects and |find-module| returns the
 * module associated to |name| if it exists. If there is no module
 * associated to |name|, an error is signaled if no |default| is
 * provided, otherwise |find-module| returns |default|.
doc>
*/
DEFINE_PRIMITIVE("find-module", scheme_find_module, subr12, (SCM name, SCM def))
{
  SCM module;

  if (!SYMBOLP(name)) error_bad_module_name(name);

  module = find_module(name, FALSE);
  if (module == STk_void) {
    if (!def) error_not_exist(name);
    return def;
  }
  return module;
}

/*
<doc EXT current-module
 * (current-module)
 *
 * Returns the current module.
 * @lisp
 * (define-module M
 *   (display
 *       (cons (eq? (current-module) (find-module 'M))
 *             (eq? (current-module) (find-module 'STklos)))))
 *    @print{} (#t . #f)
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("current-module", current_module, subr0, (void))
{
  if (STk_primordial_thread) {
    vm_thread_t *vm = STk_get_current_vm();
    return vm->current_module;
  } else {
    return STk_STklos_module;
  }
}


/*
<doc EXT module-name
 * (module-name module)
 *
 * Returns the name (a symbol) associated to a |module|.
doc>
 */
DEFINE_PRIMITIVE("module-name", module_name, subr1, (SCM module))
{
  if (!MODULEP(module)) error_bad_module(module);
  return MODULE_NAME(module);
}


/*
<doc EXT module-imports
 * (module-imports module)
 *
 * Returns the list of modules that |module| (fully) imports.
doc>
 */
DEFINE_PRIMITIVE("module-imports", module_imports, subr1, (SCM module))
{
  if (!MODULEP(module)) error_bad_module(module);
  return MODULE_IMPORTS(module);
}


/*
<doc EXT module-exports
 * (module-exports module)
 *
 * Returns the list of symbols exported by |module|. Note that this function
 * returns the list of symbols given in the module |export| clause and that
 * some of these symbols can be not yet defined.
doc>
 */
DEFINE_PRIMITIVE("module-exports", module_exports, subr1, (SCM module))
{
  if (!MODULEP(module)) error_bad_module(module);

  /* STklos module is special: everything is exported ==> module-symbols */
  return (module == STk_STklos_module) ?
                make_export_list(STk_hash_keys(&MODULE_HASH_TABLE(module))) :
                MODULE_EXPORTS(module);
}


/*
<doc EXT module-symbols
 * (module-symbols module)
 *
 * Returns the list of symbols already defined in |module|.
doc>
 */
DEFINE_PRIMITIVE("module-symbols", module_symbols, subr1,  (SCM module))
{
  if (!MODULEP(module)) error_bad_module(module);
  return STk_hash_keys(&MODULE_HASH_TABLE(module));
}

/*
<doc EXT all-modules
 * (all-modules)
 *
 * Returns the list of all the living modules.
doc>
 */
DEFINE_PRIMITIVE("all-modules", all_modules, subr0, (void))
{
  return STk_list_copy(all_modules);
}


/*
<doc EXT symbol-value
 * (symbol-value symbol module)
 * (symbol-value symbol module default)
 *
 * Returns the value bound to |symbol| in |module|. If |symbol| is not bound,
 * an error is signaled if no |default| is provided, otherwise |symbol-value|
 * returns |default|.
doc>
 */
DEFINE_PRIMITIVE("symbol-value", symbol_value, subr23,
                 (SCM symbol, SCM module, SCM default_value))
{
  SCM res;
  int i;

  if (!SYMBOLP(symbol)) error_bad_symbol(symbol);
  if (!MODULEP(module)) error_bad_module(module);

  res = STk_hash_get_variable(&MODULE_HASH_TABLE(module), symbol, &i);
  if (res) {
    return *BOX_VALUES(CDR(res));    /* sure that this box arity is 1 */
  } else {
    if (!default_value) error_unbound_variable(symbol);
    return default_value;
  }
}


DEFINE_PRIMITIVE("%populate-scheme-module", populate_scheme_module, subr0, (void))
{
  // This function is called to populate the Scheme module with all the
  // symbols defined in STklos module. This permits to have a copy of the
  // original bindings that can reliably be used by the runtime.
  // FIXME: the bindings in the Scheme module should be immutable. 
  for (SCM lst = STk_hash_keys(&MODULE_HASH_TABLE(STk_STklos_module));
       !NULLP(lst);
       lst = CDR(lst)) {
    int i;
    SCM res = STk_hash_get_variable(&MODULE_HASH_TABLE(STk_STklos_module),
                                    CAR(lst),
                                    &i);
    /* Redefine symbol in (car lst) in SCHEME module */
    STk_define_variable(CAR(lst), *BOX_VALUES(CDR(res)), Scheme_module);
  }
  return STk_void;
}


/*===========================================================================*\
 *
 *              E n v i r o n m e n t   M a n a g e m e n t
 *
\*===========================================================================*/

#define FRAME_ALLOC_BYTES(len) (sizeof(struct frame_obj) + ((len)-1)*sizeof(SCM))

SCM STk_make_frame(int len)
{
  SCM z;

  NEWCELL_WITH_LEN(z, frame, FRAME_ALLOC_BYTES(len));
  FRAME_LENGTH(z) = len;
  return z;
}

SCM STk_clone_frame(SCM f)
{
  int len = FRAME_ALLOC_BYTES(FRAME_LENGTH(f));
  struct frame_obj *clone;

  clone = (struct frame_obj *) STk_must_malloc(len);
  memcpy(clone, (struct frame_obj *)f, len);
  return (SCM) clone;
}


void STk_define_variable(SCM symbol, SCM value, SCM module)
{
  STk_hash_set_variable(&MODULE_HASH_TABLE(module), symbol, value, TRUE);
}


DEFINE_PRIMITIVE("%symbol-define", symbol_define, subr23,
                 (SCM symbol, SCM value, SCM module))
{
  if (!SYMBOLP(symbol)) error_bad_symbol(symbol);
  if (module) {
    if (!MODULEP(module)) error_bad_module(module);
  }
  else
    module = STk_current_module();

  STk_define_variable(symbol, value, module);
  return value;
}

DEFINE_PRIMITIVE("%symbol-alias", symbol_alias, subr23,
                 (SCM new, SCM old, SCM module))
{
  SCM res, mod = STk_current_module();
  int i;

  if (!SYMBOLP(new)) error_bad_symbol(new);
  if (!SYMBOLP(old)) error_bad_symbol(old);
  if (!module)
    module = mod;
  else
    if (!MODULEP(module)) error_bad_module(module);

  res = STk_hash_get_variable(&MODULE_HASH_TABLE(module), old, &i);
  if (!res)
    error_unbound_variable(old);

  STk_hash_set_alias(&MODULE_HASH_TABLE(mod), new, CDR(res), 0);
  return STk_void;
}

DEFINE_PRIMITIVE("%symbol-link", symbol_link, subr4,
                 (SCM new, SCM old, SCM new_module, SCM old_module))
{
  SCM res;
  int i;

  if (!SYMBOLP(new)) error_bad_symbol(new);
  if (!SYMBOLP(old)) error_bad_symbol(old);
  if (!MODULEP(new_module)) error_bad_module(new_module);
  if (!MODULEP(old_module)) error_bad_module(old_module);

  res = STk_hash_get_variable(&MODULE_HASH_TABLE(old_module), old, &i);
  if (!res)
    error_unbound_variable(old);

  STk_hash_set_alias(&MODULE_HASH_TABLE(new_module), new, CDR(res), 1);
  return STk_void;
}


/*===========================================================================*\
 *                                  Lookup
\*===========================================================================*/

SCM STk_lookup(SCM symbol, SCM env, SCM *ref, int err_if_unbound)
{
  int i;
  SCM res;

  while (FRAMEP(env)) env = FRAME_NEXT(env);

  res = STk_hash_get_variable(&MODULE_HASH_TABLE(env), symbol, &i);
  if (res) {
    *ref = res;
    return *BOX_VALUES(CDR(res));
  }

  // symbol was not found in the given env module. Try to find it in
  // the the STklos modle (if this is not a R7RS library)
  if (!MODULE_IS_LIBRARY(env) &&  env != STk_STklos_module) {
    env = STk_STklos_module;
    res = STk_hash_get_variable(&MODULE_HASH_TABLE(env), symbol, &i);
    if (res) {
      *ref = res;
      return *BOX_VALUES(CDR(res));
    }
  }

  /* It definitively does not exists  :-< */
  if (err_if_unbound)
    error_unbound_variable(symbol);
  return STk_void;
}



/*===========================================================================*\
 *
 *              E n v i r o n m e n t   P r i m i t i v e s
 *
\*===========================================================================*/

/* The stucture which describes the modules type */
static struct extended_type_descr xtype_module = {
  .name  = "module",
  .print = print_module
};




/* The stucture which describes the frame type */
static struct extended_type_descr xtype_frame = {
  .name= "frame"                      /* name */
};


int STk_init_env(void)
{
  all_modules = STk_nil;

  /* Create the stklos module */
  STk_STklos_module  = STk_makemodule(STk_void); /* will be changed later */

  /* Declare the extended types module_obj and frame_obj */
  DEFINE_XTYPE(module, &xtype_module);
  DEFINE_XTYPE(frame,  &xtype_frame);
  return TRUE;
}

int STk_late_init_env(void)
{
  /* Now that symbols are initialized change the STklos module name */
  MODULE_NAME(STk_STklos_module) = STk_intern("stklos");
  MODULE_IMPORTS(STk_STklos_module) = LIST1(STk_STklos_module);

  /* Create the SCHEME module */
  Scheme_module = STk_makemodule(STk_intern("SCHEME"));

  /* ==== Undocumented primitives ==== */
  ADD_PRIMITIVE(make_library);
  ADD_PRIMITIVE(register_library);
  ADD_PRIMITIVE(create_module);
  ADD_PRIMITIVE(select_module);
  ADD_PRIMITIVE(module_imports_set);
  ADD_PRIMITIVE(module_exports_set);
  ADD_PRIMITIVE(populate_scheme_module);

  /* ==== User primitives ==== */
  ADD_PRIMITIVE(modulep);
  ADD_PRIMITIVE(libraryp);
  ADD_PRIMITIVE(scheme_find_module);
  ADD_PRIMITIVE(current_module);
  ADD_PRIMITIVE(module_name);
  ADD_PRIMITIVE(module_imports);
  ADD_PRIMITIVE(module_exports);
  ADD_PRIMITIVE(module_symbols);
  ADD_PRIMITIVE(all_modules);

  ADD_PRIMITIVE(symbol_value);
  ADD_PRIMITIVE(symbol_define);
  ADD_PRIMITIVE(symbol_alias);
  ADD_PRIMITIVE(symbol_link);

  return TRUE;
}
