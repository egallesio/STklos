/*
 *
 * e n v . c			-- Environment management
 *
 * Copyright © 1993-2007 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 * Last file update: 11-Apr-2007 18:03 (eg)
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
 * 				M O D U L E S 
 * 
\*===========================================================================*/
struct module_obj {
  stk_header header;
  SCM name;			/* module name */
  SCM exported_symbols;		/* symbols declared as exported */
  SCM imports;			/* imported modules */
  struct hash_table_obj hash;	/* The associated hash table */	
};


#define MODULE_NAME(m)		(((struct module_obj *) (m))->name)
#define MODULE_EXPORTS(m)	(((struct module_obj *) (m))->exported_symbols)
#define MODULE_IMPORTS(m)	(((struct module_obj *) (m))->imports)
#define MODULE_HASH_TABLE(m)	(((struct module_obj *) (m))->hash)

#define VISIBLE_P(symb, mod)	(((mod) == STk_STklos_module) ||	\
				 (STk_memq((symb), MODULE_EXPORTS(mod))!=STk_false))


SCM STk_STklos_module;		/* The module whose name is STklos */ 
static SCM all_modules;		/* List of all knowm modules */		

static void print_module(SCM module, SCM port, int mode)
{
  STk_nputs(port, "#[module ", 9);
  STk_print(MODULE_NAME(module), port, mode);
  STk_putc(']', port);
}


static SCM STk_makemodule(SCM name)
{
  register SCM z;
  
  NEWCELL(z, module);
  MODULE_NAME(z)	= name;
  MODULE_EXPORTS(z)	= STk_nil;
  MODULE_IMPORTS(z)	= (name == STk_void)? STk_nil : LIST1(STk_STklos_module);
  /* Initialize the associated hash table & store the module in the global list*/
  STk_hashtable_init(&MODULE_HASH_TABLE(z), HASH_VAR_FLAG);
  {
    MUT_DECL(lck);
    MUT_LOCK(lck);
    all_modules = STk_cons(z, all_modules);
    MUT_UNLOCK(lck);
  }
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

/* ==== Undocumented primitives ==== */

DEFINE_PRIMITIVE("%create-module", create_module, subr1, (SCM name))
{
  if (!SYMBOLP(name)) error_bad_module_name(name);
  return find_module(name, TRUE);
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

  if (CONSP(imported)) 
    MODULE_IMPORTS(importer) = STk_dappend2(imported, LIST1(STk_STklos_module));
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
 * Returns |¤t| if |object| is a module and |¤f| otherwise.
 * @lisp
 * (module? (find-module 'ST\klos))  => #t
 * (module? 'ST\klos)                => #f
 * (module? 123 'no)                => no
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("module?", modulep, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(MODULEP(obj));
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
 * Returns the list of modules that |module| imports.
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
    		STk_hash_keys(&MODULE_HASH_TABLE(module)) :
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
  return STk_copy_tree(all_modules);
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
    return BOX_VALUE(CDR(res));
  } else {
    if (!default_value) error_unbound_variable(symbol);
    return default_value;
  }
}


DEFINE_PRIMITIVE("%redefine-module-exports", redefine_module_exports, subr12,
		 (SCM from, SCM to))
{
  SCM lst, res;
  int i;

  if (!MODULEP(from)) error_bad_module(from);
  if (!to)
    to =  STk_current_module();
  else 
    if (!MODULEP(to)) error_bad_module(to);

  /* Compute the list of exported symbols */
  if (from == STk_STklos_module)
    lst = STk_hash_keys(&MODULE_HASH_TABLE(STk_STklos_module)); /* everybody */
  else 
    lst = MODULE_EXPORTS(from);				/* explicitly exported */

  for (     ; !NULLP(lst); lst = CDR(lst)) {
    res = STk_hash_get_variable(&MODULE_HASH_TABLE(from), CAR(lst), &i);
    if (res)
      /* symbol (car lst) is bound in module from. redefine it in module to */
      STk_define_variable(CAR(lst), BOX_VALUE(CDR(res)), to);
  }
  return STk_void;
}
  
/*===========================================================================*\
 * 
 * 		E n v i r o n m e n t   M a n a g e m e n t
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
  STk_hash_set_variable(&MODULE_HASH_TABLE(module), symbol, value);
}


DEFINE_PRIMITIVE("%symbol-define", symbol_define, subr3, 
		 (SCM symbol, SCM value, SCM module))
{
  if (!SYMBOLP(symbol)) error_bad_symbol(symbol);
  if (!MODULEP(module)) error_bad_module(module);

  STk_define_variable(symbol, value, module);
  return value;
}

DEFINE_PRIMITIVE("%symbol-alias", symbol_alias, subr23, 
		 (SCM new, SCM old, SCM module))
{
  SCM res, mod = STk_current_module();;
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

  STk_hash_set_alias(&MODULE_HASH_TABLE(mod), new, CDR(res));
  return STk_void;
}


/*===========================================================================*\
 * 				    Lookup
\*===========================================================================*/

SCM STk_lookup(SCM symbol, SCM env, SCM *ref, int err_if_unbound)
{
  int i;
  SCM l, module, res;

  while (FRAMEP(env)) env = FRAME_NEXT(env);

  res = STk_hash_get_variable(&MODULE_HASH_TABLE(env), symbol, &i);
  if (res) {
    *ref = res;
    return BOX_VALUE(CDR(res));
  }
  else {
    /* symbol was not found in the given env module. Try to find it in
     * the  exported symbols of its imported modules. 
     */
    for (l = MODULE_IMPORTS(env)  ; !NULLP(l); l = CDR(l)) {
      module = CAR(l);
      res    = STk_hash_get_variable(&MODULE_HASH_TABLE(module), symbol, &i);
      if (res && VISIBLE_P(symbol, module)) {
	*ref = res;
	return BOX_VALUE(CDR(res));
      }
    }
    
    /* It definitively does not exists  :-< */
    if (err_if_unbound) 
      error_unbound_variable(symbol);
    return STk_void;
  }
}


/*===========================================================================*\
 * 
 * 		E n v i r o n m e n t   P r i m i t i v e s
 * 
\*===========================================================================*/

/* The stucture which describes the modules type */
static struct extended_type_descr xtype_module = {
  "module",			/* name */
  print_module			/* print function */
};




/* The stucture which describes the frame type */
static struct extended_type_descr xtype_frame = { 
  "frame",			/* name */
  NULL				/* print function */
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

  /* ==== Undocumented primitives ==== */
  ADD_PRIMITIVE(create_module);
  ADD_PRIMITIVE(select_module);
  ADD_PRIMITIVE(module_imports_set);
  ADD_PRIMITIVE(module_exports_set);
  ADD_PRIMITIVE(redefine_module_exports);

  /* ==== User primitives ==== */
  ADD_PRIMITIVE(modulep);
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
  

  return TRUE;
}
