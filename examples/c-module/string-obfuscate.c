/*
 * string-obfuscate.c  -- A simple module in C and Scheme
 *
 * Copyright © 2024 Jeronimo Pellegrini <j_p@aleph0.info>
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
 *    Creation date: 28-May-2024 19:12 (jpellegrini)
 */

/*
  This file is an example of a STklos module written part in C and
  part in Scheme.

  Below is a diagram showing how compilation of such a module is
  done.

  Suppose a module named "module" is written, partly in C and partly
  in Scheme.
  
  The two files written by the user are underlined with "~" signs:
  
  - module.c   which actually creates the module, and contains its
               C-written part.
  - module.stk which executes Scheme code inside the module, after it
               has been created -- it contains the Scheme-written
               part of the module.

  The files module-incl.c and module.o are intermediate files.

  The output file, containing the loadable module, is the shared
  object file, "module.so".
               
                 +----------------+
  module.stk --> | stklos-compile | --> module-incl.c
  ~~~~~~~~~~     +----------------+         |
                                            |
                                            v
                                     +--------------+
                                     | #included in |
                                     +--------------+
                                            |
                                            |
                      +------------+        v
         module.o <-- | C compiler | <-- module.c
            |         +------------+     ~~~~~~~~
            |
            v
      +------------+
      | C compiler | --> module.so
      +------------+

  Following is the C part of a module, with comments explaining what
  everything does.
 */

/*
  The "stklos.h" header contains the most fundamental
  of the C function definitions that are needed to
  write a STklos module.
*/
#include "stklos.h"

/*
  "string-obfuscate-incl.c" is exactly "string-obfuscate.stk",
  which was compiled into a C program containing the Scheme
  bytecode, and it is included here. It will be executed after
  module creation (see the end of this file).
 */
#include "string-obfuscate-incl.c"

/*
<doc EXT string-add
 * (string-add str n)
 *
 * Obfuscates the string |str| using the integer |n|. The
 * internal method is to add |n| to each byte.
 *
 * This is one of the procedures that can be passed to
 * |string-obfuscate-set!|.
doc>
*/
DEFINE_PRIMITIVE("string-add", obf_add, subr2, (SCM str, SCM n))
{
    if (!STRINGP(str)) STk_error("bad string ~S", str);
    if (!INTP(n)) STk_error("bad integer ~S", n);
    
    int len = STRING_SIZE(str); // size in bytes, not characters!

    SCM res = STk_makestring(len, NULL);
    
    for (int i=0; i<len; i++)
	STRING_CHARS(res)[i] = (INT_VAL(n) + STRING_CHARS(str)[i]);
    
    return res;
}

/*
<doc EXT string-xor
 * (string-xor str n)
 *
 * Obfuscates the string |str| using the integer |n|. The
 * internal method is to do an exclusive-or of each byte
 * with |n|.
 *
 * This is one of the procedures that can be passed to
 * |string-obfuscate-set!|.
doc>
*/
DEFINE_PRIMITIVE("string-xor", obf_xor, subr2, (SCM str, SCM n))
{
    if (!STRINGP(str)) STk_error("bad string ~S", str);
    if (!INTP(n)) STk_error("bad integer ~S", n);
    int len = STRING_SIZE(str); // size in bytes, not characters!

    SCM res = STk_makestring(len, NULL);
    
    for (int i=0; i<len; i++)
	STRING_CHARS(res)[i] = (INT_VAL(n) ^ STRING_CHARS(str)[i]);
    
    return res;
}

/* string-obfuscate-hidden does nothing special: it just returns
   a fixed string. It is here to illustrate how to include a
   primitive that is not exported. */
DEFINE_PRIMITIVE("string-obfuscate-hidden", obf_hidden, subr0, ())
{
    return STk_Cstring2string("Hello there from the hidden part of the module!");
}

/*
 * After defining C functions and STklos primitives, we need to create the module.
 * This is done below.
 */

/*
  The module configuration and initialization is done between the
  macros MODULE_ENTRY_START and MODULE_ENTRY_END.
  
  The first (MODULE_ENTRY_START) will start the definion of an
  initialization function, which is called when the module is
  loaded, and *** the code included afterwards, up to
  MODULE_ENTRY_END, will be included in this initialization
  function ***
 */
MODULE_ENTRY_START("string-obfuscate")
{
  /* We first actually create the module: */
  SCM module =  STk_create_module(STk_intern("string-obfuscate"));

  /* Include Scheme primitives in the module: */
  ADD_PRIMITIVE_IN_MODULE(obf_add, module);
  ADD_PRIMITIVE_IN_MODULE(obf_xor, module);

  /* Export all primitives included so far: */
  STk_export_all_symbols(module);

  /* Now, the primitives included after the call to
     STk_export_all_symbols will not be exported: */

  ADD_PRIMITIVE_IN_MODULE(obf_hidden, module);

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
