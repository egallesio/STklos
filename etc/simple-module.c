/*
 * simple-module.c	-- A simple C module for STklos
 *
 * Copyright © 2000 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 22-Jul-2000 12:10 (eg)
 * Last file update: 22-Jul-2000 12:41 (eg)
 */

/* This module defines two simple primitive called "test" and "add".
 * "test" takes one parameter, displays it and returns it.
 * "add" return the sum of its two (small) integer parameters
 *
 * New primitives are defined with the DEFINE_PRIMITIVE macro
 * and module entry is defined with the MODULE_ENTRY_* macros
 *
 * To test this module on Linux: just do
 *
 *      $ gcc -I../gc -I../src -fPIC -c simple-module.c
 *      $ ld -shared simple-module.o -o simple-module.so
 *
 * File can be loaded from the interpreter by doing
 *
 *      stklos> (load "simple-module.so")
 *
 * (You may have to set you LD_LIBRARY_PATH variable to find the file,
 * or place the shared object in a directory present in *load-path*)
 *
 */

#include "stklos.h"


DEFINE_PRIMITIVE("test", tst, subr1, (SCM l))
{
  STk_fprintf(STk_curr_oport, "Parameter is ");
  STk_display(l, STk_curr_oport);
  STk_newline(STk_curr_oport);
  return l;
}

DEFINE_PRIMITIVE("add", add, subr2, (SCM a, SCM b))
{
  long sum;
  if (!INTP(a)) STk_error("First parameter ~S is not an integer", a);
  if (!INTP(b)) STk_error("Second parameter ~S is not an integer", b);

  sum = INT_VAL(a) + INT_VAL(b);
  return MAKE_INT(sum);
}


/*
 * The name of the module is just used for display error message. Put
 * something meaningful in it. The statements beween MODULE_ENTRY_START
 * and MODULE_ENTRY_END are executed when the module is loaded.
 */
MODULE_ENTRY_START("simple-module")
     STk_puts("Loading extension", STk_curr_oport);
     ADD_PRIMITIVE(tst);
     ADD_PRIMITIVE(add);
MODULE_ENTRY_END
