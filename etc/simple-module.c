/*
 * simple-module.c  -- A simple C module for STklos
 *
 * Copyright © 2000-2024 Erick Gallesio <eg@stklos.net>
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
 */

/* This module defines three simple primitives called "hello", "test" and "add".
 *    - "hello" takes no parameter and displays a simple message
 *    - "test" takes one parameter, displays it and returns it (reversed if it
 *      is a pair, unmodified otherwise.
 *    - "add" returns the sum of its two (small) integer parameters
 *
 * New primitives are defined with the DEFINE_PRIMITIVE macro
 * and module entry is defined with the MODULE_ENTRY_* macros
 *
 * The utility "stklos-config" helps you to compile this module
 * The main option of this programs are:
 *     --compile (or -c) to find the command to compile a C file
 *     --link (or -l) to find the command to pre-link the shared object
 *     --shared-suffix (or -s) to find the suffix used by your system for
 *             shared libraries
 *
 * To test this module: just enter in a terminal:
 *
 *      $ $(stklos-config -c) -c -o simple-module.o simple-module.c
 *      $ $(stklos-config -l) -o simple-module.$(stklos-config -s) simple-module.o
 *
 * File can be loaded from the interpreter by doing
 *
 *      stklos> (load "simple-module")
 *
 * (You may have to set you LD_LIBRARY_PATH variable to find the file,
 * or place the shared object in a directory present in *load-path*)
 *
 */

#include <stklos.h>


DEFINE_PRIMITIVE("hello", hello, subr0, (void))
{
  SCM msg = STk_Cstring2string("Hello from STklos.\n"); // convert C str -> SCM str
  STk_display(msg, STk_current_output_port());          // call SCM display
  return STk_void;                                      // return #void
}


DEFINE_PRIMITIVE("test", tst, subr1, (SCM l))
{
  SCM oport =  STk_current_output_port();
  STk_fprintf(oport, "Parameter is ");
  STk_display(l, oport);
  STk_putc('\n', oport);

  return CONSP(l) ? STk_cons(CDR(l), CAR(l)): l;       // Reverse CONSes. Keep others
}

DEFINE_PRIMITIVE("add", add, subr2, (SCM a, SCM b))
{
  long sum;
  if (!INTP(a)) STk_error("First parameter ~S is not an integer", a);
  if (!INTP(b)) STk_error("Second parameter ~S is not an integer", b);

  sum = INT_VAL(a) + INT_VAL(b);                      // INT_VAL returns a C int
  return MAKE_INT(sum);                               // MAKE_INT returns a SCM int
}


/*
 * The name of the module is just used to display error message. Put
 * something meaningful in it. The statements beween MODULE_ENTRY_START
 * and MODULE_ENTRY_END are executed when the module is loaded.
 *
 * ADD_PRIMITIVE_IN_MODULE adds a primitive in the given module.
 */
MODULE_ENTRY_START("simple-module")
{
  SCM mod = STk_current_module();
  STk_puts("Loading extension simple-module\n", STk_current_output_port());

  ADD_PRIMITIVE_IN_MODULE(hello, mod);
  ADD_PRIMITIVE_IN_MODULE(tst, mod);
  ADD_PRIMITIVE_IN_MODULE(add, mod);
}
MODULE_ENTRY_END
