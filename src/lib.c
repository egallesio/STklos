/*
 * l i b . c					-- Scheme library 
 * 
 * Copyright © 2000-2007 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date:  5-Jan-2000 12:17 (eg)
 * Last file update: 29-Aug-2007 12:33 (eg)
 */


#include "stklos.h"
#include "hash.h"


int STk_library_initialized = FALSE;  /* True when successfully initialized */

int
STk_init_library(int *argc, char ***argv, int stack_size)
{
  void * start_stack;

  STk_get_stack_pointer(&start_stack);

  return 
    STk_init_env()				&&
    STk_init_symbol()				&&
    STk_late_init_env() 			&&
    STk_init_struct()				&&
    STk_init_cond()				&&
    STk_init_vm()				&&
    STk_init_threads(stack_size, start_stack)	&&
    STk_init_port() 				&&
    STk_init_extend()				&&
    STk_init_list() 				&&
    STk_init_vector() 				&&
    STk_init_uniform_vector()			&&
    STk_init_char()				&&
    STk_init_keyword()				&&
    STk_init_string()   			&&
    STk_init_parameter()			&&
    STk_init_proc()				&&
    STk_init_boolean()				&&
    STk_init_reader()   			&&
    STk_init_system()   			&&
    STk_init_mutexes()				&&
    STk_init_number()				&&
    STk_init_fixnum()				&&
    STk_init_hash()				&&
    STk_init_misc()				&&
    STk_init_signal()				&&
    STk_init_promise()				&&
    STk_init_regexp()				&&
    STk_init_process()				&&
    STk_init_socket()				&&
    STk_init_object()				&&
    STk_init_base64()				&&
    STk_init_md5()				&&
    STk_init_cpointer()				&&
    STk_init_ffi()				&&
    (STk_library_initialized = TRUE);
}
