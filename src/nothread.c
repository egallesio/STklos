/*
 * nothread.c			-- Threads support in STklos
 * 
 * Copyright © 2006 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 *           Author: Stephane Epardaud [stephane.epardaud@inria.fr]
 *    Creation date: 23-Jan-2006 12:14 (se)
 * Last file update: 13-Apr-2006 15:51 (se)
 */

#include "nothread.h"
#include "stklos.h"
#include "vm.h"

SCM STk_primordial_thread = NULL;
static vm_thread_t *current_vm = NULL;

vm_thread_t *STk_get_current_vm(void){
  return current_vm;
}

DEFINE_PRIMITIVE("current-thread", current_thread, subr0, (void))
{
  vm_thread_t *vm = STk_get_current_vm();
  return vm->scheme_thread;
}

DEFINE_PRIMITIVE("%thread-system", thread_system, subr0, (void))
{
  return STk_intern("none");
}

int STk_init_threads(int stack_size)
{
  current_vm = STk_allocate_vm(stack_size);
  current_vm->scheme_thread = STk_false;
  STk_primordial_thread = STk_false;

  /* Thread primitives */
  ADD_PRIMITIVE(thread_system);
  ADD_PRIMITIVE(current_thread);

  return TRUE;
}

