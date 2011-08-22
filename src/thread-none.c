/*
 * thread-none.c			-- Threads support in STklos
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
 * Last file update: 27-Oct-2006 14:55 (eg)
 */

#include "thread-common.h"
#include "stklos.h"
#include "vm.h"

SCM STk_primordial_thread = NULL;
SCM STk_cond_thread_terminated;
static SCM cond_thread_abandonned_mutex, cond_join_timeout;

static vm_thread_t *current_vm;


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


DEFINE_PRIMITIVE("%thread-no-support", threadno, vsubr, (int argc, SCM *argv))
{
  STk_error("your version of stklos does not provide thread support");
  return STk_void;
}

DEFINE_PRIMITIVE("%thread-dynwind-stack", thread_dynwind_stack, subr0, (void))
{
  vm_thread_t *vm = STk_get_current_vm();
  return vm->dynwind_stack;
}

DEFINE_PRIMITIVE("%thread-dynwind-stack-set!", thread_dynwind_stack_set, subr1,
		 (SCM value))
{
  vm_thread_t *vm = STk_get_current_vm();
  vm->dynwind_stack = value;
  return STk_void;

}

int STk_init_threads(int stack_size, void *start_stack)
{
  /* Define the threads exceptions */
  STk_cond_thread_terminated =
    STk_defcond_type("&thread-terminated", STk_false,
                     LIST1(STk_intern("canceller")),
                     STk_STklos_module);
  cond_thread_abandonned_mutex =  STk_defcond_type("&thread-abandonned-mutex",
                                                   STk_false,
                                                   STk_nil,
                                                   STk_STklos_module);
  cond_join_timeout = STk_defcond_type("&thread-join-timeout", STk_false,
                                       STk_nil, STk_STklos_module);

  /* Wrap the main thread in a thread called "primordial" */
  current_vm = STk_allocate_vm(stack_size);
  current_vm->scheme_thread = STk_false;
  current_vm->start_stack   = start_stack;
  STk_primordial_thread = STk_false;

  /* Thread primitives */
  ADD_PRIMITIVE(current_thread);
  ADD_PRIMITIVE(thread_system);
  ADD_PRIMITIVE(thread_dynwind_stack);
  ADD_PRIMITIVE(thread_dynwind_stack_set);

  /* Fake primitives */
  ADD_PRIMITIVE(threadno);

  FAKE_PRIMITIVE("%make-thread");
  FAKE_PRIMITIVE("thread?");
  FAKE_PRIMITIVE("thread-name");
  FAKE_PRIMITIVE("%thread-end-exception");
  FAKE_PRIMITIVE("%thread-end-exception-set!");
  FAKE_PRIMITIVE("%thread-end-result");
  FAKE_PRIMITIVE("%thread-end-result-set!");
  FAKE_PRIMITIVE("thread-specific");
  FAKE_PRIMITIVE("thread-specific-set!");
  FAKE_PRIMITIVE("thread-start!");
  FAKE_PRIMITIVE("thread-yield!");
  FAKE_PRIMITIVE("thread-terminate!");
  FAKE_PRIMITIVE("%thread-join!");
  FAKE_PRIMITIVE("%thread-sleep!");

  return TRUE;
}

