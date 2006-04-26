/*
 * thread-common.c			-- Threads support in STklos
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
 *           Author: Erick Gallesio [eg@essi.fr]
 *    Creation date: 23-Jan-2006 12:14 (eg)
 * Last file update: 26-Apr-2006 16:18 (eg)
 */
#include <unistd.h>
#include "stklos.h"
#include "vm.h"
#include "thread-common.h"

SCM STk_primordial_thread = NULL;

SCM STk_cond_thread_terminated;
static SCM cond_thread_abandonned_mutex, cond_join_timeout;

void STk_error_bad_thread(SCM obj)
{
  STk_error("bad thread ~S", obj);
}


struct timeval STk_thread_abstime_to_reltime(double abs_secs)
{
  struct timeval abs, cur, rel;

  abs.tv_sec  = (long) abs_secs; /* trim to the second */
  abs.tv_usec = (long) ((abs_secs - abs.tv_sec) * 1000000);

  /* now deduce the current time */
  gettimeofday(&cur, NULL);
  rel.tv_sec  = abs.tv_sec - cur.tv_sec;
  rel.tv_usec = abs.tv_usec - cur.tv_usec;
  if (rel.tv_usec < 0) {
    rel.tv_sec  -= 1;
    rel.tv_usec += 1000000;
  }

  /* is it negative ? */
  if (rel.tv_sec < 0) {
    rel.tv_sec = 0;
    rel.tv_usec = 0;
  }
  return rel;
}

/* ====================================================================== */

DEFINE_PRIMITIVE("current-thread", current_thread, subr0, (void))
{
  vm_thread_t *vm = STk_get_current_vm();
  return vm->scheme_thread;
}

static SCM do_make_thread(SCM thunk, SCM name)
{
  SCM z;
  
  NEWCELL(z, thread);
  
  THREAD_THUNK(z)     = thunk;
  THREAD_NAME(z)      = name;
  THREAD_SPECIFIC(z)  = STk_void;
  THREAD_RESULT(z)    = STk_void;
  THREAD_EXCEPTION(z) = STk_false;
  THREAD_STATE(z)     = th_new;
  THREAD_VM(z)        = NULL;

  STk_do_make_sys_thread(z);

  return z;
}

DEFINE_PRIMITIVE("%make-thread", make_thread, subr12, (SCM thunk, SCM name))
{
  SCM z;

  if (STk_procedurep(thunk) == STk_false) 
    STk_error("bad thunk ~S", thunk);

  z = do_make_thread(thunk, (name ? name : STk_false));
  return z;
}


DEFINE_PRIMITIVE("thread?", threadp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(THREADP(obj));
}

DEFINE_PRIMITIVE("thread-name", thread_name, subr1, (SCM thr))
{
  if (! THREADP(thr)) STk_error_bad_thread(thr);
  return THREAD_NAME(thr);
}

DEFINE_PRIMITIVE("%thread-end-exception", thread_end_exception, subr1, (SCM thr))
{
  if (!THREADP(thr)) STk_error_bad_thread(thr);
  return THREAD_EXCEPTION(thr);
}

DEFINE_PRIMITIVE("%thread-end-exception-set!", thread_end_exception_set, 
		 subr2, (SCM thr, SCM val))
{
  if (!THREADP(thr)) STk_error_bad_thread(thr);
  THREAD_EXCEPTION(thr) = val;
  return STk_void;
}

DEFINE_PRIMITIVE("%thread-end-result", thread_end_result, subr1, (SCM thr))
{
  if (!THREADP(thr)) STk_error_bad_thread(thr);
  return THREAD_RESULT(thr);
}

DEFINE_PRIMITIVE("%thread-end-result-set!", thread_end_result_set, 
		 subr2, (SCM thr, SCM val))
{
  if (!THREADP(thr)) STk_error_bad_thread(thr);
  THREAD_RESULT(thr) = val;
  return STk_void;
}


DEFINE_PRIMITIVE("thread-specific", thread_specific, subr1, (SCM thr))
{
  if (! THREADP(thr)) STk_error_bad_thread(thr);
  return THREAD_SPECIFIC(thr);
}

DEFINE_PRIMITIVE("thread-specific-set!", thread_specific_set, subr2, 
		 (SCM thr, SCM value))
{
  if (!THREADP(thr)) STk_error_bad_thread(thr);
  THREAD_SPECIFIC(thr) = value;
  return STk_void;
}

DEFINE_PRIMITIVE("thread-start!", thread_start, subr1, (SCM thr))
{
  vm_thread_t *vm, *new;
  
  if (!THREADP(thr)) STk_error_bad_thread(thr);
  if (THREAD_STATE(thr) != th_new) 
    STk_error("thread has already been started ~S", thr);

  vm  = STk_get_current_vm();
  new = STk_allocate_vm(5000);			// FIX:

  new->current_module = vm->current_module;
  new->iport          = vm->iport;
  new->oport          = vm->oport;
  new->eport          = vm->eport;
  new->parameters     = STk_copy_tree(vm->parameters);
  new->scheme_thread  = thr;
  
  THREAD_VM(thr)      = new; 
  THREAD_STATE(thr)   = th_runnable;  

  STk_sys_thread_start(thr);
  
  return thr;
}

/* ======================================================================
 * 	Initialization ...
 * ====================================================================== 
 */

static void print_thread(SCM thread, SCM port, int mode)
{
  char *s;
  SCM name = THREAD_NAME(thread);
  
  STk_puts("#[thread ", port);
  if (name != STk_false) 
    STk_print(name, port, DSP_MODE);
  else
    STk_fprintf(port, "%lx", (unsigned long) thread);
  switch (THREAD_STATE(thread)) {
    case th_new:        s = "new"; break;
    case th_runnable:   s = "runnable"; break;
    case th_terminated: s = "terminated"; break;
    case th_blocked:    s = "blocked"; break;
    default:            s = "???"; break;
  }
  STk_fprintf(port, " (%s)", s);
  STk_putc(']', port);
}


/* The stucture which describes the thread type */
static struct extended_type_descr xtype_thread = {
  "thread",			/* name */
  print_thread			/* print function */
};

int STk_init_threads(int stack_size)
{
  vm_thread_t *vm = STk_allocate_vm(stack_size);
  SCM primordial;

  /* Thread Type declaration */
  DEFINE_XTYPE(thread, &xtype_thread);
  
  /* Specific thread initialisation */ 
  if(STk_init_sys_threads(vm) != TRUE)
    return FALSE;

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
  primordial = do_make_thread(STk_false, STk_Cstring2string("primordial"));
  THREAD_STATE(primordial) = th_runnable;
  THREAD_VM(primordial)    = vm;
  vm->scheme_thread        = primordial;
  STk_primordial_thread    = primordial;

  /* Thread primitives */
  ADD_PRIMITIVE(current_thread);
  ADD_PRIMITIVE(make_thread);
  ADD_PRIMITIVE(threadp);
  ADD_PRIMITIVE(thread_name);
  ADD_PRIMITIVE(thread_end_exception);
  ADD_PRIMITIVE(thread_end_exception_set);
  ADD_PRIMITIVE(thread_end_result);
  ADD_PRIMITIVE(thread_end_result_set);
  ADD_PRIMITIVE(thread_specific);
  ADD_PRIMITIVE(thread_specific_set);
  ADD_PRIMITIVE(thread_start);
  ADD_PRIMITIVE(thread_yield);
  ADD_PRIMITIVE(thread_terminate);
  ADD_PRIMITIVE(thread_join);
  ADD_PRIMITIVE(thread_sleep);
  ADD_PRIMITIVE(thread_system);

  return TRUE;
}
