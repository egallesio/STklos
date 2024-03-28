/*
 * thread-common.c                      -- Threads support in STklos
 *
 * Copyright © 2006-2023 Erick Gallesio <eg@stklos.net>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.mu
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

double STk_verify_timeout(SCM tm) {
  double res = STk_number2double(tm);

  if (isnan(res)) STk_error("bad timeout ~S", tm);
  return res;
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


/*
<doc EXT current-thread
 * (current-thread)
 *
 * Returns the current thread.
 * @lisp
 * (eq? (current-thread) (current-thread))  =>  #t
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("current-thread", current_thread, subr0, (void))
{
  vm_thread_t *vm = STk_get_current_vm();
  return vm->scheme_thread;
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


/* ====================================================================== */


static SCM do_make_thread(SCM thunk, SCM name, int stack_size)
{
  SCM z;

  NEWCELL(z, thread);

  THREAD_THUNK(z)      = thunk;
  THREAD_NAME(z)       = name;
  THREAD_SPECIFIC(z)   = STk_void;
  THREAD_RESULT(z)     = STk_void;
  THREAD_EXCEPTION(z)  = STk_false;
  THREAD_STATE(z)      = th_new;
  THREAD_STACK_SIZE(z) = stack_size;
  THREAD_VM(z)         = NULL;

  STk_do_make_sys_thread(z);

  return z;
}


DEFINE_PRIMITIVE("%make-thread", make_thread, subr3,(SCM thunk, SCM name, SCM ssize))
{
  SCM z;
  long stack_size;

  if (STk_procedurep(thunk) == STk_false)
    STk_error("bad thunk ~S", thunk);
  if (ssize == STk_false)
    /* If no size is specified, use primordial thread stack size */
    stack_size = THREAD_STACK_SIZE(STk_primordial_thread);
  else {
    stack_size = STk_integer_value(ssize);
    if (stack_size < 0)
      STk_error("bad stack size ~S", ssize);
  }

  z = do_make_thread(thunk, (name ? name : STk_false), stack_size);
  return z;
}

/*
<doc EXT thread?
 * (thread? obj)
 *
 * Returns |#t| if |obj| is a thread, otherwise returns |#f|.
 * @lisp
 * (thread? (current-thread))  => #t
   (thread? 'foo)              => #f
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("thread?", threadp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(THREADP(obj));
}


/*
<doc EXT thread-name
 * (thread-name thread)
 *
 * Returns the name of the |thread|.
 * @lisp
 * (thread-name (make-thread (lambda () #f) 'foo))  =>  foo
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("thread-name", thread_name, subr1, (SCM thr))
{
  if (! THREADP(thr)) STk_error_bad_thread(thr);
  return THREAD_NAME(thr);
}

/*
<doc EXT thread-stack-size
 * (thread-stack-size thread)
 *
 * Returns the allocated stack size for |thread|.
 * @lisp
 * (thread-stack-size (make-thread (lambda () #f) 'foo 2000)) => 2000
 * @end lisp
 *
 * Note that this procedure is not present in {{quick-link-srfi 18}}.
doc>
*/
DEFINE_PRIMITIVE("thread-stack-size", thread_ssize, subr1, (SCM thr))
{
  if (! THREADP(thr)) STk_error_bad_thread(thr);
  return MAKE_INT(THREAD_STACK_SIZE(thr));
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

/*
<doc EXT thread-specific
 * (thread-specific thread)
 *
 * Returns the content of the |thread|'s specific field.
doc>
*/
DEFINE_PRIMITIVE("thread-specific", thread_specific, subr1, (SCM thr))
{
  if (! THREADP(thr)) STk_error_bad_thread(thr);
  return THREAD_SPECIFIC(thr);
}

/*
<doc EXT thread-specific-set!
 * (thread-specific-set! thread)
 *
 * Stores |obj| into the |thread|'s specific field. |Thread-specific-set!|
 * returns an unspecified value.
 * @lisp
 * (thread-specific-set! (current-thread) "hello")
 *            =>  unspecified
 * (thread-specific (current-thread))
 *            =>  "hello"
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("thread-specific-set!", thread_specific_set, subr2,
                 (SCM thr, SCM value))
{
  if (!THREADP(thr)) STk_error_bad_thread(thr);
  THREAD_SPECIFIC(thr) = value;
  return STk_void;
}


/*
<doc EXT thread-start!
 * (thread-start! thread)
 *
 * Makes |thread| runnable. The |thread| must be a new thread.
 * |Thread-start!| returns the thread.
 * @lisp
 * (let ((t (thread-start! (make-thread
 *                            (lambda () (write 'a))))))
 *    (write 'b)
 *    (thread-join! t))       =>  unspecified
 *                                after writing ab or ba
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("thread-start!", thread_start, subr1, (SCM thr))
{
  vm_thread_t *vm, *new;

  if (!THREADP(thr)) STk_error_bad_thread(thr);
  if (THREAD_STATE(thr) != th_new)
    STk_error("thread has already been started ~S", thr);

  vm  = STk_get_current_vm();
  new = STk_allocate_vm(THREAD_STACK_SIZE(thr));

  new->current_module = vm->current_module;
  new->iport          = vm->iport;
  new->oport          = vm->oport;
  new->eport          = vm->eport;
  new->scheme_thread  = thr;

  THREAD_VM(thr)      = new;
  THREAD_STATE(thr)   = th_runnable;

  THREAD_ALLOCATIONS(thr)     = 0;
  THREAD_BYTES_ALLOCATED(thr) = 0;

  STk_sys_thread_start(thr);

  return thr;
}

void thread_inc_allocs(SCM thr, int size) {
  THREAD_ALLOCATIONS(thr) ++;
  THREAD_BYTES_ALLOCATED(thr) += size;
}

DEFINE_PRIMITIVE("%thread-allocations-reset!", thread_allocs_reset, subr1, (SCM thr)) {
  if (!THREADP(thr)) STk_error_bad_thread(thr);
  THREAD_BYTES_ALLOCATED(thr) = 0;
  THREAD_ALLOCATIONS(thr) = 0;
  return STk_void;
}

DEFINE_PRIMITIVE("%thread-allocations", thread_allocs, subr1, (SCM thr)) {
  if (!THREADP(thr)) STk_error_bad_thread(thr);
  unsigned long bytes = THREAD_BYTES_ALLOCATED(thr);
  unsigned long cells = bytes / sizeof(SCM);
  return STk_n_values(3,
                      STk_ulong2integer(THREAD_BYTES_ALLOCATED(thr)),
                      STk_ulong2integer(cells),
                      STk_ulong2integer(THREAD_ALLOCATIONS(thr)));
}

/* ======================================================================
 *      Initialization ...
 * ======================================================================
 */

static void print_thread(SCM thread, SCM port, int _UNUSED(mode))
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
  .name  = "thread",
  .print = print_thread
};

/* ---------------------------------------------------------------------- */

int STk_init_threads(int stack_size, void *start_stack)
{
  vm_thread_t *vm = STk_allocate_vm(stack_size);
  SCM primordial;

  /* Thread Type declaration */
  DEFINE_XTYPE(thread, &xtype_thread);

  /* Specific thread initialisation */
  if (STk_init_sys_threads(vm) != TRUE)
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
  primordial = do_make_thread(STk_false,
                              STk_Cstring2string("primordial"),
                              stack_size);
  THREAD_STATE(primordial) = th_runnable;
  THREAD_VM(primordial)    = vm;
  THREAD_ALLOCATIONS(primordial) = 0;
  THREAD_BYTES_ALLOCATED(primordial) = 0;
  vm->scheme_thread        = primordial;
  vm->start_stack          = start_stack;
  STk_primordial_thread    = primordial;

  /* Thread primitives */
  ADD_PRIMITIVE(current_thread);
  ADD_PRIMITIVE(thread_dynwind_stack);
  ADD_PRIMITIVE(thread_dynwind_stack_set);
  ADD_PRIMITIVE(make_thread);
  ADD_PRIMITIVE(threadp);
  ADD_PRIMITIVE(thread_name);
  ADD_PRIMITIVE(thread_ssize);
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
  ADD_PRIMITIVE(thread_allocs);
  ADD_PRIMITIVE(thread_allocs_reset);

  return TRUE;
}
