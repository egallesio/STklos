/*
 * threads.c			-- Threads support in STklos
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
 * Last file update:  1-Feb-2006 18:07 (eg)
 */


#define _REENTRANT 1
#define GC_LINUX_THREADS 1
#include <pthread.h>
#include <unistd.h>
#include "stklos.h"
#include "vm.h"

static SCM primordial, thread_terminated_cond;


enum thread_state { th_new, th_runnable, th_terminated, th_blocked};

struct thread_obj {
  stk_header header;
  SCM thunk;
  SCM name;
  SCM specific;
  SCM end_result;
  SCM end_exception;
  SCM mutexes;
  SCM dynwind;
  enum thread_state state;
  vm_thread_t *vm;
  pthread_t pthread;
};


#define THREADP(p)		(BOXED_TYPE_EQ((p), tc_thread))
#define THREAD_THUNK(p)		(((struct thread_obj *) (p))->thunk)
#define THREAD_NAME(p)		(((struct thread_obj *) (p))->name)
#define THREAD_SPECIFIC(p)	(((struct thread_obj *) (p))->specific)
#define THREAD_RESULT(p)	(((struct thread_obj *) (p))->end_result)
#define THREAD_EXCEPTION(p)	(((struct thread_obj *) (p))->end_exception)
#define THREAD_MUTEXES(p)	(((struct thread_obj *) (p))->mutexes)
#define THREAD_DYNWIND(p)	(((struct thread_obj *) (p))->dynwind)
#define THREAD_STATE(p)		(((struct thread_obj *) (p))->state)
#define THREAD_VM(p)		(((struct thread_obj *) (p))->vm)
#define THREAD_PTHREAD(p)	(((struct thread_obj *) (p))->pthread)

static SCM all_threads = STk_nil;


static void error_bad_thread(SCM obj)
{
  STk_error("bad thread ~S", obj);
}


/*
 * Thread specific value (the VM)
 */
static pthread_key_t vm_key;

static void *cleanup_vm_specific(void *p)    /* Nothing to do for now */
{
  return NULL;
}

static void initialize_vm_key(void)
{
  int n =  pthread_key_create(&vm_key, (void (*) (void *)) cleanup_vm_specific);

  if (n) {
    fprintf(stderr, "Cannot initialize the VM specific data\n");
    perror("stklos");
    exit(1);
  }
}

vm_thread_t *STk_get_current_vm(void)
{
  return (vm_thread_t *) pthread_getspecific(vm_key);
}


/* ====================================================================== */

static SCM do_make_thread(SCM thunk, char *name)
{
  SCM z;

  NEWCELL(z, thread);
  
  THREAD_THUNK(z)     = thunk;
  THREAD_NAME(z)      = name;
  THREAD_SPECIFIC(z)  = STk_void;
  THREAD_RESULT(z)    = STk_void;
  THREAD_EXCEPTION(z) = STk_false;
  THREAD_MUTEXES(z)   = STk_nil;
  THREAD_DYNWIND(z)   = STk_nil;
  THREAD_STATE(z)     = th_new;

  // FIX: lock
  all_threads = STk_cons(z, all_threads); /* For the GC */
  return z;
}

DEFINE_PRIMITIVE("current-thread", current_thread, subr0, (void))
{
  vm_thread_t *vm = STk_get_current_vm();
  return vm->scheme_thread;
}

DEFINE_PRIMITIVE("make-thread", make_thread, subr12, (SCM thunk, SCM name))
{
  SCM z;

  if (STk_procedurep(thunk) == STk_false) 
    STk_error("bad thunk ~S", thunk);
  if (name) {
    if (!STRINGP(name))
      STk_error("bad thread name ~S", name);
  }
  else name = STk_Cstring2string("");

  z = do_make_thread(thunk, name);
  return z;
}


DEFINE_PRIMITIVE("thread?", threadp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(THREADP(obj));
}

DEFINE_PRIMITIVE("thread-name", thread_name, subr1, (SCM thr))
{
  if (! THREADP(thr)) error_bad_thread(thr);
  return THREAD_NAME(thr);
}

DEFINE_PRIMITIVE("thread-specific", thread_specific, subr1, (SCM thr))
{
  if (! THREADP(thr)) error_bad_thread(thr);
  return THREAD_SPECIFIC(thr);
}

DEFINE_PRIMITIVE("thread-specific-set!", thread_specific_set, subr2, 
		 (SCM thr, SCM value))
{
  if (!THREADP(thr)) error_bad_thread(thr);
  THREAD_SPECIFIC(thr) = value;
  return STk_void;
}


static void terminate_scheme_thread(SCM thr)
{
  THREAD_STATE(thr)  = th_terminated;
  // ...........................
}



static void * start_scheme_thread(void *arg)
{
  SCM thr = (SCM) arg;
  vm_thread_t *vm;

  vm = STk_allocate_vm(5000);			// FIX:
  
  pthread_setspecific(vm_key, vm);
  THREAD_VM(thr) = vm;
  vm->scheme_thread = thr;

  THREAD_RESULT(thr) = STk_C_apply(THREAD_THUNK(thr), 0);
  STk_debug("On termine normallement la thread ~S", thr);
  terminate_scheme_thread(thr);
  return NULL;
}


DEFINE_PRIMITIVE("thread-start!", thread_start, subr1, (SCM thr))
{
  if (!THREADP(thr)) error_bad_thread(thr);
  if (THREAD_STATE(thr) != th_new) 
    STk_error("thread has already been started ~S", thr);

  THREAD_STATE(thr) = th_runnable;

  if (pthread_create(&THREAD_PTHREAD(thr), NULL, start_scheme_thread, thr))
    STk_error("cannot start thread ~S", thr);

  return thr;
}



DEFINE_PRIMITIVE("thread-yield!", thread_yield, subr0, (void))
{
#ifdef _POSIX_PRIORITY_SCHEDULING
  sched_yield();
#else
  /* Do nothing. Is it correct? */
#endif
  return STk_void;
}

DEFINE_PRIMITIVE("thread-terminate!", thread_terminate, subr1, (SCM thr))
{
  if (!THREADP(thr)) error_bad_thread(thr);

  
  if (THREAD_STATE(thr) != th_terminated) {
    terminate_scheme_thread(thr);
    if (thr == primordial) {
      /* Terminate the primordial thread exits the program */
      STk_quit(0);
    }
    THREAD_EXCEPTION(thr) = STk_nil;		//FIX:
    pthread_cancel(THREAD_PTHREAD(thr));
  }
  return STk_void;
}


DEFINE_PRIMITIVE("all-threads", all_threads, subr0, (void))
{
  /* Use reverse to give a (time creation ordered) copy of our list */
  return STk_reverse(all_threads);
}



/* ======================================================================
 * 	Initialization ...
 * ====================================================================== 
 */

static void print_thread(SCM thread, SCM port, int mode)
{
  char *s, *name = STRING_CHARS(THREAD_NAME(thread));
  
  STk_puts("#[thread ", port);
  if (*name) 
    STk_puts(name, port);
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

  /* Thread Type declaration */
  DEFINE_XTYPE(thread, &xtype_thread);
  
  /* Define the key to access the thead specific VM */ 
  initialize_vm_key();
  pthread_setspecific(vm_key, vm);

  /* Define the threads exceptions */
  //  thread_terminated_cond =  STk_defcond_type("&thread-terminated", STk_false,
  //					     STk_nil, STk_current_module);
  
  /* Wrap the main thread in a thread called "primordial" */
  primordial = do_make_thread(STk_false, STk_Cstring2string("primordial"));
  THREAD_STATE(primordial) = th_runnable;
  THREAD_VM(primordial)    = vm;
  vm->scheme_thread        = primordial;

  /* Thread primitives */
  ADD_PRIMITIVE(current_thread);
  ADD_PRIMITIVE(make_thread);
  ADD_PRIMITIVE(threadp);
  ADD_PRIMITIVE(thread_name);
  ADD_PRIMITIVE(thread_specific);
  ADD_PRIMITIVE(thread_specific_set);
  ADD_PRIMITIVE(thread_start);
  ADD_PRIMITIVE(thread_yield);
  ADD_PRIMITIVE(thread_terminate);

  
  ADD_PRIMITIVE(all_threads);

  return TRUE;
}
