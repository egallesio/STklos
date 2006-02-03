/*
 * thread.c			-- Threads support in STklos
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
 * Last file update:  3-Feb-2006 11:06 (eg)
 */


#define _REENTRANT 1
#define GC_LINUX_THREADS 1
#include <pthread.h>
#include <unistd.h>
#include "stklos.h"
#include "vm.h"

static SCM primordial, cond_thread_terminated;


enum thread_state { th_new, th_runnable, th_terminated, th_blocked};

struct thread_obj {
  stk_header header;
  SCM thunk;
  SCM name;
  SCM specific;
  SCM end_result;
  SCM end_exception;
  enum thread_state state;
  vm_thread_t *vm;
  pthread_t pthread;
  pthread_mutex_t mymutex;
  pthread_cond_t  mycondv;
};


#define THREADP(p)		(BOXED_TYPE_EQ((p), tc_thread))
#define THREAD_THUNK(p)		(((struct thread_obj *) (p))->thunk)
#define THREAD_NAME(p)		(((struct thread_obj *) (p))->name)
#define THREAD_SPECIFIC(p)	(((struct thread_obj *) (p))->specific)
#define THREAD_RESULT(p)	(((struct thread_obj *) (p))->end_result)
#define THREAD_EXCEPTION(p)	(((struct thread_obj *) (p))->end_exception)
#define THREAD_STATE(p)		(((struct thread_obj *) (p))->state)
#define THREAD_VM(p)		(((struct thread_obj *) (p))->vm)
#define THREAD_PTHREAD(p)	(((struct thread_obj *) (p))->pthread)
#define THREAD_MYMUTEX(p)	(((struct thread_obj *) (p))->mymutex)
#define THREAD_MYCONDV(p)	(((struct thread_obj *) (p))->mycondv)

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

static void terminate_scheme_thread(void *arg)
{
  SCM thr = (SCM) arg;

  STk_debug("Cleaning thread thr");

  pthread_mutex_lock(&THREAD_MYMUTEX(thr));
  THREAD_STATE(thr)  = th_terminated;

  /* signal the death of this thread to the ones waiting it */
  pthread_cond_broadcast(&THREAD_MYCONDV(thr));
  pthread_mutex_unlock(&THREAD_MYMUTEX(thr));
}


static void *start_scheme_thread(void *arg)
{
  volatile SCM thr = (SCM) arg;
  vm_thread_t *vm;
  SCM res;

  vm = THREAD_VM(thr) = STk_allocate_vm(5000);			// FIX:
  vm->scheme_thread = thr;  
  pthread_setspecific(vm_key, vm);

  pthread_cleanup_push(terminate_scheme_thread, thr);
  
  res = STk_C_apply(THREAD_THUNK(thr), 0);
  if (THREAD_EXCEPTION(thr) == STk_false) {
    THREAD_RESULT(thr) = res;
  }
  pthread_cleanup_pop(1);
  return NULL;
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
  THREAD_STATE(z)     = th_new;

  // FIX: lock
  //  all_threads = STk_cons(z, all_threads); /* For the GC */
  return z;
}

DEFINE_PRIMITIVE("current-thread", current_thread, subr0, (void))
{
  vm_thread_t *vm = STk_get_current_vm();
  return vm->scheme_thread;
}

DEFINE_PRIMITIVE("%make-thread", make_thread, subr12, (SCM thunk, SCM name))
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

DEFINE_PRIMITIVE("%thread-end-exception", thread_end_exception, subr1, (SCM thr))
{
  if (!THREADP(thr)) error_bad_thread(thr);
  return THREAD_EXCEPTION(thr);
}

DEFINE_PRIMITIVE("%thread-end-exception-set!", thread_end_exception_set, 
		 subr2, (SCM thr, SCM val))
{
  if (!THREADP(thr)) error_bad_thread(thr);
  THREAD_EXCEPTION(thr) = val;
  return STk_void;
}

DEFINE_PRIMITIVE("%thread-end-result", thread_end_result, subr1, (SCM thr))
{
  if (!THREADP(thr)) error_bad_thread(thr);
  return THREAD_RESULT(thr);
}

DEFINE_PRIMITIVE("%thread-end-result-set!", thread_end_result_set, 
		 subr2, (SCM thr, SCM val))
{
  if (!THREADP(thr)) error_bad_thread(thr);
  THREAD_RESULT(thr) = val;
  return STk_void;
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




DEFINE_PRIMITIVE("thread-start!", thread_start, subr1, (SCM thr))
{
  pthread_attr_t attr;
  
  if (!THREADP(thr)) error_bad_thread(thr);
  if (THREAD_STATE(thr) != th_new) 
    STk_error("thread has already been started ~S", thr);

  THREAD_STATE(thr) = th_runnable;

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, TRUE);
  pthread_mutex_init(&THREAD_MYMUTEX(thr), NULL);
  pthread_cond_init(&THREAD_MYCONDV(thr), NULL);

  // pthread_mutex_lock(&THREAD_MYMUTEX(thr));

  if (pthread_create(&THREAD_PTHREAD(thr), NULL, start_scheme_thread, thr))
    STk_error("cannot start thread ~S", thr);

  pthread_attr_destroy(&attr);

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
    THREAD_EXCEPTION(thr) = STk_make_C_cond(cond_thread_terminated, 0);
    pthread_cancel(THREAD_PTHREAD(thr));
  }
  return STk_void;
}


DEFINE_PRIMITIVE("%thread-join!", thread_join, subr4, (SCM thr, SCM tm1, SCM tm2,
						       SCM use_time))
{
  struct timespec ts;
  int overflow;
  time_t t1 = STk_integer2uint32(tm1, &overflow);
  long   t2 = STk_integer2uint32(tm2, &overflow);
  SCM res = STk_false;

  if (!THREADP(thr)) error_bad_thread(thr);
  
  ts.tv_sec  = t1;
  ts.tv_nsec = t2;

  pthread_mutex_lock(&THREAD_MYMUTEX(thr));
  while (THREAD_STATE(thr) != th_terminated) {
    STk_debug("On est dans la boucle avec %d", THREAD_STATE(thr));
    if (use_time != STk_false) {
      int n = pthread_cond_timedwait(&THREAD_MYCONDV(thr), 
				     &THREAD_MYMUTEX(thr),
				     &ts);
      if (n == ETIMEDOUT) { STk_debug("TIMEOUT"); res = STk_true; break; }
    }
    else 
      pthread_cond_wait(&THREAD_MYCONDV(thr), &THREAD_MYMUTEX(thr));
  }
  pthread_mutex_unlock(&THREAD_MYMUTEX(thr));
  STk_debug("Fin de l'attente");
  return res;
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
  cond_thread_terminated =  STk_defcond_type("&thread-terminated", STk_false,
					     STk_nil, STk_current_module);
  
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

  return TRUE;
}

