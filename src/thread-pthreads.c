/*
 * thread-pthreads.c			-- Threads support in STklos
 * 
 * Copyright  2006 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 * Last file update:  3-Jan-2009 22:28 (eg)
 */


#include <unistd.h>
#include <sched.h>
#include "stklos.h"
#include "vm.h"
#include "thread-common.h"

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
  vm_thread_t *vm = pthread_getspecific(vm_key);
  
  return vm? vm : THREAD_VM(STk_primordial_thread);
}


/* ====================================================================== */

static void thread_finalizer(SCM thr)
{
  pthread_mutex_destroy(&THREAD_MYMUTEX(thr));
  pthread_cond_destroy(&THREAD_MYCONDV(thr));
}

static void terminate_scheme_thread(void *arg)
{
  SCM thr = (SCM) arg;
  
  pthread_mutex_lock(&THREAD_MYMUTEX(thr));
  THREAD_STATE(thr)  = th_terminated;

  /* signal the death of this thread to the ones awaiting it */
  pthread_cond_broadcast(&THREAD_MYCONDV(thr));
  pthread_mutex_unlock(&THREAD_MYMUTEX(thr));
}


static void *start_scheme_thread(void *arg)
{
  volatile SCM thr = (SCM) arg;
  SCM res;
  void * start_stack;

  /* Get the stack start address and place it in the thread (for call/cc) */
  STk_get_stack_pointer(&start_stack);
  THREAD_VM(thr)->start_stack = start_stack;
  
  pthread_setspecific(vm_key, THREAD_VM(thr));
  pthread_cleanup_push(terminate_scheme_thread, thr);

  res = STk_C_apply(THREAD_THUNK(thr), 0);
  if (THREAD_EXCEPTION(thr) == STk_false) {
    THREAD_RESULT(thr) = res;
  }
  pthread_cleanup_pop(1);
  return NULL;
}



/* ====================================================================== */

void STk_do_make_sys_thread(SCM thr)
{
  pthread_mutex_init(&THREAD_MYMUTEX(thr), NULL);
  pthread_cond_init(&THREAD_MYCONDV(thr), NULL);

  // now the finalizer
  // STk_register_finalizer(thr, thread_finalizer);
}

void STk_sys_thread_start(SCM thr)
{
  pthread_attr_t attr;

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, TRUE);

  // pthread_mutex_lock(&THREAD_MYMUTEX(thr));
  
  /* !!!!! Use the GC_pthread_create instaed of pthread_create !!!!! */
  if (GC_pthread_create(&THREAD_PTHREAD(thr), NULL, start_scheme_thread, thr))
    STk_error("cannot start thread ~S", thr);

  pthread_attr_destroy(&attr);
}


/*
<doc EXT thread-yield! 
 * (thread-yield!)
 * 
 * The current thread exits the running state as if its quantum had 
 * expired. |Thread-yield!| returns an unspecified value.
doc>
*/
DEFINE_PRIMITIVE("thread-yield!", thread_yield, subr0, (void))
{
  sched_yield();
  return STk_void;
}


/*
<doc EXT thread-terminate! 
 * (thread-terminate! thread)
 * 
 * Causes an abnormal termination of the |thread|. If the |thread| is not 
 * already terminated, all mutexes owned by the |thread| become 
 * unlocked/abandoned and a "terminated thread exception" object is stored 
 * in the thread's end-exception field. If |thread| is the current thread, 
 * |thread-terminate!| does not return. Otherwise, |thread-terminate!| 
 * returns an unspecified value; the termination of the thread will occur
 * before |thread-terminate!| returns. 
 * £
 * ,(bold "Note: ")
 * This operation must be used carefully because it terminates a thread 
 * abruptly and it is impossible for that thread to perform any kind of
 * cleanup. This may be a problem if the thread is in the middle of a
 * critical section where some structure has been put in an inconsistent
 * state. However, another thread attempting to enter this critical section 
 * will raise an "abandoned mutex exception" because the mutex is
 * unlocked/abandoned.
doc>
*/
DEFINE_PRIMITIVE("thread-terminate!", thread_terminate, subr1, (SCM thr))
{
  if (!THREADP(thr)) STk_error_bad_thread(thr);

  if (THREAD_STATE(thr) != th_terminated) {
    enum thread_state saved_state = THREAD_STATE(thr);

    terminate_scheme_thread(thr);

    pthread_mutex_lock(&THREAD_MYMUTEX(thr));
    if (THREAD_EXCEPTION(thr) == STk_false) {
      /* Be sure to register the first canceller only!  */
      THREAD_EXCEPTION(thr) = 
        STk_make_C_cond(STk_cond_thread_terminated, 1, thr);
    }
    /* Terminate effectively the thread */
    if (thr == STk_get_current_vm()->scheme_thread)
      pthread_exit(0); 				/* Suicide */
    else {
      if (saved_state != th_new)
	pthread_cancel(THREAD_PTHREAD(thr));	/* terminate an other thread */
    }
    pthread_mutex_unlock(&THREAD_MYMUTEX(thr));
  }
  return STk_void;
}

/*
<doc EXT thread-join! 
 * (thread-join! thread)
 * (thread-join! thread timeout)
 * (thread-join! thread timeout timeout-val)
 *
 * The current thread waits until the |thread| terminates (normally or not) 
 * or until the timeout is reached if |timeout| is supplied. 
 * If the timeout is reached, |thread-join!| returns |timeout-val| if it is 
 * supplied, otherwise a "join timeout exception" is raised. 
 * If the |thread| terminated normally, the content of the end-result 
 * field is returned, otherwise the content of the end-exception field 
 * is raised.
 * @lisp
 * (let ((t (thread-start! (make-thread (lambda () 
 *                                        (expt 2 100))))))
 *   (thread-sleep! 1)
 *   (thread-join! t)) => 1267650600228229401496703205376
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("%thread-join!", thread_join, subr2, (SCM thr, SCM tm))
{
  struct timespec ts;
  SCM res = STk_false;
  double tmd;


  if (!THREADP(thr)) STk_error_bad_thread(thr);

  if (REALP(tm)) {
    tmd = REAL_VAL(tm);
    ts.tv_sec  = (time_t) tmd;
    ts.tv_nsec = (suseconds_t) ((tmd - ts.tv_sec) * 1000000);
  } 
  else if (!BOOLEANP(tm))
    STk_error("bad timeout ~S", tm);
  
  pthread_mutex_lock(&THREAD_MYMUTEX(thr));
  while (THREAD_STATE(thr) != th_terminated) {
    if (tm != STk_false) {
      int n = pthread_cond_timedwait(&THREAD_MYCONDV(thr), 
				     &THREAD_MYMUTEX(thr),
				     &ts);
      if (n == ETIMEDOUT) { res = STk_true; break; }
    }
    else 
      pthread_cond_wait(&THREAD_MYCONDV(thr), &THREAD_MYMUTEX(thr));
  }
  pthread_mutex_unlock(&THREAD_MYMUTEX(thr));
  return res;
}

/*
<doc EXT thread-sleep!
 * (thread-sleep! timeout)
 * 
 * The current thread waits until the |timeout| is reached. This blocks the
 * thread only if timeout represents a point in the future. It is an error 
 * for timeout to be |#f|. |Thread-sleep!| returns an unspecified value.
doc>
*/
DEFINE_PRIMITIVE("%thread-sleep!", thread_sleep, subr1, (SCM tm))
{
  if (REALP(tm)) {
    struct timeval tv = STk_thread_abstime_to_reltime(REAL_VAL(tm));
    struct timespec ts;
    
    /* convert a timeval (in µs) to a timesepc (in ns) */
    ts.tv_sec  = (time_t) tv.tv_sec;
    ts.tv_nsec = (long) tv.tv_usec * 1000; 
    nanosleep(&ts, NULL);
  } else
    STk_error("bad timeout ~S", tm);

  return STk_void;
}


DEFINE_PRIMITIVE("%thread-system", thread_system, subr0, (void))
{
  return STk_intern("pthread");
}

/* ======================================================================
 * 	Initialization ...
 * ====================================================================== 
 */

int STk_init_sys_threads(vm_thread_t *vm)
{
  /* Define the key to access the thead specific VM */ 
  initialize_vm_key();
  pthread_setspecific(vm_key, vm);
  return TRUE;
}
