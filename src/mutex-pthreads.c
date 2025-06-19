/*
 * mutex-pthreads.c     -- Pthread Mutexes in Scheme
 *
 * Copyright © 2006-2025 Erick Gallesio <eg@stklos.net>
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
 *    Creation date:  2-Feb-2006 21:58 (eg)
 */

#include "stklos.h"
#include "vm.h"
#include "mutex-common.h"
#include "thread-common.h"

#include <unistd.h>

/* ====================================================================== *\
 *
 *                             M U T E X E S
 *
\* ====================================================================== */

static void mutex_finalizer(SCM mtx)
{
  pthread_mutex_destroy(&MUTEX_MYMUTEX(mtx));
  pthread_cond_destroy(&MUTEX_MYCONDV(mtx));
}


void STk_make_sys_mutex(SCM z)
{
  pthread_mutex_init(&MUTEX_MYMUTEX(z), NULL);
  pthread_cond_init(&MUTEX_MYCONDV(z), NULL);

  STk_register_finalizer(z, (void (*)()) mutex_finalizer);
}

/*
<doc EXT mutex-state
 * (mutex-state mutex)
 *
 * Returns information about the state of the |mutex|. The possible results
 * are:
 *
 *  * a thread *T*: the mutex is in the locked/owned state and
 *    thread *T* is the owner of the mutex
 *  * the symbol *not-owned*: the mutex is in the locked/not-owned
 *    state
 *  * the symbol *abandoned*: the mutex is in the unlocked/abandoned
 *    state
 *  * the symbol *not-abandoned*: the mutex is in the
 *     unlocked/not-abandoned state
 *
 * @lisp
 * (mutex-state (make-mutex))  =>  not-abandoned
 *
 * (define (thread-alive? thread)
 *   (let ((mutex (make-mutex)))
 *     (mutex-lock! mutex #f thread)
 *     (let ((state (mutex-state mutex)))
 *       (mutex-unlock! mutex) ; avoid space leak
 *       (eq? state thread))))
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("mutex-state", mutex_state, subr1, (SCM mtx))
{
  SCM res;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);

  pthread_mutex_lock(&MUTEX_MYMUTEX(mtx));

  if (MUTEX_LOCKED(mtx) &&
      (MUTEX_OWNER(mtx) != STk_false) &&
      (THREAD_STATE(MUTEX_OWNER(mtx)) == th_terminated)) {
    /* The thread which owns this mutex is terminated => Unlock the mutex */
    MUTEX_LOCKED(mtx) = FALSE;
  }

  if (MUTEX_LOCKED(mtx))
    res = (MUTEX_OWNER(mtx) == STk_false) ? STk_sym_not_owned : MUTEX_OWNER(mtx);
  else
    res = (MUTEX_OWNER(mtx) == STk_false) ? STk_sym_not_abandoned: STk_sym_abandoned;

  pthread_mutex_unlock(&MUTEX_MYMUTEX(mtx));

  return res;
}


/*
<doc EXT mutex-lock!
 * (mutex-lock! mutex)
 * (mutex-lock! mutex timeout)
 * (mutex-lock! mutex timeout thread)
 *
 * If the |mutex| is currently locked, the current thread waits until the
 * |mutex| is unlocked, or until the timeout is reached if |timeout| is supplied.
 * If the |timeout| is reached, |mutex-lock!| returns |#f|.
 * Otherwise, the state of the mutex is changed as follows:
 *
 *  * if thread is |#f| the mutex becomes _locked/not-owned_,
 *  * otherwise, let T be thread (or the current thread if thread
 *    is not supplied),
 *  ** if T is terminated the mutex becomes _unlocked/abandoned_,
 *  ** otherwise mutex becomes _locked/owned_ with T as the owner.
 *
 * After changing the state of the mutex, an "abandoned mutex exception" is
 * raised if the mutex was unlocked/abandoned before the state change,
 * otherwise |mutex-lock!| returns |#t|.
 * @lisp
 * (define (sleep! timeout)
 *   ;; an alternate implementation of thread-sleep!
 *   (let ((m (make-mutex)))
 *   (mutex-lock! m #f #f)
 *   (mutex-lock! m timeout #f)))
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("%mutex-lock!", mutex_lock, subr3, (SCM mtx, SCM tm, SCM thread))
{
  struct timespec ts;
  volatile SCM res = STk_true;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  if (!BOOLEANP(tm)) {
    double tmd = STk_verify_timeout(tm);
    ts.tv_sec  = (time_t) tmd;
    ts.tv_nsec = (suseconds_t) ((tmd - ts.tv_sec) * 1000000);
  }

  pthread_cleanup_push((void (*)(SCM)) mutex_finalizer, mtx);

  if (pthread_mutex_lock(&MUTEX_MYMUTEX(mtx)) != 0)
    STk_error_deadlock();

  while (MUTEX_LOCKED(mtx)) {
    if ((MUTEX_OWNER(mtx) != STk_false) &&
        (THREAD_STATE(MUTEX_OWNER(mtx)) == th_terminated)) {
      MUTEX_LOCKED(mtx) = FALSE;
      MUTEX_OWNER(mtx)  = STk_false;
      res = MUTEX_OWNER(mtx);
      break;
    }
    if (tm != STk_false) {
      int n = pthread_cond_timedwait(&MUTEX_MYCONDV(mtx), &MUTEX_MYMUTEX(mtx), &ts);

      if (n == ETIMEDOUT) { res = STk_false; break; }
    }
    else
      pthread_cond_wait(&MUTEX_MYCONDV(mtx), &MUTEX_MYMUTEX(mtx));
  }
  if (res == STk_true) {
    /* We can lock the mutex */
    MUTEX_LOCKED(mtx) = TRUE;
    MUTEX_OWNER(mtx) = thread;
  }
  pthread_mutex_unlock(&MUTEX_MYMUTEX(mtx));
  pthread_cleanup_pop(0);

  /* Different cases for res:
   *  - The owning thread which is now terminated (a condition must be raised)
   *  - #f: we had a timeout
   *  - #t: otherwise
   */
  return res;
}

/*
<doc EXT mutex-unlock!
 * (mutex-unlock! mutex)
 * (mutex-unlock! mutex condition-variable)
 * (mutex-unlock! mutex condition-variable timeout)
 *
 * Unlocks the |mutex| by making it unlocked/not-abandoned. It is not an error
 * to unlock an unlocked mutex and a mutex that is owned by any thread.
 * If |condition-variable| is supplied, the current thread is blocked and
 * added to the |condition-variable| before unlocking |mutex|; the thread
 * can unblock at any time but no later than when an appropriate call to
 * |condition-variable-signal!| or |condition-variable-broadcast!| is
 * performed (see below), and no later than the timeout (if timeout is
 * supplied). If there are threads waiting to lock this mutex, the scheduler
 * selects a thread, the |mutex| becomes locked/owned or locked/not-owned,
 * and the thread is unblocked. |mutex-unlock!| returns |#f| when the
 * |timeout| is reached, otherwise it returns |#t|.
doc>
*/
DEFINE_PRIMITIVE("%mutex-unlock!", mutex_unlock, subr3, (SCM mtx, SCM cv, SCM tm))
{
  struct timespec ts;
  volatile SCM res = STk_true;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  if ((cv != STk_false) && (!CONDVP(cv))) STk_error_bad_condv(cv);

  if (!BOOLEANP(tm)) {
    double tmd = STk_verify_timeout(tm);
    ts.tv_sec  = (time_t) tmd;
    ts.tv_nsec = (suseconds_t) ((tmd - ts.tv_sec) * 1000000000);
  }

  pthread_cleanup_push((void (*)(SCM)) mutex_finalizer, mtx);

  if (pthread_mutex_lock(&MUTEX_MYMUTEX(mtx)) != 0)
    STk_error_deadlock();

  /* Go in the unlocked/abandonned state */
  MUTEX_LOCKED(mtx) = FALSE;
  MUTEX_OWNER(mtx)  = STk_false;

  /* Signal to waiting threads */
  pthread_cond_signal(&MUTEX_MYCONDV(mtx));
  if (cv != STk_false) {
    if (tm != STk_false) {
      int n = pthread_cond_timedwait(&CONDV_MYCONDV(cv), &MUTEX_MYMUTEX(mtx), &ts);

      if (n == ETIMEDOUT) res = STk_false;
    } else {
      pthread_cond_wait(&CONDV_MYCONDV(cv), &MUTEX_MYMUTEX(mtx));
    }
  }
  pthread_mutex_unlock(&MUTEX_MYMUTEX(mtx));
  pthread_cleanup_pop(0);
  return res;
}


/* ====================================================================== *\
 *
 *                             C O N D   V A R S
 *
\* ====================================================================== */

static void condv_finalizer(SCM cv)
{
  pthread_cond_destroy(&CONDV_MYCONDV(cv));
}


void STk_make_sys_condv(SCM z)
{
  pthread_cond_init(&CONDV_MYCONDV(z), NULL);
  STk_register_finalizer(z, (void (*) ()) condv_finalizer);
}


/*
<doc EXT condition-variable-signal!
 * (condition-variable-signal! condition-variable)
 *
 * If there are threads blocked on the |condition-variable|, the scheduler
 * selects a thread and unblocks it. |Condition-variable-signal!|  returns
 * an unspecified value.
doc>
*/
DEFINE_PRIMITIVE("condition-variable-signal!", condv_signal, subr1, (SCM cv))
{
   if (! CONDVP(cv)) STk_error_bad_condv(cv);
   pthread_cond_signal(&CONDV_MYCONDV(cv));
   return STk_void;
}

/*
<doc EXT condition-variable-broadcast!
 * (condition-variable-broadcast! condition-variable)
 *
 * Unblocks all the threads blocked on the |condition-variable|.
 * |Condition-variable-broadcast!| returns an unspecified value.
doc>
*/
DEFINE_PRIMITIVE("condition-variable-broadcast!", condv_broadcast, subr1, (SCM cv))
{
   if (! CONDVP(cv)) STk_error_bad_condv(cv);
   pthread_cond_broadcast(&CONDV_MYCONDV(cv));
   return STk_void;
}
