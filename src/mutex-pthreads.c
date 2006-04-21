/*
 * mutex-pthreads.c	-- Pthread Mutexes in Scheme
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
 *    Creation date:  2-Feb-2006 21:58 (eg)
 * Last file update: 17-Apr-2006 00:00 (eg)
 */

#include <unistd.h>
#include "stklos.h"
#include "vm.h"
#include "mutex-common.h"
#include "thread-common.h"


/* ====================================================================== *\
 *
 * 			       M U T E X E S
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

  STk_register_finalizer(z, mutex_finalizer);
}

DEFINE_PRIMITIVE("mutex-state", mutex_state, subr1, (SCM mtx))
{
  SCM res;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  
  pthread_mutex_lock(&MUTEX_MYMUTEX(mtx));

  if (MUTEX_LOCKED(mtx))
    res = (MUTEX_OWNER(mtx) == STk_false) ? STk_sym_not_owned : MUTEX_OWNER(mtx);
  else 
    res = (MUTEX_OWNER(mtx) == STk_false) ? STk_sym_not_abandoned: STk_sym_abandoned;
  
  pthread_mutex_unlock(&MUTEX_MYMUTEX(mtx));

  return res;
}


DEFINE_PRIMITIVE("%mutex-lock!", mutex_lock, subr3, (SCM mtx, SCM tm, SCM thread))
{
  struct timespec ts;
  double tmd;
  SCM res = STk_true;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  if (REALP(tm)) {
    tmd = REAL_VAL(tm);
    ts.tv_sec  = (time_t) tmd;
    ts.tv_nsec = (suseconds_t) ((tmd - ts.tv_sec) * 1000000);
  }
  else if (!BOOLEANP(tm))
    STk_error_bad_timeout(tm);

  pthread_cleanup_push((void (*)(void*))mutex_finalizer, mtx);
  
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

DEFINE_PRIMITIVE("%mutex-unlock!", mutex_unlock, subr3, (SCM mtx, SCM cv, SCM tm))
{
  struct timespec ts;
  double tmd;
  SCM res = STk_true;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  if (REALP(tm)) {
    tmd = REAL_VAL(tm);
    ts.tv_sec  = (time_t) tmd;
    ts.tv_nsec = (suseconds_t) ((tmd - ts.tv_sec) * 1000000);
  }
  else if (!BOOLEANP(tm))
    STk_error_bad_timeout(tm);
  
  pthread_cleanup_push((void (*)(void*))mutex_finalizer, mtx);

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
 * 			       C O N D   V A R S
 * 
\* ====================================================================== */

static void condv_finalizer(SCM cv)
{
  pthread_cond_destroy(&CONDV_MYCONDV(cv));
}

void STk_make_sys_condv(SCM z)
{
  pthread_cond_init(&CONDV_MYCONDV(z), NULL);

  STk_register_finalizer(z, condv_finalizer);
}

DEFINE_PRIMITIVE("condition-variable-signal!", condv_signal, subr1, (SCM cv))
{
   if (! CONDVP(cv)) STk_error_bad_condv(cv);
   pthread_cond_signal(&CONDV_MYCONDV(cv));
   return STk_void;
}

DEFINE_PRIMITIVE("condition-variable-brodcast!", condv_broadcast, subr1, (SCM cv))
{
   if (! CONDVP(cv)) STk_error_bad_condv(cv);
   pthread_cond_broadcast(&CONDV_MYCONDV(cv));
   return STk_void;
}
