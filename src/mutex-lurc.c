/*
 * mutex.c	-- Pthread Mutexes in Scheme
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
 * Last file update: 16-Apr-2006 10:51 (eg)
 */

#include <lurc.h>

#ifdef LURC_ENABLE_PTHREAD
#error LURC pthreads not supported yet
#endif

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
  lurc_mutex_destroy(&MUTEX_MYMUTEX(mtx));
  lurc_signal_destroy(&MUTEX_MYSIGNAL(mtx));
}


void STk_make_mutex_specific(SCM z)
{
  lurc_mutex_init(&MUTEX_MYMUTEX(z), NULL);
  lurc_signal_init(&MUTEX_MYSIGNAL(z), NULL);

  STk_register_finalizer(z, mutex_finalizer);
}


DEFINE_PRIMITIVE("mutex-state", mutex_state, subr1, (SCM mtx))
{
  SCM res;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  
  lurc_mutex_lock(&MUTEX_MYMUTEX(mtx));

  if (MUTEX_LOCKED(mtx))
    res = (MUTEX_OWNER(mtx) == STk_false) ? STk_sym_not_owned : MUTEX_OWNER(mtx);
  else 
    res = (MUTEX_OWNER(mtx) == STk_false) ? STk_sym_not_abandoned: STk_sym_abandoned;
  
  lurc_mutex_unlock(&MUTEX_MYMUTEX(mtx));

  return res;
}


DEFINE_PRIMITIVE("%mutex-lock!", mutex_lock, subr3, (SCM mtx, SCM tm, SCM thread))
{
  struct timeval rel_tv;
  SCM res = STk_true;
  int did_loop = 0;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  if (REALP(tm)){
    // bah nothing
  }else if (!BOOLEANP(tm))
    STk_error_bad_timeout(tm);

  if (lurc_mutex_lock(&MUTEX_MYMUTEX(mtx)) != 0)
    STk_error_deadlock();

  while (MUTEX_LOCKED(mtx)) {
    if ((MUTEX_OWNER(mtx) != STk_false) &&
 	(THREAD_STATE(MUTEX_OWNER(mtx)) == th_terminated)) {
      MUTEX_LOCKED(mtx) = FALSE;
      MUTEX_OWNER(mtx)  = STk_false;
      res = MUTEX_OWNER(mtx);
      break;
    }
    // reset the signal ?
    if(did_loop)
      lurc_pause();
    did_loop = 1;
    if (tm != STk_false) {
      char timedout = 1;
      // get a new timeout
      rel_tv = lthr_abs_time_to_rel_time(REAL_VAL(tm));
      if(rel_tv.tv_sec != 0 || rel_tv.tv_usec != 0){
        lurc_signal_t sig_to = lurc_timeout_signal(NULL, rel_tv);
        // await the given timeout
        LURC_PROTECT{
          LURC_WATCH(&sig_to){
            lurc_signal_await(&MUTEX_MYSIGNAL(mtx));
            // we did not timeout
            timedout = 0;
          }
        }LURC_WITH{
          lurc_signal_destroy(&sig_to);
        }LURC_PROTECT_END;
      }
      if (timedout) { res = STk_false; break; }
    }
    else
      lurc_signal_await(&MUTEX_MYSIGNAL(mtx));
  }
  if (res == STk_true) {
    /* We can lock the mutex */
    MUTEX_LOCKED(mtx) = TRUE;
    MUTEX_OWNER(mtx) = thread;
  }
  lurc_mutex_unlock(&MUTEX_MYMUTEX(mtx));

  /* Different cases for res:
   *  - The owning thread which is now terminated (a condition must be raised)
   *  - #f: we had a timeout
   *  - #t: otherwise
   */
  return res;
}

DEFINE_PRIMITIVE("%mutex-unlock!", mutex_unlock, subr3, (SCM mtx, SCM cv, SCM tm))
{
  SCM res = STk_true;
  struct timeval rel_tv;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  if (REALP(tm)) {
    rel_tv = lthr_abs_time_to_rel_time(REAL_VAL(tm));
  }
  else if (!BOOLEANP(tm))
    STk_error_bad_timeout(tm);
  
  if (lurc_mutex_lock(&MUTEX_MYMUTEX(mtx)) != 0)
    STk_error_deadlock();

  /* Go in the unlocked/abandonned state */
  MUTEX_LOCKED(mtx) = FALSE;
  MUTEX_OWNER(mtx)  = STk_false;
  
  /* Signal to waiting threads */
  lurc_signal_emit(&MUTEX_MYSIGNAL(mtx));
  if (cv != STk_false) {
    if (tm != STk_false) {
      char timedout = 1;
      lurc_signal_t sig_to = lurc_timeout_signal(NULL, rel_tv);
      // await the signal, but no longer than given timeout
      LURC_PROTECT{
        LURC_WATCH(&sig_to){
          do{
            // skip the first emission if needed
            // not that this also works for loop cases
            if(CONDV_EMITTED(cv) == lurc_instant())
              lurc_pause();
            lurc_signal_await(&CONDV_MYSIGNAL(cv));
            // was it for us ?
          }while(CONDV_TARGET(cv) == CV_NONE);
          // it was for us, we take it
          CONDV_TARGET(cv) = CV_NONE;
          // we did not timeout
          timedout = 0;
        }
      }LURC_WITH{
        lurc_signal_destroy(&sig_to);
      }LURC_PROTECT_END;

      if (timedout) res = STk_false; 
    } else {
      lurc_signal_await(&CONDV_MYSIGNAL(cv));
    }
  }
  lurc_mutex_unlock(&MUTEX_MYMUTEX(mtx));
  return res;
}


/* ====================================================================== *\
 *
 * 			       C O N D   V A R S
 * 
\* ====================================================================== */

static void condv_finalizer(SCM cv)
{
  lurc_signal_destroy(&CONDV_MYSIGNAL(cv));
}

void STk_make_condv_specific(SCM z)
{
  CONDV_TARGET(z) = CV_NONE;
  CONDV_EMITTED(z) = -1;
  lurc_signal_init(&CONDV_MYSIGNAL(z), NULL);

  STk_register_finalizer(z, condv_finalizer);
}


DEFINE_PRIMITIVE("condition-variable-signal!", condv_signal, subr1, (SCM cv))
{
   if (! CONDVP(cv)) STk_error_bad_condv(cv);
   // find a free instant to emit
   while(lurc_instant() == CONDV_EMITTED(cv))
     lurc_pause();

   // we can now safely emit it for one person
   CONDV_EMITTED(cv) = lurc_instant();
   CONDV_TARGET(cv) = CV_ONE;
   lurc_signal_emit(&CONDV_MYSIGNAL(cv));

   return STk_void;
}

DEFINE_PRIMITIVE("condition-variable-brodcast!", condv_broadcast, subr1, (SCM cv))
{
   if (! CONDVP(cv)) STk_error_bad_condv(cv);
   // find a free instant to emit
   while(lurc_instant() == CONDV_EMITTED(cv))
     lurc_pause();

   // we can now safely emit it for one person
   CONDV_EMITTED(cv) = lurc_instant();
   CONDV_TARGET(cv) = CV_ALL;
   lurc_signal_emit(&CONDV_MYSIGNAL(cv));

   return STk_void;
}
