/*
 * mutex.c	-- Pthread Mutexes in Scheme
 * 
 * Copyright © 2006-2007 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 * Last file update:  1-Feb-2007 17:20 (eg)
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

static void lurc_error(int err){
  STk_error("Lurc error: ~S", lurc_strerror(err));
}

/* ====================================================================== *\
 *
 * 			       M U T E X E S
 * 
\* ====================================================================== */

static void mutex_finalizer(SCM mtx)
{
  // any error here is non-forwardable
  int err;
  if(MUTEX_MYMUTEX(mtx) != NULL
     && (err = lurc_mutex_destroy(&MUTEX_MYMUTEX(mtx))) != 0)
    STk_panic("Lurc error: ~S", lurc_strerror(err));
  if(MUTEX_MYSIGNAL(mtx) != NULL
     && (err = lurc_signal_destroy(&MUTEX_MYSIGNAL(mtx))) != 0)
    STk_panic("Lurc error: ~S", lurc_strerror(err));

  MUTEX_MYMUTEX(mtx) = NULL;
  MUTEX_MYSIGNAL(mtx) = NULL;
}


void STk_make_sys_mutex(SCM z)
{
  int err;
  if((err = lurc_mutex_init(&MUTEX_MYMUTEX(z), NULL)) != 0)
    lurc_error(err);
  if((err = lurc_signal_init(&MUTEX_MYSIGNAL(z), NULL)) != 0){
    lurc_mutex_destroy(&MUTEX_MYMUTEX(z));
    lurc_error(err);
  }

  STk_register_finalizer(z, mutex_finalizer);
}


DEFINE_PRIMITIVE("mutex-state", mutex_state, subr1, (SCM mtx))
{
  SCM res;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  
  if (MUTEX_LOCKED(mtx))
    res = (MUTEX_OWNER(mtx) == STk_false) ? STk_sym_not_owned : MUTEX_OWNER(mtx);
  else 
    res = (MUTEX_OWNER(mtx) == STk_false) ? STk_sym_not_abandoned: STk_sym_abandoned;
  
  return res;
}

struct mut_lock_t {
  SCM mtx;
  SCM thread;
  SCM tm;
  SCM res;
  lurc_signal_t sig_to;
  unsigned raising:1;
  unsigned timedout:1;
};

static void lock_watched_await(void *arg){
  struct mut_lock_t* ml = (struct mut_lock_t*)arg;
  int err;
  if((err = lurc_signal_await(&MUTEX_MYSIGNAL(ml->mtx))) != 0){
    ml->raising = 1;
    lurc_error(err);
  }
  // we did not timeout
  ml->timedout = 0;
}

static void lock_protected_watch(void *arg){
  struct mut_lock_t* ml = (struct mut_lock_t*)arg;
  int err;
  if((err = lurc_watch(&(ml->sig_to), &lock_watched_await, ml)) != 0){
    ml->raising = 1;
    lurc_error(err);
  }
}

static void lock_finally_destroy(void *arg){
  struct mut_lock_t* ml = (struct mut_lock_t*)arg;
  int err;
  if((err = lurc_signal_destroy(&(ml->sig_to))) != 0
     && ! ml->raising){
    ml->raising = 1;
    lurc_error(err);
  }
}

static void lock_protected_grab(void *arg){
  struct mut_lock_t* ml = (struct mut_lock_t*)arg;
  SCM mtx = ml->mtx;
  struct timeval rel_tv;
  int did_loop = 0;
  int err;

  while (MUTEX_LOCKED(mtx)) {
    if ((MUTEX_OWNER(mtx) != STk_false) &&
        (THREAD_STATE(MUTEX_OWNER(mtx)) == th_terminated)) {
      MUTEX_LOCKED(mtx) = FALSE;
      MUTEX_OWNER(mtx)  = STk_false;
      ml->res = MUTEX_OWNER(mtx);
      break;
    }
    // reset the signal ?
    if(did_loop){
      if((err = lurc_pause()) != 0)
        lurc_error(err);
    }
    did_loop = 1;
    if (ml->tm != STk_false) {
      ml->timedout = 1;
      // get a new timeout
      rel_tv = STk_thread_abstime_to_reltime(REAL_VAL(ml->tm));
      if(rel_tv.tv_sec != 0 || rel_tv.tv_usec != 0){
        ml->sig_to = lurc_timeout_signal(NULL, rel_tv);
        if(ml->sig_to == NULL)
          STk_error("Lurc cannot allocate signal");
        // await the given timeout
        if((err = lurc_protect_with(&lock_protected_watch, ml,
                                    &lock_finally_destroy, ml)) != 0){
          lurc_signal_destroy(&(ml->sig_to));
          ml->raising = 1;
          lurc_error(err);
        }
      }
      if (ml->timedout) { ml->res = STk_false; break; }
    }else{
      if((err = lurc_signal_await(&MUTEX_MYSIGNAL(mtx))) != 0){
        ml->raising = 1;
        lurc_error(err);
      }
    }
  }
  if (ml->res == STk_true) {
    /* We can lock the mutex */
    MUTEX_LOCKED(mtx) = TRUE;
    MUTEX_OWNER(mtx) = ml->thread;
  }
}


static void lock_finally_unlock(void *arg){
  struct mut_lock_t* ml = (struct mut_lock_t*)arg;
  SCM mtx = ml->mtx;
  int err;
  if((err = lurc_mutex_unlock(&MUTEX_MYMUTEX(mtx))) != 0
     && !ml->raising)
    lurc_error(err);
}

DEFINE_PRIMITIVE("%mutex-lock!", mutex_lock, subr3, (SCM mtx, SCM tm, SCM thread))
{
  struct mut_lock_t ml;
  int err;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  if (REALP(tm)){
    // bah nothing
  }else if (!BOOLEANP(tm))
    STk_error_bad_timeout(tm);

  if ((err = lurc_mutex_lock(&MUTEX_MYMUTEX(mtx))) != 0)
    lurc_error(err);

  ml.res = STk_true;
  ml.thread = thread;
  ml.tm = tm;
  ml.mtx = mtx;
  ml.raising = 0;
  if((err = lurc_protect_with(&lock_protected_grab, &ml,
                              &lock_finally_unlock, &ml)) != 0){
    lurc_mutex_unlock(&MUTEX_MYMUTEX(mtx));
    lurc_error(err);
  }

  /* Different cases for res:
   *  - The owning thread which is now terminated (a condition must be raised)
   *  - #f: we had a timeout
   *  - #t: otherwise
   */
  return ml.res;
}


struct mut_unlock_t {
  SCM mtx;
  SCM tm;
  SCM cv;
  SCM res;
  lurc_signal_t sig_to;
  unsigned raising:1;
  unsigned timedout:1;
};

static void unlock_watched_await(void *arg){
  struct mut_unlock_t* ml = (struct mut_unlock_t*)arg;
  SCM cv = ml->cv;
  int err;

  do{
    // skip the first emission if needed
    // note that this also works for loop cases
    if(CONDV_EMITTED(cv) == lurc_instant()){
      if((err = lurc_pause()) != 0){
        ml->raising = 1;
        lurc_error(err);
      }
    }
    if((err = lurc_signal_await(&CONDV_MYSIGNAL(cv))) != 0){
      ml->raising = 1;
      lurc_error(err);
    }
    // was it for us ?
  }while(CONDV_TARGET(cv) == CV_NONE);
  // it was for us, we take it
  CONDV_TARGET(cv) = CV_NONE;
  // we did not timeout
  ml->timedout = 0;
}

static void unlock_protected_watch(void *arg){
  struct mut_unlock_t* ml = (struct mut_unlock_t*)arg;
  int err;
  if((err = lurc_watch(&(ml->sig_to), &unlock_watched_await, ml)) != 0){
    ml->raising = 1;
    lurc_error(err);
  }
}

static void unlock_finally_destroy(void *arg){
  struct mut_unlock_t* ml = (struct mut_unlock_t*)arg;
  int err;
  if((err = lurc_signal_destroy(&(ml->sig_to))) != 0
     && ! ml->raising){
    ml->raising = 1;
    lurc_error(err);
  }
}

DEFINE_PRIMITIVE("%mutex-unlock!", mutex_unlock, subr3, (SCM mtx, SCM cv, SCM tm))
{
  struct mut_unlock_t ml;
  int err;

  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  if (REALP(tm)) {
  }
  else if (!BOOLEANP(tm))
    STk_error_bad_timeout(tm);
  
  if((err = lurc_mutex_lock(&MUTEX_MYMUTEX(mtx))) != 0)
    lurc_error(err);

  /* Go in the unlocked/abandonned state */
  MUTEX_LOCKED(mtx) = FALSE;
  MUTEX_OWNER(mtx)  = STk_false;

  // now try to drop it
  ml.res = STk_true;
  ml.cv = cv;
  ml.tm = tm;
  ml.mtx = mtx;
  ml.raising = 0;
  if((err = lurc_mutex_unlock(&MUTEX_MYMUTEX(mtx))) != 0)
    lurc_error(err);

  /* Signal to waiting threads */
  if((err = lurc_signal_emit(&MUTEX_MYSIGNAL(mtx))) != 0)
    lurc_error(err);

  if (ml.cv != STk_false) {
    if (ml.tm != STk_false) {
      struct timeval rel_tv = STk_thread_abstime_to_reltime(REAL_VAL(ml.tm));
      ml.timedout = 1;
      ml.sig_to = lurc_timeout_signal(NULL, rel_tv);
      if(ml.sig_to == NULL)
        STk_error("Lurc cannot allocate signal");

      // await the signal, but no longer than given timeout
      if((err = lurc_protect_with(&unlock_protected_watch, &ml,
                                  &unlock_finally_destroy, &ml)) != 0){
        lurc_signal_destroy(&(ml.sig_to));
        lurc_error(err);
      }
      if (ml.timedout) ml.res = STk_false; 
    } else {
      unlock_watched_await(&ml);
    }
  }
  
  return ml.res;
}


/* ====================================================================== *\
 *
 * 			       C O N D   V A R S
 * 
\* ====================================================================== */

static void condv_finalizer(SCM cv)
{
  int err;
  // anything here is fatal since we cannot propagate it
  if(CONDV_MYSIGNAL(cv) != NULL
     && (err = lurc_signal_destroy(&CONDV_MYSIGNAL(cv))) != 0)
    STk_panic("Lurc error: ~S", lurc_strerror(err));
  CONDV_MYSIGNAL(cv) = NULL;
}

void STk_make_sys_condv(SCM z)
{
  int err;
  CONDV_TARGET(z) = CV_NONE;
  CONDV_EMITTED(z) = -1;
  if((err = lurc_signal_init(&CONDV_MYSIGNAL(z), NULL)) != 0)
    lurc_error(err);

  STk_register_finalizer(z, condv_finalizer);
}


DEFINE_PRIMITIVE("condition-variable-signal!", condv_signal, subr1, (SCM cv))
{
  int err;
  if (! CONDVP(cv)) STk_error_bad_condv(cv);
  // find a free instant to emit
  while(lurc_instant() == CONDV_EMITTED(cv)){
    if((err = lurc_pause()) != 0)
      lurc_error(err);
  }

  // we can now safely emit it for one person
  CONDV_EMITTED(cv) = lurc_instant();
  CONDV_TARGET(cv) = CV_ONE;
  if((err = lurc_signal_emit(&CONDV_MYSIGNAL(cv))) != 0)
    lurc_error(err);
  
  return STk_void;
}

DEFINE_PRIMITIVE("condition-variable-broadcast!", condv_broadcast, subr1, (SCM cv))
{
  int err;
  if (! CONDVP(cv)) STk_error_bad_condv(cv);
  // find a free instant to emit
  while(lurc_instant() == CONDV_EMITTED(cv)){
    if((err = lurc_pause()) != 0)
      lurc_error(err);
  }
  
  // we can now safely emit it for one person
  CONDV_EMITTED(cv) = lurc_instant();
  CONDV_TARGET(cv) = CV_ALL;
  if((err = lurc_signal_emit(&CONDV_MYSIGNAL(cv))) != 0)
    lurc_error(err);
  
  return STk_void;
}
