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
 * Last file update: 15-Apr-2006 13:05 (eg)
 */

#include <lurc.h>

#ifdef LURC_ENABLE_PTHREAD
#error LURC pthreads not supported yet
#endif

#include <unistd.h>
#include "stklos.h"
#include "vm.h"
#include "lurc_thread.h"

static SCM sym_not_owned, sym_abandoned, sym_not_abandoned;


/* ====================================================================== *\
 *
 * 			       M U T E X E S
 * 
\* ====================================================================== */

struct mutex_obj {
  stk_header header;
  SCM name;
  SCM specific;
  SCM owner;
  int locked;
  lurc_mutex_t mymutex;
  lurc_signal_t mysignal;
};

#define MUTEXP(p)		(BOXED_TYPE_EQ((p), tc_mutex))
#define MUTEX_NAME(p)		(((struct mutex_obj *) (p))->name)
#define MUTEX_SPECIFIC(p)	(((struct mutex_obj *) (p))->specific)
#define MUTEX_OWNER(p)		(((struct mutex_obj *) (p))->owner)
#define MUTEX_LOCKED(p)		(((struct mutex_obj *) (p))->locked)
#define MUTEX_MYMUTEX(p)	(((struct mutex_obj *) (p))->mymutex)
#define MUTEX_MYSIGNAL(p)	(((struct mutex_obj *) (p))->mysignal)

typedef enum {CV_NONE, CV_ONE, CV_ALL} cv_target_t;

struct condv_obj {
  stk_header header;
  SCM name;
  SCM specific;
  lurc_signal_t mysignal;
  cv_target_t target;
  lurc_instant_t emitted;
};

#define CONDVP(p)		(BOXED_TYPE_EQ((p), tc_mutex))
#define CONDV_NAME(p)		(((struct condv_obj *) (p))->name)
#define CONDV_SPECIFIC(p)	(((struct condv_obj *) (p))->specific)
#define CONDV_MYSIGNAL(p)	(((struct condv_obj *) (p))->mysignal)
#define CONDV_TARGET(p)	        (((struct condv_obj *) (p))->target)
#define CONDV_EMITTED(p)	(((struct condv_obj *) (p))->emitted)


void error_bad_mutex(SCM obj)
{
  STk_error("bad mutex ~S", obj);
}

void error_deadlock(void)
{
  STk_error("cannot lock mutex (deadlock will occur)");
}

void error_bad_timeout(SCM tm)
{
  STk_error("bad timeout ~S", tm);
}


void mutex_finalizer(SCM mtx)
{
  lurc_mutex_destroy(&MUTEX_MYMUTEX(mtx));
  lurc_signal_destroy(&MUTEX_MYSIGNAL(mtx));
}


/* ====================================================================== */

DEFINE_PRIMITIVE("make-mutex", make_mutex, subr01, (SCM name))
{
  SCM z;

  if (name) {
    if (!STRINGP(name))
      STk_error("bad mutex name ~S", name);
  }
  else name = STk_Cstring2string("");
  
  NEWCELL(z, mutex);
  MUTEX_NAME(z)     = name;
  MUTEX_SPECIFIC(z) = STk_void;
  MUTEX_OWNER(z)    = STk_false;
  MUTEX_LOCKED(z)   = FALSE;

  lurc_mutex_init(&MUTEX_MYMUTEX(z), NULL);
  lurc_signal_init(&MUTEX_MYSIGNAL(z), NULL);

  STk_register_finalizer(z, mutex_finalizer);

  return z;
}


DEFINE_PRIMITIVE("mutex?", mutexp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(MUTEXP(obj));
}

DEFINE_PRIMITIVE("mutex-name", mutex_name, subr1, (SCM mtx))
{
  if (! MUTEXP(mtx)) error_bad_mutex(mtx);
  return MUTEX_NAME(mtx);
}

DEFINE_PRIMITIVE("mutex-specific", mutex_specific, subr1, (SCM mtx))
{
  if (! MUTEXP(mtx)) error_bad_mutex(mtx);
  return MUTEX_SPECIFIC(mtx);
}

DEFINE_PRIMITIVE("mutex-specific-set!", mutex_specific_set, subr2, (SCM mtx, SCM v))
{
  if (! MUTEXP(mtx)) error_bad_mutex(mtx);
  MUTEX_SPECIFIC(mtx) = v;
  return STk_void;
}

DEFINE_PRIMITIVE("mutex-state", mutex_state, subr1, (SCM mtx))
{
  SCM res;

  if (! MUTEXP(mtx)) error_bad_mutex(mtx);
  
  lurc_mutex_lock(&MUTEX_MYMUTEX(mtx));

  if (MUTEX_LOCKED(mtx))
    res = (MUTEX_OWNER(mtx) == STk_false) ? sym_not_owned : MUTEX_OWNER(mtx);
  else 
    res = (MUTEX_OWNER(mtx) == STk_false) ? sym_not_abandoned: sym_abandoned;
  
  lurc_mutex_unlock(&MUTEX_MYMUTEX(mtx));

  return res;
}


DEFINE_PRIMITIVE("%mutex-lock!", mutex_lock, subr3, (SCM mtx, SCM tm, SCM thread))
{
  struct timeval rel_tv;
  SCM res = STk_true;
  int did_loop = 0;

  if (! MUTEXP(mtx)) error_bad_mutex(mtx);
  if (REALP(tm)){
    // bah nothing
  }else if (!BOOLEANP(tm))
    error_bad_timeout(tm);

  if (lurc_mutex_lock(&MUTEX_MYMUTEX(mtx)) != 0)
    error_deadlock();

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

  if (! MUTEXP(mtx)) error_bad_mutex(mtx);
  if (REALP(tm)) {
    rel_tv = lthr_abs_time_to_rel_time(REAL_VAL(tm));
  }
  else if (!BOOLEANP(tm))
    error_bad_timeout(tm);
  
  if (lurc_mutex_lock(&MUTEX_MYMUTEX(mtx)) != 0)
    error_deadlock();

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


void error_bad_condv(SCM obj)
{
  STk_error("bad confdition variable ~S", obj);
}

void condv_finalizer(SCM cv)
{
  lurc_signal_destroy(&CONDV_MYSIGNAL(cv));
}


/* ====================================================================== */

DEFINE_PRIMITIVE("make-condition-variable", make_condv, subr01, (SCM name))
{
  SCM z;

  if (name) {
    if (!STRINGP(name))
      STk_error("bad condition variable name ~S", name);
  }
  else name = STk_Cstring2string("");
  
  NEWCELL(z, condv);
  CONDV_NAME(z)     = name;
  CONDV_SPECIFIC(z) = STk_void;
  CONDV_TARGET(z) = -1;
  CONDV_EMITTED(z) = CV_NONE;
  lurc_signal_init(&CONDV_MYSIGNAL(z), NULL);

  STk_register_finalizer(z, condv_finalizer);

  return z;
}


DEFINE_PRIMITIVE("condition-variable?", condvp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(CONDVP(obj));
}

DEFINE_PRIMITIVE("condition-variable-name", condv_name, subr1, (SCM cv))
{
  if (! CONDVP(cv)) error_bad_condv(cv);
  return CONDV_NAME(cv);
}

DEFINE_PRIMITIVE("condition-variable-specific", condv_specific, subr1, (SCM cv))
{
  if (! CONDVP(cv)) error_bad_condv(cv);
  return CONDV_SPECIFIC(cv);
}

DEFINE_PRIMITIVE("condition-variable-specific-set!", condv_specific_set, subr2, 
		 (SCM cv, SCM v))
{
  if (! CONDVP(cv)) error_bad_condv(cv);
  CONDV_SPECIFIC(cv) = v;
  return STk_void;
}

DEFINE_PRIMITIVE("condition-variable-signal!", condv_signal, subr1, (SCM cv))
{
   if (! CONDVP(cv)) error_bad_condv(cv);
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
   if (! CONDVP(cv)) error_bad_condv(cv);
   // find a free instant to emit
   while(lurc_instant() == CONDV_EMITTED(cv))
     lurc_pause();

   // we can now safely emit it for one person
   CONDV_EMITTED(cv) = lurc_instant();
   CONDV_TARGET(cv) = CV_ALL;
   lurc_signal_emit(&CONDV_MYSIGNAL(cv));

   return STk_void;
}


/* ====================================================================== *\
 * 	Initialization ...
\* ====================================================================== */

static void print_mutex(SCM mutex, SCM port, int mode)
{
  char *name = STRING_CHARS(MUTEX_NAME(mutex));
  
  STk_puts("#[mutex ", port);
  if (*name) 
    STk_puts(name, port);
  else
    STk_fprintf(port, "%lx", (unsigned long) mutex);
  STk_putc(']', port);
}

static void print_condv(SCM condv, SCM port, int mode)
{
  char *name = STRING_CHARS(CONDV_NAME(condv));
  
  STk_puts("#[condition-variable ", port);
  if (*name) 
    STk_puts(name, port);
  else
    STk_fprintf(port, "%lx", (unsigned long) condv);
  STk_putc(']', port);
}


/* The stucture which describes the mutex type */
static struct extended_type_descr xtype_mutex = {
  "mutex",			/* name */
  print_mutex			/* print function */
};

/* The stucture which describes the condv type */
static struct extended_type_descr xtype_condv = {
  "condv",			/* name */
  print_condv			/* print function */
};


int STk_init_mutexes(void)
{
  /* Define some symbols */
  sym_not_owned     = STk_intern("not-owned");
  sym_abandoned     = STk_intern("abandoned");
  sym_not_abandoned = STk_intern("not-abandoned");
  
  /* Mutex and condv type declarations */
  DEFINE_XTYPE(mutex, &xtype_mutex);
  DEFINE_XTYPE(condv, &xtype_condv);

  /* Mutexes primitives */
  ADD_PRIMITIVE(make_mutex);
  ADD_PRIMITIVE(mutexp);
  ADD_PRIMITIVE(mutex_name);
  ADD_PRIMITIVE(mutex_specific);
  ADD_PRIMITIVE(mutex_specific_set);
  ADD_PRIMITIVE(mutex_state);
  ADD_PRIMITIVE(mutex_lock);
  ADD_PRIMITIVE(mutex_unlock);

  /* Condv primitives */
  ADD_PRIMITIVE(make_condv);
  ADD_PRIMITIVE(condvp);
  ADD_PRIMITIVE(condv_name);
  ADD_PRIMITIVE(condv_specific);
  ADD_PRIMITIVE(condv_specific_set);
  ADD_PRIMITIVE(condv_signal);
  ADD_PRIMITIVE(condv_broadcast);
  return TRUE;
}
