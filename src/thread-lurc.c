/*
 * thread-lurc.c			-- Threads support in STklos
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
 * Last file update: 16-Apr-2006 13:16 (eg)
 */


#define _BSD_SOURCE // for timersub

#include <lurc.h>

#ifdef LURC_ENABLE_PTHREAD
#error LURC pthreads not supported yet
#endif

#include <unistd.h>
#include <time.h>
#include "stklos.h"
#include "vm.h"
#include "thread-common.h"

/*
 * Thread specific value (the VM)
 */

vm_thread_t *STk_get_current_vm(void)
{
  return (vm_thread_t *) lurc_get_data();
}


/* ====================================================================== */

// this is the lurc thread entry function
static void start_scheme_thread(void *arg)
{
  volatile SCM thr = (SCM) arg;
  SCM res;

  lurc_set_data(THREAD_VM(thr), NULL);

  // we do the VM loop until we're terminated
  LURC_WATCH(&THREAD_TERM_SIG(thr)){
    res = STk_C_apply(THREAD_THUNK(thr), 0);
    if (THREAD_EXCEPTION(thr) == STk_false) {
      THREAD_RESULT(thr) = res;
    }
  }

  THREAD_STATE(thr)  = th_terminated;

  // signal the death of this thread
  lurc_signal_emit(&THREAD_DEATH_SIG(thr));

  // FIXME: abandon the mutexes ?

  // now deallocate the signals
  lurc_signal_destroy(&THREAD_TERM_SIG(thr));
  lurc_signal_destroy(&THREAD_DEATH_SIG(thr));

  STk_thread_terminate_common(thr);
}

/* ====================================================================== */

void STk_thread_start_specific(SCM thr)
{
  // give them semi-meaningful names
  THREAD_TERM_SIG(thr)  = lurc_signal("thread-term-sig");
  THREAD_DEATH_SIG(thr)  = lurc_signal("thread-death-sig");

  if (lurc_thread_create(&THREAD_LTHREAD(thr), NULL, 
                         &start_scheme_thread, thr))
    STk_error("cannot start thread ~S", thr);
}

DEFINE_PRIMITIVE("thread-yield!", thread_yield, subr0, (void))
{
  lurc_pause();
  return STk_void;
}

DEFINE_PRIMITIVE("thread-terminate!", thread_terminate, subr1, (SCM thr))
{
  if (!THREADP(thr)) error_bad_thread(thr);

  if (THREAD_STATE(thr) != th_terminated) {

    // emit its term signal
    lurc_signal_emit(&THREAD_TERM_SIG(thr));
    if (THREAD_EXCEPTION(thr) == STk_void) {
      /* Be sure to register the first canceller only!  */
      THREAD_EXCEPTION(thr) =
        STk_make_C_cond(STk_cond_thread_terminated, 1, thr);
    }
    
    /* wait for it to terminate (also works for self) */
    lurc_signal_await(&THREAD_DEATH_SIG(thr));
  }
  return STk_void;
}

struct timeval 
lthr_abs_time_to_rel_time(double abs_secs){
  struct timeval abs_tv, cur_tv, rel_tv;
  abs_tv.tv_sec  = (long) abs_secs; // trim to the second
  abs_tv.tv_usec = (long) ((abs_secs - abs_tv.tv_sec) * 1000000);
  // now deduce the current time
  gettimeofday(&cur_tv, NULL);
  timersub(&abs_tv, &cur_tv, &rel_tv);
  // is it negative ?
  if(rel_tv.tv_sec < 0 || rel_tv.tv_usec < 0){
    rel_tv.tv_sec = 0;
    rel_tv.tv_usec = 0;
  }
  // we've got it
  return rel_tv;
}

DEFINE_PRIMITIVE("%thread-join!", thread_join, subr2, (SCM thr, SCM tm))
{
  SCM res = STk_true;
  struct timeval rel_tv;

  if (!THREADP(thr)) error_bad_thread(thr);

  if (REALP(tm))
    rel_tv = lthr_abs_time_to_rel_time(REAL_VAL(tm));
  else if (!BOOLEANP(tm))
    STk_error("bad timeout ~S", tm);
  
  if(THREAD_STATE(thr) != th_terminated){
    if(tm != STk_false){
      lurc_signal_t sig_to = lurc_timeout_signal(NULL, rel_tv);
      // await its death signal, but no longer than given timeout
      LURC_PROTECT{
        LURC_WATCH(&sig_to){
          lurc_signal_await(&THREAD_DEATH_SIG(thr));
          res = STk_false;
        }
      }LURC_WITH{
        lurc_signal_destroy(&sig_to);
      }LURC_PROTECT_END;
    }else{ // just a wait
      lurc_signal_await(&THREAD_DEATH_SIG(thr));
      res = STk_false;
    }
  }else
    res = STk_false;
  return res;
}

DEFINE_PRIMITIVE("%thread-sleep!", thread_sleep, subr1, (SCM tm))
{
  struct timeval rel_tv;

  if (REALP(tm))
    rel_tv = lthr_abs_time_to_rel_time(REAL_VAL(tm));
  else
    STk_error("bad timeout ~S", tm);

  // do the sleep only if > 0
  if(rel_tv.tv_sec != 0 || rel_tv.tv_usec != 0){
    lurc_signal_t sig_to = lurc_timeout_signal(NULL, rel_tv);
    // await the given timeout
    LURC_PROTECT{
      lurc_signal_await(&sig_to);
    }LURC_WITH{
      lurc_signal_destroy(&sig_to);
    }LURC_PROTECT_END;
  }
  return STk_void;
}

DEFINE_PRIMITIVE("%thread-system", thread_system, subr0, (void))
{
  return STk_intern("lurc");
}


/* ======================================================================
 * 	Initialization ...
 * ====================================================================== 
 */

int STk_init_threads_specific(vm_thread_t *vm)
{
  /* Define the key to access the thead specific VM */ 
  lurc_set_data(vm, NULL);

  return TRUE;
}

static int ret;

// call the main stklos function in a lurc thread
lurc_cb3(stk_lmain, STk_main_t, themain, int, argc, char**, argv){
  ret = themain(argc, argv);
}

int STk_thread_main(STk_main_t themain, int argc, char **argv){
  stk_lmain_thread(NULL, NULL, themain, argc, argv);
  lurc_main();
  return ret;
}
