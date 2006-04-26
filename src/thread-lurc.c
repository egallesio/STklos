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
 * Last file update: 26-Apr-2006 16:21 (eg)
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

static void no_vm_error(void){
  STk_error("No VM");
}

static void lurc_error(int err){
  STk_error("Lurc error: ~S", lurc_strerror(err));
}

/*
 * Thread specific value (the VM)
 */

vm_thread_t *STk_get_current_vm(void)
{
  vm_thread_t *vm = (vm_thread_t *) lurc_get_data();
  if(vm == NULL)
    no_vm_error();
  return vm;
}


/* ====================================================================== */

static void thread_finalizer(SCM thr){
  int err;

  // deallocate the signals
  if((THREAD_TERM_SIG(thr) != NULL
      && ((err = lurc_signal_destroy(&THREAD_TERM_SIG(thr))) != 0))
     || (THREAD_DEATH_SIG(thr) != NULL
         && ((err = lurc_signal_destroy(&THREAD_DEATH_SIG(thr))) != 0))){
    // any error here is fatal since we cannot raise it properly
    STk_panic("Lurc error: ~S", lurc_strerror(err));
  }
}

static void thread_watch(void *arg){
  SCM thr = (SCM) arg;
  // run the thread thunk
  SCM res = STk_C_apply(THREAD_THUNK(thr), 0);
  // only catch the result if we did not get out of apply via an exception
  if (THREAD_EXCEPTION(thr) == STk_false) {
    THREAD_RESULT(thr) = res;
  }
}

// this is the lurc thread entry function
static void start_scheme_thread(void *arg)
{
  SCM thr = (SCM) arg;
  int err;

  lurc_set_data(THREAD_VM(thr), NULL);
  
  // we do the VM loop until we're terminated
  if((err = lurc_watch(&THREAD_TERM_SIG(thr), &thread_watch, thr)) != 0){
    // this one is tricky we cannot raise, so we save it in the thread
    THREAD_EXCEPTION(thr) = STk_make_error("Lurc error: ~S", 
                                           lurc_strerror(err));
  }

  THREAD_STATE(thr)  = th_terminated;

  // signal the death of this thread
  if((err = lurc_signal_emit(&THREAD_DEATH_SIG(thr))) != 0){
    // we cannot notify any waiting threads that we have a problem,
    // this is a real panic since there's no way to forward the error.
    STk_panic("Lurc error: ~S", lurc_strerror(err));
  }

  // FIXME: abandon the mutexes ?
}

/* ====================================================================== */

void STk_do_make_sys_thread(SCM thr)
{
  lurc_signal_attr_t attr;
  int err;

  THREAD_TERM_SIG(thr) = NULL;
  THREAD_DEATH_SIG(thr) = NULL;

  // give them semi-meaningful names
  if((err = lurc_signal_attr_init(&attr)) != 0)
    lurc_error(err);
  // first signal
  if((err = lurc_signal_attr_setname(&attr, "thread-term-sig")) != 0
     || (err = lurc_signal_init(&(THREAD_TERM_SIG(thr)), &attr)) != 0){
    lurc_signal_attr_destroy(&attr);
    lurc_error(err);
  }
  // second signal
  if((err = lurc_signal_attr_setname(&attr, "thread-death-sig")) != 0
     || (err = lurc_signal_init(&(THREAD_DEATH_SIG(thr)), &attr)) != 0){
    lurc_signal_attr_destroy(&attr);
    // do not forget the first signal
    lurc_signal_destroy(&(THREAD_TERM_SIG(thr)));
    lurc_error(err);
  }
  // cleanup
  if((err = lurc_signal_attr_destroy(&attr)) != 0){
    // do not forget the signals
    lurc_signal_destroy(&(THREAD_TERM_SIG(thr)));
    lurc_signal_destroy(&(THREAD_DEATH_SIG(thr)));
    lurc_error(err);
  }

  // now the finalizer
  STk_register_finalizer(thr, thread_finalizer);
}

void STk_sys_thread_start(SCM thr)
{
  int err;

  if((err = lurc_thread_create(&THREAD_LTHREAD(thr), NULL, 
                               &start_scheme_thread, thr)) != 0){
    // do not forget the signals
    lurc_signal_destroy(&(THREAD_TERM_SIG(thr)));
    lurc_signal_destroy(&(THREAD_DEATH_SIG(thr)));
    lurc_error(err);
  }
}

DEFINE_PRIMITIVE("thread-yield!", thread_yield, subr0, (void))
{
  int err;
  if((err = lurc_pause()) != 0)
    lurc_error(err);
  return STk_void;
}

DEFINE_PRIMITIVE("thread-terminate!", thread_terminate, subr1, (SCM thr))
{
  if (!THREADP(thr)) STk_error_bad_thread(thr);

  if (THREAD_STATE(thr) != th_terminated) {
    int err;
    // emit its term signal
    if((err = lurc_signal_emit(&THREAD_TERM_SIG(thr))) != 0)
      lurc_error(err);
    if (THREAD_EXCEPTION(thr) == STk_void) {
      /* Be sure to register the first canceller only!  */
      THREAD_EXCEPTION(thr) =
        STk_make_C_cond(STk_cond_thread_terminated, 1, thr);
    }
    
    /* wait for it to terminate (also works for self) */
    if((err = lurc_signal_await(&THREAD_DEATH_SIG(thr))) != 0){
      // if we cannot terminate ourselves, we can still notify
      // if we cannot terminate someone else, we can notify
      lurc_error(err);
    }
  }
  return STk_void;
}

struct prot_wait_t {
  lurc_signal_t sig;
  SCM thr;
  SCM res;
};

static void join_watched_await(void *arg){
  struct prot_wait_t *pw = (struct prot_wait_t *)arg;
  int err;
  if((err = lurc_signal_await(&THREAD_DEATH_SIG(pw->thr))) != 0)
    lurc_error(err);
  pw->res = STk_false;
}

static void join_protected_await(void *arg){
  struct prot_wait_t *pw = (struct prot_wait_t *)arg;
  int err;
  if((err = lurc_watch(&(pw->sig), &join_watched_await, arg)) != 0)
    lurc_error(err);
}

static void join_finally_destroyer(void *arg){
  struct prot_wait_t *pw = (struct prot_wait_t *)arg;
  int err;
  if((err = lurc_signal_destroy(&(pw->sig))) != 0)
    lurc_error(err);
}

DEFINE_PRIMITIVE("%thread-join!", thread_join, subr2, (SCM thr, SCM tm))
{
  SCM res = STk_true;
  struct timeval rel_tv;

  if (!THREADP(thr)) STk_error_bad_thread(thr);

  if (REALP(tm))
    rel_tv = STk_thread_abstime_to_reltime(REAL_VAL(tm));
  else if (!BOOLEANP(tm))
    STk_error("bad timeout ~S", tm);
  
  if(THREAD_STATE(thr) != th_terminated){
    if(tm != STk_false){
      lurc_signal_t sig_to = lurc_timeout_signal(NULL, rel_tv);
      struct prot_wait_t pw;
      int err;

      if(sig_to == NULL)
        STk_error("Lurc cannot allocate signal");
      // await its death signal, but no longer than given timeout
      pw.thr = thr;
      pw.res = STk_true;
      pw.sig = sig_to;
      if((err = lurc_protect_with(&join_protected_await, &pw, 
                                  &join_finally_destroyer, &pw)) != 0){
        // try to destroy the signal first
        lurc_signal_destroy(&sig_to);
        lurc_error(err);
      }
      // take the result
      res = pw.res;
    }else{ 
      int err;
      // just a wait
      if((err = lurc_signal_await(&THREAD_DEATH_SIG(thr))) != 0)
        lurc_error(err);
      res = STk_false;
    }
  }else
    res = STk_false;
  return res;
}

static void sleep_protected_await(void *arg){
  lurc_signal_t *sig = (lurc_signal_t *)arg;
  int err;
  if((err = lurc_signal_await(sig)) != 0)
    lurc_error(err);
}

static void sleep_finally_destroyer(void *arg){
  lurc_signal_t *sig = (lurc_signal_t *)arg;
  int err;
  if((err = lurc_signal_destroy(sig)) != 0)
    lurc_error(err);
}

DEFINE_PRIMITIVE("%thread-sleep!", thread_sleep, subr1, (SCM tm))
{
  struct timeval rel_tv;

  if (REALP(tm))
    rel_tv = STk_thread_abstime_to_reltime(REAL_VAL(tm));
  else
    STk_error("bad timeout ~S", tm);

  // do the sleep only if > 0
  if(rel_tv.tv_sec != 0 || rel_tv.tv_usec != 0){
    int err;
    lurc_signal_t sig_to = lurc_timeout_signal(NULL, rel_tv);
    if(sig_to == NULL)
      STk_error("Lurc cannot allocate signal");
    // await the given timeout
    if((err = lurc_protect_with(&sleep_protected_await, &sig_to,
                                &sleep_finally_destroyer, &sig_to)) != 0){
      // try to destroy the signal first
      lurc_signal_destroy(&sig_to);
      lurc_error(err);
    }
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

int STk_init_sys_threads(vm_thread_t *vm)
{
  /* Define the key to access the thead specific VM */ 
  if(lurc_set_data(vm, NULL) != 0)
    return FALSE;

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
