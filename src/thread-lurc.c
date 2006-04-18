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


#define _REENTRANT 1
#define GC_LURC_THREADS 1
#define _BSD_SOURCE // for timersub

#include <lurc.h>

#ifdef LURC_ENABLE_PTHREAD
#error LURC pthreads not supported yet
#endif

#include <unistd.h>
#include <string.h>
#include <time.h>
#include "stklos.h"
#include "vm.h"
#include "thread-lurc.h"

SCM STk_primordial_thread = NULL;
static SCM primordial;
static SCM cond_thread_terminated, cond_join_timeout, cond_thread_abandonned_mutex;
static SCM all_threads = STk_nil;


static void error_bad_thread(SCM obj)
{
  STk_error("bad thread ~S", obj);
}


/*
 * Thread specific value (the VM)
 */
static pthread_key_t vm_key;

static void cleanup_vm_specific(void *p)    /* Nothing to do for now */
{
}

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

  lurc_set_data(THREAD_VM(thr), &cleanup_vm_specific);

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

  // remove the thread from the GC list, it can now dissapear when
  // everyone has stopped referencing it
  all_threads = STk_dremq(thr, all_threads);
}



/* ====================================================================== */

static SCM do_make_thread(SCM thunk, SCM name)
{
  SCM z;
  
  NEWCELL(z, thread);
  
  THREAD_THUNK(z)     = thunk;
  THREAD_NAME(z)      = name;
  THREAD_SPECIFIC(z)  = STk_void;
  THREAD_RESULT(z)    = STk_void;
  THREAD_EXCEPTION(z) = STk_false;
  THREAD_STATE(z)     = th_new;

  // give them semi-meaningful names
  THREAD_TERM_SIG(z)  = lurc_signal("thread-term-sig");

  THREAD_DEATH_SIG(z)  = lurc_signal("thread-death-sig");
  
  all_threads = STk_cons(z, all_threads); /* For the GC */
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

  z = do_make_thread(thunk, (name ? name : STk_false));
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
  vm_thread_t *vm, *new;
  
  if (!THREADP(thr)) error_bad_thread(thr);
  if (THREAD_STATE(thr) != th_new) 
    STk_error("thread has already been started ~S", thr);

  vm  = STk_get_current_vm();
  new = STk_allocate_vm(5000);			// FIX:

  new->current_module = vm->current_module;
  new->iport          = vm->iport;
  new->oport          = vm->oport;
  new->eport          = vm->eport;
  new->parameters     = STk_copy_tree(vm->parameters);
  new->scheme_thread  = thr;
  
  THREAD_VM(thr)      = new; 
  THREAD_STATE(thr)   = th_runnable;  

  if (lurc_thread_create(&THREAD_LTHREAD(thr), NULL, 
                         &start_scheme_thread, thr))
    STk_error("cannot start thread ~S", thr);

  return thr;
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
      THREAD_EXCEPTION(thr) = STk_make_C_cond(cond_thread_terminated, 1, thr);
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

static void print_thread(SCM thread, SCM port, int mode)
{
  char *s;
  SCM name = THREAD_NAME(thread);
  
  STk_puts("#[thread ", port);
  if (name != STk_false) 
    STk_print(name, port, DSP_MODE);
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
  lurc_set_data(vm, NULL);

  /* Define the threads exceptions */
  cond_thread_terminated =  STk_defcond_type("&thread-terminated", STk_false,
					     LIST1(STk_intern("canceller")),
					     STk_STklos_module);
  cond_thread_abandonned_mutex =  STk_defcond_type("&thread-abandonned-mutex", 
						   STk_false,
						   STk_nil,
						   STk_STklos_module);
  cond_join_timeout = STk_defcond_type("&thead-join-timeout", STk_false,
				       STk_nil, STk_STklos_module);
  
  /* Wrap the main thread in a thread called "primordial" */
  primordial = do_make_thread(STk_false, STk_Cstring2string("primordial"));
  THREAD_STATE(primordial) = th_runnable;
  THREAD_VM(primordial)    = vm;
  vm->scheme_thread        = primordial;
  STk_primordial_thread    = primordial;

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
  ADD_PRIMITIVE(thread_sleep);
  ADD_PRIMITIVE(thread_system);

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
