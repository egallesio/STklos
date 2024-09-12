/*
 * thread-common.h      -- Thread support for STklos
 *
 * Copyright © 2006-2018 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 *    Creation date:  4-Feb-2006 11:03 (eg)
 */
#ifndef _STK_THREAD_H
#define _STK_THREAD_H

#include <math.h>    /* for isnan */
#include "stklos.h"

#if defined(THREADS_PTHREADS)
# include "thread-pthreads.h"
#else
# include "thread-none.h"
#endif

#ifndef THREADS_NONE

enum thread_state { th_new, th_runnable, th_terminated, th_blocked};

struct thread_obj {
  stk_header header;
  SCM thunk;
  SCM name;
  SCM specific;
  SCM end_result;
  SCM end_exception;
  enum thread_state state;
  int stack_stize;
  vm_thread_t *vm;
  unsigned long allocations;
  unsigned long bytes_allocated;
  struct sys_thread_obj sys_thread;
};


#define THREADP(p)                (BOXED_TYPE_EQ((p), tc_thread))
#define THREAD_THUNK(p)           (((struct thread_obj *) (p))->thunk)
#define THREAD_NAME(p)            (((struct thread_obj *) (p))->name)
#define THREAD_SPECIFIC(p)        (((struct thread_obj *) (p))->specific)
#define THREAD_RESULT(p)          (((struct thread_obj *) (p))->end_result)
#define THREAD_EXCEPTION(p)       (((struct thread_obj *) (p))->end_exception)
#define THREAD_STATE(p)           (((struct thread_obj *) (p))->state)
#define THREAD_STACK_SIZE(p)      (((struct thread_obj *) (p))->stack_stize)
#define THREAD_VM(p)              (((struct thread_obj *) (p))->vm)
#define THREAD_ALLOCATIONS(p)     (((struct thread_obj *) (p))->allocations)
#define THREAD_BYTES_ALLOCATED(p) (((struct thread_obj *) (p))->bytes_allocated)

extern void STk_error_bad_thread(SCM obj);
double STk_verify_timeout(SCM tm);

extern SCM STk_cond_thread_terminated;

#endif /* ! THREADS_NONE */

struct timeval STk_thread_abstime_to_reltime(double abs_secs);
extern SCM STk_primordial_thread;

#endif /* ! _STK_THREAD_H */
