/*
 * threads.c			-- Threads support in STklos
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
 * Last file update: 23-Jan-2006 14:52 (eg)
 */

#include <pthread.h>
#include "stklos.h"
#include "vm.h"


struct thread_obj {
  stk_header header;
  SCM thunk;
  SCM name;
  SCM specific;
  SCM end_result;
  SCM end_exception;
  SCM mutexes;
  SCM dynwind;
  vm_thread_t *vm;
  pthread_t pthread;
};

#define THREADP(p)		(BOXED_TYPE_EQ((p), tc_thread))
#define THREAD_THUNK(p)		(((struct thread_obj *) (p))->thunk)
#define THREAD_NAME(p)		(((struct thread_obj *) (p))->name)
#define THREAD_SPECIFIC(p)	(((struct thread_obj *) (p))->specific)
#define THREAD_RESULT(p)	(((struct thread_obj *) (p))->end_result)
#define THREAD_EXCEPTION(p)	(((struct thread_obj *) (p))->end_exception)
#define THREAD_MUTEXES(p)	(((struct thread_obj *) (p))->mutexes)
#define THREAD_DYNWIND(p)	(((struct thread_obj *) (p))->dynwind)
#define THREAD_VM(p)		(((struct thread_obj *) (p))->vm)
#define THREAD_PTHREAD(p)	(((struct thread_obj *) (p))->pthread)

static void error_bad_thread(SCM obj)
{
  STk_error("bad thread ~S", obj);
}


DEFINE_PRIMITIVE("make-thread", make_thread, subr12, (SCM thunk, SCM name))
{
  SCM z;

  if (STk_procedurep(thunk) == STk_false) 
    STk_error("bad thunk ~S", thunk);
  if (name) {
    if (!STRINGP(name))
      STk_error("bad thread name ~S", name);
  }
  else name = STk_Cstring2string("");

  NEWCELL(z, thread);
  
  THREAD_THUNK(z)     = name;
  THREAD_NAME(z)      = name;
  THREAD_SPECIFIC(z)  = STk_void;
  THREAD_RESULT(z)    = STk_void;
  THREAD_EXCEPTION(z) = STk_false;
  THREAD_MUTEXES(z)   = STk_nil;
  THREAD_DYNWIND(z)   = STk_nil;
  THREAD_VM(z)	      = NULL;
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


void *start_scheme_thread(void *arg)
{
  SCM thr = (SCM) arg;
  
  STk_debug("Démarrer la thread ~S ~S", thr, THREAD_THUNK(thr));
  exit(0);
}


DEFINE_PRIMITIVE("thread-start", thread_start, subr1, (SCM thr))
{
  if (!THREADP(thr)) error_bad_thread(thr);
  if (THREAD_VM(thr)) STk_error("thread alrady started ~S", thr);
  
  if (!pthread_create(&THREAD_PTHREAD(thr), NULL, start_scheme_thread, thr))
    STk_error("cannot start thread ~S", thr);
  return thr;
}


/* ======================================================================
 * 	Initialization ...
 * ====================================================================== 
 */

static void print_thread(SCM thread, SCM port, int mode)
{
  char *name = STRING_CHARS(THREAD_NAME(thread));
  
  STk_puts("#[thread ", port);
  if (*name) 
    STk_puts(name, port);
  else
    STk_fprintf(port, "%lx", (unsigned long) thread);
  STk_putc(']', port);
}


/* The stucture which describes the thread type */
static struct extended_type_descr xtype_thread = {
  "thread",			/* name */
  print_thread			/* print function */
};


  
int STk_init_threads(void)
{
  /* Thread Type declaration */
  DEFINE_XTYPE(thread, &xtype_thread);

  /* Thread primitives */
  ADD_PRIMITIVE(make_thread);
  ADD_PRIMITIVE(threadp);
  ADD_PRIMITIVE(thread_name);
  ADD_PRIMITIVE(thread_specific);
  ADD_PRIMITIVE(thread_specific_set);
  ADD_PRIMITIVE(thread_start);
   
  return TRUE;
}
