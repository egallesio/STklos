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
 * Last file update:  3-Feb-2006 10:55 (eg)
 */

#define _REENTRANT 1
#define GC_LINUX_THREADS 1
#include <pthread.h>
#include <unistd.h>
#include "stklos.h"
#include "vm.h"

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
  pthread_mutex_t mymutex;
  pthread_cond_t mycondv;
};

#define MUTEXP(p)		(BOXED_TYPE_EQ((p), tc_mutex))
#define MUTEX_NAME(p)		(((struct mutex_obj *) (p))->name)
#define MUTEX_SPECIFIC(p)	(((struct mutex_obj *) (p))->specific)
#define MUTEX_OWNER(p)		(((struct mutex_obj *) (p))->owner)
#define MUTEX_LOCKED(p)		(((struct mutex_obj *) (p))->locked)
#define MUTEX_MYMUTEX(p)	(((struct mutex_obj *) (p))->mymutex)
#define MUTEX_MYCONDV(p)	(((struct mutex_obj *) (p))->mycondv)


void error_bad_mutex(SCM obj)
{
  STk_error("bad mutex ~S", obj);
}

void mutex_finalizer(SCM mtx)
{
  STk_debug("Finalizer mutex ~S", mtx);
  pthread_mutex_destroy(&MUTEX_MYMUTEX(mtx));
  pthread_cond_destroy(&MUTEX_MYCONDV(mtx));
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
  MUTEX_OWNER(z)    = STk_void;
  MUTEX_LOCKED(z)   = FALSE;

  pthread_mutex_init(&MUTEX_MYMUTEX(z), NULL);
  pthread_cond_init(&MUTEX_MYCONDV(z), NULL);

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
  
  pthread_mutex_lock(&MUTEX_MYMUTEX(mtx));

  if (MUTEX_LOCKED(mtx))
    res = (MUTEX_OWNER(mtx) == STk_void) ? sym_not_owned : MUTEX_OWNER(mtx);
  else 
    res = (MUTEX_OWNER(mtx) == STk_void) ? sym_not_abandoned: sym_abandoned;
  
  pthread_mutex_unlock(&MUTEX_MYMUTEX(mtx));

  return res;
}


/* ====================================================================== *\
 *
 * 			       C O N D   V A R S
 * 
\* ====================================================================== */

struct condv_obj {
  stk_header header;
  SCM name;
  SCM specific;
  pthread_cond_t mycondv;
};


#define CONDVP(p)		(BOXED_TYPE_EQ((p), tc_mutex))
#define CONDV_NAME(p)		(((struct mutex_obj *) (p))->name)
#define CONDV_SPECIFIC(p)	(((struct mutex_obj *) (p))->specific)
#define CONDV_MYCONDV(p)	(((struct mutex_obj *) (p))->mycondv)


void error_bad_condv(SCM obj)
{
  STk_error("bad confdition variaable ~S", obj);
}

void condv_finalizer(SCM cv)
{
  STk_debug("Finalizer condv ~S", cv);
  pthread_cond_destroy(&CONDV_MYCONDV(cv));
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
  pthread_cond_init(&CONDV_MYCONDV(z), NULL);

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
   pthread_cond_signal(&CONDV_MYCONDV(cv));
   return STk_void;
}

DEFINE_PRIMITIVE("condition-variable-brodcast!", condv_broadcast, subr1, (SCM cv))
{
   if (! CONDVP(cv)) error_bad_condv(cv);
   pthread_cond_broadcast(&CONDV_MYCONDV(cv));
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
