/*
 * mutex-common.c       -- Common Mutexes in Scheme
 *
 * Copyright Â© 2006 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 */

#include "stklos.h"
#include "vm.h"
#include "thread-common.h"
#include "mutex-common.h"

#include <unistd.h>

/* ====================================================================== *\
 *
 *                             M U T E X E S
 *
\* ====================================================================== */

SCM STk_sym_not_owned, STk_sym_abandoned, STk_sym_not_abandoned;

void STk_error_bad_mutex(SCM obj)
{
  STk_error("bad mutex ~S", obj);
}

void STk_error_deadlock(void)
{
  STk_error("cannot lock mutex (deadlock will occur)");
}

/* ====================================================================== */

/*
<doc EXT make-mutex
 * (make-mutex)
 * (make-mutex name)
 *
 * Returns a new mutex in the unlocked/not-abandoned state. The optional |name|
 * is an arbitrary Scheme object which identifies the mutex
 * (useful for debugging); it defaults to an unspecified value.
 * The mutex's specific field is set to an unspecified value.
doc>
*/
DEFINE_PRIMITIVE("make-mutex", make_mutex, subr01, (SCM name))
{
  SCM z;

  NEWCELL(z, mutex);
  MUTEX_NAME(z)     = (name ? name : STk_false);
  MUTEX_SPECIFIC(z) = STk_void;
  MUTEX_OWNER(z)    = STk_false;
  MUTEX_LOCKED(z)   = FALSE;

  STk_make_sys_mutex(z);

  return z;
}

/*
<doc EXT mutex?
 * (mutex? obj)
 *
 * Returns |#t| if obj is a mutex, otherwise returns |#f|.
doc>
*/
DEFINE_PRIMITIVE("mutex?", mutexp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(MUTEXP(obj));
}

/*
<doc EXT mutex-name
 * (mutex-name mutex)
 *
 * Returns the name of the |mutex|.
 * @lisp
 * (mutex-name (make-mutex 'foo))  =>  foo
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("mutex-name", mutex_name, subr1, (SCM mtx))
{
  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  return MUTEX_NAME(mtx);
}

/*
<doc EXT mutex-specific
 * (mutex-specific mutex)
 *
 * Returns the content of the |mutex|'s specific field.
doc>
*/
DEFINE_PRIMITIVE("mutex-specific", mutex_specific, subr1, (SCM mtx))
{
  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  return MUTEX_SPECIFIC(mtx);
}


/*
<doc EXT mutex-specific-set!
 * (mutex-specific! mutex obj)
 *
 * Stores |obj| into the |mutex|'s specific field and eturns an unspecified value.
 * @lisp
 * (define m (make-mutex))
 * (mutex-specific-set! m "hello")  =>  unspecified
 * (mutex-specific m)               =>  "hello"
 *
 * (define (mutex-lock-recursively! mutex)
 *   (if (eq? (mutex-state mutex) (current-thread))
 *       (let ((n (mutex-specific mutex)))
 *         (mutex-specific-set! mutex (+ n 1)))
 *       (begin
 *         (mutex-lock! mutex)
 *         (mutex-specific-set! mutex 0))))
 *
 * (define (mutex-unlock-recursively! mutex)
 *   (let ((n (mutex-specific mutex)))
 *     (if (= n 0)
 *         (mutex-unlock! mutex)
 *         (mutex-specific-set! mutex (- n 1)))))
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("mutex-specific-set!", mutex_specific_set, subr2, (SCM mtx, SCM v))
{
  if (! MUTEXP(mtx)) STk_error_bad_mutex(mtx);
  MUTEX_SPECIFIC(mtx) = v;
  return STk_void;
}

/* ====================================================================== *\
 *
 *                             C O N D   V A R S
 *
\* ====================================================================== */


void STk_error_bad_condv(SCM obj)
{
  STk_error("bad condition variable ~S", obj);
}


/*
<doc EXT make-condition-variable
 * (make-conditon-variable)
 * (make-conditon-variable name)
 *
 * Returns a new empty condition variable. The optional |name| is an arbitrary
 * Scheme object which identifies the condition variable (useful for debugging);
 * it defaults to an unspecified value. The condition variable's specific
 * field is set to an unspecified value.
doc>
*/
DEFINE_PRIMITIVE("make-condition-variable", make_condv, subr01, (SCM name))
{
  SCM z;

  NEWCELL(z, condv);
  CONDV_NAME(z)     = (name ? name : STk_false);
  CONDV_SPECIFIC(z) = STk_void;

  STk_make_sys_condv(z);

  return z;
}


/*
<doc EXT condition-variable?
 * (conditon-variable? obj)
 *
 * Returns |#t| if |obj| is a condition variable, otherwise returns |#f|.
doc>
*/
DEFINE_PRIMITIVE("condition-variable?", condvp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(CONDVP(obj));
}

/*
<doc EXT condition-variable-name
 * (conditon-variable-name conditon-variable)
 *
 * Returns the name of the |condition-variable|.
doc>
*/
DEFINE_PRIMITIVE("condition-variable-name", condv_name, subr1, (SCM cv))
{
  if (! CONDVP(cv)) STk_error_bad_condv(cv);
  return CONDV_NAME(cv);
}


/*
<doc EXT condition-variable-specific
 * (conditon-variable-specific conditon-variable)
 *
 * Returns the content of the |condition-variable|'s specific field.
doc>
*/
DEFINE_PRIMITIVE("condition-variable-specific", condv_specific, subr1, (SCM cv))
{
  if (! CONDVP(cv)) STk_error_bad_condv(cv);
  return CONDV_SPECIFIC(cv);
}


/*
<doc EXT condition-variable-specific-set!
 * (conditon-variable-specific-set! conditon-variable obj)
 *
 * Stores |obj| into the |condition-variable|'s specific field.
doc>
*/
DEFINE_PRIMITIVE("condition-variable-specific-set!", condv_specific_set, subr2,
                 (SCM cv, SCM v))
{
  if (! CONDVP(cv)) STk_error_bad_condv(cv);
  CONDV_SPECIFIC(cv) = v;
  return STk_void;
}

/* ====================================================================== *\
 *      Initialization ...
\* ====================================================================== */

static void print_mutex(SCM mutex, SCM port, int _UNUSED(mode))
{
  STk_puts("#[mutex ", port);
  STk_print(MUTEX_NAME(mutex), port, DSP_MODE);
  STk_putc(']', port);
}

static void print_condv(SCM condv, SCM port, int _UNUSED(mode))
{
  STk_puts("#[condition-variable ", port);
  STk_print(CONDV_NAME(condv), port, DSP_MODE);
  STk_putc(']', port);
}


/* The stucture which describes the mutex type */
static struct extended_type_descr xtype_mutex = {
  .name  = "mutex",
  .print = print_mutex
};

/* The stucture which describes the condv type */
static struct extended_type_descr xtype_condv = {
  .name  = "condv",
  .print = print_condv
};


int STk_init_mutexes(void)
{
  /* Define some symbols */
  STk_sym_not_owned     = STk_intern("not-owned");
  STk_sym_abandoned     = STk_intern("abandoned");
  STk_sym_not_abandoned = STk_intern("not-abandoned");

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
