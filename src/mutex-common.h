/*
 * mutex-common.h       -- Mutex support for STklos
 *
 * Copyright Â© 2006-2009 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
#ifndef _STK_MUTEX_H
#define _STK_MUTEX_H

#include "stklos.h"

#if defined(THREADS_PTHREADS)
# include "mutex-pthreads.h"
#else
# include "mutex-none.h"
#endif

/* ====================================================================== *\
 *
 *                             M U T E X E S
 *
\* ====================================================================== */

struct mutex_obj {
  stk_header header;
  SCM name;
  SCM specific;
  SCM owner;
  int locked;
  struct sys_mutex_obj sys_mutex;
};

#define MUTEXP(p)               (BOXED_TYPE_EQ((p), tc_mutex))
#define MUTEX_NAME(p)           (((struct mutex_obj *) (p))->name)
#define MUTEX_SPECIFIC(p)       (((struct mutex_obj *) (p))->specific)
#define MUTEX_OWNER(p)          (((struct mutex_obj *) (p))->owner)
#define MUTEX_LOCKED(p)         (((struct mutex_obj *) (p))->locked)

/* ====================================================================== *\
 *
 *                             C O N D   V A R S
 *
\* ====================================================================== */

struct condv_obj {
  stk_header header;
  SCM name;
  SCM specific;
  struct sys_condv_obj sys_condv;
};

#define CONDVP(p)               (BOXED_TYPE_EQ((p), tc_condv))
#define CONDV_NAME(p)           (((struct condv_obj *) (p))->name)
#define CONDV_SPECIFIC(p)       (((struct condv_obj *) (p))->specific)

/* ====================================================================== */

extern SCM STk_sym_not_owned, STk_sym_abandoned, STk_sym_not_abandoned;

extern void STk_error_bad_mutex(SCM obj);
extern void STk_error_deadlock(void);

extern void STk_error_bad_condv(SCM obj);

#endif /* ! _STK_MUTEX_H */
