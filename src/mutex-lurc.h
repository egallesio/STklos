/*
 * mutex-lurc.h	-- Mutex support for STklos
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
 *    Creation date:  4-Feb-2006 11:03 (eg)
 * Last file update:  4-Feb-2006 11:04 (eg)
 */
#ifndef _STK_MUTEX_LURC_H
#define _STK_MUTEX_LURC_H

#include <lurc.h>

#ifdef LURC_ENABLE_PTHREAD
#error LURC pthreads not supported yet
#endif

#include "stklos.h"

/* ====================================================================== *\
 *
 * 			       M U T E X E S
 * 
\* ====================================================================== */

struct sys_mutex_obj {
  lurc_mutex_t mymutex;
  lurc_signal_t mysignal;
};

#define MUTEX_MYMUTEX(p)	(((struct mutex_obj *) (p))->sys_mutex.mymutex)
#define MUTEX_MYSIGNAL(p)	(((struct mutex_obj *) (p))->sys_mutex.mysignal)

/* ====================================================================== *\
 *
 * 			       C O N D   V A R S
 * 
\* ====================================================================== */

typedef enum {CV_NONE, CV_ONE, CV_ALL} cv_target_t;

struct sys_condv_obj {
  lurc_signal_t mysignal;
  cv_target_t target;
  lurc_instant_t emitted;
};

#define CONDV_MYSIGNAL(p)	(((struct condv_obj *) (p))->sys_condv.mysignal)
#define CONDV_TARGET(p)	        (((struct condv_obj *) (p))->sys_condv.target)
#define CONDV_EMITTED(p)	(((struct condv_obj *) (p))->sys_condv.emitted)

/* ====================================================================== */

EXTERN_PRIMITIVE("mutex-state", mutex_state, subr1, (SCM mtx));
EXTERN_PRIMITIVE("%mutex-lock!", mutex_lock, subr3, (SCM mtx, SCM tm, SCM thread));
EXTERN_PRIMITIVE("%mutex-unlock!", mutex_unlock, subr3, (SCM mtx, SCM cv, SCM tm));
EXTERN_PRIMITIVE("condition-variable-signal!", condv_signal, subr1, (SCM cv));
EXTERN_PRIMITIVE("condition-variable-brodcast!", condv_broadcast, subr1, (SCM cv));

extern void STk_make_sys_condv(SCM z);
extern void STk_make_sys_mutex(SCM z);


#endif /* ! _STK_MUTEX_LURC_H */
