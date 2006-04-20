/*
 * thread-lurc.h	-- Thread support for STklos
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
#ifndef _STK_THREAD_LURC_H
#define _STK_THREAD_LURC_H

#include <lurc.h>

struct thread_obj_specific {
  lurc_thread_t lthread;
  lurc_signal_t term_sig; // emit to terminate this thread
  lurc_signal_t death_sig; // emitted on thread death
};


#define THREAD_LTHREAD(p)	(((struct thread_obj *) (p))->sys_thread.lthread)
#define THREAD_TERM_SIG(p)	(((struct thread_obj *) (p))->sys_thread.term_sig)
#define THREAD_DEATH_SIG(p)	(((struct thread_obj *) (p))->sys_thread.death_sig)

extern struct timeval lthr_abs_time_to_rel_time(double abs_secs);

extern void STk_thread_start_specific(SCM thr);
extern int STk_init_threads_specific(vm_thread_t *vm);

EXTERN_PRIMITIVE("thread-yield!", thread_yield, subr0, (void));
EXTERN_PRIMITIVE("thread-terminate!", thread_terminate, subr1, (SCM thr));
EXTERN_PRIMITIVE("%thread-join!", thread_join, subr2, (SCM thr, SCM tm));
EXTERN_PRIMITIVE("%thread-sleep!", thread_sleep, subr1, (SCM tm));
EXTERN_PRIMITIVE("%thread-system", thread_system, subr0, (void));


#endif /* ! _STK_THREAD_LURC_H */
