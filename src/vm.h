/*
 * v m . h				-- The STklos Virtual Machine
 * 
 * Copyright © 2000-2006 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *           Author: Erick Gallesio [eg@unice.fr]
 *    Creation date:  1-Mar-2000 19:51 (eg)
 * Last file update: 12-Apr-2006 15:57 (eg)
 */


/*===========================================================================*\
 * Jmp_buf management
 *
 * Things are a little bit complicated because (at least on Linux) the 
 * mask of blocked signal is not properly saved (or you have to use the 
 * non POSIX function sigsetjmp). Following code should work on any POSIX 
 * system.
\*===========================================================================*/
#include <signal.h>

typedef struct {	/* simple wrapper around jmp_buf */
  jmp_buf j;
  sigset_t blocked;
} jbuf;


/*===========================================================================*\
 * 
 * 			 C O N T I N U A T I O N S
 * 
\*===========================================================================*/

struct continuation_obj {
  stk_header header;
  int csize;			/* C stack size */
  void *cstart, *cend;		/* Start and end of the C stack */
  int ssize;			/* Scheme stack size */
  void *sstart, *send;		/* Start and end of the Scheme stack */
  
  jbuf state;
  int fresh;
  STk_instr *pc;		/* VM registers */
  SCM *fp;
  SCM *sp;
  SCM env;
  SCM *constants;
  SCM *handlers;
  jbuf *jb;
  //  void *cstack;
  //  void *sstack;
  char stacks[1];
};

#define CONTP(k) 	(BOXED_TYPE_EQ((k), tc_continuation))

SCM STk_make_continuation(void);
SCM STk_restore_continuation(SCM cont, SCM val);


/*===========================================================================*\
 * 
 *  			T H R E A D   S U P P O R T
 * 
\*===========================================================================*/

#define MAX_VALS 8	/* static number of values	*/

typedef struct {
  STk_instr *pc;	/* Program Counter		*/
  SCM *fp;		/* Frame pointer		*/
  SCM *sp;		/* Stack pointer		*/
  SCM val;		/* Current value register 	*/
  SCM env;		/* Current environment register */
  SCM *constants;	/* Constants of current code 	*/
  SCM *handlers;	/* Exceptions handlers		*/
  
  SCM r1, r2;		/* general registers		 */
  
  SCM vals[MAX_VALS];	/* registers for multiple values */
  int valc;		/* # of multiple values 	 */
    
  jbuf *top_jmp_buf;  

  SCM *stack;
  int stack_len;
  SCM current_module;
  SCM iport, oport,eport; /* Standard ports */
  SCM scheme_thread; 	  /* Scheme associated thread 	*/
  SCM parameters;	  /* Scheme dynamic environement (parameter objects) */
} vm_thread_t;


vm_thread_t *STk_allocate_vm(int stack_size);
vm_thread_t inline *STk_get_current_vm(void);

