/*
 * v m . h                              -- The STklos Virtual Machine
 *
 * Copyright © 2000-2023 Erick Gallesio <eg@stklos.net>
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
#include "stklosconf.h"

typedef struct {        /* simple wrapper around jmp_buf */
  jmp_buf j;
  sigset_t blocked;
} jbuf;


/*===========================================================================*\
 *
 *                       C O N T I N U A T I O N S
 *
\*===========================================================================*/

struct continuation_obj {
  stk_header header;
  int csize;                    /* C stack size */
  void *cstart, *cend;          /* Start and end of the C stack */
  int ssize;                    /* Scheme stack size */
  void *sstart, *send;          /* Start and end of the Scheme stack */

  jbuf state;
  int fresh;
  STk_instr *pc;                /* VM registers */
  SCM *fp;
  SCM *sp;
  SCM env;
  SCM *constants;
  SCM *handlers;
  jbuf *jb;
  void *cstack;
  void *sstack;
};

#define CONTP(k)        (BOXED_TYPE_EQ((k), tc_continuation))

SCM STk_make_continuation(void);
SCM STk_restore_cont(SCM cont, SCM val);


/*===========================================================================*\
 *
 *                              G L O B A L S
 *
\*===========================================================================*/

// The use of two arrays, rather than a struct, permit to avoid alignement
// problems (on a 64 bits machine, it would take 16 bytes by variable, instead
// of 5 with two arrays.

extern SCM **STk_global_store;    // the store for all global variables
extern uint8_t *STk_global_flags;  // Information on the store cells

#define GLOBAL_CONST     (1 << 0)    // Global is RO
#define GLOBAL_ALIAS     (1 << 2)    // Global is an alias
#define GLOBAL_RESERVED  (1 << 3)


int STk_reserve_store(void);      // -> the index where value will be stored
SCM STk_global_store_define(SCM descr, SCM v, SCM value); // Define a new variable
SCM STk_global_store_alias(SCM descr, SCM v, SCM old);    // Link v -> old

// Fast access to read/write a global variable.
// hv is the (not null) result of STk_hash_get_variable
#define vm_global_ref(hv)    (STk_global_store[INT_VAL(CDR(hv))])
#define vm_global_set(hv, v) do {                    \
        STk_global_store[INT_VAL(CDR(hv))] = v; \
    } while(0)


/*===========================================================================*\
 *
 *                      T H R E A D   S U P P O R T
 *
\*===========================================================================*/

#define MAX_VALS 8      /* static number of values      */

typedef struct {
  STk_instr *pc;        /* Program Counter              */
  SCM *fp;              /* Frame pointer                */
  SCM *sp;              /* Stack pointer                */
  SCM val;              /* Current value register       */
  SCM env;              /* Current environment register */
  SCM *constants;       /* Constants of current code    */
  SCM *handlers;        /* Exceptions handlers          */

  SCM r1, r2;           /* general registers             */

  SCM vals[MAX_VALS];   /* registers for multiple values */
  int valc;             /* # of multiple values          */

  jbuf *top_jmp_buf;
  void *start_stack;

  SCM *stack;
  int stack_len;
  SCM current_module;
  SCM iport, oport,eport; /* Standard ports */
  SCM scheme_thread;      /* Scheme associated thread   */
  SCM dynwind_stack;
} vm_thread_t;


vm_thread_t *STk_allocate_vm(int stack_size);
vm_thread_t *STk_get_current_vm(void);
