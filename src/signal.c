/*
 *
 * s i g n a l . c			-- Signal handling
 *
 * Copyright © 1993-2010 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 10-Oct-1995 07:55
 * Last file update:  7-Nov-2010 13:16 (eg)
 *
 */

#include "stklos.h"
#include <signal.h>

struct signal_info {
  char *name;
  int value;
};


static struct signal_info signal_set[] = {
  /* Posix.1 signals */
  {"sighup", 	SIGHUP},
  {"sigint", 	SIGINT},
  {"sigquit", 	SIGQUIT},
  {"sigill", 	SIGILL},
  {"sigabrt", 	SIGABRT},
  {"sigfpe", 	SIGFPE},
  {"sigkill", 	SIGKILL},
  {"sigsegv", 	SIGSEGV},
  {"sigpipe", 	SIGPIPE},
  {"sigalrm", 	SIGALRM},
  {"sigterm", 	SIGTERM},
  {"sigusr1", 	SIGUSR1},
  {"sigusr2", 	SIGUSR2},
  {"sigchld",	SIGCHLD},
  {"sigcont", 	SIGCONT},
  {"sigstop", 	SIGSTOP},
  {"sigtstp", 	SIGTSTP},
  {"sigttin", 	SIGTTIN},
  {"sigttou",   SIGTTOU},
  /* Non Posix.1 signals stolen on Linux and Solaris */
#ifdef SIGBUS
  {"sigbus", 	SIGBUS},
#endif
#ifdef SIGPOLL
  {"sigpoll", 	SIGPOLL},
#endif
#ifdef SIGPROF
  {"sigprof", 	SIGPROF},
#endif
#ifdef SIGSYS
  {"sigsys", 	SIGSYS},
#endif
#ifdef SIGTRAP
  {"sigtrap", 	SIGTRAP},
#endif
#ifdef SIGURG
  {"sigurg", 	SIGURG},
#endif
#ifdef SIGVTALRM
  {"sigvtalrm",	SIGVTALRM},
#endif
#ifdef SIGXCPU
  {"sigxcpu", 	SIGXCPU},
#endif
#ifdef SIGXFSZ
  {"sigxfsz", 	SIGXFSZ},
#endif
#ifdef SIGIOT
  {"sigiot", 	SIGIOT},
#endif
#ifdef SIGEMT
  {"sigemt", 	SIGEMT},
#endif
#ifdef SIGSTKFLT
  {"sigstkflt", SIGSTKFLT},
#endif
#ifdef SIGIO
  {"sigio", 	SIGIO},
#endif
#ifdef SIGCLD
  {"sigcld", 	SIGCLD},
#endif
#ifdef SIGPWR
  {"sigpwr", 	SIGPWR},
#endif
#ifdef SIGINFO
  {"siginfo", 	SIGINFO},
#endif
#ifdef SIGLOST
  {"siglost", 	SIGLOST},
#endif
#ifdef SIGWINCH
  {"sigwinch", 	SIGWINCH},
#endif
  {NULL, 0}
};


static void sigint(int i)
{
  STk_signal("\n*** Interrupt ***\n");
}

static void sigsegv(int i)
{
  fprintf(stderr,
	  "Received a SIGSGV signal.\n"
	  "Try to augment stack size (--stack-size option).\n"
	  "If the problem persists, send a mail to <bugs@stklos.net>\n");
  fflush(stderr);
  _exit(1);
}

static void sighup(int i)
{
  /* FIXME: perhaps we should be more verbose */
  fprintf(stderr, "Received a SIGHUP signal.\n");
  STk_exit(0);
}


int STk_get_signal_value(SCM sig)
{
  char *s;
  struct signal_info *p;

  if (!SYMBOLP(sig)) goto Error;

  /* Search sig in the table of signal names */
  s = SYMBOL_PNAME(sig);
  for (p = signal_set; p->name; p++)
    if (strcasecmp(s, p->name) == 0) return p->value;
Error:
  STk_error("bad signal name ~S", sig);
  return 0;	/* never reached */
}


DEFINE_PRIMITIVE("%initialize-signals", initialize_signals, subr0, (void))
{
  struct sigaction sigact;

  /* ====  SIGSEGV  handler */
  sigfillset(&(sigact.sa_mask));
  sigact.sa_handler = sigsegv;
  sigact.sa_flags   = 0;
  sigaction(SIGSEGV, &sigact, NULL);

  /* ====  SIGINT  handler */
  sigfillset(&(sigact.sa_mask));
  sigact.sa_handler = sigint;
  sigact.sa_flags   = 0;
  sigaction(SIGINT, &sigact, NULL);

  /* ====  SIGHUP  handler */
  sigfillset(&(sigact.sa_mask));
  sigact.sa_handler = sighup;
#ifdef SA_RESTART
  sigact.sa_flags   = SA_RESTART;
#else
  sigact.sa_flags   = 0;
#endif
  sigaction(SIGHUP, &sigact, NULL);

  return STk_void;
}


int STk_init_signal()
{
  ADD_PRIMITIVE(initialize_signals);
  return TRUE;
}
