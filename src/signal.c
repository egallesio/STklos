/*
 *
 * s i g n a l . c          -- Signal handling
 *
 * Copyright © 1993-2022 Erick Gallesio <eg@stklos.net>
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
 * Last file update:  4-Dec-2022 21:47 (eg)
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
  {"SIGHUP",    SIGHUP},
  {"SIGINT",    SIGINT},
  {"SIGQUIT",   SIGQUIT},
  {"SIGILL",    SIGILL},
  {"SIGABRT",   SIGABRT},
  {"SIGFPE",    SIGFPE},
  {"SIGKILL",   SIGKILL},
  {"SIGSEGV",   SIGSEGV},
  {"SIGPIPE",   SIGPIPE},
  {"SIGALRM",   SIGALRM},
  {"SIGTERM",   SIGTERM},
  {"SIGUSR1",   SIGUSR1},
  {"SIGUSR2",   SIGUSR2},
  {"SIGCHLD",   SIGCHLD},
  {"SIGCONT",   SIGCONT},
  {"SIGSTOP",   SIGSTOP},
  {"SIGTSTP",   SIGTSTP},
  {"SIGTTIN",   SIGTTIN},
  {"SIGTTOU",   SIGTTOU},
  /* Non Posix.1 signals stolen on Linux and Solaris */
#ifdef SIGBUS
  {"SIGBUS",    SIGBUS},
#endif
#ifdef SIGPOLL
  {"SIGPOLL",   SIGPOLL},
#endif
#ifdef SIGPROF
  {"SIGPROF",   SIGPROF},
#endif
#ifdef SIGSYS
  {"SIGSYS",    SIGSYS},
#endif
#ifdef SIGTRAP
  {"SIGTRAP",   SIGTRAP},
#endif
#ifdef SIGURG
  {"SIGURG",    SIGURG},
#endif
#ifdef SIGVTALRM
  {"SIGVTALRM", SIGVTALRM},
#endif
#ifdef SIGXCPU
  {"SIGXCPU",   SIGXCPU},
#endif
#ifdef SIGXFSZ
  {"SIGXFSZ",   SIGXFSZ},
#endif
#ifdef SIGIOT
  {"SIGIOT",    SIGIOT},
#endif
#ifdef SIGEMT
  {"SIGEMT",    SIGEMT},
#endif
#ifdef SIGSTKFLT
  {"SIGSTKFLT", SIGSTKFLT},
#endif
#ifdef SIGIO
  {"SIGIO",     SIGIO},
#endif
#ifdef SIGCLD
  {"SIGCLD",    SIGCLD},
#endif
#ifdef SIGPWR
  {"SIGPWR",    SIGPWR},
#endif
#ifdef SIGINFO
  {"SIGINFO",   SIGINFO},
#endif
#ifdef SIGLOST
  {"SIGLOST",   SIGLOST},
#endif
#ifdef SIGWINCH
  {"SIGWINCH",  SIGWINCH},
#endif
  {NULL, 0}
};


static void sigint(int _UNUSED(i))
{
  STk_signal("\n*** Interrupt ***\n");
}

static void sigsegv(int _UNUSED(i))
{
  fprintf(stderr,
      "Received a SIGSGV signal.\n"
      "Try to augment stack size (--stack-size option).\n"
      "If the problem persists, send a mail to <bugs@stklos.net>\n");
  fflush(stderr);
  _exit(1);
}

static void sighup(int _UNUSED(i))
{
  /* FIXME: perhaps we should be more verbose */
  fprintf(stderr, "Received a SIGHUP signal.\n");
  STk_exit(0);
}

static void sigabort(int _UNUSED(i))
{
  /* GMP uses abort() whan it detects problems (mainly number too try).  Try
   * to trap SIGABRT signals, hoping that next GC will recover the memory used
   */
  STk_error("Received a SIGABRT signal.");
}

int STk_get_signal_value(SCM sig)
{
  const char *s;
  struct signal_info *p;

  if (!SYMBOLP(sig)) goto Error;

  /* Search sig in the table of signal names */
  s = SYMBOL_PNAME(sig);
  for (p = signal_set; p->name; p++)
    if (strcasecmp(s, p->name) == 0) return p->value;
Error:
  STk_error("bad signal name ~S", sig);
  return 0; /* never reached */
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

  /* ====  SIGABRT  handler */
  sigfillset(&(sigact.sa_mask));
  sigact.sa_handler = sigabort;
  sigact.sa_flags   = 0;
  sigaction(SIGABRT, &sigact, NULL);

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
