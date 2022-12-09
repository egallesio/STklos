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
 * Last file update:  9-Dec-2022 19:09 (eg)
 *
 */

#include "stklos.h"
#include <signal.h>

#ifdef SA_RESTART
#  define SA_FLAGS SA_RESTART
#else
#  define SA_FLAGS 0
#endif

/* NSIG is not POSIX, but it seems to be always defined. Anyway....  */
#ifndef NSIG
#  define NSIG 65
#endif

/*
 * Definition of the signal table
 *
 * Convention: signals[i] can be
 *     - #f if signal is ignored
 *     - #t if signal is set to default
 *     - a Scheme procedure
 */
static SCM signals[NSIG];


/* ====================================================================== */
struct signal_info {
  char *name;
  int value;
};


static struct signal_info signal_names[] = {
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


static void error_bad_signal_number(SCM sig)
{
  STk_error("bad signal number ~S", sig);
}

/* ====================================================================== */
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

/* ====================================================================== */
int STk_get_signal_value(SCM sig)
{
  const char *s;
  struct signal_info *p;

  if (!SYMBOLP(sig)) goto Error;

  /* Search sig in the table of signal names */
  s = SYMBOL_PNAME(sig);
  for (p = signal_names; p->name; p++)
    if (strcasecmp(s, p->name) == 0) return p->value;
Error:
  STk_error("bad signal name ~S", sig);
  return 0; /* never reached */
}


static void exec_signal_handler(int i) // Run a Scheme procedure for signal i
{
  SCM proc = signals[i];

  if (STk_procedurep(proc)) {
    STk_C_apply(signals[i], 1, MAKE_INT(i));
  }
  else STk_panic("PROBLEM sith signal %d", i);
}


static void set_signal_handler(int sig, void(*proc)(int), int flag)
{
  struct sigaction sigact;

  sigfillset(&(sigact.sa_mask));
  sigact.sa_handler = proc;
  sigact.sa_flags   = flag;
  sigaction(sig, &sigact, NULL);
}


DEFINE_PRIMITIVE("%default-signal-handler", dflt_sighdlr, subr1, (SCM sig))
{
  long n = STk_integer_value(sig);
  if (n <= 0 || n >= NSIG)
    STk_error("bad signal number ~S", sig);

  switch (n) {
    case SIGINT:  sigint(SIGINT);    break;
    case SIGSEGV: sigsegv(SIGSEGV);  break;
    case SIGABRT: sigabort(SIGABRT); break;
    case SIGHUP:  sighup(SIGHUP);    break;
  default: STk_error("signal ~S si not managed by this handler", sig);
  }
  return STk_void;
}


#define SET_DEFAULT_HANDLER(sig) do {                           \
     signals[(sig)] = THE_PRIMITIVE(dflt_sighdlr);              \
     set_signal_handler((sig), exec_signal_handler, SA_FLAGS);  \
  } while (0);


// This primitive is called when the REPL is initialized
DEFINE_PRIMITIVE("%initialize-signals", initialize_signals, subr0, (void))
{
  SET_DEFAULT_HANDLER(SIGSEGV);
  SET_DEFAULT_HANDLER(SIGINT);
  SET_DEFAULT_HANDLER(SIGABRT);
  SET_DEFAULT_HANDLER(SIGHUP);

  return STk_void;
}


DEFINE_PRIMITIVE("set-signal-handler!", set_sig_hdlr, subr2, (SCM sig, SCM proc))
{
  void(*p)(int);
  long s = STk_integer_value(sig);

  if (s <= 0 || s >= NSIG) error_bad_signal_number(sig);

  if (BOOLEANP(proc))
    p = (proc == STk_true)? SIG_DFL: SIG_IGN;
  else {
    if (STk_procedurep(proc) == STk_false)
      STk_error("bad procedure ~S", proc);
    p = exec_signal_handler;
  }
  signals[s] = proc;
  set_signal_handler(s, p, SA_FLAGS);

  return STk_void;
}

DEFINE_PRIMITIVE("get-signal-handler", get_sig_hdlr, subr1, (SCM sig))
{
  long s = STk_integer_value(sig);

  if (s <= 0 || s >= NSIG) error_bad_signal_number(sig);
  return signals[s];
}


DEFINE_PRIMITIVE("send-signal", send_signal, subr12, (SCM sig, SCM process))
{
  long s    = STk_integer_value(sig);
  long pid = (process != NULL)? STk_integer_value(process): (long) getpid();


  if (s <= 0 || s >= NSIG) error_bad_signal_number(sig);
  if (pid == LONG_MIN)     STk_error("bad process number ~S", process);

  kill((pid_t) pid, s);
  return STk_void;
}


int STk_init_signal()
{
  // Initialize the signals table
  for (int i=0; i < NSIG; i++) {
    signals[i] = STk_true;
  }

  // Define the symbols assiocated to signal names
  for (struct signal_info *p = signal_names; p->name; p++) {
    STk_define_variable(STk_intern(p->name), MAKE_INT(p->value), STk_STklos_module);
  }

  ADD_PRIMITIVE(initialize_signals);
  ADD_PRIMITIVE(set_sig_hdlr);
  ADD_PRIMITIVE(get_sig_hdlr);
  ADD_PRIMITIVE(send_signal);

  return TRUE;
}
