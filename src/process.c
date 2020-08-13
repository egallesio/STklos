/*
 * p r o c e s s . c            -- Access to processes from STklos
 *
 * Copyright © 1994-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
 *
 *
 * Permission to use, copy, modify, distribute,and license this
 * software and its documentation for any purpose is hereby granted,
 * provided that existing copyright notices are retained in all
 * copies and that this notice is included verbatim in any
 * distributions.  No written agreement, license, or royalty fee is
 * required for any of the authorized uses.
 * This software is provided ``AS IS'' without express or implied
 * warranty.
 *
 *            Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: ??-???-1994 ??:??
 * Last file update: 26-Jun-2020 15:13 (eg)
 *
 * Code for Win32 conributed by (Paul Anderson <paul@grammatech.com> and
 * Sarah Calvo <sarah@grammatech.com>) has been deleted for now. It should be
 * reintroduced for a Win32 port. Look at file proces.c in Win32 for that.
 *
 */

/******************************************************************************
 *
 * Process extended type definition
 *
 ******************************************************************************/

#include "stklos.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/wait.h>
#include <sys/resource.h>  
#include <signal.h>
#include "fport.h"

/*
 * Data
 */
static char *stdStreams[3] = {          /* Used for messages */
  "input",
  "output",
  "error",
};


#define MAX_PROC_NUM      40            /* (simultaneous processes) enough? */

struct process_obj {
  stk_header header;
  int pid;                      /* Process id */
  SCM streams[3];               /* standard ports for the process */
  int exited;                   /* Process is terminated */
  int exit_status;              /* Exit status of the processus */
  int waited_on;                /* non zero if the process is being
                                   waited on by a waitpid(..,..,0) */
};


#define PROCESS_PID(p)          (((struct process_obj *) (p))->pid)
#define PROCESS_STREAMS(p)      (((struct process_obj *) (p))->streams)
#define PROCESS_EXITED(p)       (((struct process_obj *) (p))->exited)
#define PROCESS_STATUS(p)       (((struct process_obj *) (p))->exit_status)
#define PROCESS_WAITED(p)       (((struct process_obj *) (p))->waited_on)
#define PROCESSP(p)             (BOXED_TYPE_EQ((p), tc_process))

static SCM all_processes = STk_nil;

#if defined(SIGCHLD) && !defined(HPUX)
#  define USE_SIGCHLD 1 /* What's the problem with HP? */
#endif

#ifdef USE_SIGCHLD
#  define PURGE_PROCESS_TABLE()                             /* Nothing to do */
#else
#  define PURGE_PROCESS_TABLE() process_terminate_handler(0)/* Simulate a SIGCHLD */
#endif

MUT_DECL(process_table_mutex)

/******************************************************************************/

static void error_bad_process(SCM proc)
{
  STk_error("bad process ~S", proc);
}


static int process_alivep(SCM process)
{
   if (PROCESS_EXITED(process))
     return FALSE;
   else if (PROCESS_WAITED(process))
     return TRUE;
   else {
     int info, res;

     /* Use waitpid to gain the info. */
     res = waitpid(PROCESS_PID(process), &info, WNOHANG);
     if (res == 0)
       /* process is still running */
       return TRUE;
     else
       if (res == PROCESS_PID(process)) {
         /* process has terminated and we must save this information */
         PROCESS_EXITED(process) = TRUE;
         PROCESS_STATUS(process) = info;
         return FALSE;
       } else {
         /* might not have found process because we've already waited for it */
         /* if so, then status has already been updated */
         return FALSE;
       }
   }
}

static void process_terminate_handler(int _UNUSED(sig)) /* called when a child dies */
{
  SCM prev, l;

  /* Delete the processes which are not alive from the global list
   * This loop may delete nobody if this the process has been deleted
   * before (a previous call to this function may have deleted more than
   * one process.
   * Note: No assumption is made on the process which has terminated;
   * we act blindly here since it does not seem that there is a POSIX way
   * to find the id of the process which died.
   */
  MUT_LOCK(process_table_mutex);
  for (prev=l=all_processes; !NULLP(l); prev=l, l=CDR(l)) {
    if (!process_alivep(CAR(l))) {
      /* Process died. delete it from the list */
      if (l == all_processes)
        all_processes = CDR(l);
      else
        CDR(prev) = CDR(l);
    }
  }
  MUT_UNLOCK(process_table_mutex);
}


static SCM make_process(void)
{
  SCM z;

  PURGE_PROCESS_TABLE();

  NEWCELL(z, process);
  PROCESS_STREAMS(z)[0] = STk_false;
  PROCESS_STREAMS(z)[1] = STk_false;
  PROCESS_STREAMS(z)[2] = STk_false;
  PROCESS_EXITED(z) = 0;
  PROCESS_STATUS(z) = 0;
  return z;
}

static void close_all_files(int pipes[3][2])
{
  int i;

  for (i = 0; i < 3; i++) {
    if (pipes[i][0] != -1) close(pipes[i][0]);
    if (pipes[i][1] != -1) close(pipes[i][1]);
  }
}

static int same_files(char* f1, char* f2)
{
  struct stat s1, s2;

  if (stat(f1, &s1) < 0) return FALSE;
  if (stat(f2, &s2) < 0) return FALSE;

  return (s1.st_dev==s2.st_dev && s1.st_ino==s2.st_ino);
}

static int is_pipe_p(SCM key)
{
  return KEYWORDP(key) && (strcmp(KEYWORD_PNAME(key), "pipe") == 0);
}


static char *maybe_redirect_input(SCM in, int pipes[3][2])
{
  char *name = "";
  int fd;

  if (STRINGP(in)) {
    /* redirection in a file */
    name = STRING_CHARS(in);
    fd = open(name, O_RDONLY);
    if (fd < 0) {
      close_all_files(pipes);
      STk_error("cannot redirect input to ~S", in);
    }
    pipes[0][0] = fd;
  }
  else if (is_pipe_p(in)) {
    /* Redirection in a pipe*/
    if (pipe(pipes[0]) < 0) {
      close_all_files(pipes);
      STk_error("cannot create pipe for input");
    }
  }
  else if (FPORTP(in)) {
    /* Redirection in an open file port */
    if (!IPORTP(in)) STk_error("port is not opened for read ~S", in);
    //pipes[0][0] = dup(PORT_FD(PORT_STREAM(in)));
    //pipes[0][0] = PORT_FD(PORT_STREAM(in));
  }
  return name;
}

static char *maybe_redirect_output(SCM out, int index, int pipes[3][2],
                                   char *input, char *output)
{
  char *name = "";
  int fd;

  if (STRINGP(out)) {
    /* redirection in a file */
    name = STRING_CHARS(out);
    if (same_files(name, input))
      STk_error("file ~S used for input and output", out);

    if (same_files(name, output))
      fd = dup(pipes[1][0]);       /* :output "file" :error "file" */
    else
      fd = open(name, O_WRONLY|O_TRUNC|O_CREAT, 0666); /* out != err */

    if (fd < 0) {
      close_all_files(pipes);
      STk_error("cannot redirect input to ~S", out);
    }
    pipes[index][0] = fd;
  }
  else if (is_pipe_p(out)) {
    /* Redirection in a pipe*/
    if (pipe(pipes[index]) < 0) {
      close_all_files(pipes);
      STk_error("cannot create pipe for output");
    }
  }
  else if (FPORTP(out)) {
    /* Redirection in an open file port */
    if (!OPORTP(out)) STk_error("port is not opened for write ~S", out);
    //    name = "output port";
    //pipes[index][0] = dup(PORT_FD(PORT_STREAM(out)));
  }
  return name;
}

/*===========================================================================*\
 *
 *              Implementation of run-process and fork for Unix
 *
\*==========================================================================*/

DEFINE_PRIMITIVE("%run-process", run_process, subr4,
                 (SCM redirections, SCM do_wait, SCM do_fork, SCM args))
{
  SCM z, l;
  char **argv;
  char *in_name, *out_name; // *err_name;
  int i, len;
  int pipes[3][2];
  SCM *redir;
  pid_t pid;

  /* Initialisations */
  for (i = 0; i < 3; i++)
    pipes[i][0] =  pipes[i][1] = -1;

  redir = VECTOR_DATA(redirections);

  /* Build an argv array for exec system call */
  len = STk_int_length(args);             /* //FIXME:  Pas traité le rsh */
  if (len < 0)
    STk_error("bad argument list ~S", args);
  argv = STk_must_malloc((len + 3) * sizeof(char *));

  for (i=0, l=args; i < len; i++, l=CDR(l)) {
    if (!STRINGP(CAR(l))) STk_error("bad string ~S", CAR(l));
    argv[i] = STRING_CHARS(CAR(l));
  }
  argv[len] = NULL;

  /* Do (eventually) redirections */
  in_name  = maybe_redirect_input (redir[0], pipes);
  out_name = maybe_redirect_output(redir[1], 1, pipes, in_name, "");
  maybe_redirect_output(redir[2], 2, pipes, in_name, out_name);

  /* Build a process object */
  z   = make_process();

  /* Fork another process */
  pid = (do_fork == STk_false) ? 0 : fork();
  switch (pid) {
    case -1:  close_all_files(pipes);
              STk_error("cannot create a new process");
              break;
    case 0:  /* CHILD */
             for(i = 0; i < 3; i++) {
               if (STRINGP(redir[i])) {
                 /* Redirection in a file */
                 dup2(pipes[i][0], i);
                 close(pipes[i][0]);
               } else if (is_pipe_p(redir[i])) {
                 /* Redirection in a pipe */
                 dup2(pipes[i][(i==0)? 0 : 1], i);
                 close(pipes[i][0]);
                 close(pipes[i][1]);
               } else if (PORTP(redir[i])) {
                 int fd= PORT_FD(PORT_STREAM(redir[i]));

                 dup2(fd, i);
                 close(fd);
               }
             }

             /* close all remaining files */
             {
               struct rlimit rl;

               if (getrlimit(RLIMIT_NOFILE, &rl) == 0) 
                 for(i = 3; i < (int) rl.rlim_cur; i++) close(i);
               else
                 STk_warning("run-process: cannot close file descriptors > 2");
             }

             /*  And then, EXEC'ing...  */
             execvp(*argv, argv);

             /* Cannot exec if we are here */
             STk_fprintf(STk_current_error_port(), "**** Cannot  exec %s!\n", *argv);
             _exit(1);
    default: /* PARENT */
             PROCESS_PID(z) = pid;
             for(i = 0; i < 3; i++) {
               if (STRINGP(redir[i]))
                 /* Redirection in a file */
                 close(pipes[i][0]);
               else if (is_pipe_p(redir[i])) {
                 /* Redirection in a pipe */
                 SCM port;
                 char buffer[100];

                 close(pipes[i][i == 0 ? 0 : 1]);
                 /* Make a new file descriptor to access the pipe */
                 sprintf(buffer, "pipe-%s-%d", stdStreams[i], pid);
                 port = (i == 0) ?
                                STk_fd2scheme_port(pipes[i][1], "w", buffer) :
                                STk_fd2scheme_port(pipes[i][0], "r", buffer);
                 if (!port) {
                   close_all_files(pipes);
                   STk_error("cannot reopen pipe %d for process %d", i, pid);
                 }
                 PROCESS_STREAMS(z)[i] = port;
               } else if (PORTP(redir[i])) {
                 /* Redirection in a port */
                 PROCESS_STREAMS(z)[i]= redir[i];
               }

               if (do_wait != STk_false) {
                 PROCESS_WAITED(z) = 1;
                 waitpid(pid, &(PROCESS_STATUS(z)), 0);
                 PROCESS_WAITED(z) = 0;
                 PROCESS_EXITED(z) = TRUE;
               }
             }
  }
  /* Chain new process in the list of all process */
  MUT_LOCK(process_table_mutex);
  all_processes = STk_cons(z, all_processes);
  MUT_UNLOCK(process_table_mutex);
  return z;
}

                /* ======================================== */

/*
<doc EXT fork
 * (fork)
 * (fork thunk)
 *
 * This procedure is a wrapper around the standard Unix |fork| system
 * call which permits to create a new (heavy) process.
 * When called without parameter, this procedure returns two times
 * (one time in the parent process and one time in the child process).
 * The value returned in the parent process is a process object
 * representing the child process and the value returned in the child
 * process is always the value |#f|.
 * When called with a parameter (which must be a thunk), the new process
 * excutes |thunk| and terminate it execution when |thunk| returns. The
 * value returned in the parent process is a process object representing
 * the child process.
doc>
*/
DEFINE_PRIMITIVE("fork", fork, subr01, (SCM thunk))
{
  SCM z;
  pid_t pid;

  if (thunk && STk_procedurep(thunk) == STk_false)
    STk_error("bad procedure", thunk);

  /* Fork another process */
  pid = fork();
  switch (pid) {
    case -1:                                                  /* ERROR */
      STk_error("cannot create a new process");
      /* FALLTHRU */
    case 0:                                                   /* CHILD */
      if (thunk) {
        STk_C_apply(thunk, 0);
        STk_exit(0);
      }
      return STk_false;
    default:                                                  /* PARENT */
      z  = make_process();
      PROCESS_PID(z) = pid;
      /* Chain new process in the list of all process */
      MUT_LOCK(process_table_mutex);
      all_processes = STk_cons(z, all_processes);
      MUT_UNLOCK(process_table_mutex);
      return z;
  }
  return STk_false; /* never reached */
}

#ifdef NOTDEF
// DEFINE_PRIMITIVE("%fork", fork, subr3, (SCM in, SCM out, SCM err))
// {
//   SCM z;
//   char *in_name, *out_name, *err_name;
//   int i;
//   int pipes[3][2];
//   pid_t pid;
//   SCM redir[3];
//
//
//   ENTER_PRIMITIVE(fork);
//
//   /* Initialisations */
//   for (i = 0; i < 3; i++)
//     pipes[i][0] =  pipes[i][1] = -1;
//
//   redir[0] = in; redir[1] = out; redir[2] = err;
//
//   /* Do (eventually) redirections */
//   in_name  = maybe_redirect_input (redir[0], pipes);
//   out_name = maybe_redirect_output(redir[1], 1, pipes, in_name, "");
//   err_name = maybe_redirect_output(redir[2], 2, pipes, in_name, out_name);
//
//   /* Fork another process */
//   pid = fork();
//   switch (pid) {
//     case -1:  close_all_files(pipes);
//            STk_error("cannot create a new process");
//     case 0:
//       /* CHILD */
//       {
//      char buffer[100], *str;
//
//      for(i = 0; i < 3; i++) {
//        if (STRINGP(redir[i])) {
//          /* Redirection in a file */
//          dup2(pipes[i][0], i);
//          close(pipes[i][0]);
//
//          str = STk_expand_file_name(STRING_CHARS(redir[i]));
//          switch (i) {
//          case 0:   /* FIXME: STk_close_port(STk_curr_iport);*/
//                    STk_curr_iport = STk_fd2scheme_port(i, "r", str);
//                    break;
//            case 1: STk_close_port(STk_curr_oport);
//                    STk_curr_oport = STk_fd2scheme_port(i, "w", str);
//                    break;
//            case 2: STk_close_port(STk_curr_eport);
//                    STk_curr_eport = STk_fd2scheme_port(i, "w", str);
//                    break;
//          }
//        } else if (is_pipe_p(redir[i])) {
//          /* Redirection in a pipe */
//          dup2(pipes[i][i==0? 0 : 1], i);
//          close(pipes[i][0]);
//          close(pipes[i][1]);
//
//          sprintf(buffer, "pipe-%s-%d", stdStreams[i], getpid());
//          switch (i) {
//            case 0: /* FIXME: STk_close_port(STk_curr_iport); */
//                    STk_curr_iport = STk_fd2scheme_port(i, "r", buffer);
//                    break;
//            case 1: STk_close_port(STk_curr_oport);
//                    STk_curr_oport = STk_fd2scheme_port(i, "w", buffer);
//                    break;
//            case 2: STk_close_port(STk_curr_eport);
//                    STk_curr_eport = STk_fd2scheme_port(i, "w", buffer);
//                    break;
//          }
//        }
//      }
//
//      /* close all remaining files */
//      for(i = 3; i < NOFILE; i++) close(i);
//      return STk_false;
//       }
//     default:
//       {
//      /* PARENT */
//      z  = make_process();
//      PROCESS_PID(z) = pid;
//
//      /* Chain new process in the list of all process */
//      all_processes = STk_cons(z, all_processes);
//
//      /* Do the redirections if necessary */
//      for(i = 0; i < 3; i++) {
//        if (STRINGP(redir[i]))
//          /* Redirection in a file */
//          close(pipes[i][0]);
//        else if (is_pipe_p(redir[i])) {
//          /* Redirection in a pipe */
//          char buffer[100];
//
//          switch (i) {
//            case 0:
//              dup2(pipes[i][1], 1);
//              //STk_close_port(STk_curr_oport);
//              sprintf(buffer, "pipe-%s-%d", stdStreams[1], pid);
//              STk_curr_oport = STk_fd2scheme_port(1, "w", buffer);
//              break;
//          case 1:
//          case 2: ;
//          }
//          close(pipes[i][0]);
//          close(pipes[i][1]);
//        }
//      }
//      return z;
//       }
//   }
//   return STk_false;
// }
#endif /* NOTDEF */


/*
<doc EXT process?
 * (process? obj)
 *
 * Returns |#t| if |obj| is a process , otherwise returns |#f|.
doc>
*/
DEFINE_PRIMITIVE("process?", processp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(PROCESSP(obj));
}

/*
<doc EXT process-alive?
 * (process-alive? proc)
 *
 * Returns |#t| if process |proc| is currently running, otherwise returns |#f|.
doc>
*/
DEFINE_PRIMITIVE("process-alive?", proc_alivep, subr1, (SCM proc))
{
  if (!PROCESSP(proc)) error_bad_process(proc);
  return  MAKE_BOOLEAN(process_alivep(proc));
}


/*
<doc EXT process-pid
 * (process-pid proc)
 *
 * Returns an integer which represents the Unix identification (PID) of the
 * processus.
doc>
*/
DEFINE_PRIMITIVE("process-pid", proc_pid, subr1, (SCM proc))
{
  if (!PROCESSP(proc)) error_bad_process(proc);
  return STk_long2integer((long) PROCESS_PID(proc));
}


/*
<doc EXT process-list
 * (process-list)
 *
 * Returns the list of processes which are currently running (i.e. alive).
doc>
*/
DEFINE_PRIMITIVE("process-list", proc_list, subr0, (void))
{
  PURGE_PROCESS_TABLE();
  return STk_list_copy(all_processes);
}


/*
<doc EXT process-input process-output process-error
 * (process-input proc)
 * (process-output proc)
 * (process-error proc)
 *
 * Returns the file port associated to the standard input, output or error
 * of |proc|, if it is redirected in (or to) a pipe; otherwise
 * returns |#f|. Note that the returned port is opened for reading
 * when calling |process-output| or |process-error|; it is opened
 * for writing when calling |process-input|.
doc>
*/

DEFINE_PRIMITIVE("process-input", proc_input, subr1, (SCM proc))
{
  if (!PROCESSP(proc)) error_bad_process(proc);
  return PROCESS_STREAMS(proc)[0];
}

DEFINE_PRIMITIVE("process-output", proc_output, subr1, (SCM proc))
{
  if (!PROCESSP(proc)) error_bad_process(proc);
  return PROCESS_STREAMS(proc)[1];
}

DEFINE_PRIMITIVE("process-error", proc_error, subr1, (SCM proc))
{
  if (!PROCESSP(proc)) error_bad_process(proc);
  return PROCESS_STREAMS(proc)[2];
}


/*
<doc EXT process-wait
 * (process-wait proc)
 *
 * Stops the current process (the Scheme process) until |proc| completion.
 * |Process-wait| returns |#f| when |proc| is already terminated; it returns
 * |#t| otherwise.
doc>
*/
DEFINE_PRIMITIVE("process-wait", proc_wait, subr1, (SCM proc))
{
  PURGE_PROCESS_TABLE();

  if (!PROCESSP(proc)) error_bad_process(proc);

  if (PROCESS_EXITED(proc)) return STk_false;
  else {
    int res, info;
    SCM ret_val = STk_false;

    PROCESS_WAITED(proc) = 1;
    res = waitpid(PROCESS_PID(proc), &info, 0);
    if (res == PROCESS_PID(proc)) {
      PROCESS_STATUS(proc) = info;
      ret_val =  STk_true;
    }

    PROCESS_WAITED(proc) = 0;
    PROCESS_EXITED(proc) = TRUE;
    return ret_val;
  }
}

/*
<doc EXT process-exit-status
 * (process-exit-status proc)
 *
 * Returns the exit status of |proc| if it has finished its execution;
 * returns |#f| otherwise.
doc>
*/
DEFINE_PRIMITIVE("process-exit-status", proc_xstatus, subr1, (SCM proc))
{
  int info, n, res;

  PURGE_PROCESS_TABLE();

  if (!PROCESSP(proc)) error_bad_process(proc);


  if (PROCESS_EXITED(proc)) {
#ifndef WIN32
    if (WIFSIGNALED(PROCESS_STATUS(proc)))
      n = WCOREDUMP(PROCESS_STATUS(proc));
    else
#endif
      n = WEXITSTATUS(PROCESS_STATUS(proc));
  } else {
    res = waitpid(PROCESS_PID(proc), &info, WNOHANG);
    if (res == 0) {
      /* Process is still running */
      return STk_false;
    }
    else if (res == PROCESS_PID(proc)) {
      /* Process is now terminated */
      PROCESS_EXITED(proc) = TRUE;
      PROCESS_STATUS(proc) = info;
      n = WEXITSTATUS(info);
    } else
      return STk_false;
  }
  return STk_long2integer((long) n);
}

/*
<doc EXT process-signal
 * (process-signal proc sig)
 *
 * Sends the integer signal |sig| to |proc|. Since value of |sig| is system
 * dependant, use the symbolic defined signal constants to make your program
 * independant of the running system (see ,(ref :mark "signals")).
 * The result of |process-signal| is ,(emph "void").
doc>
*/
DEFINE_PRIMITIVE("process-signal", proc_signal, subr2, (SCM proc, SCM sig))
{
  int S;
  PURGE_PROCESS_TABLE();

  if (!PROCESSP(proc)) error_bad_process(proc);
  S = STk_get_signal_value(sig);

  kill(PROCESS_PID(proc), S);
  return STk_void;
}


/******************************************************************************/
static void print_process(SCM p, SCM port, int _UNUSED(mode))
{
  char buffer[100];

  sprintf(buffer, "#<process PID=%d>", PROCESS_PID(p));
  STk_puts(buffer, port);
}



static struct extended_type_descr xtype_process = {
  "process",
  print_process
};



int STk_init_process(void)
{
  /*
   * On systems which support SIGCHLD, the processes table is cleaned up
   * as soon as a process terminate. On other systems this is done from time
   * to time to avoid a too long list of porcesses
   */
  struct sigaction sigact;

  /* Define information for process type */
  DEFINE_XTYPE(process, &xtype_process);

  /* Define the handler for process termination. */
  sigemptyset(&(sigact.sa_mask));
  sigact.sa_handler = process_terminate_handler;
  sigact.sa_flags   = SA_NOCLDSTOP;     /* Ignore SIGCHLD generated by SIGSTOP */
#ifdef SA_RESTART
  /* Thanks to Harvey J. Stein <hjstein@MATH.HUJI.AC.IL> for the fix */
  sigact.sa_flags  |= SA_RESTART;
#endif
  sigaction(SIGCHLD, &sigact, NULL);

  ADD_PRIMITIVE(run_process);
  ADD_PRIMITIVE(fork);
  ADD_PRIMITIVE(processp);
  ADD_PRIMITIVE(proc_alivep);
  ADD_PRIMITIVE(proc_pid);
  ADD_PRIMITIVE(proc_list);
  ADD_PRIMITIVE(proc_input);
  ADD_PRIMITIVE(proc_output);
  ADD_PRIMITIVE(proc_error);
  ADD_PRIMITIVE(proc_wait);
  ADD_PRIMITIVE(proc_xstatus);
  ADD_PRIMITIVE(proc_signal);
  return TRUE;
}
