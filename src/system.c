/*
 *
 * s y s t e m . c                              -- System relative primitives
 *
 * Copyright © 1994-2024 Erick Gallesio <eg@stklos.net>
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
 *           Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: 29-Mar-1994 10:57
 */

#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <fcntl.h>
#include <dirent.h>
#include <time.h>
#include <locale.h>
#include <math.h>
#include "stklos.h"
#include "struct.h"

#ifndef MAXBUFF
#  define MAXBUFF 1024
#endif

#ifdef WIN32
#  define TIME_DIV_CONST 10000
#else
#  define TIME_DIV_CONST 1000
#endif


static SCM exit_procs = STk_nil; /* atexit functions */
static SCM date_type, time_type;
static SCM *temp_file_prefix;

/******************************************************************************
 *
 * Utilities
 *
 ******************************************************************************/
static void error_bad_path(SCM path)
{
  STk_error("~S is a bad pathname", path);
}

static void error_bad_string(SCM path)
{
  STk_error("~S is a bad string", path);
}

static void error_bad_int(SCM n)
{
  STk_error("~S is a bad int", n);
}

static void error_bad_int_or_out_of_bounds(SCM val)
{
  STk_error("bad integer ~S (or out of range)", val);
}

static void error_win32_primitive(void)
{
  STk_error("Win32 primitive not available on this system");
}

static void error_posix(SCM obj1, SCM obj2)
{
  if (obj1 && obj2)
    STk_error("%s: ~A ~A", strerror(errno), obj1, obj2);
  else
    if (obj1)
      STk_error("%s: ~A", strerror(errno), obj1);
    else
      STk_error("%s", strerror(errno));
}

static SCM my_access(SCM path, int mode)
{
  if (!STRINGP(path)) error_bad_path(path);
  return MAKE_BOOLEAN(access(STk_expand_file_name(STRING_CHARS(path)), mode) == 0);
}

static int my_stat(SCM path, struct stat *s)
{
  if (!STRINGP(path)) error_bad_path(path);
  return stat(STk_expand_file_name(STRING_CHARS(path)), s);
}

static int my_lstat(SCM path, struct stat *s)
{
  if (!STRINGP(path)) error_bad_path(path);
  return lstat(STk_expand_file_name(STRING_CHARS(path)), s);
}



int STk_dirp(const char *path)
{
  struct stat buf;

  if (stat(path, &buf) >= 0)
    return S_ISDIR(buf.st_mode);
  return FALSE;
}



#ifdef FIXME
//EG: void STk_whence(char *exec, char *path)
//EG: {
//EG:   char *p, *q, dir[MAX_PATH_LENGTH];
//EG:   struct stat buf;
//EG:
//EG:   if (ISABSOLUTE(exec)) {
//EG:     strncpy(path, exec, MAX_PATH_LENGTH);
//EG:     return;
//EG:   }
//EG:
//EG:   /* the executable path may be specified by relative path from the cwd. */
//EG:   /* Patch suggested by Shiro Kawai <shiro@squareusa.com> */
//EG:   if (strchr(exec, DIRSEP) != NULL) {
//EG:     getcwd(dir, MAX_PATH_LENGTH);
//EG:     sprintf(dir + strlen(dir), "%c%s", DIRSEP, exec);
//EG:     absolute(dir, path);
//EG:     return;
//EG:   }
//EG:
//EG: #ifdef FREEBSD
//EG:   /* I don't understand why this is needed */
//EG:   if (access(path, X_OK) == 0) {
//EG:     stat(path, &buf);
//EG:     if (!S_ISDIR(buf.st_mode)) return;
//EG:   }
//EG: #endif
//EG:
//EG:   p = getenv("PATH");
//EG:   if (p == NULL) {
//EG:     p = "/bin:/usr/bin";
//EG:   }
//EG:
//EG:   while (*p) {
//EG:     /* Copy the stuck of path in dir */
//EG:     for (q = dir; *p && *p != PATHSEP; p++, q++) *q = *p;
//EG:     *q = '\000';
//EG:
//EG:     if (!*dir) {
//EG:       /* patch suggested by Erik Ostrom <eostrom@vesuvius.ccs.neu.edu> */
//EG:       getcwd(path, MAX_PATH_LENGTH);
//EG:       sprintf(path + strlen(path), "%c%s", DIRSEP, exec);
//EG:     }
//EG:     else
//EG:       sprintf(path, "%s%c%s", dir, DIRSEP, exec);
//EG:
//EG:     sprintf(path, "%s%c%s", dir, DIRSEP, exec);
//EG:     if (access(path, X_OK) == 0) {
//EG:       stat(path, &buf);
//EG:       if (!S_ISDIR(buf.st_mode)) return;
//EG:     }
//EG:
//EG:     /* Try next path */
//EG:     if (*p) p++;
//EG:   }
//EG:   /* Not found. Set path to "" */
//EG:   path[0] = '\0';
//EG: }
#endif

/******************************************************************************
 *
 *  SRFI 170 support
 *
 ******************************************************************************/

#define CODESET_ERRNO(x) { #x, x },

struct codeset_code STk_errno_names[] = {
#ifdef E2BIG
    CODESET_ERRNO(E2BIG)
#endif
#ifdef EACCES
    CODESET_ERRNO(EACCES)
#endif
#ifdef EADDRINUSE
    CODESET_ERRNO(EADDRINUSE)
#endif
#ifdef EADDRNOTAVAIL
    CODESET_ERRNO(EADDRNOTAVAIL)
#endif
#ifdef EAFNOSUPPORT
    CODESET_ERRNO(EAFNOSUPPORT)
#endif
#ifdef EAGAIN
    CODESET_ERRNO(EAGAIN)
#endif
#ifdef EALREADY
    CODESET_ERRNO(EALREADY)
#endif
#ifdef EBADE
    CODESET_ERRNO(EBADE)
#endif
#ifdef EBADF
    CODESET_ERRNO(EBADF)
#endif
#ifdef EBADFD
    CODESET_ERRNO(EBADFD)
#endif
#ifdef EBADMSG
    CODESET_ERRNO(EBADMSG)
#endif
#ifdef EBADR
    CODESET_ERRNO(EBADR)
#endif
#ifdef EBADRQC
    CODESET_ERRNO(EBADRQC)
#endif
#ifdef EBADSLT
    CODESET_ERRNO(EBADSLT)
#endif
#ifdef EBUSY
    CODESET_ERRNO(EBUSY)
#endif
#ifdef ECANCELED
    CODESET_ERRNO(ECANCELED)
#endif
#ifdef ECHILD
    CODESET_ERRNO(ECHILD)
#endif
#ifdef ECHRNG
    CODESET_ERRNO(ECHRNG)
#endif
#ifdef ECOMM
    CODESET_ERRNO(ECOMM)
#endif
#ifdef ECONNABORTED
    CODESET_ERRNO(ECONNABORTED)
#endif
#ifdef ECONNREFUSED
    CODESET_ERRNO(ECONNREFUSED)
#endif
#ifdef ECONNRESET
    CODESET_ERRNO(ECONNRESET)
#endif
#ifdef EDEADLK
    CODESET_ERRNO(EDEADLK)
#endif
#ifdef EDEADLOCK
    CODESET_ERRNO(EDEADLOCK)
#endif
#ifdef EDESTADDRREQ
    CODESET_ERRNO(EDESTADDRREQ)
#endif
#ifdef EDOM
    CODESET_ERRNO(EDOM)
#endif
#ifdef EDQUOT
    CODESET_ERRNO(EDQUOT)
#endif
#ifdef EEXIST
    CODESET_ERRNO(EEXIST)
#endif
#ifdef EFAULT
    CODESET_ERRNO(EFAULT)
#endif
#ifdef EFBIG
    CODESET_ERRNO(EFBIG)
#endif
#ifdef EHOSTDOWN
    CODESET_ERRNO(EHOSTDOWN)
#endif
#ifdef EHOSTUNREACH
    CODESET_ERRNO(EHOSTUNREACH)
#endif
#ifdef EHWPOISON
    CODESET_ERRNO(EHWPOISON)
#endif
#ifdef EIDRM
    CODESET_ERRNO(EIDRM)
#endif
#ifdef EILSEQ
    CODESET_ERRNO(EILSEQ)
#endif
#ifdef EINPROGRESS
    CODESET_ERRNO(EINPROGRESS)
#endif
#ifdef EINTR
    CODESET_ERRNO(EINTR)
#endif
#ifdef EINVAL
    CODESET_ERRNO(EINVAL)
#endif
#ifdef EIO
    CODESET_ERRNO(EIO)
#endif
#ifdef EISCONN
    CODESET_ERRNO(EISCONN)
#endif
#ifdef EISDIR
    CODESET_ERRNO(EISDIR)
#endif
#ifdef EISNAM
    CODESET_ERRNO(EISNAM)
#endif
#ifdef EKEYEXPIRED
    CODESET_ERRNO(EKEYEXPIRED)
#endif
#ifdef EKEYREJECTED
    CODESET_ERRNO(EKEYREJECTED)
#endif
#ifdef EKEYREVOKED
    CODESET_ERRNO(EKEYREVOKED)
#endif
#ifdef EL2HLT
    CODESET_ERRNO(EL2HLT)
#endif
#ifdef EL2NSYNC
    CODESET_ERRNO(EL2NSYNC)
#endif
#ifdef EL3HLT
    CODESET_ERRNO(EL3HLT)
#endif
#ifdef EL3RST
    CODESET_ERRNO(EL3RST)
#endif
#ifdef ELIBACC
    CODESET_ERRNO(ELIBACC)
#endif
#ifdef ELIBBAD
    CODESET_ERRNO(ELIBBAD)
#endif
#ifdef ELIBEXEC
    CODESET_ERRNO(ELIBEXEC)
#endif
#ifdef ELIBMAX
    CODESET_ERRNO(ELIBMAX)
#endif
#ifdef ELIBSCN
    CODESET_ERRNO(ELIBSCN)
#endif
#ifdef ELNRANGE
    CODESET_ERRNO(ELNRANGE)
#endif
#ifdef ELOOP
    CODESET_ERRNO(ELOOP)
#endif
#ifdef EMEDIUMTYPE
    CODESET_ERRNO(EMEDIUMTYPE)
#endif
#ifdef EMFILE
    CODESET_ERRNO(EMFILE)
#endif
#ifdef EMLINK
    CODESET_ERRNO(EMLINK)
#endif
#ifdef EMSGSIZE
    CODESET_ERRNO(EMSGSIZE)
#endif
#ifdef EMULTIHOP
    CODESET_ERRNO(EMULTIHOP)
#endif
#ifdef ENAMETOOLONG
    CODESET_ERRNO(ENAMETOOLONG)
#endif
#ifdef ENETDOWN
    CODESET_ERRNO(ENETDOWN)
#endif
#ifdef ENETRESET
    CODESET_ERRNO(ENETRESET)
#endif
#ifdef ENETUNREACH
    CODESET_ERRNO(ENETUNREACH)
#endif
#ifdef ENFILE
    CODESET_ERRNO(ENFILE)
#endif
#ifdef ENOANO
    CODESET_ERRNO(ENOANO)
#endif
#ifdef ENOBUFS
    CODESET_ERRNO(ENOBUFS)
#endif
#ifdef ENODATA
    CODESET_ERRNO(ENODATA)
#endif
#ifdef ENODEV
    CODESET_ERRNO(ENODEV)
#endif
#ifdef ENOENT
    CODESET_ERRNO(ENOENT)
#endif
#ifdef ENOEXEC
    CODESET_ERRNO(ENOEXEC)
#endif
#ifdef ENOKEY
    CODESET_ERRNO(ENOKEY)
#endif
#ifdef ENOLCK
    CODESET_ERRNO(ENOLCK)
#endif
#ifdef ENOLINK
    CODESET_ERRNO(ENOLINK)
#endif
#ifdef ENOMEDIUM
    CODESET_ERRNO(ENOMEDIUM)
#endif
#ifdef ENOMEM
    CODESET_ERRNO(ENOMEM)
#endif
#ifdef ENOMSG
    CODESET_ERRNO(ENOMSG)
#endif
#ifdef ENONET
    CODESET_ERRNO(ENONET)
#endif
#ifdef ENOPKG
    CODESET_ERRNO(ENOPKG)
#endif
#ifdef ENOPROTOOPT
    CODESET_ERRNO(ENOPROTOOPT)
#endif
#ifdef ENOSPC
    CODESET_ERRNO(ENOSPC)
#endif
#ifdef ENOSR
    CODESET_ERRNO(ENOSR)
#endif
#ifdef ENOSTR
    CODESET_ERRNO(ENOSTR)
#endif
#ifdef ENOSYS
    CODESET_ERRNO(ENOSYS)
#endif
#ifdef ENOTBLK
    CODESET_ERRNO(ENOTBLK)
#endif
#ifdef ENOTCONN
    CODESET_ERRNO(ENOTCONN)
#endif
#ifdef ENOTDIR
    CODESET_ERRNO(ENOTDIR)
#endif
#ifdef ENOTEMPTY
    CODESET_ERRNO(ENOTEMPTY)
#endif
#ifdef ENOTRECOVERABLE
    CODESET_ERRNO(ENOTRECOVERABLE)
#endif
#ifdef ENOTSOCK
    CODESET_ERRNO(ENOTSOCK)
#endif
#ifdef ENOTSUP
    CODESET_ERRNO(ENOTSUP)
#endif
#ifdef ENOTTY
    CODESET_ERRNO(ENOTTY)
#endif
#ifdef ENOTUNIQ
    CODESET_ERRNO(ENOTUNIQ)
#endif
#ifdef ENXIO
    CODESET_ERRNO(ENXIO)
#endif
#ifdef EOPNOTSUPP
    CODESET_ERRNO(EOPNOTSUPP)
#endif
#ifdef EOVERFLOW
    CODESET_ERRNO(EOVERFLOW)
#endif
#ifdef EOWNERDEAD
    CODESET_ERRNO(EOWNERDEAD)
#endif
#ifdef EPERM
    CODESET_ERRNO(EPERM)
#endif
#ifdef EPFNOSUPPORT
    CODESET_ERRNO(EPFNOSUPPORT)
#endif
#ifdef EPIPE
    CODESET_ERRNO(EPIPE)
#endif
#ifdef EPROTO
    CODESET_ERRNO(EPROTO)
#endif
#ifdef EPROTONOSUPPORT
    CODESET_ERRNO(EPROTONOSUPPORT)
#endif
#ifdef EPROTOTYPE
    CODESET_ERRNO(EPROTOTYPE)
#endif
#ifdef ERANGE
    CODESET_ERRNO(ERANGE)
#endif
#ifdef EREMCHG
    CODESET_ERRNO(EREMCHG)
#endif
#ifdef EREMOTE
    CODESET_ERRNO(EREMOTE)
#endif
#ifdef EREMOTEIO
    CODESET_ERRNO(EREMOTEIO)
#endif
#ifdef ERESTART
    CODESET_ERRNO(ERESTART)
#endif
#ifdef ERFKILL
    CODESET_ERRNO(ERFKILL)
#endif
#ifdef EROFS
    CODESET_ERRNO(EROFS)
#endif
#ifdef ESHUTDOWN
    CODESET_ERRNO(ESHUTDOWN)
#endif
#ifdef ESOCKTNOSUPPORT
    CODESET_ERRNO(ESOCKTNOSUPPORT)
#endif
#ifdef ESPIPE
    CODESET_ERRNO(ESPIPE)
#endif
#ifdef ESRCH
    CODESET_ERRNO(ESRCH)
#endif
#ifdef ESTALE
    CODESET_ERRNO(ESTALE)
#endif
#ifdef ESTRPIPE
    CODESET_ERRNO(ESTRPIPE)
#endif
#ifdef ETIME
    CODESET_ERRNO(ETIME)
#endif
#ifdef ETIMEDOUT
    CODESET_ERRNO(ETIMEDOUT)
#endif
#ifdef ETOOMANYREFS
    CODESET_ERRNO(ETOOMANYREFS)
#endif
#ifdef ETXTBSY
    CODESET_ERRNO(ETXTBSY)
#endif
#ifdef EUCLEAN
    CODESET_ERRNO(EUCLEAN)
#endif
#ifdef EUNATCH
    CODESET_ERRNO(EUNATCH)
#endif
#ifdef EUSERS
    CODESET_ERRNO(EUSERS)
#endif
#ifdef EWOULDBLOCK
    CODESET_ERRNO(EWOULDBLOCK)
#endif
#ifdef EXDEV
    CODESET_ERRNO(EXDEV)
#endif
#ifdef EXFULL
    CODESET_ERRNO(EXFULL)
#endif
    {NULL, 0}
};


static SCM get_posix_error_name (int n)
{
  /* Search in STk_errno_names. */
  for (struct codeset_code *p=STk_errno_names; p->name; p++) {
    if (p->code == n)
      return STk_intern((char *) p->name);
  }
  return STk_intern("UNKNOWN_NAME");
}


/* Raise a &posix-error message */
void STk_error_posix(int err, char *proc_name, SCM obj1, SCM obj2)
{
    SCM err_no = MAKE_INT(err);
    SCM errname = get_posix_error_name(err);
    SCM r7_msg = STk_Cstring2string(strerror(err));
    SCM procedure = (*proc_name) ? STk_intern(proc_name): STk_false;
    SCM args, message;


    /* Build the error message */
    if (obj1 && obj2) {
      message = STk_format_error("~a: ~s, ~s", r7_msg, obj1, obj2);
      args    = LIST2(obj1, obj2);
    } else if (obj1) {
      message = STk_format_error("~a: ~s", r7_msg, obj1);
      args    = LIST1(obj1);
    } else {
      message = r7_msg;
      args    = STk_nil;
    }

    STk_raise_exception(STk_make_C_cond(STk_posix_error_condition,
                                        7,
                                        procedure,    /* location  */
                                        STk_vm_bt(),  /* backtrace */
                                        message,      /* message */
                                        r7_msg,       /* r7rs-msg */
                                        args,         /* r7rs-irritants */
                                        errname,      /* errname */
                                        err_no));     /* errno */
}


/*
<doc EXT temp-file-prefix
 * (temp-file-prefix)
 * (temp-file-prefix value)
 *
 * This parameter object permits to change the default prefix used to build
 * temporary file name. Its default value is built using the |TMPDIR|
 * environment variable (if it is defined) and the current process ID.
 * If a value is provided, it must be a string designating a valid prefix path.
 * @l
 * This parameter object is also defined in {{link-srfi 170}}.
doc>
*/
static SCM init_temp_file_prefix(void)
{
  char *tmpdir = getenv("TMPDIR");
  char buffer[MAX_PATH_LENGTH];

  if (!tmpdir) tmpdir = "/tmp";
  snprintf(buffer, MAX_PATH_LENGTH, "%s/%d", tmpdir, getpid());

  return temp_file_prefix = STk_Cstring2string(buffer);
}

static SCM temp_file_prefix_conv(SCM val)
{
  if (!STRINGP(val)) error_bad_string(val);
  return (temp_file_prefix = val);
}


/******************************************************************************
 *
 * Primitives
 *
 ******************************************************************************/

/*
<doc EXT winify-file-name
 * (winify-file-name fn)
 *
 * On Win32 system, when compiled with the Cygwin environment,
 * file names are internally represented in a POSIX-like internal form.
 * |Winify-file-bame| permits to obtain back the Win32 name of an interned
 * file name
 * @lisp
 * (winify-file-name "/tmp")
 *     => "C:\\cygwin\\tmp"
 * (list (getcwd) (winify-file-name (getcwd)))
 *     => ("//saxo/homes/eg/Projects/STklos"
 *         "\\\\saxo\\homes\\eg\\Projects\\STklos")
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("winify-file-name", winify_filename, subr1, (SCM _UNUSED(f)))
{
#ifdef WIN32
  char expanded[2 * MAX_PATH_LENGTH];

  if (!STRINGP(f)) error_bad_string(f);

  cygwin_conv_to_win32_path(STRING_CHARS(f), expanded);
  return STk_Cstring2string(expanded);
#else
  error_win32_primitive();
  return STk_void;
#endif
}


/*
<doc EXT posixify-file-name
 * (posixify-file-name fn)
 *
 * On Win32 system, when compiled with the Cygwin environment,
 * file names are internally represented in a POSIX-like internal form.
 * |posixify-file-bame| permits to obtain the interned file name from
 * its external form.
 * file name
 * @lisp
 * (posixify-file-name "C:\\cygwin\\tmp")
 *        => "/tmp"
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("posixify-file-name", posixify_filename, subr1, (SCM _UNUSED(f)))
{
#ifdef WIN32
  char expanded[2 * MAX_PATH_LENGTH];

  if (!STRINGP(f)) error_bad_string(f);

  cygwin_conv_to_posix_path(STRING_CHARS(f), expanded);
  return STk_Cstring2string(expanded);
#else
  error_win32_primitive();
  return STk_void;
#endif
}


/*
<doc EXT expand-file-name
 * (expand-file-name path)
 *
 * |Expand-file-name| expands the filename given in |path| to
 * an absolute path.
 * @lisp
 *   ;; Current directory is ~eg/stklos (i.e. /users/eg/stklos)
 *   (expand-file-name "..")            => "/users/eg"
 *   (expand-file-name "~eg/../eg/bin") => "/users/eg/bin"
 *   (expand-file-name "~/stklos)"      => "/users/eg/stk"
 * @end lisp
doc>
*/

DEFINE_PRIMITIVE("expand-file-name", expand_fn, subr1, (SCM s))
{
  if (!STRINGP(s)) error_bad_string(s);
  return STk_Cstring2string(STk_expand_file_name(STRING_CHARS(s)));
}


/*
<doc EXT canonical-file-name
 * (canonical-file-name path)
 *
 * Expands all symbolic links in |path| and returns its canonicalized
 * absolute path name. The resulting path does not have symbolic links.
 * If |path| doesn't designate a valid path name, |canonical-file-name|
 * returns |#f|.
doc>
*/
DEFINE_PRIMITIVE("canonical-file-name", canonical_path, subr1, (SCM str))
{
  if (!STRINGP(str)) error_bad_string(str);
  return STk_resolve_link(STRING_CHARS(str), 0);
}

/*
<doc EXT getcwd
 * (getcwd)
 *
 * Returns a string containing the current working directory.
doc>
*/
DEFINE_PRIMITIVE("getcwd", getcwd, subr0, (void))
{
  char buf[MAX_PATH_LENGTH], *s;
  SCM z;

  s = getcwd(buf, MAX_PATH_LENGTH);
  if (!s)
    error_posix(NULL, NULL);
  z = STk_Cstring2string(buf);

  return z;
}


/*
<doc EXT chdir
 * (chdir dir)
 *
 * Changes the current directory to the directory given in string |dir|.
doc>
*/
DEFINE_PRIMITIVE("chdir", chdir, subr1, (SCM s))
{
  if (!STRINGP(s)) error_bad_path(s);

  if (chdir(STk_expand_file_name(STRING_CHARS(s))) != 0)
    error_posix(s, NULL);

  return STk_void;
}

/*
<doc EXT make-directory create-directory
 * (create-directory dir)
 * (create-directory dir permissions)
 *
 * Create a directory with name |dir|. If |permissions| is omitted, it
 * defaults to #o775 (masked by the current umask).
 *
 * NOTE: This function is also defined in {{link-srfi 170}}. The old name
 * |make-directory| is deprecated.
doc>
*/
DEFINE_PRIMITIVE("create-directory", make_directory, subr12, (SCM path, SCM perms))
{
  mode_t msk;

  if (!STRINGP(path)) error_bad_path(path);

  if (perms) {
    if (!INTP(perms)) error_bad_int(perms);
    msk = INT_VAL(perms);
  }
  else {
    umask(msk = umask(0));
    msk = 0775;
  }

  if (mkdir(STRING_CHARS(path), msk) != 0)
    STk_error_posix(errno, "", path, perms);

  return STk_void;
}


/*
<doc EXT delete-directory remove-directory
 * (delete-directory dir)
 * (remove-directory dir)
 *
 * Delete the directory with name |dir|.
 * @l
 * NOTE: This function is also defined in {{link-srfi 170}}. The name
 * |remove-directory| is kept for compatibility.
doc>
*/
DEFINE_PRIMITIVE("delete-directory", delete_directory, subr1, (SCM path))
{
  if (!STRINGP(path)) error_bad_path(path);
  if (rmdir(STRING_CHARS(path)) != 0)
    STk_error_posix(errno, "", path, NULL);
  return STk_void;
}

/*
<doc EXT getpid
 * (getpid)
 *
 * Returns the system process number of the current program (i.e. the
 * Unix _PID_ as an integer).
doc>
*/
DEFINE_PRIMITIVE("getpid", getpid, subr0, (void))
{
  return (MAKE_INT((int) getpid()));
}


/*
<doc EXT system
 * (system string)
 *
 * Sends the given |string| to the system shell |/bin/sh|. The result of
 * |system| is the integer status code the shell returns.
doc>
*/
DEFINE_PRIMITIVE("system", system, subr1, (SCM com))
{
  if (!STRINGP(com)) error_bad_string(com);
  return MAKE_INT(system(STRING_CHARS(com)));
}

/*
<doc EXT file-is-directory? file-is-regular? file-is-writable? file-is-readable? file-is-executable?
 * (file-is-directory?  string)
 * (file-is-regular?    string)
 * (file-is-readable?   string)
 * (file-is-writable?   string)
 * (file-is-executable? string)
 *
 * Returns |#t| if the predicate is true for the path name given in
 * |string|; returns |#f| otherwise (or if |string| denotes a file
 * which does not exist).
doc>
 */
DEFINE_PRIMITIVE("file-is-directory?", file_is_directoryp, subr1, (SCM f))
{
  struct stat info;

  if (my_stat(f, &info) != 0) return STk_false;
  return MAKE_BOOLEAN((S_ISDIR(info.st_mode)));
}


DEFINE_PRIMITIVE("file-is-regular?", file_is_regularp, subr1, (SCM f))
{
  struct stat info;

  if (my_stat(f, &info) != 0) return STk_false;
  return MAKE_BOOLEAN((S_ISREG(info.st_mode)));
}


DEFINE_PRIMITIVE("file-is-readable?", file_is_readablep, subr1, (SCM f))
{
  return my_access(f, R_OK);
}


DEFINE_PRIMITIVE("file-is-writable?", file_is_writablep, subr1, (SCM f))
{
  return my_access(f, W_OK);
}


DEFINE_PRIMITIVE("file-is-executable?", file_is_executablep, subr1, (SCM f))
{
  return my_access(f, X_OK);
}



/*
<doc R7RS file-exists?
 * (file-exists? string)
 *
 * Returns |#t| if the path name given in |string| denotes an existing file;
 * returns |#f| otherwise.
doc>
 */
DEFINE_PRIMITIVE("file-exists?", file_existsp, subr1, (SCM f))
{
  struct stat info;

  return MAKE_BOOLEAN(my_lstat(f, &info) == 0);
}

/*
<doc EXT file-size
 * (file-size string)
 *
 * Returns the size of the file whose path name is given in
 * |string|. If |string| denotes a file which does not exist,
 * |file-size| returns |#f|.
doc>
 */

DEFINE_PRIMITIVE("file-size", file_size, subr1, (SCM f))
{
  struct stat info;

  if (my_stat(f, &info) != 0) return STk_false;
  return STk_long2integer((long) info.st_size);
}

/*
<doc EXT glob
 * (glob pattern ...)
 *
 * |Glob| performs file name ``globbing'' in a fashion similar to the
 * csh shell. |Glob| returns a list of the filenames that match at least
 * one of |pattern| arguments.  The |pattern| arguments may contain
 * the following special characters:
 *
 * - |?| Matches any single character.
 * - |*| Matches any sequence of zero or more characters.
 * - |\[chars\]| Matches any single character in |chars|.
 *   If chars contains a sequence of the form |a-b| then any character
 *   between |a| and |b| (inclusive) will match.
 * - |\x| Matches the character |x|.
 * - |{a,b,...}| Matches any of the strings |a|, |b|, etc.
 * )
 *
 * As with csh, a '.' at the beginning of a file's name or just after
 * a '/' must be matched explicitly or with a |@{@}| construct.
 * In addition, all '/' characters must be matched explicitly.
 *
 * If the first character in a pattern is '~' then it refers to
 * the home directory of the user whose name follows the '~'.
 * If the '~' is followed immediately by '/' then the value of
 * the environment variable HOME is used.
 *
 * |Glob| differs from csh globbing in two ways:
 *
 * 1. it does not  sort its result list (use the |sort| procedure
 * if you want the list  sorted).
 * 2. |glob| only returns the names of files that actually exist;
 *    in csh no check for existence is made unless a pattern contains a
 *    |?|, |*|, or |[]| construct.
doc>
*/
DEFINE_PRIMITIVE("glob", glob, vsubr, (int argc, SCM *argv))
{
  return STk_do_glob(argc, argv);
}


/*
<doc R7RS delete-file remove-file
 * (delete-file string)
 *
 * Removes the file whose path name is given in |string|.
 * The result of |delete-file| is *_void_*.
 * @l
 * This function is also called |remove-file| for compatibility
 * reasons. ,(index "remove-file")
doc>
*/
#define do_remove(filename)                          do{\
  if (!STRINGP(filename)) error_bad_string(filename);   \
  if (remove(STRING_CHARS(filename)) != 0)              \
    STk_error_posix(errno, "", filename, NULL);         \
  return STk_void;                                      \
}while(0)
DEFINE_PRIMITIVE("delete-file", delete_file, subr1, (SCM filename))
{
  do_remove(filename);
}

DEFINE_PRIMITIVE("remove-file", remove_file, subr1, (SCM filename))
{
  do_remove(filename);
}


/*
<doc EXT rename-file
 * (rename-file string1 string2)
 *
 * Renames the file whose path-name is |string1| to a file whose path-name is
 * |string2|. The result of |rename-file| is *_void_*.
 * @l
 * This function is also defined in {{link-srfi 170}}.
doc>
*/
DEFINE_PRIMITIVE("rename-file", rename_file, subr2, (SCM filename1, SCM filename2))
{
  if (!STRINGP(filename1)) error_bad_string(filename1);
  if (!STRINGP(filename2)) error_bad_string(filename2);
  if (rename(STRING_CHARS(filename1), STRING_CHARS(filename2)) != 0)
    STk_error_posix(errno, "", filename1, filename2);
  return STk_void;
}


/*
<doc EXT directory-files
 * (directory-files path)
 * (directory-files path dotfiles?)
 *
 * Returns the list of the files in the directory |path|. The |dotfiles?| flag
 * (default |#f|) causes files beginning with ,(q ".") to be included in the list.
 * Regardless of the value of |dotfiles?|, the two files ,(q ".") and ,(q "..")
 * are never  returned.
 * @l
 * This function is also defined in {{link-srfi 170}}.
doc>
*/
DEFINE_PRIMITIVE("directory-files", directory_files, subr12, (SCM dirname, SCM dot))
{
  char *path;
  DIR* dir;
  SCM res = STk_nil;
  struct dirent *d;
  int add_dot_files = dot? (dot  != STk_false): 0;

  if (!STRINGP(dirname)) error_bad_string(dirname);
  path = STk_expand_file_name(STRING_CHARS(dirname));
  dir  = opendir(path);
  if (!dir) STk_error_posix(errno, "", dirname, NULL);

  /* readdir and closedir can yield an error (EBADF) only on  when dir is incorrect
   * This cannot occur here since we have tested that opendir result is OK.
   */
  for (d = readdir(dir); d ; d = readdir(dir)) {
    if (d->d_name[0] == '.') {
      if (!add_dot_files ||
          ((d->d_name[1] == '\0') || (d->d_name[1] == '.' && d->d_name[2] == '\0')))
        continue;
    }
    res = STk_cons(STk_Cstring2string(d->d_name), res);
  }
  closedir(dir);
  return STk_dreverse(res);
}



/*
<doc EXT copy-file
 * (copy-file string1 string2)
 *
 * Copies the file whose path-name is |string1| to a file whose path-name is
 * |string2|. If the file |string2| already exists, its content prior
 * the call to |copy-file| is lost. The result of |copy-file| is *_void_*.
doc>
*/
DEFINE_PRIMITIVE("copy-file", copy_file, subr2, (SCM filename1, SCM filename2))
{
  char buff[1024];
  int f1, f2, n;

  /* Should I use sendfile on Linux here? */
  if (!STRINGP(filename1)) error_bad_string(filename1);
  if (!STRINGP(filename2)) error_bad_string(filename2);

  f1 = open(STRING_CHARS(filename1), O_RDONLY);
  if (f1 == -1) error_posix(filename1, NULL);
  f2 = open(STRING_CHARS(filename2), O_WRONLY|O_CREAT|O_TRUNC, 0666);
  if (f2 == -1) error_posix(filename2, NULL);

  while ((n = read(f1, buff, MAXBUFF)) > 0) {
    if ((n < 0) || (write(f2, buff, n) < n)) {
      close(f1); close(f2);
      error_posix(filename1, filename2);
    }
  }

  close(f1); close(f2);
  return STk_void;
}


/*
<doc EXT temporary-file-name create-temp-file
 * (create-temp-file)
 * (create-temp-file prefix)
 *
 * Creates a new temporary file and returns two values: its name and an opened
 * file port on it. The optional argument specifies the filename prefix
 * to use, and defaults to the result of invoking |temp-file-prefix|.
 * The returned file port is opened in read/write mode. It ensures that
 * the name cannot be reused by another process before being used
 * in the program that calls |create-temp-file|.
 * Note, that if the opened port is not used, it can be closed and collected
 * by the GC.
 * @lisp
 * (let-values (((name port) (create-temp-file)))
 *   (let ((msg (format "Name: ~s\n" name)))
 *     (display msg)
 *     (display msg port)
 *     (close-port port)))     => prints the name of the temp. file on the
 *                                current output port and in the file itself.
 * @end lisp
 *
 * NOTE: This function is also defined in {{link-srfi 170}}.However,
 * in SRFI-170, |create-temp-file| returns only the name of the temporary file.
 *
 * NOTE: |temporary-file-name| is another name for this function.
doc>
*/

static void initialize_temp_template(char *buffer, SCM arg)
{
  SCM prefix = temp_file_prefix;

  if (arg){
    if (!STRINGP(arg)) error_bad_string(arg);
    prefix = arg;
  }
  snprintf(buffer, MAX_PATH_LENGTH, "%sXXXXXX", STRING_CHARS(prefix));
}


DEFINE_PRIMITIVE("create-temp-file", create_tmp_file, subr01, (SCM arg))
{
  char buffer[MAX_PATH_LENGTH];
  SCM port, res[2];
  int fd;

  initialize_temp_template(buffer, arg);
  fd = mkstemp(buffer);
  if (fd<0) STk_error_posix(errno,"", arg, NULL);

  /* Since fd designs an open port, create a Scheme port to avoid a file descriptor
   * leak (it will be GC'ed if not used). The result will be two values (the name
   * of the temporary file name and a write port associated to the file
   */
  port = STk_fd2scheme_port(fd, "w", buffer);
  PORT_FLAGS(port) |= PORT_TEXTUAL | PORT_BINARY;

  res[0]= port;
  res[1]= STk_Cstring2string(buffer);
  return STk_values(2, res +1);
}

/*
<doc EXT create-temp-directory
 * (create-temp-directory)
 * (create-temp-directory prefix)
 *
 * Creates a new temporary directory and returns its name as a string.
 * The optional argument specifies the filename prefix to use, and
 * defaults to the result of invoking |temp-file-prefix|.
doc>
*/
DEFINE_PRIMITIVE("create-temp-directory", create_tmp_dir, subr01, (SCM arg))
{
  char buffer[MAX_PATH_LENGTH];
  char *dir;

  initialize_temp_template(buffer, arg);
  if ((dir = mkdtemp(buffer)) == NULL) {
    STk_error_posix(errno,"", arg, NULL);
  }
  return STk_Cstring2string(dir);
}


/*
<doc EXT register-exit-function!
 * (register-exit-function! proc)
 *
 * This function registers |proc| as an exit function. This function will
 * be called when the program exits. When called, |proc| will be passed one
 * parmater which is the status given to the |exit| function (or 0 if the
 * programe terminates normally). The result of  |register-exit-function!|
 * is undefined.
 * @lisp
 * (let* ((tmp (temporary-file-name))
 *        (out (open-output-file tmp)))
 *   (register-exit-function! (lambda (n)
 *                              (when (zero? n)
 *                                (delete-file tmp))))
 *   out)
 * @end lisp
doc>
*/
MUT_DECL(at_exit_mutex);         /* The exit mutex */

DEFINE_PRIMITIVE("register-exit-function!", at_exit, subr1, (SCM proc))
{
  if (STk_procedurep(proc) == STk_false) STk_error("bad procedure ~S", proc);

  MUT_LOCK(at_exit_mutex);
  exit_procs = STk_cons(proc, exit_procs);
  MUT_UNLOCK(at_exit_mutex);
  return STk_void;
}


DEFINE_PRIMITIVE("%pre-exit", pre_exit, subr1, (SCM retcode))
{
  /* Execute the at-exit handlers */
  MUT_LOCK(at_exit_mutex);
  for (  ; !NULLP(exit_procs); exit_procs = CDR(exit_procs))
    STk_C_apply(CAR(exit_procs), 1, retcode);
  MUT_UNLOCK(at_exit_mutex);

  /* Flush all bufers */
  STk_close_all_ports();

  return STk_void;
}

/*
<doc EXT exit
 * (exit)
 * (exit ret-code)
 *
 * Exits the program with the specified integer return code. If |ret-code|
 * is omitted, the program terminates with a return code of 0.
 * If  program has registered exit functions with |register-exit-function!|,
 * they are called (in an order which is the reverse of their call order).
 * @l
 * NOTE: The {{stklos}} |exit| primitive accepts also an
 * integer value as parameter (R7RS accepts only a boolean).
doc>
*/
DEFINE_PRIMITIVE("exit", exit, subr01, (SCM retcode))
{
  long ret = 0;
  SCM cond;

  if (retcode) {
    if (BOOLEANP(retcode)) {
      ret = (retcode != STk_true);
    } else {
      ret = STk_integer_value(retcode);
      if (ret == LONG_MIN) STk_error("bad return code ~S", retcode);
    }
  }

  /* Raise an &exit-r7rs condition  with the numeric value of the exit code*/
  cond = STk_make_C_cond(STk_exit_condition, 1, MAKE_INT(ret));
  STk_raise(cond);

  return STk_void; /* never reached */
}

/*
<doc EXT emergency-exit
 * (emergency-exit)
 * (emergency-exit ret-code)
 *
 * Terminates the program without running any outstanding
 * dynamic-wind _after_ procedures and communicates an exit
 * value to the operating system in the same manner as |exit|.
 * @l
 * NOTE: The {{stklos}} |emergency-exit| primitive accepts also an
 * integer value as parameter (R7RS accepts only a boolean).
doc>
*/
DEFINE_PRIMITIVE("emergency-exit", emergency_exit, subr01, (SCM retcode))
{
  long ret = 0;

  if (retcode) {
    if (BOOLEANP(retcode)) {
      ret = (retcode != STk_true);
    } else {
      ret = STk_integer_value(retcode);
      if (ret == LONG_MIN) STk_error("bad return code ~S", retcode);
    }
  }
  _exit(ret);

  return STk_void; /* never reached */
}


/*
<doc EXT machine-type
 * (machine-type)
 *
 * Returns a string identifying the kind of machine which is running the
 * program. The result string is of the form
 * |[os-name]-[os-version]-[cpu-architecture]|.
doc>
*/
DEFINE_PRIMITIVE("machine-type", machine_type, subr0, (void))
{
  return STk_Cstring2string(BUILD_MACHINE);
}


#ifdef FIXME
//EG: #ifndef HZ
//EG: #define HZ 60.0
//EG: #endif
//EG:
//EG: #ifdef CLOCKS_PER_SEC
//EG: #  define TIC CLOCKS_PER_SEC
//EG: #else
//EG: #  define TIC HZ
//EG: #endif
//EG:
//EG: PRIMITIVE STk_get_internal_info(void)
//EG: {
//EG:   SCM z = STk_makevect(7, STk_nil);
//EG:   long allocated, used, calls;
//EG:
//EG:   /* The result is a vector which contains
//EG:    *      0 The total cpu used in ms
//EG:    *      1 The number of cells currently in use.
//EG:    *    2 Total number of allocated cells
//EG:    *      3 The number of cells used since the last call to get-internal-info
//EG:    *      4 Number of gc calls
//EG:    *    5 Total time used in the gc
//EG:    *      6 A boolean indicating if Tk is initialized
//EG:    */
//EG:
//EG:   STk_gc_count_cells(&allocated, &used, &calls);
//EG:
//EG:   VECT(z)[0] = STk_makenumber(STk_my_time());
//EG:   VECT(z)[1] = STk_makeinteger(used);
//EG:   VECT(z)[2] = STk_makeinteger(allocated);
//EG:   VECT(z)[3] = STk_makenumber((double) STk_alloc_cells);
//EG:   VECT(z)[4] = STk_makeinteger(calls);
//EG:   VECT(z)[5] = STk_makenumber((double) STk_total_gc_time);
//EG: #ifdef USE_TK
//EG:   VECT(z)[6] = Tk_initialized ? STk_true: STk_false;
//EG: #else
//EG:   VECT(z)[6] = STk_false;
//EG: #endif
//EG:
//EG:   STk_alloc_cells = 0;
//EG:   return z;
//EG: }
#endif


/*
<doc EXT date
 * (date)
 *
 * Returns the current date in a string.
doc>
*/
DEFINE_PRIMITIVE("date", date, subr0, (void))
{
  time_t t   = time(NULL);
  char * res;

  res = ctime(&t);
  res[strlen(res)-1] = '\0'; /* to "delete" the newline added by asctime */
  return STk_Cstring2string(res);
}


/*
<doc EXT clock
 * (clock)
 *
 * Returns an approximation of processor time, in milliseconds, used so far by the
 * program. The value returned is a real.
doc>
 */
DEFINE_PRIMITIVE("clock", clock, subr0, (void))
{
  return STk_double2real((double) clock() /
                         CLOCKS_PER_SEC * (double) TIME_DIV_CONST);
}

/*
<doc EXT exact-clock
 * (exact-clock)
 *
 * Returns an approximation of processor time, in microseconds, used so far by the
 * program. The value returned is an integer.
doc>
 */
DEFINE_PRIMITIVE("exact-clock", exact_clock, subr0, (void)) // Result is in microseconds
{
  return MAKE_INT(clock() * CLOCKS_PER_SEC / (TIME_DIV_CONST * 1000));
}



/*
<doc EXT current-seconds
 * (current-seconds)
 *
 * Returns the time since the Epoch (that is 00:00:00 UTC, January 1, 1970),
 * measured in seconds in the Coordinated Universal Time (UTC) scale.
 *
 * NOTE: This {{stklos}} function should not be confused with
 * the R7RS  primitive |current-second| which returns an inexact number
 * and whose result is expressed using  the International Atomic Time
 * instead of UTC.
 *
doc>
 */
DEFINE_PRIMITIVE("current-seconds", current_seconds, subr0, (void))
{
  return STk_ulong2integer(time(NULL));
}

/*
<doc R7RS current-second
 * (current-second)
 *
 * Returns an inexact number representing the current time on the
 * International Atomic Time (TAI) scale.  The value 0.0 represents
 * midnight on January 1, 1970 TAI (equivalent to ten seconds before
 * midnight Universal Time) and the value 1.0 represents one TAI
 * second later.
doc>
 */

/* Offset: https://fr.wikipedia.org/wiki/Temps_atomique_international */
#define TAI_OFFSET +37.0L

#ifndef HAVE_CLOCK_GETTIME
#  ifndef CLOCK_REALTIME
#    define CLOCK_REALTIME 1  /* to avoid undeclared identifier later */
#  endif

int clock_gettime(clockid_t _UNUSED(clockid), struct timespec *tp)
{
  /* System doesn't provide clock_gettime. Define ours, which  always
   * returns a CLOCK_REALTIME time.
   * NOTE: We cannot have a error if clockid is not CLOCK_REALTIME since
   * GC is initialized with CLOCK_MONOTONIC. Consequently, the result
   * with CLOCK_MONOTONIC may in some circumstances go backward.
   */
  struct timeval tv;

  gettimeofday(&tv, NULL);
  tp->tv_sec  = tv.tv_sec;
  tp->tv_nsec = tv.tv_usec * 1000;
  return 0;
}
#endif


DEFINE_PRIMITIVE("current-second", current_second, subr0, (void))
{
  /* R7RS states: Neither high accuracy nor high precision are
   * required; in particular, returning Coordinated Universal Time plus
   * a suitable constant might be the best an implementation can do.
   */
  struct timespec now;

  clock_gettime(CLOCK_REALTIME, &now);
  return STk_double2real(TAI_OFFSET +
                         (double) now.tv_sec +
                         1.0E-9 * (double) now.tv_nsec);
}


/* Return a time object corresponding to the current time  (UTC). */
DEFINE_PRIMITIVE("%current-time", current_time, subr0, (void))
{
  struct timespec now;
  SCM argv[4];

  clock_gettime(CLOCK_REALTIME, &now);

  argv[3] = time_type;
  argv[2] = STk_intern("time-utc");
  argv[1] = STk_long2integer(now.tv_nsec);
  argv[0] = STk_long2integer(now.tv_sec);
  return STk_make_struct(4, &argv[3]);
}


/*
<doc EXT sleep
 * (sleep n)
 *
 * Suspend the execution of the program for at |ms| milliseconds. Note that due
 * to system clock resolution, the pause may be a little bit longer. If a
 * signal arrives during the pause, the execution may be resumed.
 *
doc>
*/
DEFINE_PRIMITIVE("sleep", sleep, subr1, (SCM ms))
{
  long n = STk_integer_value(ms);
  struct timespec ts;

  if (n == LONG_MIN)
    error_bad_int_or_out_of_bounds(ms);

  ts.tv_sec  = n / 1000;
  ts.tv_nsec = (n % 1000) * 1000000;

  nanosleep(&ts, NULL);
  return STk_void;
}

/* this will calculate the offset in seconds
   of the given time
   slightly adapted from a StackOverflow question
   ( https://stackoverflow.com/questions/32424125/c-code-to-get-local-time-offset-in-minutes-relative-to-utc/32433990#32433990 )
   Code posted on StackOverflow is licensed CC-BY-SA, so the attribution:
   By: StackOverflow user Serge Ballesta (https://stackoverflow.com/users/3545273/serge-ballesta)
*/
long tz_offset(time_t t) {
  struct tm local = *localtime(&t);
  struct tm utc = *gmtime(&t);

  long offset = ((local.tm_hour - utc.tm_hour) * 60 +
                 (local.tm_min - utc.tm_min))  * 60L +
                (local.tm_sec - utc.tm_sec);

  int offset_day = local.tm_mday - utc.tm_mday;

  /* end of month? */
  if ((offset_day == 1) || (offset_day < -1)) {
    offset += 24L * 3600;
  } else if ((offset_day == -1) || (offset_day > 1)) {
    offset -= 24L * 3600;
  }
  return offset;
}


/*
<doc EXT local-timezone-offset
 * (local-timezone-offset)
 *
 * Returns the local timezone offset, in seconds.
 *
 * For example, for GMT+2 it will be |2 * 60 * 60| = |7200|
 *
 * @lisp
 * (local-timezone-offset) => 0        ;; for GMT
 * (local-timezone-offset) => 7200     ;; for GMT+2
 * (local-timezone-offset) => -10800   ;; for GMT-3
 * @end lisp
 *
 * The timezone is searched for in the environment variable |TZ|. If this
 * variable does not appear in the environment, the system timezone is used.
doc>
*/
DEFINE_PRIMITIVE("local-timezone-offset", local_timezone_offset, subr0, ())
{
  /* Init the timezone: */
  time_t tt = (time_t) 0L;
  struct tm *t __attribute__ ((unused)) = localtime(&tt);

  /* We used to use "- timezone" to get the offset in seconds, but it's not
     portable, so we use the tz_offset function: */
  return STk_long2integer(tz_offset(tt));
}


/* Convert the date in seconds since the Epoch into a date object */
DEFINE_PRIMITIVE("%seconds->date", seconds2date, subr1, (SCM seconds))
{
  int overflow;
  SCM argv[12];
  struct tm *t;
  time_t tt = (time_t) 0L;
  long nsec = 0L;

  if (INTP(seconds)) {
      tt = (time_t) STk_integer2int32(seconds, &overflow);
      if (overflow) error_bad_int_or_out_of_bounds(seconds);
  } else if (REALP(seconds)) {
      double sec = REAL_VAL(seconds);
      tt = (time_t) STk_integer2int32(MAKE_INT((long)floor(sec)), &overflow);
      if (overflow) STk_error("bad number ~S (or out of range)",seconds);
      /* There doesn't seem to be an easy way to portably detect the maximum
         value for time_t, so we have to assume it's *at least* the same as
         a long int... If time_t is a float or double, it could hold larger
         numbers, but then we'd have to detect the typt of time_t, and the
         magnitude of the times represented doesn't seem like something one'd
         actually need.
         --jpellegrini */
      if (tt > INT_MAX_VAL || tt < INT_MIN_VAL)
          STk_error("seconds value out of representable bounds ~S", seconds);
      nsec = (sec - tt) * 1000000000;
  } else STk_error("bad integer or real ~S", seconds);

  t = localtime(&tt);
  argv[11] = date_type;
  argv[10] = MAKE_INT(nsec);
  argv[9]  = MAKE_INT(t->tm_sec);
  argv[8]  = MAKE_INT(t->tm_min);
  argv[7]  = MAKE_INT(t->tm_hour);
  argv[6]  = MAKE_INT(t->tm_mday);
  argv[5]  = MAKE_INT(t->tm_mon + 1);
  argv[4]  = MAKE_INT(t->tm_year + 1900);
  argv[3]  = MAKE_INT(t->tm_wday);
  argv[2]  = MAKE_INT(t->tm_yday + 1);
  argv[1]  = MAKE_INT(t->tm_isdst);
#ifdef DARWIN
  argv[0]  = MAKE_INT(0);       /* Cannot figure how to find the timezone */
#else
  /* For some strange reason, timezone contains the offset seconds with the
     opposite sign as one would usually see, so we swap it here. */
  argv[0]  = STk_long2integer(tz_offset(tt));
#endif
  return STk_make_struct(12, &argv[11]);
}


/*
<doc EXT date->seconds
 * (date->seconds d)
 *
 * Convert the date |d| to the number of seconds since the _Epoch_,
 * 1970-01-01 00:00:00 +0000 (UTC).
 *
 * @lisp
 * (date->seconds (make-date 0 37 53 1 26 10 2012 0))   => 1351216417.0
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("date->seconds", date2seconds, subr1, (SCM date))
{
  struct tm t;
  time_t n;
  SCM *p;
  double nanoseconds;

  if (!STRUCTP(date) || STRUCT_TYPE(date) != date_type)
    STk_error("bad date ~S", date);

  p = (SCM *) &(STRUCT_SLOTS(date));
  nanoseconds = STk_integer_value(*p++) / 1000000000.0;
  t.tm_sec    = STk_integer_value(*p++);
  t.tm_min    = STk_integer_value(*p++);
  t.tm_hour   = STk_integer_value(*p++);
  t.tm_mday   = STk_integer_value(*p++);
  t.tm_mon    = STk_integer_value(*p++) - 1;
  t.tm_year   = STk_integer_value(*p++) - 1900;
  t.tm_isdst  = -1;                      /* to ignore DST */

  n = mktime(&t);
  if (n == (time_t)(-1)) STk_error("cannot convert date to seconds (~S)", date);

  return STk_double2real((double) n + nanoseconds);
}



DEFINE_PRIMITIVE("%seconds->string", date2string, subr2, (SCM fmt, SCM seconds))
{
  char buffer[1024];
  struct tm *p;
  time_t tt;
  int len, overflow;

  tt = (time_t) STk_integer2int32(seconds, &overflow);

  if (!STRINGP(fmt)) error_bad_string(fmt);
  if (overflow)      error_bad_int_or_out_of_bounds(seconds);

  p   = localtime(&tt);
  len = strftime(buffer, 1023, STRING_CHARS(fmt), p);

  if (len > 0)
    return STk_Cstring2string(buffer);
  else
    STk_error("buffer too short!");

  return STk_void; /* never reached */
}


/*
<doc EXT running-os
 * (running-os)
 *
 * Returns the name of the underlying Operating System which is running
 * the program.
 * The value returned by |runnin-os| is a symbol. For now, this procedure
 * returns either |unix|, |android|, |windows|, or |cygwin-windows|.
doc>
*/
DEFINE_PRIMITIVE("running-os", running_os, subr0, (void))
{
#ifdef WIN32
# ifdef __CYGWIN32__
  return STk_intern("cygwin-windows");
# else
  return STk_intern("windows");
# endif
#else
#  ifdef __ANDROID__
  return STk_intern("android");
#  else
  return STk_intern("unix");
#endif
#endif
}



/*
<doc EXT getenv
 * (getenv str)
 * (getenv)
 *
 * Looks for the environment variable named |str| and returns its
 * value as a string, if it exists. Otherwise, |getenv| returns |#f|.
 * If |getenv| is called without parameter, it returns the list of
 * all the environment variables accessible from the program as an
 * A-list.
 * @lisp
 * (getenv "SHELL")
 *      => "/bin/zsh"
 * (getenv)
 *      => (("TERM" . "xterm") ("PATH" . "/bin:/usr/bin") ...)
 * @end lisp
doc>
 */
static SCM build_posix_environment(char **env)
{
  SCM l;

  for (l=STk_nil; *env; env++) {
    char *s, *p;

    s = *env; p = strchr(s, '=');
    if (p)
      l = STk_cons(STk_cons(STk_makestring(p-s, s), STk_Cstring2string(p+1)),l);
  }
  return l;
}


DEFINE_PRIMITIVE("getenv", getenv, subr01, (SCM str))
{
  char *tmp;

  if (str) {            /* One parameter: find the value of the given variable */
    if (!STRINGP(str)) error_bad_string(str);

    tmp = getenv(STRING_CHARS(str));
    return tmp ? STk_Cstring2string(tmp) : STk_false;
  } else {              /* No parameter: give the complete environment */
    extern char **environ;
    return build_posix_environment(environ);
  }
}

/*
<doc EXT setenv!
 * (setenv! var value)
 *
 * Sets the environment variable |var| to |value|. |Var| and
 * |value| must be strings. The result of |setenv!| is *_void_*.
doc>
 */
DEFINE_PRIMITIVE("setenv!", setenv, subr2, (SCM var, SCM value))
{
  char *s;
  unsigned long len;

  if (!STRINGP(var))                  error_bad_string(var);
  if (strchr(STRING_CHARS(var), '=')) STk_error("variable ~S contains a '='", var);
  if (!STRINGP(value))                STk_error("value ~S is not a string", value);

  len = strlen(STRING_CHARS(var))   +
        strlen(STRING_CHARS(value)) + 2; /* 2 because of '=' & \0 */

  s = STk_must_malloc_atomic(len);
  snprintf(s, len, "%s=%s", STRING_CHARS(var), STRING_CHARS(value));
  putenv(s);
  return STk_void;
}
/*
<doc EXT unsetenv!
 * (unsetenv! var)
 *
 * Unsets the environment variable |var|. |Var| must be a string.
 * The result of |unsetenv!| is *_void_*.
doc>
 */
DEFINE_PRIMITIVE("unsetenv!", unsetenv, subr1, (SCM var))
{
#ifndef SOLARIS
  if (!STRINGP(var)) error_bad_string(var);
  unsetenv(STRING_CHARS(var));
  return STk_void;
#else
  /* not exactly the same since getenv will not return #f after that */
  return STk_setenv(var, STk_Cstring2string(""));
#endif
}

/*
<doc EXT hostname
 * (hostname)
 *
 * Return the  host name of the current processor as a string.
doc>
*/
DEFINE_PRIMITIVE("hostname", hostname, subr0, (void))
{
  char buff[256];

  if (gethostname(buff, 256) < 0)
    buff[255] = '0';
  return STk_Cstring2string(buff);
}

/*
<doc EXT pause
 * (pause)
 *
 * Pauses the STklos process until the delivery of a signal whose action
 * is either to execute a signal-catching function or to terminate the
 * process. If the action is to terminate the process,  |pause| will not
 * return. If the action is to execute a signal-catching function, |pause|
 * will terminate after the signal-catching function returns.
doc>
*/
DEFINE_PRIMITIVE("pause", pause, subr0, (void))
{
  pause();
  return STk_void;
}



/*
 * Undocumented primitives
 *
 */

DEFINE_PRIMITIVE("%library-prefix", library_prefix, subr01, (SCM arg))
{
  char *res = "";

  if (arg) {
    if (SYMBOLP(arg)) {
      if (strcmp(SYMBOL_PNAME(arg), "lib") == 0) res = EXECDIR;
      else if (strcmp(SYMBOL_PNAME(arg), "data") == 0) res = SCMDIR;
    }
    if (!*res)
      STk_error("bad argument (must be either the symbol lib or data)");
  } else {
    /* No argument => return the prefix only */
    res = PREFIXDIR;
  }
  return STk_Cstring2string(res);
}

DEFINE_PRIMITIVE("%shared-suffix", shared_suffix, subr0, (void))
{
  return STk_Cstring2string(SHARED_SUFFIX);
}

DEFINE_PRIMITIVE("%shared-library-suffix", shared_library_suffix, subr0, (void))
{
  return STk_Cstring2string(SHARED_LIB_SUFFIX);
}

DEFINE_PRIMITIVE("%chmod", change_mode, subr2, (SCM file, SCM value))
{
  long mode = STk_integer_value(value);

  if (!STRINGP(file))   error_bad_path(file);
  if (mode < 0 || mode > 0777) error_bad_int_or_out_of_bounds(value);

  return MAKE_BOOLEAN(chmod(STRING_CHARS(file), mode) == 0);
}


DEFINE_PRIMITIVE("%big-endian?", big_endianp, subr0, (void))
{
  int i = 1;
  char *p = (char *)&i;

  return MAKE_BOOLEAN(p[0] != 1);
}


DEFINE_PRIMITIVE("%get-locale", get_locale, subr0, (void))
{
  char *str = setlocale(LC_ALL, NULL);

  return str? STk_Cstring2string(str) : STk_false;
}


DEFINE_PRIMITIVE("%uname", uname, subr0, (void))
{
  struct utsname name;
  SCM z;

  if (uname(&name) != 0) {
    error_posix(NULL, NULL);
  }
  z = STk_makevect(5, STk_void);
  VECTOR_DATA(z)[0] = STk_Cstring2string(name.sysname);
  VECTOR_DATA(z)[1] = STk_Cstring2string(name.nodename);
  VECTOR_DATA(z)[2] = STk_Cstring2string(name.release);
  VECTOR_DATA(z)[3] = STk_Cstring2string(name.version);
  VECTOR_DATA(z)[4] = STk_Cstring2string(name.machine);

  return z;
}

DEFINE_PRIMITIVE("os-name", os_name, subr0, (void))
{
  struct utsname name;

  if (uname(&name) != 0) {
    error_posix(NULL, NULL);
  }
  return STk_Cstring2string(name.sysname);
}

/* Allocates up to *n CONS cells. That is, the result is a list with
   AT MOST *n cells, but maybe less.

   1. The parameter (*n) will be changed to the number of cells still
      missing, or zero if the list is complete.
   2. The parameter (*last) will be set to the last CONS cell of the
      list. This is to optimize the situation in which this function
      did not yet return enough cells, and will be called again. Later,
      this link will be used to glue the lists together without the
      need to use APPEND.  */
SCM STk_must_malloc_list_upton(int *n, SCM init, SCM *last) {
  if (*n == 0) return STk_nil;
  if (*n < 0)  STk_error("Negative list size ~s", MAKE_INT(*n));

  /* libgc does not allow us to choose how many objects we'd like to
     allocate.  We ask for many objects and get a linked list where
     each object has the size given as argument.
     The first word of the object is a link to the next.   */
  void **ptr = GC_malloc_many(sizeof(struct cons_obj));
  if (!ptr) STk_error("Cannot allocate list of size ~s", MAKE_INT(*n));

  /* GC_NEXT gets the next in the list. */
  void **next =  GC_NEXT(ptr);

  /* Adjust the first CONS cell (there is at least one, since we
     already returned STk_nil for zero) */
  BOXED_TYPE((struct cons_obj* ) ptr) = tc_cons;
  BOXED_INFO((struct cons_obj* ) ptr) = 0;
  CAR((struct cons_obj* ) ptr) = init;
  CDR((struct cons_obj* ) ptr) = STk_nil;

  /* ptr will be the last cons on the list, so we update the
     last parameter: */
  *last = (SCM) ptr;

  /* count is the number of actually allocated cells. */
  int count = 1;
  void **tmp;

  while ((void **) next) {
    tmp = GC_NEXT(next);
    /* We only run up to n cells. After that, we just follow the
       links, clearing up the first word in order to give them back to
       the GC. */
    if (count >= *n) {
      next = NULL;
    } else {
      count++;
      BOXED_TYPE((struct cons_obj* ) next) = tc_cons;
      BOXED_INFO((struct cons_obj* ) next) = 0;
      CAR((struct cons_obj* ) next) = init;
      CDR((struct cons_obj* ) next) = ptr;
      ptr = next;
    }
    next = tmp;
  }
  /* Let's count the bytes effectively given to the user, not those
   * that we threw away. */
  if (STk_count_allocations)
    STk_thread_inc_allocs(STk_current_thread(),
                          count * sizeof(struct cons_obj));
  /* *n will hold the cells still missing. */
  *n -= count;
  return (SCM) ptr;
}

/* Allocates, at once, a list of n cons cells, all of them initialzied
   with the value of init. */
SCM STk_must_malloc_list(int n, SCM init) {
  SCM z2;
  SCM last, new_last;
  SCM z = STk_must_malloc_list_upton(&n, init, &last);
  while (n) {
    /* n is the remaining cells to allocate. Try again:*/
    z2 = STk_must_malloc_list_upton(&n, init, &new_last);

    /* last was the last cell in the old list. We point its CDR to
       the new list, and set last to the new_last (of the new list).
       This effectively appends z2 to the end of the old list. */
    CDR((SCM)last) = z2;
    last = new_last;
  }
  return z;
}

int STk_init_system(void)
{
  SCM current_module = STk_STklos_module;

  /* Create the system-date structure-type */
  date_type =  STk_make_struct_type(STk_intern("%date"),
                                    STk_false,
                                    STk_cons(STk_intern("nanosecond"),
                                             LIST10(STk_intern("second"),
                                                    STk_intern("minute"),
                                                    STk_intern("hour"),
                                                    STk_intern("day"),
                                                    STk_intern("month"),
                                                    STk_intern("year"),
                                                    STk_intern("week-day"),
                                                    STk_intern("year-day"),
                                                    STk_intern("dst"),
                                                    STk_intern("tz"))));
  STk_define_variable(STk_intern("%date"), date_type, current_module);

  /* Create the time structure-type */
  time_type =  STk_make_struct_type(STk_intern("%time"),
                                    STk_false,
                                    LIST3(STk_intern("type"),
                                          STk_intern("nanosecond"),
                                          STk_intern("second")));
  STk_define_variable(STk_intern("%time"), time_type, current_module);


  /* Add parameter for generating temporary files */
  STk_make_C_parameter("temp-file-prefix",
                       init_temp_file_prefix(),
                       temp_file_prefix_conv,
                       current_module);

  /* Declare primitives */
  ADD_PRIMITIVE(clock);
  ADD_PRIMITIVE(exact_clock);
  ADD_PRIMITIVE(date);
  ADD_PRIMITIVE(current_seconds);
  ADD_PRIMITIVE(current_second);
  ADD_PRIMITIVE(current_time);
  ADD_PRIMITIVE(sleep);
  ADD_PRIMITIVE(seconds2date);
  ADD_PRIMITIVE(date2seconds);
  ADD_PRIMITIVE(date2string);
  ADD_PRIMITIVE(local_timezone_offset);
  ADD_PRIMITIVE(running_os);
  ADD_PRIMITIVE(getenv);
  ADD_PRIMITIVE(setenv);
  ADD_PRIMITIVE(unsetenv);
  ADD_PRIMITIVE(hostname);
  ADD_PRIMITIVE(library_prefix);
  ADD_PRIMITIVE(shared_suffix);
  ADD_PRIMITIVE(shared_library_suffix);
  ADD_PRIMITIVE(change_mode);

  ADD_PRIMITIVE(getcwd);
  ADD_PRIMITIVE(chdir);
  ADD_PRIMITIVE(make_directory);
  ADD_PRIMITIVE(delete_directory);
  ADD_PRIMITIVE(directory_files);
  ADD_PRIMITIVE(getpid);
  ADD_PRIMITIVE(system);

  ADD_PRIMITIVE(file_is_directoryp);
  ADD_PRIMITIVE(file_is_regularp);
  ADD_PRIMITIVE(file_is_readablep);
  ADD_PRIMITIVE(file_is_writablep);
  ADD_PRIMITIVE(file_is_executablep);
  ADD_PRIMITIVE(file_existsp);
  ADD_PRIMITIVE(file_size);
  ADD_PRIMITIVE(glob);
  ADD_PRIMITIVE(expand_fn);
  ADD_PRIMITIVE(canonical_path);

  ADD_PRIMITIVE(remove_file);
  ADD_PRIMITIVE(delete_file);
  ADD_PRIMITIVE(rename_file);
  ADD_PRIMITIVE(copy_file);
  ADD_PRIMITIVE(create_tmp_file);
  ADD_PRIMITIVE(create_tmp_dir);
  ADD_PRIMITIVE(pre_exit);
  ADD_PRIMITIVE(exit);
  ADD_PRIMITIVE(emergency_exit);
  ADD_PRIMITIVE(at_exit);
  ADD_PRIMITIVE(machine_type);

  ADD_PRIMITIVE(winify_filename);
  ADD_PRIMITIVE(posixify_filename);

  ADD_PRIMITIVE(pause);
  ADD_PRIMITIVE(big_endianp);
  ADD_PRIMITIVE(get_locale);
  ADD_PRIMITIVE(uname);
  ADD_PRIMITIVE(os_name);
  return TRUE;
}
