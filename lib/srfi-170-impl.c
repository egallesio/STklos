/*
 * srfi-170-impl.c   -- C part of SRFI-170: POSIX API
 *
 * Copyright © 2021 Jeronimo Pellegrini <j_p@aleph0.info>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 *           Author: Jeronimo Pellegrini [j_p@aleph0.info]
 *    Creation date: 09-Jan-2021 11:54
 * Last file update: 19-Mar-2021 18:52 (eg)
 */

#include <limits.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <dirent.h>
#include <fcntl.h>
#include <string.h>
#include <time.h>
#include <grp.h>
#include <pwd.h>

#include "stklos.h"
#include "struct.h"
#include "fport.h"

static SCM file_info_type, dir_info_type, user_info_type, group_info_type;
static SCM posix_error;

SCM time_type; /* FIXME */

/* 3.1 Error handling */


DEFINE_PRIMITIVE("posix-get-message-for-errno", posix_strerror, subr1, (SCM obj))
{
    if (!  ( INTP(obj) ||
             (CONDP(obj) && 1)))
        STk_error("bad integer or condition", obj);

    char *errstr="";


    if (INTP(obj)) errstr = strerror(INT_VAL(obj));
    else if (CONDP(obj)) {
        /* don't presume that the message field in obj has been
           set up properly. get the string from errno */
        SCM e = STk_int_struct_ref(obj,STk_intern("errno"));
        errstr = strerror(INT_VAL(e));
    } else STk_error("impossible error!");

    return STk_Cstring2string(errstr);
}

/* # 3.1 */
static SCM symb_EACCES, symb_EBADF, symb_EBUSY, symb_EDQUOT, symb_EEXIST, symb_EFAULT, symb_EINVAL,
           symb_EINTR, symb_EIO, symb_ELOOP, symb_EMLINK, symb_ENAMETOOLONG, symb_ENOENT,
           symb_ENOMEM, symb_ENOSPC, symb_ENOSYS, symb_ENOTDIR, symb_ENOTEMPTY, symb_ENOTTY,
           symb_EOVERFLOW, symb_EPERM, symb_EROFS, symb_ESRCH, symb_EXDEV;

/* # 3.2 */
// TODO

/* # 3.3 */
static SCM symb_time_now, symb_time_unchanged, symb_second, symb_nanosecond;



static void initialize_global_symbols(void)
{
  /* ========== POSIX errrors ========== */
  symb_EACCES       = STk_intern("EACCES");
  symb_EBADF        = STk_intern("EBADF");
  symb_EBUSY        = STk_intern("EBUSY");
  symb_EDQUOT       = STk_intern("EDQUOT");
  symb_EEXIST       = STk_intern("EEXIST");
  symb_EFAULT       = STk_intern("EFAULT");
  symb_EINVAL       = STk_intern("EINVAL");
  symb_EINTR        = STk_intern("EINTR");
  symb_EIO          = STk_intern("EIO");
  symb_ELOOP        = STk_intern("ELOOP");
  symb_EMLINK       = STk_intern("EMLINK");
  symb_ENAMETOOLONG = STk_intern("ENAMETOOLONG");
  symb_ENOENT       = STk_intern("ENOENT");
  symb_ENOMEM       = STk_intern("ENOMEM");
  symb_ENOSPC       = STk_intern("ENOSPC");
  symb_ENOSYS       = STk_intern("ENOSYS");
  symb_ENOTDIR      = STk_intern("ENOTDIR");
  symb_ENOTEMPTY    = STk_intern("ENOTEMPTY");
  symb_ENOTTY       = STk_intern("ENOTTY");
  symb_EOVERFLOW    = STk_intern("EOVERFLOW");
  symb_EPERM        = STk_intern("EPERM");
  symb_EROFS        = STk_intern("EROFS");
  symb_ESRCH        = STk_intern("ESRCH");
  symb_EXDEV        = STk_intern("EXDEV");

  /* ========== Section #3.3 ========== */
  symb_time_now       = STk_intern("time/now");
  symb_time_unchanged = STk_intern("time/unchanged");
  symb_second         = STk_intern("second");
  symb_nanosecond     = STk_intern("nanosecond");

}



DEFINE_PRIMITIVE("%get-posix-error-name", get_posix_error_name,subr1,(SCM n))
{
  if (!INTP(n)) STk_error("bad integer ~S", n);

  switch (INT_VAL(n)) {
    case EACCES:        return symb_EACCES;
    case EBADF:         return symb_EBADF;
    case EBUSY:         return symb_EBUSY;
    case EDQUOT:        return symb_EDQUOT;
    case EEXIST:        return symb_EEXIST;
    case EFAULT:        return symb_EFAULT;
    case EINVAL:        return symb_EINVAL;
    case EINTR:         return symb_EINTR;
    case EIO:           return symb_EIO;
    case ELOOP:         return symb_ELOOP;
    case EMLINK:        return symb_EMLINK;
    case ENAMETOOLONG:  return symb_ENAMETOOLONG;
    case ENOENT:        return symb_ENOENT;
    case ENOMEM:        return symb_ENOMEM;
    case ENOSPC:        return symb_ENOSPC;
    case ENOSYS:        return symb_ENOSYS;
    case ENOTDIR:       return symb_ENOTDIR;
    case ENOTEMPTY:     return symb_ENOTEMPTY;
    case ENOTTY:        return symb_ENOTTY;
    case EOVERFLOW:     return symb_EOVERFLOW;
    case EPERM:         return symb_EPERM;
    case EROFS:         return symb_EROFS;
    case ESRCH:         return symb_ESRCH;
    case EXDEV:         return symb_EXDEV;
    default:            STk_error("POSIX unknown error ~S", INT_VAL(n));
  }
  return STk_void;  /* for the compiler */
}

void STk_error_posix(int err,char *proc_name, SCM args)
{
    SCM err_no = MAKE_INT(err);
    SCM errname = STk_get_posix_error_name(err_no);
    char *msg = strerror(err);
    SCM message = STk_Cstring2string(msg);
    SCM procedure = STk_intern(proc_name);

    STk_raise_exception(STk_make_C_cond(posix_error,
                                        7,
                                        procedure,    /* location  */
                                        STk_vm_bt(),  /* backtrace */
                                        message,      /* message */
                                        message,      /* r7rs-msg */
                                        args,         /* r7rs-irritants */
                                        errname,      /* errname */
                                        err_no));     /* errno */
}


/* 3.2 I/O */

DEFINE_PRIMITIVE("open-file",posix_open,vsubr, (int argc, SCM *argv))
{

    if (argc < 3) STk_error("expects at least three arguments");
    if (argc > 5) STk_error("expects at most five arguments");

    SCM name  = *argv--;
    SCM type  = *argv--;
    SCM flags = *argv--;

    if (!STRINGP(name)) STk_error("bad string ~S", name);
    char *cname = STRING_CHARS(name);

    if (!INTP(flags)) STk_error("bad integer ~S", flags);
    int cflags = INT_VAL(flags);

    int cpermbits = 0666;
    if (argc > 3) {
        SCM permbits = *argv--;
        if (!INTP(permbits)) STk_error("bad integer ~S", permbits);
        cpermbits = INT_VAL(permbits);
    }

    if (argc > 4) {
        // SCM bufmode = *argv--;
        STk_error("setting buffering mode not supported");
    }

    char *mode;
    int pflags; /* int? */
    if      (STk_eqv(type, STk_intern("binary-input")) == STk_true)
    {
        mode = "r";
        pflags = PORT_BINARY|PORT_READ;
        cflags |= O_RDONLY;
    }
    else if (STk_eqv(type, STk_intern("textual-input")) == STk_true)
    {
        mode = "r";
        pflags = PORT_TEXTUAL|PORT_READ;
        cflags |= O_RDONLY;
    }
    else if (STk_eqv(type, STk_intern("binary-output")) == STk_true)
    {
        mode = "a";
        pflags = PORT_BINARY|PORT_WRITE;
        cflags |= O_WRONLY;
    }
    else if (STk_eqv(type, STk_intern("textual-output")) == STk_true)
    {
        mode = "a";
        pflags = PORT_TEXTUAL|PORT_WRITE;
        cflags |= O_WRONLY;
    }
    else if (STk_eqv(type, STk_intern("binary-input/output")) == STk_true)
    {
        mode = "r+";
        pflags = PORT_BINARY|PORT_RW;
        cflags |= O_RDWR;
    }
    else {
        STk_error("bad port type ~S", type);
        return STk_void;     /* to avoid a compiler warning */
    }

    int fd = open(cname, cflags, cpermbits);
    if (fd == -1) STk_error_posix(errno,"open-file",STk_argv2list(argc,argv));

    SCM port = STk_fd2scheme_port (fd,mode,cname);
    if (port == NULL) STk_error_posix(errno,"open-file",STk_argv2list(argc,argv));
    PORT_FLAGS(port) = PORT_IS_FILE | pflags;
    return port;
}

DEFINE_PRIMITIVE("fd->port",posix_fd_port,subr23,(SCM fd, SCM type, SCM bufmode))
{
    /* FIXME: we ignore bufmode */


    if (!bufmode) bufmode = STk_false;

    char cname[50];
    snprintf(cname,sizeof(cname),"fd->port(%ld)",INT_VAL(fd));

    char *mode;

    int flags; /* "int"? */

    if      (STk_eqv(type, STk_intern("binary-input")) == STk_true)   { mode = "r"; flags = PORT_BINARY|PORT_READ;}
    else if (STk_eqv(type, STk_intern("textual-input")) == STk_true)  { mode = "r"; flags = PORT_TEXTUAL|PORT_READ;}
    else if (STk_eqv(type, STk_intern("binary-output")) == STk_true)  { mode = "a"; flags = PORT_BINARY|PORT_WRITE;}
    else if (STk_eqv(type, STk_intern("textual-output")) == STk_true) { mode = "a"; flags = PORT_TEXTUAL|PORT_WRITE;}
    else if (STk_eqv(type, STk_intern("binary-input/output")) == STk_true) { mode = "r+"; flags = PORT_BINARY|PORT_RW;}
    else { STk_error("bad port type ~S", type); return STk_void; }

    SCM port = STk_fd2scheme_port (INT_VAL(fd),mode,cname);
    if (port == NULL) STk_error_posix(errno,"fd->port",LIST3(fd,type,bufmode));
    PORT_FLAGS(port) = flags;
    return port;
}

/* 3.3 File System */


DEFINE_PRIMITIVE("create-directory", posix_mkdir, subr12, (SCM name, SCM bits))
{
    if (!STRINGP(name)) STk_error("bad string ~S", name);

    int cbits;
    if (bits) {
        if (!INTP(bits)) STk_error("bad integer ~S", bits); /* FIXME: positive? */
        cbits = INT_VAL(bits);
    }
    else {
        bits = STk_false; /* to include in error reporting */
        cbits = 0775;
    }

    char *cname = STRING_CHARS(name);
    int e = mkdir(cname,cbits);

    if (e != 0) STk_error_posix(errno,"create-directory",LIST2(name,bits));

    return STk_void;
}

DEFINE_PRIMITIVE("create-fifo", posix_mkfifo, subr12, (SCM name, SCM bits))
{
    if (!STRINGP(name)) STk_error("bad string ~S", name);
        int cbits;
    if (bits) {
        if (!INTP(bits)) STk_error("bad integer ~S", bits); /* FIXME: positive? */
    }
    else {
        bits = STk_false; /* to include in error reporting */
        cbits = 0664;
    }

    char *cname = STRING_CHARS(name);
    cbits = INT_VAL(bits);

    int e = mkfifo(cname,cbits);

    if (e != 0) STk_error_posix(errno,"create-fifo", LIST2(name,bits));

    return STk_void;
}



DEFINE_PRIMITIVE("create-hard-link", posix_link, subr2, (SCM old, SCM new))
{
    if (!STRINGP(old)) STk_error("bad string ~S", old);
    if (!STRINGP(new)) STk_error("bad string ~S", new);

    char *oldname = STRING_CHARS(old);
    char *newname = STRING_CHARS(new);

    int e = link(oldname,newname);

    if (e != 0) STk_error_posix(errno,"create-hard-link",LIST2(old,new));

    return STk_void;
}

DEFINE_PRIMITIVE("create-symlink", posix_symlink, subr2, (SCM old, SCM new))
{
    if (!STRINGP(old)) STk_error("bad string ~S", old);
    if (!STRINGP(new)) STk_error("bad string ~S", new);

    char *oldname = STRING_CHARS(old);
    char *newname = STRING_CHARS(new);

    int e = symlink(oldname,newname);

    if (e != 0) STk_error_posix(errno,"create-symlink",LIST2(old,new));

    return STk_void;
}

DEFINE_PRIMITIVE("read-symlink", posix_readlink, subr1, (SCM name))
{
    if (!STRINGP(name)) STk_error("bad string ~S", name);

    struct stat sb;
    char *cname = STRING_CHARS(name);

    int e = lstat(cname, &sb);

    if (e) STk_error_posix(errno,"read-symlink",LIST1(name));

    /* SRFI-170 requires us to error based on errno.
       When a pathname exists but is not a link, readlink will
       set errno to EINVAL.
       The following would be more informative, but we stick to the SRFI.

      if ((sb.st_mode & S_IFMT) != S_IFLNK) STk_error("bad symlink ~S", name);
    */

    int bsize = sb.st_size+1;

    /* FROM MANPAGE OF readlink:
       Some magic symlinks under (for example) /proc and /sys
       report 'st_size' as zero. In that case, take PATH_MAX as
       a "good enough" estimate. */
    if  (sb.st_size == 0)
        bsize = PATH_MAX;

    char *buf = STk_must_malloc(bsize);
    if (buf == NULL) STk_error("cannot allocate memory for symlink resolved name ~S", name);

    e = readlink(cname, buf, bsize);

    /* if (e == bsize) printf("buffer was truncated\n");fflush(stdout); */
    if (e == -1) STk_error_posix(errno,"read-symlink",LIST1(name));

    /* readlink does not append the trailing zero! */
    buf[e]=0;

    return STk_Cstring2string(buf);
}

DEFINE_PRIMITIVE("rename-file", posix_rename,subr2, (SCM old, SCM new))
{
    if (!STRINGP(old)) STk_error("bad string ~S", old);
    if (!STRINGP(new)) STk_error("bad string ~S", new);
    char *cold = STRING_CHARS(old);
    char *cnew = STRING_CHARS(new);
    int e = rename(cold,cnew);

    if (e != 0)  STk_error_posix(errno,"rename-file",LIST2(old,new));
    return STk_void;
}

DEFINE_PRIMITIVE("delete-directory", posix_rmdir,subr1, (SCM dir))
{
    if (!STRINGP(dir)) STk_error("bad string ~S", dir);
    char *cdir = STRING_CHARS(dir);
    int e = rmdir(cdir);
    if (e != 0) STk_error_posix(errno,"delete-directory",LIST1(dir));
    return STk_void;
}

DEFINE_PRIMITIVE("truncate-file", posix_truncate, subr2, (SCM p, SCM length))
{
    int e;

    if (STRINGP(p)) { /* it's a path */
        e = truncate(STRING_CHARS(p), INT_VAL(length));
    } else if (FPORTP(p)) { /* it's a port */
        e = ftruncate(PORT_FD(PORT_STREAM(p)), INT_VAL(length));
    } else {
      STk_error("bad string or port ~S", p);
      return STk_void;     /* to avoid a compiler warning */
    }

    if (e != 0) STk_error_posix(errno,"truncate-file",LIST2(p,length));
    return STk_void;
}

DEFINE_PRIMITIVE("file-info", posix_stat, subr2, (SCM p, SCM follow))
{
    struct stat st;
    int e;

    errno = 0;
    if (STRINGP(p)) { /* it's a path */
        if (follow != STk_false)
            e = stat(STRING_CHARS(p), &st);
        else
            e = lstat(STRING_CHARS(p), &st);
    } else if (FPORTP(p)) { /* it's a port */
        e = fstat(PORT_FD(PORT_STREAM(p)), &st);
    } else {
      STk_error("bad string or port ~S", p);
      return STk_void;     /* to avoid a compiler warning */
    }

    if (e == -1) STk_error_posix(errno,"file-info",LIST2(p,follow));
    else {
        SCM ats[3]; /* atime */
        ats[2] = time_type;
        ats[1] = MAKE_INT(st.st_atim.tv_sec);
        ats[0] = MAKE_INT(st.st_atim.tv_nsec);

        SCM mts[3]; /* mtime */
        mts[2] = time_type;
        mts[1] = MAKE_INT(st.st_mtim.tv_sec);
        mts[0] = MAKE_INT(st.st_mtim.tv_nsec);

        SCM cts[3]; /* ctime */
        cts[2] = time_type;
        cts[1] = MAKE_INT(st.st_ctim.tv_sec);
        cts[0] = MAKE_INT(st.st_ctim.tv_nsec);

        SCM argv[17];
        argv[16] = file_info_type;
        argv[15] = MAKE_INT(st.st_dev);
        argv[14] = MAKE_INT(st.st_ino);
        argv[13] = MAKE_INT(st.st_mode);
        argv[12] = MAKE_INT(st.st_nlink);
        argv[11] = MAKE_INT(st.st_uid);
        argv[10] = MAKE_INT(st.st_gid);
        argv[9]  = MAKE_INT(st.st_dev);
        argv[8]  = MAKE_INT(st.st_size);
        argv[7]  = MAKE_INT(st.st_blksize);
        argv[6]  = MAKE_INT(st.st_blocks);
        argv[5]  = MAKE_INT(st.st_atim.tv_sec);
        argv[4]  = MAKE_INT(st.st_mtim.tv_sec);
        argv[3]  = MAKE_INT(st.st_ctim.tv_sec);
        argv[2]  = STk_make_struct(3, &ats[2]);
        argv[1]  = STk_make_struct(3, &mts[2]);
        argv[0]  = STk_make_struct(3, &cts[2]);
        return STk_make_struct(17, &argv[16]);
    }
    return STk_false; /* never reached */
}

void check_file_info(SCM info) {
    if (!STRUCTP(info)) STk_error("bad structure", info);
    if (STRUCT_TYPE(info) != file_info_type)
        STk_error("bad file-info structure", info);
}



DEFINE_PRIMITIVE("file-info-directory?",posix_isdir,subr1,(SCM info, SCM mode))
{
    check_file_info(info);
    if (INTP(mode)) STk_error("bad integer ~S", mode);
    SCM m = STk_int_struct_ref(info,STk_intern("mode"));
    return MAKE_BOOLEAN(S_ISDIR(INT_VAL(m)));
}

DEFINE_PRIMITIVE("file-info-fifo?",posix_isfifo,subr1,(SCM info, SCM mode))
{
    check_file_info(info);
    if (INTP(mode)) STk_error("bad integer ~S", mode);
    SCM m = STk_int_struct_ref(info,STk_intern("mode"));
    return MAKE_BOOLEAN(S_ISFIFO(INT_VAL(m)));
}

DEFINE_PRIMITIVE("file-info-symlink?",posix_issymlink,subr1,(SCM info, SCM mode))
{
    check_file_info(info);
    if (INTP(mode)) STk_error("bad integer ~S", mode);
    SCM m = STk_int_struct_ref(info,STk_intern("mode"));
    return MAKE_BOOLEAN(S_ISLNK(INT_VAL(m)));
}

DEFINE_PRIMITIVE("file-info-regular?",posix_isregular,subr1,(SCM info, SCM mode))
{
    check_file_info(info);
    if (INTP(mode)) STk_error("bad integer ~S", mode);
    SCM m = STk_int_struct_ref(info,STk_intern("mode"));
    return MAKE_BOOLEAN(S_ISREG(INT_VAL(m)));
}

DEFINE_PRIMITIVE("file-info-socket?",posix_issocket,subr1,(SCM info, SCM mode))
{
    check_file_info(info);
    if (INTP(mode)) STk_error("bad integer ~S", mode);
    SCM m = STk_int_struct_ref(info,STk_intern("mode"));
    return MAKE_BOOLEAN(S_ISFIFO(INT_VAL(m)));
}
DEFINE_PRIMITIVE("file-info-device?",posix_isdevice,subr1,(SCM info, SCM mode))
{
    check_file_info(info);
    if (INTP(mode)) STk_error("bad integer ~S", mode);
    SCM m = STk_int_struct_ref(info,STk_intern("mode"));
    int v = INT_VAL(m);
    return MAKE_BOOLEAN(S_ISCHR(v)||S_ISBLK(v));
}


DEFINE_PRIMITIVE("set-file-mode", posix_chmod, subr2, (SCM name, SCM bits) )
{
    if (!STRINGP(name)) STk_error("bad string ~S", name);
    if (!INTP(bits)) STk_error("bad integer ~S", bits);

    int e = chmod(STRING_CHARS(name), INT_VAL(bits));

    if (e == -1) STk_error_posix(errno,"set-file-mode",LIST2(name,bits));

    return STk_void;
}

DEFINE_PRIMITIVE("%mkstemp", posix_mkstemp, subr1, (SCM prefix) )
{
    if (!STRINGP(prefix)) STk_error("bad string ~S", prefix); /* shouldn't happen */

    /* prefix will be mutated! */
    int fd = mkstemp(STRING_CHARS(prefix));
    if (fd == -1) STk_error_posix(errno,"%mkstemp",LIST1(prefix));
    return prefix;
}

DEFINE_PRIMITIVE("%set-file-owner", posix_chown, subr3, (SCM name, SCM uid, SCM gid) )
{
    if (!STRINGP(name)) STk_error("bad string ~S", name);
    if (!INTP(uid)) STk_error("bad integer (uid) ~S", uid);
    if (!INTP(gid)) STk_error("bad integer (gid) ~S", gid);

    int e = chown(STRING_CHARS(name),
                  INT_VAL(uid),
                  INT_VAL(gid));

    if (e == -1) STk_error_posix(errno,"set-file-owner",LIST3(name,uid,gid));

    return STk_void;
}


DEFINE_PRIMITIVE("set-file-times",posix_utimensat, vsubr, (int argc, SCM *argv))
{
    /* FIXME: this implementation seems overly complicated. There must be easier
       ways to get it done. */
    if ((argc == 0) || (argc == 2) || (argc > 3))
        STk_error("expects exactly one or three arguments", STk_false);
    SCM name = *argv--;
    if (!STRINGP(name)) STk_error("bad string ~S", name);

    char *cname = STRING_CHARS(name);

    time_t sec1 = 0;        // FIXME: ::EG:: not sure.
    long   nsec1;
    time_t sec2 = 0;        // FIXME: ::EG:: not sure.
    long   nsec2;


    if (argc==1) {
        nsec1 = UTIME_NOW;
        nsec2 = UTIME_NOW;
    } else {
        SCM time1 = *argv--;
        SCM time2 = *argv--;

        if (SYMBOLP(time1)) {
            if      (STk_eqv(time1, symb_time_now) == STk_true)       nsec1 = UTIME_NOW;
            else if (STk_eqv(time1, symb_time_unchanged) == STk_true) nsec1 = UTIME_OMIT;
            else {
              STk_error("bad argument ~S",time1);
              return STk_void;     /* to avoid a compiler warning */
            }
        } else {
            if (!STRUCTP(time1)) STk_error("bad structure ~S",time1);
            if (STRUCT_TYPE(time1) != time_type) STk_error("bad time structure ~S",time1);
            sec1  = INT_VAL(STk_int_struct_ref(time1, symb_second));
            nsec1 = INT_VAL(STk_int_struct_ref(time1, symb_nanosecond));
        }

        if (SYMBOLP(time2)) {
            if      (STk_eqv(time2, symb_time_now) == STk_true)       nsec2 = UTIME_NOW;
            else if (STk_eqv(time2, symb_time_unchanged) == STk_true) nsec2 = UTIME_OMIT;
            else {
              STk_error("bad argument ~S",time2);
              return STk_void;     /* to avoid a compiler warning */
            }
        } else {
            if (!STRUCTP(time1)) STk_error("bad structure ~S",time2);
            if (STRUCT_TYPE(time2) != time_type) STk_error("bad time structure ~S",time2);
            sec2  = INT_VAL(STk_int_struct_ref(time2, symb_second));
            nsec2 = INT_VAL(STk_int_struct_ref(time2, symb_nanosecond));
        }
    }

    struct timespec t[2];
    t[0].tv_sec  = sec1;
    t[0].tv_nsec = nsec1;
    t[1].tv_sec  = sec2;
    t[1].tv_nsec = nsec2;

    int e = utimensat(AT_FDCWD,cname, t, 0);

    if (e == -1) STk_error_posix(errno,"set-file-times",STk_argv2list(argc,argv));

    return STk_void;
}



DEFINE_PRIMITIVE("open-directory", posix_opendir, subr12, (SCM name, SCM dot) )
{
    if (!STRINGP(name)) STk_error("bad string ~S", name);
    if (!dot) dot = STk_false;


    DIR *d = opendir(STRING_CHARS(name));
    if (d == 0) STk_error_posix(errno,"open-directory",LIST2(name,dot));

    SCM obj[3];
    obj[2] = dir_info_type;
    obj[1] = d;
    obj[0] = dot;
    return STk_make_struct(3,&obj[2]);
}

DEFINE_PRIMITIVE("read-directory", posix_readdir, subr1, (SCM dir) )
{
    if (!STRUCTP(dir)) STk_error("bad structure", STk_false);
    if (STRUCT_TYPE(dir) != dir_info_type)
        STk_error("bad directory structure", STk_false);

    DIR  *d = STk_int_struct_ref(dir,STk_intern("dir-object"));
    SCM dot = STk_int_struct_ref(dir,STk_intern("dot-files"));

    errno = 0;
    struct dirent *e = readdir(d);
    if (e == 0) {
        if (errno !=0) STk_error_posix(errno,"read-directory",LIST1(dir));
        else return STk_eof;
    }
    while (!strcmp(e->d_name,".")  ||
           !strcmp(e->d_name,"..") ||
           (dot == STk_false && !strncmp(e->d_name,".",1))) {
            e = readdir(d);
            if (e == 0) {
                if (errno !=0) STk_error_posix(errno,"read-directory",LIST1(dir));
                else return STk_eof;
            }
        }
    return STk_Cstring2string(e->d_name);
}

DEFINE_PRIMITIVE("close-directory", posix_closedir, subr1, (SCM dir) )
{
    if (!STRUCTP(dir)) STk_error("bad structure", STk_false);
    if (STRUCT_TYPE(dir) != dir_info_type)
        STk_error("bad directory structure", STk_false);
    DIR  *d = STk_int_struct_ref(dir,STk_intern("dir-object"));
    int e = closedir(d);
    if (e != 0) STk_error_posix(errno,"close-directory",LIST1(dir));
    return STk_void;
}

DEFINE_PRIMITIVE("real-path", posix_realpath, subr1, (SCM p) )
{
    char* pa;
    char* rpath = STk_must_malloc(PATH_MAX);

    if (!STRINGP(p)) STk_error("bad string ~S", p);

    pa = STRING_CHARS(p);

    /* I have tried "rpath = realpath(pa,NULL)", as per the manpage
       (man 3 realpath) but it would always return NULL and set errno
       to 22. Giving realpath an allocated buffer seems to work.
       -- jpellegrini */
    rpath = realpath(pa, rpath);

    if (rpath == NULL) {
        STk_error_posix(errno,"real-path",LIST1(p));
        return STk_void; /* never reached */
    } else
      return STk_Cstring2string(rpath);
}

DEFINE_PRIMITIVE("file-space", posix_statvfs, subr1, (SCM p))
{
    int e;
    struct statvfs st;

    /* We do not accept non-file ports as aspecial case (as Gauche
       seems to do, for example). If the behavior of POSIX is to
       return an EBADF error when passed std{in,out,err}, then we
       mimic that. */

    if (STRINGP(p)) { /* it's a path */
        e = statvfs(STRING_CHARS(p), &st);
    } else if (FPORTP(p)) { /* it's a port */
        e = fstatvfs(PORT_FD(PORT_STREAM(p)), &st);
    } else {
      STk_error("bad string or port ~S", p);
      return STk_void;     /* to avoid a compiler warning */
    }

    if (e != 0) STk_error_posix(errno,"file-space",LIST1(p));

    /* We first make Scheme objects, then multiply,
       in order to avoid a possible overflow. */
    return STk_mul2(MAKE_INT(st.f_bsize),MAKE_INT(st.f_bfree));
}

/* 3.5 Process state */

DEFINE_PRIMITIVE("umask", posix_umask, subr0, (void))
{
    mode_t u = umask(0);
    umask(u);
    return MAKE_INT(u);
}

DEFINE_PRIMITIVE("set-umask!", posix_set_umask, subr1, (SCM u))
{
    if (!INTP(u)) STk_error("Bad integer ~S", u);
    umask(INT_VAL(u));
    return STk_void;
}

DEFINE_PRIMITIVE("current-directory", posix_getcwd, subr0, (void))
{
  char buf[MAX_PATH_LENGTH], *s;
  s = getcwd(buf, MAX_PATH_LENGTH);
  if (!s) STk_error_posix(errno,"current-directory",STk_nil);
  return STk_Cstring2string(buf);
}

DEFINE_PRIMITIVE("set-current-directory!", posix_chdir, subr1, (SCM dir))
{
    if (!STRINGP(dir)) STk_error("bad string ~S", dir);
    int e = chdir(STRING_CHARS(dir));
    if (e != 0) STk_error_posix(errno,"set-current-directory!",LIST1(dir));
    return STk_void;
}


DEFINE_PRIMITIVE("nice", posix_nice, subr01, (SCM delta))
{
  int d = 1;

  if (delta) {
    if (!INTP(delta)) STk_error("bad integer ~S", delta);
    d = INT_VAL(delta);
  }
  int new_delta = nice(d);
  if (new_delta == -1)  STk_error_posix(errno,"nice", LIST1(delta));
  return MAKE_INT(new_delta);
}


DEFINE_PRIMITIVE("user-uid", posix_getuid, subr0, (void))
{
    /* getuid is always successful, don't check for errors */
    uid_t u = getuid();

    /* we *suppose* uid_t is always integer */
    return MAKE_INT(u);
}

DEFINE_PRIMITIVE("user-effective-uid", posix_geteuid, subr0, (void))
{
    /* geteuid is always successful, don't check for errors */
    uid_t u = geteuid();

    /* we *suppose* uid_t is always integer */
    return MAKE_INT(u);
}


DEFINE_PRIMITIVE("user-gid", posix_getgid, subr0, (void))
{
    /* getuid is always successful, don't check for errors */
    uid_t u = getgid();

    /* we *suppose* uid_t is always integer */
    return MAKE_INT(u);
}

DEFINE_PRIMITIVE("user-effective-gid", posix_getegid, subr0, (void))
{
    /* geteuid is always successful, don't check for errors */
    uid_t u = getegid();

    /* we *suppose* uid_t is always integer */
    return MAKE_INT(u);
}

DEFINE_PRIMITIVE("user-supplementary-gids", posix_getgroups, subr0, (void))
{
    gid_t *groups;
    int n = getgroups(0,NULL);

    groups = STk_must_malloc(sizeof(gid_t) * n);
    if (groups == 0) STk_error("memory allocation error");

    int gs = getgroups(n, groups);
    SCM list = STk_nil;

    if (gs != 0) {
        for (int i=0; i < n; i++) {
            list = STk_cons(MAKE_INT(*groups),list);
            groups++;
        }
    } else
        STk_error_posix(errno,"user-supplementary-gids",STk_nil);
    return(list);
}


/* 3.6 User and group database access */

DEFINE_PRIMITIVE("user-info",get_user_info, subr1, (SCM uid_name))
{
    struct passwd* info;

    errno = 0;
    if (STRINGP (uid_name))
        info = getpwnam(STRING_CHARS(uid_name));
    else if (INTP (uid_name)) /* FIXME: and positive */
        info = getpwuid(INT_VAL(uid_name));
    else {
        STk_error("not string or integer ~S", uid_name);
        return STk_void;     /* to avoid a compiler warning */
    }
    if (info == 0) {
        if (errno) STk_error_posix(errno,"user-info",LIST1(uid_name)); /* <- error */
        else return STk_false;                                         /* <- no error, user
                                                                          doesn't exist */
    } else {
        SCM argv[8];
        argv[7]=user_info_type;
        argv[6]=STk_Cstring2string(info->pw_name);
        argv[5]=MAKE_INT(info->pw_uid);
        argv[4]=MAKE_INT(info->pw_gid);
        argv[3]=STk_Cstring2string(info->pw_dir);
        argv[2]=STk_Cstring2string(info->pw_shell);
        argv[1]=STk_Cstring2string(info->pw_gecos);
        char *saveptr;
        char *t = strtok_r(info->pw_gecos,",", &saveptr);
        SCM name = STk_nil;
        while(t != 0) {
            name = STk_cons(STk_Cstring2string(t),name);
            t = strtok_r(0, ",", &saveptr);
        }
        argv[0]=name;
        return STk_make_struct(8, &argv[7]);
    }
    return STk_void; /* never reached */
}

DEFINE_PRIMITIVE("group-info",get_group_info, subr1, (SCM gid_name))
{
    struct group* info;

    errno = 0;
    if (STRINGP (gid_name))
        info = getgrnam(STRING_CHARS(gid_name));
    else if (INTP (gid_name)) /* FIXME: and positive */
        info = getgrgid(INT_VAL(gid_name));
    else {
        STk_error("not string or integer ~S", gid_name);
        return STk_void;     /* to avoid a compiler warning */
    }
    if (info == 0) {
        if (errno) STk_error_posix(errno,"group-info",LIST1(gid_name)); /* <- error */
        else return STk_false;                                          /* <- no error, group
                                                                           doesn't exist */
    } else {
        SCM argv[3];
        argv[2]=group_info_type;
        argv[1]=STk_Cstring2string(info->gr_name);
        argv[0]=MAKE_INT(info->gr_gid);
        return STk_make_struct(3, &argv[2]);
    }
    return STk_void; /* never reached */
}

/* 3.10 Time */

DEFINE_PRIMITIVE("posix-time",posix_time, subr0, (void))
{
    struct timespec ts;
    int e = clock_gettime(CLOCK_REALTIME, &ts);

    if (e==-1) STk_error_posix(errno,"posix-time",STk_nil);

    SCM argv[3];
    argv[2]=time_type;
    argv[1]=MAKE_INT(ts.tv_sec);
    argv[0]=MAKE_INT(ts.tv_nsec);
    return STk_make_struct(3, &argv[2]);
}

DEFINE_PRIMITIVE("monotonic-time",posix_monotonic_time, subr0, (void))
{
    struct timespec ts;
    int e = clock_gettime(CLOCK_MONOTONIC, &ts);

    if (e==-1) STk_error_posix(errno,"monotonic-time",STk_nil);

    SCM argv[3];
    argv[2]=time_type;
    argv[1]=MAKE_INT(ts.tv_sec);
    argv[0]=MAKE_INT(ts.tv_nsec);
    return STk_make_struct(3, &argv[2]);
}

/* 3.12 Terminal device control */

DEFINE_PRIMITIVE("terminal?",posix_isatty, subr1, (SCM port))
{
    if (!PORTP(port)) STk_error("bad port ~S", port);
    if (!FPORTP(port)) return STk_false;
    int fd = PORT_FD(PORT_STREAM(port));
    int e = isatty(fd);

    if (e == 1) return STk_true;
    if (errno == ENOTTY) return STk_false;
    STk_error_posix(errno,"terminal?",LIST1(port));
    return STk_false; /* never reached */
}



/* -----------------
   MODULE
   ----------------- */

MODULE_ENTRY_START("srfi-170")

{
  SCM module =  STk_create_module(STk_intern("SRFI-170"));

  initialize_global_symbols();

  /* 3.1 Error handling */

  posix_error = STk_defcond_type("&posix-error",
                                 STk_err_mess_condition,
                                 LIST2(STk_intern("errname"), STk_intern("errno")),
                                 module);

  ADD_PRIMITIVE_IN_MODULE(posix_strerror,module);
  ADD_PRIMITIVE_IN_MODULE(get_posix_error_name,module);

  /* 3.2 I/O */

  ADD_PRIMITIVE_IN_MODULE(posix_open,module);
  ADD_PRIMITIVE_IN_MODULE(posix_fd_port,module);

  /* R,W,R+W should be specified in mode, but let's define the flags, since
     the tests use them */
  STk_define_variable(STk_intern("open/read"),      MAKE_INT(O_RDONLY), module);
  STk_define_variable(STk_intern("open/write"),     MAKE_INT(O_WRONLY), module);
  STk_define_variable(STk_intern("open/read+write"),  MAKE_INT(O_RDWR), module);

  STk_define_variable(STk_intern("open/append"),    MAKE_INT(O_APPEND), module);
  STk_define_variable(STk_intern("open/create"),    MAKE_INT(O_CREAT), module);
  STk_define_variable(STk_intern("open/exclusive"), MAKE_INT(O_EXCL), module);
  STk_define_variable(STk_intern("open/nofollow"),  MAKE_INT(O_NOFOLLOW), module);
  STk_define_variable(STk_intern("open/truncate"),  MAKE_INT(O_TRUNC), module);

  /* 3.3 File System */

  file_info_type = STk_make_struct_type(STk_intern("%file-info"),
                                        STk_false,
                                        STk_append2(
                                            LIST6(STk_intern("device"),
                                                  STk_intern("inode"),
                                                  STk_intern("mode"),
                                                  STk_intern("nlinks"),
                                                  STk_intern("uid"),
                                                  STk_intern("gid")),
                                            LIST10(STk_intern("rdev"),
                                                   STk_intern("size"),
                                                   STk_intern("blksize"),
                                                   STk_intern("blocks"),
                                                   STk_intern("atime"),
                                                   STk_intern("mtime"),
                                                   STk_intern("ctime"),
                                                   STk_intern("atim"),
                                                   STk_intern("mtim"),
                                                   STk_intern("ctim"))));

  STk_define_variable(STk_intern("%file-info"), file_info_type, module);

  dir_info_type = STk_make_struct_type(STk_intern("%directory-info"),
                                       STk_false,
                                       LIST2(STk_intern("dir-object"),
                                             STk_intern("dot-files")));

  STk_define_variable(STk_intern("%directory-info"), dir_info_type, module);


  ADD_PRIMITIVE_IN_MODULE(posix_mkdir,module);
  ADD_PRIMITIVE_IN_MODULE(posix_mkfifo,module);
  ADD_PRIMITIVE_IN_MODULE(posix_link,module);
  ADD_PRIMITIVE_IN_MODULE(posix_rename,module);
  ADD_PRIMITIVE_IN_MODULE(posix_rmdir,module);
  ADD_PRIMITIVE_IN_MODULE(posix_utimensat,module);
  ADD_PRIMITIVE_IN_MODULE(posix_mkstemp,module);
  ADD_PRIMITIVE_IN_MODULE(posix_readlink,module);
  ADD_PRIMITIVE_IN_MODULE(posix_symlink,module);
  ADD_PRIMITIVE_IN_MODULE(posix_truncate, module);
  ADD_PRIMITIVE_IN_MODULE(posix_stat, module);
  ADD_PRIMITIVE_IN_MODULE(posix_opendir, module);
  ADD_PRIMITIVE_IN_MODULE(posix_readdir, module);
  ADD_PRIMITIVE_IN_MODULE(posix_closedir, module);
  ADD_PRIMITIVE_IN_MODULE(posix_realpath, module);
  ADD_PRIMITIVE_IN_MODULE(posix_chmod, module);
  ADD_PRIMITIVE_IN_MODULE(posix_chown, module);
  ADD_PRIMITIVE_IN_MODULE(posix_statvfs, module);

  ADD_PRIMITIVE_IN_MODULE(posix_isdir, module);
  ADD_PRIMITIVE_IN_MODULE(posix_isfifo, module);
  ADD_PRIMITIVE_IN_MODULE(posix_issymlink, module);
  ADD_PRIMITIVE_IN_MODULE(posix_isregular, module);
  ADD_PRIMITIVE_IN_MODULE(posix_issocket, module);
  ADD_PRIMITIVE_IN_MODULE(posix_isdevice, module);

  /* 3.5 Process state */

  ADD_PRIMITIVE_IN_MODULE(posix_umask, module);
  ADD_PRIMITIVE_IN_MODULE(posix_set_umask, module);
  ADD_PRIMITIVE_IN_MODULE(posix_getcwd, module);
  ADD_PRIMITIVE_IN_MODULE(posix_chdir, module);
  ADD_PRIMITIVE_IN_MODULE(posix_nice, module);
  ADD_PRIMITIVE_IN_MODULE(posix_getuid, module);
  ADD_PRIMITIVE_IN_MODULE(posix_geteuid, module);
  ADD_PRIMITIVE_IN_MODULE(posix_getgid, module);
  ADD_PRIMITIVE_IN_MODULE(posix_getegid, module);
  ADD_PRIMITIVE_IN_MODULE(posix_getgroups,module);

  /* 3.6  User and group database access */

  user_info_type = STk_make_struct_type(STk_intern("%user-info"),
                                         STk_false,
                                         LIST7(STk_intern("name"),
                                               STk_intern("uid"),
                                               STk_intern("gid"),
                                               STk_intern("home-dir"),
                                               STk_intern("shell"),
                                               STk_intern("full-name"),
                                               STk_intern("parsed-full-name")));

  STk_define_variable(STk_intern("%user-info"), user_info_type, module);

  group_info_type = STk_make_struct_type(STk_intern("%group-info"),
                                         STk_false,
                                         LIST2(STk_intern("name"),
                                               STk_intern("gid")));

  STk_define_variable(STk_intern("%group-info"), group_info_type, module);

  ADD_PRIMITIVE_IN_MODULE(get_group_info,module);
  ADD_PRIMITIVE_IN_MODULE(get_user_info,module);


  /* 3.10 Time */

  /* We need to make reference to structure %time: */
  SCM ref;
  time_type = STk_lookup(STk_intern("%time"), STk_STklos_module,&ref,TRUE);

  ADD_PRIMITIVE_IN_MODULE(posix_time,module);
  ADD_PRIMITIVE_IN_MODULE(posix_monotonic_time,module);


  /* 3.12 Terminal device control */

  ADD_PRIMITIVE_IN_MODULE(posix_isatty, module);

  /* Export all the symbols we have just defined */
  STk_export_all_symbols(module);

}
MODULE_ENTRY_END
