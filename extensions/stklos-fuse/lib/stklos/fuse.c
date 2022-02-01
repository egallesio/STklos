/* -*- coding: utf-8 -*-
 *
 * fuse.c                       -- Interface wrapper for FUSE
 *
 * Copyright © 2005-2022 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 *    Creation date:  7-Dec-2005 11:30 (eg)
 * Last file update:  1-Feb-2022 17:31 (eg)
 */


//
// FUSE Configuration
//
#define FUSE_USE_VERSION   26     /* version of the API we want to use */
#define _FILE_OFFSET_BITS  64
#include <fuse.h>

/* ---------------------------------------------------------------------- */
#include "stklos.h"
#include "fuse-incl.c"

static struct user_operations {
  SCM getattr;
  SCM readlink;
  SCM mknod;
  SCM mkdir;
  SCM unlink;
  SCM rmdir;
  SCM symlink;
  SCM rename;
  SCM link;
  SCM chmod;
  SCM chown;
  SCM truncate;
  SCM utime;
  SCM open;
  SCM read;
  SCM write;
  SCM statfs;
  SCM flush;
  SCM release;
  SCM fsync;
  SCM setxattr;
  SCM getxattr;
  SCM listxattr;
  SCM removexattr;
  SCM opendir;
  SCM readdir;
  SCM releasedir;
  SCM fsyncdir;
  SCM init;
  SCM destroy;
} user_fops;

static int error_bad_value(char *func)
{
  fprintf(stderr, "**** Fuse: Implementation of '%s' returned an incorrect value\n",
          func);
  fflush(stderr);
  return EINVAL;
}

/* ----------------------------------------------------------------------
 *
 *      Fuse trampoline operations ...
 *
 * ----------------------------------------------------------------------
 */

/* ---------------------------------------------------------------------- *\
 *      f_getattr ...
\* ---------------------------------------------------------------------- */
static int f_getattr(const char *path, struct stat *stbuf)
{
  SCM res;

  if (user_fops.getattr == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.getattr, 1, STk_Cstring2string((char*) path));

  if (INTP(res))
    return INT_VAL(res);

  if (VECTORP(res) && (VECTOR_SIZE(res) == 8)) {
    memset(stbuf, 0, sizeof(struct stat));
    stbuf->st_mode    = STk_integer_value(VECTOR_DATA(res)[0]);
    stbuf->st_nlink   = STk_integer_value(VECTOR_DATA(res)[1]);
    stbuf->st_size    = STk_integer_value(VECTOR_DATA(res)[2]);
    stbuf->st_uid     = STk_integer_value(VECTOR_DATA(res)[3]);
    stbuf->st_gid     = STk_integer_value(VECTOR_DATA(res)[4]);
    stbuf->st_atime   = STk_uinteger_value(VECTOR_DATA(res)[5]);
    stbuf->st_mtime   = STk_uinteger_value(VECTOR_DATA(res)[6]);
    stbuf->st_ctime   = STk_uinteger_value(VECTOR_DATA(res)[7]);
    /* We arbritarily fix that the block size id 1024 bytes */
    stbuf->st_blocks  = (blkcnt_t) ((2 * stbuf->st_size + 1023) / 1024);
    return 0;
  }
  return error_bad_value("getattr");
}


/* ---------------------------------------------------------------------- *\
 *      f_readdir ...
\* ---------------------------------------------------------------------- */
static int f_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
                     off_t _UNUSED(offset), struct fuse_file_info _UNUSED(*fi))
{
  SCM spath = STk_Cstring2string((char*) path);
  SCM l, res;

  if (user_fops.readdir == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.readdir, 1, spath);

  if (INTP(res))
    return INT_VAL(res);

  if (CONSP(res)) {
    for (l = res; !NULLP(l); l = CDR(l)) {
      if (!STRINGP(CAR(l)))
        STk_error("Bad string in fuse readdir ~S", CAR(l));
      filler(buf, STRING_CHARS(CAR(l)), NULL, 0);
    }
    return 0;
  }
  return error_bad_value("readdir");
}

/* ---------------------------------------------------------------------- *\
 *      f_open ...
\* ---------------------------------------------------------------------- */
static int f_open(const char *path, struct fuse_file_info *fi)
{
  static unsigned long handle = 0;
  SCM spath = STk_Cstring2string((char*) path);
  SCM res;

  if (user_fops.open == STk_false) return -ENOSYS;

  fi->fh = handle++;
  res    = STk_C_apply(user_fops.open, 3, spath, MAKE_INT(fi->flags & 3),
                       STk_ulong2integer(fi->fh));
  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("open");
}

/* ---------------------------------------------------------------------- *\
 *      f_read ...
\* ---------------------------------------------------------------------- */
static int f_read(const char _UNUSED(*path), char *buf, size_t size, off_t offset,
                  struct fuse_file_info *fi)
{
  SCM res;

  if (user_fops.read == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.read, 3, STk_ulong2integer(fi->fh),
                    MAKE_INT(size), MAKE_INT(offset));

  if (INTP(res))
    return INT_VAL(res);
  if (STRINGP(res)) {
    memcpy(buf, STRING_CHARS(res), STRING_SIZE(res));
    return STRING_SIZE(res);
  }
  return error_bad_value("read");
}

/* ---------------------------------------------------------------------- *\
 *      f_write ...
\* ---------------------------------------------------------------------- */
static int f_write(const char _UNUSED(*path), const char *buf, size_t size,
                   off_t offset, struct fuse_file_info *fi)
{
  SCM tmp, res;

  if (user_fops.write == STk_false) return -ENOSYS;

  tmp = STk_makestring(size, NULL);
  memcpy(STRING_CHARS(tmp), buf, size);

  res = STk_C_apply(user_fops.write, 4, STk_ulong2integer(fi->fh), tmp,
                    MAKE_INT(size), MAKE_INT(offset));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("write");
}


/* ---------------------------------------------------------------------- *\
 *      f_mknod ...
\* ---------------------------------------------------------------------- */
static int f_mknod(const char *path, mode_t mode, dev_t _UNUSED(rdev))
{
  SCM res;

  if (user_fops.mknod == STk_false) return -ENOSYS;

  if (!S_ISREG(mode))
    /* Accept only regular file creation with mknod */
    return -EINVAL;

  res = STk_C_apply(user_fops.mknod, 2, STk_Cstring2string((char *) path),
                    MAKE_INT(mode));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("mknod");
}

/* ---------------------------------------------------------------------- *\
 *      f_rename ...
\* ---------------------------------------------------------------------- */
static int f_rename(const char *from, const char *to)
{
  SCM res;

  if (user_fops.rename == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.rename, 2, STk_Cstring2string((char *) from),
                    STk_Cstring2string((char *) to));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("rename");
}

/* ---------------------------------------------------------------------- *\
 *      f_link ...
\* ---------------------------------------------------------------------- */
static int f_link(const char *old, const char *new)
{
  SCM res;

  if (user_fops.link == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.link, 2, STk_Cstring2string((char *) old),
                    STk_Cstring2string((char *) new));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("link");
}


/* ---------------------------------------------------------------------- *\
 *      f_unlink ...
\* ---------------------------------------------------------------------- */
static int f_unlink(const char *path)
{
  SCM res;

  if (user_fops.unlink == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.unlink, 1, STk_Cstring2string((char *) path));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("unlink");
}


/* ---------------------------------------------------------------------- *\
 *      f_symlink ...
\* ---------------------------------------------------------------------- */
static int f_symlink(const char *old, const char *new)
{
  SCM res;

  if (user_fops.symlink == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.symlink, 2, STk_Cstring2string((char *) old),
                    STk_Cstring2string((char *) new));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("symlink");
}

/* ---------------------------------------------------------------------- *\
 *      f_readlink ...
\* ---------------------------------------------------------------------- */
static int f_readlink(const char *path, char *buf, size_t bufsiz)
{
  SCM res;

  if (user_fops.readlink == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.readlink, 1, STk_Cstring2string((char *) path));

  if (INTP(res))
    return INT_VAL(res);

  if (STRINGP (res)) {
    size_t len = (size_t) STRING_SIZE(res);

    if (len >= bufsiz-1) {
      memcpy(buf, STRING_CHARS(res), bufsiz - 1);
      buf[bufsiz-1] = '\0';
      return -ENAMETOOLONG;
    } else {
      memcpy(buf, STRING_CHARS(res), len);
      buf[len] = '\0';
      return 0;
    }
  }
  return error_bad_value("readlink");
}


/* ---------------------------------------------------------------------- *\
 *      f_mkdir ...
\* ---------------------------------------------------------------------- */
static int f_mkdir(const char *path, mode_t mode)
{
  SCM res;

  if (user_fops.mkdir == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.mkdir, 2, STk_Cstring2string((char *) path),
                    MAKE_INT(mode));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("mkdir");
}

/* ---------------------------------------------------------------------- *\
 *      f_rmdir ...
\* ---------------------------------------------------------------------- */
static int f_rmdir(const char *path)
{
  SCM res;

  if (user_fops.rmdir == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.rmdir, 1, STk_Cstring2string((char *) path));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("rmdir");
}

/* ---------------------------------------------------------------------- *\
 *      f_utime ...
\* ---------------------------------------------------------------------- */
static int f_utime(const char *path, struct utimbuf *buf)
{
  SCM res;

  if (user_fops.utime == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.utime, 3, STk_Cstring2string((char*) path),
                    STk_ulong2integer(buf->actime),
                    STk_ulong2integer(buf->modtime));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("utime");
}

/* ---------------------------------------------------------------------- *\
 *      f_chown ...
\* ---------------------------------------------------------------------- */
static int f_chown(const char *path, uid_t uid, gid_t gid)
{
  SCM res;

  if (user_fops.chown == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.chown, 3, STk_Cstring2string((char*) path),
                    STk_ulong2integer(uid),
                    STk_ulong2integer(gid));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("chown");
}

/* ---------------------------------------------------------------------- *\
 *      f_chmod ...
\* ---------------------------------------------------------------------- */
static int f_chmod(const char *path, mode_t mode)
{
  SCM res;

  if (user_fops.chmod == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.chmod, 2, STk_Cstring2string((char*) path),
                    STk_ulong2integer(mode));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("chmod");
}

/* ---------------------------------------------------------------------- *\
 *      f_truncate ...
\* ---------------------------------------------------------------------- */
static int f_truncate(const char *path, off_t offset)
{
  SCM res;

  if (user_fops.truncate == STk_false) return -ENOSYS;

  res = STk_C_apply(user_fops.truncate, 2, STk_Cstring2string((char*) path),
                    STk_ulong2integer(offset));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("truncate");
}


/* ---------------------------------------------------------------------- *\
 *      f_flush ...
\* ---------------------------------------------------------------------- */
static int f_flush(const char *path, struct fuse_file_info *fi)
{
  SCM res;

  if (user_fops.flush == STk_false) return 0;

  res = STk_C_apply(user_fops.flush, 2, STk_Cstring2string((char*) path),
                    STk_ulong2integer(fi->fh));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("flush");
}


/* ---------------------------------------------------------------------- *\
 *      f_release ...
\* ---------------------------------------------------------------------- */
static int f_release(const char _UNUSED(*path), struct fuse_file_info _UNUSED(*fi))
{
  if (user_fops.release == STk_false) return 0;

  STk_C_apply(user_fops.release, 1, STk_ulong2integer(fi->fh));
  return 0;
}


/* ---------------------------------------------------------------------- *\
 *      f_fsync ...
\* ---------------------------------------------------------------------- */
static int f_fsync(const char *path, int datasync, struct fuse_file_info *fi)
{
  SCM res;

  if (user_fops.fsync == STk_false) return 0;

  res = STk_C_apply(user_fops.fsync, 3, STk_Cstring2string((char*) path),
                    MAKE_INT(datasync), STk_ulong2integer(fi->fh));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("fsync");
}

/* ---------------------------------------------------------------------- *\
 *      f_opendir ...
\* ---------------------------------------------------------------------- */
static int f_opendir(const char *path, struct fuse_file_info _UNUSED(*fi))
{
  SCM res;

  if (user_fops.opendir == STk_false) return 0;

  res = STk_C_apply(user_fops.opendir, 1, STk_Cstring2string((char*) path));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("opendir");
}

/* ---------------------------------------------------------------------- *\
 *      f_releasedir ...
\* ---------------------------------------------------------------------- */
static int f_releasedir(const char *path, struct fuse_file_info _UNUSED(*fi))
{
  SCM res;

  if (user_fops.releasedir == STk_false) return 0;

  res = STk_C_apply(user_fops.releasedir, 1, STk_Cstring2string((char*) path));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("releasedir");
}

/* ---------------------------------------------------------------------- *\
 *      f_syncdir ...
\* ---------------------------------------------------------------------- */
static int f_fsyncdir(const char *path, int datasync,
                      struct fuse_file_info _UNUSED(*fi))
{
  SCM res;

  if (user_fops.fsyncdir == STk_false) return 0;

  res = STk_C_apply(user_fops.fsyncdir, 2, STk_Cstring2string((char*) path),
                    MAKE_INT(datasync));

  if (INTP(res))
    return INT_VAL(res);
  return error_bad_value("fsyncdir");
}

/* ---------------------------------------------------------------------- *\
 *      f_init ...
\* ---------------------------------------------------------------------- */
static void *f_init(struct fuse_conn_info _UNUSED(*conn))
{
  if (user_fops.init == STk_false) return STk_false;

  /* STk_debug("major %d minor %d async-read %d max-write max-read %d\n",
            conn->proto_major, conn->proto_minor, conn->async_read,
            conn->max_write, conn->max_readahead);
  */
  return (void *) STk_C_apply(user_fops.init, 0);
}

/* ---------------------------------------------------------------------- *\
 *      f_destroy ...
\* ---------------------------------------------------------------------- */
static void f_destroy(void *arg)
{
  if (user_fops.destroy != STk_false)
    STk_C_apply(user_fops.destroy, 1, (SCM) arg);
}



static struct fuse_operations my_fops = {
  .getattr      = f_getattr,
  .readdir      = f_readdir,
  .open         = f_open,
  .read         = f_read,
  .write        = f_write,
  .mknod        = f_mknod,
  .rename       = f_rename,
  .unlink       = f_unlink,
  .mkdir        = f_mkdir,
  .rmdir        = f_rmdir,
  .link         = f_link,
  .symlink      = f_symlink,
  .readlink     = f_readlink,
  .utime        = f_utime,
  .chown        = f_chown,
  .chmod        = f_chmod,
  .truncate     = f_truncate,
  .flush        = f_flush,
  .release      = f_release,
  .fsync        = f_fsync,
  .opendir      = f_opendir,
  .releasedir   = f_releasedir,
  .fsyncdir     = f_fsyncdir,
  .init         = f_init,
  .destroy      = f_destroy,
};


/* ----------------------------------------------------------------------
 *
 *      Fuse table of operations ...
 *
 * ----------------------------------------------------------------------
 */

#define FOPS_NUMBER_OF_OPERATIONS 30

static struct user_operations user_fops;


DEFINE_PRIMITIVE("%fuse-mount", f_mount, subr2, (SCM argv, SCM ops))
{
  int i, len;
  char **args;
  SCM *tmp;

  if (!VECTORP (argv))
    STk_error("bad vector for arguments ~S", argv);
  if (!VECTORP(ops) || VECTOR_SIZE(ops) != FOPS_NUMBER_OF_OPERATIONS)
    STk_error("bad vector of operations ~S", ops);

  /* Allocate the arguments vector */
  len  = VECTOR_SIZE(argv);
  args = STk_must_malloc_atomic(sizeof(char*) * (len + 1));

  for (i=0, tmp=VECTOR_DATA(argv); i < len; i++, tmp++) {
    if (!STRINGP(*tmp))
      STk_error("bad argument ~S", *tmp);
    else
      args[i] = STRING_CHARS(*tmp);
  }
  args[len] = NULL;

  /* Fill in the operation structure */
  user_fops.getattr     = VECTOR_DATA(ops)[0];
  user_fops.readlink    = VECTOR_DATA(ops)[1];
  user_fops.mknod       = VECTOR_DATA(ops)[2];
  user_fops.mkdir       = VECTOR_DATA(ops)[3];
  user_fops.unlink      = VECTOR_DATA(ops)[4];
  user_fops.rmdir       = VECTOR_DATA(ops)[5];
  user_fops.symlink     = VECTOR_DATA(ops)[6];
  user_fops.rename      = VECTOR_DATA(ops)[7];
  user_fops.link        = VECTOR_DATA(ops)[8];
  user_fops.chmod       = VECTOR_DATA(ops)[9];
  user_fops.chown       = VECTOR_DATA(ops)[10];
  user_fops.truncate    = VECTOR_DATA(ops)[11];
  user_fops.utime       = VECTOR_DATA(ops)[12];
  user_fops.open        = VECTOR_DATA(ops)[13];
  user_fops.read        = VECTOR_DATA(ops)[14];
  user_fops.write       = VECTOR_DATA(ops)[15];
  user_fops.statfs      = VECTOR_DATA(ops)[16];
  user_fops.flush       = VECTOR_DATA(ops)[17];
  user_fops.release     = VECTOR_DATA(ops)[18];
  user_fops.fsync       = VECTOR_DATA(ops)[19];
  user_fops.setxattr    = VECTOR_DATA(ops)[20];
  user_fops.getxattr    = VECTOR_DATA(ops)[21];
  user_fops.listxattr   = VECTOR_DATA(ops)[22];
  user_fops.removexattr = VECTOR_DATA(ops)[23];
  user_fops.opendir     = VECTOR_DATA(ops)[24];
  user_fops.readdir     = VECTOR_DATA(ops)[25];
  user_fops.releasedir  = VECTOR_DATA(ops)[26];
  user_fops.fsyncdir    = VECTOR_DATA(ops)[27];
  user_fops.init        = VECTOR_DATA(ops)[28];
  user_fops.destroy     = VECTOR_DATA(ops)[29];

  fuse_main(len, args, &my_fops, NULL);
  return STk_void;
}


/* ----------------------------------------------------------------------
 *
 *      POSIX symbolic names ...
 *
 * ----------------------------------------------------------------------
 */
static void def_errno(char *name, long value, SCM module, SCM *lst)
{
  SCM symb = STk_intern(name);

  STk_define_variable(symb, MAKE_INT(value), module);
  *lst = STk_cons(symb, *lst);
}

#define DEF_ERRNO(err)        {def_errno(#err, err, module, &lst);}

static void init_posix_errno(SCM module)
{
  /* Since the errno values are sytem dependent, we define here some of the
   * most used symbolic names. The names come from the file
   * "/usr/include/asm-generic/errno-base.h" on a Linux distribution. All the
   * needed values should be here, and are general enough to be present
   * almost everywhere ...
   */

  SCM lst = STk_nil;

#ifdef E2BIG
  DEF_ERRNO(E2BIG);
#endif
#ifdef EACCES
  DEF_ERRNO(EACCES);
#endif
#ifdef EADDRINUSE
  DEF_ERRNO(EADDRINUSE);
#endif
#ifdef EADDRNOTAVAIL
  DEF_ERRNO(EADDRNOTAVAIL);
#endif
#ifdef EAFNOSUPPORT
  DEF_ERRNO(EAFNOSUPPORT);
#endif
#ifdef EAGAIN
  DEF_ERRNO(EAGAIN);
#endif
#ifdef EALREADY
  DEF_ERRNO(EALREADY);
#endif
#ifdef EBADE
  DEF_ERRNO(EBADE);
#endif
#ifdef EBADF
  DEF_ERRNO(EBADF);
#endif
#ifdef EBADFD
  DEF_ERRNO(EBADFD);
#endif
#ifdef EBADMSG
  DEF_ERRNO(EBADMSG);
#endif
#ifdef EBADR
  DEF_ERRNO(EBADR);
#endif
#ifdef EBADRQC
  DEF_ERRNO(EBADRQC);
#endif
#ifdef EBADSLT
  DEF_ERRNO(EBADSLT);
#endif
#ifdef EBUSY
  DEF_ERRNO(EBUSY);
#endif
#ifdef ECANCELED
  DEF_ERRNO(ECANCELED);
#endif
#ifdef ECHILD
  DEF_ERRNO(ECHILD);
#endif
#ifdef ECHRNG
  DEF_ERRNO(ECHRNG);
#endif
#ifdef ECOMM
  DEF_ERRNO(ECOMM);
#endif
#ifdef ECONNABORTED
  DEF_ERRNO(ECONNABORTED);
#endif
#ifdef ECONNREFUSED
  DEF_ERRNO(ECONNREFUSED);
#endif
#ifdef ECONNRESET
  DEF_ERRNO(ECONNRESET);
#endif
#ifdef EDEADLK
  DEF_ERRNO(EDEADLK);
#endif
#ifdef EDESTADDRREQ
  DEF_ERRNO(EDESTADDRREQ);
#endif
#ifdef EDOM
  DEF_ERRNO(EDOM);
#endif
#ifdef EDQUOT
  DEF_ERRNO(EDQUOT);
#endif
#ifdef EEXIST
  DEF_ERRNO(EEXIST);
#endif
#ifdef EFAULT
  DEF_ERRNO(EFAULT);
#endif
#ifdef EFBIG
  DEF_ERRNO(EFBIG);
#endif
#ifdef EHOSTDOWN
  DEF_ERRNO(EHOSTDOWN);
#endif
#ifdef EHOSTUNREACH
  DEF_ERRNO(EHOSTUNREACH);
#endif
#ifdef EHWPOISON
  DEF_ERRNO(EHWPOISON);
#endif
#ifdef EIDRM
  DEF_ERRNO(EIDRM);
#endif
#ifdef EILSEQ
  DEF_ERRNO(EILSEQ);
#endif
#ifdef EINPROGRESS
  DEF_ERRNO(EINPROGRESS);
#endif
#ifdef EINTR
  DEF_ERRNO(EINTR);
#endif
#ifdef EINVAL
  DEF_ERRNO(EINVAL);
#endif
#ifdef EIO
  DEF_ERRNO(EIO);
#endif
#ifdef EISCONN
  DEF_ERRNO(EISCONN);
#endif
#ifdef EISDIR
  DEF_ERRNO(EISDIR);
#endif
#ifdef EISNAM
  DEF_ERRNO(EISNAM);
#endif
#ifdef EKEYEXPIRED
  DEF_ERRNO(EKEYEXPIRED);
#endif
#ifdef EKEYREJECTED
  DEF_ERRNO(EKEYREJECTED);
#endif
#ifdef EKEYREVOKED
  DEF_ERRNO(EKEYREVOKED);
#endif
#ifdef EL2HLT
  DEF_ERRNO(EL2HLT);
#endif
#ifdef EL2NSYNC
  DEF_ERRNO(EL2NSYNC);
#endif
#ifdef EL3HLT
  DEF_ERRNO(EL3HLT);
#endif
#ifdef EL3RST
  DEF_ERRNO(EL3RST);
#endif
#ifdef ELIBACC
  DEF_ERRNO(ELIBACC);
#endif
#ifdef ELIBBAD
  DEF_ERRNO(ELIBBAD);
#endif
#ifdef ELIBMAX
  DEF_ERRNO(ELIBMAX);
#endif
#ifdef ELIBSCN
  DEF_ERRNO(ELIBSCN);
#endif
#ifdef ELIBEXEC
  DEF_ERRNO(ELIBEXEC);
#endif
#ifdef ELNRANGE
  DEF_ERRNO(ELNRANGE);
#endif
#ifdef ELOOP
  DEF_ERRNO(ELOOP);
#endif
#ifdef EMEDIUMTYPE
  DEF_ERRNO(EMEDIUMTYPE);
#endif
#ifdef EMFILE
  DEF_ERRNO(EMFILE);
#endif
#ifdef EMLINK
  DEF_ERRNO(EMLINK);
#endif
#ifdef EMSGSIZE
  DEF_ERRNO(EMSGSIZE);
#endif
#ifdef EMULTIHOP
  DEF_ERRNO(EMULTIHOP);
#endif
#ifdef ENAMETOOLONG
  DEF_ERRNO(ENAMETOOLONG);
#endif
#ifdef ENETDOWN
  DEF_ERRNO(ENETDOWN);
#endif
#ifdef ENETRESET
  DEF_ERRNO(ENETRESET);
#endif
#ifdef ENETUNREACH
  DEF_ERRNO(ENETUNREACH);
#endif
#ifdef ENFILE
  DEF_ERRNO(ENFILE);
#endif
#ifdef ENOANO
  DEF_ERRNO(ENOANO);
#endif
#ifdef ENOBUFS
  DEF_ERRNO(ENOBUFS);
#endif
#ifdef ENODATA
  DEF_ERRNO(ENODATA);
#endif
#ifdef ENODEV
  DEF_ERRNO(ENODEV);
#endif
#ifdef ENOENT
  DEF_ERRNO(ENOENT);
#endif
#ifdef ENOEXEC
  DEF_ERRNO(ENOEXEC);
#endif
#ifdef ENOKEY
  DEF_ERRNO(ENOKEY);
#endif
#ifdef ENOLCK
  DEF_ERRNO(ENOLCK);
#endif
#ifdef ENOLINK
  DEF_ERRNO(ENOLINK);
#endif
#ifdef ENOMEDIUM
  DEF_ERRNO(ENOMEDIUM);
#endif
#ifdef ENOMEM
  DEF_ERRNO(ENOMEM);
#endif
#ifdef ENOMSG
  DEF_ERRNO(ENOMSG);
#endif
#ifdef ENONET
  DEF_ERRNO(ENONET);
#endif
#ifdef ENOPKG
  DEF_ERRNO(ENOPKG);
#endif
#ifdef ENOPROTOOPT
  DEF_ERRNO(ENOPROTOOPT);
#endif
#ifdef ENOSPC
  DEF_ERRNO(ENOSPC);
#endif
#ifdef ENOSR
  DEF_ERRNO(ENOSR);
#endif
#ifdef ENOSTR
  DEF_ERRNO(ENOSTR);
#endif
#ifdef ENOSYS
  DEF_ERRNO(ENOSYS);
#endif
#ifdef ENOTBLK
  DEF_ERRNO(ENOTBLK);
#endif
#ifdef ENOTCONN
  DEF_ERRNO(ENOTCONN);
#endif
#ifdef ENOTDIR
  DEF_ERRNO(ENOTDIR);
#endif
#ifdef ENOTEMPTY
  DEF_ERRNO(ENOTEMPTY);
#endif
#ifdef ENOTRECOVERABLE
  DEF_ERRNO(ENOTRECOVERABLE);
#endif
#ifdef ENOTSOCK
  DEF_ERRNO(ENOTSOCK);
#endif
#ifdef ENOTSUP
  DEF_ERRNO(ENOTSUP);
#endif
#ifdef ENOTTY
  DEF_ERRNO(ENOTTY);
#endif
#ifdef ENOTUNIQ
  DEF_ERRNO(ENOTUNIQ);
#endif
#ifdef ENXIO
  DEF_ERRNO(ENXIO);
#endif
#ifdef EOVERFLOW
  DEF_ERRNO(EOVERFLOW);
#endif
#ifdef EOWNERDEAD
  DEF_ERRNO(EOWNERDEAD);
#endif
#ifdef EPERM
  DEF_ERRNO(EPERM);
#endif
#ifdef EPFNOSUPPORT
  DEF_ERRNO(EPFNOSUPPORT);
#endif
#ifdef EPIPE
  DEF_ERRNO(EPIPE);
#endif
#ifdef EPROTO
  DEF_ERRNO(EPROTO);
#endif
#ifdef EPROTONOSUPPORT
  DEF_ERRNO(EPROTONOSUPPORT);
#endif
#ifdef EPROTOTYPE
  DEF_ERRNO(EPROTOTYPE);
#endif
#ifdef ERANGE
  DEF_ERRNO(ERANGE);
#endif
#ifdef EREMCHG
  DEF_ERRNO(EREMCHG);
#endif
#ifdef EREMOTE
  DEF_ERRNO(EREMOTE);
#endif
#ifdef EREMOTEIO
  DEF_ERRNO(EREMOTEIO);
#endif
#ifdef ERESTART
  DEF_ERRNO(ERESTART);
#endif
#ifdef ERFKILL
  DEF_ERRNO(ERFKILL);
#endif
#ifdef EROFS
  DEF_ERRNO(EROFS);
#endif
#ifdef ESHUTDOWN
  DEF_ERRNO(ESHUTDOWN);
#endif
#ifdef ESPIPE
  DEF_ERRNO(ESPIPE);
#endif
#ifdef ESOCKTNOSUPPORT
  DEF_ERRNO(ESOCKTNOSUPPORT);
#endif
#ifdef ESRCH
  DEF_ERRNO(ESRCH);
#endif
#ifdef ESTALE
  DEF_ERRNO(ESTALE);
#endif
#ifdef ESTRPIPE
  DEF_ERRNO(ESTRPIPE);
#endif
#ifdef ETIME
  DEF_ERRNO(ETIME);
#endif
#ifdef ETIMEDOUT
  DEF_ERRNO(ETIMEDOUT);
#endif
#ifdef ETOOMANYREFS
  DEF_ERRNO(ETOOMANYREFS);
#endif
#ifdef ETXTBSY
  DEF_ERRNO(ETXTBSY);
#endif
#ifdef EUCLEAN
  DEF_ERRNO(EUCLEAN);
#endif
#ifdef EUNATCH
  DEF_ERRNO(EUNATCH);
#endif
#ifdef EUSERS
  DEF_ERRNO(EUSERS);
#endif
#ifdef EXDEV
  DEF_ERRNO(EXDEV);
#endif
#ifdef EXFULL
  DEF_ERRNO(EXFULL);
#endif
#ifdef EOPNOTSUPP
  DEF_ERRNO(EOPNOTSUPP);
#endif
#ifdef EWOULDBLOCK
  DEF_ERRNO(EWOULDBLOCK);
#endif
#ifdef EDEADLOCK
  DEF_ERRNO(EDEADLOCK);
#endif
  /* place all the known errno names in the list *all-errno-symbols* */
  STk_define_variable(STk_intern("*all-errno-symbols*"), lst, module);
}

static void def_mode(char *name, long value, SCM module)
{
  STk_define_variable(STk_intern(name), MAKE_INT(value), module);
}

#define DEF_MODE(x);     {def_mode(#x, x, module);}

static void init_posix_modes(SCM module)
{
  DEF_MODE(S_IFSOCK); DEF_MODE(S_IFLNK);  DEF_MODE(S_IFREG);  DEF_MODE(S_IFBLK);
  DEF_MODE(S_IFDIR);  DEF_MODE(S_IFCHR);  DEF_MODE(S_IFIFO);  DEF_MODE(S_ISUID);
  DEF_MODE(S_ISGID);  DEF_MODE(S_ISVTX);  DEF_MODE(S_IRWXU);  DEF_MODE(S_IRUSR);
  DEF_MODE(S_IWUSR);  DEF_MODE(S_IXUSR);  DEF_MODE(S_IRWXG);  DEF_MODE(S_IRGRP);
  DEF_MODE(S_IWGRP);  DEF_MODE(S_IXGRP);  DEF_MODE(S_IRWXO);  DEF_MODE(S_IROTH);
  DEF_MODE(S_IWOTH);  DEF_MODE(S_IXOTH);
}


/* ----------------------------------------------------------------------
 *
 *      Module fuse starts here
 *
 * ----------------------------------------------------------------------
 */
MODULE_ENTRY_START("stklos/fuse")
{
  SCM module = STk_create_module(STk_intern("stklos/fuse"));

  ADD_PRIMITIVE_IN_MODULE(f_mount, module);
  init_posix_errno(module);   // Some names are in fact Linux and not POSIX
  init_posix_modes(module);
  
  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
} MODULE_ENTRY_END


DEFINE_MODULE_INFO
