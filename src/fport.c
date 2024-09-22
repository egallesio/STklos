/*
 * f p o r t . c                                -- File ports
 *
 * Copyright © 2000-2024 Erick Gallesio <eg@stklos.net>
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
 *    Creation date:  8-Jan-2000 14:48 (eg)
 *
 * This implementation is built by reverse engineering an old SUNOS 4.1.1
 * stdio.h. It has been simplified to fit the needs for STklos. In particular
 * non-buffered files are not implemented. Anyway this is faster than an
 * implementation using the C buffered IO (at least on glibc).
 *
 */
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/wait.h>

#include "stklos.h"
#include "fport.h"
#include "vm.h"

int STk_interactive = 0;                  /* We are in interactive mode */
SCM STk_stdin, STk_stdout, STk_stderr;    /* The unredirected ports */
MUT_DECL(all_fports_mutex);

/*
 * Implementation of our own popen/pclose. We use its file descriptor instead
 * of FILE *.
 *
 * The fd_popen function also returns the pid of the shell process launched
 * for the redirection. This pid is stored in the internal file port
 * representation to wait on it during fd_pclose.
 */
static int fd_popen(const char *cmd, const char *mode, int *pid)
{
  int p[2];

  if ((*mode != 'r' && *mode != 'w') || mode[1] ||
      pipe(p) == -1) {
    return -1;
  }

  switch (*pid = fork()) {
  case -1:                                   /* Cannot fork */
      close(p[0]); close(p[1]);
      return -1;
    case 0:                                  /* Child process */
      if (*mode == 'r') {
        if (p[1] != 1) dup2(p[1], 1);
        close(p[0]);
      } else {
        if (p[0] != 0) dup2(p[0], 0);
        close(p[1]);
      }
      execlp("sh", "sh", "-c", cmd, NULL);
      STk_panic("*** shell error in fd_popen");
      return -1; /* for the compiler */
    default:                                 /* Parent process */
      if (*mode == 'r') {
        close(p[1]);
        return p[0];
      } else {
        close(p[0]);
        return p[1];
      }
    }
}

static int fd_pclose(int fd, int pid) {
  int status, n;
  close(fd);
  do {
    n = waitpid(pid, &status, 0);
  } while (n == -1 && errno == EINTR);
  return WEXITSTATUS(status);
}


/*
 * Since we manage our ports ourselves, we need to keep a reference to
 * all the open ports such that all the ports are flushed when we exit
 * from the program. However, to avoid the that the GC "sees" these
 * references, we need to use a list of objects containing "masqueraded"
 * pointers.
 */

#define MAKE_FAKE_POINTER(x) ((void*) ((unsigned long) (x) ^ 0xabcde))
#define GET_FAKE_POINTER(x)  ((void*) ((unsigned long) (x) ^ 0xabcde))

static struct port_list {
  SCM port;
  struct port_list *next;
} *all_file_ports = NULL;


static void nop_release_port(SCM _UNUSED(port))
{
  /* This function is used as a release procedure when closing all files
   * at program exit. We use an empty procedure to avoid interference
   * in all_file_ports management
   */
}

static void register_port(SCM port)
{
  struct port_list *new;

  MUT_LOCK(all_fports_mutex);
  new            = STk_must_malloc(sizeof(struct port_list));
  new->port      = MAKE_FAKE_POINTER(port);
  new->next      = all_file_ports;
  all_file_ports = new;
  MUT_UNLOCK(all_fports_mutex);
}

static void unregister_port(SCM port)
{
  struct port_list *cur, *prev;

  MUT_LOCK(all_fports_mutex);
  for (prev = cur = all_file_ports; cur ; prev=cur, cur=cur->next) {
    if (GET_FAKE_POINTER(cur->port) == port) {
      if (cur == prev)
        all_file_ports = all_file_ports->next;
      else
        prev->next = cur->next;
      break;
    }
  }
  MUT_UNLOCK(all_fports_mutex);
}

void STk_close_all_ports(void)
{
  struct port_list *tmp, *cur;
  SCM eport = STk_current_error_port();
  SCM oport = STk_current_output_port();

  MUT_LOCK(all_fports_mutex);
  for (cur = all_file_ports; cur ; cur = cur->next) {
    tmp = GET_FAKE_POINTER(cur->port);
    PORT_RELEASE(tmp) = nop_release_port;
    if (OPORTP(tmp)) {
      if (tmp != eport && tmp != oport)
        STk_close(tmp);
    }
  }
  MUT_UNLOCK(all_fports_mutex);

  /* Finally, close error and output ports (must be done last) */
  STk_close(eport);
  STk_close(oport);
}

/*===========================================================================*\
 *
 *                                      Utils
 *
\*===========================================================================*/

static void fill_buffer(struct fstream *f)
{
  int n = 0;                            /* to avoid gcc warning */
  unsigned char *ptr = PORT_BASE(f);

  /* Interactive ports can see multiple EOF, so clear EOF flag  */
  PORT_STREAM_FLAGS(f) &= ~STK_IOEOF;

  /* Read */
  do {
    if (n == -1) perror("fill_buffer");
    if (PORT_USERDATA(f))
      n = PORT_LOWREAD(f)(f, ptr, PORT_BUFSIZE(f));
    else
      n = read(PORT_FD(f), ptr, PORT_BUFSIZE(f));
  }
  while ((n == -1) && (errno == EINTR));

  if (n <= 0) {
    PORT_STREAM_FLAGS(f) |= STK_IOEOF;
  } else {
    PORT_CNT(f) = n;
    PORT_PTR(f) = ptr;
  }
}

/*=============================================================================*/

static int flush_buffer(struct fstream *f)
{
  int n, ret;

  if (PORT_STREAM_FLAGS(f) & STK_IOREAD) {
    ret = 0; /* FIXME: don't return. See why */
  } else {
    n = PORT_CNT(f);
    /* Write buffer */
    if (PORT_USERDATA(f))
      ret = PORT_LOWWRITE(f)(f, PORT_BASE(f), n);
    else
      ret = write(PORT_FD(f), PORT_BASE(f), n);
  }
  /* Update structure */
  PORT_CNT(f) = 0;
  PORT_PTR(f) = PORT_BASE(f);

  return ret < 0;
}

/*=============================================================================*/


static inline int Feof(void *stream)
{
  return PORT_STREAM_FLAGS(stream) & STK_IOEOF;
}


static inline int Freadyp(void *stream)
{
  if (PORT_CNT(stream) > 0)
    return TRUE;
  else {
    fd_set readfds;
    struct timeval timeout;
    int f = PORT_FD(stream);

    FD_ZERO(&readfds);
    FD_SET(f, &readfds);
    timeout.tv_sec = 0; timeout.tv_usec = 10; /* FIXME: 0=>charge trop importante*/
    return (select(f+1, &readfds, NULL, NULL, &timeout));
  }
}

static inline int Fgetc(void *stream)
{
  if (PORT_IDLE(stream) != STk_nil) {
    /* There is at least an idle handler */
    SCM idle;

    while (!Freadyp(stream))
      for (idle = PORT_IDLE(stream); !NULLP(idle); idle = CDR(idle))
        STk_C_apply(CAR(idle), 0);
  }

  for ( ; ; ) {
    if (--PORT_CNT(stream) >= 0)
      return (int) *PORT_PTR(stream)++;
    else {
      fill_buffer(stream);
      if (Feof(stream)) return EOF;
    }
  }
}


static inline int Fread(void *stream, void *buff, int count)
{
  int copied, avail;

  avail  = PORT_CNT(stream);
  copied = 0;

   if (avail > 0) {
     /* First place the characters which are already available in buffer */
     if (count <= avail) {
       memcpy(buff, PORT_PTR(stream), count);
       PORT_PTR(stream) += count;
       PORT_CNT(stream) -= count;
       return count;
     } else {
       memcpy(buff, PORT_PTR(stream), avail);
       PORT_PTR(stream) += avail;
       PORT_CNT(stream) -= avail;
       buff += avail;
       count -= avail;
       copied = avail;
     }
   }
   if (PORT_USERDATA(stream))
     return copied + PORT_LOWREAD(stream)(stream, buff, count);
   else
     return copied + read(PORT_FD(stream), buff, count);
}

static int Fclose(void *stream)         /* File version */
{
  int ret = flush_buffer(stream);

  return (ret == EOF) ? EOF : close(PORT_FD(stream));
}

static int Fclose_pipe(void *stream)    /* pipe version (used by "| cmd" files */
{
  int ret = flush_buffer(stream);

  return (ret == EOF) ? EOF : fd_pclose(PORT_FD(stream), PORT_PID(stream));
}


static off_t Fseek(void *stream, off_t offset, int whence)
{
  return lseek(PORT_FD(stream), offset, whence);
}


static inline int Fputc(int c, void *stream)
{
  int ret = c;

  for ( ; ; ) {
    if (PORT_CNT(stream) < PORT_BUFSIZE(stream)) {
      *PORT_PTR(stream)++ = (unsigned char) c;
      PORT_CNT(stream)   += 1;
      if (c == '\n' && PORT_STREAM_FLAGS(stream) & STK_IOLBF) {
        if (flush_buffer(stream) != 0)
          return EOF;
      }
      break;
    }
    else {
      /* buffer is full */
      if (flush_buffer(stream) != 0)
        return EOF;
    }
  }
  return ret;
}


static inline int Fwrite(void *stream, const void *buff, int count)
{
  /* Flush (eventually) chars which are already in the buffer before writing  */
  if (PORT_CNT(stream)) {
    if (flush_buffer(stream) != 0)
      return EOF;
  }

  /* Write buffer on stream */
  if (PORT_USERDATA(stream))
    return PORT_LOWWRITE(stream)(stream, buff, count);
  else
    return write(PORT_FD(stream), buff, count);
}


static inline int Fnputs(void *stream, const char *s, int len)
{
  int res, flush = (PORT_STREAM_FLAGS(stream) & STK_IOLBF);

  if (len > TTY_BUFSIZE) {
    /* This is a long string so don't use the buffer; write it directly to the stream */
    res = Fwrite(stream, s, len);
  } else {
    int free, count = len;

    for ( ; ; ) {
      free = PORT_BUFSIZE(stream) - PORT_CNT(stream);

      if (len <= free) {
        memcpy(PORT_PTR(stream), s, len);
        PORT_PTR(stream) += len;
        PORT_CNT(stream) += len;
        res = count;
        break;
      } else {
        memcpy(PORT_PTR(stream), s, free);
        PORT_PTR(stream) += free;
        PORT_CNT(stream) += free;
        s += free;
        len -= free;
        if (flush_buffer(stream) != 0)
          { res = EOF; break; }
      }
    }
  }
  if (res == EOF)
    return EOF;
  if (flush) {
    if (flush_buffer(stream) != 0)
      return EOF;
  }
  return res;
}


static inline int Fputs(const char *s, void *stream)
{
  return Fnputs(stream, s, strlen(s));
}

static inline int Fputstring(SCM s, void *stream)
{
  return Fnputs(stream, STRING_CHARS(s), STRING_SIZE(s));
}


static inline int  Fflush(void *stream)
{
  return flush_buffer(stream);          /* Problem if file opened for reading */
}


static void fport_print(SCM obj, SCM port)      /* Generic printing of file ports */
{
  char buffer[MAX_PATH_LENGTH + 40];

  snprintf(buffer, sizeof(buffer), "#[%s-port '%s' (%d)%s]",
           IPORTP(obj) ? "input" : "output",
           PORT_FNAME(obj), PORT_FD(PORT_STREAM(obj)),
           PORT_IS_CLOSEDP(obj) ? " (closed)" : "");
  STk_puts(buffer, port);
}


static void fport_finalizer(struct port_obj *port, void _UNUSED(*client_data))
{
  if (!PORT_IS_CLOSEDP(port))
    /* Close the associated stream */
    STk_close((SCM) port);
}


static struct port_obj *
make_fport(const char *fname, int fd, int flags)
{
  struct fstream  *fs = STk_must_malloc(sizeof(struct fstream));
  int n, mode;
  SCM res;

  /* allocate buffer for file */
  if (isatty(fd) || (STk_interactive && fd < 2)) {
    n      = TTY_BUFSIZE;
    mode   = STK_IOLBF;
    flags |= PORT_IS_INTERACTIVE;
  } else {
    n    = OTHER_BUFSIZE;
    mode = STK_IOFBF;
  }

  /* keep the indication that file is opened in read in the stream part */
  if (flags & (PORT_READ | PORT_RW)) mode |= STK_IOREAD;

  /* Set the case-sensitive bit */
  flags |= PORT_CASE_SENSITIVE;

  /* Initialize the stream part */
  PORT_BASE(fs)          = STk_must_malloc_atomic(n);
  PORT_PTR(fs)           = PORT_BASE(fs);
  PORT_CNT(fs)           = 0;
  PORT_BUFSIZE(fs)       = n;
  PORT_STREAM_FLAGS(fs)  = mode;
  PORT_PID(fs)           = 0; /* will be changed if port is a pipe */
  PORT_FD(fs)            = fd;
  PORT_REVENT(fs)        = STk_false;
  PORT_WEVENT(fs)        = STk_false;
  PORT_IDLE(fs)          = STk_nil;

  /* Now initialize the port itself */
  NEWCELL(res, port);

  PORT_STREAM(res)      = fs;
  PORT_FLAGS(res)       = flags;
  PORT_UNGETC(res)      = EOF;
  PORT_FNAME(res)       = STk_strdup(fname);
  PORT_LINE(res)        = 1;
  PORT_POS(res)         = 0;
  PORT_KW_COL_POS(res)  = STk_keyword_colon_convention();
  PORT_CLOSEHOOK(res)   = STk_false;

  PORT_PRINT(res)       = fport_print;
  PORT_RELEASE(res)     = unregister_port;
  PORT_GETC(res)        = Fgetc;
  PORT_READY(res)       = Freadyp;
  PORT_EOFP(res)        = Feof;
  PORT_CLOSE(res)       = (flags & PORT_IS_PIPE) ? Fclose_pipe: Fclose;
  PORT_PUTC(res)        = Fputc;
  PORT_PUTS(res)        = Fputs;
  PORT_PUTSTRING(res)   = Fputstring;
  PORT_NPUTS(res)       = Fnputs;
  PORT_FLUSH(res)       = Fflush;
  PORT_BREAD(res)       = Fread;
  PORT_BWRITE(res)      = Fwrite;
  PORT_SEEK(res)        = Fseek;

  /* Add a finalizer on file to close it when the GC frees it */
  STk_register_finalizer(res, fport_finalizer);

  /* Add res to the list of open ports */
  register_port(res);

  return (struct port_obj *)res;
}


#ifdef WIN32_0000
static char *convert_for_win32(char *mode)
{
  /* Things are complicated on Win32 (as always) so we convert all files
   * to binary files. Note that this function is not called when we work
   * on ports since they only accept versions without "b" on Cygwin.
   */
  switch (*mode) {
    case 'r': if (mode[1] == '\0') return "rb";
              if (mode[1] == '+')  return "rb+";
              break;
    case 'w': if (mode[1] == '\0') return "wb";
              if (mode[1] == '+')  return "wb+";
              break;
    case 'a': if (mode[1] == '\0') return "ab";
              if (mode[1] == '+')  return "ab+";
              break;
  }
  return mode;
}
#endif

static int convert_mode(const char *mode) {
  char first = *mode;

  if (mode[1] == 'b')
    mode++;

  switch (first) {
    case 'r':
      if (mode[1] == '\0') return O_RDONLY;
      if (mode[1] == '+')  return O_RDWR;
      break;
    case 'w':
      if (mode[1] == '\0') return O_WRONLY | O_TRUNC | O_CREAT;
      if (mode[1] == '+')  return O_RDWR   | O_TRUNC | O_CREAT;
      break;
    case 'a':
      if (mode[1] == '\0') return O_WRONLY | O_CREAT | O_APPEND;
      if (mode[1] == '+')  return O_RDWR   | O_CREAT | O_APPEND;
      break;
    default:
      break;
  }
  STk_error("bad file opening mode %s", mode);
  return 0;
}


static SCM open_file_port(SCM filename, const char *mode, int flags, int error)
{
  const char *full_name, *name;
  SCM z;
  int fd;

  name = STRING_CHARS(filename);

  if (strncmp(name, "| ", 2)) {
    full_name  = STk_expand_file_name(name);
    flags     |= PORT_IS_FILE;

    if ((fd = open(full_name, convert_mode(mode), 0666)) == -1) {
      if (error)
        STk_error_file_name("could not open file ~S", filename);
      else
        return STk_false;
    }
    if (*mode == 'a') lseek(fd, 0L, SEEK_END);
    z = make_fport(full_name, fd, flags);
  } else {
    pid_t pid = 0; /* for the compiler */

    full_name  = name;
    flags     |= PORT_IS_PIPE;
    if ((fd = fd_popen(name+1, mode, &pid)) < 0) {
      if (error)
        STk_error_file_name("could not create pipe for ~S",
                            STk_Cstring2string(name+2));
      else
        return STk_false;
    }
    z = make_fport(full_name, fd, flags);
    PORT_PID(PORT_STREAM(z)) = pid;
  }

  return z;
}




SCM STk_fd2scheme_port(int fd, const char *mode, char *identification)
{
  int flags;

  flags = PORT_IS_FILE | ((*mode == 'r') ? PORT_READ : PORT_WRITE);
  return (SCM) make_fport(identification, fd, flags);
}


SCM STk_open_file(char *filename, char *mode)
{
  int type;

  switch (mode[0]) {
    case 'r': type = (mode[1] == '+') ? PORT_RW : PORT_READ;  break;
    case 'a':
    case 'w': type = (mode[1] == '+') ? PORT_RW : PORT_WRITE; break;
    default:  goto Error;
  }
  type |= PORT_TEXTUAL;                     /* by default */

  return open_file_port(STk_Cstring2string(filename), mode, type, FALSE);
Error:
  STk_panic("bad opening mode %s", mode);
  return STk_void; /* for the compiler */
}

SCM STk_add_port_idle(SCM port, SCM idle_func)
{
  PORT_IDLE(PORT_STREAM(port)) = STk_cons(idle_func,
                                          PORT_IDLE(PORT_STREAM(port)));
  return STk_void;
}

void STk_set_line_buffered_mode(SCM port)
{
  struct fstream* fs;
  int flags;

  /* Assert: port is a valid input port */
  fs    = PORT_STREAM(port);
  flags = PORT_STREAM_FLAGS(fs);

  flags &= ~(STK_IOFBF|STK_IONBF);
  PORT_STREAM_FLAGS(fs) = flags | STK_IOLBF;
}


/*=============================================================================*\
 *                               Open/Close
\*=============================================================================*/

/*
<doc open-input-file
 * (open-input-file filename)
 *
 * Takes a string naming an existing file and returns an input port capable
 * of delivering characters from the file. If the file cannot be opened,
 * an error is signalled.
 *
 * NOTE: if |filename| starts with the string `"| "`, this procedure returns a pipe port.
 * Consequently, it is not possible to open a file whose name starts with those two
 * characters.
doc>
 */
DEFINE_PRIMITIVE("open-input-file", open_input_file, subr1, (SCM filename))
{
  if (!STRINGP(filename)) STk_error_bad_file_name(filename);
  return open_file_port(filename, "r", PORT_READ|PORT_TEXTUAL, TRUE);
}

DEFINE_PRIMITIVE("open-binary-input-file", open_binary_ifile, subr1, (SCM filename))
{
  if (!STRINGP(filename)) STk_error_bad_file_name(filename);
  return open_file_port(filename, "r", PORT_READ|PORT_BINARY, TRUE);
}

/*
<doc open-output-file
 * (open-output-file filename)
 *
 * Takes a string naming an output file to be created and returns an output
 * port capable of writing characters to a new file by that name. If the file
 * cannot be opened, an error is signalled. If a file with the given name
 * already exists, it is rewritten.
 *
 * NOTE: if |filename| starts with the string `"| "`, this procedure returns a pipe port.
 * Consequently, it is not possible to open a file whose name starts with those two
 * characters.
doc>
 */
DEFINE_PRIMITIVE("open-output-file", open_output_file, subr1, (SCM filename))
{
  if (!STRINGP(filename)) STk_error_bad_file_name(filename);
  return open_file_port(filename, "w", PORT_WRITE|PORT_TEXTUAL, TRUE);
}

DEFINE_PRIMITIVE("open-binary-output-file", open_binary_ofile, subr1, (SCM filename))
{
  if (!STRINGP(filename)) STk_error_bad_file_name(filename);
  return open_file_port(filename, "w", PORT_WRITE|PORT_BINARY, TRUE);
}


/*
<doc EXT input-file-port? output-file-port?
 * (input-file-port? obj)
 * (output-file-port? obj)
 *
 * Returns |#t| if |obj| is a file input port or a file output port respectively,
 * otherwise returns |#f|.
doc>
 */
DEFINE_PRIMITIVE("input-file-port?", input_fportp, subr1, (SCM port))
{
  return MAKE_BOOLEAN(IFPORTP(port));
}


DEFINE_PRIMITIVE("output-file-port?", output_fportp, subr1, (SCM port))
{
  return MAKE_BOOLEAN(OFPORTP(port));
}

/*
<doc EXT open-file
 * (open-file filename mode)
 *
 * Opens the file whose name is |filename| with the specified string
 * |mode| which can be:
 *
 * - |"r"| to open the file for reading. The stream is positioned at
 *   the beginning of the file.
 *
 * - |"r+"| to open the file for reading and writing. The stream is
 *   positioned at the beginning of the file.
 *
 * - |"w"| to truncate the file to zero length or create the file for writing.
 *   The stream is positioned at the beginning of the file.
 *
 * - |"w+"| to open the file for reading and writing. The file is created
 *   if it does not exist, otherwise it is truncated. The stream is positioned
 *   at the beginning of the file.
 *
 * - |"a"| to open the file for writing. The file is created if it does
 *   not exist. The stream is positioned at the end of the file.
 *
 * - |"a+"| to open the file for reading and writing. The file is created
 *   if it does not exist. The stream is positioned at the end of the file.
 *
 * If the file can be opened, |open-file| returns the textual port associated
 * with the given file, otherwise it returns |#f|. Here again, the *_magic_*
 * string "@pipe " permits to open a pipe port. (In this case mode can only be
 * |"r"| or |"w"|.)
doc>
 */

DEFINE_PRIMITIVE("open-file", scheme_open_file, subr2, (SCM filename, SCM mode))
{
  int type;

  if (!STRINGP(filename)) STk_error_bad_file_name(filename);
  if (!STRINGP(mode))     goto Error;

  switch (STRING_CHARS(mode)[0]) {
    case 'r': type = (STRING_CHARS(mode)[1] == '+') ? PORT_RW : PORT_READ;  break;
    case 'a':
    case 'w': type = (STRING_CHARS(mode)[1] == '+') ? PORT_RW : PORT_WRITE; break;
    default:  goto Error;
  }
  return open_file_port(filename, STRING_CHARS(mode), type | PORT_TEXTUAL, FALSE);
Error:
  STk_error_bad_io_param("bad opening mode ~S", mode);
  return STk_void; /* for the compiler */
}


/*
<doc EXT port-file-name
 * (port-file-name port)
 *
 * Returns the file name used to open |port|; |port| must be a file port.
doc>
 */
DEFINE_PRIMITIVE("port-file-name", port_file_name, subr1, (SCM port))
{
  if (!FPORTP(port)) STk_error_bad_port(port);
  return STk_Cstring2string(PORT_FNAME(port));
}


DEFINE_PRIMITIVE("%port-file-fd", port_file_fd, subr1, (SCM port))
{
  return FPORTP(port)? MAKE_INT(PORT_FD(PORT_STREAM(port))) : STk_false;
}


DEFINE_PRIMITIVE("%port-idle", port_idle, subr12, (SCM port, SCM val))
{
  if (!FPORTP(port)) STk_error_bad_port(port);

  if (val) {
    /* Set the idle list to the given value. No control on the content of
     * the procedure list (must be done in Scheme)
     */
    if (STk_int_length(val) < 0) STk_error_bad_io_param("bad list ~S", val);
    PORT_IDLE(PORT_STREAM(port)) = val;
  }
  return  PORT_IDLE(PORT_STREAM(port));
}

/*=============================================================================*\
 *                              Load
\*=============================================================================*/

#define FILE_IS_SOURCE          0
#define FILE_IS_BCODE           1
#define FILE_IS_OBJECT          2
#define FILE_IS_DIRECTORY       3
#define FILE_DOES_NOT_EXISTS    4

static int find_file_nature(SCM f)
{
  int c;
  SCM tmp;

  c = STk_getc(f); STk_ungetc(c, f);

  if (c != EOF) {
    if ((iscntrl(c) && c!= '\n' && c!= '\t') || !isascii(c))
      return FILE_IS_OBJECT;

    tmp = STk_read(f, TRUE);
    if (tmp == STk_intern("STklos"))
      return FILE_IS_BCODE;

    /* We'll suppose that this is a source file, but we have read the first sexpr */
    STk_rewind(f);
  }
  return FILE_IS_SOURCE;
}


SCM STk_load_source_file(SCM f, SCM env)
{
  SCM sexpr;
  SCM eval, ref, eval_symb = STk_intern("eval");

  for ( ; ; ) {
    /* We need to lookup for eval symbol for each expr, since it can
     * change while loading the file (with R5RS macro for instance, it
     * is the case)
     */
    sexpr = STk_read_constant(f, PORT_CASE_SENSITIVEP(f));
    if (sexpr == STk_eof) break;
    eval  = STk_lookup(eval_symb, STk_current_module(), &ref, TRUE);

    /* NOTE: we cannot call *always* eval with 2 parameters since, at boot
     * time, things about modules are not completely initialized.
     */
     if (env) STk_C_apply(eval, 2, sexpr, env);
     else     STk_C_apply(eval, 1, sexpr);
  }
  STk_close_port(f);
  return STk_true;
}


static SCM load_file(SCM filename, SCM env)
{
  SCM f;
  char *fname;

  if (!STRINGP(filename))   STk_error_bad_file_name(filename);
  if (env && !MODULEP(env)) STk_error("bad environment", env);

  /* Verify that file is not a directory */
  fname = STRING_CHARS(filename);
  if (STk_dirp(fname)) return STk_false;

  /* It's Ok, try now to load file */
  f = STk_open_file(fname, "r");
  if (f != STk_false) {
    switch (find_file_nature(f)) {
      case FILE_IS_SOURCE: return STk_load_source_file(f, env);
      case FILE_IS_BCODE:  return STk_load_bcode_file(f);           // TODO
      case FILE_IS_OBJECT: return STk_load_object_file(f, fname);
    }
  }
  return STk_false;
}


/*
<doc load
 * (load filename)
 *
 * (((load-path)))
 * (((load-suffixes)))
 * |Filename| should be a string naming an existing file containing Scheme
 * expressions. |Load| has been extended in STklos to allow loading of
 * files containing Scheme compiled code as well as object files (_aka_
 * shared objects). The loading of object files is not available on
 * all architectures. The value returned by |load| is *_void_*.
 *
 * If the file whose name is |filename| cannot be located, |load| will try
 * to find it in one of the directories given by `"load-path"`
 * with the suffixes given by `"load-suffixes"`.
doc>
 */

DEFINE_PRIMITIVE("load", scheme_load, subr12, (SCM filename, SCM env))
{
  // NOTE: This function is overloaded by the load function in
  // "lib/load.stk". We keep it here to have a fallback function, for a
  // possible alternative boot image.
  if (load_file(filename, env) == STk_false)
    STk_error_file_name("cannot load ~S", filename);
  return STk_void;
}


/*
<doc EXT try-load
 * (try-load filename)
 *
 * |try-load| tries to load the file named |filename|. As with |load|,
 * |try-load| tries to find the file given the current load path
 * and a set of suffixes if |filename| cannot be loaded. If |try-load|
 * is able to find a readable file, it is loaded, and |try-load| returns
 * |#t|. Otherwise, |try-load| retuns |#f|.
doc>
 */
DEFINE_PRIMITIVE("try-load", scheme_try_load, subr12, (SCM filename, SCM env))
{
  return load_file(filename, env);
}



DEFINE_PRIMITIVE("%file-information", file_information, subr1, (SCM filename))
{
  SCM f, res = STk_nil;
  char *fname;

  if (!STRINGP(filename)) STk_error_bad_file_name(filename);

  fname = STRING_CHARS(filename);

  /* Verify that file is not a directory */
  if (!STk_dirp(fname)) {
    f = STk_open_file(fname, "r");
    if (f != STk_false) {
      int type  = find_file_nature(f);
      char *str = "";

      switch (type) {
        case FILE_IS_SOURCE: str = "source"; break;
        case FILE_IS_BCODE:  str = "byte-code"; res = STk_read(f, TRUE); break;
        case FILE_IS_OBJECT: str = "object";
                             res = STk_info_object_file(STRING_CHARS(filename));
                             break;
      }
      STk_close_port(f);
      /* Add the file nature to result */
      res = STk_cons(STk_makekey("nature"),
                     STk_cons(STk_intern(str),
                              res));
    }
  }
  return res;
}


int STk_init_fport(void)
{
  vm_thread_t *vm = STk_get_current_vm();

  STk_stdin  = vm->iport = (SCM) make_fport("*stdin*", 0,
                                            PORT_IS_FILE | PORT_READ |
                                            PORT_BINARY | PORT_TEXTUAL);
  STk_stdout = vm->oport = (SCM) make_fport("*stdout*", 1,
                                            PORT_IS_FILE | PORT_WRITE |
                                            PORT_BINARY | PORT_TEXTUAL);
  STk_stderr = vm->eport = (SCM) make_fport("*stderr*", 2,
                                            PORT_IS_FILE | PORT_WRITE |
                                            PORT_BINARY | PORT_TEXTUAL);

  ADD_PRIMITIVE(scheme_load);
  ADD_PRIMITIVE(scheme_try_load);


  ADD_PRIMITIVE(open_input_file);
  ADD_PRIMITIVE(open_output_file);
  ADD_PRIMITIVE(open_binary_ifile);
  ADD_PRIMITIVE(open_binary_ofile);

  ADD_PRIMITIVE(input_fportp);
  ADD_PRIMITIVE(output_fportp);
  ADD_PRIMITIVE(scheme_open_file);
  ADD_PRIMITIVE(port_file_name);

  ADD_PRIMITIVE(port_file_fd);
  ADD_PRIMITIVE(port_idle);

  //  ADD_PRIMITIVE(dbg);
  ADD_PRIMITIVE(file_information);
  return TRUE;
}

/*  LocalWords:  filename
 */
