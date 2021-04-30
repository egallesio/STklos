/*
 * vport.c                                      -- Virtual Ports
 *
 * Copyright © 2005-2021 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 *    Creation date: 17-Aug-2005 08:31 (eg)
 * Last file update: 30-Apr-2021 14:19 (eg)
 */

#include "stklos.h"

struct vstream {
  SCM port;                     /* circular reference to find back the port */
  SCM getc, readyp, eofp;       /* input port */
  SCM putc, putstring, flush;   /* output port */
  SCM close;                    /* input & output port */
};


/* ----------------------------------------------------------------------
 * Utilities
 * ----------------------------------------------------------------------
 */
static void error_bad_vector(SCM v, int sz)
{
  STk_error("bad parameter ~S (must be a vector of size %d)", v, sz);
}


static void verify_proc(SCM proc, int arity)
{
  if ((proc != STk_false) &&
      (STk_procedurep(proc) == STk_false ||
       STk_proc_arity(proc) != MAKE_INT(arity)))
    STk_error("bad procedure ~S", proc);
}


static void vport_print(SCM obj, SCM port)   /* Generic printing of virtual ports */
{
  char buffer[MAX_PATH_LENGTH + 20];

  sprintf(buffer, "#[%s-virtual-port %lx%s]",
          IVPORTP(obj) ? "input" : "output",
          (unsigned long) obj,
          PORT_IS_CLOSEDP(obj) ? " (closed)" : "");
  STk_puts(buffer, port);
}


static void vport_release(SCM _UNUSED(port))
{
  /* Nothing to do */
}

/* ----------------------------------------------------------------------
 * Default primitives implementation
 * ----------------------------------------------------------------------
 */
static int call_user_eofp(void *stream)
{
  struct vstream *vs = stream;
  SCM res;

  if (vs->eofp == STk_false) return TRUE;
  res = STk_C_apply(vs->eofp, 1, vs->port);
  return  res != STk_false;
}

/*
 * READ
 */
static int call_user_getc(void *stream)
{
  struct vstream *vs = stream;
  SCM res;

  if ((vs->getc == STk_false) || call_user_eofp(stream)) return EOF;
  res = STk_C_apply(vs->getc, 1, vs->port);
  return  CHARACTERP(res) ? CHARACTER_VAL(res) : EOF;
}


static int call_user_ready(void *stream)
{
  struct vstream *vs = stream;
  SCM res;

  if (vs->readyp == STk_false) return TRUE;
  res = STk_C_apply(vs->readyp, 1, vs->port);
  return  res != STk_false;
}


static int call_user_close(void *stream)
{
  struct vstream *vs = stream;

  if (vs->close != STk_false)
    STk_C_apply(vs->close, 1, vs->port);
  return 0;
}

/*
 * WRITE
 */

static int call_user_putstring(SCM s, void *stream);
static int vport_nputs(void *stream, char *s, int len);

static int call_user_putc(int c, void *stream)
{
  struct vstream *vs = stream;
  SCM res;

  if (vs->putc == STk_false) {
    if (vs->putstring != STk_false) {
      char str[2];
      str[0] = c; str[1] = '\0';
      return call_user_putstring(STk_Cstring2string(str), stream);
    }
    return 0;
  }
  res = STk_C_apply(vs->putc, 2, MAKE_CHARACTER(c), vs->port);
  return (res ==  STk_eof) ? EOF: c;
}


static int call_user_putstring(SCM s, void *stream)
{
  struct vstream *vs = stream;

  if (vs->putstring == STk_false) {
    if (vs->putc != STk_false) {
      return vport_nputs(stream, STRING_CHARS(s), STRING_SIZE(s));
    }
    return 0;
  }
  STk_C_apply(vs->putstring, 2, s, vs->port);
  return STRING_SIZE(s);
}

static int call_user_flush(void *stream)
{
  struct vstream *vs = stream;

  if (vs->flush != STk_false)
    STk_C_apply(vs->flush, 1, vs->port);
  return 0;
}


/*
 * Higher level primitives
 */
static int vport_read(void *stream, void *buf, int count)
{
  int i;
  char *s = buf;

  for (i = 0; i < count; i++) {
    int c = call_user_getc(stream);
    if (c == EOF) break;
    *s++ = c;
  }
  return s - (char*)buf;
}


static int vport_write(void *stream, void *buf, int count)
{
  int i;
  char *s = buf;

  for (i = 0; i < count; i++) {
    int c = call_user_putc(*s++, stream);
    if (c == EOF) break;
  }
  return s - (char*)buf;
}

static off_t vport_seek(void  _UNUSED(*stream),
                        off_t _UNUSED(offset),
                        int   _UNUSED(whence))
{
  STk_error("cannot seek a virtual port");
  return 0;
}

static int vport_nputs(void *stream, char *s, int len)
{
  int i;

  for (i = 0; i < len; i++)
    if (call_user_putc(*s++, stream) == EOF) return EOF;
  return len;
}

static int vport_puts(char *s, void *stream)
{
  return vport_nputs(stream, s, strlen(s));
}


/* ----------------------------------------------------------------------
 * Primitives
 * ----------------------------------------------------------------------
 */

/*
<doc EXT open-input-virtual
 * (open-input-virtual :key  (read-char #f) (ready? #f) (eof? #f) (close #f))
 *
 * Returns a virtual port using the |read-char| procedure to read a
 * character from the port, |ready?| to know if there is any data to
 * read from the port, |eof?| to know if the end of file is reached
 * on the port and finally |close| to close the port. All theses
 * procedure takes one parameter which is the port from which the input
 * takes place.  |Open-input-virtual| accepts also the special value
 * |#f| for the I/O procedures with the following conventions:
 * ,(itemize
 *    (item [if |read-char| or |eof?| is |#f|, any attempt to read
 * the virtual port will return an eof object;])
 *    (item [if |ready?| is |#f|, the file is always  ready
 * for reading;])
 *    (item [if |close| is |#f|, no action is done when the port is
 * closed.]))
 * @l
 * Hereafter is a possible implementation of |open-input-string|
 * using virtual ports:
 * @lisp
 * (define (open-input-string str)
 *   (let ((index 0))
 *     (open-input-virtual
 *        :read-char (lambda (p)
 *                  ;; test on eof is already done by the system
 *                  (let ((res (string-ref str index)))
 *                    (set! index (+ index 1))
 *                    res))
 *        :eof? (lambda (p) (>= index (string-length str))))))
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("%open-input-virtual", open_input_vport, subr1, (SCM v))
{
  SCM z;
  struct vstream  *vs;
  int flag = (STk_read_case_sensitive) ? PORT_CASE_SENSITIVE : 0;

  if (!VECTORP(v) || VECTOR_SIZE(v) != 4) error_bad_vector(v, 4);

  NEWCELL(z, port);

  vs  = STk_must_malloc(sizeof(struct vstream));
  verify_proc(vs->getc   = VECTOR_DATA(v)[0], 1);
  verify_proc(vs->readyp = VECTOR_DATA(v)[1], 1);
  verify_proc(vs->eofp   = VECTOR_DATA(v)[2], 1);
  verify_proc(vs->close  = VECTOR_DATA(v)[3], 1);
  vs->port = z;
  vs->putc = vs->putstring = vs->flush = NULL;

  PORT_STREAM(z)        = vs;
  PORT_FLAGS(z)         = PORT_READ | PORT_IS_VIRTUAL | PORT_TEXTUAL | flag;
  PORT_UNGETC(z)        = EOF;
  PORT_LINE(z)          = 1;
  PORT_POS(z)           = 0;
  PORT_FNAME(z)         = "virtual input port";
  PORT_KW_COL_POS(z)    = STk_keyword_colon_convention();
  PORT_CLOSEHOOK(z)     = STk_false;

  PORT_PRINT(z)         = vport_print;
  PORT_RELEASE(z)       = vport_release;
  PORT_GETC(z)          = call_user_getc;
  PORT_READY(z)         = call_user_ready;
  PORT_EOFP(z)          = call_user_eofp;
  PORT_CLOSE(z)         = call_user_close;
  PORT_PUTC(z)          = NULL;
  PORT_PUTS(z)          = NULL;
  PORT_PUTSTRING(z)     = NULL;
  PORT_NPUTS(z)         = NULL;
  PORT_FLUSH(z)         = NULL;
  PORT_BREAD(z)         = vport_read;
  PORT_BWRITE(z)        = NULL;
  PORT_SEEK(z)          = vport_seek;

  return (struct port_obj *) z;
}


/*
<doc EXT open-output-virtual
 * (open-output-virtual :key  (write-char #f) (write-string #f) (flush #f) (close #f))
 *
 * Returns a virtual port using the |write-char| procedure to write a
 * character to the port, |write-string| to write a string to the port,
 * |flush| to (eventuelly) flush the characters on the port and finally
 * |close|to close the port. |Write-char| takes two parameters: a character and
 * the port to which the output must be done. |write-string| takes two
 * parameters: a string and a port. |Flush| and |Close| take one
 * parameter which is the port on which the action must be done.
 * |Open-output-virtual| accepts also the special value |#f|
 * for the I/O procedures. If a procedure is |#f| nothing is done
 * on the corresponding action.
 * @l
 * Hereafter is an (very inefficient) implementation of a variant of
 * |open-output-string| using virtual ports. The value of the output
 * string is printed when the port is closed:
 * @lisp
 * (define (open-output-string)
 *   (let ((str ""))
 *     (open-output-virtual
 *        :write-char (lambda (c p)
 *                   (set! str (string-append str (string c))))
 *        :write-string (lambda (s p)
 *                     (set! str (string-append str s)))
 *        :close (lambda (p) (write str) (newline)))))
 * @end lisp
 * ,(bold "Note:") |write-string| is mainly used for writing strings and is
 * generally more efficient than writing the string character by character.
 * However, if |write-string| is not provided, strings are printed with
 * |write-char|.  On the other hand, if |write-char| is absent,
 * characters are written by successive allocation of one character strings.
 * @l
 * Hereafter is another example: a virtual file port where all characters
 * are converted to upper case:
 * @lisp
 * (define (open-output-uppercase-file file)
 *   (let ((out (open-file file "w")))
 *     (and out
 *          (open-output-virtual
 *              :write-string (lambda (s p)
 *                              (display (string-upper s) out))
 *              :close (lambda (p)
 *                       (close-port out))))))
 * @end lisp
doc>
*/

DEFINE_PRIMITIVE("%open-output-virtual", open_output_vport, subr1, (SCM v))
{
  SCM z;
  struct vstream  *vs;
  int flag = (STk_read_case_sensitive) ? PORT_CASE_SENSITIVE : 0;

  if (!VECTORP(v) || VECTOR_SIZE(v) != 4) error_bad_vector(v, 4);

  NEWCELL(z, port);

  vs  = STk_must_malloc(sizeof(struct vstream));
  verify_proc(vs->putc      = VECTOR_DATA(v)[0], 2);
  verify_proc(vs->putstring = VECTOR_DATA(v)[1], 2);
  verify_proc(vs->flush     = VECTOR_DATA(v)[2], 1);
  verify_proc(vs->close     = VECTOR_DATA(v)[3], 1);
  vs->port = z;
  vs->getc = vs->readyp = vs->eofp = NULL;

  PORT_STREAM(z)        = vs;
  PORT_FLAGS(z)         = PORT_WRITE | PORT_IS_VIRTUAL | PORT_TEXTUAL | flag;
  PORT_UNGETC(z)        = EOF;
  PORT_LINE(z)          = 1;
  PORT_POS(z)           = 0;
  PORT_FNAME(z)         = "virtual output port";

  PORT_PRINT(z)         = vport_print;
  PORT_RELEASE(z)       = vport_release;
  PORT_GETC(z)          = NULL;
  PORT_READY(z)         = NULL;
  PORT_EOFP(z)          = NULL;
  PORT_CLOSE(z)         = call_user_close;
  PORT_PUTC(z)          = call_user_putc;
  PORT_PUTS(z)          = vport_puts;
  PORT_PUTSTRING(z)     = call_user_putstring;
  PORT_NPUTS(z)         = vport_nputs;
  PORT_FLUSH(z)         = call_user_flush;
  PORT_BREAD(z)         = NULL;
  PORT_BWRITE(z)        = vport_write;
  PORT_SEEK(z)          = vport_seek;

  return (struct port_obj *) z;
}

/*
<doc EXT input-virtual-port? output-virtual-port?
 * (input-virtual-port? obj)
 * (output-virtual-port? obj)
 *
 * Returns |#t| if |obj| is a virtual input port or a virtual output port
 * respectively, otherwise returns #f.
doc>
 */
DEFINE_PRIMITIVE("input-virtual-port?", input_vportp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(IVPORTP(obj));
}

DEFINE_PRIMITIVE("output-virtual-port?", output_vportp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(OVPORTP(obj));
}


/* ====================================================================== */
int STk_init_vport(void)
{
  ADD_PRIMITIVE(open_input_vport);
  ADD_PRIMITIVE(open_output_vport);
  ADD_PRIMITIVE(input_vportp);
  ADD_PRIMITIVE(output_vportp);
  return TRUE;
}

