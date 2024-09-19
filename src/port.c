/*
 *  p o r t . c                 -- ports implementation
 *
 * Copyright © 1993-2024 Erick Gallesio <eg@stklos.net>
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
 *            Author: Erick Gallesio [eg@unice.fr]
 *    Creation date: 17-Feb-1993 12:27
 *
 */

#include <ctype.h>
#include "stklos.h"
#include "vm.h"


#define INITIAL_LINE_SIZE 256           /* Initial size for readline */

static SCM CrLf;                        /* used in read-line only */

static SCM io_error, io_port_error, io_read_error, io_write_error,
  io_closed_error, io_fn_error, io_malformed, io_prot_error,
  io_ro_error, io_exists_error, io_no_file_error, io_bad_param;


static void general_io_error(SCM type, char *format, SCM f)
{
  STk_raise_exception(STk_make_C_cond(type,
                                      4,
                                      STk_false,
                                      STk_vm_bt(),
                                      STk_format_error(format, f),
                                      f));
}

static void error_closed_port(SCM p)
{
  general_io_error(io_closed_error, "port ~S is closed", p);
}

static void error_bad_utf8_character(int byte)
{
  general_io_error(io_read_error, "bad UTF-8 byte: ~S", MAKE_INT(byte));
}

void STk_error_bad_io_param(char *fmt, SCM p)
{
  general_io_error(io_bad_param, fmt, p);
}

void STk_error_file_name(char *fmt, SCM fn)
{ STk_raise_exception(STk_make_C_cond(io_fn_error,
                                      6,
                                      STk_false,
                                      STk_vm_bt(),
                                      STk_format_error(fmt, fn),
                                      fn,
                                      STk_Cstring2string(fmt),
                                      LIST1(fn)));
}


void STk_error_bad_port(SCM p)
{
  general_io_error(io_port_error, "bad port ~S", p);
}


void STk_error_bad_file_name(SCM f)
{
  general_io_error(io_malformed, "bad file name ~S", f);
}

static void error_bad_binary_port(SCM port)
{
  general_io_error(io_malformed, "bad binary port ~S", port);
}

static void error_bad_textual_port(SCM port)
{
  general_io_error(io_malformed, "bad textual port ~S", port);
}

static void error_bad_string(SCM obj)
{
  general_io_error(io_bad_param, "bad string ~S", obj);
}


static SCM verify_port(SCM port, int mode)
{
  if (mode & PORT_WRITE) {
    if (!port) return STk_current_output_port();
    if (!OPORTP(port)) STk_error_bad_port(port);
  } else {
    if (!port) return STk_current_input_port();
    if (!IPORTP(port)) STk_error_bad_port(port);
  }
  if (PORT_IS_CLOSEDP(port)) error_closed_port(port);
  if ((mode & PORT_BINARY)  && !PORT_BINARYP(port))  error_bad_binary_port(port);
  if ((mode & PORT_TEXTUAL) && !PORT_TEXTUALP(port)) error_bad_textual_port(port);

  return port;
}


/*
<doc  input-port? output-port?
 * (input-port? obj)
 * (output-port? obj)
 *
 * Returns |#t| if |obj| is an input port or output port respectively,
 * otherwise returns |#f|.
doc>
 */
DEFINE_PRIMITIVE("input-port?", input_portp, subr1, (SCM port))
{
  return MAKE_BOOLEAN(IPORTP(port));
}

DEFINE_PRIMITIVE("output-port?", output_portp, subr1, (SCM port))
{
  return MAKE_BOOLEAN(OPORTP(port));
}

/*
<doc R7RS textual-port? binary-port?
 * (textual-port? obj)
 * (binary-port? obj)
 *
 * Returns |#t| if |obj| is a textual port or binary port respectively,
 * otherwise returns |#f|.
doc>
 */
DEFINE_PRIMITIVE("textual-port?", textual_portp, subr1, (SCM port))
{
  return MAKE_BOOLEAN(PORTP(port) && (PORT_FLAGS(port) & PORT_TEXTUAL));
}

DEFINE_PRIMITIVE("binary-port?", binary_portp, subr1, (SCM port))
{
  return MAKE_BOOLEAN(PORTP(port) && (PORT_FLAGS(port) & PORT_BINARY));
}


/*
<doc R7RS port?
 * (port? obj)
 *
 * Returns |#t| if |obj| is an input port or an output port,
 * otherwise returns |#f|.
doc>
 */
DEFINE_PRIMITIVE("port?", portp, subr1, (SCM port))
{
  return MAKE_BOOLEAN(PORTP(port));
}


/*
<doc EXT interactive-port?
 * (interactive-port? port)
 *
 * Returns |#t| if |port| is connected to a terminal and |#f| otherwise.
doc>
 */
DEFINE_PRIMITIVE("interactive-port?", interactive_portp, subr1, (SCM port))
{
  if (!PORTP(port)) STk_error_bad_port(port);

  return MAKE_BOOLEAN(PORT_FLAGS(port) & PORT_IS_INTERACTIVE);
}


/*
<doc  current-input-port current-output-port
 * (current-input-port obj)
 * (current-output-port obj)
 *
 * Returns the current default input or output port.
doc>
 */
/*
<doc EXT current-error-port
 * (current-error-port obj)
 *
 * Returns the current default error port.
doc>
 */

/* The 3 following C functions correspond to the R5RS primitives.
 * For compatibility reason, they are not renamed.
 */
SCM STk_current_input_port(void) { return STk_get_current_vm()->iport; }
SCM STk_current_output_port(void){ return STk_get_current_vm()->oport; }
SCM STk_current_error_port(void) { return STk_get_current_vm()->eport; }

/* The setters for the standard port (since current-xxx-port are parameters
 * in R7RS).
 */
static SCM STk_set_current_input_port(SCM port)
{
  if (!IPORTP(port)) STk_error_bad_port(port);
  return STk_get_current_vm()->iport = port;
}

static SCM STk_set_current_output_port(SCM port)
{
  if (!OPORTP(port)) STk_error_bad_port(port);
  return STk_get_current_vm()->oport = port;
}

static SCM STk_set_current_error_port(SCM port)
{
  if (!OPORTP(port)) STk_error_bad_port(port);
  return STk_get_current_vm()->eport = port;
}

/*=============================================================================*\
 *                              Read
\*=============================================================================*/

/*
<doc read
 * (read)
 * (read port)
 *
 * |Read| converts external representations of Scheme objects into the
 * objects themselves. |Read| returns the next object parsable from the given
 * input port, updating port to point to the first character past the end of
 * the external representation of the object.
 * @l
 * If an end of file is encountered in the input before any characters are found
 * that can begin an object, then an end of file object is returned. The port
 * remains open, and further attempts to read will also return an end of file
 * object. If an end of file is encountered after the beginning of an object's
 * external representation, but the external representation is incomplete
 * and therefore not parsable, an error is signalled.
 * @l
 * The port argument may be omitted, in which case it defaults to the value
 * returned by |current-input-port|. It is an error to read from a closed port.
 * @l
 * {{stklos}} |read| supports the {{quick-link-srfi 10}} |#,()| form that can be used
 * to denote values that do not have a convenient printed representation. See
 * the SRFI document for more information.
doc>
 */
/*
<doc read-ci
 * (read-ci)
 * (read-ci port)
 *
 * |Read-ci| result is identical to |read| except that symbols are always
 * read as case insensitive.
doc>
*/
/*
<doc EXT read-with-shared-structure
 * (read-with-shared-structure)
 * (read-with-shared-structure  port)
 * (read/ss)
 * (read/ss port)
 *
 * |read-with-shared-structure| is identical to |read|. It has been added to
 * be compatible with ,(link-srfi 38). STklos always knew how to deal with
 * recursive input data. |read/ss| is only a shorter name for
 * |read-with-shared-structure|.
 *
doc>
<doc EXT define-reader-ctor
 * (define-reader-ctor tag proc)
 *
 * This procedure permits to define a new user to reader constructor procedure
 * at run-time. It is defined in ,(link-srfi 10) document. See  SRFI document
 * for more information.
 * @lisp
 * (define-reader-ctor 'rev (lambda (x y) (cons y x)))
 * (with-input-from-string "#,(rev 1 2)" read)
 *                              => (2 . 1)
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("read", scheme_read, subr01, (SCM port))
{
  port = verify_port(port, PORT_READ | PORT_TEXTUAL);
  return STk_read(port, PORT_CASE_SENSITIVEP(port));
}

DEFINE_PRIMITIVE("read-ci", scheme_read_ci, subr01, (SCM port))
{
  port = verify_port(port, PORT_READ | PORT_TEXTUAL);
  return STk_read(port, 0);
}



/* The same one but for reading code => code is really constant */
DEFINE_PRIMITIVE("%read", scheme_read_cst, subr01, (SCM port))
{
  port = verify_port(port, PORT_READ | PORT_TEXTUAL);
  return STk_read_constant(port, PORT_CASE_SENSITIVEP(port));
}


/*
<doc  read-char
 * (read-char)
 * (read-char port)
 *
 * Returns the next character available from the input |port|, updating the |port|
 * to point to the following character. If no more characters are available,
 * an end of file object is returned. |Port| may be omitted, in which case
 * it defaults to the value returned by |current-input-port|.
doc>
 */
DEFINE_PRIMITIVE("read-char", read_char, subr01, (SCM port))
{
  int c;

  port = verify_port(port, PORT_READ | PORT_TEXTUAL);
  c = STk_get_character(port);
  if (c == UTF8_INCORRECT_SEQUENCE)
    error_bad_utf8_character(c);
  return (c == EOF) ? STk_eof : MAKE_CHARACTER(c);
}


/*
<doc EXT read-chars read-bytes
 * (read-bytes size)
 * (read-bytes size port)
 *
 * Returns a newly allocated string made of |size| characters read from |port|.
 * If less than |size| characters are available on the input port, the returned
 * string is smaller than |size| and its size is the number of available
 * characters. |Port| may be omitted, in which case it defaults to the
 * value returned by |current-input-port|.
 *
 * NOTE: This function was previously called |read-chars|. Usage
 * of the old name is deprecated.
doc>
 */
DEFINE_PRIMITIVE("read-bytes", read_bytes, subr12, (SCM size, SCM port))
{
  int count, n = STk_integer_value(size);
  SCM z;

  port = verify_port(port, PORT_READ);
  if (n < 0) STk_error("bad length");

  /* Allocate a new string for result  */
  z     = STk_makestring(n, NULL);
  count = STk_read_buffer(port, STRING_CHARS(z), n);

  if (count == 0)
    return STk_eof;
  if (count < n) {
    /* String is shorter than the allocated one */
    STRING_CHARS(z)[count] = '\0';
    return STk_makestring(count, STRING_CHARS(z));
  }
  return z;
}


/*
<doc EXT read-chars! read-bytes!
 * (read-bytes! str)
 * (read-bytes! str port)
 *
 * This function reads the characters available from |port| in the string |str|
 * by chuncks whose size is equal to the length of |str|.
 * The value returned by |read-bytes!| is an integer indicating the number
 * of characters read. |Port| may be omitted, in which case it defaults to the
 * value returned by |current-input-port|.
 *
 * This function is similar to |read-bytes| except that it avoids to allocate
 * a new string for each read.
 * @lisp
 * (define (copy-file from to)
 *   (let* ((size 1024)
 *          (in  (open-input-file from))
 *          (out (open-output-file to))
 *          (s   (make-string size)))
 *     (let Loop ()
 *       (let ((n (read-bytes! s in)))
 *         (cond
 *           ((= n size)
 *              (write-chars s out)
 *              (Loop))
 *           (else
 *              (write-chars (substring s 0 n) out)
 *              (close-port out)))))))
 * @end lisp
 *
 * NOTE: This function was previously called |read-chars!|. Usage
 * of the old name is deprecated.
doc>
 */
DEFINE_PRIMITIVE("read-bytes!", d_read_bytes, subr12, (SCM str, SCM port))
{
  port = verify_port(port, PORT_READ);
  if (!STRINGP(str)) error_bad_string(str);

  return MAKE_INT(STk_read_buffer(port, STRING_CHARS(str), STRING_LENGTH(str)));
}


/*
<doc R7RS read-bytevector
 * (read-bytevector k)
 * (read-bytevector k port)
 *
 * Reads the next |k| bytes, or as many as are available
 * before the end of file, from the textual input |port| into a
 * newly allocated string in left-to-right order and returns the
 * string. If no characters are available before the end of file,
 * an end-of-file object is returned.
doc>
*/
DEFINE_PRIMITIVE("read-bytevector", read_bytevector, subr12, (SCM size, SCM port))
{
  long count, n = STk_integer_value(size);
  SCM z;

  port = verify_port(port, PORT_READ | PORT_BINARY);
  if (n < 0) STk_error("bad length");

  /* Allocate a new bytevector for result */
  z     = STk_make_C_bytevector(n);
  count = STk_read_buffer(port, UVECTOR_DATA(z), n);

  if (n && !count)
    return STk_eof;
  if (count < n) {
    /* result is shorter than the allocated bytevector */
    return STk_make_bytevector_from_C_string(UVECTOR_DATA(z), count);
  }
  return z;
}


DEFINE_PRIMITIVE("%read-bytevector!", d_read_bytevector, subr4,
                 (SCM bv, SCM port, SCM start, SCM end))
{
  long vstart = STk_integer_value(start);
  long vend   = STk_integer_value(end);
  long count, n;

  port = verify_port(port, PORT_READ | PORT_BINARY);
  if (!BYTEVECTORP(bv)) STk_error("bad bytevector ~S", bv);

  if (vstart < 0) STk_error("bad start value ~S", start);
  if (vend == LONG_MIN || vend > UVECTOR_SIZE(bv))
    STk_error("bad end value ~S", end);
  if (vstart > vend) STk_error("start index is bigger than end index");

  n     = vend - vstart;
  count = STk_read_buffer(port, UVECTOR_DATA(bv)+ vstart, n);

  if (n && !count)
    return STk_eof;
  return MAKE_INT(count);
}



/*
<doc EXT read-byte
 * (read-byte)
 * (read-byte port)
 *
 * Returns the next character available from the input |port| as an integer.
 * If the end of file is reached, this function returns the end of file
 * object.
doc>
*/
DEFINE_PRIMITIVE("read-byte", read_byte, subr01, (SCM port))
{
  int c;

  port = verify_port(port, PORT_READ);
  c = STk_getc(port);
  return (c == EOF) ? STk_eof : MAKE_INT(c);
}

/*
<doc  peek-char
 * (peek-char)
 * (peek-char port)
 *
 * Returns the next character available from the input |port|, without updating
 * the port to point to the following character. If no more characters are
 * available, an end of file object is returned. |Port| may be omitted, in
 * which case it defaults to the value returned by |current-input-port|.
 * @l
 * NOTE: The value returned by a call to |peek-char| is the same as the
 * value that would have been returned by a call to |read-char| with the same
 * port. The only difference is that the very next call to |read-char| or
 * |peek-char| on that port will return the value returned by the preceding
 * call to |peek-char|. In particular, a call to |peek-char| on an interactive
 * port will hang waiting for input whenever a call to |read-char| would have
 * hung.
doc>
 */
DEFINE_PRIMITIVE("peek-char", peek_char, subr01, (SCM port))
{
  int c;

  port = verify_port(port, PORT_READ | PORT_TEXTUAL);

  c = STk_get_character(port);
  if (c == UTF8_INCORRECT_SEQUENCE) error_bad_utf8_character(c);
  STk_ungetc(c, port);

  return (c == EOF) ? STk_eof : MAKE_CHARACTER(c);
}

/*
<doc EXT peek-byte
 * (peek-byte)
 * (peek-byte port)
 *
 * Returns the next character available from the input |port|, without updating
 * the port to point to the following character. Whereas |peek-char|
 * returns a character, this function returns an integer between 0and 255.
doc>
*/
DEFINE_PRIMITIVE("peek-byte", peek_byte, subr01, (SCM port))
{
  int c;

  port = verify_port(port, PORT_READ);
  c = STk_getc(port);
  STk_ungetc(c, port);

  return (c == EOF) ? STk_eof : MAKE_INT(c);
}


/*
<doc  eof-object?
 * (eof-object? obj)
 *
 * Returns |#t| if |obj| is an end of file object, otherwise returns |#f|.
doc>
 */
DEFINE_PRIMITIVE("eof-object?", eof_objectp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(obj == STk_eof);
}


/*
<doc EXT eof-object
 * (eof-object)
 *
 * (((#eof)))
 * ((end of file))
 * Returns an end of file object. Note that the special notation |#eof| is
 * another way to return such an end of file object.
doc>
 */
DEFINE_PRIMITIVE("eof-object", eof_object, subr0, (void))
{
  return STk_eof;
}


/*
<doc  char-ready?
 * (char-ready?)
 * (char-ready? port)
 *
 * Returns |#t| if a character is ready on the input port and returns |#f|
 * otherwise. If char-ready returns |#t| then the next read-char operation on
 * the given port is guaranteed not to hang. If the port is at end of file
 * then |char-ready?| returns |#t|. Port may be omitted, in which case it
 * defaults to the value returned by |current-input-port|.
doc>
 */
DEFINE_PRIMITIVE("char-ready?", char_readyp, subr01, (SCM port))
{
  port = verify_port(port, PORT_READ | PORT_TEXTUAL);
  return MAKE_BOOLEAN(STk_readyp(port));
}


/*
<doc R7RS u8-ready?
 * (u8-ready?)
 * (u8-ready? port)
 *
 * Returns |#t| if a byte is ready on the binary input |port| and
 * returns |#f| otherwise. If |u8-ready?| returns |#t| then the
 * next read-u8 operation on the given port is guaranteed
 * not to hang. If the |port| is at end of file then |u8-ready?|
 * returns |#t|.
doc>
*/
DEFINE_PRIMITIVE("u8-ready?", u8_readyp, subr01, (SCM port))
{
  port = verify_port(port, PORT_READ | PORT_BINARY);
  return MAKE_BOOLEAN(STk_readyp(port));
}


/*=============================================================================*\
 *                              Write
\*=============================================================================*/


/*
<doc  write
 * (write obj)
 * (write obj port)
 *
 * Writes a written representation of |obj| to the given |port|. Strings that
 * appear in the written representation are enclosed in doublequotes, and
 * within those strings backslash and doublequote characters are escaped
 * by backslashes. Character objects are written using the ,(emph "#\\") notation.
 * |Write| returns an unspecified value. The |port| argument may be omitted, in
 * which case it defaults to the value returned by |current-output-port|.
doc>
 */
DEFINE_PRIMITIVE("write", write, subr12, (SCM expr, SCM port))
{
  port = verify_port(port, PORT_WRITE | PORT_TEXTUAL);
  STk_print(expr, port, WRT_MODE);
  return STk_void;
}


/*
<doc R7RS write-shared write*
 * (write-shared obj)
 * (write-shared obj port)
 *
 * Writes a written representation of |obj| to the given port.  The
 * main difference with the |write| procedure is that |write*|
 * handles data structures with cycles. Circular structure written by
 * this procedure use the `"{{sharp}}n="`)) and  `"{{sharp}}n{{sharp}}"`))
 * notations (see <<_other_notations>>).
 *
 * NOTE: This function is also called |write*|.
 * The name |write*| was the name used by {{stklos}} for
 * |write-shared| before it was introduced in R7RS.
 *
doc>
<doc EXT write-with-shared-structure
 * (write-with-shared-structure obj)
 * (write-with-shared-structure obj port)
 * (write-with-shared-structure obj port optarg)
 * (write/ss obj)
 * (write/ss obj port)
 * (write/ss obj port optarg)
 *
 * |write-with-shared-structure| has been added to be compatible with
 * {{link-srfi 38}}. It is is identical to |write*|, except that it accepts one
 * more parameter (|optarg|). This parameter, which is not specified
 * in {{quick-link-srfi 38}}, is always ignored. |write/ss| is only a shorter name for
 * |write-with-shared-structure|.
 *
doc>
*/
DEFINE_PRIMITIVE("write*", write_star, subr12, (SCM expr, SCM port))
{
  port = verify_port(port, PORT_WRITE | PORT_TEXTUAL);
  STk_print_star(expr, port, WRT_MODE);
  return STk_void;
}

/*
<doc display
 * (display obj)
 * (display obj port)
 *
 * Writes a representation of |obj| to the given |port|. Strings that
 * appear in the written representation are not enclosed in
 * doublequotes, and no characters are escaped within those
 * strings. Character objects appear in the representation as if
 * written by |write-char| instead of by |write|. |Display| returns an
 * unspecified value. The |port| argument may be omitted, in which
 * case it defaults to the value returned by |current-output-port|.
 * @l
 * IMPORTANT: |Write| is intended for producing machine-readable
 * output and |display| is for producing human-readable output.
 * @l
 * NOTE: As required by R7RS does not loop forever when
 * |obj| contains self-references.
doc>
 */
DEFINE_PRIMITIVE("display", display, subr12, (SCM expr, SCM port))
{
  port = verify_port(port, PORT_WRITE | PORT_TEXTUAL);
  STk_print_star(expr, port, DSP_MODE);
  return STk_void;
}


/*
<doc EXT display-simple
 * (display-simple obj)
 * (display-simple obj port)
 *
 * The |display-simple| procedure is the same as |display|, except
 * that shared structure is never represented using datum labels.
 * This can cause |display-simple| not to terminate if |obj|
 * contains circular structure.
doc>
 */
DEFINE_PRIMITIVE("display-simple", display_simple, subr12, (SCM expr, SCM port))
{
  port = verify_port(port, PORT_WRITE | PORT_TEXTUAL);
  STk_print(expr, port, DSP_MODE);
  return STk_void;
}
/*
<doc EXT display-shared
 * (display-shared obj)
 * (display-shared obj port)
 *
 * The |display-shared| procedure is the same as |display|, except
 * that shared structure are represented using datum labels.
doc>
 */

/* Aliased to display in lib/bonus.stk */


/*
<doc  newline
 * (newline)
 * (newline port)
 *
 * Writes an end of line to |port|. Exactly how this is done differs from
 * one operating system to another. Returns an unspecified value. The |port|
 * argument may be omitted, in which case it defaults to the value returned
 * by |current-output-port|.
doc>
 */
DEFINE_PRIMITIVE("newline", newline, subr01, (SCM port))
{
  port = verify_port(port, PORT_WRITE | PORT_TEXTUAL);
  STk_putc('\n', port);
  return STk_void;
}



/*
<doc  write-char
 * (write-char char)
 * (write-char char port)
 *
 * Writes the character |char| (not an external representation of the
 * character) to the given |port| and returns an unspecified value.
 * The |port| argument may be omitted, in which case it defaults to the
 * value returned by |current-output-port|.
doc>
 */
DEFINE_PRIMITIVE("write-char", write_char, subr12, (SCM c, SCM port))
{
  if (!CHARACTERP(c)) STk_error_bad_io_param("bad character ~S", c);
  port = verify_port(port, PORT_WRITE | PORT_TEXTUAL);
  STk_put_character(CHARACTER_VAL(c), port);
  return STk_void;
}

DEFINE_PRIMITIVE("%write-string", write_string, subr4, (SCM str, SCM port,
                                                        SCM start, SCM end))
{
  long vstart = STk_integer_value(start);
  long vend   = STk_integer_value(end);

  if (!STRINGP(str)) error_bad_string(str);
  verify_port(port, PORT_WRITE | PORT_TEXTUAL);

  if (vstart < 0) STk_error("bad start value ~S", start);
  if (vend == LONG_MIN || vend > STRING_LENGTH(str))
    STk_error("bad end value ~S", end);
  if (vstart > vend) STk_error("start index is bigger than end index");

  if (STk_use_utf8 && !STRING_MONOBYTE(str)) {
    char *adr_start = STk_utf8_index(STRING_CHARS(str), vstart, STRING_SIZE(str));
    char *adr_end   = STk_utf8_index(STRING_CHARS(str), vend,   STRING_SIZE(str));

    STk_write_buffer(port, adr_start, adr_end - adr_start);
  }
  else {
    STk_write_buffer(port, STRING_CHARS(str)+vstart, vend -vstart);
  }
  return STk_void;
}


/*
<doc EXT write-chars
 * (write-chars str)
 * (write-chars str port)
 *
 * Writes the characters of string |str| to the given |port| and
 * returns an unspecified value.  The |port| argument may be omitted,
 * in which case it defaults to the value returned by
 * |current-output-port|.
 * @l
 * NOTE: This function is generally
 * faster than |display| for strings. Furthermore, this primitive does
 * not use the buffer associated to |port|.
 *
doc>
 */
DEFINE_PRIMITIVE("write-chars", write_chars, subr12, (SCM str, SCM port))
{
  if (!STRINGP(str)) error_bad_string(str);
  port = verify_port(port, PORT_WRITE);
  STk_write_buffer(port, STRING_CHARS(str), STRING_SIZE(str));
  return STk_void;
}



/*
<doc EXT write-byte
 * (write-byte b)
 * (write-byte b port)
 *
 * Write byte |b| to the port. |b| must be an exact integer in range between 0
 * and 255.
doc>
*/
DEFINE_PRIMITIVE("write-byte", write_byte, subr12, (SCM byte, SCM port))
{
  long b = STk_integer_value(byte);

  if ((b < 0) || (b > 255))
    STk_error_bad_io_param("bad byte value ~S", byte);
  port = verify_port(port, PORT_WRITE);
  STk_putc(b, port);
  return STk_void;

}


/*===========================================================================*\
 *
 *                      S T k   b o n u s
 *
\*===========================================================================*/
#define FMT_SIZE 7


static SCM internal_format(int argc, SCM *argv, int error)
     /* a very simple and poor format */
{
  SCM port, fmt;
  int format_in_string = 0;
  char *p, *start_fmt = "", prev_char;

  if (error) {
    if (argc < 1) goto Bad_list;
    format_in_string = 1;
    port = STk_open_output_string();
    argc -= 1;
  }
  else {
    if (STRINGP(*argv)) {
      /* This is a SRFI-28 format */
      format_in_string = 1;
      port = STk_open_output_string();
      argc -= 1;
    } else {
      if (argc < 2) goto Bad_list;
      port = *argv--;
      argc -= 2;

      if (BOOLEANP(port)){
        if (port == STk_true) port = STk_current_output_port();
        else {
          format_in_string = 1;
          port = STk_open_output_string();
        }
      } else {
        verify_port(port, PORT_WRITE | PORT_TEXTUAL);
      }
    }
  }

  fmt = *argv--;
  if (!STRINGP(fmt)) STk_error_bad_io_param("bad format string ~S", fmt);

  /* Parse the format string */
  start_fmt = STRING_CHARS(fmt);
  prev_char = ' ';

  for(p = start_fmt; *p; p++) {
    if (*p == '~') {
      switch(*(++p)) {
        case '\0':{  /* A ~ at the end of the string */
                     STk_putc('~', port);
                     goto EndFormatAnalysis;
                  }
        case 'A':
        case 'a': {
                    SCM tmp;

                    if (argc-- <= 0) goto TooMuch;
                    tmp = *argv--;
                    if (STRINGP(tmp)) {
                      if (STRING_SIZE(tmp) > 0)
                        prev_char = STRING_CHARS(tmp)[STRING_SIZE(tmp) - 1];
                    }
                    else if (CHARACTERP(tmp))
                      prev_char= CHARACTER_VAL(tmp);

                    STk_print(tmp, port, DSP_MODE);
                    continue;           /* because we set ourselves prev_char */
                  }
        case 'S':
        case 's': if (argc-- <= 0) goto TooMuch;
                  STk_print(*argv--, port, WRT_MODE);
                  break;
        case 'W':
        case 'w': if (argc-- <= 0) goto TooMuch;
          STk_print_star(*argv--, port, WRT_MODE);
                  break;
        case 'X':
        case 'x': if (argc-- <= 0) goto TooMuch;
                  STk_print(STk_number2string(*argv--, MAKE_INT(16)),port,DSP_MODE);
                  break;
        case 'D':
        case 'd': if (argc-- <= 0) goto TooMuch;
                  STk_print(STk_number2string(*argv--, MAKE_INT(10)),port,DSP_MODE);
                  break;
        case 'O':
        case 'o': if (argc-- <= 0) goto TooMuch;
                  STk_print(STk_number2string(*argv--, MAKE_INT(8)),port,DSP_MODE);
                  break;
        case 'B':
        case 'b': if (argc-- <= 0) goto TooMuch;
                  STk_print(STk_number2string(*argv--, MAKE_INT(2)),port,DSP_MODE);
                  break;
        case 'C':
        case 'c': if (argc-- <= 0) goto TooMuch;
                  if (!CHARACTERP(*argv))
                    STk_error_bad_io_param("bad character ~S", *argv);
                  prev_char = CHARACTER_VAL(*argv);
                  STk_print(*argv--, port, DSP_MODE);
                  continue;     /* because we set ourselves prev_char */
        case 'Y':
        case 'y': {                                     /* Yuppify */
                      SCM ref, pp;

                      if (argc-- <= 0) goto TooMuch;
                      pp = STk_lookup(STk_intern("pp"),
                                      STk_current_module(),
                                      &ref,
                                      TRUE);
                      STk_print(STk_C_apply(pp, 3, *argv--,
                                            STk_makekey("port"),
                                            STk_false),
                                port,
                                WRT_MODE);
                      prev_char = '\n'; /* since our pp always add a newline */
                      continue;         /* because we set ourselves prev_char */
        }
        case 'F':
        case 'f':
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9': {
                  char width[FMT_SIZE], digits[FMT_SIZE];
                  SCM ff, ref, tmp;
                  int i;

                  *width = *digits = '\0'; /* initialize arrays */

                  if (argc-- <= 0) goto TooMuch;

                  for (i=0; isdigit(*p); i++) {
                    if (i >= FMT_SIZE) goto Incorrect_format_width;
                    width[i] = *p++;
                  }
                  if (!i)
                    /* no size given <=> 1 */
                    width[i++] = '1';
                  width[i] = '\0';

                  if (*p == ',') {
                    p++;
                    for (i=0; isdigit(*p); i++) {
                      if (i >= FMT_SIZE) goto Incorrect_format_width;
                      digits[i] = *p++;
                    }
                    digits[i] = '\0';
                  }
                  if (*p != 'f' && *p != 'F') goto Incorrect_format_width;

                  /* width and digits are strings which contains the width
                   * and the number of digits for the format
                   * Call the Scheme routine srfi48:format-fixed
                   */
                  ff = STk_lookup(STk_intern("srfi48:format-fixed"),
                                  STk_current_module(),
                                  &ref,
                                  TRUE);

                  tmp = STk_C_apply(ff, 3,
                                        *argv--,
                                        STk_Cstr2number(width, 10L),
                                        STk_Cstr2number(digits, 10L));

                  if (STRINGP(tmp)) {
                    if (STRING_SIZE(tmp) > 0)
                      prev_char = STRING_CHARS(tmp)[STRING_SIZE(tmp) - 1];
                  }
                  STk_print(tmp, port, DSP_MODE);
                  continue;
        }
        case '?':
        case 'K':
        case 'k': {
                    SCM fmt, ref,args;
                    int len;

                    if (argc-- <= 0) goto TooMuch;
                    fmt = *argv--;
                    if (!STRINGP(fmt))
                      STk_error_bad_io_param("bad string for ~~? format ~S", fmt);

                    if (argc-- <= 0) goto TooMuch;
                    args = *argv--;
                    len  = STk_int_length(args);
                    if (len < 0)
                      STk_error_bad_io_param("bad list for ~~? format ~S", args);

                    /* Do (apply format port fmt args) */
                    STk_C_apply_list(STk_lookup(STk_intern("format"),
                                                STk_current_module(), &ref, TRUE),
                                     STk_cons(port, STk_cons(fmt, args)));
                    break;
                  }
        case 'H':
        case 'h': {                                     /* Help */
                     SCM ref, help;

                      help = STk_lookup(STk_intern("srfi48:help"),
                                        STk_current_module(),
                                        &ref,
                                        TRUE);
                      STk_C_apply(help, 1, port);
                      break;
        }
        case 'T':
        case 't': STk_putc('\t', port);
                  break;
        case '_': STk_putc(' ',port);
                  break;
        case '&': if (prev_char == '\n') continue; /* FALLTHROUGH */
        case '%': STk_putc('\n', port);
                  prev_char = '\n';
                  continue;
        case '~': STk_putc('~', port);
                  break;
        default:  STk_putc('~',  port);
                  if (*p) STk_putc(*p, port);
      }
      prev_char = '?';
    } else {
      /* Not a ~ sequence */
      prev_char = *p;
      STk_putc(*p, port);
    }
  }

 EndFormatAnalysis:
  /* Verify that it doesn't remain arguments on the list */
  if (argc)
    STk_error_bad_io_param("too few ``~~'' in format string %S", start_fmt);

  return format_in_string ? STk_get_output_string(port) : STk_void;

TooMuch:
  STk_error_bad_io_param("too many ``~~'' in format string %S", start_fmt);
Bad_list:
  STk_error_bad_io_param("bad list of parameters ~S", *argv);
Incorrect_format_width:
  STk_error_bad_io_param("Format too long or 'f' expected in %S", start_fmt);

  return STk_void;
}

/*
<doc EXT format
 * (format port str obj ...)
 * (format str obj)
 *
 * Writes the |obj|s to the given |port|, according to the format
 * string |str|. |Str| is written literally, except for the following
 * sequences:
 *
 * - |~a| or |~A| is replaced by the printed representation of the
 *   next |obj|.
 *
 * - |~s| or |~S| is replaced by the _slashified_ printed
 *   representation of the next |obj|.
 *
 * - |~w| or |~W| is replaced by the printed representation
 *   of the next |obj| (circular structures are correctly handled and
 *   printed using |write*|).
 *
 * - |~d| or |~D| is replaced by the decimal printed representation
 *   of the next |obj| (which must be a number).
 *
 * - |~x| or |~X| is replaced by the hexadecimal printed representation
 *   of the next |obj| (which must be a number).
 *
 * - |~o| or |~O| is replaced by the octal printed representation
 *   of the next |obj| (which must be a number).
 *
 * - |~b| or |~B| is replaced by the binary printed representation
 *   of the next |obj| (which must be a number).
 *
 * - |~c| or |~C| is replaced by the printed representation
 *   of the next |obj| (which must be a character).
 *
 * - |~y| or |~Y| is replaced by the pretty-printed representation
 *   of the next |obj|. The standard pretty-printer is used here.
 *
 * - |~?| is replaced by the result of the recursive call of |format|
 *   with the two next |obj|: the first item should be a string, and the
 *   second, a list with the arguments.
 *
 * - |~k| or |~K| is another name for |~?|
 *
 * - |~[w[,d]]f| or |~[w[,d]]F| is replaced by the printed
 *   representation of next |obj| (which must be a number) with width |w|
 *   and |d| digits after the decimal. Eventually, |d| may be omitted.
 *
 * - |~~| is replaced by a single tilde character.
 *
 * - |~%| is replaced by a newline
 *
 * - |~t| or |~T| is replaced by a tabulation character.
 *
 * - |~&| is replaced by a newline character if it is known that the
 *   previous character was not a newline
 *
 * - |~_| is replaced by a space
 *
 * - |~h| or |~H| provides some help
 *
 * |Port| can be a boolean or a port. If |port| is |#t|, output goes to
 * the current output port; if |port| is |#f|, the output is returned as a
 * string.  Otherwise, the output is printed on the specified port.
 * @lisp
 *    (format #f "A test.")        => "A test."
 *    (format #f "A ~a." "test")   => "A test."
 *    (format #f "A ~s." "test")   => "A \"test\"."
 *    (format "~8,2F" 1/3)         => "    0.33"
 *    (format "~6F" 32)            => "    32"
 *    (format "~1,2F" 4321)        => "4321.00"
 *    (format "~1,2F" (sqrt -3.9)) => "0.00+1.97i"
 *    (format "#d~d #x~x #o~o #b~b~%" 32 32 32 32)
 *                                 => "#d32 #x20 #o40 #b100000\n"
 *    (format #f "~&1~&~&2~&~&~&3~%")
 *                                 => "\n1\n2\n3\n"
 *    (format "~a ~? ~a" 'a "~s" '(new) 'test)
 *                                 => "a new test"
 * @end lisp
 *
 * NOTE: The second form of |format| is compliant with {{link-srfi 28}}.
 * That is, when |port| is omitted, the output is returned as a string as if
 * |port| was given the value |#f|.
 *
 * NOTE: Since version 0.58, |format| is also compliant with {{link-srfi 48}}.
doc>
 */
DEFINE_PRIMITIVE("format", format, vsubr, (int argc, SCM *argv))
{
  return internal_format(argc, argv, FALSE);
}


/*
<doc R7RS error
 * (error str obj ...)
 * (error name str obj ...)
 *
 * |error| is used to signal an error to the user. The second form
 * of |error| takes  a symbol as first parameter; it is generally used for the
 * name of the procedure which raises the error.
 *
 * NOTE: {{rseven}} permits only the fist form of call. Using a symbol as first
 * parameter is {{stklos}} specific.
 * Furthermore, the specification string may follow the _tilde conventions_ of
 * |format| (see _<<format, primitive `format`>>_); in this case this
 * procedure builds an error message according to the specification
 * given in |str|. Otherwise,
 * this procedure is in conformance with the |error| procedure defined in
 * {{link-srfi 23}} and |str| is printed with the |display| procedure,
 * whereas the |obj| parameters are printed  with the |write| procedure.
 *
 * Hereafter, are some calls of the |error| procedure using a formatted string
 * @lisp
 * (error "bad integer ~A" "a")
 *                      @print{} bad integer a
 * (error 'vector-ref "bad integer ~S" "a")
 *                      @print{} vector-ref: bad integer "a"
 * (error 'foo "~A is not between ~A and ~A" "bar" 0 5)
 *                      @print{} foo: bar is not between 0 and 5
 * @end lisp
 *
 * and some conform to {{quick-link-srfi 23}}
 * @lisp
 * (error "bad integer" "a")
 *                     @print{} bad integer "a"
 * (error 'vector-ref "bad integer" "a")
 *                    @print{} vector-ref: bad integer "a"
 * (error "bar" "is not between" 0 "and" 5)
 *                    @print{} bar "is not between" 0 "and" 5
 * @end lisp
doc>
 */
static SCM srfi_23_error(int argc, SCM *argv)
{
  SCM port = STk_open_output_string();

  STk_print(*argv--, port, DSP_MODE); /* the message (we know that it exists) */
  for (argc--; argc; argc--) {
    STk_putc(' ', port);
    STk_print(*argv--, port, WRT_MODE);
  }
  STk_close_port(port);
  return STk_get_output_string(port);
}

static int msg_use_tilde(char *s)
{
  char *p;

  p = strchr(s, '~');
  return p ? (p[1] && strchr("aAsSwW~", p[1]) != NULL): 0;
}

static SCM do_error(SCM type, int argc, SCM *argv)
{
  SCM who            = STk_false;
  SCM r7rs_msg       = STk_false;
  SCM r7rs_irritants = STk_nil;

  if (argc > 0) {
    if (SYMBOLP(*argv)) {
      who = *argv;
      argc -= 1;
      argv -= 1;
    }
    if (argc > 0) {
      SCM msg;
      // R7RS specifies that the message and the irritants must be
      // available as such. Build these values explicitly.
      r7rs_msg = *argv;
      r7rs_irritants = STk_list(argc-1, argv-1);

      /* See if we have a formatted message or a plain SRFI-23 call */
      if (STRINGP(*argv) && !msg_use_tilde(STRING_CHARS(*argv)))
        msg = srfi_23_error(argc, argv);
      else
        msg = internal_format(argc, argv, TRUE);
      STk_signal_error(type, who, msg, r7rs_msg, r7rs_irritants);
    }
  }
  r7rs_msg = STk_Cstring2string("");
  STk_signal_error(type, who, r7rs_msg, r7rs_msg, r7rs_irritants);
  return STk_void;
}


DEFINE_PRIMITIVE("error", scheme_error, vsubr, (int argc, SCM *argv))
{
  return do_error(STk_err_mess_condition, argc, argv);
}


/*
<doc EXT signal-error
 * (signal-error cond str obj ...)
 * (signal-error cond name str obj ...)
 *
 * This procedure is similar to error, except that the type of the error
 * can be passed as the first parameter. The type of the error must be a
 * condition which inherits from |&error-message|.
 * @l
 * Note that |(error arg ...)| is equivalent to
 * @lisp
 * (signal-error &error-message arg ...)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("signal-error", scheme_signal_error, vsubr, (int argc, SCM *argv))
{
  SCM type_error;

  if (! argc) STk_error("error condition expected");

  type_error = *argv;
  argc -= 1;
  argv -= 1;

  if (STk_condition_type_is_a(type_error, STk_err_mess_condition) == STk_false)
    STk_error("bad &error-message ~S", type_error);
  return do_error(type_error, argc, argv);
}


/*
<doc close-input-port close-output-port
 * (close-input-port port)
 * (close-output-port port)
 *
 * Closes the port associated with |port|, rendering the port incapable of
 * delivering or accepting characters. These routines have no effect if the
 * port has already been closed. The value returned is *_void_*.
doc>
 */
DEFINE_PRIMITIVE("close-input-port", close_input_port, subr1, (SCM port))
{
  if (!IPORTP(port)) STk_error_bad_port(port);
  STk_close(port);
  return STk_void;
}

DEFINE_PRIMITIVE("close-output-port", close_output_port, subr1, (SCM port))
{
  if (!OPORTP(port)) STk_error_bad_port(port);
  STk_close(port);
  return STk_void;
}


/*
<doc R7RS close-port
 * (close-port port)
 *
 * Closes the port associated with |port|.
doc>
 */
DEFINE_PRIMITIVE("close-port", close_port, subr1, (SCM port))
{
  if (!PORTP(port)) STk_error_bad_port(port);

  STk_close(port);
  return STk_void;
}

/*
<doc EXT port-open? port-closed?
 * (port-closed? port)
 * (port-open?  port)
 *
 * |port-closed?| returns |#t| if |port| is closed and |#f| otherwise.
 * On the contrary, |port-open?| returns |#t| if |port| is open and
 * |#f| otherwise.
 * @l
 * NOTE: |port-closed?| was the usual STklos function to
 * test if a port is closed. |port-open?| has been added to be the companion
 * of the R7RS functions |input-port-open?| and |output-port-open?|
doc>
*/
DEFINE_PRIMITIVE("port-closed?", port_closed, subr1, (SCM port))
{
  if (!PORTP(port)) STk_error_bad_port(port);

  return MAKE_BOOLEAN(PORT_IS_CLOSEDP(port));
}

DEFINE_PRIMITIVE("port-open?", port_open, subr1, (SCM port))
{
  if (!PORTP(port)) STk_error_bad_port(port);

  return MAKE_BOOLEAN(!PORT_IS_CLOSEDP(port));
}


/*
<doc EXT read-line
 * (read-line)
 * (read-line port)
 *
 * Reads the next line available from the input port |port|. This function
 * returns 2 values: the first one is the string which contains the line
 * read, and the second one is the end of line delimiter. The end of line
 * delimiter can be an end of file object, a character or a string in case
 * of a multiple character delimiter. If no more characters are available
 * on |port|, an end of file object is returned.  |Port| may be omitted,
 * in which case it defaults to the value returned by |current-input-port|.
 * @l
 * NOTE: As said in _<<values, primitive `values`>>_, if |read-line| is not
 * used in  the context of |call-with-values|, the second value returned by
 * this procedure is ignored.
doc>
*/
DEFINE_PRIMITIVE("read-line", read_line, subr01, (SCM port))
{
  int prev, c;
  char buffer[INITIAL_LINE_SIZE], *buff;
  size_t i, size = INITIAL_LINE_SIZE;
  SCM res, delim;

  port = verify_port(port, PORT_READ | PORT_TEXTUAL);
  buff = buffer;
  prev = ' ';

  for (i = 0; ; i++) {
    if (i == size) {
      /* We must enlarge the buffer */
      size += size / 2;
      if (i == INITIAL_LINE_SIZE) {
        /* This is the first resize. Pass from static to dynamic allocation */
        buff = STk_must_malloc_atomic(size);
        memcpy(buff, buffer, INITIAL_LINE_SIZE);
      }
      else
        buff = STk_must_realloc(buff, size);
    }
    switch (c = STk_getc(port)) {
      case EOF:  res = (i == 0) ? STk_eof : STk_makestring(i, buff);
                 if (buff != buffer) STk_free(buff);
                 return STk_n_values(2, res, STk_eof);

      case '\n': if (prev == '\r')
                   { i -= 1; delim = CrLf; }
                 else
                   delim = MAKE_CHARACTER('\n');

                 res = STk_makestring(i, buff);
                 if (buff != buffer) STk_free(buff);
                 return STk_n_values(2, res, delim);

      default:  buff[i] = prev = c;
    }
  }
}

/*
<doc EXT copy-port
 * (copy-port in out)
 * (copy-port in out max)
 *
 * Copy the content of port |in|, which must be opened for reading, on
 * port |out|, which must be opened for writing. If |max| is not specified,
 * All the characters from the input port are copied on ouput port. If |max|
 * is specified, it must be an integer indicating the maximum number of characters
 * which are copied from |in| to |out|.
doc>
*/
#define COPY_PORT_SIZE 4096
DEFINE_PRIMITIVE("copy-port", copy_port, subr23, (SCM p1, SCM p2, SCM max))
{
  char buffer[COPY_PORT_SIZE];
  int n, m, sz = -1;

  if (!IPORTP(p1)) STk_error_bad_port(p1);
  if (!OPORTP(p2)) STk_error_bad_port(p2);
  if (max) {
    sz = STk_integer_value(max);
    if (sz < 0)
      STk_error("bad size ~S", max);
  }

  /* Copy at most sz characters from p1 to p2 */
  for ( ; ; ) {
    if (sz < 0) {
      n = COPY_PORT_SIZE;
    } else if (sz > COPY_PORT_SIZE) {
      n = COPY_PORT_SIZE;
      sz -= COPY_PORT_SIZE;
    } else {
      n = sz;
      sz = 0;
    }

    if (n == 0) break;
    if ((n = STk_read_buffer(p1, buffer, n)) > 0) {
      m = STk_write_buffer(p2, buffer, n);
      if (n != m) goto Error;
    }
    if (n <= 0) break;
  }
  if (n != 0) goto Error;
  return STk_void;

 Error:
  STk_error("problem while copying port ~S on port ~S (~S)",
            p1 , p2, STk_Cstring2string(strerror(errno)));
  return STk_void;
}

/*
<doc EXT flush-output-port
 * (flush-output-port)
 * (flush-output-port port)
 *
 * Flushes the buffer associated with the given output |port|. The
 * |port| argument may be omitted, in which case it defaults to the value
 * returned by |current-output-port|
doc>
 */
DEFINE_PRIMITIVE("flush-output-port", port_flush, subr01, (SCM port))
{
  port = verify_port(port, PORT_WRITE);
  if (STk_flush(port))
    general_io_error(io_write_error, "cannot flush port ~S", port);
  return STk_void;
}


/*
<doc EXT port-current-line
 * (port-current-line)
 * (port-current-line port)
 *
 * Returns the current line number associated to the given input |port| as an
 * integer. The |port| argument may be omitted, in which case it defaults to
 * the value returned by |current-input-port|.
 * @l
 * NOTE: The |port-seek|, |read-chars| and |read-chars!| procedures
 * generally break the line-number. After using one of these procedures, the
 * value returned by |port-current-line| will be |-1| (except a |port-seek|
 * at the beginning of the port reinitializes the line counter).
doc>
 */
DEFINE_PRIMITIVE("port-current-line", port_current_line, subr01, (SCM port))
{
  port = verify_port(port, PORT_READ | PORT_TEXTUAL);
  return MAKE_INT(PORT_LINE(port));
}


/*
<doc EXT port-current-position
 * (port-current-position)
 * (port-current-position port)
 *
 * Returns the position associated to the given |port| as an
 * integer (i.e. number of characters from the beginning of the port).
 * The |port| argument may be omitted, in which case it defaults to
 * the value returned by |current-input-port|.
doc>
 */
DEFINE_PRIMITIVE("port-current-position", port_position, subr01, (SCM port))
{
  if (!port)
    port = STk_current_input_port();
  else
    if (!PORTP(port)) STk_error_bad_port(port);
  return MAKE_INT(STk_tell(port));
}


/*
<doc EXT seek-file-port
 * (port-seek port pos)
 * (port-seek port pos whence)
 *
 * Sets the file position for the given |port| to the position |pos|.
 * The new position, measured in bytes, is obtained by adding |pos|
 * bytes to the position specified by |whence|. If passed, |whence|
 * must be one of |:start|, |:current| or |:end|. The resulting
 * position is relative to the start of the file, the current position
 * indicator, or end-of-file, respectively. If |whence| is omitted, it
 * defaults to |:start|.
 * @l
 * NOTE: After using port-seek, the value returned by
 * |port-current-line| may be incorrect.
doc>
 */
DEFINE_PRIMITIVE("port-seek", port_seek, subr23, (SCM port, SCM pos, SCM w))
{
  off_t n;
  long p = STk_integer_value(pos);
  int whence = -1;

  if (!PORTP(port))  STk_error_bad_port(port);
  if (p == LONG_MIN) STk_error_bad_io_param("bad offset ~S", pos);
  if (w) {
    if (KEYWORDP(w)) {
      const char *s = KEYWORD_PNAME(w);

      if (strcmp(s, "start") == 0) whence = SEEK_SET;
      else if (strcmp(s, "end") == 0) whence = SEEK_END;
      else if (strcmp(s, "current") == 0) whence = SEEK_CUR;
    }
  }
  else
    whence = SEEK_SET;

  if (whence < 0)
     STk_error_bad_io_param("bad keyword position ~S", w);

  /* ----------*/
  STk_flush(port);
  n = STk_seek(port, (off_t) p, whence);

  if (n < 0)
    general_io_error(io_malformed, "cannot seek position ~S", pos);

  return STk_long2integer((long) n);
}

/*
<doc EXT port-rewind
 * (port-rewind port)
 *
 * Sets the port position to the beginning of |port|. The value returned by
 * |port-rewind| is *_void_*.
doc>
 */
DEFINE_PRIMITIVE("port-rewind", port_rewind, subr1, (SCM port))
{
  if (!PORTP(port)) STk_error_bad_port(port);
  STk_rewind(port);
  return STk_void;
}

/*
<doc EXT port-close-hook-set!
 * (port-close-hook-set! port thunk)
 *
 * Associate the procedure |thunk| to |port|. The thunk will be called
 * the first time |port| is closed.
 * @lisp
 * (let* ((tmp (temporary-file-name))
 *        (p   (open-output-file tmp))
 *        (foo #t))
 *   (port-close-hook-set! p
 *                      (lambda()
 *                        (remove-file tmp)
 *                        (set! foo #t)))
 *   (close-port p)
 *   foo)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("port-close-hook-set!", port_close_hook_set, subr2,
                 (SCM port, SCM thunk))
{
  if (!PORTP(port)) STk_error_bad_port(port);
  if (!STk_procedurep(thunk)) STk_error("bad procedure ~S", thunk);

  PORT_CLOSEHOOK(port) = thunk;
  return STk_void;
}


/*
<doc EXT port-close-hook
 * (port-close-hook port)
 *
 * Returns the user close procedure associated to the given |port|.
doc>
*/
DEFINE_PRIMITIVE("port-close-hook", port_close_hook, subr1, (SCM port))
{
  if (!PORTP(port)) STk_error_bad_port(port);
  return PORT_CLOSEHOOK(port);
}


/*
 * Port case sensitivity accesoors
 */
DEFINE_PRIMITIVE("%port-case-sensitive", port_cs, subr1, (SCM port))
{
  if (!PORTP(port)) STk_error_bad_port(port);
  return MAKE_BOOLEAN(PORT_CASE_SENSITIVEP(port));
}


DEFINE_PRIMITIVE("%port-case-sensitive-set!", port_cs_set, subr2, (SCM port,SCM val))
{
  if (!PORTP(port)) STk_error_bad_port(port);

  if (val != STk_false)
    PORT_FLAGS(port) |= PORT_CASE_SENSITIVE;
  else
    PORT_FLAGS(port) &= ~PORT_CASE_SENSITIVE;
  return STk_void;
}


/*===========================================================================*\
 *
 * Initializations
 *
\*===========================================================================*/
static void initialize_io_conditions(void)
{
  SCM module = STk_STklos_module;

#define DEFCOND(x, name, parent, slots)                 \
  x = STk_defcond_type(name, parent, slots, module)

  DEFCOND(io_error, "&i/o-error", STk_err_mess_condition, STk_nil);

  DEFCOND(io_port_error, "&i/o-port-error", io_error, LIST1(STk_intern("port")));
  DEFCOND(io_read_error, "&i/o-read-error", io_port_error, STk_nil);
  DEFCOND(io_write_error, "&i/o-write-error", io_port_error, STk_nil);
  DEFCOND(io_closed_error, "&i/o-closed-error", io_port_error, STk_nil);

  DEFCOND(io_fn_error,"&i/o-filename-error",io_error,LIST1(STk_intern("filename")));
  DEFCOND(io_malformed, "&i/o-malformed-filename-error", io_fn_error, STk_nil);
  DEFCOND(io_prot_error, "&i/o-file-protection-error", io_fn_error, STk_nil);
  DEFCOND(io_ro_error, "&i/o-file-is-read-only-error", io_prot_error, STk_nil);
  DEFCOND(io_exists_error, "&i/o-file-already-exists-error", io_fn_error, STk_nil);
  DEFCOND(io_no_file_error, "&i/o-no-such-file-error", io_fn_error, STk_nil);
  DEFCOND(io_bad_param,"&i/o-bad-parameter",io_error,LIST1(STk_intern("parameter")));
}


static void print_port(SCM obj, SCM port, int _UNUSED(mode))
{
  PORT_PRINT(obj)(obj, port);
}


/* The stucture which describes the port type */
static struct extended_type_descr xtype_port = {
  .name  = "port",
  .print = print_port
};



/*===========================================================================*/

int STk_init_port(void)
{
  /* Define a constant for lines terminated by CR/LF to avoid multiple
   * allocations. Make it constant to avoid the user break it
   */
  CrLf                 = STk_Cstring2string("\r\n");
  BOXED_INFO(CrLf)    |= STRING_CONST;

  /* Define the port file */
  DEFINE_XTYPE(port, &xtype_port);

  /* Initialize  I/O Condition (aka SRFI-36) */
  initialize_io_conditions();

  /* and its associated primitives */
  ADD_PRIMITIVE(input_portp);
  ADD_PRIMITIVE(output_portp);
  ADD_PRIMITIVE(binary_portp);
  ADD_PRIMITIVE(textual_portp);
  ADD_PRIMITIVE(portp);
  ADD_PRIMITIVE(interactive_portp);

  STk_make_C_parameter2("current-input-port", STk_current_input_port,
                        STk_set_current_input_port, STk_STklos_module);
  STk_make_C_parameter2("current-output-port", STk_current_output_port,
                        STk_set_current_output_port, STk_STklos_module);
  STk_make_C_parameter2("current-error-port", STk_current_error_port,
                        STk_set_current_error_port, STk_STklos_module);

  ADD_PRIMITIVE(scheme_read);
  ADD_PRIMITIVE(scheme_read_ci);
  ADD_PRIMITIVE(scheme_read_cst);
  ADD_PRIMITIVE(read_char);
  ADD_PRIMITIVE(read_bytes);
  ADD_PRIMITIVE(d_read_bytes);
  ADD_PRIMITIVE(read_bytevector);
  ADD_PRIMITIVE(d_read_bytevector);
  ADD_PRIMITIVE(peek_char);
  ADD_PRIMITIVE(peek_byte);
  ADD_PRIMITIVE(read_byte);
  ADD_PRIMITIVE(eof_objectp);
  ADD_PRIMITIVE(eof_object);
  ADD_PRIMITIVE(char_readyp);
  ADD_PRIMITIVE(u8_readyp);

  ADD_PRIMITIVE(write);
  ADD_PRIMITIVE(display);
  ADD_PRIMITIVE(display_simple);
  ADD_PRIMITIVE(newline);
  ADD_PRIMITIVE(write_char);
  ADD_PRIMITIVE(write_string);
  ADD_PRIMITIVE(write_chars);
  ADD_PRIMITIVE(write_byte);

  ADD_PRIMITIVE(write_star);
  ADD_PRIMITIVE(format);
  ADD_PRIMITIVE(scheme_error);
  ADD_PRIMITIVE(scheme_signal_error);

  ADD_PRIMITIVE(close_input_port);
  ADD_PRIMITIVE(close_output_port);
  ADD_PRIMITIVE(textual_portp);
  ADD_PRIMITIVE(binary_portp);
  ADD_PRIMITIVE(close_port);
  ADD_PRIMITIVE(port_closed);
  ADD_PRIMITIVE(port_open);
  ADD_PRIMITIVE(copy_port);

  ADD_PRIMITIVE(read_line);
  ADD_PRIMITIVE(port_flush);
  ADD_PRIMITIVE(port_current_line);
  ADD_PRIMITIVE(port_position);
  ADD_PRIMITIVE(port_seek);
  ADD_PRIMITIVE(port_rewind);
  ADD_PRIMITIVE(port_close_hook);
  ADD_PRIMITIVE(port_close_hook_set);

  ADD_PRIMITIVE(port_cs);
  ADD_PRIMITIVE(port_cs_set);
  
  return STk_init_fport() &&
         STk_init_sport() &&
         STk_init_vport();
}
