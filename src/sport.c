/*
 * s p o r t . c			-- String ports management
 *
 * Copyright © 1993-2007 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 * Last file update: 14-May-2007 12:02 (eg)
 *
 */

#include "stklos.h"

/*===========================================================================*\
 * 
 * 			Utilities
 * 
\*===========================================================================*/

static void error_bad_string(SCM s)
{
  STk_error("bad string ~S", s);
}

/*===========================================================================*\
 * 
 * Low level plugins
 * 
\*===========================================================================*/

#define START_ALLOC_SIZE  100	/* Initial size of an ouput string port */

struct sstream {
  char *ptr;
  char *base;
  char *end;
  int  bufsize;
  SCM  str;	/* keep a ref on original string to avoid GC problems */
};

#define PORT_BASE(x)	(((struct sstream *) (x))->base)
#define PORT_PTR(x)	(((struct sstream *) (x))->ptr)
#define PORT_END(x)	(((struct sstream *) (x))->end)
#define PORT_BUFSIZE(x) (((struct sstream *) (x))->bufsize)
#define PORT_STR(x)	(((struct sstream *) (x))->str)

static Inline int Sgetc(void *stream)
{
  return (PORT_PTR(stream) < PORT_END(stream)) ? 
    ((unsigned char) *PORT_PTR(stream)++) : EOF;
}


static Inline int Seof(void * stream)
{
  return (PORT_PTR(stream) >= PORT_END(stream));
}

static Inline int Sreadyp(void *stream)
{
  return !Seof(stream);
}

static Inline int Sclose(void * stream)
{
  return 0;
}

static Inline int Sread(void *stream, void *buffer, int count)
{
  int avail = PORT_END(stream) - PORT_PTR(stream);

  if (count > avail) count = avail;
  if (count) {
    memcpy(buffer, PORT_PTR(stream), count);
    PORT_PTR(stream) += count;
  }
  return count;
}

static Inline int Sputc(int c, void *stream)
{
  register unsigned int tmp;

  if (PORT_PTR(stream) >= PORT_END(stream)) {
    if (PORT_END(stream) == PORT_BASE(stream) + PORT_BUFSIZE(stream)) {
      /* No more room => allocate a new buffer */
      tmp	  = PORT_BUFSIZE(stream);
      tmp	 += tmp/2;
      PORT_BASE(stream)= STk_must_realloc(PORT_BASE(stream), tmp);
      PORT_PTR(stream) = PORT_BASE(stream) + PORT_BUFSIZE(stream);/*base can move*/
      PORT_BUFSIZE(stream) = tmp;
    }
    PORT_END(stream) = PORT_PTR(stream) + 1;
  }
  *PORT_PTR(stream)++ = (unsigned char) c;

  return c;
}


static Inline int Swrite(void *stream, void *buffer, int count)
{
  int tmp, pos;

  if (PORT_PTR(stream) + count >= PORT_END(stream)) {
    tmp = PORT_BUFSIZE(stream) + count + START_ALLOC_SIZE;
    pos = PORT_PTR(stream) - PORT_BASE(stream);
    PORT_BASE(stream) = STk_must_realloc(PORT_BASE(stream), tmp);
    PORT_PTR(stream)  = PORT_BASE(stream)+ pos; /* base can move */
    PORT_BUFSIZE(stream) = tmp;
    PORT_END(stream) = PORT_PTR(stream) + count;
  }
  memcpy(PORT_PTR(stream), buffer, count);
  PORT_PTR(stream) += count;

  return count;
}

static Inline int Sputs(char *s, void *stream)
{
  return Swrite(stream, s, strlen(s));
}

static Inline int Sputstring(SCM s, void *stream)
{
  return Swrite(stream, STRING_CHARS(s), STRING_SIZE(s));
}

static Inline int Snputs(void *stream, char *s, int len)
{
  return Swrite(stream, s, len);
}

static Inline int Sflush(void *stream)
{
  return 0;
}


static off_t Sseek(void *stream, off_t offset, int whence)
{
  char* p;

  switch (whence) {
  case SEEK_SET: 
    p = PORT_BASE(stream) + offset;
    break;
  case SEEK_CUR: 
    p = PORT_PTR(stream) + offset;
    break;
  default: /* SEEK_END */
    p = PORT_END(stream) + offset;
    break;
  }
  
  if ((PORT_BASE(stream) <= p) && (p <= PORT_END(stream))) {
    PORT_PTR(stream) = p;
    return p - PORT_BASE(stream);
  }
  else
    return -1;
}


static void sport_print(SCM obj, SCM port)   /* Generic printing of string ports */
{
  char buffer[MAX_PATH_LENGTH + 20];

  sprintf(buffer, "#[%s-string-port %lx%s]", 
	  ISPORTP(obj) ? "input" : "output",
	  (unsigned long) obj,
	  PORT_IS_CLOSEDP(obj) ? " (closed)" : "");
  STk_puts(buffer, port);
}

static void sport_release(SCM port)
{
  /* Nothing to do */
}


/*===========================================================================*\
 * 
 * 			Input ports
 * 
\*===========================================================================*/
enum kind_port {SREAD_C, SREAD, SWRITE};

static struct port_obj *
make_sport(enum kind_port kind,  SCM str, int init_len, int flags)
{
  struct sstream  *ss = STk_must_malloc(sizeof(struct sstream));
  SCM res;

  /* Initialize the stream part */
  switch (kind) {
    case SREAD:   /* this is a input string */
		  {
		    char *s = STRING_CHARS(str);
		    
		    PORT_BASE(ss) = s;
		    PORT_END(ss)  = s + init_len;
		    PORT_STR(ss)  = str;
		    break;
		  }
    case SREAD_C: /* this is a input string (from a C string) */
      		  PORT_BASE(ss) = (char *) str;
		  PORT_END(ss)  = (char *) str + init_len;
		  PORT_STR(ss)  = str;
		  break;
    case SWRITE:  /* This is an output string */
      		  PORT_BASE(ss) = PORT_END(ss) = STk_must_malloc_atomic(init_len);
		  PORT_STR(ss)  = STk_false;
		  break;
  }
  
  PORT_PTR(ss)     = PORT_BASE(ss);
  PORT_BUFSIZE(ss) = init_len;


  /* Initialize now the port itsef */
  NEWCELL(res, port);
  PORT_STREAM(res)	= ss;
  PORT_FLAGS(res)	= flags | PORT_IS_STRING;
  PORT_UNGETC(res) 	= EOF;
  PORT_LINE(res)	= 1;
  PORT_POS(res)		= 0;
  PORT_FNAME(res)	= NULL;
  PORT_CLOSEHOOK(res)	= STk_false;
  
  PORT_PRINT(res)	= sport_print;
  PORT_RELEASE(res)	= sport_release;
  PORT_GETC(res)	= Sgetc;
  PORT_READY(res)	= Sreadyp;
  PORT_EOFP(res)	= Seof;
  PORT_CLOSE(res)	= Sclose;
  PORT_PUTC(res)	= Sputc;
  PORT_PUTS(res)	= Sputs;
  PORT_PUTSTRING(res)	= Sputstring;
  PORT_NPUTS(res)	= Snputs;
  PORT_FLUSH(res)	= Sflush;
  PORT_BREAD(res)	= Sread;
  PORT_BWRITE(res)	= Swrite;
  PORT_SEEK(res)	= Sseek;
  
  return (struct port_obj *) res;
} 

/*
 * open-input-string with a C string ...
 */
SCM STk_open_C_string(char *str)
{
  return (SCM) make_sport(SREAD_C, (SCM) str, strlen(str), PORT_READ);
}


/*
<doc EXT open-input-string
 * (open-input-string str)
 *
 * Returns an input string port capable of delivering characters from
 * |str|.
doc>
 */
DEFINE_PRIMITIVE("open-input-string", open_input_string, subr1, (SCM s))
{
  if (!STRINGP(s)) error_bad_string(s);
  return (SCM) make_sport(SREAD, s, STRING_SIZE(s), PORT_READ);
}


/*
<doc EXT open-output-string
 * (open-output-string)
 *
 * Returns an output string port capable of receiving and collecting characters.
doc>
 */
DEFINE_PRIMITIVE("open-output-string", open_output_string, subr0,(void))
{
  return (SCM) make_sport(SWRITE, (SCM) NULL, START_ALLOC_SIZE, PORT_WRITE);
}


/*
<doc EXT get-output-string
 * (get-output-string port)
 *
 * Returns a string containing all the text that has been written on the
 * output string |port|.
 * @lisp
 *  (let ((p (open-output-string)))
 *     (display "Hello, world" p)
 *     (get-output-string p))         => "Hello, world"
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("get-output-string", get_output_string, subr1, (SCM port))
{
  struct port_obj* p;
  char *base;

  if (! OSPORTP(port)) STk_error_bad_port(port);

  p    = PORT_STREAM(port);
  base = PORT_BASE(p);
  return STk_makestring(PORT_END(p) - base, base);
}


/*
<doc EXT input-string-port? output-string-port?
 * (input-string-port? obj)
 * (output-string-port? obj)
 *
 * Returns |#t| if |obj| is an input string port or output string port 
 * respectively, otherwise returns #f.
doc>
 */
DEFINE_PRIMITIVE("input-string-port?", input_string_portp, subr1, (SCM port))
{
  return MAKE_BOOLEAN(ISPORTP(port));
}

DEFINE_PRIMITIVE("output-string-port?", output_string_portp, subr1, (SCM port))
{
  return MAKE_BOOLEAN(OSPORTP(port));
}


int STk_init_sport(void)
{
  ADD_PRIMITIVE(open_input_string);
  ADD_PRIMITIVE(open_output_string);
  ADD_PRIMITIVE(get_output_string);
  ADD_PRIMITIVE(input_string_portp);
  ADD_PRIMITIVE(output_string_portp);
  return TRUE;
}
