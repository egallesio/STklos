/*
 * s i o . c					-- Low level I/O
 *
 * Copyright © 1993-2011 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *
 *	     Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: ????
 * Last file update:  1-May-2011 22:30 (eg)
 *
 *
 * Completely rewritten for the STklos version (Jan. 2000)
 *
 */

#include "stklos.h"

int
STk_readyp(SCM port)
{
  if (PORT_UNGETC(port) != EOF) return 1;
  return PORT_READY(port)(PORT_STREAM(port));
}

int
STk_getc(SCM port)
{
  int result = PORT_UNGETC(port);

  if (result != EOF) {
    /* There was an ungetted char */
    PORT_UNGETC(port) = EOF;
  } else {
    /* No ungetted char; read ahead */
    result = PORT_GETC(port)(PORT_STREAM(port));
  }
  if (result == '\n') PORT_LINE(port) +=1;
  PORT_POS(port) += 1;

  return result;
}


int 
STk_get_character(SCM port)  /* result may be a wide character */
{
  return (PORT_UNGETC(port) != EOF) ? 
              /* we have an ungetted char, call normal getc */
              STk_getc(port):
              /* try to read it as an UTF-8 sequence */
              STk_utf8_read_char(port);
}


int
STk_ungetc(int c, SCM port)
{
  int result = PORT_UNGETC(port);

  if (result != EOF) STk_error("INTERNAL ERROR: cannot unget character");
  PORT_UNGETC(port) = c;
  if (c == '\n') PORT_LINE(port) -= 1;
  PORT_POS(port) -=1;
  return c;
}

int
STk_close(SCM port)
{
  int res, exec_hook = FALSE;;

  if (! (PORT_FLAGS(port) & PORT_CLOSED)) {
    PORT_RELEASE(port)(port);
    exec_hook = TRUE;
  }

  STk_register_finalizer(port, NULL); /* Unregister (possible) finalizer */
  PORT_FLAGS(port) |= PORT_CLOSED;
  res = PORT_CLOSE(port)(PORT_STREAM(port));

  /* Eventually call the close hook */
  if (exec_hook && (PORT_CLOSEHOOK(port) != STk_false))
    STk_C_apply(PORT_CLOSEHOOK(port), 0);

  return res;
}

int
STk_putc(int c, SCM port)
{
  int n = PORT_PUTC(port)(c, PORT_STREAM(port));
  if (n >= 0)
    PORT_POS(port) += 1;
  return n;
}

int
STk_puts(char *s, SCM port)
{
  int n =  PORT_PUTS(port)(s, PORT_STREAM(port));
  if (n >= 0)
    PORT_POS(port) += n;
  return n;
}

int
STk_putstring(SCM s, SCM port)
{
  int n =  PORT_PUTSTRING(port)(s, PORT_STREAM(port));
  if (n >= 0)
    PORT_POS(port) += n;
  return n;
}


int
STk_nputs(SCM port, char *s, int len)
{
  int n = PORT_NPUTS(port)(PORT_STREAM(port), s, len);
  if (n >= 0)
     PORT_POS(port) += n;
  return n;
}


off_t
STk_seek(SCM port, off_t offset, int whence)
{
  /* Invalidate PORT_LINE (except if we rewind the file  (in this case it is 0) */
  PORT_LINE(port) =  ((offset == 0) && (whence == SEEK_SET)) ? 1: -1;

  if (whence == SEEK_CUR) {
    //if (PORT_UNGETC(port) != EOF) offset -= 1;
    /* Don't use relative access since fports stream does'nt know its cur. pos. */
    offset = PORT_POS(port) + offset;
    whence = SEEK_SET;
  }

  PORT_UNGETC(port) = EOF;
  return  PORT_POS(port) = PORT_SEEK(port)(PORT_STREAM(port), offset, whence);
}


void
STk_rewind(SCM port)
{
  PORT_FLUSH(port)(PORT_STREAM(port));
  STk_seek(port, (off_t) 0, SEEK_SET);
}


off_t
STk_tell(SCM port)
{
  return PORT_POS(port);
}


int
STk_flush(SCM port)
{
  return PORT_FLUSH(port)(PORT_STREAM(port));
}

int
STk_feof(SCM port)
{
  return PORT_EOFP(port)(PORT_STREAM(port));
}

int
STk_read_buffer(SCM port, void *buff, int count)
{
  int n = PORT_BREAD(port)(PORT_STREAM(port), buff, count);
  if (n >= 0) {
    PORT_POS(port) += n;
    PORT_LINE(port) = -1;
  }
  return n;
}

int
STk_write_buffer(SCM port, void *buff, int count)
{
  int n = PORT_BWRITE(port)(PORT_STREAM(port), buff, count);
  if (n >= 0)
    PORT_POS(port) += n;
  return n;
}

int
STk_fprintf(SCM port, char *format, ...)
{
  va_list ap;
  char buffer[PORT_MAX_PRINTF];

  va_start(ap, format);
  vsprintf(buffer, format, ap);
  return STk_puts(buffer, port);
}
