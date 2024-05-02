/*
 * utf8.c               -- UTF-8 support functions
 *
 * Copyright © 2011-2023 Erick Gallesio <eg@stklos.net>
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
 *    Creation date: 30-Apr-2011 19:46 (eg)
 */

#include "stklos.h"

int STk_use_utf8 = -1;


static void error_bad_sequence(const char *str)
{
  int i;
  char *buffer = STk_must_malloc_atomic(strlen(str) + 1);

  for (i = 0; *str; i++, str++) {
    buffer[i] = ((' ' < *str) && (*str < 0x7f)) ? *str : '.';
  }
  buffer[i] = '\0';

  STk_error("bad UTF-8 sequence in '%s'", buffer);
}


char *STk_utf8_grab_char(char *str, utf8_char *c) /* result = pos. after current one */
{
  uint8_t *buff = (uint8_t *) str;

  if (!STk_use_utf8 || (buff[0] & 0x80) == 0) {
    *c = buff[0];
    return str + 1;
  }

  if ((buff[0] < 0xc0) || (buff[0] > 0xf7))
    return NULL;


  if (buff[0] < 0xe0) {
    *c = ((buff[0] & 0x3f) << 6) + (buff[1] & 0x3f);
    return str + 2;
  }

 if (buff[0] < 0xf0) {
    *c = ((buff[0] & 0x1f) << 12) +
         ((buff[1] & 0x3f) <<  6) +
          (buff[2] & 0x3f);
    return str + 3;
  }

  *c = ((buff[0] & 0x07) << 18) +
       ((buff[1] & 0x3f) << 12) +
       ((buff[2] & 0x3f) <<  6) +
        (buff[3] & 0x3f);
  return str + 4;
}

int STk_utf8_read_char(SCM port)
{
  int c = STk_getc(port);

  if (STk_use_utf8 && (c >= 0x80)) {
    /* Read a UTF-8 character */
    if ((c < 0xc0) || (c > 0xf7))
      return UTF8_INCORRECT_SEQUENCE;
    else if (c < 0xe0) {
      c  = (c & 0x3f) << 6;
      c += STk_getc(port) & 0x3F;
    } else if (c < 0xf0) {
      c  = (c & 0x1f) << 12;
      c += (STk_getc(port) & 0x3f) << 6;
      c += (STk_getc(port) & 0x3f);
    } else {
      c  = (c & 0x0F) << 16;
      c += (STk_getc(port) &0x3f) << 6;
      c += (STk_getc(port) &0x3f) << 6;
      c += (STk_getc(port) &0x3F);
    }
  }
  return c;
}


int STk_char2utf8(int ch, char *str) /* result = length of the UTF-8 repr. */
{
  uint8_t *buff = (uint8_t *)str;
  int n = 0;

  if (VALID_UTF8_VALUE(ch)) {
    if (ch < 0x80) {
      *buff++ = ch;
      n = 1;
    } else if (ch < 0x800) {
      *buff++ = (ch >> 6) | 0xc0;
      *buff++ = (ch & 0x3f) | 0x80;
      n = 2;
    } else if (ch < 0x10000) {
      *buff++ = (ch >> 12) | 0xe0;
      *buff++ = ((ch >> 6) & 0x3f) | 0x80;
      *buff++ = (ch & 0x3f) | 0x80;
      n = 3;
    } else if (ch < 0x110000) {
      *buff++ = (ch >> 18) | 0xf0;
      *buff++ = ((ch >> 12) & 0x3f) | 0x80;
      *buff++ = ((ch >> 6)  & 0x3f) | 0x80;
      *buff++ = (ch & 0x3f) | 0x80;
      n = 4;
    }
  }
  /* *buff = '\0'; */
  return n;
}

int STk_utf8_char_bytes_needed(unsigned int ch)
{
  /* # of bytes needed ro represent character ch */
  if (ch < 0x80)        return 1;
  if (ch < 0x800)       return 2;
  if (ch < 0x10000)     return 3;
  if (ch < 0x110000)    return 4;
  return 1; /* to avoid infinite loop, but obiously incorrect */
}

int STk_utf8_sequence_length(const char *str)
{
  /* return length of the UTF-8 sequence starting at given address */
  uint8_t c = *((uint8_t *) str);

  if (c < 0x80)                         return 1;
  if ((c < 0xc0) || (c > 0xf7))         return UTF8_INCORRECT_SEQUENCE;
  if (c < 0xe0)                         return 2;
  if (c < 0xf0)                         return 3;
  return 4;
}


int STk_utf8_strlen(const char *s, int max)
{
  int len;
  const char *start = s, *end = s + max;

  for (len = 0;  s < end; len++) {
    int sz =  STk_utf8_sequence_length(s);

    if (sz == UTF8_INCORRECT_SEQUENCE)
      error_bad_sequence(start);
    s += sz;
  }
  return len;
}

int STk_utf8_verify_sequence(char *s, int len)
/* Are the len bytes starting at s constitute a valid UTF8 sequence? */
{
  char *end = s + len;

  while (s < end) {
    int sz =  STk_utf8_sequence_length(s);

    if (sz == UTF8_INCORRECT_SEQUENCE) return 0;
    s += sz;
  }
  return (s == end);
}

char *STk_utf8_index(char *s, int i, int max) /* return the address of ith char of s*/
{
  char *start = s, *end = s + max;

  while ((s < end) && i--) {
    int sz =  STk_utf8_sequence_length(s);

    if (sz == UTF8_INCORRECT_SEQUENCE)
      error_bad_sequence(start);
    s += sz;
  }

  return s;
}

int STk_utf8_char_from_byte(char *s, int i, int max) /*  byte index => char index */
{
  char *start = s,
       *end   = s + max,
       *idx   = s + i;
  int pos = 0;

  while ((s != idx) && (s < end) ) {
    int sz =  STk_utf8_sequence_length(s);

    if (sz == UTF8_INCORRECT_SEQUENCE)
      error_bad_sequence(start);
    s += sz;
    pos += 1;
  }

  return (s == idx) ? pos : -1;
}

#ifdef STK_DEBUG
/* ======================================================================
 *      STklos Primitives (defined only if we are in debug mode)
 * ====================================================================== */
DEFINE_PRIMITIVE("%char-utf8-encoding", char_utf8_encoding, subr1, (SCM c))
{
  SCM lst = STk_nil;
  char buffer[5] = {0};
  int i;

  if (!CHARACTERP(c)) STk_error("bad char ~S", c);
  STk_char2utf8(CHARACTER_VAL(c), buffer);

  for (i = strlen((char*) buffer)-1; i >= 0; i--)
    lst = STk_cons(MAKE_INT(buffer[i]), lst);
  return lst;
}

DEFINE_PRIMITIVE("%dump-string", dump_string, subr12, (SCM str, SCM index))
{
  long i;
  utf8_char c = 0;

  STk_debug("String ~S. space=%d, size=%d, len =%d", str,
            STRING_SPACE(str), STRING_SIZE(str), STRING_LENGTH(str));

  printf("[");
  for (i=0; i < STRING_SIZE(str); i++)
    printf(" %02x", (uint8_t) STRING_CHARS(str)[i]);
  printf(" ]\n");

  if (index) {
    i = STk_integer_value(index);
    printf("------\nChar starting at index %ld\n", i);
    STk_debug("  length of char = %d",
              STk_utf8_sequence_length(&(STRING_CHARS(str)[i])));
    STk_utf8_grab_char(STRING_CHARS(str)+i, &c);
    STk_debug("   character is %d ~S", (unsigned) c, MAKE_CHARACTER(c));
  }

  return STk_void;
}

/* ======================================================================
 *      Initialization
 * ====================================================================== */
int STk_init_utf8(void)
{
  ADD_PRIMITIVE(char_utf8_encoding);
  ADD_PRIMITIVE(dump_string);
  return TRUE;
}

#endif
