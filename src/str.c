/*
 *
 * s t r . c				-- Strings management
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
 *           Author: Erick Gallesio [eg@unice.fr]
 *    Creation date: ??????
 * Last file update: 10-Aug-2011 00:34 (eg)
 */

#include <ctype.h>
#include <wctype.h>
#include "stklos.h"

/* min size added to a string when reallocated in a string-set! */
#define UTF8_STRING_INCR	8

#define STRING_MONOBYTE(str)	(STRING_LENGTH(str) == STRING_SIZE(str))

/*
 * Utilities
 *
 */

static void error_bad_string(SCM s)
{
  STk_error("bad string ~s", s);
}

static void error_bad_character(SCM s)
{
  STk_error("bad character ~S", s);
}

static void error_change_const_string(SCM s)
{
  STk_error("changing the constant string ~s is not allowed", s);
}

static void error_index_out_of_bound(SCM str, SCM index)
{
  STk_error("index ~S out of bound in string ~S", index, str);
}

static void error_bad_sequence(SCM str)
{
  STk_error("bad UTF-8 sequence in ~S", str);
}


static int stringcomp(SCM s1, SCM s2)
{
  register char *str1, *str2;

  if (!STRINGP(s1)) error_bad_string(s1);
  if (!STRINGP(s2)) error_bad_string(s2);

  if (STk_use_utf8 && (!STRING_MONOBYTE(s1) || !STRING_MONOBYTE(s2))) {
    /* At least one string is multi-bytes */
    uint32_t ch1, ch2;
    char *end1, *end2;

    str1 = STRING_CHARS(s1); end1 = str1 + STRING_SIZE(s1);
    str2 = STRING_CHARS(s2); end2 = str2 + STRING_SIZE(s2);

    while ((str1 < end1) && (str2 < end2)) {
      if ((str1 = STk_utf8_grab_char(str1, &ch1)) == NULL) error_bad_sequence(s1);
      if ((str2 = STk_utf8_grab_char(str2, &ch2)) == NULL) error_bad_sequence(s2);

      if (ch1 != ch2) return ch1 - ch2;
    }

    /* str1 < end1 || str2 < end2 */
    return (str1 < end1) ? +1 : ((str2 < end2) ? -1 : 0);
  } else {
    /* fast-path for mono-byte strings */
    register int l1, l2;

    for (l1=STRING_SIZE(s1), str1=STRING_CHARS(s1),
	 l2=STRING_SIZE(s2),str2=STRING_CHARS(s2);
	 l1 && l2;
	 l1--, str1++, l2--, str2++)
      if (*str1 != *str2) return ((unsigned char) *str1 - (unsigned char) *str2);

    /* l1 == 0 || l2 == 0 */
    return l1 ? +1 : (l2 ? -1 : 0);
  }
}


static int stringcompi(SCM s1, SCM s2)
{
  register char *str1, *str2;

  if (!STRINGP(s1)) error_bad_string(s1);
  if (!STRINGP(s2)) error_bad_string(s2);

  if (STk_use_utf8 && (!STRING_MONOBYTE(s1) || !STRING_MONOBYTE(s2))) {
    /* At least one string is multi-bytes */
    uint32_t ch1, ch2;
    char *end1, *end2;

    str1 = STRING_CHARS(s1); end1 = str1 + STRING_SIZE(s1);
    str2 = STRING_CHARS(s2); end2 = str2 + STRING_SIZE(s2);

    while ((str1 < end1) && (str2 < end2)) {
      if ((str1 = STk_utf8_grab_char(str1, &ch1)) == NULL) error_bad_sequence(s1);
      if ((str2 = STk_utf8_grab_char(str2, &ch2)) == NULL) error_bad_sequence(s2);

      if (towlower(ch1) != towlower(ch2)) return towlower(ch1) - towlower(ch2);
    }

    /* str1 < end1 || str2 < end2 */
    return (str1 < end1) ? +1 : ((str2 < end2) ? -1 : 0);
  } else {
    /* fast-path for mono-byte strings */
    register int l1, l2;

    for (l1=STRING_SIZE(s1), str1=STRING_CHARS(s1),
	 l2=STRING_SIZE(s2),str2=STRING_CHARS(s2);
	 l1 && l2;
	 l1--, str1++, l2--, str2++)
      if (tolower(*str1) != tolower(*str2))
	return (tolower(*str1) - tolower(*str2));

    /* l1 == 0 || l2 == 0 */
    return l1 ? +1 : (l2 ? -1 : 0);
  }
}


static SCM control_index(int argc, SCM *argv, int *pstart, int *pend)
{
  SCM s = NULL;
  long len, start=0, end=-1;

  /* Controling number of arguments */
  switch (argc) {
  case 1: s = argv[0]; break;
  case 2: s = argv[0]; start = STk_integer_value(argv[-1]); break;
  case 3: s = argv[0]; start = STk_integer_value(argv[-1]);
	  end = STk_integer_value(argv[-2]); break;
  default: STk_error("incorrect number of argument (%d)", argc);
  }

  /* Controlling s */
  if (!STRINGP(s)) error_bad_string(s);
  len = STRING_LENGTH(s);

  /* Controling start index */
  if (start == LONG_MIN || start < 0 || start > len)
    /* argc cannot be 1 (start would be 0) */
    STk_error("bad starting index ~S", argv[(argc==2) ? 0 : -1]);

  /* controling end index */
  if (end == -1)
    end = len;
  else
    if (end == LONG_MIN  || end < 0 || end > len)
      /* We have an end index ==> argc = 3 */
      STk_error("bad ending index ~S", argv[0]);

  if (start > end)
    STk_error("low index is greater than high index");

  /* everything is correct, return values */
  *pstart = start;
  *pend   = end;
  return s;
}

static uint32_t *string2int(char *s, int len, int *utf8_len, wint_t(*func)(wint_t))
{
  uint32_t ch, *tmp, *buff = STk_must_malloc_atomic(len * sizeof(uint32_t));
  int space = 0;

  for (tmp = buff; len--; tmp++) {
    s      = STk_utf8_grab_char(s, &ch);
    ch     = func(ch);
    space += STk_utf8_char_bytes_needed(ch);
    *tmp   = ch;
  }

  *utf8_len = space;
  return buff;
}

static SCM make_string_from_int_array(uint32_t *buff, int len, int utf8_len)
{
  SCM z;
  char *s, *end;

  NEWCELL(z, string);
  STRING_CHARS(z)  = s = STk_must_malloc_atomic(utf8_len + 1);
  STRING_SPACE(z)  = STRING_SIZE(z) = utf8_len;
  STRING_LENGTH(z) = len;

  end = s + utf8_len;
  while (s < end) {
    s += STk_char2utf8(*buff++, s);
  }
  *s = '\0';

 return z;
}


static void copy_array(uint32_t *buff, int len, char* from)
{
  while (len--)
    from += STk_char2utf8(*buff++, from);
}


SCM STk_makestring(int len, char *init)
{
  register SCM z;

  NEWCELL(z, string);
  STRING_CHARS(z) = STk_must_malloc_atomic(len + 1);
  STRING_SPACE(z) = STRING_SIZE(z) = STRING_LENGTH(z) = len;

  if (init) {
    memcpy(STRING_CHARS(z), init, (size_t) len);
    STRING_CHARS(z)[len] = '\0'; /* so that STRING_CHARS is compatible with C */

    if (STk_use_utf8)
      /* Eventually correct the length to be in charcaters instead of bytes */
      STRING_LENGTH(z) = STk_utf8_strlen(STRING_CHARS(z), len);
  }
  else
    bzero(STRING_CHARS(z), len+1);

  return z;
}


SCM STk_Cstring2string(char *str) /* Embed a C string in Scheme world  */
{
  SCM  z;
  size_t len = strlen(str);

  NEWCELL(z, string);
  STRING_CHARS(z) = STk_must_malloc_atomic(len + 1);
  STRING_SPACE(z) = STRING_SIZE(z) = STRING_LENGTH(z) = len;
  strcpy(STRING_CHARS(z), str);

  return z;
}


SCM STk_chars2string(char *str, size_t len)
{
  SCM  z;

  NEWCELL(z, string);
  STRING_CHARS(z) = STk_must_malloc_atomic(len + 1);
  STRING_SPACE(z) = STRING_SIZE(z) = STRING_LENGTH(z) = len;
  memcpy(STRING_CHARS(z), str, len);
  STRING_CHARS(z)[len] = '\0'; /* so that STRING_CHARS is compatible with C */
  return z;
}


DEFINE_PRIMITIVE("string?", stringp, subr1, (SCM obj))
/*
<doc string?
 * (string? obj)
 *
 * Returns |#t| if |obj| is a string, otherwise returns |#f|.
doc>
*/
{
  return MAKE_BOOLEAN(STRINGP(obj));
}

/*
<doc  make-string
 * (make-string k)
 * (make-string k char)
 *
 * |Make-string| returns a newly allocated string of length |k|. If |char| is
 * given, then all elements of the string are initialized to |char|, otherwise
 * the contents of the string are unspecified.
doc>
 */
DEFINE_PRIMITIVE("make-string", make_string, subr12, (SCM len, SCM init_char))
{
  long k = STk_integer_value(len);

  if (k < 0) STk_error("bad string length: ~S", len);

  if (init_char) {
    SCM z = STk_void;

    if (! CHARACTERP(init_char))
      STk_error("initializing char ~S is not valid", init_char);
    else {
      char *s, buff[5] = {0};
      int c = CHARACTER_VAL(init_char);

      if (STk_use_utf8 && c >= 0x80) {
	/* unicode character */
	int n = STk_char2utf8(c, buff);

	z = STk_makestring(k * n, NULL);
	s = STRING_CHARS(z);
	STRING_LENGTH(z) = k; /* incorrectly set to  k*n before */

	while (k--) {
	  *s++ = buff[0];
	  if (!buff[1]) continue;
	  *s++ = buff[1];
	  if (!buff[2]) continue;
	  *s++ = buff[2];
	  if (!buff[3]) continue;
	  *s++ = buff[3];
	}
      } else {
	/* non unicode character */
	z = STk_makestring(k, NULL);
	s = STRING_CHARS(z);

	while (k--) *s++ = c;
      }
    }
    return z;
  }
  else
    /* No initialization character */
    return STk_makestring(k, NULL);
}

/*
<doc  string
 * (string char ...)
 *
 * Returns a newly allocated string composed of the arguments.
doc>
 */
DEFINE_PRIMITIVE("string", string, vsubr, (int argc, SCM* argv))
{
  SCM z;
  char *s;
  int i, space;
  char buff[5];

  for (space = i = 0; i < argc; i++) {
    if (!CHARACTERP(argv[-i])) error_bad_character(argv[-i]);
    /* compute the size that must be allocated */
    if (STk_use_utf8)
      space += STk_char2utf8(CHARACTER_VAL(argv[-i]), buff);
    else
      if (CHARACTER_VAL(argv[-i]) > 255)
	STk_error("character ~S is too big", argv[-i]);
      else
	space += 1;
  }

  z = STk_makestring(space, NULL);
  STRING_LENGTH(z) = argc;	/* correct the length */

  /* copy element in newly allocated string */
  for (s=STRING_CHARS(z); argc--; argv--) {
    if (STk_use_utf8) {
      int n = STk_char2utf8(CHARACTER_VAL(*argv), buff);
      memcpy(s, buff, n);
      s += n;
    } else
      *s++ = CHARACTER_VAL(*argv);
  }
  *s = '\0';

  return z;
}


/*
<doc  string-length
 * (string-length string)
 *
 * Returns the number of characters in the given |string|.
doc>
 */
DEFINE_PRIMITIVE("string-length", string_length, subr1, (SCM str))
{
  if (!STRINGP(str)) error_bad_string(str);
  return MAKE_INT(STRING_LENGTH(str));
}


/*
<doc  string-ref
 * (string-ref string k)
 *
 * |String-ref| returns character k of string using zero-origin indexing
 * (|k| must be a valid index of string).
doc>
 */
DEFINE_PRIMITIVE("string-ref", string_ref, subr2, (SCM str, SCM index))
{
  long k = STk_integer_value(index);

  if (!STRINGP(str))			error_bad_string(str);
  if (k < 0 || k >= STRING_LENGTH(str)) error_index_out_of_bound(str, index);

  if (!STk_use_utf8 || (STRING_SIZE(str) == STRING_LENGTH(str)))
    /* string doesn't contain multibytes chars */
    return MAKE_CHARACTER(STRING_CHARS(str)[k]);
  else {
    /* We have multibytes chars */
    uint32_t c;
    char *s = STRING_CHARS(str);

    do
      s = STk_utf8_grab_char(s, &c);
    while (k--);
    return  MAKE_CHARACTER(c);
  }
}


/*
<doc  string-set!
 * (string-set! string k char)
 *
 * |String-set!| stores |char| in element |k| of |string| and returns
 * ,(emph "void") (|k| must be a valid index of |string|).
 *
 * @lisp
 * (define (f) (make-string 3 #\*))
 * (define (g) "***")
 * (string-set! (f) 0 #\?)  =>  void
 * (string-set! (g) 0 #\?)  =>  error
 * (string-set! (symbol->string 'immutable) 0 #\?)
 *                          =>  error
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("string-set!", string_set, subr3, (SCM str, SCM index, SCM value))
{
  long k = STk_integer_value(index);
  int cval;

  if (!STRINGP(str))    	        error_bad_string(str);
  if (BOXED_INFO(str) & STRING_CONST)   error_change_const_string(str);
  if (!CHARACTERP(value))	        error_bad_character(value);
  if (k < 0 || k >= STRING_LENGTH(str)) error_index_out_of_bound(str, index);


  cval = CHARACTER_VAL(value);
  if (!STk_use_utf8 || (STRING_MONOBYTE(str) &&
			(STk_utf8_char_bytes_needed(cval) == 1)))
    /* string doesn't contain multibytes chars and value is mono byte */
    STRING_CHARS(str)[k] = cval;
  else {
    /* Multi bytes string. The following code could be better factorized */
    char buffer[5];
    char *start     = STRING_CHARS(str);
    char *pos       = STk_utf8_index(start, k, STRING_SIZE(str));
    int new_char_sz = STk_char2utf8(cval, buffer);
    int old_char_sz = STk_utf8_sequence_length(pos);

    if (old_char_sz < new_char_sz) {
      /* new character has a longer representation than the old one */
      if (STRING_LENGTH(str) + new_char_sz - old_char_sz >= STRING_SPACE(str)) {
	/* not enough space; allocate some more bytes */
	char *new = STk_must_malloc_atomic(STRING_SPACE(str) + UTF8_STRING_INCR + 1);

	memcpy(new, start, pos - start);
	memcpy(new + (pos - start), buffer, new_char_sz);
	memcpy(new + (pos - start) + new_char_sz,
	       pos + old_char_sz,
	       start + STRING_SIZE(str) - pos + old_char_sz);
	STRING_CHARS(str)  = new;
	STRING_SPACE(str) += UTF8_STRING_INCR;
      } else {
	memmove(pos + new_char_sz,
		pos + old_char_sz,
		start + STRING_SIZE(str) - pos + old_char_sz);
	memcpy(pos, buffer, new_char_sz);
      }
      STRING_SIZE(str) += new_char_sz - old_char_sz;
    }
    else if (old_char_sz > new_char_sz) {
      /* old character has a longer representation than the old one */
      memmove(pos + new_char_sz,
	      pos + old_char_sz,
	      start + STRING_SIZE(str) - pos + old_char_sz);
      memcpy(pos, buffer, new_char_sz);
      STRING_SIZE(str) += new_char_sz - old_char_sz;
    }
    else {
      /* characters have the same number of bytes Replace character */
      memcpy(pos, buffer, new_char_sz);
    }
    /* ensure that last character is a 0 for C compatibility */
    STRING_CHARS(str)[STRING_SIZE(str)] = '\0';
  }
  return STk_void;
}

/*
<doc string=? string-ci=?
 * (string=? string1 string2)
 * (string-ci=? string1 string2)
 *
 * Returns |#t| if the two strings are the same length and contain the same
 * characters in the same positions, otherwise returns |#f|. |String-ci=?|
 * treats upper and lower case letters as though they were the same character,
 * but |string=?| treats upper and lower case as distinct characters.
doc>
 */

/*
<doc  string<? string<=? string>? string>=? string-ci<? string-ci<=? string-ci>? string-ci>=?
 * (string<? string1 string2)
 * (string>? string1 string2)
 * (string<=? string1 string2)
 * (string>=? string1 string2)
 * (string-ci<? string1 string2)
 * (string-ci>? string1 string2)
 * (string-ci<=? string1 string2)
 * (string-ci>=? string1 string2)
 *
 * These procedures are the lexicographic extensions to strings of the
 * corresponding orderings on characters. For example, |string<?| is the
 * lexicographic ordering on strings induced by the ordering |char<?| on
 * characters. If two strings differ in length but are the same up to the
 * length of the shorter string, the shorter string is considered to be
 * lexicographically less than the longer string.
 *
doc>
 */

DEFINE_PRIMITIVE("string=?", streq, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcomp(s1, s2) == 0); }

DEFINE_PRIMITIVE("string<?", strlt, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcomp(s1, s2) < 0); }

DEFINE_PRIMITIVE("string>?", strgt, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcomp(s1, s2) > 0); }

DEFINE_PRIMITIVE("string<=?", strle, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcomp(s1, s2) <= 0); }

DEFINE_PRIMITIVE("string>=?", strge, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcomp(s1, s2) >= 0); }



DEFINE_PRIMITIVE("string-ci=?", streqi, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcompi(s1, s2) == 0); }

DEFINE_PRIMITIVE("string-ci<?", strlti, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcompi(s1, s2) < 0); }

DEFINE_PRIMITIVE("string-ci>?", strgti, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcompi(s1, s2) > 0); }

DEFINE_PRIMITIVE("string-ci<=?", strlei, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcompi(s1, s2) <= 0); }

DEFINE_PRIMITIVE("string-ci>=?", strgei, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcompi(s1, s2) >= 0); }



/*
<doc  substring
 * (substring string start end)
 *
 * |String| must be a string, and |start| and |end| must be exact integers
 * satisfying
 * @lisp
 * 0 <= start <= end <= (string-length string).
 * @end lisp
 * |Substring| returns a newly allocated string formed from the characters
 * of |string| beginning with index |start| (inclusive) and ending with
 * index |end| (exclusive).
doc>
 */

DEFINE_PRIMITIVE("substring", substring, subr3, (SCM string, SCM start, SCM end))
{
  long from, to;

  if (!STRINGP(string)) error_bad_string(string);

  from = STk_integer_value(start);
  to   = STk_integer_value(end);

  if (from == LONG_MIN) STk_error("bad lower index ~S", start);
  if (to   == LONG_MIN) STk_error("bad upper index ~S", end);

  if (0 <= from && from <= to && to <= STRING_SIZE(string)) {
    if (STRING_MONOBYTE(string))
      return STk_makestring(to - from, STRING_CHARS(string)+from);
    else {
      /* multi-bytes string */
      uint32_t c;
      char *pfrom, *pto;
      SCM z;

      pto = pfrom = STk_utf8_index(STRING_CHARS(string), from, STRING_SIZE(string));

      for ( ; from < to; from++)
	pto = STk_utf8_grab_char(pto, &c);

      z = STk_makestring(pto - pfrom, pfrom);
      STRING_LENGTH(z) = STk_utf8_strlen(STRING_CHARS(z), pto-pfrom);
      return z;
    }
  }
  STk_error("index ~S or ~S incorrect", start, end);
  return STk_void; /* cannot occur */
}

/*
<doc  string-append
 * (string-append string ...)
 *
 * Returns a newly allocated string whose characters form the concatenation
 * of the given strings.
doc>
 */
DEFINE_PRIMITIVE("string-append", string_append, vsubr, (int argc, SCM* argv))
{
  int i, total=0;
  SCM z;
  char *p;

  /* Compute total length of resulting string */
  for (i = 0; i < argc; i++) {
    p = argv[-i];
    if (!STRINGP(p)) error_bad_string(p);
    total += STRING_SIZE(p);
  }

  /* Allocate result */
  z = STk_makestring(total, NULL);
  p = STRING_CHARS(z);

  /* copy strings */
  for (total=0, i=0; i < argc; i++) {
    memcpy(p, STRING_CHARS(*argv), (unsigned int) STRING_SIZE(*argv));
    p     += STRING_SIZE(*argv);
    total += STRING_LENGTH(*argv);
    argv  -=1;
  }
  STRING_LENGTH(z) = total;

  return z;
}


/*
<doc  string->list list->string
 * (string->list string)
 * (list->string list)
 *
 * |String->list| returns a newly allocated list of the characters that make
 * up the given string. |List->string| returns a newly allocated string
 * formed from the characters in the list |list|, which must be a list of
 * characters. |String->list| and |list->string| are inverses so far as
 * |equal?| is concerned.
doc>
 */
DEFINE_PRIMITIVE("string->list", string2list, subr1, (SCM str))
{
  register char *s;
  int len;
  uint32_t c;
  SCM tmp, tmp1, z;

  if (!STRINGP(str)) error_bad_string(str);

  len = STRING_LENGTH(str);
  s   = STRING_CHARS(str);

  tmp = z = STk_nil;

  while (len--) {
    s = STk_utf8_grab_char(s, &c);
    tmp1 = STk_cons(MAKE_CHARACTER(c), STk_nil);
    if (z == STk_nil)
      tmp = z = tmp1;
    else
      tmp = CDR(tmp) = tmp1;
  }
  return z;
}

DEFINE_PRIMITIVE("list->string", list2string, subr1, (SCM l))
{
  int bytes = 0, len = STk_int_length(l);
  register char *s;
  SCM z, tmp;

  if (len < 0) STk_error("bad list ~S", l);

  /* compute the number of bytes needed */
  for (tmp=l; !NULLP(tmp); tmp=CDR(tmp)) {
    if (!CHARACTERP(CAR(tmp))) error_bad_character(CAR(tmp));
    bytes += STk_utf8_char_bytes_needed(CHARACTER_VAL(CAR(tmp)));
  }

  z = STk_makestring(bytes, NULL);
  s = STRING_CHARS(z);

  /* copy the characters in the newly allocated string */
  for ( ; !NULLP(l); l=CDR(l)) {
    s += STk_char2utf8(CHARACTER_VAL(CAR(l)), s);
  }
  *s = '\0';
  return z;
}


/*
<doc string-copy
 * (string-copy string)
 *
 * Returns a newly allocated copy of the given |string|.
doc>
*/
DEFINE_PRIMITIVE("string-copy", string_copy, subr1, (SCM str))
{
  if (!STRINGP(str)) error_bad_string(str);
  return STk_makestring(STRING_SIZE(str), STRING_CHARS(str));
}


/*
<doc EXT string-fill!
 * (string-fill! string char)
 *
 * Stores |char| in every element of the given |string| and returns ,(emph "void").
doc>
*/
DEFINE_PRIMITIVE("string-fill!", string_fill, subr2, (SCM str, SCM c))
{
  int bytes, len, c_char, c_len;
  char *s;

  if (!STRINGP(str))  		      error_bad_string(str);
  if (!CHARACTERP(c)) 		      error_bad_character(c);
  if (BOXED_INFO(str) & STRING_CONST) error_change_const_string(str);

  len    = STRING_LENGTH(str);
  s      = STRING_CHARS(str);
  c_char = CHARACTER_VAL(c);
  c_len  = STk_utf8_char_bytes_needed(c_char);
  bytes  = len * c_len;

  if (c_len == 1) {	/* unibyte character */
    while (len--) {
      *s++ = c_char;
    }
  } else {		/* multibyte character */
    char buffer[5];

    if (bytes > STRING_SPACE(str)) {
      STRING_CHARS(str) = s = STk_must_malloc_atomic(bytes + 1);
      STRING_SPACE(str) = bytes;
    }

    STk_char2utf8(c_char, buffer);
    while (len--) {
      memcpy(s, buffer, c_len);
      s += c_len;
    }
    *s = '\0';
  }

  STRING_SIZE(str) = bytes;
  return STk_void;
}


/*
 *
 * STk bonus
 *
 */

static int Memmem(char *s1, int l1, char *s2, int l2, int use_utf8)
{
  int pos;

  if (l2 == 0) return 0;

  for (pos=0; l1 >= l2 ; pos++, l1--) {
    if (memcmp(s1, s2, (unsigned int) l2) == 0) return pos;

    /* go to next character */
    if (use_utf8) {
      int len = STk_utf8_sequence_length(s1);

      if (len == UTF8_INCORRECT_SEQUENCE) STk_error("bad UTF-8 sequence");
      s1 += len;
    } else {
      s1++;
    }
  }
  return -1; /* not found */
}

/*
<doc EXT string-find?
 * (string-find? str1 str2)
 *
 * Returns |#t| if |str1| appears somewhere in |str2|; otherwise returns |#f|.
doc>
*/
DEFINE_PRIMITIVE("string-find?", string_find, subr2, (SCM s1, SCM s2))
{
  int pos;

  if (!STRINGP(s1)) error_bad_string(s1);
  if (!STRINGP(s2)) error_bad_string(s2);

  pos = Memmem(STRING_CHARS(s2), STRING_SIZE(s2),
	       STRING_CHARS(s1), STRING_SIZE(s1),
	       FALSE);

  return MAKE_BOOLEAN(pos != -1);
}

/*
<doc EXT string-index
 * (string-index str1 str2)
 *
 * Returns the (first) index where |str1| is a substring of |str2| if it exists;
 * otherwise returns |#f|.
 * @lisp
 * (string-index "ca" "abracadabra") =>  4
 * (string-index "ba" "abracadabra") =>  #f
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("string-index", string_index, subr2, (SCM s1, SCM s2))
{
  int pos;

  if (!STRINGP(s1)) error_bad_string(s1);
  if (!STRINGP(s2)) error_bad_string(s2);

  pos = Memmem(STRING_CHARS(s2), STRING_SIZE(s2),
	       STRING_CHARS(s1), STRING_SIZE(s1),
	       STk_use_utf8 && !STRING_MONOBYTE(s2));

  return (pos != -1) ? STk_long2integer(pos) : STk_false;
}


/*
<doc EXT string-split
 * (string-split str)
 * (string-split str delimiters)
 *
 * parses |string| and returns a list of tokens ended by a character of the
 * |delimiters| string. If |delimiters| is omitted, it defaults to a string
 * containing a space, a tabulation and a newline characters.
 * @lisp
 * (string-split "/usr/local/bin" "/")
 *                        => ("usr" "local" "bin")
 * (string-split "once   upon a time")
 *                        => ("once" "upon" "a" "time")
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("string-split", string_split, subr12, (SCM string, SCM delimiters))
{
  SCM result = STk_nil;
  char *c_string, *c_delimiters, *s;
  int i, l_string, l_delimiters;

  if (!STRINGP(string)) error_bad_string(string);
  c_string = STRING_CHARS(string);
  l_string = STRING_SIZE(string);

  if (!delimiters) {
    c_delimiters = " \t\n";
    l_delimiters = 3;
  } else {
    if (!STRINGP(delimiters)) error_bad_string(delimiters);
    c_delimiters = STRING_CHARS(delimiters);
    l_delimiters = STRING_SIZE(delimiters);
  }

  for (s=c_string, i=0; i < l_string; s++, i++) {
    if (memchr(c_delimiters, *s, l_delimiters)) {
      if (s > c_string)
	result = STk_cons(STk_makestring(s-c_string, c_string),
			  result);
      c_string = s + 1;
    }
  }
  if (s > c_string)
    result = STk_cons(STk_makestring(s-c_string, c_string),
		      result);

  return STk_dreverse(result);
}

/*
<doc EXT string-mutable?
 * (string-mutable? obj)
 *
 * Returns |#t| if |obj| is a mutable string, otherwise returns |#f|.
 * @lisp
 * (string-mutable? "abc")                => #f
 * (string-mutable? (string-copy "abc"))  => #t
 * (string-mutable? (string #\a #\b #\c)) => #t
 * (string-mutable? 12)                   => #f
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("string-mutable?", string_mutable, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(STRINGP(obj) && !(BOXED_INFO(obj) & STRING_CONST));
}


/*
<doc EXT string-downcase
 * (string-downcase str)
 * (string-downcase str start)
 * (string-downcase str start end)
 *
 * Returns a string in which the upper case letters of string |str| between the
 * |start| and |end| indices have been replaced by their lower case equivalent.
 * If |start| is omited, it defaults to 0. If |end| is omited, it defaults to
 * the length of |str|.
 * @lisp
 * (string-downcase "Foo BAR")        => "foo bar"
 * (string-downcase "Foo BAR" 4)      => "bar"
 * (string-downcase "Foo BAR" 4 6)    => "ba"
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("string-downcase", string_downcase, vsubr, (int argc, SCM *argv))
{
  SCM s;
  int start, end;

  s = control_index(argc, argv, &start, &end);

  if (STk_use_utf8 && !STRING_MONOBYTE(s)) {
    uint32_t *wchars;
    int len;
    char *startp = STk_utf8_index(STRING_CHARS(s), start, STRING_SIZE(s));

    /* collect all characters in an allocated array of int and convert it */
    wchars = string2int(startp, end-start, &len, towlower);

    return make_string_from_int_array(wchars, end-start, len);
  } else {
    char *endp, *p, *q;
    SCM  z =  STk_makestring(end-start, NULL);

    endp = STRING_CHARS(s) + end;
    for (p=STRING_CHARS(s)+start, q=STRING_CHARS(z); p < endp; p++, q++)
      *q = tolower(*p);

    return z;
  }
}

/*
<doc EXT string-downcase!
 * (string-downcase! str)
 * (string-downcase! str start)
 * (string-downcase! str start end)
 *
 * This is the in-place side-effecting variant of |string-downcase|.
 * @lisp
 * (string-downcase! (string-copy "Foo BAR") 4)    => "Foo bar"
 * (string-downcase! (string-copy "Foo BAR") 4 6)  => "Foo baR"
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("string-downcase!", string_ddowncase, vsubr, (int argc, SCM *argv))
{
  SCM s;
  int i, start, end;

  s    = control_index(argc, argv, &start, &end);
  if (BOXED_INFO(s) & STRING_CONST) error_change_const_string(s);

  if (STk_use_utf8 && !STRING_MONOBYTE(s)) {	    /* multibyte string */
    uint32_t *wchars;
    int len;
    char *startp = STk_utf8_index(STRING_CHARS(s), start, STRING_SIZE(s));
    char *endp   = STk_utf8_index(STRING_CHARS(s), end, STRING_SIZE(s));

    /* collect all characters in an allocated array of int and convert it */
    wchars = string2int(startp, end-start, &len, towlower);
    if (len == endp-startp) {
      copy_array(wchars, end-start, startp);
    }
    else {
      /* This code is inefficient, but it seems that that the converted case
	 character always use the same length encoding. It is likely that this
	 code is never used in practice
      */
      for (i= start; i < end; i++)
	STk_string_set(s, MAKE_INT(i), MAKE_CHARACTER(*wchars++));
    }
  } else {				    /* monobyte string */
    char *p , *endp = STRING_CHARS(s) + end;

    for (p=STRING_CHARS(s)+start; p < endp; p++) *p = tolower(*p);
  }

  return STk_void;
}


/*
<doc EXT string-upcase
 * (string-upcase str)
 * (string-upcase str start)
 * (string-upcase str start end)
 *
 * Returns a string in which the upper case letters of string |str| between the
 * |start| and |end| indices have been replaced by their upper case equivalent.
 * If |start| is omited, it defaults to 0. If |end| is omited, it defaults to
 * the length of |str|.
doc>
 */
DEFINE_PRIMITIVE("string-upcase", string_upcase, vsubr, (int argc, SCM *argv))
{
  SCM s;
  int start, end;

  s = control_index(argc, argv, &start, &end);

  if (STk_use_utf8 && !STRING_MONOBYTE(s)) {
    uint32_t *wchars;
    int len;
    char *startp = STk_utf8_index(STRING_CHARS(s), start, STRING_SIZE(s));

    /* collect all characters in an allocated array of int and convert it */
    wchars = string2int(startp, end-start, &len, towupper);

    return make_string_from_int_array(wchars, end-start, len);
  } else {
    char *endp, *p, *q;
    SCM  z =  STk_makestring(end-start, NULL);

    endp = STRING_CHARS(s) + end;
    for (p=STRING_CHARS(s)+start, q=STRING_CHARS(z); p < endp; p++, q++)
      *q = toupper(*p);

    return z;
  }
}


/*
<doc EXT string-upcase!
 * (string-upcase! str)
 * (string-upcase! str start)
 * (string-upcase! str start end)
 *
 * This is the in-place side-effecting variant of |string-upcase|.
doc>
*/
DEFINE_PRIMITIVE("string-upcase!", string_dupcase, vsubr, (int argc, SCM *argv))
{
  SCM s;
  int i, start, end;

  s    = control_index(argc, argv, &start, &end);
  if (BOXED_INFO(s) & STRING_CONST) error_change_const_string(s);

  if (STk_use_utf8 && !STRING_MONOBYTE(s)) {	    /* multibyte string */
    uint32_t *wchars;
    int len;
    char *startp = STk_utf8_index(STRING_CHARS(s), start, STRING_SIZE(s));
    char *endp   = STk_utf8_index(STRING_CHARS(s), end, STRING_SIZE(s));

    /* collect all characters in an allocated array of int and convert it */
    wchars = string2int(startp, end-start, &len, towupper);
    if (len == endp-startp) {
      copy_array(wchars, end-start, startp);
    }
    else {
      /* This code is inefficient, but it seems that that the converted case
	 character always use the same length encoding. It is likely that this
	 code is never used in practice
      */
      for (i= start; i < end; i++)
	STk_string_set(s, MAKE_INT(i), MAKE_CHARACTER(*wchars++));
    }
  } else {				    /* monobyte string */
    char *p , *endp = STRING_CHARS(s) + end;

    for (p=STRING_CHARS(s)+start; p < endp; p++) *p = toupper(*p);
  }

  return STk_void;
}

/*
<doc EXT string-titlecase
 * (string-titlecase str)
 * (string-titlecase str start)
 * (string-titlecase str start end)
 *
 * This function returns a string.  For every character |c| in the
 * selected range of |str|, if |c| is preceded by a cased character, it
 * is downcased; otherwise it is titlecased. If |start| is omited, it
 * defaults to 0. If |end| is omited, it defaults to the length of |str|.
 * Note that if a |start| index is specified, then the character preceding
 * |s[start]| has no effect on the titlecase decision for character |s[start]|.
 * @lisp
 * (string-titlecase "--capitalize tHIS sentence.")
 *          =>  "--Capitalize This Sentence."
 * (string-titlecase "see Spot run. see Nix run.")
 *          =>  "See Spot Run. See Nix Run."
 * (string-titlecase "3com makes routers.")
 *          =>  "3Com Makes Routers."
 * (string-titlecase "greasy fried chicken" 2)
 *          => "Easy Fried Chicken"
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("string-titlecase", string_titlecase, vsubr, (int argc, SCM *argv))
{
  SCM s, z;
  int start, end;
  char *endp, *p, *q;
  char prev_is_sep = 1, curr_is_sep;

  s    = control_index(argc, argv, &start, &end);
  endp = STRING_CHARS(s) + end;
  z    = STk_makestring(end-start, NULL);

  for (p=STRING_CHARS(s)+start, q=STRING_CHARS(z); p < endp; p++, q++) {
    curr_is_sep = !(isalpha(*p));
    if (curr_is_sep)
      *q = *p;
    else
      *q = (prev_is_sep) ? toupper(*p) : tolower(*p);
    prev_is_sep = curr_is_sep;
  }
  return z;
}

/*
<doc EXT string-titlecase!
 * (string-titlecase! str)
 * (string-titlecase! str start)
 * (string-titlecase! str start end)
 *
 * This is the in-place side-effecting variant of |string-titlecase|.
doc>
*/
DEFINE_PRIMITIVE("string-titlecase!", string_dtitlecase,vsubr,(int argc, SCM *argv))
{
  SCM s;
  int start, end;
  char *endp, *p;
  char prev_is_sep = 1, curr_is_sep;

  s    = control_index(argc, argv, &start, &end);
  endp = STRING_CHARS(s) + end;

  if (BOXED_INFO(s) & STRING_CONST) error_change_const_string(s);

  for (p=STRING_CHARS(s)+start; p < endp; p++) {
    curr_is_sep = !(isalpha(*p));
    if (!curr_is_sep)
      *p = (prev_is_sep) ? toupper(*p) : tolower(*p);
    prev_is_sep = curr_is_sep;
  }
  return s;
}


/*
<doc EXT string-blit!
 * (string-blit! s1 s2 offset)
 *
 * This function places the characters of string |s2| in the string |s1|
 * starting at position |offset|. The result of |string-blit!| may modify
 * the string |s1|. Note that the characters of |s2| can be written after
 * the end of |s1| (in which case a new string is allocated).
 * @lisp
 * (string-blit! (make-string 6 #\X) "abc" 2)
 *               => "XXabcX"
 * (string-blit! (make-string 10 #\X) "abc" 5)
 *               => "XXXXXabcXX"
 * (string-blit! (make-string 6 #\X) "a" 10)
 *               => "XXXXXX\0\0\0\0a"
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("string-blit!", string_blit, subr3,
		 (SCM str1, SCM str2, SCM offset))
{
  long off = STk_integer_value(offset);
  int len1, len2;


  if (!STRINGP(str1))  error_bad_string(str1);
  if (!STRINGP(str2))  error_bad_string(str2);
  if (off == LONG_MIN) STk_error("bad offset ~S", offset);

  if (BOXED_INFO(str1) & STRING_CONST) error_change_const_string(str1);

  len1 = STRING_SIZE(str1);
  len2 = STRING_SIZE(str2);

  if ((len1 == 0) && (off == 0))
    return str2;
  else if ((off + len2) < len1) {    /* str2 can be written in str1 */
    memcpy(STRING_CHARS(str1)+off, STRING_CHARS(str2), len2);
    return str1;
  } else {    /* Size of original string changes => allocate a new string */
    int newl = (len1 >= (off + len2)) ? len1: (off + len2);
    int i, j = 0;
    SCM  new;
    char *snew, *sstr1, *sstr2;

    new    = STk_makestring(newl, NULL);
    snew   = STRING_CHARS(new);
    sstr1  = STRING_CHARS(str1);
    sstr2  = STRING_CHARS(str2);

    for (i = 0; i < newl; i++) {
      if ((i >= off) && (i < (off + len2)))
	*snew++ = sstr2[j++];
      else if (i < len1)
	*snew++ =  sstr1[i];
      else
	snew++;
    }
    return new;
  }
}


/*
DEFINE_PRIMITIVE("string-pos", string_pos, subr2, (SCM str, SCM index))
{
  long k = STk_integer_value(index);
  int n;

  if (!STRINGP(str))			error_bad_string(str);
  if (k < 0 || k >= STRING_LENGTH(str)) error_index_out_of_bound(str, index);

  if (STk_use_utf8 && !STRING_MONOBYTE(str))
    return MAKE_INT(STk_utf8_index(STRING_CHARS(str), STRING_SIZE(str)));
  else
    return index;
}
*/


DEFINE_PRIMITIVE("%use-utf8?", using_utf8, subr0, (void))
{
  return MAKE_BOOLEAN(STk_use_utf8);
}

DEFINE_PRIMITIVE("%string-use-utf8?", string_use_utf8, subr1, (SCM str))
{
  if (!STRINGP(str)) error_bad_string(str);

  return MAKE_BOOLEAN(STk_use_utf8 && !STRING_MONOBYTE(str));
}


int STk_init_string(void)
{
  ADD_PRIMITIVE(stringp);
  ADD_PRIMITIVE(make_string);
  ADD_PRIMITIVE(string);
  ADD_PRIMITIVE(string_length);
  ADD_PRIMITIVE(string_ref);
  ADD_PRIMITIVE(string_set);
  ADD_PRIMITIVE(streq);
  ADD_PRIMITIVE(strlt);
  ADD_PRIMITIVE(strgt);
  ADD_PRIMITIVE(strle);
  ADD_PRIMITIVE(strge);
  ADD_PRIMITIVE(streqi);
  ADD_PRIMITIVE(strlti);
  ADD_PRIMITIVE(strgti);
  ADD_PRIMITIVE(strlei);
  ADD_PRIMITIVE(strgei);
  ADD_PRIMITIVE(substring);
  ADD_PRIMITIVE(string_append);
  ADD_PRIMITIVE(string2list);
  ADD_PRIMITIVE(list2string);
  ADD_PRIMITIVE(string_copy);
  ADD_PRIMITIVE(string_fill);

  ADD_PRIMITIVE(string_find);
  ADD_PRIMITIVE(string_index);
  ADD_PRIMITIVE(string_split);
  ADD_PRIMITIVE(string_mutable);
  ADD_PRIMITIVE(string_downcase);
  ADD_PRIMITIVE(string_ddowncase);
  ADD_PRIMITIVE(string_upcase);
  ADD_PRIMITIVE(string_dupcase);
  ADD_PRIMITIVE(string_titlecase);
  ADD_PRIMITIVE(string_dtitlecase);
  ADD_PRIMITIVE(string_blit);

  ADD_PRIMITIVE(using_utf8);
  ADD_PRIMITIVE(string_use_utf8);
  return TRUE;
}
