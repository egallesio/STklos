/*                                              -*- coding: utf-8 -*-
 *
 * c h a r . c                          -- Characters management
 *
 * Copyright © 1993-2022 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *           Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: ??????
 * Last file update:  3-Feb-2022 12:02 (eg)
 */

#include <ctype.h>
#include "stklos.h"

struct charelem {
  char *name;
  unsigned char value;
};

static struct charelem chartable [] = {
  {"null",       '\000'},
  {"alarm",      '\007'}, /* R7RS name */
  {"bell",       '\007'}, /* old STklos name, for backward compatibility */
  {"backspace",  '\010'},
  {"tab",        '\011'},
  {"newline",    '\012'},
  {"page",       '\014'},
  {"return",     '\015'},
  {"escape",     '\033'},
  {"space",      '\040'},
  {"delete",     '\177'},

  /* poeticless names */
  {"nul",        '\000'},
  {"soh",        '\001'},
  {"stx",        '\002'},
  {"etx",        '\003'},
  {"eot",        '\004'},
  {"enq",        '\005'},
  {"ack",        '\006'},
  {"bel",        '\007'},

  {"bs",         '\010'},
  {"ht",         '\011'},
  {"nl",         '\012'},
  {"vt",         '\013'},
  {"np",         '\014'},
  {"cr",         '\015'},
  {"so",         '\016'},
  {"si",         '\017'},

  {"dle",        '\020'},
  {"dc1",        '\021'},
  {"dc2",        '\022'},
  {"dc3",        '\023'},
  {"dc4",        '\024'},
  {"nak",        '\025'},
  {"syn",        '\026'},
  {"etb",        '\027'},

  {"can",        '\030'},
  {"em",         '\031'},
  {"sub",        '\032'},
  {"esc",        '\033'},
  {"fs",         '\034'},
  {"gs",         '\035'},
  {"rs",         '\036'},
  {"us",         '\037'},

  {"sp",        '\040'},
  {"del",       '\177'},

  {"",           '\000'}
};

struct utf8_conversion_char {
  uint32_t key;
  uint32_t val;
};

typedef uint32_t utf8_char;

#include "utf8-tables.inc"

static int search_conversion_table(unsigned int ch, struct utf8_conversion_char table[],
                                   int len) {
  unsigned int min = table[0].key;
  unsigned int max = table[len-1].key;

  if (min <= ch && ch <= max) {
    /* seach the value in the table by dichotomy */
    int left, right, i;

    left = 0; right = len-1;
    do {
      i = (left + right) / 2;
      if (ch == table[i].key)
        return table[i].val;
      else
        if (ch < table[i].key)
          right = i-1;
        else
          left = i+1;
    }
    while (left <= right);
  }
  /* not found of not in the interval of special character => return -1 */
  return -1;
}

static int search_ordered_list(utf8_char ch,  utf8_char table[], int len) {
  unsigned int min = table[0];
  unsigned int max = table[len-1];

  if (min <= ch && ch <= max) {
    /* seach the value in the table by dichotomy */
    int left, right, i;

    left = 0; right = len-1;
    do {
      i = (left + right) / 2;
      if (ch == table[i])
        return table[i];
      else
        if (ch < table[i])
          right = i-1;
        else
          left = i+1;
    }
    while (left <= right);
  }
  /* not found of not in the interval of special character => return -1 */
  return -1;
}


/*===========================================================================*\
 *
 *                                   Utilities
 *
\*===========================================================================*/

static void error_bad_char(SCM c)
{
  STk_error("bad char", c);
}

static Inline int charcomp(SCM c1, SCM c2)
{
  return (CHARACTER_VAL(c1) - CHARACTER_VAL(c2));
}


static int charcompi(SCM c1, SCM c2)
{
  return STk_use_utf8 ?
    (int) (STk_to_fold(CHARACTER_VAL(c1)) -
           STk_to_fold(CHARACTER_VAL(c2))):
    (int) (tolower((unsigned char) CHARACTER_VAL(c1)) -
           tolower((unsigned char) CHARACTER_VAL(c2)));
}

/* Comparison of characters. No test on types */
int STk_charcomp(SCM c1, SCM c2)  { return charcomp(c1,  c2);  }
int STk_charcompi(SCM c1, SCM c2) { return charcompi(c1,  c2); }


int STk_string2char(char *s)
/* converts a char name to a char */
{
  register struct charelem *p;
  uint32_t val;

  /* Try to see if it is a multibyte character */
  if (* (STk_utf8_grab_char(s, &val)) == '\0') return val;

  if (*s == 'x') {
    char *end;
    long int val = strtol(s+1, &end, 16);

    if (val == LONG_MIN || val == LONG_MAX || *end)
      STk_error("bad hexdecimal value '%s' when reading char", s);
    return (int) val;
  }

  for (p=chartable; *(p->name); p++) {
    if (strcasecmp(p->name, s) == 0) return (int) (p->value);
  }
  STk_error("bad char name %S", s);
  return 0; /* never reached */
}


char *STk_char2string(int c)            /* convert a character to it's */
{                                       /* external representation */
  register struct charelem *p;

  for (p=chartable; *(p->name); p++)
    if (p->value == c) return (char *) p->name;

  /* If we are here it's a "normal" char */
  return NULL;
}


/*===========================================================================*\
 *
 *                                   PRIMITIVES
 *
\*===========================================================================*/

DEFINE_PRIMITIVE("char?", charp, subr1, (SCM obj))
/*
<doc char?
 * (char? obj)
 *
 * Returns |#t| if |obj| is a character, otherwise returns |#f|.
doc>
 */
{
  return MAKE_BOOLEAN(CHARACTERP(obj));
}

/*=============================================================================*/

/*
<doc char=? char<? char>? char<=? char>=?
 * (char=? char1 char2 ...)
 * (char<? char1 char2 ...)
 * (char>? char1 char2 ...)
 * (char<=? char1 char2 ...)
 * (char>=? char1 char2 ...)
 *
 * These procedures impose a total ordering on the set of characters.
 * It is guaranteed that under this ordering:
 *
 * - The upper case characters are in order.
 * - The lower case characters are in order.
 * - The digits are in order.
 * - Either all the digits precede all the upper case letters, or vice versa.
 * - Either all the digits precede all the lower case letters, or vice versa.
 * )
doc>
 */

#define CHAR_COMPARE(sname, name, tst)                         \
  DEFINE_PRIMITIVE(sname, name, vsubr, (int argc, SCM *argv)) { \
    SCM last;                                                   \
                                                                \
    if (!argc) STk_error("need at least one parameter");        \
    if (!CHARACTERP(*argv)) error_bad_char(*argv);              \
                                                                \
    for (last = *argv--; --argc; last=*argv--) {                \
      if (!CHARACTERP(*argv)) error_bad_char(*argv);            \
      if (tst) return STk_false;                                \
    }                                                           \
    return STk_true;                                            \
  }

CHAR_COMPARE("char=?",  chareq, (charcomp(last, *argv) != 0))
CHAR_COMPARE("char<?",  charlt, (charcomp(last, *argv) >= 0))
CHAR_COMPARE("char>?",  chargt, (charcomp(last, *argv) <= 0))
CHAR_COMPARE("char<=?", charle, (charcomp(last, *argv) > 0))
CHAR_COMPARE("char>=?", charge, (charcomp(last, *argv) < 0))


/*
<doc char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
 * (char-ci=? char1 char2 ...)
 * (char-ci<? char1 char2 ...)
 * (char-ci>? char1 char2 ...)
 * (char-ci<=? char1 char2 ...)
 * (char-ci>=? char1 char2 ...)
 *
 * These procedures are similar to |char=?| et cetera, but they treat
 * upper case and lower case letters as the same. For example,
 * |(char-ci=? #\A #\a)| returns |#t|.
doc>
 */
CHAR_COMPARE("char-ci=?",  chareqi, (charcompi(last,*argv) != 0))
CHAR_COMPARE("char-ci<?",  charlti, (charcompi(last,*argv) >= 0))
CHAR_COMPARE("char-ci>?",  chargti, (charcompi(last,*argv) <= 0))
CHAR_COMPARE("char-ci<=?", charlei, (charcompi(last,*argv) > 0))
CHAR_COMPARE("char-ci>=?", chargei, (charcompi(last,*argv) < 0))


/*
<doc char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case?
 * (char-alphabetic? char)
 * (char-numeric? char)
 * (char-whitespace? char)
 * (char-upper-case? letter)
 * (char-lower-case? letter)
 *
 * These procedures return |#t| if their arguments are alphabetic, numeric,
 * whitespace, upper case, or lower case characters, respectively, otherwise they
 * return |#f|. The following remarks, which are specific to the ASCII character
 * set, are intended only as a guide: The alphabetic characters are the 52
 * upper and lower case letters. The numeric characters are the ten decimal
 * digits. The whitespace characters are space, tab, line feed, form feed,
 * and carriage return.
doc>
 */

DEFINE_PRIMITIVE("char-alphabetic?", char_isalpha, subr1, (SCM c)) {
  if (!CHARACTERP(c)) error_bad_char(c);
  if (STk_use_utf8)
    return MAKE_BOOLEAN(-1 != search_ordered_list(CHARACTER_VAL(c),
                                                  letters_table,
                                                  letters_table_length));
  else
    return MAKE_BOOLEAN(isalpha(CHARACTER_VAL(c)));
}


DEFINE_PRIMITIVE("char-numeric?", char_isdigit, subr1, (SCM c)) {
  if (!CHARACTERP(c)) error_bad_char(c);
  if (STk_use_utf8)
    return MAKE_BOOLEAN(-1 != search_conversion_table(CHARACTER_VAL(c),
                                                      digits_table,
                                                      digits_table_length));
  else
    return MAKE_BOOLEAN(isdigit(CHARACTER_VAL(c)));
}


DEFINE_PRIMITIVE("char-whitespace?", char_isspace, subr1, (SCM c)) {
  if (!CHARACTERP(c)) error_bad_char(c);
  if (STk_use_utf8)
    return MAKE_BOOLEAN(-1 != search_ordered_list(CHARACTER_VAL(c),
                                                  spaces_table,
                                                  spaces_table_length));
  else
    return MAKE_BOOLEAN(isspace(CHARACTER_VAL(c)));
}

DEFINE_PRIMITIVE("char-upper-case?", char_isupper, subr1, (SCM c)) {
  if (!CHARACTERP(c)) error_bad_char(c);
  if (STk_use_utf8)
    return MAKE_BOOLEAN(-1 != search_conversion_table(CHARACTER_VAL(c),
                                                      upper_table,
                                                      upper_table_length));
  else
    return MAKE_BOOLEAN(isupper(CHARACTER_VAL(c)));
}


DEFINE_PRIMITIVE("char-lower-case?", char_islower, subr1, (SCM c)) {
  if (!CHARACTERP(c)) error_bad_char(c);
  if (STk_use_utf8)
    return MAKE_BOOLEAN(-1 != search_conversion_table(CHARACTER_VAL(c),
                                                      lower_table,
                                                      lower_table_length));
  else
    return MAKE_BOOLEAN(islower(CHARACTER_VAL(c)));
}

/*
<doc R7RS digit-value
 * (digit-value char)
 *
 * This procedure returns the numeric value (0 to 9) of its
 * argument if it is a numeric digit (that is, if char-numeric?
 * returns #t), or #f on any other character.
 * @lisp
 * (digit-value
 * (digit-value #\3)        => 3
 * (digit-value #\x0664)    => 4
 * (digit-value #\x0AE6)    => 0
 * (digit-value #\x0EA6)    => #f
 * @end lisp
doc>
 */

DEFINE_PRIMITIVE("digit-value", digit_value, subr1, (SCM c))
{
  int val;

  if (!CHARACTERP(c)) error_bad_char(c);

  val = CHARACTER_VAL(c);
  if (STk_use_utf8) {
    int res = search_conversion_table(val, digits_table, digits_table_length);
    return (res == -1) ? STk_false: MAKE_INT(res);
  } else {
    return (('0' <= val) && (val <= '9')) ? MAKE_INT(val - '0'): STk_false;
  }
}
/*=============================================================================*/

/*
<doc char->integer integer->char
 * (char->integer char)
 * (integer->char n)
 *
 * Given a character, |char->integer| returns an exact integer
 * representation of the character. Given an exact integer that is the
 * image of a character under |char->integer|, |integer->char| returns
 * that character. These procedures implement order-preserving
 * isomorphisms between the set of characters under the |char<=?|
 * ordering and some subset of the integers under the |<=|
 * ordering. That is, if
 * @lisp
 *    (char<=? a b) => #t  and  (<= x y) => #t
 * @end lisp
 * and x and y are in the domain of |integer->char|, then
 * @lisp
 *    (<= (char->integer a)
 *        (char->integer b))         =>  #t
 *
 *   (char<=? (integer->char x)
 *            (integer->char y))     =>  #t
 * @end lisp
 * |integer->char| accepts an exact number between 0 and #xD7FFF or between
 * #xE000 and #x10FFFF, if UTF8 encoding is used. Otherwise, it accepts a
 * number between 0 and #xFF.
doc>
 */
DEFINE_PRIMITIVE("char->integer", char2integer, subr1, (SCM c))
{
  if (!CHARACTERP(c)) error_bad_char(c);
  return MAKE_INT((long) CHARACTER_VAL(c));
}


DEFINE_PRIMITIVE("integer->char", integer2char, subr1, (SCM i))
{
  int c = STk_integer_value(i);

  if (STk_use_utf8) {
    /* Unicode defines characters in the range [0, #xd7FF] U [#xE000, #x10FFFF] */
    if (! VALID_UTF8_VALUE(c))
      STk_error("bad integer ~S (must be in range [0, #xd7FF] U [#xE000, #x10FFFF]",
                i);
  }
  else
    /* Monobyte character: use them in the range [0, #xFF] */
    if (! (0 <= c  && c <=  0xff))
      STk_error("bad integer ~S (must be in range [0, #xFF]", i);

  return MAKE_CHARACTER(c);
}

/*=============================================================================*/

/*
<doc char-upcase char-downcase
 * (char-upcase char)
 * (char-downcase char)
 *
 * These procedures return a character |char2| such that
 * |(char-ci=? char char2)|. In addition, if char is alphabetic, then the
 * result of |char-upcase| is upper case and the result of |char-downcase| is
 * lower case.
doc>
 */
uint32_t STk_to_upper(uint32_t c) {
  if (STk_use_utf8) {
    int res = search_conversion_table(c, lower_table, lower_table_length);
    return (res <= 0) ? c : (uint32_t) res;   // -1: not a lowercase, 0 lower without upper
  } else
    return toupper(c);
}

uint32_t STk_to_lower(uint32_t c) {
  if (STk_use_utf8) {
    int res = search_conversion_table(c, upper_table, upper_table_length);
    return (res <=0) ? c : (uint32_t) res;    // -1: not a lowercase, 0 lower without lower
  } else
    return tolower(c);
}

DEFINE_PRIMITIVE("char-upcase", char_upcase, subr1, (SCM c))
{
  if (!CHARACTERP(c)) error_bad_char(c);
  return MAKE_CHARACTER(STk_to_upper((uint32_t) CHARACTER_VAL(c)));
}


DEFINE_PRIMITIVE("char-downcase", char_downcase, subr1, (SCM c))
{
  if (!CHARACTERP(c)) error_bad_char(c);
  return MAKE_CHARACTER(STk_to_lower((uint32_t) CHARACTER_VAL(c)));
}
/*
<doc EXT char-foldcase
 * (char-foldcase char)
 *
 * This procedure applies the Unicode simple case folding algorithm and returns
 * the result. Note that language-sensitive folding is not used. If
 * the argument is an uppercase letter, the result will be either a
 * lowercase letter or the same as the argument if the lowercase letter
 * does not exist.
doc>
 */
uint32_t STk_to_fold(uint32_t c) {
  if (STk_use_utf8) {
    int res = search_conversion_table(c, fold_table, fold_table_length);
    return (res <=0) ? STk_to_lower(c) : (uint32_t) res;
  } else
    return tolower(c);
}

DEFINE_PRIMITIVE("char-foldcase", char_foldcase, subr1, (SCM c))
{
  if (!CHARACTERP(c))  error_bad_char(c);
  return MAKE_CHARACTER(STk_to_fold((uint32_t) CHARACTER_VAL(c)));
}


int STk_init_char(void)
{
  ADD_PRIMITIVE(charp);

  ADD_PRIMITIVE(chareq);
  ADD_PRIMITIVE(charlt);
  ADD_PRIMITIVE(chargt);
  ADD_PRIMITIVE(charle);
  ADD_PRIMITIVE(charge);

  ADD_PRIMITIVE(chareqi);
  ADD_PRIMITIVE(charlti);
  ADD_PRIMITIVE(chargti);
  ADD_PRIMITIVE(charlei);
  ADD_PRIMITIVE(chargei);

  ADD_PRIMITIVE(char_isalpha);
  ADD_PRIMITIVE(char_isdigit);
  ADD_PRIMITIVE(char_isspace);
  ADD_PRIMITIVE(char_isupper);
  ADD_PRIMITIVE(char_islower);

  ADD_PRIMITIVE(digit_value);


  ADD_PRIMITIVE(char2integer);
  ADD_PRIMITIVE(integer2char);

  ADD_PRIMITIVE(char_upcase);
  ADD_PRIMITIVE(char_downcase);
  ADD_PRIMITIVE(char_foldcase);

  return TRUE;
}
