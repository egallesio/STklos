/*						-*- coding: utf-8 -*-
 *
 * c h a r . c				-- Chaacters management
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
 *           Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: ??????
 * Last file update: 16-Aug-2011 19:53 (eg)
 */

#include <ctype.h>
#include "stklos.h"
#include <wctype.h>


struct charelem {
  char *name;
  unsigned char value;
};

static struct charelem chartable [] = {
  {"null",       '\000'},
  {"alarm",	 '\007'}, /* R7RS name */
  {"bell",       '\007'}, /* old STklos name, for backward compatibility */
  {"backspace",  '\010'},
  {"tab", 	 '\011'},
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

  {"sp",	'\040'},
  {"del",	'\177'},

  {"",           '\000'}
};


static struct {
  uint16_t ch;
  uint16_t fold;
} casefold_table [] = {
  { 0xb5, 0x3bc },       /*  µ -> μ */
  { 0x17f, 0x73 },       /* ſ -> s */
  { 0x345, 0x3b9 },      /*  ͅ -> ι */
  { 0x3c2, 0x3c3 },      /* ς -> σ */
  { 0x3cf, 0x3d7 },      /*  Ϗ -> ϗ */
  { 0x3d0, 0x3b2 },      /* ϐ -> β */
  { 0x3d1, 0x3b8 },      /*  ϑ -> θ */
  { 0x3d5, 0x3c6 },      /* ϕ -> φ */
  { 0x3d6, 0x3c0 },      /*  ϖ -> π */
  { 0x3f0, 0x3ba },      /* ϰ -> κ */
  { 0x3f1, 0x3c1 },      /* ϱ -> ρ */
  { 0x3f5, 0x3b5 },      /* ϵ -> ε */
  { 0x524, 0x525 },      /* Ԥ -> ԥ */
  { 0x526, 0x527 },      /* Ԧ -> ԧ */
  { 0x1e9b, 0x1e61 },    /* ẛ -> ṡ */
  { 0x1fbe, 0x3b9 },     /* ι -> ι */
  { 0x2c70, 0x252 },     /* Ɒ -> ɒ */
  { 0x2c7e, 0x23f },     /* Ȿ -> ȿ */
  { 0x2c7f, 0x240 },     /* Ɀ -> ɀ */
  { 0x2ceb, 0x2cec },    /* Ⳬ -> ⳬ */
  { 0x2ced, 0x2cee },    /* Ⳮ -> ⳮ */
  { 0xa640, 0xa641 },    /* Ꙁ -> ꙁ */
  { 0xa642, 0xa643 },    /* Ꙃ -> ꙃ */
  { 0xa644, 0xa645 },    /* Ꙅ -> ꙅ */
  { 0xa646, 0xa647 },    /* Ꙇ -> ꙇ */
  { 0xa648, 0xa649 },    /* Ꙉ -> ꙉ */
  { 0xa64a, 0xa64b },    /* Ꙋ -> ꙋ */
  { 0xa64c, 0xa64d },    /* Ꙍ -> ꙍ */
  { 0xa64e, 0xa64f },    /* Ꙏ -> ꙏ */
  { 0xa650, 0xa651 },    /* Ꙑ -> ꙑ */
  { 0xa652, 0xa653 },    /* Ꙓ -> ꙓ */
  { 0xa654, 0xa655 },    /* Ꙕ -> ꙕ */
  { 0xa656, 0xa657 },    /* Ꙗ -> ꙗ */
  { 0xa658, 0xa659 },    /* Ꙙ -> ꙙ */
  { 0xa65a, 0xa65b },    /* Ꙛ -> ꙛ */
  { 0xa65c, 0xa65d },    /* Ꙝ -> ꙝ */
  { 0xa65e, 0xa65f },    /* Ꙟ -> ꙟ */
  { 0xa660, 0xa661 },    /* Ꙡ -> ꙡ */
  { 0xa662, 0xa663 },    /* Ꙣ -> ꙣ */
  { 0xa664, 0xa665 },    /* Ꙥ -> ꙥ */
  { 0xa666, 0xa667 },    /* Ꙧ -> ꙧ */
  { 0xa668, 0xa669 },    /* Ꙩ -> ꙩ */
  { 0xa66a, 0xa66b },    /* Ꙫ -> ꙫ */
  { 0xa66c, 0xa66d },    /* Ꙭ -> ꙭ */
  { 0xa680, 0xa681 },    /* Ꚁ -> ꚁ */
  { 0xa682, 0xa683 },    /* Ꚃ -> ꚃ */
  { 0xa684, 0xa685 },    /* Ꚅ -> ꚅ */
  { 0xa686, 0xa687 },    /* Ꚇ -> ꚇ */
  { 0xa688, 0xa689 },    /* Ꚉ -> ꚉ */
  { 0xa68a, 0xa68b },    /* Ꚋ -> ꚋ */
  { 0xa68c, 0xa68d },    /* Ꚍ -> ꚍ */
  { 0xa68e, 0xa68f },    /* Ꚏ -> ꚏ */
  { 0xa690, 0xa691 },    /* Ꚑ -> ꚑ */
  { 0xa692, 0xa693 },    /* Ꚓ -> ꚓ */
  { 0xa694, 0xa695 },    /* Ꚕ -> ꚕ */
  { 0xa696, 0xa697 },    /* Ꚗ -> ꚗ */
  { 0xa722, 0xa723 },    /* Ꜣ -> ꜣ */
  { 0xa724, 0xa725 },    /* Ꜥ -> ꜥ */
  { 0xa726, 0xa727 },    /* Ꜧ -> ꜧ */
  { 0xa728, 0xa729 },    /* Ꜩ -> ꜩ */
  { 0xa72a, 0xa72b },    /* Ꜫ -> ꜫ */
  { 0xa72c, 0xa72d },    /* Ꜭ -> ꜭ */
  { 0xa72e, 0xa72f },    /* Ꜯ -> ꜯ */
  { 0xa732, 0xa733 },    /* Ꜳ -> ꜳ */
  { 0xa734, 0xa735 },    /* Ꜵ -> ꜵ */
  { 0xa736, 0xa737 },    /* Ꜷ -> ꜷ */
  { 0xa738, 0xa739 },    /* Ꜹ -> ꜹ */
  { 0xa73a, 0xa73b },    /* Ꜻ -> ꜻ */
  { 0xa73c, 0xa73d },    /* Ꜽ -> ꜽ */
  { 0xa73e, 0xa73f },    /* Ꜿ -> ꜿ */
  { 0xa740, 0xa741 },    /* Ꝁ -> ꝁ */
  { 0xa742, 0xa743 },    /* Ꝃ -> ꝃ */
  { 0xa744, 0xa745 },    /* Ꝅ -> ꝅ */
  { 0xa746, 0xa747 },    /* Ꝇ -> ꝇ */
  { 0xa748, 0xa749 },    /* Ꝉ -> ꝉ */
  { 0xa74a, 0xa74b },    /* Ꝋ -> ꝋ */
  { 0xa74c, 0xa74d },    /* Ꝍ -> ꝍ */
  { 0xa74e, 0xa74f },    /* Ꝏ -> ꝏ */
  { 0xa750, 0xa751 },    /* Ꝑ -> ꝑ */
  { 0xa752, 0xa753 },    /* Ꝓ -> ꝓ */
  { 0xa754, 0xa755 },    /* Ꝕ -> ꝕ */
  { 0xa756, 0xa757 },    /* Ꝗ -> ꝗ */
  { 0xa758, 0xa759 },    /* Ꝙ -> ꝙ */
  { 0xa75a, 0xa75b },    /* Ꝛ -> ꝛ */
  { 0xa75c, 0xa75d },    /* Ꝝ -> ꝝ */
  { 0xa75e, 0xa75f },    /* Ꝟ -> ꝟ */
  { 0xa760, 0xa761 },    /* Ꝡ -> ꝡ */
  { 0xa762, 0xa763 },    /* Ꝣ -> ꝣ */
  { 0xa764, 0xa765 },    /* Ꝥ -> ꝥ */
  { 0xa766, 0xa767 },    /* Ꝧ -> ꝧ */
  { 0xa768, 0xa769 },    /* Ꝩ -> ꝩ */
  { 0xa76a, 0xa76b },    /* Ꝫ -> ꝫ */
  { 0xa76c, 0xa76d },    /* Ꝭ -> ꝭ */
  { 0xa76e, 0xa76f },    /* Ꝯ -> ꝯ */
  { 0xa779, 0xa77a },    /* Ꝺ -> ꝺ */
  { 0xa77b, 0xa77c },    /* Ꝼ -> ꝼ */
  { 0xa77d, 0x1d79 },    /* Ᵹ -> ᵹ */
  { 0xa77e, 0xa77f },    /* Ꝿ -> ꝿ */
  { 0xa780, 0xa781 },    /* Ꞁ -> ꞁ */
  { 0xa782, 0xa783 },    /* Ꞃ -> ꞃ */
  { 0xa784, 0xa785 },    /* Ꞅ -> ꞅ */
  { 0xa786, 0xa787 },    /* Ꞇ -> ꞇ */
  { 0xa78b, 0xa78c },    /* Ꞌ -> ꞌ */
  { 0xa78d, 0x265 },     /* Ɥ -> ɥ */
  { 0xa790, 0xa791 },    /* Ꞑ -> ꞑ */
  { 0xa7a0, 0xa7a1 },    /* Ꞡ -> ꞡ */
  { 0xa7a2, 0xa7a3 },    /* Ꞣ -> ꞣ */
  { 0xa7a4, 0xa7a5 },    /* Ꞥ -> ꞥ */
  { 0xa7a6, 0xa7a7 },    /* Ꞧ -> ꞧ */
  { 0xa7a8, 0xa7a9 }     /* Ꞩ -> ꞩ */
};


int STk_casefold_char(int ch)
{
  static int min = -1, max= -1, len = -1;

  if (len == -1)  {
    /* Never run before. Initialize static variables */
    len = sizeof(casefold_table) / sizeof(casefold_table[0]);
    min = casefold_table[0].ch;
    max = casefold_table[len-1].ch;
  }

  if (min <= ch && ch <= max) {
    /* seach the value in the casefold_table by dichotomy */
    int left, right, i;

    left = 0; right = len-1;
    do {
      i = (left + right) / 2;
      if (ch == casefold_table[i].ch)
	return casefold_table[i].fold;
      else
	if (ch < casefold_table[i].ch)
	  right = i -1;
	else
	  left = i +1;
    }
    while (left <= right);
  }

  /* not found of not in the interval of special character => return the
   * corresponding lowercase character
   */
  return towlower(ch);
}


/*===========================================================================*\
 *
 * 				     Utilities
 *
\*===========================================================================*/

static int my_strcmpi(register char *p1, register char *p2)
{
  for( ; tolower(*p1) == tolower(*p2); p1++, p2++)
    if (!*p1) return 0;
  return tolower(*p1) - tolower(*p2);
}


static void error_bad_char(SCM c)
{
  STk_error("bad char", c);
}

static int charcomp(SCM c1, SCM c2)
{
  if (!CHARACTERP(c1)) error_bad_char(c1);
  if (!CHARACTERP(c2)) error_bad_char(c2);
  return (CHARACTER_VAL(c1) - CHARACTER_VAL(c2));
}


static int charcompi(SCM c1, SCM c2)
{
  if (!CHARACTERP(c1)) error_bad_char(c1);
  if (!CHARACTERP(c2)) error_bad_char(c2);
  return STk_use_utf8 ?
         (STk_casefold_char(CHARACTER_VAL(c1)) -
	  STk_casefold_char(CHARACTER_VAL(c2))):
         (tolower((unsigned char) CHARACTER_VAL(c1)) -
	  tolower((unsigned char) CHARACTER_VAL(c2)));
}


int STk_string2char(char *s)
/* converts a char name to a char */
{
  register struct charelem *p;
  uint32_t val;

  /* Try to see if it is a multi-byte character */
  if (* (STk_utf8_grab_char(s, &val)) == '\0') return val;

  if (*s == 'x') {
    char *end;
    long int val = strtol(s+1, &end, 16);

    if (val == LONG_MIN || val == LONG_MAX || *end)
      STk_error("bad hexdecimal value '%s' when reading char", s);
    return (int) val;
  }

  for (p=chartable; *(p->name); p++) {
    if (my_strcmpi(p->name, s) == 0) return (int) (p->value);
  }
  STk_error("bad char name %S", s);
  return 0; /* never reached */
}


char *STk_char2string(int c)  		/* convert a character to it's */
{					/* external representation */
  register struct charelem *p;

  for (p=chartable; *(p->name); p++)
    if (p->value == c) return (char *) p->name;

  /* If we are here it's a "normal" char */
  return NULL;
}


/*===========================================================================*\
 *
 * 				     PRIMITIVES
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


#define CHAR_COMPARE(sname, cname, tst)				\
   DEFINE_PRIMITIVE(sname, cname, subr2, (SCM c1, SCM c2))	\
   {								\
     return MAKE_BOOLEAN(tst);					\
   }

/*
<doc char=? char<? char>? char<=? char>=?
 * (char=? char1 char2)
 * (char<? char1 char2)
 * (char>? char1 char2)
 * (char<=? char1 char2)
 * (char>=? char1 char2)
 *
 * These procedures impose a total ordering on the set of characters.
 * It is guaranteed that under this ordering:
 * ,(itemize
 * (item [The upper case characters are in order.])
 * (item [The lower case characters are in order.])
 * (item [The digits are in order.])
 * (item [Either all the digits precede all the upper case letters, or vice versa.])
 * (item [Either all the digits precede all the lower case letters, or vice versa.])
 * )
doc>
 */

CHAR_COMPARE("char=?",  chareq, (charcomp(c1,c2) == 0))
CHAR_COMPARE("char<?",  charlt, (charcomp(c1,c2) <  0))
CHAR_COMPARE("char>?",  chargt, (charcomp(c1,c2) >  0))
CHAR_COMPARE("char<=?", charle, (charcomp(c1,c2) <= 0))
CHAR_COMPARE("char>=?", charge, (charcomp(c1,c2) >= 0))


/*
<doc char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
 * (char-ci=? char1 char2)
 * (char-ci<? char1 char2)
 * (char-ci>? char1 char2)
 * (char-ci<=? char1 char2)
 * (char-ci>=? char1 char2)
 *
 * These procedures are similar to |char=?| et cetera, but they treat
 * upper case and lower case letters as the same. For example,
 * |(char-ci=? #\A #\a)| returns |#t|.
doc>
 */

CHAR_COMPARE("char-ci=?",  chareqi, (charcompi(c1,c2) == 0))
CHAR_COMPARE("char-ci<?",  charlti, (charcompi(c1,c2) <  0))
CHAR_COMPARE("char-ci>?",  chargti, (charcompi(c1,c2) >  0))
CHAR_COMPARE("char-ci<=?", charlei, (charcompi(c1,c2) <= 0))
CHAR_COMPARE("char-ci>=?", chargei, (charcompi(c1,c2) >= 0))


/*=============================================================================*/


#define TEST_CTYPE(tst, name)                                                       \
   DEFINE_PRIMITIVE(name, CPP_CONCAT(char_is, tst), subr1, (SCM c))                 \
   {                                                                                \
     if (!CHARACTERP(c)) error_bad_char(c);                                         \
     return STk_use_utf8 ?                                                          \
              MAKE_BOOLEAN(CPP_CONCAT(isw, tst)(CHARACTER_VAL(c))):                 \
              MAKE_BOOLEAN(CPP_CONCAT(is,  tst)((unsigned char) CHARACTER_VAL(c))); \
   }

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

TEST_CTYPE(alpha, "char-alphabetic?")
TEST_CTYPE(digit, "char-numeric?")
TEST_CTYPE(space, "char-whitespace?")
TEST_CTYPE(upper, "char-upper-case?")
TEST_CTYPE(lower, "char-lower-case?")


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
 *    (char<=? a b) => #t  ,(bold "and")  (<= x y) => #t
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
 * #xE000 and #x10FFFF, if UTF8 encoding is used. Otherwise it accepts a
 * number between0 and #xFF.
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
DEFINE_PRIMITIVE("char-upcase", char_upcase, subr1, (SCM c))
{
  if (!CHARACTERP(c)) error_bad_char(c);
  return MAKE_CHARACTER(STk_use_utf8 ?
			towupper(CHARACTER_VAL(c)):
			toupper((unsigned char) CHARACTER_VAL(c)));
}

DEFINE_PRIMITIVE("char-downcase", char_downcase, subr1, (SCM c))
{
  if (!CHARACTERP(c)) error_bad_char(c);
  return MAKE_CHARACTER(STk_use_utf8 ?
			towlower(CHARACTER_VAL(c)) :
			tolower((unsigned char) CHARACTER_VAL(c)));
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
DEFINE_PRIMITIVE("char-foldcase", char_foldcase, subr1, (SCM c))
{
  if (!CHARACTERP(c))  error_bad_char(c);
  return MAKE_CHARACTER(STk_use_utf8 ?
			STk_casefold_char(CHARACTER_VAL(c)):
			tolower((unsigned char) CHARACTER_VAL(c)));
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


  ADD_PRIMITIVE(char2integer);
  ADD_PRIMITIVE(integer2char);

  ADD_PRIMITIVE(char_upcase);
  ADD_PRIMITIVE(char_downcase);
  ADD_PRIMITIVE(char_foldcase);

  return TRUE;
}
