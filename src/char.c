/*
 *
 * c h a r . c				-- Characters management
 *
 * Copyright © 1993-2006 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 * Last file update:  6-Aug-2006 22:16 (eg)
 */

#include <ctype.h>
#include "stklos.h"

struct charelem {
  char *name;
  unsigned char value;
};

static struct charelem chartable [] = { 
  {"null",       '\000'},
  {"bell",       '\007'},
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
  return (tolower(CHARACTER_VAL(c1)) - tolower(CHARACTER_VAL(c2)));
}


unsigned char STk_string2char(char *s)
/* converts a char name to a char */
{
  register struct charelem *p;
  
  if (s[1] == '\0') return s[0];
  for (p=chartable; *(p->name); p++) {
    if (my_strcmpi(p->name, s) == 0) return p->value;
  }
  STk_error("bad char name %S", s);
  return '\0'; /* never reached */
}


char *STk_char2string(char c)  		/* convert a char to it's */
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


#define TEST_CTYPE(tst, name) 					  \
   DEFINE_PRIMITIVE(name, CPP_CONCAT(char_, tst), subr1, (SCM c)) \
   { 								  \
     if (!CHARACTERP(c)) error_bad_char(c);			  \
     return MAKE_BOOLEAN(tst(CHARACTER_VAL(c)));		  \
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

TEST_CTYPE(isalpha, "char-alphabetic?")
TEST_CTYPE(isdigit, "char-numeric?")
TEST_CTYPE(isspace, "char-whitespace?")
TEST_CTYPE(isupper, "char-upper-case?")
TEST_CTYPE(islower, "char-lower-case?")


/*=============================================================================*/

DEFINE_PRIMITIVE("char->integer", char2integer, subr1, (SCM c))
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
doc>
 */
{
  if (!CHARACTERP(c)) error_bad_char(c);
  return MAKE_INT((long) CHARACTER_VAL(c));
}


DEFINE_PRIMITIVE("integer->char", integer2char, subr1, (SCM i))
{
  int c = STk_integer_value(i);

  if (c < 0 || c > MAX_CHAR_CODE) STk_error("bad integer ~S", i);
  return MAKE_CHARACTER(c);
}

/*=============================================================================*/

DEFINE_PRIMITIVE("char-upcase", char_upcase, subr1, (SCM c))
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
{ 
  if (!CHARACTERP(c)) error_bad_char(c);
  return MAKE_CHARACTER(toupper(CHARACTER_VAL(c)));
}

DEFINE_PRIMITIVE("char-downcase", char_downcase, subr1, (SCM c))
{
  if (!CHARACTERP(c)) error_bad_char(c);
  return MAKE_CHARACTER(tolower(CHARACTER_VAL(c)));
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
  return TRUE;
}
