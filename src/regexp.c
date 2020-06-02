/*
 * regexp.c -- STklos Regexps
 *
 * Copyright © 2000-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 24-Nov-2000 10:35 (eg)
 * Last file update:  2-Jun-2020 12:19 (eg)
 */

#include "stklos.h"

/*
 * All the complexity comes from MAC OS. I don't remember why I had to do
 * this, but I remember the culprit !!!
 */

#ifdef HAVE_PCRE
#  define PCRE_regexec  regexec
#  define PCRE_regcomp  regcomp
#  define PCRE_regerror regerror
#  define PCRE_regfree  regfree
#else
#  define regexec  PCRE_regexec
#  define regcomp  PCRE_regcomp
#  define regerror PCRE_regerror
#  define regfree  PCRE_regfree
#endif

/* ---------------------------------------------------------------------- */
#ifdef PCRE_PKG_CONFIG
#  include <pcreposix.h>
#else
/* Here again Mac Os problems.
 * Here, we used to have a #include <pcreposix.h>
 * However, on a fresh 10.6 install, there is a pcre lib which is
 * installed, but the "pcreposix.h" file isn't.
 *
 * So what we do here is potentially FALSE. However, it is unlikely
 * that the code include in STklos is not compatible with the one
 * used to compile the installed library (this file seems to have
 * been always "semantically" compatible). The only point where we
 * can have difference should be the definition of regmatch_t type
 */
#  include "../pcre/pcreposix.h"
#endif

#ifdef REG_UTF8
#  define PCRE_COMP_FLAG REG_UTF8
#else
#  define PCRE_COMP_FLAG 0
#endif


/* ---------------------------------------------------------------------- */

struct regexp_obj {
  stk_header header;
  SCM src;
  regex_t buffer;
};

#define REGEXPP(p)      (BOXED_TYPE_EQ((p), tc_regexp))
#define REGEXP_SRC(p)       (((struct regexp_obj *) (p))->src)
#define REGEXP_BUFFER(p)    (((struct regexp_obj *) (p))->buffer)
#define REGEXP_DEPTH(p) ((((struct regexp_obj *) (p))->buffer).re_nsub)


static void error_bad_string(SCM obj)
{
  STk_error("bad string ~S", obj);
}


static void signal_regexp_error(int code, regex_t *buffer)
{
  size_t len;
  char *msg;

  /* First call regerror with a null buffer to get the message length */
  len = PCRE_regerror(code, buffer, NULL, 0);

  /* Call again regerror with a freshly allocated buffer */
  msg = STk_must_malloc_atomic(len+2); /* ??? */
  PCRE_regerror(code, buffer, msg, len+1);

  STk_error("%s", msg);
}

static void regexp_finalizer(SCM re, void _UNUSED(*client_data))
{
  PCRE_regfree(&REGEXP_BUFFER(re));
}

static void print_regexp(SCM obj, SCM port, int _UNUSED(mode))
{
  STk_fprintf(port,
          "#[regexp '%s' @ %lx]",
          STRING_CHARS(REGEXP_SRC(obj)),
          (unsigned long) obj);
}

/*
<doc EXT string->regexp
 * (string->regexp string)
 *
 * |String->regexp| takes a string representation of a regular
 * expression and compiles it into a regexp value. Other regular
 * expression procedures accept either a string or a regexp value as
 * the matching pattern. If a regular expression string is used
 * multiple times, it is faster to compile the string once to a regexp
 * value and use it for repeated matches instead of using the string
 * each time.
doc>
 */
DEFINE_PRIMITIVE("string->regexp", str2regexp, subr1, (SCM re))
{
  SCM z;
  int ret;

  if (!STRINGP(re)) error_bad_string(re);
  NEWCELL(z, regexp);

  ret = PCRE_regcomp(&REGEXP_BUFFER(z),
             STRING_CHARS(re),
             STk_use_utf8? PCRE_COMP_FLAG: 0);
  REGEXP_SRC(z) = re;
  if (ret) signal_regexp_error(ret, &REGEXP_BUFFER(z));
  STk_register_finalizer(z, regexp_finalizer);

  return z;
}

/*
<doc EXT regexp?
 * (regexp? obj)
 *
 * |Regexp| returns |#t| if |obj| is a regexp value created by the |regexp|,
 * otherwise |regexp| returns |#f|.
doc>
 */
DEFINE_PRIMITIVE("regexp?", regexpp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(REGEXPP(obj));
}


/*
<doc EXT regexp-match regexp-match-positions
 * (regexp-match pattern str)
 * (regexp-match-positions pattern str)
 *
 * These functions attempt to match |pattern| (a string or a regexp value)
 * to |str|. If the match fails, |#f| is returned. If the match succeeds,
 * a list (containing strings for |regexp-match| and positions for
 * |regexp-match-positions| is returned. The first string (or positions) in
 * this list is the portion of string that matched pattern. If two portions
 * of string can match pattern, then the earliest and longest match is found,
 * by default.
 *
 * Additional strings or positions are returned in the list if pattern contains
 * parenthesized sub-expressions; matches for the sub-expressions are provided
 * in the order of the opening parentheses in pattern.
 * @lisp
 * (regexp-match-positions "ca" "abracadabra")
 *                  => ((4 6))
 * (regexp-match-positions "CA" "abracadabra")
 *                  => #f
 * (regexp-match-positions "(?i)CA" "abracadabra")
 *                  => ((4 6))
 * (regexp-match "(a*)(b*)(c*)" "abc")
 *                  => ("abc" "a" "b" "c")
 * (regexp-match-positions "(a*)(b*)(c*)" "abc")
 *                  => ((0 3) (0 1) (1 2) (2 3))
 * (regexp-match-positions "(a*)(b*)(c*)" "c")
 *                  => ((0 1) (0 0) (0 0) (0 1))
 * (regexp-match-positions "(?<=\\\\d{3})(?<!999)foo"
 *                         "999foo and 123foo")
 *      => ((14 17))
 * @end lisp
doc>
*/
#define MAX_PMATCH 30

static SCM regexec_helper(SCM re, SCM str, int pos_only)
{
  regmatch_t pmatch[MAX_PMATCH+1];
  int i, ret, depth, max;
  SCM result;

  /* RE can be a string or a already compiled regexp */
  if (STRINGP(re)) re = STk_str2regexp(re);
  else if (!REGEXPP(re)) STk_error("bad compiled regexp ~S", re);

  if (!STRINGP(str)) error_bad_string(str);

  ret = PCRE_regexec(&REGEXP_BUFFER(re), STRING_CHARS(str), MAX_PMATCH, pmatch, 0);

  if (ret) {
    if (ret == REG_NOMATCH) /* No match ==> #f */ return STk_false;
    signal_regexp_error(ret, &REGEXP_BUFFER(re));
  }

  result = STk_nil;
  depth  = REGEXP_DEPTH(re);
  max    = (depth < MAX_PMATCH) ? (depth + 1) : MAX_PMATCH;

  for(i=0; i < max; i++) {
    int from = pmatch[i].rm_so;
    int to   = pmatch[i].rm_eo;

    if (from == -1)
      result = STk_cons((pos_only)?
                LIST2(MAKE_INT(0), MAKE_INT(0)) :
                STk_false,
            result);
    else {
      int ifrom, ito;
      char *s = STRING_CHARS(str);
      int size = STRING_SIZE(str);

      if (STRING_MONOBYTE(str)) {
    ifrom = from;
    ito = to;
      } else {
    ifrom = STk_utf8_char_from_byte(s, from, size);
    ito   = STk_utf8_char_from_byte(s, to, size);
      }

      result = STk_cons((pos_only)?
                    LIST2(STk_long2integer(ifrom), STk_long2integer(ito)) :
                    STk_makestring(to-from, s+from),
            result);
    }
  }

  return STk_dreverse(result);
}


DEFINE_PRIMITIVE("regexp-match", regmatch, subr2, (SCM re, SCM str))
{
  return regexec_helper(re, str, FALSE);
}

DEFINE_PRIMITIVE("regexp-match-positions", regmatch_pos, subr2, (SCM re, SCM str))
{
  return regexec_helper(re, str, TRUE);
}


/*
<doc EXT regexp-quote
 * (regexp-quote str)
 *
 * Takes an arbitrary string and returns a string where characters of
 * |str| that could serve as regexp metacharacters are escaped with a
 * backslash, so that they safely match only themselves.
 * @lisp
 * (regexp-quote "cons")       => "cons"
 * (regexp-quote "list?")      => "list\\\\?"
 * @end lisp
 * |regexp-quote| is useful when building a composite regexp from
 * a mix of regexp strings and verbatim strings.
doc>
*/
#define REGEXP_SPECIALS "\\.?*+|[]{}()"
DEFINE_PRIMITIVE("regexp-quote", regexp_quote, subr1, (SCM str))
{
  int len;
  char *s, *t, *end;
  SCM z;

  if (!STRINGP(str)) error_bad_string(str);

  end = STRING_CHARS(str) + STRING_SIZE(str);

  /* compute length of new string */
  for (s = STRING_CHARS(str), len=0; s < end; s++, len++)
    if (strchr(REGEXP_SPECIALS, *s)) len++;

  if (len > STRING_SIZE(str)) {
    /* make new string */
    z = STk_makestring(len, NULL);
    for (s = STRING_CHARS(str), t = STRING_CHARS(z);
     s < end;
     s++, t++) {
      if (strchr(REGEXP_SPECIALS, *s)) *t++ = '\\';
      *t = *s;
    }
    return z;
  } else {
    /* return the original string */
    return str;
  }
}

/*===========================================================================*\
 *
 *              Initialization
 *
\*===========================================================================*/

/* The stucture which describes the regexp type */
static struct extended_type_descr xtype_regexp = {
  "regexp",         /* name */
  print_regexp          /* print function */
};


int STk_init_regexp(void)
{
  DEFINE_XTYPE(regexp,  &xtype_regexp);

  ADD_PRIMITIVE(str2regexp);
  ADD_PRIMITIVE(regmatch);
  ADD_PRIMITIVE(regmatch_pos);
  ADD_PRIMITIVE(regexpp);
  ADD_PRIMITIVE(regexp_quote);
  return TRUE;
}
