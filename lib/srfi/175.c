/*
 * 175.c   -- Implementation fo SRFI-175
 *
 * Copyright © 2020-2021 Erick Gallesio - I3S-CNRS/Polytech Nice-Sophia <eg@unice.fr>
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
 *    Creation date: 23-Jul-2020 18:41
 */

#include <stklos.h>
#include "175-incl.c"

/*
 * Refrain to use <ctype.h> and implement our own ctype function in the
 * (highly improbable) case this file is compiled on a non ASCCI machine.
 */
#define _0_            0x30
#define _9_            0x39
#define _A_            0x41
#define _Z_            0x5a
#define _a_            0x61
#define _z_            0x7a
#define _LPARENT_      0x28 /* ( */
#define _RPARENT_      0x29 /* ) */
#define _LBRACKET_     0x5b /* [ */
#define _RBRACKET_     0x5d /* ] */
#define _LBRACE_       0x7b /* { */
#define _RBRACE_       0x7d /* } */
#define _LANGLE_       0x3c /* < */
#define _RANGLE_       0x3e /* } */

static Inline int is_digit(char c) { return (_0_ <= c && c <= _9_); }
static Inline int is_upper(char c) { return (_A_ <= c && c <= _Z_); }
static Inline int is_lower(char c) { return (_a_ <= c && c <= _z_); }
static Inline int is_alpha(char c) { return ((_a_ <= c && c <= _z_) ||
                                            (_A_ <= c && c <= _Z_)); }

static Inline int is_alnum(char c) { return ((_a_ <= c && c <= _z_) ||
                                            (_A_ <= c && c <= _Z_) ||
                                            (_0_ <= c && c <= _9_)); }
static Inline int to_lower(char c) { return (_A_ <= c && c <= _Z_) ?
                                        (c - _A_ + _a_): c; }
static Inline int to_upper(char c) { return (0x61 <= c && c <= 0x7a) ?
                                        (c - _a_ + _A_): c; }




static int ensure_int(SCM x)
{
  int v =0;

  if (CHARACTERP(x))
    v = CHARACTER_VAL(x);
  else if (INTP(x))
    v = INT_VAL(x);
  else
    STk_error("bad character or codepoint ~S", x);
  return v;
}


/* ======================================================================
 * Predicates to test for ASCII vs non-ASCII objects
 * ====================================================================== */
DEFINE_PRIMITIVE("ascii-codepoint?", ascii_codepointp, subr1, (SCM x))
{
  if (INTP(x)) {
    long val = INT_VAL(x);
    if (val >=0 && val < 0x80) return STk_true;
  }
  return STk_false;
}

DEFINE_PRIMITIVE("ascii-char?", ascii_charp, subr1, (SCM x))
{ return (CHARACTERP(x) && CHARACTER_VAL(x) < 0x80) ? STk_true: STk_false; }

DEFINE_PRIMITIVE("ascii-bytevector?", ascii_bytevectorp, subr1, (SCM x))
{
  unsigned char *p, *last;

  if (!BYTEVECTORP(x)) return STk_false;

  for (p = (unsigned char *) UVECTOR_DATA(x), last = p + UVECTOR_SIZE(x);
       p < last;
       p++) {
    if (*p >= 0x80) return STk_false;
  }
  return STk_true;
}

DEFINE_PRIMITIVE("ascii-string?", ascii_stringp, subr1, (SCM x))
{
  unsigned char *p, *last;

  if (!STRINGP(x)) return STk_false;
  for (p = (unsigned char *) STRING_CHARS(x), last = p + STRING_LENGTH(x);
       p < last;
       p++) {
    if (*p >= 0x80) return STk_false;
  }
  return STk_true;
}

/* ======================================================================
 * Predicates to test for subsets of ASCII
 * ====================================================================== */
DEFINE_PRIMITIVE("ascii-control?", ascii_controlp, subr1, (SCM x))
{
  int v = ensure_int(x);
  return MAKE_BOOLEAN((0x00 <= v && v <= 0x1f) || (v == 0x7f));
}


DEFINE_PRIMITIVE("ascii-non-control?", ascii_non_controlp, subr1, (SCM x))
{
  int v = ensure_int(x);
  return MAKE_BOOLEAN(0x20 <= v && v <= 0x7e);
}

DEFINE_PRIMITIVE("ascii-whitespace?", ascii_whitespacep, subr1, (SCM x))
{
  int v = ensure_int(x);
  return MAKE_BOOLEAN((0x09 <= v && v < 0x0e) || (v == 0x20));
}

DEFINE_PRIMITIVE("ascii-space-or-tab?", ascii_space_or_tabp, subr1, (SCM x))
{
  int v = ensure_int(x);
  return  MAKE_BOOLEAN(v == 0x09 || v == 0x20);
}

DEFINE_PRIMITIVE("ascii-other-graphic?", ascii_other_graphicp, subr1, (SCM x))
{
  int v = ensure_int(x);
  return MAKE_BOOLEAN( (0x21 <= v && v <= 0x2f) ||
                       (0x3a <= v && v <= 0x40) ||
                       (0x5b <= v && v <= 0x60) ||
                       (0x7b <= v && v <= 0x7e));
}

DEFINE_PRIMITIVE("ascii-alphanumeric?", ascii_alphanumericp, subr1, (SCM x))
{ return MAKE_BOOLEAN(is_alnum(ensure_int(x))); }

/* ======================================================================
 *  Subset predicates with standard Scheme equivalents
 * ====================================================================== */
DEFINE_PRIMITIVE("ascii-alphabetic?", ascii_alphabeticp, subr1, (SCM x))
{ return MAKE_BOOLEAN(is_alpha(ensure_int(x))); }

DEFINE_PRIMITIVE("ascii-numeric?", ascii_numericp, subr1, (SCM x))
{ return MAKE_BOOLEAN(is_digit(ensure_int(x))); }

DEFINE_PRIMITIVE("ascii-upper-case?", ascii_upper_casep, subr1, (SCM x))
{ return MAKE_BOOLEAN(is_upper(ensure_int(x))); }

DEFINE_PRIMITIVE("ascii-lower-case?", ascii_lower_casep, subr1, (SCM x))
{ return MAKE_BOOLEAN(is_lower(ensure_int(x))); }

/* ======================================================================
 * Case-insensitive character comparison procedures
 * ====================================================================== */
DEFINE_PRIMITIVE("ascii-ci=?", ascii_ci_eq, subr2, (SCM x, SCM y))
{ return MAKE_BOOLEAN(to_lower(ensure_int(x)) == to_lower(ensure_int(y))); }

DEFINE_PRIMITIVE("ascii-ci<?", ascii_ci_lt, subr2, (SCM x, SCM y))
{ return MAKE_BOOLEAN(to_lower(ensure_int(x)) < to_lower(ensure_int(y))); }

DEFINE_PRIMITIVE("ascii-ci>?", ascii_ci_gt, subr2, (SCM x, SCM y))
{ return MAKE_BOOLEAN(to_lower(ensure_int(x)) > to_lower(ensure_int(y))); }

DEFINE_PRIMITIVE("ascii-ci<=?", ascii_ci_le, subr2, (SCM x, SCM y))
{ return MAKE_BOOLEAN(to_lower(ensure_int(x)) <= to_lower(ensure_int(y))); }

DEFINE_PRIMITIVE("ascii-ci>=?", ascii_ci_ge, subr2, (SCM x, SCM y))
{ return MAKE_BOOLEAN(to_lower(ensure_int(x)) >= to_lower(ensure_int(y))); }

/* ======================================================================
 * Case-insensitive string comparison procedures
 * ====================================================================== */
static void error_bad_string(SCM str)
{ STk_error("bad string ~s", str); }

static void error_bad_string_character(char chr, SCM str)
{ STk_error("bad ASCII character ~s in string ~s", chr, str); }

/* A simplified version for ASCII strings of the stringompi
 * function defined in src/str.c
 */
static int stringcompi(SCM s1, SCM s2)
{
  register char *str1, *str2;
  register int l1, l2;

  if (!STRINGP(s1)) error_bad_string(s1);
  if (!STRINGP(s2)) error_bad_string(s2);

  for (l1=STRING_SIZE(s1), str1=STRING_CHARS(s1),
       l2=STRING_SIZE(s2),str2=STRING_CHARS(s2);
       l1 && l2;
       l1--, str1++, l2--, str2++) {
    if (*(unsigned char*)str1 >= 0x80) error_bad_string_character(*str1, s1);
    if (*(unsigned char*)str2 >= 0x80) error_bad_string_character(*str2, s2);
    if (to_lower(*str1) != to_lower(*str2))
      return (to_lower(*str1) - to_lower(*str2));
  }
  /* l1 == 0 || l2 == 0 */
  return l1 ? +1 : (l2 ? -1 : 0);
}

DEFINE_PRIMITIVE("ascii-string-ci=?", ascii_streqi, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcompi(s1, s2) == 0); }

DEFINE_PRIMITIVE("ascii-string-ci<?", ascii_strlti, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcompi(s1, s2) < 0); }

DEFINE_PRIMITIVE("ascii-string-ci>?", ascii_strgti, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcompi(s1, s2) > 0); }

DEFINE_PRIMITIVE("ascii-string-ci<=?", ascii_strlei, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcompi(s1, s2) <= 0); }

DEFINE_PRIMITIVE("ascii-string-ci>=?", ascii_strgei, subr2, (SCM s1, SCM s2))
{ return MAKE_BOOLEAN(stringcompi(s1, s2) >= 0); }

/* ======================================================================
 * Case conversion procedures
 * ====================================================================== */

DEFINE_PRIMITIVE("ascii-upcase", ascii_upcase, subr1, (SCM x))
{
  char ch = to_upper(ensure_int(x));
  return (CHARACTERP(x)? MAKE_CHARACTER(ch) : MAKE_INT(ch));
}

DEFINE_PRIMITIVE("ascii-downcase", ascii_downcase, subr1, (SCM x))
{
  char ch = to_lower(ensure_int(x));
  return (CHARACTERP(x)? MAKE_CHARACTER(ch) : MAKE_INT(ch));
}


/* ======================================================================
 * Control character conversion procedures
 * ====================================================================== */
DEFINE_PRIMITIVE("ascii-control->graphic", ascii_control2graphic, subr1, (SCM x))
{
  int v = ensure_int(x);
  if (0x00 <= v && v <= 0x1f)
    return CHARACTERP(x) ? MAKE_CHARACTER(v+0x40) : MAKE_INT(v+0x40);
  if (v == 0x7f)
    return CHARACTERP(x) ? MAKE_CHARACTER(0x3f): MAKE_INT(0x3f);
  return STk_false;
}

DEFINE_PRIMITIVE("ascii-graphic->control", ascii_graphic2control, subr1, (SCM x))
{
  int v = ensure_int(x);
  if (0x40<=v  && v<=0x5f)
    return CHARACTERP(x)? MAKE_CHARACTER(v-0x40): MAKE_INT(v-0x40);
  if (v == 0x3f)
    return CHARACTERP(x)? MAKE_CHARACTER(0x7f) : MAKE_INT(0x7f);
  return STk_false;
}


/* ======================================================================
 * Bracket matching procedure
 * ====================================================================== */
DEFINE_PRIMITIVE("ascii-mirror-bracket", ascii_mirror_bracket, subr1, (SCM x))
{
  char ch;
  switch (ensure_int(x)) {
    case _LPARENT_:  ch = _RPARENT_;  break;
    case _RPARENT_:  ch = _LPARENT_;  break;
    case _LBRACKET_: ch = _RBRACKET_; break;
    case _RBRACKET_: ch = _LBRACKET_; break;
    case _LBRACE_:   ch = _RBRACE_;   break;
    case _RBRACE_:   ch = _LBRACE_;   break;
    case _LANGLE_:   ch = _RANGLE_;   break;
    case _RANGLE_:   ch = _LANGLE_;   break;
    default: return STk_false;
  }
  return MAKE_CHARACTER(ch);
}

/* ======================================================================
 * Transformation procedures
 * ====================================================================== */
extern SCM STk_modulo(SCM n1, SCM n2);

static SCM ascii_nth(SCM x, int max, char base)
{
  if (INTP(x)) {
    int v;
    if (base == 9)
      v = INT_VAL(x);
    else
      v = INT_VAL(STk_modulo(x, MAKE_INT(26)));

    if (base != 9) /* for letters x is modulo 26 */
      v %= 26;
    if (0 <= v && v <= max)
      return MAKE_CHARACTER(v + base);
  }
  return STk_false;
}

static void error_bad_integer(SCM value)
{
  STk_error("bad intger ~s", value);
}


DEFINE_PRIMITIVE("ascii-nth-digit", ascii_nth_digit, subr1, (SCM x))
{ return ascii_nth(x, 9, _0_); }

DEFINE_PRIMITIVE("ascii-nth-upper-case", ascii_nth_upper_case, subr1, (SCM x))
{ return ascii_nth(x, 25, _A_); }

DEFINE_PRIMITIVE("ascii-nth-lower-case", ascii_nth_lower_case, subr1, (SCM x))
{ return ascii_nth(x, 25, _a_); }

DEFINE_PRIMITIVE("ascii-digit-value", ascii_digit_value, subr2, (SCM x, SCM limit))
{
  if (!INTP(limit))
    error_bad_integer(limit);
  else {
    int v = ensure_int(x);
    if (is_digit(v)) {
      int o = v - _0_;
      if (o < INT_VAL(limit))
        return MAKE_INT(o);
    }
  }
  return STk_false;
}

DEFINE_PRIMITIVE("ascii-upper-case-value", ascii_up_value, subr3, (SCM x, SCM off, SCM lim))
{
  if (!INTP(off)) error_bad_integer(off);
  if (!INTP(lim)) error_bad_integer(lim);
  {
    int v = ensure_int(x);
    if (is_upper(v)) {
      int o = v - _A_;
      if (o < INT_VAL(lim))
        return MAKE_INT(o + INT_VAL(off));
    }
  }
  return STk_false;
}

DEFINE_PRIMITIVE("ascii-lower-case-value", ascii_low_value, subr3, (SCM x, SCM off, SCM lim))
{
  if (!INTP(off)) error_bad_integer(off);
  if (!INTP(lim)) error_bad_integer(lim);
  {
    int v = ensure_int(x);
    if (is_lower(v)) {
      int o = v - _a_;
      if (o < INT_VAL(lim))
        return MAKE_INT(o + INT_VAL(off));
    }
  }
  return STk_false;
}


MODULE_ENTRY_START("srfi/175")
{
  SCM module =  STk_create_module(STk_intern("srfi/175"));

  /* Predicates to test for ASCII vs non-ASCII objects */
  ADD_PRIMITIVE_IN_MODULE(ascii_codepointp, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_charp, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_bytevectorp, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_stringp, module);

  /* Predicates to test for subsets of ASCII */
  ADD_PRIMITIVE_IN_MODULE(ascii_controlp, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_non_controlp, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_whitespacep, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_space_or_tabp, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_other_graphicp, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_alphanumericp, module);

  /* Subset predicates with standard Scheme equivalents */
  ADD_PRIMITIVE_IN_MODULE(ascii_alphabeticp, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_numericp, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_upper_casep, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_lower_casep, module);

  /* Case-insensitive character comparison procedures */
  ADD_PRIMITIVE_IN_MODULE(ascii_ci_eq, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_ci_lt, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_ci_gt, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_ci_le, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_ci_ge, module);

  /* Case-insensitive string comparison procedures */
  ADD_PRIMITIVE_IN_MODULE(ascii_streqi, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_strlti, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_strgti, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_strlei, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_strgei, module);

  /* Case conversion procedures */
  ADD_PRIMITIVE_IN_MODULE(ascii_upcase, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_downcase, module);

  /* Control character conversion procedures */
  ADD_PRIMITIVE_IN_MODULE(ascii_control2graphic, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_graphic2control, module);

  /* Bracket matching procedure */
  ADD_PRIMITIVE_IN_MODULE(ascii_mirror_bracket, module);

  /* Transformation procedures */
  ADD_PRIMITIVE_IN_MODULE(ascii_nth_digit, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_nth_lower_case, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_nth_upper_case, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_digit_value, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_up_value, module);
  ADD_PRIMITIVE_IN_MODULE(ascii_low_value, module);

  /* Export all the symbols we have just defined */
  STk_export_all_symbols(module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
