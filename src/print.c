/*
 * p r i n t . c                                -- writing stuff
 *
 * Copyright © 1993-2021 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: ??-Oct-1993 ??:??
 * Last file update:  1-Sep-2021 15:26 (eg)
 *
 */
#include <ctype.h>
#include "stklos.h"

static int pretty_quotes = 1;


static void printlist(SCM exp, SCM port, int mode)
{
  register SCM tmp;
  char *s;

  if (pretty_quotes) {
    /* Special case for pretty printing of quoted expressions */
    s = STk_quote2str(CAR(exp));
    if (s && !NULLP(CDR(exp)) && NULLP(CDR(CDR(exp)))) {
      STk_puts(s, port);
      STk_print(CAR(CDR(exp)), port, mode);
      return;
    }
  }

  STk_putc('(', port);
  STk_print(CAR(exp), port, mode);

  for (tmp=CDR(exp); CONSP(tmp); tmp=CDR(tmp)) {
    STk_putc(' ', port);
    STk_print(CAR(tmp), port, mode);
  }
  if (!NULLP(tmp)) {
    STk_nputs(port, " . ", 3);
    STk_print(tmp, port, mode);
  }
  STk_putc(')', port);
}


static Inline void printsymbol(SCM symb, SCM port, int mode)
{
  char *s = SYMBOL_PNAME(symb);

  if ((mode==WRT_MODE) &&
      ((BOXED_INFO(symb) & SYMBOL_NEEDS_BARS) ||
       ((!PORT_CASE_SENSITIVEP(port)) && (BOXED_INFO(symb) & SYMBOL_HAS_UPPER)))) {
    STk_putc('|', port);
    for ( ; *s; s++ ) {
      int c = 0;
      switch (*s) {
        case '\a' : c = 'a';  break;
        case '\b' : c = 'b';  break;
        case '\f' : c = 'f';  break;
        case '\n' : c = 'n';  break;
        case '\r' : c = 'r';  break;
        case '\t' : c = 't';  break;
        case '\v' : c = 'v';  break;
        case '|'  : c = '|';  break;
        case '\\' : c = '\\'; break;
      }
      if (c) {
        STk_putc('\\', port);
        STk_putc(c, port);
      }
      else 
        STk_putc(*s, port);
   }
    STk_putc('|', port);
  } else
    STk_puts(*s ? s: "||", port); /* print bars around the "null" symbol */
}

static Inline void printkeyword(SCM key, SCM port, int mode)
{
  char *s = KEYWORD_PNAME(key);

  if (mode==WRT_MODE) {
    if ((BOXED_INFO(key) & SYMBOL_NEEDS_BARS) ||
        ((!PORT_CASE_SENSITIVEP(port)) && (BOXED_INFO(key) & SYMBOL_HAS_UPPER))) {
      STk_nputs(port, "#:|", 3);  STk_puts(s, port); STk_putc('|', port);
      return;
    }
    STk_nputs(port, "#:", 2);
  }
  STk_puts(s, port);
}


static Inline char printhexa(int x)
{
  return (x >= 10) ? (x - 10 + 'a') : (x + '0');
}


static void printstring(SCM s, SCM port, int mode)
{
  if (mode == DSP_MODE) {
    STk_putstring(s, port);
  } else {
    register char *p    = STRING_CHARS(s);
    register size_t len = STRING_SIZE(s);
    char buffer[MAX_TOKEN_SIZE], *buff = buffer;

    *buff++ = '"';
    for (   ; len; len--, p++) {
      if (buff >= buffer + MAX_TOKEN_SIZE - 7) { /* 7 because we can add \X" and */
        /* buffer is full. Flush it */           /* a null char at this positon  */
        *buff = '\0';
        STk_puts(buffer, port);
        buff = buffer;
      }

      switch (*p) {
        case '\0' : *buff++ = '\\'; *buff++ = '0'; break;
        case '\a' : *buff++ = '\\'; *buff++ = 'a'; break;
        case '\b' : *buff++ = '\\'; *buff++ = 'b'; break;
        case '\f' : *buff++ = '\\'; *buff++ = 'f'; break;
        case '\n' : *buff++ = '\\'; *buff++ = 'n'; break;
        case '\r' : *buff++ = '\\'; *buff++ = 'r'; break;
        case '\t' : *buff++ = '\\'; *buff++ = 't'; break;
        case '\v' : *buff++ = '\\'; *buff++ = 'v'; break;
        case '"'  :
        case '\\' : *buff++ = '\\'; *buff++ = *p;  break;
        default   : {
                      int printable;

                      if (STk_use_utf8)
                        printable =
                          (((unsigned) *p) >= (unsigned) ' ');
                      else
                        printable =
                          ((((unsigned char) *p) & 0177) >= (unsigned char) ' ');

                      if (printable)
                        *buff++ = *p;
                      else {
                        /* Non printable char. (It works only for char < 0xFF !!) */
                        *buff++ = '\\';
                        *buff++ = 'x';
                        *buff++ = printhexa((unsigned char) *p / 16);
                        *buff++ = printhexa((unsigned char) *p % 16);
                        *buff++ = ';';
                      }
                    }
      }
    }
    *buff++ = '"';
    *buff   = '\0';
    STk_puts(buffer, port);
  }
}



void STk_print(SCM exp, SCM port, int mode)
{
  char buffer[512]; /* for small results */

  if (SCONSTP(exp)) {
    /* Expression is a small constant */
    switch (AS_LONG(exp)) {
      case AS_LONG(STk_nil):   STk_nputs(port, "()", 2);                return;
      case AS_LONG(STk_false): STk_nputs(port, "#f", 2);                return;
      case AS_LONG(STk_true):  STk_nputs(port, "#t", 2);                return;
      case AS_LONG(STk_eof):   STk_nputs(port, "#eof", 4);              return;
      case AS_LONG(STk_void):  STk_nputs(port, "#void", 5);             return;
      default:                 STk_panic("Bad small constant %d", exp); return;
    }
  }

  if (INTP(exp)) {
    int len = snprintf(buffer, sizeof(buffer), "%ld", INT_VAL(exp));
    STk_nputs(port, buffer, len);
    return;
  }

  if (CHARACTERP(exp)) {
    char buffer[5] = {0};
    int c = CHARACTER_VAL(exp);

    if (mode!=DSP_MODE){
      char *s = STk_char2string(c);

      STk_puts("#\\", port);
      if (s)
        STk_puts(s, port);
      else
        if (c < 0x80)
          STk_putc(c, port);
        else {
          STk_char2utf8(c, buffer);
          STk_puts((char *) buffer, port);
        }
    }
    else {
      STk_char2utf8(c, buffer);
      STk_puts((char *) buffer, port);
    }
    return;
  }

  switch (BOXED_TYPE(exp)) {
    case tc_cons:
      printlist(exp, port, mode);
      return;
    case tc_real:
      STk_double2Cstr(buffer, sizeof(buffer), REAL_VAL(exp));
      STk_puts(buffer, port);
      return;
    case tc_symbol:
      printsymbol(exp, port, mode);
      return;
    case tc_keyword:
      printkeyword(exp, port, mode);
      return;
    case tc_string:
      printstring(exp, port, mode);
      return;
    case tc_box:
      if (BOX_ARITY(exp) == 1) {
        STk_putc('#', port);
        STk_putc('&', port);
        STk_print_star(*BOX_VALUES(exp), port, mode);
      }
      else {
        snprintf(buffer, sizeof(buffer),
                 "#[box (%d) %lx]", BOX_ARITY(exp), (unsigned long) exp);
        STk_puts(buffer, port);
      }
      return;
    case tc_pointer:
      if (CPOINTER_TYPE(exp) == STk_void) {
        snprintf(buffer, sizeof(buffer), "#[C-pointer %lx @ %lx]",
                (unsigned long) CPOINTER_VALUE(exp), (unsigned long) exp);
      } else {
        STk_puts("#[", port);
        STk_print(CPOINTER_TYPE(exp), port, mode);
        snprintf(buffer, sizeof(buffer), "-pointer %lx @ %lx]", (unsigned long) CPOINTER_VALUE(exp),
                (unsigned long) exp);
      }
      STk_puts(buffer, port);
      return;
    case tc_subr0:       /* ==================> Utiliser un type étendu //FIXME */
    case tc_subr1:
    case tc_subr2:
    case tc_subr3:
    case tc_subr4:
    case tc_subr5:
    case tc_subr01:
    case tc_subr12:
    case tc_subr23:
    case tc_vsubr:
    case tc_apply:
      STk_puts("#[primitive ", port);
      STk_puts(PRIMITIVE_NAME(exp), port);
      STk_putc(']', port);
      return;
#ifdef HAVE_FFI
    case tc_ext_func:
      STk_puts("#[external-func ", port);
      STk_puts(STRING_CHARS(STk_ext_func_name(exp)), port);
      STk_putc(']', port);
      return;
    case tc_callback:
      snprintf(buffer, sizeof(buffer), "#[callback %lx]", (unsigned long) exp);
      STk_puts(buffer, port);
      return;
#endif
    default:
      {
        struct extended_type_descr *xdescr = BOXED_XTYPE(exp);

        if (xdescr) {
          void (*p)() = XTYPE_PRINT(xdescr);

          if (p)
            /* Use the defined function */
            p(exp, port, mode);
          else {
            /* No print function. Try to display something useful */
            snprintf(buffer, sizeof(buffer),
                     "#[%s %lx]", XTYPE_NAME(xdescr), (unsigned long) exp);
            STk_puts(buffer, port);
          }
        }
        else
          STk_panic("no extended type descriptor for %d", BOXED_TYPE(exp));
      }
  }
}



/*=============================================================================
 *
 *                      Printing of circular structures
 *
 *=============================================================================*/
#include "hash.h"

typedef struct {
  SCM seen;
  int label;
} cycles;

static void pass1(SCM exp, cycles *c);                     /* pass 1: mark cells */
static void pass2(SCM exp, SCM port, int mode, cycles *c); /* pass 2: print      */


static void print_cycle(SCM exp, SCM port, int mode, cycles *c)
{
  SCM value;

  value =  STk_hash_ref_default(c->seen, exp, STk_void);
  if (INTP(value)) {
    // value= CDR(tmp);
    STk_fprintf(port, "#%ld#", INT_VAL(value));
    return;
  }
  /* This is not a cycle. Do a normal print */
  pass2(exp, port, mode, c);
}


static void printlist_star(SCM exp, SCM port, int mode, cycles *c)
{
  SCM value;
  char *s;

  if (pretty_quotes) {
    /* Special case for pretty printing of quoted expressions */
    s = STk_quote2str(CAR(exp));
    if (s && !NULLP(CDR(exp)) && CONSP(CDR(exp)) && NULLP(CDR(CDR(exp)))) {
      STk_puts(s, port);
      print_cycle(CAR(CDR(exp)), port, mode, c);
      return;
    }
  }

  STk_putc('(', port);

  for ( ; ; ) {
    print_cycle(CAR(exp), port, mode, c);

    if (NULLP(exp=CDR(exp))) break;

    value = STk_hash_ref_default(c->seen, exp, STk_false);
    if (!CONSP(exp) || value != STk_false) { /* value is #t or an integer */
        /* either  ". X" or ". #0=(...)" or ". #0#" */
        STk_nputs(port, " . ", 3);
        print_cycle(exp, port, mode, c);
        break;
    }
    STk_putc(' ', port);
  }
  STk_putc(')', port);
}


static void printvector_star(SCM exp, SCM port, int mode, cycles *c)
{
  int j, n = VECTOR_SIZE(exp);

  STk_nputs(port, "#(", 2);
  for(j=0; j < n; j++) {
    print_cycle(VECTOR_DATA(exp)[j], port, mode, c);
    if ((j + 1) < n) STk_putc(' ', port);
  }
  STk_putc(')', port);
}


static void pass1(SCM exp, cycles *c)
{
Top:
  if (!CONSP(exp) && !VECTORP(exp)) return;

  if ((STk_hash_ref_default(c->seen, exp, STk_void)) == STk_void) {
    /* We have never seen this cell so far */
    STk_hash_set(c->seen, exp, STk_false);

    if (CONSP(exp)) {                   /* it's a cons */
      pass1(CAR(exp), c);
      exp = CDR(exp);
      goto Top;
    }
    else {                              /* it's a vector */
      int i, len = VECTOR_SIZE(exp)-1;
      for (i = 0; i < len; i++) pass1(VECTOR_DATA(exp)[i], c);
      if (len >= 0) {exp = VECTOR_DATA(exp)[len]; goto Top;}
    }
  }
  else {
    /* This item was already seen. Note that this is the second time */
    STk_hash_set(c->seen, exp, STk_true);
  }
}


static void pass2(SCM exp, SCM port, int mode, cycles *c)
{
  if (!CONSP(exp) && !VECTORP(exp))
    STk_print(exp, port, mode);     /* Normal print */
  else {
    /* Eventually print a definition label */
    if (STk_hash_ref_default(c->seen, exp, STk_void) == STk_true) {
      /* First use of this label. Assign it a value */
      STk_fprintf(port, "#%d=", c->label);
      STk_hash_set(c->seen, exp, MAKE_INT(c->label++));
    }

    if (CONSP(exp)) printlist_star(exp, port, mode, c);
    else            printvector_star(exp, port, mode, c);
  }
}


void STk_print_star(SCM exp, SCM port, int mode)
{
  cycles c;

  if (!CONSP(exp) &&  !VECTORP(exp)) {
      STk_print(exp, port, mode);
      return;
  }

  /* Initialize the cycle structure */
  c.seen = STk_make_basic_hash_table();
  c.label = 0;

  pass1(exp, &c);
  pass2(exp, port, mode, &c);
}

/*
<doc EXT write-pretty-quotes
 * (write-pretty-quotes)
 * (write-pretty-quotes value)
 *
 * This parameter object permits to change the default behaviour of
 * the |display| or |write| primitives when they write a list which starts with
 *  the symbol quote,  quasiquote, unquote or unquote-splicing. If this parameter
 * has a false value, the writer uses the list notation instead of a
 * more human-readable value.
 * By default, this parameter value is set to |#t|.
 * @lisp
 * (let ((x ''a))
 *   (display x)
 *   (display " ")
 *   (write-pretty-quotes #f)
 *   (display x))               @print 'a (quote a)
 * @end lisp
doc>
*/
static SCM write_pretty_quotes_conv(SCM value)
{
  pretty_quotes = (value != STk_false);
  return MAKE_BOOLEAN(pretty_quotes);
}

/*===========================================================================*\
 *
 *                      I n i t i a l i z a t i o n
 *
\*===========================================================================*/
int STk_init_printer(void)
{
  STk_make_C_parameter("write-pretty-quotes",
                       MAKE_BOOLEAN(pretty_quotes),
                       write_pretty_quotes_conv,
                       STk_STklos_module);
  return TRUE;
}
