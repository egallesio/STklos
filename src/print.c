/*
 * p r i n t . c				-- writing stuff
 *
 * Copyright © 1993-2007 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 * Last file update: 28-Jun-2007 18:21 (eg)
 *
 */
#include <ctype.h>
#include "stklos.h"


static void printlist(SCM exp, SCM port, int mode)
{
  register SCM tmp;
  char *s;

  /* Special case for pretty printing of quoted expressions */
  s = STk_quote2str(CAR(exp));
  if (s && !NULLP(CDR(exp)) && NULLP(CDR(CDR(exp)))) {
    STk_puts(s, port);
    STk_print(CAR(CDR(exp)), port, mode);
    return;
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


static void Inline printsymbol(SCM symb, SCM port, int mode)
{
  char *s = SYMBOL_PNAME(symb);
  
  if ((mode==WRT_MODE) && 
      ((BOXED_INFO(symb) & SYMBOL_NEEDS_BARS) ||
       (!STk_read_case_sensitive && (BOXED_INFO(symb) & SYMBOL_HAS_UPPER)))) {
    STk_putc('|', port);  STk_puts(s, port); STk_putc('|', port);
  } else
    STk_puts(s, port);
}

static void Inline printkeyword(SCM key, SCM port, int mode)
{
  char *s = KEYWORD_PNAME(key);

  if (mode==WRT_MODE) {
    if ((BOXED_INFO(key) & SYMBOL_NEEDS_BARS) ||
	(!STk_read_case_sensitive && (BOXED_INFO(key) & SYMBOL_HAS_UPPER))) {
      STk_nputs(port, "|:", 2);  STk_puts(s, port); STk_putc('|', port);
      return;
    }
    STk_putc(':', port);
  }
  STk_puts(s, port);
}


static char printhexa(int x)
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
	/* buffer is full. Flush it */		 /* a null char at this positon  */
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
      default   : if ((((unsigned char) *p) & 0177) < (unsigned char) ' ') {
	  	      /* Non printable character (It works only for ISO 8859-x !!) */
	  	      *buff++ = '\\';
	  	      *buff++ = 'x';
		      *buff++ = printhexa((unsigned char) *p / 16);
		      *buff++ = printhexa((unsigned char) *p % 16);
		    } 
		    else *buff++ = *p;
      }
    }
    *buff++ = '"';
    *buff   = '\0';
    STk_puts(buffer, port);
  }
}



void STk_print(SCM exp, SCM port, int mode)
{
  char buffer[100]; /* for small results */

  if (SCONSTP(exp)) {
    /* Expression is a small constant */
    switch (AS_LONG(exp)) {
      case AS_LONG(STk_nil):   STk_nputs(port, "()", 2); 	  	return;
      case AS_LONG(STk_false): STk_nputs(port, "#f", 2); 	  	return;
      case AS_LONG(STk_true):  STk_nputs(port, "#t", 2); 	  	return;
      case AS_LONG(STk_eof):   STk_nputs(port, "#eof", 4);	  	return;
      case AS_LONG(STk_void):  STk_nputs(port, "#void", 5);	 	return;
      default:		       STk_panic("Bad small constant %d", exp); return;
    }
  }
  
  if (INTP(exp)) {
    int len = sprintf(buffer, "%ld", INT_VAL(exp));
    STk_nputs(port, buffer, len);
    return;
  }

  if (CHARACTERP(exp)) {
    if (mode!=DSP_MODE){
      char *s = STk_char2string(CHARACTER_VAL(exp));

      STk_puts("#\\", port);
      if (s) 
	STk_puts(STk_char2string(CHARACTER_VAL(exp)), port);
      else
	STk_putc(CHARACTER_VAL(exp), port);
    }
    else STk_putc(CHARACTER_VAL(exp), port);
    return;
  }

  switch (BOXED_TYPE(exp)) {
    case tc_cons:
      printlist(exp, port, mode);
      return;
    case tc_real:
      STk_double2Cstr(buffer, REAL_VAL(exp));
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
    case tc_box:		/* Should never occur in user code */
      STk_putc('{', port);
      STk_print(BOX_VALUE(exp), port, mode);
      STk_putc('}', port);
      return;
    case tc_pointer:
      sprintf(buffer, "#[C-pointer %lx]", (unsigned long) CPOINTER_VALUE(exp));
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
      sprintf(buffer, "#[callback %lx]", (unsigned long) exp);
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
	    sprintf(buffer, "#[%s %lx]", XTYPE_NAME(xdescr), (unsigned long) exp);
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
 *			Printing of circular structures 
 *
 *=============================================================================*/

static SCM cycles      = STk_nil;;
static int index_label = 0;

static void pass1(SCM exp);		/* pass 1: mark cells */
static void pass2(SCM exp, SCM port);	/* pass 2: print      */


static void print_cycle(SCM exp, SCM port)
{  
  SCM value, tmp;

  if ((tmp = STk_assv(exp, cycles)) != STk_false) {
    value= CDR(tmp);
    if (INTP(value)) {
      STk_fprintf(port, "#%ld#", INT_VAL(value));
      return;
    }
  }
  /* This is not a cycle. Do a normal print */
  pass2(exp, port);
}


static void printlist_star(SCM exp, SCM port)
{
  SCM value, tmp;
  char *s;

  tmp = STk_nil;		/* for GCC */

  /* Special case for pretty printing of quoted expressions */
  s = STk_quote2str(CAR(exp));
  if (s && !NULLP(CDR(exp)) && NULLP(CDR(CDR(exp)))) {
    STk_puts(s, port);
    print_cycle(CAR(CDR(exp)), port);
    return;
  }

  STk_putc('(', port);

  for ( ; ; ) {
    print_cycle(CAR(exp), port);

    if (NULLP(exp=CDR(exp))) break;

    if (!CONSP(exp) || (tmp = STk_assv(exp, cycles)) != STk_false) {
      if (!CONSP(exp) || (value = CDR(tmp)) == STk_true || INTP(value)) { 
	/* either  ". X" or ". #0=(...)" or ". #0#" */
	STk_nputs(port, " . ", 3);
	print_cycle(exp, port);
	break;
      }
    }
    STk_putc(' ', port);
  }
  STk_putc(')', port);
}


static void printvector_star(SCM exp, SCM port)
{
  int j, n = VECTOR_SIZE(exp);
  
  STk_nputs(port, "#(", 2);
  for(j=0; j < n; j++) {
    print_cycle(VECTOR_DATA(exp)[j], port);
    if ((j + 1) < n) STk_putc(' ', port);
  }
  STk_putc(')', port);
}


static void pass1(SCM exp)
{
  SCM tmp;

Top:
  if (!CONSP(exp) && !VECTORP(exp)) return;

  if ((tmp = STk_assv(exp, cycles)) == STk_false) {
    /* We have never seen this cell so far */
    cycles = STk_cons(STk_cons(exp, STk_false), cycles);

    if (CONSP(exp)) {			/* it's a cons */
      pass1(CAR(exp));
      exp = CDR(exp); 
      goto Top;
    }
    else { 				/* it's a vector */
      int i, len = VECTOR_SIZE(exp)-1;
      for (i = 0; i < len; i++) pass1(VECTOR_DATA(exp)[i]);
      if (len >= 0) {exp = VECTOR_DATA(exp)[len]; goto Top;}
    }
  } 
  else {
    /* This item was already seen. Note that this is the second time */
    CDR(tmp) = STk_true;
  }
}


static void pass2(SCM exp, SCM port)
{
  if (!CONSP(exp) && !VECTORP(exp))
    STk_print(exp, port, WRT_MODE); 	/* Normal print */
  else {
    SCM value, tmp;

    /* Eventually print a definition label */
    if ((tmp = STk_assv(exp, cycles)) != STk_false) {
      if ((value=CDR(tmp)) == STk_true) {
	/* First use of this label. Assign it a value */
	STk_fprintf(port, "#%d=", index_label);
	CDR(tmp) = MAKE_INT(index_label++);
      }
    }

    if (CONSP(exp)) printlist_star(exp, port);
    else            printvector_star(exp, port);
  }
}

void STk_print_star(SCM exp, SCM port)
{
  MUT_DECL(lck);

  if (!CONSP(exp) &&  !VECTORP(exp)) return STk_print(exp, port, WRT_MODE);
  MUT_LOCK(lck);
  cycles      = STk_nil;
  index_label = 0;

  pass1(exp); pass2(exp, port);
  MUT_UNLOCK(lck);
}
