/*
 * syntax.c   -- Syntax objects
 *
 * Copyright © 2019-2023 Erick Gallesio <eg@stklos.net>
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
 *    Creation date: 25-Nov-2019 12:14
 */

#include "stklos.h"

struct syntax_obj {
  stk_header header;
  SCM name;
  SCM expander_src;
  SCM expander;
  SCM module;
};


#define SYNTAXP(p)         (BOXED_TYPE_EQ((p), tc_syntax))
#define SYNTAX_NAME(p)     (((struct syntax_obj *) (p))->name)
#define SYNTAX_SOURCE(p)   (((struct syntax_obj *) (p))->expander_src)
#define SYNTAX_EXPANDER(p) (((struct syntax_obj *) (p))->expander)
#define SYNTAX_MODULE(p)   (((struct syntax_obj *) (p))->module)


static void error_bad_syntax(SCM obj)
{
  STk_error("bad syntax object ~S", obj);
}

DEFINE_PRIMITIVE("%make-syntax", make_syntax, subr4,
                 (SCM name, SCM src, SCM expander, SCM mod))
{
  SCM z;

  // STk_debug("DEFINING syntax ~S in ~S", name, mod);

  //FIXME: Add controls on parameter types
  NEWCELL(z, syntax);
  SYNTAX_NAME(z)     = name;
  SYNTAX_SOURCE(z)   = src;
  SYNTAX_EXPANDER(z) = expander;
  SYNTAX_MODULE(z)   = mod;
  return z;
}


DEFINE_PRIMITIVE("%syntax?", syntaxp, subr1, (SCM x))
{
  return MAKE_BOOLEAN(SYNTAXP(x));
}


DEFINE_PRIMITIVE("%syntax-name", syntax_name, subr1, (SCM x))
{
  if (!SYNTAXP(x)) error_bad_syntax(x);
  return (SYMBOLP(SYNTAX_NAME(x)))? 
    STk_Cstring2string(SYMBOL_PNAME(SYNTAX_NAME(x))):
    x;
}

DEFINE_PRIMITIVE("%syntax-source", syntax_source, subr1, (SCM x))
{
  if (!SYNTAXP(x)) error_bad_syntax(x);
  return SYNTAX_SOURCE(x);
}

DEFINE_PRIMITIVE("%syntax-expander", syntax_expander, subr1, (SCM x))
{
  if (!SYNTAXP(x)) error_bad_syntax(x);
  return SYNTAX_EXPANDER(x);
}

DEFINE_PRIMITIVE("%syntax-module", syntax_module, subr1, (SCM x))
{
  if (!SYNTAXP(x)) error_bad_syntax(x);
  return SYNTAX_MODULE(x);
}


static void print_syntax(SCM syntax, SCM port, int _UNUSED(mode))
{
  STk_fprintf(port, "#[syntax %s]", SYMBOL_PNAME(SYNTAX_NAME(syntax)));
}

static struct extended_type_descr xtype_syntax = {
  .name = "syntax",
  .print = print_syntax
};


int STk_init_syntax(void)
{
  /* register the extended type type for syntaxes */
  DEFINE_XTYPE(syntax, &xtype_syntax);

  ADD_PRIMITIVE(make_syntax);
  ADD_PRIMITIVE(syntaxp);
  ADD_PRIMITIVE(syntax_name);
  ADD_PRIMITIVE(syntax_source);
  ADD_PRIMITIVE(syntax_expander);
  ADD_PRIMITIVE(syntax_module);
  
  return TRUE;
}
