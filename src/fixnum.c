/*
 * fixnum.c	-- Fixnum operations
 * 
 * Copyright © 2007 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 *           Author: Erick Gallesio [eg@essi.fr]
 *    Creation date:  9-May-2007 17:15 (eg)
 * Last file update: 10-May-2007 15:14 (eg)
 */

#include <stklos.h>

static void error_bad_fixnum(SCM obj)
{
  STk_error("bad fixnum ~S", obj);
}

DEFINE_PRIMITIVE("fixnum?", fixnump, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(INTP(obj));
}

DEFINE_PRIMITIVE("fixnum-width", fixnum_width, subr0, (void))
{
  return MAKE_INT(sizeof(long)* 8 - 2);
}

DEFINE_PRIMITIVE("least-fixnum", least_fixnum, subr0, (void))
{
  return MAKE_INT(INT_MIN_VAL);
}

DEFINE_PRIMITIVE("greatest-fixnum", greatest_fixnum, subr0, (void))
{
  return MAKE_INT(INT_MAX_VAL);
}


/*
 * fx+, fx-, fx*, fx/, fxmod
 *
 */

#define SIMPLE_OP(name, func, op) \
DEFINE_PRIMITIVE(name, func, subr2, (SCM o1, SCM o2))		\
{								\
  if (!INTP(o1)) error_bad_fixnum(o1);				\
  if (!INTP(o2)) error_bad_fixnum(o2);				\
  return MAKE_INT(INT_VAL(o1) op INT_VAL(o2));			\
}

SIMPLE_OP("fx+",   fxplus, +)
SIMPLE_OP("fx-",   fxminus, -)
SIMPLE_OP("fx*",   fxtime, *)
SIMPLE_OP("fxdiv", fxdiv, /)
SIMPLE_OP("fxmod", fxmod, %)

/*
 * fx<, fx<=, fx>, fx>=, fx=
 *
 */

#define SIMPLE_COMP(name, func, op) \
DEFINE_PRIMITIVE(name, func, subr2, (SCM o1, SCM o2))		\
{								\
  if (!INTP(o1)) error_bad_fixnum(o1);				\
  if (!INTP(o2)) error_bad_fixnum(o2);				\
  return MAKE_BOOLEAN(INT_VAL(o1) op INT_VAL(o2));		\
}

SIMPLE_COMP("fx<",  fxlt, <)
SIMPLE_COMP("fx<=", fxle, <=)
SIMPLE_COMP("fx>",  fxgt, >)
SIMPLE_COMP("fx>=", fxge, >=)
SIMPLE_COMP("fx=",  fxeq, ==)


int STk_init_fixnum(void)
{
  ADD_PRIMITIVE(fixnump);
  ADD_PRIMITIVE(fixnum_width);
  ADD_PRIMITIVE(least_fixnum);
  ADD_PRIMITIVE(greatest_fixnum);


  ADD_PRIMITIVE(fxplus);
  ADD_PRIMITIVE(fxminus);
  ADD_PRIMITIVE(fxtime);
  ADD_PRIMITIVE(fxdiv);
  ADD_PRIMITIVE(fxmod);

  ADD_PRIMITIVE(fxlt);
  ADD_PRIMITIVE(fxle);
  ADD_PRIMITIVE(fxgt);
  ADD_PRIMITIVE(fxge);
  ADD_PRIMITIVE(fxeq);

  return TRUE;
}
