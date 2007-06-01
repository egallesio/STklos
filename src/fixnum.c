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
 * Last file update:  1-Jun-2007 20:11 (eg)
 */

#include <stklos.h>

static void error_bad_fixnum(SCM obj)
{
  STk_error("bad fixnum ~S", obj);
}

static void error_division_by_0(void)
{
  STk_error("division by 0");
}


/*
<doc EXT fixnum?
 * (fixnum? obj)
 * 
 * Returns |#t| if obj is an exact integer within the fixnum range, 
 * |#f| otherwise.
doc>
*/
DEFINE_PRIMITIVE("fixnum?", fixnump, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(INTP(obj));
}

/*
<doc EXT fixnum-width
 * (fixnum-width)
 * 
 * Returns the number of bits used to represent a fixnum number
doc>
*/
DEFINE_PRIMITIVE("fixnum-width", fixnum_width, subr0, (void))
{
  return MAKE_INT(sizeof(long)* 8 - 2);
}

/*
<doc EXT least-fixnum greatest-fixnum
 * (least-fixnum)
 * (greatest-fixnum)
 * 
 * These procedures return the minimum value and the maximum value of
 * the fixnum range.
doc>
*/
DEFINE_PRIMITIVE("least-fixnum", least_fixnum, subr0, (void))
{
  return MAKE_INT(INT_MIN_VAL);
}

DEFINE_PRIMITIVE("greatest-fixnum", greatest_fixnum, subr0, (void))
{
  return MAKE_INT(INT_MAX_VAL);
}


/*
<doc EXT fx+ fx- fx* fxdiv fxrem fxmod 
 * (fx+ fx1 fx2)
 * (fx- fx1 fx2)
 * (fx* fx1 fx2)
 * (fxdiv fx1 fx2)
 * (fxrem fx1 fx2)
 * (fxmod fx1 fx2)
 * (fx- fx)
 * 
 * These procedures compute (respectively) the sum, the difference, the product, 
 * the quotient and the remainder and modulp of the fixnums |fx1| and |fx2|. 
 * The call of  |fx-| with one parameter |fx| computes the opposite of |fx|.
doc>
 */
DEFINE_PRIMITIVE("fx+", fxplus, subr2, (SCM o1, SCM o2))
{
  if (!INTP(o1)) error_bad_fixnum(o1);
  if (!INTP(o2)) error_bad_fixnum(o2);
  return MAKE_INT(INT_VAL(o1) + INT_VAL(o2));
}

DEFINE_PRIMITIVE("fx-", fxminus, subr12, (SCM o1, SCM o2))
{
  if (!INTP(o1)) error_bad_fixnum(o1);
  if (!o2) 
    return MAKE_INT(-INT_VAL(o1));
  if (!INTP(o2)) error_bad_fixnum(o2);
  return MAKE_INT(INT_VAL(o1) + INT_VAL(o2));
}

DEFINE_PRIMITIVE("fx*", fxtime, subr2, (SCM o1, SCM o2))
{
  if (!INTP(o1)) error_bad_fixnum(o1);
  if (!INTP(o2)) error_bad_fixnum(o2);
  return MAKE_INT(INT_VAL(o1) * INT_VAL(o2));
}

DEFINE_PRIMITIVE("fxdiv", fxdiv, subr2, (SCM o1, SCM o2))
{
  int n;

  if (!INTP(o1)) error_bad_fixnum(o1);
  if (!INTP(o2)) error_bad_fixnum(o2);

  n = INT_VAL(o2);
  
  if (!n) error_division_by_0();
  return MAKE_INT(INT_VAL(o1) / n);
}

DEFINE_PRIMITIVE("fxrem", fxrem, subr2, (SCM o1, SCM o2))
{
  int n;

  if (!INTP(o1)) error_bad_fixnum(o1);
  if (!INTP(o2)) error_bad_fixnum(o2);

  n = INT_VAL(o2);
  
  if (!n) error_division_by_0();
  return MAKE_INT(INT_VAL(o1) % n);
}


DEFINE_PRIMITIVE("fxmod", fxmod, subr2, (SCM o1, SCM o2))
{
  if (!INTP(o1)) error_bad_fixnum(o1);
  if (!INTP(o2)) error_bad_fixnum(o2);
  {
    int n1 = INT_VAL(o1);
    int n2 = INT_VAL(o2);
    int r;

    if (!n2) error_division_by_0();
    r = n1 % n2;

    /* (negativep(n1) != negativep(n2) && !zerop(r)) */
    if ((((n1 < 0) && (n2 >= 0)) || ((n1 >= 0) && (n2 < 0))) &&
	r)
      r += n2;
    
    return MAKE_INT(r);
  }
}

/*
<doc EXT fx< fx<= fx> fx>= fx= 
 * (fx< fx1 fx2)
 * (fx<= fx1 fx2)
 * (fx> fx1 fx2)
 * (fx>= fx1 fx2)
 * (fx= fx1 fx2)
 * 
 * These procedures compare the fixnums |fx1| and |fx2| and retun |#t| if 
 * the comparison is true and |#f| otherwise.
doc>
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
  ADD_PRIMITIVE(fxrem);

  ADD_PRIMITIVE(fxlt);
  ADD_PRIMITIVE(fxle);
  ADD_PRIMITIVE(fxgt);
  ADD_PRIMITIVE(fxge);
  ADD_PRIMITIVE(fxeq);

  return TRUE;
}
