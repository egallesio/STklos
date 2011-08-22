/*							-*- coding: utf-8 -*-
 *
 * b o x . c				-- The box type
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
 *    Creation date:  9-Dec-2007 18:04 (eg)
 * Last file update:  9-Dec-2007 20:28 (eg)
 */

#include <stklos.h>

static void error_bad_box(SCM obj)
{
  STk_error("bad box object ~S", obj);
}


/*
<doc EXT make-box
 * (make-box obj)
 *
 * Returns a new box that contains |obj|. The box is mutable.
 * @lisp
 * (let ((x (make-box 10)))
 *   (list 10 x))        => (10 #&10)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("make-box", make_box, subr1, (SCM x))
{
  SCM z;

  NEWCELL(z, box);
  BOX_VALUE(z) = x;
  return z;
}


/*
<doc EXT make-constant-box
 * (make-constant-box obj)
 *
 * Returns a new box that contains |obj|. The box is immutable.
doc>
*/
DEFINE_PRIMITIVE("make-constant-box", make_cbox, subr1, (SCM x))
{
  SCM z;

  NEWCELL(z, box);
  BOX_VALUE(z) = x;
  BOXED_INFO(z) |= BOX_CONST;
  return z;
}


/*
<doc EXT box?
 * (box? obj)
 *
 * Returns |#t| if |obj|is box, |#f| otherwise.
doc>
*/
DEFINE_PRIMITIVE("box?", boxp, subr1, (SCM x))
{
  return MAKE_BOOLEAN(BOXP(x));
}


/*
<doc EXT box-mutable?
 * (box-mutable? obj)
 *
 * Returns |#t| if |obj|is mutable box, |#f| otherwise.
doc>
*/
DEFINE_PRIMITIVE("box-mutable?", box_mutablep, subr1, (SCM x))
{
  return MAKE_BOOLEAN(BOXP(x) && !(BOXED_INFO(x) & BOX_CONST));
}


/*
<doc EXT unbox
 * (unbox box)
 *
 * Returns the content of |box|. For any |obj|, |(unbox (make-box obj))|
 * returns |obj|.
doc>
*/
DEFINE_PRIMITIVE("unbox", unbox, subr1, (SCM x))
{
  if (! BOXP(x)) error_bad_box(x);

  return BOX_VALUE(x);
}


/*
<doc EXT box-set!
 * (box-set! box x)
 *
 * Sets the content of |box| to |x|. The box must be mutable.
doc>
*/
DEFINE_PRIMITIVE("box-set!", box_set, subr2, (SCM x, SCM val))
{
  if (!BOXP(x)) error_bad_box(x);
  if (BOXED_INFO(x) & BOX_CONST) STk_error("box is not mutable ~S", x);

  BOX_VALUE(x) = val;
  return STk_void;
}

int STk_init_box(void)
{
  ADD_PRIMITIVE(make_box);
  ADD_PRIMITIVE(make_cbox);
  ADD_PRIMITIVE(boxp);
  ADD_PRIMITIVE(box_mutablep);
  ADD_PRIMITIVE(unbox);
  ADD_PRIMITIVE(box_set);
  return TRUE;
}
