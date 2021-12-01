/*                                                      -*- coding: utf-8 -*-
 *
 * b o x . c                            -- The box type
 *
 * Copyright © 2007-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 * Last file update:  4-Sep-2020 14:56 (eg)
 */

#include "stklos.h"

static void error_bad_box(SCM obj)
{
  STk_error("bad box object ~S", obj);
}

static void error_bad_index(SCM index)
{
  STk_error("index ~S is invalid or out of bounds", index);
}

SCM STk_make_box(SCM obj)   /* A fast path for box with one value */
{
  SCM z;

  NEWCELL(z, box);
  BOX_ARITY(z) = 1;
  *BOX_VALUES(z) = obj;
  return z;
}


/*
<doc EXT make-box box
 * (box value ...)
 * (make-box value ...)
 *
 * Returns a new box that contains all the given |value|s.
 * The box is mutable.
 * @lisp
 * (let ((x (box 10)))
 *   (list 10 x))        => (10 #&10)
 * @end lisp
 *
 * The name |make-box| is now obsolete and kept only for compatibility.
doc>
*/
DEFINE_PRIMITIVE("box", box, vsubr, (int argc, SCM *argv))
{
  SCM z;
  int array_size = argc * sizeof(SCM);

  NEWCELL_WITH_LEN(z,
                   box,
                   sizeof(struct box_obj) +
                   array_size - 1); /* -1 since box_obj already contains 1 element */

  BOX_ARITY(z) = argc;
  /* arguments are in argv[-n], ..., argv[1], argv[0]. Keep it in this order */
  memcpy(BOX_VALUES(z), argv-argc+1, argc * sizeof(SCM *));

  return z;
}


/*
<doc EXT make-constant-box constant-box
 * (constant-box value ...)
 * (make-constant-box value ...)
 *
 * Returns a new box that contains all the given |value|s. The box is immutable.
 *
 * The name |make-constant-box| is now obsolete and kept only for compatibility.
doc>
*/
DEFINE_PRIMITIVE("constant-box", cbox, vsubr, (int argc, SCM *argv))
{
  SCM z = STk_box(argc, argv);
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
 * Returns the values currently in |box|.
doc>
*/
DEFINE_PRIMITIVE("unbox", unbox, subr1, (SCM x))
{
  int arity;

  if (! BOXP(x)) error_bad_box(x);
  arity = BOX_ARITY(x);
  return (arity == 1) ?
             *BOX_VALUES(x) :
             STk_values(arity, BOX_VALUES(x) + arity - 1);
}


/*
<doc EXT  box-set! set-box!
 * (set-box! box value ...)
 * (box-set! box value ...)
 *
 * Changes |box| to hold |value|s. It is an error if |set-box!| is called
 * with a number of values that differs from the number of values in the box
 * being set. (In other words, |set-box!| does not allocate memory.)
 * It is also an error to call |set-box!| on a box which is not mutable.
 *
 * The name |box-set!| is now obsolete and kept only for compatibility.
doc>
*/
DEFINE_PRIMITIVE("set-box!", set_box, vsubr, (int argc, SCM *argv))
{
  SCM x;

  if (argc == 0) STk_error("needs at least one argument");

  x = *argv--; argc--;
  if (!BOXP(x)) error_bad_box(x);
  if (BOXED_INFO(x) & BOX_CONST) STk_error("box is not mutable ~S", x);
  if (BOX_ARITY(x) != argc)
    STk_error("number of values required is %d (given: %d)", BOX_ARITY(x), argc);

  memcpy(BOX_VALUES(x), argv-argc+1, argc * sizeof(SCM *));
  return STk_void;
}

/*
<doc EXT box-arity
 * (box-arity box)
 *
 * Returns the number of values in |box|.
doc>
*/
DEFINE_PRIMITIVE("box-arity", box_arity, subr1, (SCM x))
{
  if (!BOXP(x)) error_bad_box(x);
  return MAKE_INT(BOX_ARITY(x));
}

/*
<doc EXT unbox-value
 * (unbox-value box i)
 *
 * Returns the |i|th value of |box|. It is an error if |i| is not an exact integer
 * between 0 and |n|-1, when |n| is the number of values in |box|.
doc>
*/
DEFINE_PRIMITIVE("unbox-value", unbox_value, subr2, (SCM x, SCM idx))
{
  long index = STk_integer_value(idx);

  if (!BOXP(x)) error_bad_box(x);
  if ((index < 0) || (index > BOX_ARITY(x) -1)) error_bad_index(idx);
  return BOX_VALUES(x)[BOX_ARITY(x) - index -1];
}

/*
<doc EXT set-box-value!
 * (set-box-value! box i obj)
 *
 * Changes the |i|th value of |box| to |obj|. It is an error if |i| is not an
 * exact integer between 0 and |n|-1, when |n| is the number of values in |box|.
doc>
*/
DEFINE_PRIMITIVE("set-box-value!", set_box_value, subr3, (SCM x, SCM idx, SCM obj))
{
  long index = STk_integer_value(idx);

  if (!BOXP(x)) error_bad_box(x);
  if ((index < 0) || (index > BOX_ARITY(x) -1)) error_bad_index(idx);
  BOX_VALUES(x)[BOX_ARITY(x) - index -1] = obj;
  return STk_void;
}

int STk_init_box(void)
{
  ADD_PRIMITIVE(box);
  ADD_PRIMITIVE(cbox);
  ADD_PRIMITIVE(boxp);
  ADD_PRIMITIVE(box_mutablep);
  ADD_PRIMITIVE(unbox);
  ADD_PRIMITIVE(set_box);
  ADD_PRIMITIVE(box_arity);
  ADD_PRIMITIVE(unbox_value);
  ADD_PRIMITIVE(set_box_value);
  return TRUE;
}
