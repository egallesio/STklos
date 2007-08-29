/*
 * cpointer.c	-- Pointers on C objects
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
 *    Creation date: 28-Aug-2007 14:35 (eg)
 * Last file update: 29-Aug-2007 12:35 (eg)
 */

#include <stklos.h>

static void error_bad_cpointer(SCM obj)
{
  STk_error("bad C pointer object ~S", obj);
}


SCM STk_make_Cpointer(void *ptr, SCM type, SCM data)
{
  SCM z; 
  
  NEWCELL(z, pointer);
  CPOINTER_VALUE(z) = ptr;
  CPOINTER_TYPE(z)  = type;
  CPOINTER_DATA(z)  = type;

  return z;
}

DEFINE_PRIMITIVE("cpointer-type", cpointer_type, subr1, (SCM obj))
{
  if (! CPOINTERP(obj)) error_bad_cpointer(obj);
  return CPOINTER_TYPE(obj);
}

DEFINE_PRIMITIVE("cpointer-data", cpointer_data, subr1, (SCM obj))
{
  if (! CPOINTERP(obj)) error_bad_cpointer(obj);
  return CPOINTER_DATA(obj);
}

DEFINE_PRIMITIVE("cpointer-type-set!", cpointer_type_set, subr2, (SCM obj, SCM val))
{
  if (! CPOINTERP(obj)) error_bad_cpointer(obj);
  CPOINTER_TYPE(obj) = val;
  return STk_void;
}

DEFINE_PRIMITIVE("cpointer-data-set!", cpointer_data_set, subr2, (SCM obj, SCM val))
{
  if (! CPOINTERP(obj)) error_bad_cpointer(obj);
  CPOINTER_DATA(obj) = val;
  return STk_void;
}


int STk_init_cpointer(void)
{
  ADD_PRIMITIVE(cpointer_data);
  ADD_PRIMITIVE(cpointer_type);
  ADD_PRIMITIVE(cpointer_data_set);
  ADD_PRIMITIVE(cpointer_type_set);
  return TRUE;
}
