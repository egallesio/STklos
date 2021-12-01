/*                                                      -*- coding: utf-8 -*-
 *
 * c p o i n t e r . c          -- Pointers on C objects
 *
 * Copyright © 2007-2018 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 * Last file update: 13-Dec-2018 13:52 (eg)
 */

#include "stklos.h"

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
  CPOINTER_DATA(z)  = data;

  return z;
}

/*
<doc cpointer?
 * (cpointer? obj)
 *
 * Returns |#t| is |obj| is a cpointer (an encapsulated pointer to
 * a C object), and |#f| otherwise. Cpointers are used by the C FFI.
doc>
 */
DEFINE_PRIMITIVE("cpointer?", cpointerp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(CPOINTERP(obj));
}


DEFINE_PRIMITIVE("cpointer-type", cpointer_type, subr1, (SCM obj))
{
  if (! CPOINTERP(obj)) error_bad_cpointer(obj);
  return CPOINTER_TYPE(obj);
}

/*
<doc EXT allocate-bytes cpointer-data cpointer->string
 * (allocate-bytes n)
 * (cpointer-data obj)
 * (cpointer->string obj)
 *
 * |Allocate-bytes| will allocate |n| consecutive bytes using |malloc|,
 * and that region will consequently not be managed by the STklos GC.
 * It returns a |cpointer| Schem object that points to the frst byte
 * allocated.
 *
 * |Cpointer-data| returns the data to which the cpointer |obj| points.
 * The result is not a printable Scheme object, and if it is a string,
 * then |cpointer->string| converts it into a Scheme string.
 *
 * The use case for these procedures is the STklos C FFI:
 * @lisp
 * (define-external c-ghn ((s :pointer) (size :int))
 *                  :entry-name "gethostname"
 *                  :return-type :int)
 * (define name (allocate-bytes 10))
 * 
 * name                    => #[C-pointer 7fd830820f80 @ 7fd8305bee40]
 * (c-ghn name 9)          => 0
 * (cpointer->string name) => "socrates"
 * @end lisp
doc>
 */
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


DEFINE_PRIMITIVE("cpointer->string",cpointer2string, subr1, (SCM p))
{
  if (!CPOINTERP(p))
    error_bad_cpointer(p);

  return STk_Cstring2string(CPOINTER_VALUE(p));
}

/* ----------------------------------------------------------------------
 *      User interface allocation functions ...
 *
 * Note: System functions which use malloc can use free-bytes to release
 * memory
 *
 * ----------------------------------------------------------------------
 */
#define ALLOCATED_WITH_BOEHM_GC         0x1

DEFINE_PRIMITIVE("allocate-bytes", allocate_bytes, subr1, (SCM sz))
{
  unsigned long int size = STk_uinteger_value(sz);
  void * p;
  SCM z;

  if (size == ULONG_MAX)
    STk_error("bad size ~S", sz);

  p = STk_must_malloc(size);
  if (!p)
    STk_error("cannot allocate ~S bytes", sz);

  z = STk_make_Cpointer(p, STk_void, p);
  BOXED_INFO(z) = ALLOCATED_WITH_BOEHM_GC;

  return z;
}

/*
<doc EXT free-bytes
 * (free-bytes obj)
 *
 * |Obj| must be a |cpointer| to allocated data. When |Free-bytes| is
 * called on |obj|, it will deallocate its data (calling the C function
 * |free|).
 *
 * @lisp
 * (define a (allocate-bytes 10))
 * a   => #[C-pointer 7fd91e8e0f80 @ 7fd91e897b70]
 * (free-bytes a)
 * a   => #[C-pointer 0 @ 7fd91e897b70]
 * @end lisp
 *
 * In the second time that |a| is printed, the first number shown is zero,
 * indicating that its data pointer does not point to allocated memory.
 */
DEFINE_PRIMITIVE("free-bytes", free_bytes, subr1, (SCM p))
{
  if (!CPOINTERP(p))
    error_bad_cpointer(p);

  if (CPOINTER_VALUE(p)) {
    if (BOXED_INFO(p) == ALLOCATED_WITH_BOEHM_GC)
      STk_free(CPOINTER_VALUE(p));
    else
      free(CPOINTER_VALUE(p));

    CPOINTER_VALUE(p) = NULL;
  }

  return STk_void;
}


int STk_init_cpointer(void)
{
  ADD_PRIMITIVE(cpointerp);
  ADD_PRIMITIVE(cpointer_data);
  ADD_PRIMITIVE(cpointer_type);
  ADD_PRIMITIVE(cpointer_data_set);
  ADD_PRIMITIVE(cpointer_type_set);
  ADD_PRIMITIVE(cpointer2string);

  ADD_PRIMITIVE(allocate_bytes);
  ADD_PRIMITIVE(free_bytes);
  return TRUE;
}
