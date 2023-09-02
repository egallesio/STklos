/*                                                      -*- coding: utf-8 -*-
 *
 * c p o i n t e r . c          -- Pointers on C objects
 *
 * Copyright © 2007-2022 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
<doc EXT cpointer?
 * (cpointer? obj)
 *
 * Returns `#t` is |obj| is a cpointer (a Scheme object which encapsulate
 * a pointer to a C object), and `#f` otherwise.
doc>
 */
DEFINE_PRIMITIVE("cpointer?", cpointerp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(CPOINTERP(obj));
}


/*
<doc EXT cpointer-null?
 * (cpointer-null? obj)
 *
 * Returns `#t` is |obj| is a cpointer and its value is the C NULL value.
 * Returnd `#f` otherwise.
doc>
 */
DEFINE_PRIMITIVE("cpointer-null?", cpointer_nullp, subr1, (SCM obj))
{
  if (! CPOINTERP(obj)) error_bad_cpointer(obj);
  return MAKE_BOOLEAN(CPOINTER_VALUE(obj) == NULL);
}

/*
<doc EXT cpointer-type cpointer-type-set!
 * (cpointer-type obj)
 * (cpointer-type-set! obj tag)
 *
 * |cpointer-type| returns the tag type associated to a cpointer. The C
 * runtime or an extension can associate * a tag to a cpointer to make
 *  some controls  (for instance, verify that |obj| is a cpointer  on a
 * `widget` structure). This function returns *void* if a type has not
 * been set before. The semantic associated to this tag is completely
 * left to the extension writer.
 *
 * |cpointer-type-set!| permits to set the tag of the |obj| cpointer to
 * |tag| (which can be of any type).
doc>
 */
DEFINE_PRIMITIVE("cpointer-type", cpointer_type, subr1, (SCM obj))
{
  if (! CPOINTERP(obj)) error_bad_cpointer(obj);
  return CPOINTER_TYPE(obj);
}


DEFINE_PRIMITIVE("cpointer-type-set!", cpointer_type_set, subr2, (SCM obj, SCM val))
{
  if (! CPOINTERP(obj)) error_bad_cpointer(obj);
  CPOINTER_TYPE(obj) = val;
  return STk_void;
}


/*
<doc EXT cpointer-data cpointer-data-set!
 * (cpointer-data obj)
 * (cpointer-data-set! obj adr)
 *
 * |cpointer-data| returns the value associated to cpointer |obj|
 * (that is the value of the pointer itself: an address).
 *
 * |cpointer-data-set!| permits to change the pointer stored in the |obj|
 * cpointer to |adr|. This is of course very dangerous and could lead to
 * fatal errors.
doc>
 */
DEFINE_PRIMITIVE("cpointer-data", cpointer_data, subr1, (SCM obj))
{
  if (! CPOINTERP(obj)) error_bad_cpointer(obj);
  return CPOINTER_DATA(obj);
}


DEFINE_PRIMITIVE("cpointer-data-set!", cpointer_data_set, subr2, (SCM obj, SCM val))
{
  if (! CPOINTERP(obj)) error_bad_cpointer(obj);
  CPOINTER_DATA(obj) = val;
  return STk_void;
}

/*
<doc EXT cpointer->string
 * (cpointer->string str)
 *
 * Returns the C (null terminated) string |str| as a Scheme string.
 * If |str| doesn't contain a C string, the result will probably result
 * in a fatal error.
 *
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
/*
<doc EXT allocate-bytes
 * (allocate-bytes n)
 *
 * |Allocate-bytes| will allocate |n| consecutive bytes using
 * the standard STklos allocation function (which uses the
 * Boehm–Demers–Weiser garbage collector <<BoehmGC>>).
 * It returns a |cpointer| Scheme object that points to the first byte
 * allocated. This pointer is managed by the  standard GC and doesn't need
 * to be freed.
doc>
 */
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
 * |Obj| must be a |cpointer| to allocated data. When |Free-bytes| is called
 * on |obj|, it will deallocate its data calling
 * - the C function |free| (if it  was allocated by the standard C |malloc|
 *   function), or
 * - the Boehm GC free  function (if the pointer was allocated using
 *   |allocate-bytes| primitive).
 *
 * @lisp
 * (define a (allocate-bytes 10))
 * a   => #[C-pointer 7fd91e8e0f80 @ 7fd91e897b70]
 * (cpointer-type-set! a 'myadress)
 * a   => #[myadress-pointer 7fd91e8e0f80 @ 7fd91e897b70]
 * (free-bytes a)
 * a   => #[myadress-pointer 0 @ 7fd91e897b70]
 * @end lisp
 *
 * After the call to |free-bytes|, when |a| is printed, the first number
 * shown is zero, indicating that its data pointer does not point to
 * allocated memory (a NULL value for C).
doc>
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
  ADD_PRIMITIVE(cpointer_nullp);
  ADD_PRIMITIVE(cpointer_data);
  ADD_PRIMITIVE(cpointer_type);
  ADD_PRIMITIVE(cpointer_data_set);
  ADD_PRIMITIVE(cpointer_type_set);
  ADD_PRIMITIVE(cpointer2string);

  ADD_PRIMITIVE(allocate_bytes);
  ADD_PRIMITIVE(free_bytes);
  return TRUE;
}
