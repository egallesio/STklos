/*                                                      -*- coding: utf-8 -*-
 *
 * c p o i n t e r . c          -- Pointers on C objects
 *
 * Copyright © 2007-2025 Erick Gallesio <eg@stklos.net>
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
#include <math.h> // for isnan()


static void error_bad_cpointer(SCM obj)
{
  STk_error("bad C pointer object ~S", obj);
}

static void error_bad_offset(SCM obj)
{
  STk_error("bad offset ~S", obj);
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
 * (cpointer->string str num-bytes)
 *
 * Returns the C (null terminated) string |str| as a Scheme string.
 * If |str| doesn't contain a C string, the result will probably result
 * in a fatal error. If |num-bytes| is passed as an argument, only
 * that number of bytes is copied into the new string.
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
 *
 * (define buff (allocate-bytes 5))
 * (cpointer-set! buff :uchar #\a 0)
 * (cpointer-set! buff :uchar #\B 1)
 * (cpointer-set! buff :uchar #\c 2)
 * (cpointer-set! buff :uchar #\D 3)
 * (cpointer-set! buff :uchar #\null 4)
 *
 * (cpointer->string buff)    => "aBcD"
 * (cpointer->string buff 2)  => "aB"
 * (cpointer->string buff -1) => error
 * @end lisp
doc>
 */
SCM STk_Cstring2string_nbytes(const char *str, int len) /* Embed a C string in Scheme world  */
{
  SCM  z;

  NEWCELL(z, string);
  STRING_CHARS(z)  = STk_must_malloc_atomic(len + 1);
  STRING_SPACE(z)  = STRING_SIZE(z) = len;
  STRING_LENGTH(z) = STk_use_utf8 ? (size_t) STk_utf8_strlen(str, len): len;
  snprintf(STRING_CHARS(z), len+1, "%s", str);

  return z;
}
DEFINE_PRIMITIVE("cpointer->string",cpointer2string, subr12, (SCM p, SCM nbytes))
{
  if (!CPOINTERP(p)) error_bad_cpointer(p);

  if (nbytes) {
    if (!INTP(nbytes)) STk_error("bad integer ~S", nbytes);
    if (INT_VAL(nbytes) < 0) STk_error("negative length %d", INT_VAL(nbytes));
    return STk_Cstring2string_nbytes(CPOINTER_VALUE(p), INT_VAL(nbytes));
  } else
    return STk_Cstring2string(CPOINTER_VALUE(p));
}

/*
<doc EXT cpointer-set!
 * (cpointer-set! pointer type value)
 * (cpointer-set! pointer type value offset)
 *
 * Sets the given |value| of |type| inside |pointer|. If |offset| is not given
 * it defaults to 0. Note that, as in C, the offset is multiplied by the size of
 * |type|. It permits to access the C object as an array.
 *
 * @lisp
 * (define p (allocate-bytes 1))
 * (cpointer-set! p :uint8 42)
 * @end lisp
 *
 * @lisp
 * (define p (allocate-bytes 2))
 * (cpointer-set! p :uint8 42 0)
 * (cpointer-set! p :uint8 43 1)
 * @end lisp
 *
 * The following examples shows how we cans forge a C string in Scheme
 * @lisp
 * (let (( buff (allocate-bytes 5)))
 *   (cpointer-set! buff :uchar #\a 0)
 *   (cpointer-set! buff :uchar #\b 1)
 *   (cpointer-set! buff :uchar #\c 2)
 *   (cpointer-set! buff :uchar #\d 3)
 *   (cpointer-set! buff :uchar #\null 4) ;; convention for end of string in C
 *   (cpointer->string buff))      => "abcd"
 * @end lisp
doc>
*/
#define SET_CPTR(type, v) (*((type *) ptr+ off) = (type) v)

DEFINE_PRIMITIVE("cpointer-set!", cpointer_set, subr34,
                 (SCM pointer_obj, SCM type, SCM obj, SCM offset))
{
  long kind = STk_C_type2number(type);
  long off  = offset? STk_integer_value(offset): 0;
  void *ptr = CPOINTER_VALUE(pointer_obj);

  // kind is verified by STk_C_type2number
  if (off  == LONG_MIN) error_bad_offset(offset);
  if (!CPOINTERP(pointer_obj)) error_bad_cpointer(pointer_obj);

  switch (kind) {
    case f_void:
      STk_error("Can not set type :void");
      break;

    case f_char:
    case f_uchar:
    case f_schar:
      if (CHARACTERP(obj)) {
        int val = CHARACTER_VAL(obj);
        switch (kind) {
          case f_char:      SET_CPTR(char, val); break;
          case f_uchar:     SET_CPTR(unsigned char, val); break;
          case f_schar:     SET_CPTR(signed char, val); break;
        }
        break;
      }
      /* fallthrough */ /* to see if it's an int */
    case f_short:
    case f_ushort:
    case f_int:
    case f_uint:
    case f_long:
    case f_ulong:
    case f_longlong:
    case f_ulonglong:
    case f_int8:
    case f_uint8:
    case f_int16:
    case f_uint16:
    case f_int32:
    case f_uint32:
    case f_int64:
    case f_uint64: {
      long val = STk_integer_value(obj);
      if (val != LONG_MIN) {
        switch (kind) {
          case f_char:      SET_CPTR(char, val); break;;
          case f_schar:     SET_CPTR(signed char, val); break;
          case f_uchar:     SET_CPTR(unsigned char, val); break;
          case f_short:     SET_CPTR(short, val); break;
          case f_ushort:    SET_CPTR(unsigned short, val); break;
          case f_int:       SET_CPTR(int, val); break;
          case f_uint:      SET_CPTR(unsigned int, val); break;
          case f_long:      SET_CPTR(long, val); break;
          case f_ulong:     SET_CPTR(unsigned long, val); break;
          case f_longlong:  SET_CPTR(long long, val); break;
          case f_ulonglong: SET_CPTR(unsigned long long, val); break;
          case f_int8:      SET_CPTR(int8_t, val); break;
          case f_uint8:     SET_CPTR(uint8_t, val); break;
          case f_int16:     SET_CPTR(int16_t, val); break;
          case f_uint16:    SET_CPTR(uint16_t, val); break;
          case f_int32:     SET_CPTR(int32_t, val); break;
          case f_uint32:    SET_CPTR(uint32_t, val); break;
          case f_int64:     SET_CPTR(int64_t, val); break;
          case f_uint64:    SET_CPTR(uint64_t, val); break;
        }
      } else
        STk_error("bad integer value ~S", obj);
      break;
    }

    case f_float:
    case f_double: {
      double d = STk_number2double(obj);

      if (isnan(d))
        STk_error("number ~S cannot be converted to ~S", obj, type);
      if (kind == f_float)
        SET_CPTR(float, d);
      else
        SET_CPTR(double, d);
    }
      break;

    case f_boolean: SET_CPTR(int, (obj != STk_false)); break;

    case f_pointer:
      if (CPOINTERP(obj))
        SET_CPTR(void*, CPOINTER_VALUE(obj));
      else if (obj ==STk_void)
        SET_CPTR(void*, NULL);
      else
        STk_error("cpointer or #void expected. It was ~S", obj);
      break;

    case f_string:
      if (STRINGP(obj))
        SET_CPTR(char*, STRING_CHARS(obj));
      else if (obj ==STk_void)
        SET_CPTR(char*, NULL);
      else
        STk_error("string or #void expected. It was ~S", obj);
      break;

    case f_obj:
      STk_error("cannot set a :obj pointer. Value was ~S", obj);
      break;
  }
  return STk_void;
}


/*
<doc EXT cpointer-ref
 * (cpointer-ref pointer type)
 * (cpointer-ref pointer type offset)
 *
 * Returns value of |type| from |pointer|. If |offset| is not given
 * it defaults to 0. Note that, as in C, the offset is multiplied by the size of
 * |type|. It permits to access the C object as an array.
 *
 * @lisp
 * (define p (allocate-bytes 1))
 * (cpointer-set! p :uint8 42)
 * (cpointer-ref p :uint8)
 *       => 42
 * (cpointer-ref p :uint8 0)
 *       => 42
 *
 * (define q (allocate-bytes (* 2 (c-size-of :long))))
 * (cpointer-set! q :long 1234 0)
 * (cpointer-set! q :long 6789 1) ; address is one C "long" after
 * (cons (cpointer-ref q :long 1)
 *       (cpointer-ref q :long 0))   => (6789 . 1234)
 * @end lisp
doc>
*/
#define CPTR_REF(type) (*( (type*)ptr + off))

DEFINE_PRIMITIVE("cpointer-ref", cpointer_ref, subr23,
                 (SCM pointer_obj, SCM type, SCM offset))
{
  long kind =  STk_C_type2number(type);
  long off  = offset? STk_integer_value(offset): 0;
  void *ptr = CPOINTER_VALUE(pointer_obj);

  // kind is verified by STk_C_type2number
  if (off  == LONG_MIN) error_bad_offset(offset);
  if (!CPOINTERP(pointer_obj)) error_bad_cpointer(pointer_obj);

  switch (kind) {
    case f_void:      STk_error("can not ref type :void"); return STk_void;

    case f_char:      return MAKE_INT(CPTR_REF(char));
    case f_schar:     return MAKE_INT(CPTR_REF(signed char));
    case f_uchar:     return MAKE_INT(CPTR_REF(unsigned char));

    case f_short:     return MAKE_INT(CPTR_REF(short));
    case f_ushort:    return MAKE_INT(CPTR_REF(unsigned short));
    case f_int:       return MAKE_INT(CPTR_REF(int));
    case f_uint:      return MAKE_INT(CPTR_REF(unsigned int));
    case f_long:      return MAKE_INT(CPTR_REF(long));
    case f_ulong:     return MAKE_INT(CPTR_REF(unsigned long));
    case f_longlong:  return MAKE_INT(CPTR_REF(long long));
    case f_ulonglong: return MAKE_INT(CPTR_REF(unsigned long long));
    case f_int8:      return MAKE_INT(CPTR_REF(int8_t));
    case f_uint8:     return MAKE_INT(CPTR_REF(uint8_t));
    case f_int16:     return MAKE_INT(CPTR_REF(int16_t));
    case f_uint16:    return MAKE_INT(CPTR_REF(uint16_t));
    case f_int32:     return MAKE_INT(CPTR_REF(int32_t));
    case f_uint32:    return MAKE_INT(CPTR_REF(uint32_t));
    case f_int64:     return MAKE_INT(CPTR_REF(int64_t));
    case f_uint64:    return MAKE_INT(CPTR_REF(uint64_t));

    case f_float:     return STk_double2real(CPTR_REF(float));
    case f_double:    return STk_double2real(CPTR_REF(double));

    case f_boolean:   return MAKE_BOOLEAN(CPTR_REF(int));

    case f_pointer:   return STk_make_Cpointer(CPTR_REF(void*), STk_void, STk_false);
    case f_string:    return STk_Cstring2string(CPTR_REF(char *));

    case f_obj:     STk_error("can not ref type :obj"); return STk_void;

    default: STk_error("incorrect type: ~S", type);
  }
  return STk_void; /* for the compiler */
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

/*
<doc EXT c-size-of
 * (c-size-of type)
 *
 * |c-size-of| returns the size of a C |type|, measured in units sized as char.
 * The type is given as a keyword following the conventions described in the
 * previous table.
 *
 * @lisp
 * (c-size-of :char)               => 1
 * (= (* 2 (c-size-of :int8))
 *    (c-size-of :int16))          => #t
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("c-size-of", csizeof, subr1, (SCM type))
{
  long kind =  STk_C_type2number(type);
  int res = 0;

  switch (kind) {
    case f_char:
    case f_schar:
    case f_uchar:     res = sizeof(char); break;

    case f_short:
    case f_ushort:    res = sizeof(short); break;
    case f_int:
    case f_uint:      res = sizeof(int); break;
    case f_long:
    case f_ulong:     res = sizeof(long); break;
    case f_longlong:
    case f_ulonglong: res = sizeof(long long); break;
    case f_int8:
    case f_uint8:     res = sizeof(int8_t); break;
    case f_int16:
    case f_uint16:    res = sizeof(int16_t); break;
    case f_int32:
    case f_uint32:    res = sizeof(int32_t); break;
    case f_int64:
    case f_uint64:    res = sizeof(int64_t); break;

    case f_float:     res = sizeof(float);; break;
    case f_double:    res = sizeof(double); break;

    case f_boolean:   res = sizeof(int); break;

    case f_pointer:
    case f_string:   res = sizeof(void *); break;

    case f_void:
    case f_obj:     /* fallthrough */

    default: STk_error("cannot determine the size of ~S", type);
  }
  return MAKE_INT(res);
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
  ADD_PRIMITIVE(cpointer_set);
  ADD_PRIMITIVE(cpointer_ref);

  ADD_PRIMITIVE(allocate_bytes);
  ADD_PRIMITIVE(free_bytes);
  ADD_PRIMITIVE(csizeof);

  return TRUE;
}
