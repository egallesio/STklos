/*
 * u v e c t o r . c                    -- Uniform Vectors Implementation
 *
 * Copyright © 2001-2018 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 15-Apr-2001 10:13 (eg)
 * Last file update:  2-Aug-2018 23:00 (eg)
 */

#include "stklos.h"

int STk_uvectors_allowed = 0;
static SCM u64_max, s64_min, s64_max;


/*
 * Utilities
 *
 */

static char* type_vector(int tip)
{
  switch (tip) {
    case UVECT_S8:  return "s8";
    case UVECT_U8:  return "u8";
    case UVECT_S16: return "s16";
    case UVECT_U16: return "u16";
    case UVECT_S32: return "s32";
    case UVECT_U32: return "u32";
    case UVECT_S64: return "s64";
    case UVECT_U64: return "u64";
    case UVECT_F32: return "f32";
    case UVECT_F64: return "f64";
    default:        return ""; /* never reached */
  }
}


static void error_change_const_vector(SCM v)
{
  STk_error("changing the constant vector ~s is not allowed", v);
}

static void error_bad_vector(SCM v)
{
  STk_error("bad vector ~s", v);
}

static void error_bad_uvector(SCM v, int tip)
{
  STk_error("bad #%s vector ~s", type_vector(tip), v);
}

static void error_bad_index(SCM index)
{
  STk_error("index ~S is invalid or out of bounds", index);
}

static void error_bad_length(SCM length)
{
  STk_error("invalid vector length ~s", length);
}

static void error_bad_uniform_type(SCM type)
{
  STk_error("bad type for an uniform vector ~s", type);
}

static void error_bad_list(SCM l)
{
  STk_error("bad list ~s", l);
}


static int vector_element_size(int type)
{
  /* compute len of one element depending of type.  We assume here
   * that characters use 8 bits and that we are at least on a 32 bits
   * architecture. Consquenetly, S8, S16 and S32 are represented
   * without boxing whereas S64 are represeneted by a bignum
   * (even on 64 machines where we can do better). Furthermore, we
   * suppose that C floats and doubles correspond to single and
   * double IEEE-754 reals
   */
  switch (type) {
    case UVECT_S8:  case UVECT_U8:  return 1;
    case UVECT_S16: case UVECT_U16: return 2;
    case UVECT_S32: case UVECT_U32: return 4;
    case UVECT_S64: case UVECT_U64: return sizeof(SCM);
    case UVECT_F32:                 return 4;
    case UVECT_F64:                 return 8;
  }
  return 0; /* never reached */
}


static SCM control_index(int argc, SCM *argv, long *pstart, long *pend, SCM *pfill)
{
  SCM v = NULL;
  long len, start=0, end=-1;

  /* Controling number of arguments */
  if (!pfill) {
    /* We do not have a fill parameter => vect at 0, start at -1 and end at -2 */
    switch (argc) {
      case 3: end   = STk_integer_value(argv[-2]);  /* no break */
      case 2: start = STk_integer_value(argv[-1]);  /* no break */
      case 1: v = argv[0]; break;
      default: goto bad_number_of_args;
    }
  } else {
    /* We have a fill param. => vect at 0, fill at -1, start at -2 and end at -3 */
    switch (argc) {
      case 4: end   = STk_integer_value(argv[-3]);  /* no break */
      case 3: start = STk_integer_value(argv[-2]);  /* no break */
      case 2: if (pfill) *pfill  = argv[-1];
              v = argv[0];
              break;
      default:
      bad_number_of_args:
        STk_error("incorrect number of arguments (%d)", argc);
    }
  }

  /* Controlling s */
  if (!UVECTORP(v)) error_bad_uvector(v, UVECT_U8);
  len = UVECTOR_SIZE(v);

  /* Controlling start index */
  if (start == LONG_MIN || start < 0 || start > len)
    /* argc cannot be 1 (start would be 0) */
    STk_error("bad starting index ~S", argv[pfill ? -2: -1]);

  /* Controlling end index */
  if (end == -1)
    end = len;
  else
    if (end == LONG_MIN  || end < 0 || end > len)
      /* We have an end index ==> argc = 3 */
      STk_error("bad ending index ~S", argv[pfill? -3: -2]);

  if (start > end)
    STk_error("low index is greater than high index");

  /* everything is correct, return values */
  *pstart = start;
  *pend   = end;
  return v;
}


/* Return the type of an uniform vector given its tag */
int STk_uniform_vector_tag(char *s)
{
  static char *table[] =
    {"s8", "u8", "s16", "u16", "s32", "u32", "s64", "u64", "f32", "f64", "" };
  char **p;

  for (p = table; **p; p++) {
    if (strcmp(s, *p) == 0) return p - table;
  }
  return -1;
}


int STk_uvector_equal(SCM u1, SCM u2)
{
  if ((UVECTOR_TYPE(u1) != UVECTOR_TYPE(u2)) ||
      (UVECTOR_SIZE(u1) != UVECTOR_SIZE(u2)))
    return 0;

  /* same length and same tyep, compare the bytes */
  int len = vector_element_size(UVECTOR_TYPE(u1)) * UVECTOR_SIZE(u1);

  return (memcmp(UVECTOR_DATA(u1), UVECTOR_DATA(u2), len) == 0);
}

/*
 * Basic accessors to an uniform vector
 *
 */
static void uvector_set(int type, SCM v, long i, SCM value)
{
  long vali;
  int overflow;

  /* First see if the value is correct for this type of vector */
  switch (UVECTOR_TYPE(v)) {
    case UVECT_S8:
      vali = STk_integer_value(value);
      if (-128 <= vali && vali < +128) {
        ((char *) UVECTOR_DATA(v))[i] = (char) vali;
        return;
      }
      break;
    case UVECT_U8:
      vali = STk_integer_value(value);
      if (0 <= vali && vali < +256) {
        ((unsigned char *) UVECTOR_DATA(v))[i] = (unsigned char) vali;
        return;
      }
      break;
    case UVECT_S16:
      vali = STk_integer_value(value);
      if (-32768 <= vali && vali < +32768) {
        ((short *) UVECTOR_DATA(v))[i] = (short) vali;
        return;
      }
      break;
    case UVECT_U16:
      vali = STk_integer_value(value);
      if (0 <= vali && vali < 65536) {
        ((unsigned short *) UVECTOR_DATA(v))[i] = (unsigned short) vali;
        return;
      }
      break;

    case UVECT_S32:
      vali = STk_integer2int32(value, &overflow);
      if (!overflow) {
        ((int *) UVECTOR_DATA(v))[i] = (int) vali;
        return;
      }
      break;
    case UVECT_U32:
      vali = STk_integer2uint32(value, &overflow);
      if (!overflow) {
        ((unsigned int *) UVECTOR_DATA(v))[i] = (unsigned int) vali;
        return;
      }
      break;
    case UVECT_S64:
      if (INTP(value) || BIGNUMP(value))
        if (STk_numle2(MAKE_INT(0), value) && STk_numle2(value, u64_max)) {
          ((SCM *) UVECTOR_DATA(v))[i] = value;
          return;
        }
      break;
    case UVECT_U64:
      if (INTP(value) || BIGNUMP(value))
        if (STk_numle2(s64_min, value) && STk_numle2(value, s64_max)) {
          ((SCM *) UVECTOR_DATA(v))[i] = value;
          return;
        }
      break;
    case UVECT_F32:
      if (REALP(value)) {
        ((float *) UVECTOR_DATA(v))[i] = (float) REAL_VAL(value);
        return;
      }
      break;
    case UVECT_F64:
      if (REALP(value)) {
        ((double *) UVECTOR_DATA(v))[i] = (double) REAL_VAL(value);
        return;
      }
      break;
  }

  /* If we arrive here we are sure that we have a value which is out of bounds */
  STk_error("value ~S is out of bounds or incorrect for a %svector",
            value, type_vector(UVECTOR_TYPE(v)));
}

static SCM uvector_ref(int type, SCM v, long i)
{
  switch (UVECTOR_TYPE(v)) {
    case UVECT_S8: return MAKE_INT(((char *) UVECTOR_DATA(v))[i]);
    case UVECT_U8: return MAKE_INT(((unsigned char *) UVECTOR_DATA(v))[i]);

    case UVECT_S16: return MAKE_INT(((short *) UVECTOR_DATA(v))[i]);
    case UVECT_U16: return MAKE_INT(((unsigned short *) UVECTOR_DATA(v))[i]);

    case UVECT_S32: return STk_long2integer(((int *) UVECTOR_DATA(v))[i]);
    case UVECT_U32: return STk_ulong2integer(((unsigned int*) UVECTOR_DATA(v))[i]);
    case UVECT_S64: /* next one */
    case UVECT_U64: return ((SCM*)  UVECTOR_DATA(v))[i];

    case UVECT_F32: return STk_double2real(((float *) UVECTOR_DATA(v))[i]);
    case UVECT_F64: return STk_double2real(((double *) UVECTOR_DATA(v))[i]);
  }
  return STk_void; /* never reached */
}


/*
 *
 * Uniform vector constructor
 *
 */
static SCM makeuvect(int type, int len, SCM init)
{
  long i, size = 1;
  SCM  z;

  /* compute len of one element depending of type.  We assume here
   * that characters use 8 bits and that we are at least on a 32 bits
   * architecture. Consquenetly, S8, S16 and S32 are represented
   * without boxing whereas S64 are represeneted by a bignum
   * (even on 64 machines where we can do better). Furthermore, we
   * suppose that C floats and doubles correspond to single and
   * double IEEE-754 reals
   */
  switch (type) {
    case UVECT_S8:  case UVECT_U8:  size = 1;           break;
    case UVECT_S16: case UVECT_U16: size = 2;           break;
    case UVECT_S32: case UVECT_U32: size = 4;           break;
    case UVECT_S64: case UVECT_U64: size = sizeof(SCM); break;
    case UVECT_F32:                 size = 4;           break;
    case UVECT_F64:                 size = 8;           break;
  }
  NEWCELL_WITH_LEN(z, uvector, sizeof(struct vector_obj) + size*len - 1);
  UVECTOR_TYPE(z) = type;
  UVECTOR_SIZE(z) = len;

  if (init) {
    for(i=0; i < len; i++)
      uvector_set(type, z, i, init);
  }
  return z;
}

SCM STk_list2uvector(int type, SCM l)
{
  long i, len = STk_int_length(l);
  SCM z;

  if (len < 0) error_bad_list(l);

  z = makeuvect(type, len, (SCM) NULL);
  for (i = 0; i < len; i++) {
    uvector_set(type, z, i, CAR(l));
    l = CDR(l);
  }
  return z;
}

/*===========================================================================*\
 *
 * User primitives on uniform vectors.
 * All thes functions are used by the file  which implements SRFI-4
 *
\*===========================================================================*/

DEFINE_PRIMITIVE("%make-uvector", make_uvector, subr3,(SCM type, SCM len, SCM init))
{
  long l   = STk_integer_value(len);
  long tip = STk_integer_value(type);

  if (l < 0)                             error_bad_length(len);
  if (tip < UVECT_S8 || tip > UVECT_F64) error_bad_uniform_type(type);

  return makeuvect(tip, l, init);
}

DEFINE_PRIMITIVE("%uvector?", uvectorp, subr2, (SCM type, SCM vect))
{
  long tip = STk_integer_value(type);
  return MAKE_BOOLEAN(UVECTORP(vect) && UVECTOR_TYPE(vect) == tip);
}

DEFINE_PRIMITIVE("%uvector", uvector, subr2, (SCM type, SCM values))
{
  long tip = STk_integer_value(type);

  if (tip < UVECT_S8 || tip > UVECT_F64) error_bad_uniform_type(type);
  return STk_list2uvector(tip, values);
}

DEFINE_PRIMITIVE("%uvector-length", uvector_length, subr2, (SCM type, SCM v))
{
  long tip = STk_integer_value(type);

  if (tip < UVECT_S8 || tip > UVECT_F64)        error_bad_uniform_type(type);
  if (!UVECTORP(v) || (UVECTOR_TYPE(v) != tip)) error_bad_uvector(v, tip);

  return MAKE_INT(UVECTOR_SIZE(v));
}

DEFINE_PRIMITIVE("%uvector-ref", uvector_ref, subr3, (SCM type, SCM v, SCM index))
{
  long i   = STk_integer_value(index);
  long tip = STk_integer_value(type);

  if (!UVECTORP(v) || (UVECTOR_TYPE(v) != tip))  error_bad_vector(v);
  if (tip < UVECT_S8 || tip > UVECT_F64)         error_bad_uniform_type(type);
  if (i < 0 || i >= UVECTOR_SIZE(v))             error_bad_index(index);

  return uvector_ref(tip, v, i);
}

DEFINE_PRIMITIVE("%uvector-set!", uvector_set, subr4,
                 (SCM type, SCM v, SCM index, SCM value))
{
  long i   = STk_integer_value(index);
  long tip = STk_integer_value(type);

  if (!UVECTORP(v) || (UVECTOR_TYPE(v) != tip)) error_bad_vector(v);
  if (tip < UVECT_S8 || tip > UVECT_F64)        error_bad_uniform_type(type);
  if (BOXED_INFO(v) & VECTOR_CONST)             error_change_const_vector(v);
  if (i < 0 || i >= UVECTOR_SIZE(v))            error_bad_index(index);

  uvector_set(tip, v, i, value);
  return STk_void;
}


DEFINE_PRIMITIVE("%uvector->list", uvector_list, subr2, (SCM type, SCM v))
{
  long i, len, tip = STk_integer_value(type);
  SCM z, tmp;

  if (tip < UVECT_S8 || tip > UVECT_F64)        error_bad_uniform_type(type);
  if (!UVECTORP(v) || (UVECTOR_TYPE(v) != tip)) error_bad_vector(v);

  len = UVECTOR_SIZE(v);
  if (!len) return STk_nil;

  /* len > 0. Build the fist cell and iterate */
  tmp = z = STk_cons(uvector_ref(tip, v, 0), STk_nil);
  for (i=1; i<len; i++) {
    tmp = CDR(tmp) = STk_cons(uvector_ref(tip, v, i), STk_nil);
  }
  return z;
}

DEFINE_PRIMITIVE("%list->uvector", list_uvector, subr2, (SCM type, SCM l))
{
  long tip = STk_integer_value(type);

  if (tip < UVECT_S8 || tip > UVECT_F64) error_bad_uniform_type(type);
  return STk_list2uvector(tip, l);
}

DEFINE_PRIMITIVE("%allow-uvectors", allow_uvectors, subr0, (void))
{
  ADD_PRIMITIVE(uvector_list);
  ADD_PRIMITIVE(list_uvector);

  /* initialize the maxima for 64 bits values */
  u64_max = STk_Cstr2number(U64_MAX, 10);
  s64_min = STk_Cstr2number(S64_MIN, 10);
  s64_max = STk_Cstr2number(S64_MAX, 10);

  /* Retain that we can use uniform vectors from now on */
  STk_uvectors_allowed = 1;


  return STk_void;
}


/*===========================================================================*\
 *
 * Define the uniform vector type
 *
\*===========================================================================*/
static void print_uvector(SCM vect, SCM port, int mode)
{
  int i;
  int n = UVECTOR_SIZE(vect);
  int t = UVECTOR_TYPE(vect);

  STk_fprintf(port, "#%s(", type_vector(UVECTOR_TYPE(vect)));
  for (i = 0; i < n; i++) {
    STk_print(uvector_ref(t, vect, i), port, mode);
    if (i < n - 1) STk_putc(' ', port);
  }
  STk_putc(')', port);
}


static struct extended_type_descr xtype_uvector = {
  "uvector",
  print_uvector
};

/*==========================================================================*/
/*                                                                          */
/*                              B Y T E V E C T O R S                       */
/*                                                                          */
/*==========================================================================*/

SCM STk_make_C_bytevector(int len, char *init)
{
  return makeuvect(UVECT_U8, len, init);
}


/*
<doc R7RS bytevector-copy
 * (bytevector-copy bytevector)
 * (bytevector-copy bytevector start)
 * (bytevector-copy bytevector start end)
 *
 * Returns a newly allocated bytevector containing the bytes in |bytevector|
 * between |start| and |end|.
 * @lisp
 * (define a #u8(1 2 3 4 5))
 * (bytevector-copy a 2 4))       =>  #u8(3 4)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("bytevector-copy", bytevector_copy, vsubr, (int argc, SCM *argv))
{
  long start, end, n;
  SCM z, bvect;

  bvect = control_index(argc, argv, &start, &end, NULL);
  n = end-start;
  z = makeuvect(UVECT_U8, n, (SCM) NULL);
  memcpy(UVECTOR_DATA(z),
         (unsigned char *)UVECTOR_DATA(bvect)+start,
         n * sizeof(unsigned char));
  return z;
}

/*
<doc  bytevector-append
 * (bytevector-append bytevector ...)
 *
 * Returns a newly allocated bytevector whose elements are
 * the concatenation of the elements in the given bytevectors.
 * @lisp
 * (bytevector-append #u8(0 1 2) #u8(3 4 5))
 *                     =>  #u8(0 1 2 3 4 5)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("bytevector-append", bytevector_append, vsubr,(int argc, SCM *argv))
{
  int i, len = 0, start = 0;
  SCM z;

  /* compute length of final result */
  for (i = 0; i < argc; i++) {
    if (!UVECTORP(argv[-i]) || UVECTOR_TYPE(argv[-i]) != UVECT_U8)
      error_bad_uvector(argv[-i], UVECT_U8);
    len += UVECTOR_SIZE(argv[-i]);
  }

  /* copy bytevectors */
  z = makeuvect(UVECT_U8, len, (SCM) NULL);

  for (i = 0; i < argc; i++) {
    int sz = UVECTOR_SIZE(argv[-i]);
    memcpy(UVECTOR_DATA(z+start),
           (unsigned char *) UVECTOR_DATA(argv[-i]),
           sz * sizeof(unsigned char));
    start += sz * sizeof(unsigned char);
  }
  return z;
}

/*
<doc R7RS utf8->string string->utf8
 * (utf8->string bytevector)
 * (utf8->string bytevector start)
 * (utf8->string bytevector start end)
 * (string->utf8 string)
 * (string->utf8 string start)
 * (string->utf8 string start end)
 *
 * These procedures translate between strings and bytevectors
 * that encode those strings using the UTF-8 encoding.
 * The |utf8->string| procedure decodes the bytes of
 * a bytevector between |start| and |end| and returns the
 * corresponding string; the |string->utf8| procedure encodes the
 * characters of a string between |start| and |end| and returns
 * the corresponding bytevector.
 *
 * It is an error for |bytevector| to contain invalid UTF-8 byte
 * sequences.
 * @lisp
 * (utf8->string #u8(#x41))   => "A"
 * (string->utf8 "λ")         => #u8((#xce #xbb)
 * @end lisp
doc>
*/
SCM STk_make_bytevector_from_C_string(char *str, long len)
{
  SCM z  = makeuvect(UVECT_U8, len, (SCM) NULL);
  memcpy(UVECTOR_DATA(z),str, len);
  return z;
}

DEFINE_PRIMITIVE("utf8->string", utf82string, vsubr, (int argc, SCM *argv))
{
  long start, end, len;
  SCM v;
  unsigned char *start_addr, *end_addr;

  v          = control_index(argc, argv, &start, &end, NULL);
  start_addr = (unsigned char*) UVECTOR_DATA(v) + start;
  end_addr   = (unsigned char*) UVECTOR_DATA(v) + end;
  len        = end_addr - start_addr;

  /* Verify that the sub-vector denotes a correct string */
  if (STk_utf8_verify_sequence((char *) start_addr, len)) {
    SCM z = STk_makestring(len, NULL);
    memcpy(STRING_CHARS(z), start_addr, end_addr - start_addr);
    return z;
  }
  else
    STk_error("bad UTF8 sequence between %d and %d in ~S", start, end, v);

  return STk_void;  /* for the compiler */
}



/* ====================================================================== */
int STk_init_uniform_vector(void)
{
  DEFINE_XTYPE(uvector, &xtype_uvector);

  /* Useful primitives for bytevectors. Other primitves will be defined when
   * SRFI-4 is required
   */
  ADD_PRIMITIVE(make_uvector);
  ADD_PRIMITIVE(uvector);
  ADD_PRIMITIVE(uvectorp);
  ADD_PRIMITIVE(uvector_length);
  ADD_PRIMITIVE(uvector_ref);
  ADD_PRIMITIVE(uvector_set);

  /* R7RS specific bytevectors primitives */
  ADD_PRIMITIVE(bytevector_copy);
  ADD_PRIMITIVE(bytevector_append);
  ADD_PRIMITIVE(utf82string);

  /* A pseudo primitive to launch the definition of  all the function of SRFI-4 */
  ADD_PRIMITIVE(allow_uvectors);

  return TRUE;
}
