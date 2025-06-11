/*
 * ffi.c        -- FFI support dor STklos
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
 *    Creation date: 14-Jun-2007 09:19 (eg)
 */

#include "stklos.h"
#include <math.h>


static SCM ffi_table = STk_nil; // Alist such as ( (:void 0) (:int 1) ....)

#ifdef HAVE_FFI
#  include <ffi.h>

/* not portable but this is was stolen in FFI test */
#define ffi_type_ulonglong ffi_type_ulong
#define ffi_type_slonglong ffi_type_slong


/* ------------------------------ *\
 * STklos C external functions
 * ------------------------------ */
struct ext_func_obj {
  stk_header header;
  SCM name;
  SCM params;
  SCM rettype;
  SCM libname;
  void * code;
  ffi_cif cif;
  ffi_type **cif_arg_types;
};

union any {
  char                  cvalue;
  signed char           scvalue;
  unsigned char         ucvalue;
  short                 svalue;
  unsigned short        usvalue;
  int                   ivalue;
  unsigned int          uivalue;
  long                  lvalue;
  unsigned long         ulvalue;
  long long             llvalue;
  unsigned long long    ullvalue;
  int8_t                i8value;
  uint8_t               ui8value;
  int16_t               i16value;
  uint16_t              ui16value;
  int32_t               i32value;
  uint32_t              ui32value;
  int64_t               i64value;
  uint64_t              ui64value;
  float                 fvalue;
  double                dvalue;
  void *                pvalue;
};

#define EXTFUNC_NAME(f)         (((struct ext_func_obj *) (f))->name)
#define EXTFUNC_PARAMS(f)       (((struct ext_func_obj *) (f))->params)
#define EXTFUNC_RETTYPE(f)      (((struct ext_func_obj *) (f))->rettype)
#define EXTFUNC_LIBNAME(f)      (((struct ext_func_obj *) (f))->libname)
#define EXTFUNC_CODE(f)         (((struct ext_func_obj *) (f))->code)
#define EXTFUNC_CIF(f)          (((struct ext_func_obj *) (f))->cif)
#define EXTFUNC_CIF_ARGS(f)     (((struct ext_func_obj *) (f))->cif_arg_types)


#define EXTFUNCP(f)             (BOXED_TYPE_EQ((f), tc_ext_func))


/* ------------------------------ *\
 * STklos callbacks
 * ------------------------------ */
struct callback_obj {
  stk_header header;
  SCM proc;
  SCM types;
  SCM data;
};

#define CALLBACK_PROC(p)        (((struct callback_obj *) (p))->proc)
#define CALLBACK_TYPES(p)       (((struct callback_obj *) (p))->types)
#define CALLBACK_DATA(p)        (((struct callback_obj *) (p))->data)

#define CALLBACKP(p)            (BOXED_TYPE_EQ((p), tc_callback))


static SCM pointer_on_exec_callback;

/* ====================================================================== */

static void error_bad_cpointer(SCM obj)
{
  STk_error("bad C pointer object ~S", obj);
}

static void error_bad_type_number(SCM obj)
{
  STk_error("bad type number ~S", obj);
}

static void error_bad_string(SCM obj)
{
  STk_error("bad string ~S", obj);
}



/* ====================================================================== */
#define EXT_FUNC_MAX_PARAMS 30          /* max # of parameters to an external func */

enum f_codes {
  f_void,      f_char,      f_schar,     f_uchar,
  f_short,     f_ushort,    f_int,       f_uint,
  f_long,      f_ulong,     f_longlong,  f_ulonglong,
  f_int8,      f_uint8,     f_int16,     f_uint16,
  f_int32,     f_uint32,    f_int64,     f_uint64,
  f_float,     f_double,    f_boolean,     f_obj,
  f_pointer,   f_string,
  f_last    /* MUST be the last item of the enum */
};

static ffi_type* conversion[f_last];

void register_ffi_type(char *str, enum f_codes val, ffi_type *tip)
{
  /* Add the item to the A-list table associating Scheme name and internal value */
  ffi_table = STk_cons(LIST2(STk_makekey(str), MAKE_INT(val)), ffi_table);

  /* Add the item to the conversion table */
  conversion[val] = tip;
}


#define REG_TYPE(symb, tip) register_ffi_type(#symb, f_##symb, tip)

static void build_ffi_tables(void)
{
  REG_TYPE(void,        &ffi_type_void);
  REG_TYPE(char,        &ffi_type_uchar);
  REG_TYPE(short,       &ffi_type_sshort);
  REG_TYPE(ushort,      &ffi_type_ushort);
  REG_TYPE(int,         &ffi_type_sint);
  REG_TYPE(uint,        &ffi_type_uint);
  REG_TYPE(long,        &ffi_type_slong);
  REG_TYPE(ulong,       &ffi_type_ulong);
  REG_TYPE(longlong,    &ffi_type_slong);
  REG_TYPE(ulonglong,   &ffi_type_ulong);
  REG_TYPE(float,       &ffi_type_float);
  REG_TYPE(double,      &ffi_type_double);
  REG_TYPE(boolean,     &ffi_type_uint);
  REG_TYPE(pointer,     &ffi_type_pointer);
  REG_TYPE(string,      &ffi_type_pointer);
  REG_TYPE(int8,        &ffi_type_sint8);
  REG_TYPE(int16,       &ffi_type_sint16);
  REG_TYPE(int32,       &ffi_type_sint32);
  REG_TYPE(int64,       &ffi_type_sint64);
  REG_TYPE(obj,         &ffi_type_pointer);
  REG_TYPE(uint8,       &ffi_type_uint8);
  REG_TYPE(uint16,      &ffi_type_uint16);
  REG_TYPE(uint32,      &ffi_type_uint32);
  REG_TYPE(uint64,      &ffi_type_uint64);
  REG_TYPE(schar,       &ffi_type_schar);
  REG_TYPE(uchar,       &ffi_type_uchar);
}


static ffi_type* convert(SCM obj)
{
  int n = STk_integer_value(obj);

  if (n < 0 || n >=f_last) STk_error("bad integer ~S", obj);

  return conversion[n];
}

/* ======================================================================
 *      scheme2c ...
 * ====================================================================== */
static void scheme2c(SCM obj, int type_needed, union any *res, int index)
{
  switch (type_needed) {
    case f_void:
      STk_error("conversion of a parameter to void forbidden");
      break;
    case f_char:
    case f_uchar:
    case f_schar:
      if (CHARACTERP(obj)) {
        int val = CHARACTER_VAL(obj);
        switch (type_needed) {
          case f_char:      res->cvalue    = (char) val; break;
          case f_uchar:     res->ucvalue   = (unsigned char) val; break;
          case f_schar:     res->scvalue   = (signed char) val; break;
        }
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
        switch (type_needed) {
          case f_char:      res->cvalue    = (char) val; break;
          case f_uchar:     res->ucvalue   = (unsigned char) val; break;
          case f_schar:     res->scvalue   = (signed char) val; break;
          case f_short:     res->svalue    = (short) val; break;
          case f_ushort:    res->usvalue   = (unsigned short) val; break;
          case f_int:       res->ivalue    = (int) val; break;
          case f_uint:      res->uivalue   = (unsigned int) val; break;
          case f_long:      res->lvalue    = (long) val; break;
          case f_ulong:     res->ulvalue   = (unsigned long) val; break;
          case f_longlong:  res->llvalue   = (long long) val; break;
          case f_ulonglong: res->ullvalue  = (unsigned long long) val; break;
          case f_int8:      res->i8value   = (int8_t) val; break;
          case f_uint8:     res->ui8value  = (uint8_t) val; break;
          case f_int16:     res->i16value  = (int16_t) val; break;
          case f_uint16:    res->ui16value = (uint16_t) val; break;
          case f_int32:     res->i32value  = (int32_t) val; break;
          case f_uint32:    res->ui32value = (uint32_t) val; break;
          case f_int64:     res->i64value  = (int64_t) val; break;
          case f_uint64:    res->ui64value = (uint64_t) val; break;
        }
      }
      return;
    }
    case f_float:
    case f_double:
      {
        double d =  STk_number2double(obj);

        if (!isnan(d)) {
          if (type_needed == f_float)
            res->fvalue = (float) d;
          else
            res->dvalue = d;
          return;
        }
        break;
      }
    case f_boolean:
      res->ivalue = (obj != STk_false);
      return;
    case f_pointer:                                            /* pointer */
      if (CPOINTERP(obj)) {
        res->pvalue = CPOINTER_VALUE(obj);
        return;
      }
      else if (obj == STk_void) {
        res->pvalue = NULL;
        return;
      } else if (EXTFUNCP(obj)) {
        /* Pass the address of C function for external functions */
        res->pvalue = EXTFUNC_CODE(obj);
        return;
      }
      break;
    case f_string:                                            /* string */
      if (STRINGP(obj)) {
        res->pvalue = STRING_CHARS(obj);
        return;
      }
      else if (obj == STk_void) {
        res->pvalue = NULL;
        return;
      }
      break;
    case f_obj:
      res->pvalue = obj;
      return;
  }
  STk_error("Argument #%d cannot be converted to requested type. Value was ~S",
            index, obj);
}

/* ======================================================================
 *      c2scheme ...
 * ====================================================================== */
static SCM c2scheme(union any obj, SCM rettype)
{
  switch (INT_VAL(rettype)) {
    case f_void:
      return STk_void;

    case f_char:
      return MAKE_CHARACTER(obj.cvalue);
    case f_uchar:
      return MAKE_CHARACTER(obj.ucvalue);
    case f_schar:
      return STk_long2integer(obj.scvalue); /* signed chars are (small) ints */

    case f_short:
      return STk_long2integer(obj.svalue);
    case f_ushort:
      return STk_long2integer(obj.usvalue);
    case f_int:
      return STk_long2integer(obj.ivalue);
    case f_uint:
      return STk_long2integer(obj.uivalue);
    case f_long:
      return STk_long2integer(obj.lvalue);
    case f_ulong:
      return STk_ulong2integer(obj.ulvalue);
    case f_longlong:
      return STk_ulong2integer(obj.llvalue);
    case f_ulonglong:
      return STk_ulong2integer(obj.ullvalue);
    case f_int8:
      return STk_ulong2integer(obj.i8value);
    case f_uint8:
      return STk_ulong2integer(obj.ui8value);
    case f_int16:
      return STk_ulong2integer(obj.i16value);
    case f_uint16:
      return STk_ulong2integer(obj.ui16value);
    case f_int32:
      return STk_ulong2integer(obj.i32value);
    case f_uint32:
      return STk_ulong2integer(obj.ui32value);
    case f_int64:
      return STk_ulong2integer(obj.i64value);
    case f_uint64:
      return STk_ulong2integer(obj.ui64value);

    case f_float:
      return STk_double2real((double) obj.fvalue);
    case f_double:
      return STk_double2real(obj.dvalue);

    case f_boolean:
      return MAKE_BOOLEAN(obj.ivalue);

    case f_pointer:
      return (obj.pvalue) ?
        STk_make_Cpointer(obj.pvalue, STk_void, STk_false) :
        STk_void;

    case f_string:
      if (! obj.pvalue) return STk_void;
      return STk_Cstring2string(obj.pvalue);

    case f_obj:
      return (obj.pvalue ? obj.pvalue : STk_void);
    default:
      STk_panic("incorrect type number for FFI ~S", rettype);
  }
  return STk_void;
}

/* ======================================================================
 *      STk_ext_func_name ...
 * ====================================================================== */
SCM STk_ext_func_name(SCM fct)
{
  return EXTFUNC_NAME(fct);
}


/* ======================================================================
 *      STk_make-ext_func primitive ...
 * ====================================================================== */
DEFINE_PRIMITIVE("%make-ext-func", make_ext_func, subr4,
                 (SCM name, SCM params, SCM rettype, SCM libname))
{
  SCM tmp, z;
  void *fun;
  ffi_cif cif;
  ffi_type **args;
  ffi_status n;
  int i, len = STk_int_length(params);

  if (! STRINGP(name))           STk_error("bad function name ~S", name);
  if (! STRINGP(libname))        STk_error("bad library name ~S", libname);
  if (! INTP(rettype))           STk_error("bad integer ~S", rettype);
  if (len < 0)                   STk_error("bad parameter type list ~S", params);

  /* find the function in library */
  fun = STk_find_external_function(STRING_CHARS(libname),
                                   STRING_CHARS(name),
                                   TRUE);

  /* Prepare the function descriptor */
  args = STk_must_malloc(len * sizeof(ffi_type *));
  for (i=0, tmp=params; i < len; i++, tmp=CDR(tmp)) {
    args[i] = convert(CAR(tmp));
  }

  n = ffi_prep_cif(&cif,
                   FFI_DEFAULT_ABI,
                   len,
                   convert(rettype),
                   args);
  if (n != FFI_OK) STk_error("cannot create call descriptor for ~S", name);

  /* Create external function object */
  NEWCELL(z, ext_func);
  EXTFUNC_NAME(z)     = name;
  EXTFUNC_PARAMS(z)   = params;
  EXTFUNC_RETTYPE(z)  = rettype;
  EXTFUNC_LIBNAME(z)  = libname;
  EXTFUNC_CODE(z)     = fun;
  EXTFUNC_CIF(z)      = cif;
  EXTFUNC_CIF_ARGS(z) = args;

  return z;
}


/* ======================================================================
 *      STk_call_ext_function ...
 * ====================================================================== */
SCM STk_call_ext_function(SCM fct, int argc, SCM *argv)
{
  int i;
  union any retval;
  union any v_args[EXT_FUNC_MAX_PARAMS];
  void*     p_args[EXT_FUNC_MAX_PARAMS];
  SCM params;

  if (! EXTFUNCP(fct))
    STk_error("bad external function descriptor ~S", fct);

  /* Build the parameter array */
  for (i = 0, params = EXTFUNC_PARAMS(fct);
       i < argc;
       i++, params = CDR(params)) {
    if (NULLP(params))
      STk_error("too much parameters in call");

    scheme2c(argv[-i], INT_VAL(CAR(params)), &v_args[i], i);
    p_args[i] = & (v_args[i]);
  }
  if (! NULLP(params)) STk_error("not enough parameters in call");

  /* Perform the call */
  ffi_call(&EXTFUNC_CIF(fct),
           EXTFUNC_CODE(fct),
           &retval,
           p_args);

  return c2scheme(retval, EXTFUNC_RETTYPE(fct));
}


/* ======================================================================
 *      make-callback ...
 * ====================================================================== */
DEFINE_PRIMITIVE("%make-callback", make_callback, subr3,
                 (SCM proc, SCM types, SCM data))
{
  SCM z;

  if (STk_procedurep(proc) == STk_false) STk_error("bad procedure ~S", proc);
  if (!CONSP(types)) STk_error("incorrect types description ~S", types);

  NEWCELL(z, callback);
  CALLBACK_PROC(z) = proc;
  CALLBACK_TYPES(z) = types;
  CALLBACK_DATA(z) = data;
  return z;
}


/* ======================================================================
 *      STk_exec_callback ...
 * ====================================================================== */
#ifndef LIB_FFI_KLUDGE
#define MAX_ARGS_CALLBACK 20

static int exec_callback(SCM callback, ...)
{
  va_list ap;
  SCM res, Cargs, Sargs[MAX_ARGS_CALLBACK];
  union any param;
  int i = 0;

  va_start(ap, callback);

  for (Cargs = CALLBACK_TYPES(callback); !NULLP(Cargs); Cargs = CDR(Cargs)) {
    /* grab the argument on the stack */
    switch(INT_VAL(CAR(Cargs))) {
      case f_void:
        break;

      case f_char:
        param.cvalue = va_arg(ap, int); break;
      case f_schar:
        param.scvalue = va_arg(ap, signed int); break;
      case f_uchar:
        param.ucvalue = va_arg(ap, unsigned int); break;
      case f_short:
        param.svalue = va_arg(ap, int); break;
      case f_ushort:
        param.usvalue = va_arg(ap, unsigned int); break;
      case f_int:
        param.ivalue = va_arg(ap, int); break;
      case f_uint:
        param.uivalue = va_arg(ap, unsigned int); break;
      case f_long:
        param.lvalue = va_arg(ap, long); break;
      case f_ulong:
        param.ulvalue = va_arg(ap, unsigned long); break;
      case f_longlong:
        param.llvalue = va_arg(ap, long long); break;
      case f_ulonglong:
        param.ullvalue = va_arg(ap, unsigned long long); break;
      case f_int8:
        param.i8value = va_arg(ap, int); break;
      case f_uint8:
        param.ui8value = va_arg(ap, unsigned int); break;
      case f_int16:
        param.i16value = va_arg(ap, int); break;
      case f_uint16:
        param.ui16value = va_arg(ap, unsigned int); break;
      case f_int32:      /* Be careful with  integer promotion ? */
        param.i32value = (sizeof(int32_t) >= sizeof(int)) ?
                            va_arg(ap, int32_t): va_arg(ap, int);
        break;
      case f_uint32:     /* Be careful with  integer promotion ? */
        param.ui32value = (sizeof(int32_t) >= sizeof(int)) ?
                            va_arg(ap, uint32_t): va_arg(ap, unsigned int);
        break;
      case f_int64:      /* Be careful with  integer promotion ? */
        param.i32value = (sizeof(int64_t) >= sizeof(int)) ?
                            va_arg(ap, int64_t): va_arg(ap, int);
        break;
      case f_uint64:      /* Be careful with  integer promotion ? */
        param.i32value = (sizeof(int64_t) >= sizeof(int)) ?
                            va_arg(ap, uint64_t): va_arg(ap, unsigned int);
        break;

      case f_float:
        param.fvalue = (float) va_arg(ap, double); break;
      case f_double:
        param.dvalue = va_arg(ap, double); break;

      case f_boolean:
        param.ivalue = va_arg(ap, int); break;

      case f_pointer:
        param.pvalue = va_arg(ap, void *); break;

      case f_string:
        param.pvalue = va_arg(ap, char *); break;
        STk_error("argument of type ~S in callback are not implemented yet",
                  CAR(Cargs)); break;
      case f_obj:
        param.pvalue = va_arg(ap, void *); break;

      default:
        STk_panic("incorrect type number for FFI ~s", (CAR(Cargs)));
    }

    /* param contains the C argument */
    if (i >= MAX_ARGS_CALLBACK - 1)
      STk_error("a callback cannot have more than %d arguments", MAX_ARGS_CALLBACK);

    Sargs[i++] = c2scheme(param, CAR(Cargs));
  }

  va_end(ap);

  /* Add the callback value as last parameter */
  Sargs[i++] = CALLBACK_DATA(callback);
  Sargs[i]= NULL;

  res = STk_C_apply(CALLBACK_PROC(callback), -i, Sargs);
  return (res != STk_false);
}
#else
/* KLUDGE: Since libffi doesn't support callbacks with varargs, we
 * used a special version of exec_calback which works only for the way
 * it is used in the gtk-gtklos-base ScmPkg (a callback can be called
 * with one or two pointers. Hopefully, callbacks were not documened
 * in previous versions of STklos ;-)
 *
 * 2021/06/09: This code was used before for GTk support. It doesn't
 * seem useful anymore. It is kept here for reference
 */
static int exec_callback(SCM callback, void *ptr1, void *ptr2)
{
  int len  = STk_int_length(CALLBACK_TYPES(callback));
  SCM data = CALLBACK_DATA(callback);
  SCM res  = STk_false;

  switch (len) {
    case 1:
      res = STk_C_apply(CALLBACK_PROC(callback), 2, ptr1, data);
      break;
    case 2:
      res = STk_C_apply(CALLBACK_PROC(callback), 3, ptr1, ptr2, data);
      break;
    default:
      STk_panic("ffi kludge is incorrect ~s", CALLBACK_TYPES(callback));
      break;
  }
  return (res != STk_false);
}
#endif


DEFINE_PRIMITIVE("%exec-callback-address", exec_cb_addr, subr0, (void))
{
  return pointer_on_exec_callback;
}


/* ======================================================================
 *      Build a pointer to a C variable ...
 * ====================================================================== */

DEFINE_PRIMITIVE("%get-symbol-address", get_symbol_address, subr2,
                 (SCM name, SCM libname))
{
  void *var;

  if (!STRINGP(name))  error_bad_string(name);
  if (!STRINGP(libname)) error_bad_string(libname);

  var = STk_find_external_function(STRING_CHARS(libname), STRING_CHARS(name), FALSE);

  return var? STk_make_Cpointer(var, STk_intern("extern-var"), STk_false): STk_false;
}


DEFINE_PRIMITIVE("%get-typed-ext-var", get_typed_ext_var, subr2, (SCM obj, SCM type))
{
  long kind = STk_integer_value(type);

  if (!CPOINTERP(obj))  error_bad_cpointer(obj);
  if (kind == LONG_MIN) error_bad_type_number(obj);

  switch (kind) {
    case 0:                                             /* void */
      STk_error("cannot access a void variable");
      break;
    case 1:                                             /* char */
    case 24:                                            /* schar */
      return MAKE_CHARACTER(* ((char *)CPOINTER_VALUE(obj)));
    case 2:                                             /* short */
      return STk_long2integer(* ((short *)CPOINTER_VALUE(obj)));
    case 3:                                             /* ushort */
      return STk_long2integer(* ((unsigned short *)CPOINTER_VALUE(obj)));
    case 4:                                             /* int */
      return STk_long2integer(* ((int *)CPOINTER_VALUE(obj)));
    case 5:                                             /* uint */
      return STk_long2integer(* ((unsigned int *)CPOINTER_VALUE(obj)));
    case 6:                                             /* long */
      return STk_long2integer(* ((long *)CPOINTER_VALUE(obj)));
    case 7:                                             /* ulong */
      return STk_long2integer(* ((unsigned long *)CPOINTER_VALUE(obj)));
    case 8:                                             /* lonlong */
    case 9:                                             /* ulonlong */
      STk_error("access to long long is not implemented yet");
      break;
    case 10:                                            /* float */
      return STk_double2real(* ((float *)CPOINTER_VALUE(obj)));
    case 11:                                            /* double */
      return STk_double2real(* ((double *)CPOINTER_VALUE(obj)));
    case 12:                                            /* boolean */
      return MAKE_BOOLEAN(* ((int *)CPOINTER_VALUE(obj)));
    case 13:                                            /* pointer */
      {
        void *ptr = (* ((void**) CPOINTER_VALUE(obj)));

        return (ptr) ?
          STk_make_Cpointer(ptr, STk_void, STk_false) :
          STk_void;
      }
    case 14:                                            /* string */
      {
        char *str = (* ((char **) CPOINTER_VALUE(obj)));

        return (str) ? STk_Cstring2string(str): STk_void;
      }
    case 15:                                            /* int8 */
    case 16:                                            /* int16 */
    case 17:                                            /* int32 */
    case 18:                                            /* int64 */
    case 20:                                            /* uint8 */
    case 21:                                            /* uint16 */
    case 22:                                            /* uint32 */
    case 23:                                            /* uint64 */
      STk_error("returning intXX is not implemented yet");
      break;
    case 19:                                            /* obj */
      {
        void *ptr = (* (void**) CPOINTER_VALUE(obj));

        return (ptr) ? ptr : STk_void;
      }
    default:
      STk_panic("incorrect type number for external variable ~S", type);
  }
  return STk_void;
}

DEFINE_PRIMITIVE("%set-typed-ext-var!", set_typed_ext_var, subr3,
                 (SCM obj, SCM val, SCM type))
{
  long kind = STk_integer_value(type);

  if (!CPOINTERP(obj))  error_bad_cpointer(obj);
  if (kind == LONG_MIN) error_bad_type_number(obj);

  switch (kind) {
    case 0:                                             /* void */
      STk_error("conversion to void forbidden");
      break;
    case 1:                                             /* char */
    case 2:                                             /* short */
    case 3:                                             /* ushort */
    case 4:                                             /* int */
    case 5:                                             /* uint */
    case 6:                                             /* long */
    case 7:                                             /* ulong */
    case 24:                                            /* schar */
      {
        long value = CHARACTERP(val) ?
                         (long) CHARACTER_VAL(val) :
                         STk_integer_value(val);
        if (value != LONG_MIN) {
          switch (kind) {
            case 1: (* ((char *)CPOINTER_VALUE(obj))) = (char) value; break;
            case 2: (* ((short *)CPOINTER_VALUE(obj))) = (short) value; break;
            case 3: (* ((unsigned short *)CPOINTER_VALUE(obj)))
                             = (unsigned short) value; break;
            case 4: (* ((int *)CPOINTER_VALUE(obj))) = (int) value; break;
            case 5: (* ((unsigned int *)CPOINTER_VALUE(obj)))
                             = (unsigned int) value; break;
            case 6: (* ((long *)CPOINTER_VALUE(obj))) = (long) value; break;
            case 7: (* ((unsigned long *)CPOINTER_VALUE(obj)))
                             = (unsigned long) value; break;
            case 24: (* ((unsigned char *)CPOINTER_VALUE(obj))) = (unsigned char) value; break;
          }
          return STk_void;
        }
        break;
      }
    case 8:                                             /* lonlong */
    case 9:                                             /* ulonlong */
      STk_error("setting long long is not implemented yet");
      break;
    case 10:                                            /* float */
    case 11:                                            /* double */
      {
        double d =  STk_number2double(val);

        if (!isnan(d)) {
          if (kind == 10)
            (* ((float *)CPOINTER_VALUE(obj))) = (float) d;
          else
            (* ((double *)CPOINTER_VALUE(obj))) = d;
          return STk_void;
        }
        break;
      }
    case 12:                                            /* boolean */
      (* ((int *)CPOINTER_VALUE(obj))) = (val != STk_false);
      return STk_void;
    case 13:                                            /* pointer */
    case 14:                                            /* string */
    case 15:                                            /* int8 */
    case 16:                                            /* int16 */
    case 17:                                            /* int32 */
    case 18:                                            /* int64 */
    case 20:                                            /* uint8 */
    case 21:                                            /* uint16 */
    case 22:                                            /* uint32 */
    case 23:                                            /* uint64 */
      STk_error("passing argument of type ~S is not implemented yet", type);
      break;
    case 19:                                            /* obj */
      (* ((void **)CPOINTER_VALUE(obj))) = CPOINTER_VALUE(val);
      return STk_void;
  }
  STk_error("cannot convert ~S in requested type (~S)", obj, type);
  return STk_void;
}

/* ======================================================================
 *      STk_cpointer-set_func primitive ...
 * ====================================================================== */
DEFINE_PRIMITIVE("%cpointer-set!", cpointer_set, subr4,
                 (SCM pointer_obj, SCM type, SCM value, SCM offset))
{
  long kind = STk_integer_value(type);
  char* pointer = CPOINTER_VALUE(pointer_obj) + STk_integer_value(offset);

  if (!CPOINTERP(pointer_obj))  error_bad_cpointer(pointer_obj);
  if (kind == LONG_MIN) error_bad_type_number(type);

  switch (kind) {
    case f_void:
      STk_error("Can not set type :void");
      break;
    case f_char:
        *pointer = (char)CHARACTER_VAL(value);
        break;
    case f_short:
        *(short*)pointer = (short)STk_integer_value(value);
        break;
    case f_ushort:
        *(unsigned short*)pointer = (unsigned short)STk_integer_value(value);
        break;
    case f_int:
        *(int*)pointer = (int)STk_integer_value(value);
        break;
    case f_uint:
        *(int*)pointer = (int)STk_integer_value(value);
        break;
    case f_long:
        *(long*)pointer = (long)STk_integer_value(value);
        break;
    case f_ulong:
        *(unsigned long*)pointer = (unsigned long)STk_integer_value(value);
        break;
    case f_longlong:
    case f_ulonglong:
        STk_error("passing argument of type ~S is not implemented yet", type);
        break;
    case f_float:
        *(float*)pointer = (float)STk_number2double(value);
        break;
    case f_double:
        *(double*)pointer = (double)STk_number2double(value);
        break;
    case f_boolean:
        *(int*)pointer = (value != STk_false);
        break;
    case f_pointer:
        *(char**)pointer = CPOINTER_VALUE(value);
        break;
    case f_string:
        STk_error("passing argument of type ~S is not implemented yet", type);
        break;
    case f_int8:
        *(int8_t*)pointer = (int8_t)STk_integer_value(value);
        break;
    case f_int16:
        *(int16_t*)pointer = (int16_t)STk_integer_value(value);
        break;
    case f_int32:
        *(int32_t*)pointer = (int32_t)STk_integer_value(value);
        break;
    case f_int64:
        *(int64_t*)pointer = (int64_t)STk_integer_value(value);
        break;
    case f_obj:
        STk_error("can not set type :obj");
        break;
    case f_uint8:
        *(uint8_t*)pointer = (uint8_t)STk_integer_value(value);
        break;
    case f_uint16:
        *(uint16_t*)pointer = (uint16_t)STk_integer_value(value);
        break;
    case f_uint32:
        *(uint32_t*)pointer = (uint32_t)STk_integer_value(value);
        break;
    case f_uint64:
        *(uint64_t*)pointer = (uint64_t)STk_integer_value(value);
        break;
    case f_schar:
        *pointer = (signed char)CHARACTER_VAL(value);
        break;
  }
  return STk_void;
}




/* ======================================================================
 *      STk_cpointer-ref_func primitive ...
 * ====================================================================== */
DEFINE_PRIMITIVE("%cpointer-ref", cpointer_ref, subr3,
                 (SCM pointer_obj, SCM type, SCM offset))
{
  long kind = STk_integer_value(type);
  char* pointer = CPOINTER_VALUE(pointer_obj) + STk_integer_value(offset);

  if (!CPOINTERP(pointer_obj)) error_bad_cpointer(pointer_obj);
  if (kind == LONG_MIN) error_bad_type_number(pointer);

  switch (kind) {
    case f_void:
      STk_error("Can not set type :void");
      break;
    case f_char:
        return MAKE_CHARACTER(*(unsigned char*)pointer);
    case f_short:
        return MAKE_INT(*(short*)pointer);
    case f_ushort:
        return MAKE_INT(*(short*)pointer);
    case f_int:
        return MAKE_INT(*(int*)pointer);
    case f_uint:
        return MAKE_INT(*(int*)pointer);
    case f_long:
        return MAKE_INT(*(long*)pointer);
    case f_ulong:
        return MAKE_INT(*(long*)pointer);
    case f_longlong:
    case f_ulonglong:
        STk_error("passing argument of type ~S is not implemented yet", type);
        return STk_void;
    case f_float:
        return STk_double2real(*(float*)pointer);
    case f_double:
        return STk_double2real(*(double*)pointer);
    case f_boolean:
        return MAKE_BOOLEAN(*(int*)pointer);
    case f_pointer:
        //char* p = ((char*)CPOINTER_VALUE(pointer)) + offset;
        return STk_make_Cpointer(*(char**)pointer, STk_void, STk_false);
    case f_string:
        STk_error("passing argument of type ~S is not implemented yet", type);
        return STk_void;
    case f_int8:
        return MAKE_INT(*(int8_t*)pointer);
    case f_int16:
        return MAKE_INT(*(int16_t*)pointer);
    case f_int32:
        return MAKE_INT(*(int32_t*)pointer);
    case f_int64:
        return MAKE_INT(*(int64_t*)pointer);
    case f_obj:
        STk_error("can not ref type :obj");
        return STk_void;
    case f_uint8:
        return MAKE_INT(*(uint8_t*)pointer);
    case f_uint16:
        return MAKE_INT(*(uint16_t*)pointer);
    case f_uint32:
        return MAKE_INT(*(uint32_t*)pointer);
    case f_uint64:
        return MAKE_INT(*(uint64_t*)pointer);
    case f_schar:
        return MAKE_CHARACTER(*pointer);
    default:
      STk_panic("Incorrect type number for external variable ~S", type);
  }
  return STk_void; /* for the compiler */
}

#else /* HAVE_FFI */
static void error_no_ffi(void)
{
  STk_error("current system does not support FFI");
}

DEFINE_PRIMITIVE("%make-ext-func", make_ext_func, subr4,
                 (SCM p1, SCM p2, SCM p3, SCM p4))
{ error_no_ffi(); return STk_void;}

DEFINE_PRIMITIVE("%make-callback", make_callback, subr3, (SCM p1, SCM p2, SCM p3))
{ error_no_ffi(); return STk_void;}

DEFINE_PRIMITIVE("%exec-callback-address", exec_cb_addr, subr0, (void))
{ error_no_ffi(); return STk_void;}


DEFINE_PRIMITIVE("%get-symbol-address", get_symbol_address, subr2,
                 (SCM name, SCM libname))
{ error_no_ffi(); return STk_void;}

DEFINE_PRIMITIVE("%get-typed-ext-var", get_typed_ext_var, subr2, (SCM obj, SCM type))
{error_no_ffi(); return STk_void;}


DEFINE_PRIMITIVE("%set-typed-ext-var!", set_typed_ext_var, subr3,
                 (SCM obj, SCM val, SCM type))
{ error_no_ffi(); return STk_void;}

#endif

DEFINE_PRIMITIVE("%stklos-has-ffi?", has_ffi, subr0, ())
{
#ifdef HAVE_FFI
  return STk_true;
#else
  return STk_false;
#endif
}

DEFINE_PRIMITIVE("%ffi-assoc-table", ffi_assoc_table, subr0, (void))
{
  return ffi_table;
}

/* ======================================================================
 *      INIT  ...
 * ====================================================================== */
int STk_init_ffi(void)
{
  #ifdef HAVE_FFI
  pointer_on_exec_callback = STk_make_Cpointer(exec_callback,
                                               STk_void,
                                               STk_false);
  build_ffi_tables();
  #endif

  ADD_PRIMITIVE(make_ext_func);
  ADD_PRIMITIVE(make_callback);
  ADD_PRIMITIVE(exec_cb_addr);

  ADD_PRIMITIVE(get_symbol_address);
  ADD_PRIMITIVE(get_typed_ext_var);
  ADD_PRIMITIVE(set_typed_ext_var);

  ADD_PRIMITIVE(cpointer_set);
  ADD_PRIMITIVE(cpointer_ref);

  ADD_PRIMITIVE(ffi_assoc_table);
  ADD_PRIMITIVE(has_ffi);
  return TRUE;
}
