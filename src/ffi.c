/*
 * ffi.c        -- FFI support dor STklos
 *
 * Copyright © 2007-2021 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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

#ifdef HAVE_FFI
#  include <ffi.h>

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
  unsigned char  cvalue;
  short          svalue;
  unsigned short usvalue;
  int            ivalue;
  unsigned int   uivalue;
  long           lvalue;
  unsigned long  ulvalue;
  long long      evalue;
  float          fvalue;
  double         dvalue;
  void *         pvalue;
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

/*
((:void         0)  (:char      1)  (:short     2)   (:ushort   3)
 (:int          4)  (:uint      5)  (:long      6)   (:ulong    7)
 (:lonlong      8)  (:ulonlong  9)  (:float     10)  (:double   11)
 (:boolean      12) (:pointer   13) (:string    14)  (:int8     15)
 (:int16        16) (:int32     17) (:int64     18)  (:obj      19)
 (:uint8        20) (:uint16    21) (:uint32    22)  (:uint64   23)
 (:schar        24) (:uchar     25)
 )
*/



#define EXT_FUNC_MAX_TYPE 25            /* maximal value for types */
#define EXT_FUNC_MAX_PARAMS 30          /* max # of parameters to an external func */

static ffi_type* conversion[] = {
  &ffi_type_void,               /* :void */
  &ffi_type_uchar,              /* :char */
  &ffi_type_sshort,             /* :short */
  &ffi_type_ushort,             /* :ushort */
  &ffi_type_sint,               /* :int */
  &ffi_type_uint,               /* :uint */
  &ffi_type_slong,              /* :long */
  &ffi_type_ulong,              /* :ulong */
  &ffi_type_sint64,             /* :lonlong   !!!!!!!!!!!!! */
  &ffi_type_uint64,             /* :lonlong   !!!!!!!!!!!!! */
  &ffi_type_float,              /* :float */
  &ffi_type_double,             /* :double */
  &ffi_type_uint,               /* :boolean */
  &ffi_type_pointer,            /* :pointer */
  &ffi_type_pointer,            /* :string */
  &ffi_type_sint8,              /* :int8 */
  &ffi_type_sint16,             /* :int16 */
  &ffi_type_sint32,             /* :int32 */
  &ffi_type_sint64,             /* :int64 */
  &ffi_type_pointer,            /* :obj */
  &ffi_type_uint8,              /* :uint8 */
  &ffi_type_uint16,             /* :uint16 */
  &ffi_type_uint32,             /* :uint32 */
  &ffi_type_uint64,             /* :uint64 */
  &ffi_type_schar,              /* :schar*/
  &ffi_type_uchar,              /* :uchar*/
};


static ffi_type* convert(SCM obj)
{
  int n = STk_integer_value(obj);

  if (n < 0 || n > EXT_FUNC_MAX_TYPE) STk_error("bad integer ~S", obj);

  return conversion[n];
}



/* ======================================================================
 *      scheme2c ...
 * ====================================================================== */
static void scheme2c(SCM obj, int type_needed, union any *res, int index)
{
  switch (type_needed) {
    case 0:                                             /* void */
      STk_error("conversion of a parameter to void forbidden");
      break;
    case 1:                                             /* char */
    case 2:                                             /* short */
    case 3:                                             /* ushort */
    case 4:                                             /* int */
    case 5:                                             /* uint */
    case 6:                                             /* long */
    case 7:                                             /* ulong */
    case 24:                                            /* schar */
    case 25:                                            /* uchar */
      {
        long val = STk_integer_value(obj);
        if (val != LONG_MIN) {
          switch (type_needed) {
            case 1: res->cvalue  = (unsigned char) val; break;
            case 2: res->svalue  = (short) val; break;
            case 3: res->usvalue = (unsigned short) val; break;
            case 4: res->ivalue  = (int) val; break;
            case 5: res->uivalue = (unsigned int) val; break;
            case 6: res->uivalue = (long) val; break;
            case 7: res->uivalue = (unsigned long) val; break;
            case 24: res->cvalue  = (char) val; break;
            case 25: res->cvalue  = (unsigned char) val; break;
          }
          return;
        }
        break;
      }
    case 8:                                             /* lonlong */
    case 9:                                             /* ulonlong */
      STk_error("passing long long is not implemented yet");
      break;
    case 10:                                            /* float */
    case 11:                                            /* double */
      {
        double d =  STk_number2double(obj);

        if (!isnan(d)) {
          if (type_needed == 10)
            res->fvalue = (float) d;
          else
            res->dvalue = d;
          return;
        }
        break;
      }
    case 12:                                            /* boolean */
      res->ivalue = (obj != STk_false);
      return;
    case 13:                                            /* pointer */
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
    case 14:                                            /* string */
      if (STRINGP(obj)) {
        res->pvalue = STRING_CHARS(obj);
        return;
      }
      else if (obj == STk_void) {
        res->pvalue = NULL;
        return;
      }
      break;
    case 15:                                            /* int8 */
    case 16:                                            /* int16 */
    case 17:                                            /* int32 */
    case 18:                                            /* int64 */
    case 20:                                            /* uint8 */
    case 21:                                            /* uint16 */
    case 22:                                            /* uint32 */
    case 23:                                            /* uint64 */
      STk_error("passing intXX is not implemented yet");
      break;
    case 19:                                            /* obj */
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
    case 0:                                             /* void */
      return STk_void;
    case 1:                                             /* char */
      return MAKE_CHARACTER(obj.cvalue);
    case 2:                                             /* short */
      return STk_long2integer(obj.svalue);
    case 3:                                             /* ushort */
      return STk_long2integer(obj.usvalue);
    case 4:                                             /* int */
      return STk_long2integer(obj.ivalue);
    case 5:                                             /* uint */
      return STk_long2integer(obj.uivalue);
    case 6:                                             /* long */
      return STk_long2integer(obj.lvalue);
    case 7:                                             /* ulong */
      return STk_ulong2integer(obj.ulvalue);
    case 8:                                             /* lonlong */
    case 9:                                             /* ulonlong */
      STk_error("returning long long is not implemented yet");
      break;
    case 10:                                            /* float */
      return STk_double2real((double) obj.fvalue);
    case 11:                                            /* double */
      return STk_double2real(obj.dvalue);
    case 12:                                            /* boolean */
      return MAKE_BOOLEAN(obj.ivalue);
    case 13:                                            /* pointer */
      return (obj.pvalue) ?
        STk_make_Cpointer(obj.pvalue, STk_void, STk_false) :
        STk_void;
    case 14:                                            /* string */
      if (! obj.pvalue) return STk_void;
      return STk_Cstring2string(obj.pvalue);
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
      case 0:                                           /* void */
        break;
      case 1:                                           /* char */
        param.cvalue = va_arg(ap, int); break;
      case 2:                                           /* short */
        param.svalue = va_arg(ap, int); break;
      case 3:                                           /* ushort */
        param.usvalue = va_arg(ap, unsigned int); break;
      case 4:                                           /* int */
        param.ivalue = va_arg(ap, int); break;
      case 5:                                           /* uint */
        param.uivalue = va_arg(ap, unsigned int); break;
      case 6:                                           /* long */
        param.lvalue = va_arg(ap, long); break;
      case 7:                                           /* ulong */
        param.ulvalue = va_arg(ap, unsigned long); break;
      case 8:                                           /* lonlong */
      case 9:                                           /* ulonlong */
        STk_error("long long in a callback are not implemented yet"); break;
      case 10:                                          /* float */
        param.fvalue = (float) va_arg(ap, double); break;
      case 11:                                          /* double */
        param.dvalue = va_arg(ap, double); break;
      case 12:                                          /* boolean */
        param.ivalue = va_arg(ap, int); break;
      case 13:                                          /* pointer */
        param.pvalue = va_arg(ap, void *); break;
      case 14:                                          /* string */
        param.pvalue = va_arg(ap, char *); break;
      case 15:                                          /* int8 */
      case 16:                                          /* int16 */
      case 17:                                          /* int32 */
      case 18:                                          /* int64 */
      case 20:                                          /* uint8 */
      case 21:                                          /* uint16 */
      case 22:                                          /* uint32 */
      case 23:                                          /* uint64 */
        STk_error("argument of type ~S in callback are not implemented yet",
                  CAR(Cargs)); break;
      case 19:                                          /* obj */
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
    case 25:                                            /* uchar */
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
            case 25: (* ((unsigned char *)CPOINTER_VALUE(obj))) = (unsigned char) value; break;
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
                 (SCM pointer, SCM type, SCM value, SCM offset))
{
  long kind = STk_integer_value(type);

  if (!CPOINTERP(pointer))  error_bad_cpointer(pointer);
  if (kind == LONG_MIN) error_bad_type_number(type);

  switch (kind) {
    case 0:                                             /* void */
      STk_error("Can not set type :void");
      break;
    case 1:                                             /* char */
        *((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (char)CHARACTER_VAL(value);
        break;
    case 2:                                             /* short */
        *(short*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (short)STk_integer_value(value);
        break;
    case 3:                                             /* ushort */
        *(unsigned short*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (unsigned short)STk_integer_value(value);
        break;
    case 4:                                             /* int */
        *(int*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (int)STk_integer_value(value);
        break;
    case 5:                                             /* uint */
        *(int*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (int)STk_integer_value(value);
        break;
    case 6:                                             /* long */
        *(long*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (long)STk_integer_value(value);
        break;
    case 7:                                             /* ulong */
        *(unsigned long*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (unsigned long)STk_integer_value(value);
        break;
    case 8:                                             /* lonlong */
        STk_error("passing argument of type ~S is not implemented yet", type);
        break;
    case 9:                                             /* ulonlong */
        STk_error("passing argument of type ~S is not implemented yet", type);
        break;
    case 10:                                            /* float */
        *(float*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (float)STk_number2double(value);
        break;
    case 11:                                            /* double */
        *(double*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (double)STk_number2double(value);
        break;
    case 12:                                            /* boolean */
        *(int*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (value != STk_false);
        break;
    case 13:                                            /* pointer */
        char* p = (char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset);
        *(char**)p = CPOINTER_VALUE(value);
        break;
    case 14:                                            /* string */
        STk_error("passing argument of type ~S is not implemented yet", type);
        break;
    case 15:                                            /* int8 */
        *(int8_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (int8_t)STk_integer_value(value);
        break;
    case 16:                                            /* int16 */
        *(int16_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (int16_t)STk_integer_value(value);
        break;
    case 17:                                            /* int32 */
        *(int32_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (int32_t)STk_integer_value(value);
        break;
    case 18:                                            /* int64 */
        *(int64_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (int64_t)STk_integer_value(value);
        break;
    case 19:                                            /* obj */
        STk_error("Can not set type :obj");
        break;
    case 20:                                            /* uint8 */
        *(uint8_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (uint8_t)STk_integer_value(value);
        break;
    case 21:                                            /* uint16 */
        *(uint16_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (uint16_t)STk_integer_value(value);
        break;
    case 22:                                            /* uint32 */
        *(uint32_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (uint32_t)STk_integer_value(value);
        break;
    case 23:                                            /* uint64 */
        *(uint64_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (uint64_t)STk_integer_value(value);
        break;
    case 24:                                            /* schar */
        *((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (char)CHARACTER_VAL(value);
        break;
    case 25:                                            /* uchar */
        *((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)) = (unsigned char)CHARACTER_VAL(value);
        break;
  }
  return STk_void;
}

/* ======================================================================
 *      STk_cpointer-ref_func primitive ...
 * ====================================================================== */
DEFINE_PRIMITIVE("%cpointer-ref", cpointer_ref, subr3,
                 (SCM pointer, SCM type, SCM offset))
{

  long kind = STk_integer_value(type);

  if (!CPOINTERP(pointer))  error_bad_cpointer(pointer);
  if (kind == LONG_MIN) error_bad_type_number(pointer);

  switch (kind) {
    case 0:                                             /* void */
      STk_error("Can not set type :void");
      break;
    case 1:                                             /* char */
        return MAKE_CHARACTER(*(unsigned char*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 2:                                             /* short */
        return MAKE_INT(*(short*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 3:                                             /* ushort */
        return MAKE_INT(*(short*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 4:                                             /* int */
        return MAKE_INT(*(int*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 5:                                             /* uint */
        return MAKE_INT(*(int*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 6:                                             /* long */
        return MAKE_INT(*(long*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 7:                                             /* ulong */
        return MAKE_INT(*(long*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 8:                                             /* lonlong */
        STk_error("passing argument of type ~S is not implemented yet", type);
        return STk_void;
    case 9:                                             /* ulonlong */
        STk_error("passing argument of type ~S is not implemented yet", type);
        return STk_void;
    case 10:                                            /* float */
        return STk_double2real(*(float*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 11:                                            /* double */
        return STk_double2real(*(double*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 12:                                            /* boolean */
        return MAKE_BOOLEAN(*(int*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 13:                                            /* pointer */
        char* p = ((char*)CPOINTER_VALUE(pointer)) + STk_integer_value(offset);
        return STk_make_Cpointer(*(char**)p, STk_void, STk_false);
    case 14:                                            /* string */
        STk_error("passing argument of type ~S is not implemented yet", type);
        return STk_void;
    case 15:                                            /* int8 */
        return MAKE_INT(*(int8_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 16:                                            /* int16 */
        return MAKE_INT(*(int16_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 17:                                            /* int32 */
        return MAKE_INT(*(int32_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 18:                                            /* int64 */
        return MAKE_INT(*(int64_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 19:                                            /* obj */
        STk_error("Can not ref type :obj");
        return STk_void;
    case 20:                                            /* uint8 */
        return MAKE_INT(*(uint8_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 21:                                            /* uint16 */
        return MAKE_INT(*(uint16_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 22:                                            /* uint32 */
        return MAKE_INT(*(uint32_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 23:                                            /* uint64 */
        return MAKE_INT(*(uint64_t*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 24:                                            /* schar */
        return MAKE_CHARACTER(*((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    case 25:                                            /* uchar */
        return MAKE_CHARACTER(*(unsigned char*)((char*)CPOINTER_VALUE(pointer) + STk_integer_value(offset)));
    default:
      STk_panic("Incorrect type number for external variable ~S", type);
  }
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


/* ======================================================================
 *      INIT  ...
 * ====================================================================== */
int STk_init_ffi(void)
{
  #ifdef HAVE_FFI
  pointer_on_exec_callback = STk_make_Cpointer(exec_callback,
                                               STk_void,
                                               STk_false);
  #endif

  ADD_PRIMITIVE(make_ext_func);
  ADD_PRIMITIVE(make_callback);
  ADD_PRIMITIVE(exec_cb_addr);

  ADD_PRIMITIVE(get_symbol_address);
  ADD_PRIMITIVE(get_typed_ext_var);
  ADD_PRIMITIVE(set_typed_ext_var);

  ADD_PRIMITIVE(cpointer_set);
  ADD_PRIMITIVE(cpointer_ref);

  ADD_PRIMITIVE(has_ffi);
  return TRUE;
}
