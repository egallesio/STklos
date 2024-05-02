/*
 * parameter.c  -- Parameter Objects (SRFI-39)
 *
 * Copyright © 2003-2023 Erick Gallesio <eg@stklos.net>
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
 *    Creation date:  1-Jul-2003 11:38 (eg)
 */


#include "stklos.h"
#include "vm.h"


struct parameter_obj {
  stk_header header;
  int C_type;           /* 0: parameter is expressed in Scheme                    */
                        /* 1: Converter is expressed in C rather than in Scheme   */
                        /* 2: idem and getter is a procedure to call to get value */
  SCM name;             /* #f, a string or a symbol */
  SCM value;
  SCM converter;
  SCM (*getter)(void);  /* Used only for type 2 parameter objects */
  MUT_FIELD(mtx);       /* mutex use for setting the value of the parameter object */
};

#define PARAMETERP(o)       (BOXED_TYPE_EQ((o), tc_parameter))
#define PARAMETER_C_TYPE(p) (((struct parameter_obj *) (p))->C_type)
#define PARAMETER_NAME(p)   (((struct parameter_obj *) (p))->name)
#define PARAMETER_VALUE(p)  (((struct parameter_obj *) (p))->value)
#define PARAMETER_CONV(p)   (((struct parameter_obj *) (p))->converter)
#define PARAMETER_GETTER(p) (((struct parameter_obj *) (p))->getter)
#ifdef THREADS_NONE
#  define PARAMETER_MUTEX(p)
#else
#  define PARAMETER_MUTEX(p) (((struct parameter_obj *) (p))->mtx)
#endif

/*===========================================================================*\
 *
 *  Utilities
 *
\*===========================================================================*/


static inline void verify_parameter(SCM obj)
{
  if (!PARAMETERP(obj)) STk_error("bad parameter ~S", obj);
}


SCM STk_get_parameter(SCM param)
{
  verify_parameter(param);
  return (PARAMETER_C_TYPE(param) == 2) ?
    PARAMETER_GETTER(param)():
    PARAMETER_VALUE(param);
}

SCM STk_set_parameter(SCM param, SCM value)
{
  SCM conv, new;

  verify_parameter(param);

  conv = PARAMETER_CONV(param);

  if (PARAMETER_C_TYPE(param)) {
    /* We have a C converter (which is always present) */
    new = ((SCM (*) (SCM))conv)(value);
  } else {
    /* We have a Scheme converter */
    new = (conv != STk_false) ? STk_C_apply(conv,1,value): value;
  }

  MUT_LOCK(PARAMETER_MUTEX(param));
  PARAMETER_VALUE(param) = new;
  MUT_UNLOCK(PARAMETER_MUTEX(param));

  return STk_void;
}

SCM STk_make_C_parameter(char *name, SCM value, SCM (*conv)(SCM new_value),
                         SCM module)
{
  SCM z;

  NEWCELL(z, parameter);
  PARAMETER_C_TYPE(z) = 1;
  PARAMETER_NAME(z)   = STk_Cstring2string(name);
  PARAMETER_VALUE(z)  = conv(value);
  PARAMETER_CONV(z)   = (SCM) conv;
  PARAMETER_GETTER(z) = STk_void;
  MUT_INIT(PARAMETER_MUTEX(z));

  /* Bind it to the given symbol */
  STk_define_variable(STk_intern((char *)name), z, module);

  return z;
}

SCM STk_make_C_parameter2(char *name, SCM (*getter)(void),
                          SCM (*conv)(SCM new_value), SCM module)
{
  SCM z;

  NEWCELL(z, parameter);
  PARAMETER_C_TYPE(z) = 2;
  PARAMETER_NAME(z)   = STk_Cstring2string(name);
  PARAMETER_VALUE(z)  = getter();
  PARAMETER_CONV(z)   = (SCM) conv;
  PARAMETER_GETTER(z) = getter;
  MUT_INIT(PARAMETER_MUTEX(z));

  /* Bind it to the given symbol */
  STk_define_variable(STk_intern(name), z, module);

  return z;
}

static void print_parameter(SCM param, SCM port, int _UNUSED(mode))
{
  SCM name = PARAMETER_NAME(param);

  STk_fprintf(port, "#[parameter ");
  if (name != STk_false)
    STk_fprintf(port, "%s]", STRINGP(name)? STRING_CHARS(name): SYMBOL_PNAME(name));
  else
    STk_fprintf(port, "%lx]", param);
}

/*===========================================================================*\
 *
 *  Primitives
 *
\*===========================================================================*/

DEFINE_PRIMITIVE("%parameter-name", parameter_name, subr1, (SCM obj))
{
  verify_parameter(obj);
  {
    SCM name = PARAMETER_NAME(obj);
    return SYMBOLP(name) ? STk_Cstring2string(SYMBOL_PNAME(name)) : name;
  }
}

DEFINE_PRIMITIVE("%set-parameter-name!", set_parameter_name, subr2, (SCM obj, SCM n))
{
  verify_parameter(obj);
  if (!SYMBOLP(n) && !STRINGP(n)) STk_error("bad parameter name ~S", n);
  PARAMETER_NAME(obj) = n;
  return STk_void;
}



/*
<doc EXT make-parameter
 * (make-parameter init)
 * (make-parameter init converter)
 *
 * Returns a new parameter object which is bound in the global dynamic
 * environment to a cell containing the value returned by the call
 * |(converter init)|. If the conversion procedure |converter| is not
 * specified the identity function is used instead.
 *
 * The parameter object is a procedure which accepts zero or one
 * argument. When it is called with no argument, the content of the
 * cell bound to this parameter object in the current dynamic
 * environment is returned. When it is called with one argument, the
 * content of the cell bound to this parameter object in the current
 * dynamic environment is set to the result of the call
 * |(converter arg)|, where |arg| is the argument passed to the
 * parameter object, and an unspecified value is returned.
 *
 * @lisp
 * (define radix
 *     (make-parameter 10))
 *
 * (define write-shared
 *    (make-parameter
 *       #f
 *       (lambda (x)
 *         (if (boolean? x)
 *             x
 *             (error 'write-shared "bad boolean ~S" x)))))
 *
 *  (radix)           =>  10
 *  (radix 2)
 *  (radix)           =>  2
 *  (write-shared 0)  => error
 *
 *  (define prompt
 *    (make-parameter
 *      123
 *      (lambda (x)
 *        (if (string? x)
 *            x
 *            (with-output-to-string (lambda () (write x)))))))
 *
 *  (prompt)       =>  "123"
 *  (prompt ">")
 *  (prompt)       =>  ">"
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("make-parameter", make_parameter, subr12, (SCM value, SCM conv))
{
  SCM z, v;

  if (conv && STk_procedurep(conv) == STk_false)
    STk_error("bad conversion function ~S", conv);

  /* initialize v with (conv value) */
  v = (conv) ? STk_C_apply(conv, 1, value): value;

  NEWCELL(z, parameter);
  PARAMETER_C_TYPE(z) = 0;
  PARAMETER_NAME(z)   = STk_false;
  PARAMETER_VALUE(z)  = v;
  PARAMETER_CONV(z)   = (conv) ? conv : STk_false;
  PARAMETER_GETTER(z) = STk_void;
  MUT_INIT(PARAMETER_MUTEX(z));

  return z;
}


/*
<doc EXT parameter?
 * (parameter? obj)
 *
 * Returns |#t| if |obj| is a parameter object, otherwise returns |#f|.
doc>
 */
DEFINE_PRIMITIVE("parameter?", parameterp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(PARAMETERP(obj));
}


/*===========================================================================* \
 *
 *  Initialization code
 *
\*===========================================================================*/

static struct extended_type_descr xtype_parameter = {
  .name = "parameter",
  .print = print_parameter
};

int STk_init_parameter(void)
{
  /* Define type for parameters */
  DEFINE_XTYPE(parameter, &xtype_parameter);

  /* Define primitives */
  ADD_PRIMITIVE(make_parameter);
  ADD_PRIMITIVE(parameterp);
  ADD_PRIMITIVE(parameter_name);
  ADD_PRIMITIVE(set_parameter_name);

  return TRUE;
}
