/*
 * parameter.c  -- Parameter Objects (SRFI-39)
 *
 * Copyright © 2003-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 * Last file update: 11-Aug-2020 18:17 (eg)
 */


#include "stklos.h"
#include "vm.h"


struct parameter_obj {
  stk_header header;
  int C_type;           /* 0: parameter is expressed in Scheme                    */
                        /* 1: Converter is expressed in C rather than in Scheme   */
                        /* 2: idem and getter is a procedure to call to get value */
  SCM value;
  SCM converter;
  SCM (*getter)(void);  /* Used only for type 2 parameter objects */
  SCM dynenv;           /* an A-list ((thr1 . val) ...) to look before returning value */
};

#define PARAMETERP(o)       (BOXED_TYPE_EQ((o), tc_parameter))
#define PARAMETER_C_TYPE(p) (((struct parameter_obj *) (p))->C_type)
#define PARAMETER_VALUE(p)  (((struct parameter_obj *) (p))->value)
#define PARAMETER_CONV(p)   (((struct parameter_obj *) (p))->converter)
#define PARAMETER_GETTER(p) (((struct parameter_obj *) (p))->getter)
#define PARAMETER_DYNENV(p) (((struct parameter_obj *) (p))->dynenv)

/*===========================================================================*\
 *
 *  Utilities
 *
\*===========================================================================*/

static void error_bad_parameter(SCM obj)
{
  STk_error("bad parameter ~S", obj);
}


SCM STk_get_parameter(SCM param)
{
  SCM tmp;

  if (!PARAMETERP(param)) error_bad_parameter(param);

  tmp = STk_int_assq(STk_current_thread(), PARAMETER_DYNENV(param));

  if (PARAMETER_C_TYPE(param) == 2)
    return (tmp != STk_false) ? CDR(tmp) : PARAMETER_GETTER(param)();
  else
    return (tmp != STk_false) ? CDR(tmp) : PARAMETER_VALUE(param);
}

SCM STk_set_parameter(SCM param, SCM value)
{
  SCM conv, tmp, new;

  if (!PARAMETERP(param)) error_bad_parameter(param);

  conv = PARAMETER_CONV(param);

  if (PARAMETER_C_TYPE(param)) {
    /* We have a C converter (which is always present) */
    //new = (conv != STk_false) ? ((SCM (*) (SCM))conv)(value): value;
    new = ((SCM (*) (SCM))conv)(value);
  } else {
    /* We have a Scheme converter */
    new = (conv != STk_false) ? STk_C_apply(conv,1,value): value;
  }

  tmp  = STk_int_assq(STk_current_thread(), PARAMETER_DYNENV(param));
  if (tmp != STk_false)
    CDR(tmp) = new;
  else {
    PARAMETER_VALUE(param) = new;
  }

  return STk_void;
}

SCM STk_make_C_parameter(SCM symbol, SCM value, SCM (*conv)(SCM new_value),
             SCM module)
{
  SCM z;

  NEWCELL(z, parameter);
  PARAMETER_C_TYPE(z) = 1;
  PARAMETER_VALUE(z)  = conv(value);
  PARAMETER_CONV(z)   = (SCM) conv;
  PARAMETER_GETTER(z) = STk_void;
  PARAMETER_DYNENV(z) = STk_nil;

  /* Bind it to the given symbol */
  STk_define_variable(STk_intern(symbol), z, module);

  return z;
}

SCM STk_make_C_parameter2(SCM symbol, SCM (*getter)(void), SCM (*conv)(SCM new_value),
              SCM module)
{
  SCM z;

  NEWCELL(z, parameter);
  PARAMETER_C_TYPE(z) = 2;
  PARAMETER_VALUE(z)  = getter();
  PARAMETER_CONV(z)   = (SCM) conv;
  PARAMETER_GETTER(z) = getter;
  PARAMETER_DYNENV(z) = STk_nil;

  /* Bind it to the given symbol */
  STk_define_variable(STk_intern(symbol), z, module);

  return z;
}



/*===========================================================================*\
 *
 *  Primitives
 *
\*===========================================================================*/

/*
<doc EXT make-parameter
 * (make-parameter init)
 * (make-parameter init converter)
 *
 * Returns a new parameter object which is bound in the global dynamic
 * environment to a cell containing the value returned by the call
 * |(converter init)|. If the conversion procedure |converter| is not
 * specified the identity function is used instead.
 * @l
 * The parameter object is a procedure which accepts zero or one
 * argument. When it is called with no argument, the content of the
 * cell bound to this parameter object in the current dynamic
 * environment is returned. When it is called with one argument, the
 * content of the cell bound to this parameter object in the current
 * dynamic environment is set to the result of the call
 * |(converter arg)|, where |arg| is the argument passed to the
 * parameter object, and
 * an unspecified value is returned.
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
  PARAMETER_VALUE(z)  = v;
  PARAMETER_CONV(z)   = (conv) ? conv : STk_false;
  PARAMETER_GETTER(z) = STk_void;
  PARAMETER_DYNENV(z) = STk_nil;

  return z;
}


/*
<doc EXT parameter?
 * (parameter? obj)
 *
 *  Returns |#t| if |obj| is a parameter object, otherwise returns |#f|.
doc>
 */
DEFINE_PRIMITIVE("parameter?", parameterp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(PARAMETERP(obj));
}



DEFINE_PRIMITIVE("%parameter-dynenv-push!", parameter_dynenv_push, subr1, (SCM param))
{
  if (!PARAMETERP(param)) error_bad_parameter(param);

  PARAMETER_DYNENV(param) = STk_cons(STk_cons(STk_current_thread(), STk_void),
                                     PARAMETER_DYNENV(param));
  return STk_void;
}

DEFINE_PRIMITIVE("%parameter-dynenv-pop!", parameter_dynenv_pop, subr1, (SCM param))
{
  SCM prev, tmp, thr;

  if (!PARAMETERP(param)) error_bad_parameter(param);

  thr = STk_current_thread();
  for (prev = tmp = PARAMETER_DYNENV(param); !NULLP(tmp); prev=tmp, tmp=CDR(tmp)) {
    if (CAR(CAR(tmp)) == thr) {
      if (tmp == prev) PARAMETER_DYNENV(param) = CDR(tmp);
      else CDR(prev) = CDR(tmp);
    }
  }
  return STk_void;
}



/*===========================================================================*\
 *
 *  Initialization code
 *
\*===========================================================================*/

static struct extended_type_descr xtype_parameter = { "parameter", NULL };

int STk_init_parameter(void)
{
  /* Define type for parameters */
  DEFINE_XTYPE(parameter, &xtype_parameter);

  /* Define primitives */
  ADD_PRIMITIVE(make_parameter);
  ADD_PRIMITIVE(parameterp);
  ADD_PRIMITIVE(parameter_dynenv_push);
  ADD_PRIMITIVE(parameter_dynenv_pop);

  return TRUE;
}
