/*
 * promise.c	-- Implementation of promises
 *
 * Copyright © 2000-2011 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *            Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date:  2-Jun-1993 12:27 (eg)
 * Last file update: 19-Aug-2011 18:01 (eg)
 */
#include <stklos.h>

struct promise_obj {
  stk_header header;
  SCM expr;
};

#define PFORCED 1

#define PROMISEP(x)	  	(BOXED_TYPE_EQ((x), tc_promise))
#define PROMISE_EXPR(x)   	(((struct promise_obj *) (x))->expr)
#define PROMISE_RESULT_READY(x) (BOXED_INFO(x))

#define RESULT_READYP(x)	(PROMISE_RESULT_READY(x) & 1)


DEFINE_PRIMITIVE("%make-promise", make_promise, subr1, (SCM expr))
{
  SCM z;
  NEWCELL(z, promise);
  PROMISE_EXPR(z) = expr;
  return z;
}

/*
<doc force
 * (force promise)
 *
 * Forces the value of |promise| (see ,(ref :mark "delay")). If no value has been
 * computed for the promise, then a value is computed and
 * returned. The value of the promise is cached (or "memoized") so
 * that if it is forced a second time, the previously computed value
 * is returned.
 *
 * @lisp
 * (force (delay (+ 1 2)))        =>  3
 * (let ((p (delay (+ 1 2))))
 *   (list (force p) (force p)))  =>  (3 3)
 *
 * (define a-stream
 *   (letrec ((next (lambda (n)
 *                    (cons n (delay (next (+ n 1)))))))
 *     (next 0)))
 * (define head car)
 * (define tail (lambda (stream) (force (cdr stream))))
 *
 * (head (tail (tail a-stream)))  =>  2
 * @end lisp
 *
 * |Force| and |delay| are mainly intended for programs written in
 * functional style. The following examples should not be considered
 * to illustrate good programming style, but they illustrate the
 * property that only one value is computed for a promise, no matter
 * how many times it is forced.
 * @lisp
 * (define count 0)
 * (define p (delay (begin (set! count (+ count 1))
 *                         (if (> count x)
 *                             count
 *                             (force p)))))
 * (define x 5)
 * p                     =>  a promise
 * (force p)             =>  6
 * p                     =>  a promise, still
 * (begin (set! x 10)
 *        (force p))     =>  6
 * @end lisp
 * ,(bold "Note:") See R5RS for details on a posssible way to implement
 * |force| and |delay|.
doc>
*/
DEFINE_PRIMITIVE("force", force, subr1, (SCM promise))
{
  SCM z;

  if (!PROMISEP(promise)) return promise;

  if (RESULT_READYP(promise))
    /* promise was already evaluated. It's expr field contains the result */
    return PROMISE_EXPR(promise);

  z = STk_C_apply(PROMISE_EXPR(promise), 0);

  if (RESULT_READYP(promise))
    /* R5RS: "A promise may refer to its own value.... Forcing such
     * a promise may cause the promise to be forced a second time before
     * the first value has been computed.
     */
    return PROMISE_EXPR(promise);
  else {
    PROMISE_EXPR(promise)         = z;
    PROMISE_RESULT_READY(promise) = 1;
    return z;
  }
}


/*
<doc EXT promise?
 * (promise? obj)
 *
 *  Returns |#t| if |obj| is a promise, otherwise returns |#f|.
doc>
*/
DEFINE_PRIMITIVE("promise?", promisep, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(PROMISEP(obj));
}


/* ====================================================================== */

static void print_promise(SCM promise, SCM port, int mode)
{
  char buffer[100];

  sprintf(buffer, "#[promise %lx (%sforced)]", (unsigned long) promise,
	  RESULT_READYP(promise)? "" : "not ");
  STk_puts(buffer, port);
}


static struct extended_type_descr xtype_promise = {
  "promise",
  print_promise
};


int STk_init_promise(void)
{
  /* register the extended type type for promises */
  DEFINE_XTYPE(promise,   &xtype_promise);

  /* Add primitives */
  ADD_PRIMITIVE(make_promise);
  ADD_PRIMITIVE(force);
  ADD_PRIMITIVE(promisep);

  return TRUE;
}
