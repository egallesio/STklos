/*
 * promise.c    -- Implementation of promises
 *
 * Copyright © 2000-2019 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 * Last file update: 14-Oct-2019 16:03 (eg)
 */
#include "stklos.h"

/*======================================================================
 He is a complete implementation in Scheme (adapted from Eli Barzilay one)

(define-struct promise p)
(define (set-promise-p! p v) (set! (promise-p p) v))
(define %make-promise make-promise)

(define promise-p      %promise-value)
(define set-promise-p! %promise-value-set!)

(define-syntax delay-force
  (syntax-rules ()
    [(delay-force exp) (%make-promise (lambda () exp))]))

(define-syntax delay
  (syntax-rules ()
    [(delay exp) (delay-force (%make-promise (list exp)))]))

(define (make-promise expr)    ;; R7RS one
  (if (promise? expr)
      expr
      (delay expr)))

(define (force promise)
  (if (promise? promise)
      (let ([p (promise-p promise)])
        (cond
          [(procedure? p)
           (let ([promise* (p)])
             (unless (pair? (promise-p promise))
               (if (promise? promise*)
                   (begin (set-promise-p! promise (promise-p promise*))
                          (set-promise-p! promise* promise))
                   (set-promise-p! promise (list promise*))))
             (force promise))]
          [(pair? p)
           (car p)]
          [(promise? p)   ;; <---
           (force p)]
          [else
           (error "Invalid promise, contains" p)]))
      promise))

Note:
   https://srfi-email.schemers.org/srfi-45/msg/2762169/ indicate an optimisation:
   replace  the code signaled by ; <--- by
   [(promise? p)
     (let* ((v (force p)))
       (if (not (pair? (promise-p prom)))
         (set-promise-p! prom (list v)))
       (car (promise-p prom)))]

Since I don't understand the problem and its solution, I stick with Eli solution.

**********************************************************************/

struct promise_obj {
  stk_header header;
  SCM val;
};


#define PROMISEP(x)            (BOXED_TYPE_EQ((x), tc_promise))
#define PROMISE_VAL(x)         (((struct promise_obj *) (x))->val)


DEFINE_PRIMITIVE("%make-promise", make_promise, subr1, (SCM proc))
{
  SCM z;
  NEWCELL(z, promise);
  //  PROMISE_DONEP(z) = donep;
  PROMISE_VAL(z)  = proc;
  return z;
}

#ifdef STK_DEBUG
DEFINE_PRIMITIVE("%promise-value", promise_val, subr1, (SCM p))
{
  if (!PROMISEP(p)) STk_error("bad promise ~S", p);
  return PROMISE_VAL(p);
}

DEFINE_PRIMITIVE("%promise-value-set!", promise_val_set, subr2, (SCM p, SCM v))
{
  if (!PROMISEP(p)) STk_error("bad promise ~S", p);
  PROMISE_VAL(p) = v;
  return STk_void;
}
#endif


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
  SCM p;
 Top:
  if (!PROMISEP(promise)) return promise;

  p = PROMISE_VAL(promise);

  if (CLOSUREP(p)) {
    SCM pstar = STk_C_apply(p, 0);

    if (!CONSP(PROMISE_VAL(promise))) {
      if (PROMISEP(pstar)) {
        PROMISE_VAL(promise) = PROMISE_VAL(pstar);
        PROMISE_VAL(pstar)   = promise;
      } else {
        PROMISE_VAL(promise) = LIST1(pstar);
      }
    }
    goto Top;
  } else if (CONSP(p)) {
    return CAR(p);
  } else if (PROMISEP(p)) {
    promise = p;
    goto Top;
  }
  STk_error("bad promise content: ~S", p);
  return STk_void;              /* for the compiler */
}

/*
<doc r7rs promise?
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
  sprintf(buffer, "#[promise %lx]", (unsigned long) promise);
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
  ADD_PRIMITIVE(promisep);
  ADD_PRIMITIVE(force);

#ifdef STK_DEBUG
  ADD_PRIMITIVE(promise_val);
  ADD_PRIMITIVE(promise_val_set);
#endif

  return TRUE;
}
