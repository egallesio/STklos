;*---------------------------------------------------------------------*/
;*    Copyright (c) 1997 by Manuel Serrano. All rights reserved.       */
;*                                                                     */
;*                                     ,--^,                           */
;*                               _ ___/ /|/                            */
;*                           ,;'( )__, ) '                             */
;*                          ;;  //   L__.                              */
;*                          '   \    /  '                              */
;*                               ^   ^                                 */
;*                                                                     */
;*                                                                     */
;*    This program is distributed in the hope that it will be useful.  */
;*    Use and copying of this software and preparation of derivative   */
;*    works based upon this software are permitted, so long as the     */
;*    following conditions are met:                                    */
;*           o credit to the authors is acknowledged following         */
;*             current academic behaviour                              */
;*           o no fees or compensation are charged for use, copies,    */
;*             or access to this software                              */
;*           o this copyright notice is included intact.               */
;*      This software is made available AS IS, and no warranty is made */
;*      about the software or its performance.                         */
;*                                                                     */
;*      Bug descriptions, use reports, comments or suggestions are     */
;*      welcome Send them to                                           */
;*        Manuel Serrano -- Manuel.Serrano@cui.unige.ch                */
;*---------------------------------------------------------------------*/
;;;--------------------------------------------------------------------*/
;;;   geffroy/Match3.0/s2cfun.scm ...                                  */
;;;                                                                    */
;;;   Author      :  Jean-Marie Geffroy                                */
;;;   Creation    :  Wed Mar 10 14:48:39 1993                          */
;;;   Last change :  Mon May  3 17:50:00 1993  (geffroy)               */
;;;                                                                    */
;;;   Some non-standard utilities...                                   */
;;;--------------------------------------------------------------------*/

(module __match_s2cfun

   (import  (__error                   "Llib/error.scm"))
   
   (use     (__type                    "Llib/type.scm")
            (__bigloo                  "Llib/bigloo.scm")
            (__tvector                 "Llib/tvector.scm")
            (__structure               "Llib/struct.scm")
            (__tvector                 "Llib/tvector.scm")
            (__rgc                     "Rgc/runtime.scm")
            (__r4_numbers_6_5          "Ieee/number.scm")
            (__r4_numbers_6_5_fixnum   "Ieee/fixnum.scm")
            (__r4_numbers_6_5_flonum   "Ieee/flonum.scm")
            (__r4_characters_6_6       "Ieee/char.scm")
            (__r4_equivalence_6_2      "Ieee/equiv.scm")
            (__r4_booleans_6_1         "Ieee/boolean.scm")
            (__r4_symbols_6_4          "Ieee/symbol.scm")
            (__r4_strings_6_7          "Ieee/string.scm")
            (__r4_pairs_and_lists_6_3  "Ieee/pair-list.scm")
            (__r4_input_6_10_2         "Ieee/input.scm")
            (__r4_control_features_6_9 "Ieee/control.scm")
            (__r4_vectors_6_8          "Ieee/vector.scm")
            (__r4_ports_6_10_1         "Ieee/port.scm")
            (__r4_output_6_10_3        "Ieee/output.scm")
            (__evenv                   "Eval/evenv.scm"))
            
  (export   (atom? e)
            (concat . args)
            jim-gensym
            (andmap p . args)
            (ormap p . args)))

;;; Some non-standard utilities
(define (atom? e)
  (not (pair? e)) )

(define (concat . args)
  (string->symbol 
   (apply string-append
          (map (lambda (s)
                    (cond ((string? s) s)
                          ((symbol? s) (symbol->string s))
                          ((number? s) (number->string s))
                          (else (error 'concat "" args)) ) )
                  args ) ) ) )

(define jim-gensym
  (let ((counter 100))
    (lambda args
      (set! counter (+ counter 1))
      (concat (if (pair? args) (car args) 'G)
              counter ) ) ) )

(define (andmap p . args)
  ;; use "first-finish" rule
  (let andmap ((args args) (value #t))
    (if (let any-at-end? ((ls args))
          (and (pair? ls)
               (or (not (pair? (car ls)))
                   (any-at-end? (cdr ls)))))
        value
        (let ((value (apply p (map car args))))
          (and value (andmap (map cdr args) value))))))

; ORMAP
(define (ormap p . args)
  ;; use "first-finish" rule
  (if (= (length args) 1)
      (member #t (map p (car args)))
      (let ormap ((args args) (value #f))
        (if (let any-at-end? ((ls args))
              (and (pair? ls)
                   (or (not (pair? (car ls)))
                       (any-at-end? (cdr ls)))))
            value
            (let ((value (apply p (map car args))))
              (or value (ormap (map cdr args) value)))))))

