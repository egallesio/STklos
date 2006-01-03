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
;;;   geffroy/Match3.0/expand.scm ...                                  */
;;;                                                                    */
;;;   Author      :  Jean-Marie Geffroy                                */
;;;   Creation    :  Wed Mar 10 13:21:53 1993                          */
;;;   Last change :  Tue Jun  8 10:41:34 1993  (geffroy)               */
;;;                                                                    */
;;;   An expanser for the MATCH-LAMBDA and MATCH-CASE forms            */
;;;--------------------------------------------------------------------*/


;;;--------------------------------------------------------------------*/
;;;    (match-lambda                                                   */
;;;       (f1 e1 e2 ...)                                               */
;;;       (f2 e21 ...)                                                 */
;;;       (else e ...))                                                */
;;;    the else clause being optional                                  */
;;;    expands into (lambda (e) ...)                                   */
;;;                                                                    */
;;;   (match-case <exp>                                                */
;;;      (f1 e1 e2 ...)                                                */
;;;      (f2 e21 ...)                                                  */
;;;      (else e ...))                                                 */
;;;   expands into ((lambda (e) ...) e)                                */
;;;--------------------------------------------------------------------*/

(module __match_expand

   (export  (expand-match-case   exp)
	    (expand-match-lambda exp))

   (import  (__error                   "Llib/error.scm")
	    (__match_compiler          "Match/compiler.scm")
	    (__match_descriptions      "Match/descr.scm")
	    (__match_normalize         "Match/normalize.scm")
	    (__match_s2cfun            "Match/s2cfun.scm"))
   
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
	    (__evenv                   "Eval/evenv.scm")))
	    
;;;--------------------------------------------------------------------*/
;;;   Technical note: the clauses->pattern function returns two        */
;;;   results:                                                         */
;;;   - the normalized pattern, (tagged-or f1 tag1 (t-or ...))         */
;;;   - an environment tag -> action*                                  */
;;;   and is therefore written in CPS.                                 */
;;;--------------------------------------------------------------------*/
(define (expand-match-lambda exp)
   (labels ((clauses->pattern
	     (clauses k)
	     (if (null? clauses)
		 (k '(not (any)) *the-empty-env*)
		 (let ((pattern (caar clauses))
		       (actions (cdar clauses))
		       (rest    (cdr clauses)))
		    (let ((tag (jim-gensym "TAG-")))
		       (if (eq? pattern 'else)
			   (k `(tagged-or (any) ,tag (not (any)))
			      (extend *the-empty-env* tag actions))
			   (clauses->pattern
			    rest
			    (lambda (pat env)
			       (k `(tagged-or ,(normalize-pattern pattern)
					      ,tag     
					      ,pat)
				  (extend env tag actions))))))))))
;;; 	     (match-case clauses  */
;;; 		(() (k '(not (any)) *the-empty-env*))  */
;;; 		(( (?pattern . ?actions) . ?rest )  */
;;; 		 (let ((tag (jim-gensym "TAG-")))  */
;;; 		    (if (eq? pattern 'else)  */
;;; 			(k `(tagged-or (any) ,tag (not (any)))  */
;;; 			   (extend *the-empty-env* tag actions))  */
;;; 			(clauses->pattern  */
;;; 			 rest  */
;;; 			 (lambda (pat env)  */
;;; 			    (k `(tagged-or ,(normalize-pattern pattern)  */
;;; 					   ,tag       */
;;; 					   ,pat)  */
;;; 			       (extend env tag actions))))))))))  */
      (clauses->pattern
       (cdr exp)
       (lambda (pat env)
	  (let ((compiled-pat (pcompile pat))
		(prototypes   (fetch-prototypes pat)) )
	         ;; We build a (labels ((tag1 (x ...) actions1)) ...)
		 ;; You may change it to build a letrec
	     `(labels
		    (,@(map
			(lambda (prototype)
			   (cons (car prototype)
				 (cons (cadr prototype)
				       (cdr (assq (car prototype)
						  env)))))
			prototypes))
		 ,compiled-pat))))))

(define (fetch-prototypes pat)
   (if (memq (car pat) '(t-or tagged-or))
       (cons `(,(caddr pat) ,(pattern-variables (cadr pat)))
	     (fetch-prototypes (cadddr pat)))
       '()))

(define (expand-match-case exp)
  (list (expand-match-lambda `(match-lambda . ,(cddr exp)))
        (cadr exp)))

(define (extend env pt im)
   (cons (cons pt im) env))

(define *the-empty-env* '())

