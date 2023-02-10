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

;; NOTE: Modification by eg for STklos.  Renamed extend in this file by
;; extend-it (we have an extend function in compiler.scm, which prevent to
;; place all the files of this directory in a unique module (instead of 5)

(module __match_expand

   (import  __error
            __match_compiler
            __match_descriptions
            __match_normalize
            __match_s2cfun
            __param
            __bexit
            __object
            __thread)

   (use     __type
            __bigloo
            __tvector
            __structure
            __tvector
            __bignum
            __rgc
            __bit

            __r4_numbers_6_5
            __r4_numbers_6_5_fixnum
            __r4_numbers_6_5_flonum
            __r4_numbers_6_5_flonum_dtoa
            __r4_characters_6_6
            __r4_equivalence_6_2
            __r4_booleans_6_1
            __r4_symbols_6_4
            __r4_strings_6_7
            __r4_pairs_and_lists_6_3
            __r4_input_6_10_2
            __r4_control_features_6_9
            __r4_vectors_6_8
            __r4_ports_6_10_1
            __r4_output_6_10_3
            __evenv)

   (export  (expand-match-case   exp)
            (expand-match-lambda exp)))

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
             (cond
                ((null? clauses)
                 (k '(not (any)) *the-empty-env*))
                ((not (pair? (car clauses)))
                 (error 'match-case "Illegal expression" exp))
                (else
                 (let ((pattern (caar clauses))
                       (actions (cdar clauses))
                       (rest    (cdr clauses)))
                    (let ((tag (jim-gensym "TAG-")))
                       (if (eq? pattern 'else)
                           (k `(tagged-or (any) ,tag (not (any)))
                              (extend-it *the-empty-env* tag actions))
                           (clauses->pattern
                            rest
                            (lambda (pat env)
                               (k `(tagged-or ,(normalize-pattern pattern)
                                              ,tag
                                              ,pat)
                                  (extend-it env tag actions)))))))))))
      (clauses->pattern
       (cdr exp)
       (lambda (pat env)
         (let ((compiled-pat (pcompile pat))
               (prototypes   (fetch-prototypes pat)) )
            ;; We build a (labels ((tag1 (x ...) actions1)) ...)
            ;;
            ;; STklos: replace the `labels` produced here by a `letrec`
            ;; to avoid to export labels
            ;;
            ;; Original code follows
            ;; You may change it to build a letrec
            ;; `(labels
            ;;   (,@(map
            ;;       (lambda (prototype)
            ;;         (let ((body (cdr (assq (car prototype) env))))
            ;;           (if (null? body)
            ;;               (error 'match-case "Illegal expression" exp)
            ;;               (cons (car prototype)
            ;;                     (cons (cadr prototype) body)))))
            ;;       prototypes))
            ;;   ,compiled-pat)
            `(letrec ,(map (lambda (prototype)
                             (let ((body (cdr (assq (car prototype) env))))
                               (if (null? body)
                                   (error 'match-case "Illegal expression" exp)
                                   (list (car prototype) `(lambda ,(cadr prototype) ,@body)))))
                           prototypes)
               ,compiled-pat))))))

(define (fetch-prototypes pat)
   (if (memq (car pat) '(t-or tagged-or))
       (cons `(,(caddr pat) ,(pattern-variables (cadr pat)))
             (fetch-prototypes (cadddr pat)))
       '()))

#|STklos: replaced by identity
(define (epairify p ep)
   (if (epair? ep)
       (econs (car p) (cdr p) (cer ep))
       p))
|#
(define (epairify p ep) p)

(define (expand-match-case exp)
  (list (expand-match-lambda (epairify `(match-lambda . ,(cddr exp)) exp))
        (cadr exp)))

(define (extend-it env pt im)
   (cons (cons pt im) env))

(define *the-empty-env* '())
