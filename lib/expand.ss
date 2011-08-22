;;; expand.ss						-*- Scheme -*-
;;; Robert Hieb & Kent Dybvig
;;; 92/06/18

;;; Modified by Erick Gallesio for STklos
;;; Last file update: 27-Jan-2001 12:55 (eg)


;;; Copyright (C) 1992 R. Kent Dybvig
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.  This software
;;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;;; NATURE WHATSOEVER.


;;; notes:

; "begin" is treated as a splicing construct at top level and at
; the beginning of bodies.  Any sequence of expressions that would
; be allowed where the "begin" occurs is allowed.

; Objects with no standard print syntax are allowed in quoted data.
; However, objects with cycles cause nontermination.

; All identifiers that don't have macro definitions and are
; not bound lexically are assumed to be global variables

; Top-level definitions of macro-introduced identifiers are allowed.
; This may not be appropriate for implementations in which the
; model is that bindings are created by definitions, as opposed to
; one in which initial values are assigned by definitions.

; Identifiers and syntax objects are implemented as vectors.
; They should be implemented in terms of some sort of
; (implementation-dependent) opaque structures.

; This code assumes that it is possible to generate globally unique
; symbols.  If not, new symbols can be replaced with other unique
; objects during expansion and the resulting identifiers can be fed
; directly into the compiler or replaced with locally unique symbols in
; a post-pass.

(begin

(let ()

; syntax objects---an opaque structure should really be used here

(define syntax-object?
   (lambda (x)
      (and (vector? x)
           (= (vector-length x) 3)
           (eq? (vector-ref x 0) 'syntax-object))))

(define make-syntax-object
   (lambda (expression wrap)
      (vector 'syntax-object expression wrap)))

(define syntax-object-expression (lambda (x) (vector-ref x 1)))

(define syntax-object-wrap (lambda (x) (vector-ref x 2)))

;;; reporting errors --- "error-hook" must be supplied

(define scope-error
   (lambda (id)
      (syntax-error id "invalid context for bound identifier")))

(define id-error
   (lambda (x)
      (syntax-error x "invalid context for identifier")))

(define arg-check
   (lambda (pred? x who)
      (if (not (pred? x)) (error-hook who "invalid argument" x))))


;;; identifier bindings

; <binding> ::= (macro . <procedure>) |              macros
;               (core . <procedure>) |               core forms
;               (special . <procedure>) |            top-level sensitive core
;               (syntax . <level>) |                 pattern variables
;               (global) |                           global variables
;               (global-unbound) |                   unbound globals
;               (lexical) |                          lexical variables
;               (displaced-lexical)                  displaced lexicals

;;; a macro is a user-defined syntactic-form.  a core is a system-defined
;;; syntactic form.  a special is a system-defined syntactic form that
;;; is sensitive to whether it is at top-level, i.e., define-syntax,
;;; define, and begin.

;;; a pattern variable is a variable introduced by syntax-case and can
;;; be referenced only within a syntax form.

;;; a global variable is a variable for which a top-level definition
;;; has been seen (and no subsequent top-level macro definition).
;;; a global-unbound variable is one for which no top-level definition
;;; of any kind has been seen.  Both classes of globals are treated
;;; as global references.

;;; a lexical variable is a lambda- or letrec-bound variable.

;;; a displaced-lexical identifier is a lexical identifier removed from
;;; it's scope by the return of a syntax object containing the identifier.
;;; a displaced lexical can also appear when a letrec-syntax-bound
;;; keyword is referenced on the rhs of one of the letrec-syntax clauses.
;;; a displaced lexical should never occur with properly written macros.

(define binding-type car)

(define binding-value cdr)


;;; global macro environments

; "put-global-definition-hook" & "get-global-definition-hook" must be supplied.

(define extend-global-env
   (lambda (sym binding)
      (put-global-definition-hook sym binding)))

(define global-lookup
   (lambda (sym)
      (or (get-global-definition-hook sym) '(global-unbound))))


;;; lexical compile-time environments

; <environment>              ::= (<element>*)
; <element>                  ::= <variable> |
;                                (<symbol> . <pattern-variable binding>) |
;                                (<symbol> . <macro binding>)
; <variable>                 ::= <symbol>
; <pattern-variable binding> ::= (syntax . level)
; <macro binding>            ::= (macro . <transformer>)
; <transformer>              ::= <procedure>
; <level>                    ::= <nonnegative integer>

(define null-env '())

(define extend-macro-env
   (lambda (vars vals r)
      (if (null? vars)
          r
          (cons (cons (car vars) (cons 'macro (car vals)))
                (extend-macro-env (cdr vars) (cdr vals) r)))))

(define extend-var-env append)

(define extend-syntax-env
   (lambda (vars vals r)
      (if (null? vars)
          r
          (cons (cons (car vars) (cons 'syntax (car vals)))
                (extend-syntax-env (cdr vars) (cdr vals) r)))))

(define lookup
   (lambda (name id r)
      (if (eq? name (id-sym-name id))
          (global-lookup name)
          (let search ((r r) (name name))
             (cond
                ((null? r) '(displaced-lexical))
                ((pair? (car r))
                 (if (eq? (caar r) name) (cdar r) (search (cdr r) name)))
                ((eq? (car r) name) '(lexical))
                (else (search (cdr r) name)))))))

(define global-extend
   (lambda (type sym val)
      (extend-global-env sym (cons type val))))


;;; identifiers are symbols or wrapped symbols

(define id?
   (lambda (x)
      (or (symbol? x)
          (and (syntax-object? x) (symbol? (syntax-object-expression x))))))

(define id-sym-name
   (lambda (x)
      (if (symbol? x) x (syntax-object-expression x))))


;;; syntax object wraps

;         <wrap> ::= (<wrap element>*)
; <wrap element> ::= <mark> |
;                    (<old name> <new name> <mark>*)

(define empty-wrap '())
(define top-wrap '(top))

; Marks must be comparable with "eqv?" and distinct from pairs.

(define current-mark 0)

(define new-mark-wrap
   (lambda ()
      (set! current-mark (+ current-mark 1))
      (list current-mark)))

(define make-binding-wrap
   (lambda (ids new-names w)
      (if (null? ids) w (cons (make-wrap-rib ids new-names w) w))))

(define make-wrap-rib
   (lambda (ids new-names w)
      (if (null? ids)
          '()
          (cons (let ((n&m (id-var-name&marks (car ids) w)))
                   (cons (car n&m) (cons (car new-names) (cdr n&m))))
                (make-wrap-rib (cdr ids) (cdr new-names) w)))))

(define join-wraps
   (lambda (w1 w2)
      (cond
         ((null? w2) w1)
         ((null? w1) w2)
         ((pair? (car w2)) (join-wraps1 w1 w2))
         (else (join-wraps2 w1 w2)))))

(define join-wraps1
   (lambda (w1 w2)
      (if (null? w1)
          w2
          (cons (car w1) (join-wraps1 (cdr w1) w2)))))

(define join-wraps2
   (lambda (w1 w2)
      (let ((x (car w1)) (w1 (cdr w1)))
         (if (null? w1)
             (if (and (not (pair? x)) (eqv? x (car w2)))
                 (cdr w2)
                 (cons x w2))
             (cons x (join-wraps2 w1 w2))))))

(define same-marks?
   (lambda (x y)
      (if (null? x)
          (null? y)
          (and (not (null? y))
               (eqv? (car x) (car y))
               (same-marks? (cdr x) (cdr y))))))

(define id-var-name
   (lambda (id w)
      (cond
         ((null? w)
          (if (symbol? id)
              id
              (id-var-name
                 (syntax-object-expression id)
                 (syntax-object-wrap id))))
         ((pair? (car w)) (car (id-var-name&marks id w)))
         (else (id-var-name id (cdr w))))))

(define id-var-name&marks
   (lambda (id w)
      (if (null? w)
          (if (symbol? id)
              (list id)
              (id-var-name&marks
                 (syntax-object-expression id)
                 (syntax-object-wrap id)))
          (let ((n&m (id-var-name&marks id (cdr w))) (first (car w)))
             (if (pair? first)
                 (let ((n (car n&m)))
                    (let search ((rib first))
                       (cond
                          ((null? rib) n&m)
                          ((and (eq? (caar rib) n)
                                (same-marks? (cdr n&m) (cddar rib)))
                           (cdar rib))
                          (else (search (cdr rib))))))
                 (cons (car n&m)
                       (if (or (null? (cdr n&m)) (not (eqv? first (cadr n&m))))
                           (cons first (cdr n&m))
                           (cddr n&m))))))))

(define free-id=?
   (lambda (i j)
       (and (eq? (id-sym-name i) (id-sym-name j))
            (eq? (id-var-name i empty-wrap) (id-var-name j empty-wrap)))))

(define bound-id=?
   (lambda (i j)
       (and (eq? (id-sym-name i) (id-sym-name j))
            (let ((i (id-var-name&marks i empty-wrap))
                  (j (id-var-name&marks j empty-wrap)))
               (and (eq? (car i) (car j))
                    (same-marks? (cdr i) (cdr j)))))))

; "valid-bound-ids?" returns #t if it receives a list of unique ids.
; It is quadratic on the length of the id list;
; long lists could be sorted to make it more efficient.

(define valid-bound-ids?
   (lambda (ids)
      (and (let all-ids? ((ids ids))
              (or (null? ids)
                  (and (id? (car ids))
                       (all-ids? (cdr ids)))))
           (let unique? ((ids ids))
              (or (null? ids)
                  (and (not (bound-id-member? (car ids) (cdr ids)))
                       (unique? (cdr ids))))))))

(define bound-id-member?
   (lambda (x list)
      (and (not (null? list))
           (or (bound-id=? x (car list))
               (bound-id-member? x (cdr list))))))


;;; manipulating syntax (wrapped expressions and identifiers)

(define unwrap
   (lambda (x)
      (if (syntax-object? x)
          (let ((e (syntax-object-expression x)) (w (syntax-object-wrap x)))
             (cond
                ((pair? e) (cons (wrap (car e) w) (wrap (cdr e) w)))
                ((vector? e)
                 (list->vector (map (lambda (x) (wrap x w)) (vector->list e))))
                (else e)))
          x)))

(define wrap
   (lambda (x w)
      (cond
         ((null? w) x)
         ((syntax-object? x)
          (make-syntax-object (syntax-object-expression x)
                              (join-wraps w (syntax-object-wrap x))))
         ((null? x) x)
         (else (make-syntax-object x w)))))


;;; expanding

(define chi-top
   (lambda (e r w)
      (cond
         ((pair? e) (chi-pair e r w chi-top))
         ((syntax-object? e)
          (chi-top (syntax-object-expression e)
                   r
                  (join-wraps w (syntax-object-wrap e))))
         (else (chi e r w)))))

(define chi
   (lambda (e r w)
      (cond
         ((symbol? e)
          (let ((n (id-var-name e w)))
             (chi-ref e n (lookup n e r) w)))
         ((pair? e) (chi-pair e r w chi))
         ((syntax-object? e)
          (chi (syntax-object-expression e)
               r
               (join-wraps w (syntax-object-wrap e))))
         ((or (boolean? e) (number? e) (string? e) (char? e))
          (build-data e))
         (else (syntax-error (wrap e w))))))

(define chi-pair
   (lambda (e r w k)
      (let ((first (car e)) (rest (cdr e)))
         (if (id? first)
             (let ((n (id-var-name first w)))
                (let ((b (lookup n first r)))
                   (case (binding-type b)
                      ((core) ((binding-value b) e r w))
                      ((macro) (chi-macro (binding-value b) e r w k))
                      ((special) ((binding-value b) e r w k))
                      (else (build-application
                               (chi-ref first n b w)
                               (chi-args rest r w e w))))))
             (build-application
                (chi first r w)
                (chi-args rest r w e w))))))

(define chi-macro
   (letrec ((check-macro-output
             (lambda (x)
                (cond ((pair? x) (check-macro-output (car x))
                                 (check-macro-output (cdr x)))
                      ((syntax-object? x))
                      ((vector? x)
                       (let ((n (vector-length x)))
                          (do ((i 0 (+ i 1)))
                              ((= i n))
                              (check-macro-output (vector-ref x i)))))
                      ((symbol? x)
                       (syntax-error x "encountered raw symbol"))))))
      (lambda (p e r w k)
         (let ((mw (new-mark-wrap)))
            (let ((x (p (wrap e (join-wraps mw w)))))
               (check-macro-output x)
               (k x r mw))))))

(define chi-ref
   (lambda (e name binding w)
      (case (binding-type binding)
         ((lexical) (build-lexical-reference name))
         ((global global-unbound) (build-global-reference name))
         (else (id-error (wrap e w))))))

(define chi-args
   (lambda (args r w source source-w)
      (cond
         ((pair? args)
          (cons (chi (car args) r w)
                (chi-args (cdr args) r w source source-w)))
         ((null? args) '())
         ((syntax-object? args)
          (chi-args (syntax-object-expression args)
                    r
                    (join-wraps w (syntax-object-wrap args))
                    source
                    source-w))
         (else (syntax-error (wrap source source-w))))))

(define syntax-type
   (lambda (e r w)
      (cond
         ((syntax-object? e)
          (syntax-type (syntax-object-expression e)
                       r
                       (join-wraps (syntax-object-wrap e) w)))
         ((and (pair? e) (identifier? (car e)))
          (let ((n (id-var-name (car e) w)))
             (let ((b (lookup n (car e) r)))
                (case (binding-type b)
                   ((special)
                    (case n
                      ((define) (cons 'definition (chi-definition e w)))
                      ((define-syntax)
                       (cons 'syntax-definition (chi-syntax-definition e w)))
                      ((begin) (cons 'sequence (chi-sequence e w)))))
                   (else b)))))
         (else '(other)))))

(define chi-body
   ;; should cut off search for definitions when a form (x . e) appears
   ;; where x is a one of the identifiers being defined (bound-id=? to
   ;; an element of var-ids or macro-ids) and use the (partially) expanded
   ;; version of the current form instead of the original version as is
   ;; now used.  this should work for correct code, though it won't catch
   ;; an incorrect use within the definitions.
   (lambda (body source r w)
      (if (null? (cdr body))
          (chi (car body) r w)
          (let parse1 ((body (map (lambda (x) (wrap x w)) body))
                       (var-ids '()) (var-vals '())
                       (macro-ids '()) (macro-vals '()))
             (if (null? body)
                 (syntax-error (wrap source w) "no expressions in body")
                 (let parse2 ((e (car body)))
                    (let ((b (syntax-type e r empty-wrap)))
                       (case (car b)
                          ((macro)
                           (parse2 (chi-macro (binding-value b)
                                              e
                                              r
                                              empty-wrap
                                              (lambda (e r w) (wrap e w)))))
                          ((definition)
                           (parse1 (cdr body)
                                   (cons (cadr b) var-ids)
                                   (cons (caddr b) var-vals)
                                   macro-ids macro-vals))
                          ((syntax-definition)
                           (parse1 (cdr body)
                                   var-ids var-vals
                                   (cons (cadr b) macro-ids)
                                   (cons (caddr b) macro-vals)))
                          ((sequence)
                           (parse1 (append (cdr b) (cdr body))
                                   var-ids var-vals
                                   macro-ids macro-vals))
                          (else
                           (if (valid-bound-ids? (append var-ids macro-ids))
                               (let ((new-var-names (map gen-var var-ids))
                                     (new-macro-names (map gen-var macro-ids)))
                                  (let ((w (make-binding-wrap
                                              (append macro-ids var-ids)
                                              (append new-macro-names
                                                      new-var-names)
                                              empty-wrap)))
                                     (let ((r (extend-macro-env
                                                  new-macro-names
                                                  (map (lambda (x)
                                                          (chi-macro-def x r w))
                                                       macro-vals)
                                                  (extend-var-env
                                                     new-var-names
                                                     r))))
                                        (build-letrec
                                           new-var-names
                                           (map (lambda (x) (chi x r w))
                                                var-vals)
                                           (build-sequence
                                              (map (lambda (x) (chi x r w))
                                                   body))))))
                               (syntax-error (wrap source w)
                                             "invalid identifier")))))))))))

(define chi-local-syntax
   (lambda (e r w)
      (syntax-case e ()
         ((who ((var val) ...) e1 e2 ...)
          (valid-bound-ids? (syntax (var ...)))
          (let ((new-vars (map gen-var (syntax (var ...)))))
             (let ((new-w (make-binding-wrap (syntax (var ...)) new-vars w)))
                (chi-body
                   (syntax (e1 e2 ...))
                   e
                   (extend-macro-env
                      new-vars
                      (let ((w (if (free-id=? (syntax who)
                                              (syntax letrec-syntax))
                                   new-w
                                   w)))
                         (map (lambda (x) (chi-macro-def x r w))
                              (syntax (val ...))))
                      r)
                   new-w))))
         (_ (syntax-error (wrap e w))))))

(define chi-macro-def
   (lambda (def r w)
      (eval-hook (chi def null-env w))))

(define chi-sequence
   (lambda (e w)
      (syntax-case (wrap e w) ()
         ((_ e ...) (syntax (e ...))))))

(define chi-definition
   (lambda (e w)
      (syntax-case (wrap e w) ()
         ((_ (name . args) e1 e2 ...)
          (and (id? (syntax name))
               (valid-bound-ids? (lambda-var-list (syntax args))))
          (syntax (name (lambda args e1 e2 ...))))
         ((_ name val) (syntax (name val)))
         ((_ name)
          (id? (syntax name))
          (syntax (name (void)))))))

(define chi-syntax-definition
   (lambda (e w)
      (syntax-case (wrap e w) ()
         ((_ name val)
          (id? (syntax name))
          (syntax (name val))))))

(define ellipsis?
   (lambda (x)
;[EG] Commented the following line
;     (when (and (top-level-bound? 'dp) dp) (break))
      (and (identifier? x)
           (free-id=? x (syntax (... ...))))))

(define chi-syntax
   (lambda (src exp r w)
      (let gen ((e exp) (maps '()) (k (lambda (e maps) (regen e))))
         (if (id? e)
             (let ((n (id-var-name e w)))
                (let ((b (lookup n e r)))
                   (if (eq? (binding-type b) 'syntax)
                       (let ((level (binding-value b)))
                          (if (< (length maps) level)
                              (syntax-error src "missing ellipsis in")
                              (gen-ref n level maps
                                 (lambda (x maps) (k `(ref ,x) maps)))))
                       (if (ellipsis? (wrap e w))
                           (syntax-error src "invalid context for ... in")
                           (k `(id ,(wrap e w)) maps)))))
             (syntax-case e ()
                ((dots1 dots2)
                 (and (ellipsis? (wrap (syntax dots1) w))
                      (ellipsis? (wrap (syntax dots2) w)))
                 (k `(id ,(wrap (syntax dots1) w)) maps))
                ((x dots . y)
                 (ellipsis? (wrap (syntax dots) w))
                 (gen (syntax y) maps
                    (lambda (y maps)
                       (gen (syntax x) (cons '() maps)
                          (lambda (x maps)
                             (if (null? (car maps))
                                 (syntax-error src "extra ellipsis in")
                                 (k (gen-append (gen-map x (car maps)) y)
                                    (cdr maps))))))))
                ((x . y)
                 (gen (syntax x) maps
                    (lambda (x maps)
                       (gen (syntax y) maps
                          (lambda (y maps) (k (gen-cons x y) maps))))))
                (#(e1 e2 ...)
                 (gen (syntax (e1 e2 ...)) maps
                    (lambda (e maps) (k (gen-vector e) maps))))
                (_ (k `(quote ,(wrap e w)) maps)))))))

(define gen-ref
   (lambda (var level maps k)
      (if (= level 0)
          (k var maps)
          (gen-ref var (- level 1) (cdr maps)
             (lambda (outer-var outer-maps)
                (let ((b (assq outer-var (car maps))))
                   (if b
                       (k (cdr b) maps)
                       (let ((inner-var (gen-sym var)))
                          (k inner-var
                             (cons (cons (cons outer-var inner-var) (car maps))
                                   outer-maps))))))))))

(define gen-map
   (lambda (e map-env)
      (let ((formals (map cdr map-env))
            (actuals (map (lambda (x) `(ref ,(car x))) map-env)))
         (cond
            ((eq? (car e) 'ref)
             ; identity map equivalence:
             ; (map (lambda (x) x) y) == y
             (car actuals))
            ((andmap
                (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
                (cdr e))
             ; eta map equivalence:
             ; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
             `(map (primitive ,(car e))
                   ,@(map (let ((r (map cons formals actuals)))
                             (lambda (x) (cdr (assq (cadr x) r))))
                          (cdr e))))
            (else `(map (lambda ,formals ,e) ,@actuals))))))

(define gen-cons
   (lambda (x y)
      (cond
         ((eq? (car y) 'list) `(list ,x ,@(cdr y)))
         ((and (eq? (car x) 'quote) (eq? (car y) 'quote))
          `(quote ,(cons (cadr x) (cadr y))))
         ((equal? y '(quote ())) `(list ,x))
         (else `(cons ,x ,y)))))

(define gen-append
   (lambda (x y)
      (if (equal? y '(quote ()))
          x
          `(append ,x ,y))))

(define gen-vector
   (lambda (x)
      (cond
         ((eq? (car x) 'list) `(vector ,@(cdr x)))
         ((eq? (car x) 'quote) `(quote #(,@(cadr x))))
         (else `(list->vector ,x)))))

(define regen
   (lambda (x)
      (case (car x)
         ((ref) (build-lexical-reference (cadr x)))
         ((primitive) (build-global-reference (cadr x)))
         ((id) (build-identifier (cadr x)))
         ((quote) (build-data (cadr x)))
         ((lambda) (build-lambda (cadr x) (regen (caddr x))))
         (else (build-application
                  (build-global-reference (car x))
                  (map regen (cdr x)))))))


;;; data

; Strip avoids unnecessary copying.
; It should, but doesn't, detect circular structures.

(define strip
   (lambda (x)
      (cond
         ((syntax-object? x) (strip (syntax-object-expression x)))
         ((pair? x)
          (let ((a (strip (car x))) (d (strip (cdr x))))
             (if (and (eq? a (car x)) (eq? d (cdr x)))
                 x
                 (cons a d))))
         ((vector? x)
          (let ((old (vector->list x)))
             (let ((new (map strip old)))
                (if (andmap eq? old new) x (list->vector new)))))
         (else x))))


;;; lexical variables

(define gen-sym
   (lambda (sym)
      (new-symbol-hook (symbol->string sym))))

(define gen-var
   (lambda (id)
      (gen-sym (id-sym-name id))))

(define lambda-var-list
   (lambda (vars)
      (let lvl ((vars vars) (ls '()))
         (cond
            ((pair? vars) (lvl (cdr vars) (cons (car vars) ls)))
            ((id? vars) (cons vars ls))
            ((null? vars) ls)
            ((syntax-object? vars) (lvl (unwrap vars) ls))
           ; include anything else to be caught by subsequent error
           ; checking
            (else (cons vars ls))))))


;;; core transformers

(global-extend 'core 'letrec-syntax chi-local-syntax)

(global-extend 'core 'let-syntax chi-local-syntax)

(global-extend 'core 'quote
   (lambda (e r w)
      (syntax-case e ()
         ((_ e) (build-data (strip (syntax e))))
         (_ (syntax-error (wrap e w))))))

(global-extend 'core 'syntax
   (lambda (e r w)
      (syntax-case e ()
         ((_ x) (chi-syntax e (syntax x) r w))
         (_ (syntax-error (wrap e w))))))

(global-extend 'core 'syntax-lambda
   (lambda (e r w)
      (syntax-case e ()
         ((_ ((id level) ...) exp)
          (and (valid-bound-ids? (syntax (id ...)))
               (map (lambda (x)
                       (and (integer? x) (exact? x) (not (negative? x))))
                    (map unwrap (syntax (level ...)))))
          (let ((new-vars (map gen-var (syntax (id ...)))))
             (build-lambda
                new-vars
                (chi (syntax exp)
                     (extend-syntax-env
                        new-vars
                        (map unwrap (syntax (level ...)))
                        r)
                     (make-binding-wrap (syntax (id ...)) new-vars w)))))
         (_ (syntax-error (wrap e w))))))

(global-extend 'core 'lambda
   (lambda (e r w)
      (syntax-case e ()
         ((_ (id ...) e1 e2 ...)
          (if (not (valid-bound-ids? (syntax (id ...))))
              (syntax-error (wrap e w) "invalid parameter list")
              (let ((new-vars (map gen-var (syntax (id ...)))))
                 (build-lambda
                    new-vars
                    (chi-body (syntax (e1 e2 ...))
                              e
                              (extend-var-env new-vars r)
                              (make-binding-wrap (syntax (id ...))
                                                 new-vars w))))))
         ((_ ids e1 e2 ...)
          (let ((old-ids (lambda-var-list (syntax ids))))
             (if (not (valid-bound-ids? (lambda-var-list (syntax ids))))
                 (syntax-error (wrap e w) "invalid parameter list")
                 (let ((new-vars (map gen-var old-ids)))
                    (build-improper-lambda
                       (reverse (cdr new-vars))
                       (car new-vars)
                       (chi-body (syntax (e1 e2 ...))
                                 e
                                 (extend-var-env new-vars r)
                                 (make-binding-wrap old-ids new-vars w)))))))
         (_ (syntax-error (wrap e w))))))

(global-extend 'core 'letrec
   (lambda (e r w)
      (syntax-case e ()
         ((_ ((id val) ...) e1 e2 ...)
          (valid-bound-ids? (syntax (id ...)))
          (let ((new-vars (map gen-var (syntax (id ...)))))
             (let ((w (make-binding-wrap (syntax (id ...)) new-vars w))
                   (r (extend-var-env new-vars r)))
                (build-letrec
                   new-vars
                   (map (lambda (x) (chi x r w)) (syntax (val ...)))
                   (chi-body (syntax (e1 e2 ...)) e r w)))))
         (_ (syntax-error (wrap e w))))))

(global-extend 'core 'if
   (lambda (e r w)
      (syntax-case e ()
         ((_ test then)
          (build-conditional
             (chi (syntax test) r w)
             (chi (syntax then) r w)
             (chi (syntax (void)) r empty-wrap)))
         ((_ test then else)
          (build-conditional
             (chi (syntax test) r w)
             (chi (syntax then) r w)
             (chi (syntax else) r w)))
         (_ (syntax-error (wrap e w))))))

(global-extend 'core 'set!
   (lambda (e r w)
      (syntax-case e ()
         ((_ id val)
          (id? (syntax id))
          (let ((val (chi (syntax val) r w))
                (n (id-var-name (syntax id) w)))
             (case (binding-type (lookup n (syntax id) r))
                ((lexical) (build-lexical-assignment n val))
                ((global global-unbound) (build-global-assignment n val))
                (else (id-error (wrap (syntax id) w))))))
         (_ (syntax-error (wrap e w))))))

(global-extend 'special 'begin
   (lambda (e r w k)
      (let ((body (chi-sequence e w)))
         (if (null? body)
             (if (eqv? k chi-top)
                 (chi (syntax (void)) r empty-wrap)
                 (syntax-error (wrap e w) "no expressions in body of"))
             (build-sequence
                (let dobody ((body body))
                   (if (null? body)
                       '()
                       (let ((first (k (car body) r empty-wrap)))
                          (cons first (dobody (cdr body)))))))))))

(global-extend 'special 'define
   (lambda (e r w k)
      (if (eqv? k chi-top)
          (let ((n&v (chi-definition e w)))
             (let ((n (id-var-name (car n&v) empty-wrap)))
                (global-extend 'global n '())
                (build-global-definition
                   n
                   (chi (cadr n&v) r empty-wrap))))
          (syntax-error (wrap e w) "invalid context for definition"))))

(global-extend 'special 'define-syntax
   (lambda (e r w k)
      (if (eqv? k chi-top)
          (let ((n&v (chi-syntax-definition e w)))
             (global-extend
                'macro
                (id-var-name (car n&v) empty-wrap)
                (chi-macro-def (cadr n&v) r empty-wrap))
             (chi (syntax (void)) r empty-wrap))
          (syntax-error (wrap e w) "invalid context for definition"))))


;;; exports

(set! expand-syntax (lambda (x) (chi-top x null-env top-wrap)))

(set! implicit-identifier
   (lambda (id sym)
      (arg-check id? id 'implicit-identifier)
      (arg-check symbol? sym 'implicit-identifier)
      (if (syntax-object? id)
          (wrap sym (syntax-object-wrap id))
          sym)))

(set! syntax-object->datum
   (lambda (x)
      (strip x)))

(set! generate-temporaries
   ;; this will do; it might be better to return some arbitrary
   ;; symbol wrapped with a gensym to accomodate systems without
   ;; gensym
   (lambda (ls)
      (arg-check list? ls 'generate-temporaries)
      (map (lambda (x) (wrap (gensym) top-wrap)) ls)))

(set! free-identifier=?
   (lambda (x y)
      (arg-check id? x 'free-identifier=?)
      (arg-check id? y 'free-identifier=?)
      (free-id=? x y)))

(set! bound-identifier=?
   (lambda (x y)
      (arg-check id? x 'bound-identifier=?)
      (arg-check id? y 'bound-identifier=?)
      (bound-id=? x y)))

(set! identifier? (lambda (x) (id? x)))

(set! syntax-error
   (lambda (object . messages)
      (for-each (lambda (x) (arg-check string? x 'syntax-error)) messages)
      (let ((message (if (null? messages)
                         "invalid syntax"
                         (apply string-append messages))))
         (error-hook 'expand-syntax message (strip object)))))

;;; used by preprocessed (bootstrapped) code
(set! install-global-transformer
   (lambda (sym p)
      (global-extend 'macro sym p)))

; "syntax-dispatch" expects an expression, a pattern, and a keyword vector
; If the expression matches the pattern a list of
; of the matching expressions for each "any" is returned.
; Otherwise, the symbol `no' is returned.

; The expression is matched with the pattern as follows:

; pattern:                           matches:
;   (any)                              anything
;   (free-id . <index>)                (free-identifier=?
;                                         <identifier>
;                                         (vector-ref keys <index>))
;   (each . <pattern>)                 (<pattern>*)
;   (pair . (<pattern>1 . <pattern>2)) (<pattern>1 . <pattern>2)
;   (vector . <pattern>)               (list->vector <pattern>)
;   (atom . <object>)                  <object> with "equal?"

; Some mileage might be gotten out of extra patterns such as:
;   (list . <pattern>*)                (<pattern>*)

(let ()

(define match-each
   (lambda (e p k w)
      (cond
         ((pair? e)
          (let ((first (match (car e) p k w '())))
             (if (eq? first 'no)
                 first
                 (let ((rest (match-each (cdr e) p k w)))
                    (if (eq? rest 'no) rest (cons first rest))))))
         ((null? e) '())
         ((syntax-object? e)
          (match-each (syntax-object-expression e)
                      p
                      k
                      (join-wraps w (syntax-object-wrap e))))
         (else 'no))))

(define match-each-any
   (lambda (e w)
      (cond
         ((pair? e)
          (let ((l (match-each-any (cdr e) w)))
             (if (eq? l 'no) l (cons (wrap (car e) w) l))))
         ((null? e) '())
         ((syntax-object? e)
          (match-each-any (syntax-object-expression e)
                          (join-wraps w (syntax-object-wrap e))))
         (else 'no))))

(define match-empty
   (lambda (p r)
      (case (car p)
         ((any) (cons '() r))
         ((each) (match-empty (cdr p) r))
         ((pair) (match-empty (cadr p) (match-empty (cddr p) r)))
         ((free-id atom) r)
         ((vector) (match-empty (cdr p) r)))))

(define match*
   (lambda (e p k w r)
      (case (car p)
         ((pair)
          (if (pair? e)
              (match (car e) (cadr p) k w (match (cdr e) (cddr p) k w r))
              'no))
         ((each)
          (cond
             ((eq? (cadr p) 'any)
              (let ((l (match-each-any e w)))
                 (if (eq? l 'no) l (cons l r))))
             ((null? e) (match-empty (cdr p) r))
             (else
              (let ((l (match-each e (cdr p) k w)))
                 (if (eq? l 'no)
                     l
                     (let collect ((l l))
                        (if (null? (car l))
                            r
                            (cons (map car l) (collect (map cdr l))))))))))
         ((atom) (if (equal? (cdr p) e) r 'no))
         ((vector)
          (if (vector? e) (match (vector->list e) (cdr p) k w r) 'no)))))

(define match
   (lambda (e p k w r)
      (if (eq? r 'no)
          r
          (case (car p)
             ((any) (cons (wrap e w) r))
             ((free-id)
              (if (and (identifier? e)
                       (free-id=? (wrap e w) (vector-ref k (cdr p))))
                  r
                  'no))
             (else
              (if (syntax-object? e)
                  (match* (syntax-object-expression e)
                          p
                          k
                          (join-wraps w (syntax-object-wrap e))
                          r)
                  (match* e p k w r)))))))

(set! syntax-dispatch
   (lambda (expression pattern keys)
      (match expression pattern keys empty-wrap '())))
)
)

;;; define let---this is needed here since syntax-case expands into let
(define-syntax let
   (lambda (x)
      (syntax-case x ()
         ((_ ((x v) ...) e1 e2 ...)
          (andmap identifier? (syntax (x ...)))
          (syntax ((lambda (x ...) e1 e2 ...) v ...)))
         ((_ f ((x v) ...) e1 e2 ...)
          (andmap identifier? (syntax (f x ...)))
          (syntax ((letrec ((f (lambda (x ...) e1 e2 ...))) f)
                    v ...))))))

;;; define syntax-case in terms of syntax-dispatch and syntax-lambda
(define-syntax syntax-case
   (let ()
      (define ellipsis?
         (lambda (x)
            (and (identifier? x)
                 (free-identifier=? x (syntax (... ...))))))

      (define memid
         (lambda (i ids)
            (and (not (null? ids))
                 (if (bound-identifier=? i (car ids))
                     ids
                     (memid i (cdr ids))))))

      (define key?
         (lambda (p keys)
            (and (identifier? p) (memid p keys))))

      (define key-index
         (lambda (p keys)
            (- (length keys) (length (memid p keys)))))

      (define convert-syntax-dispatch-pattern
         (lambda (pattern keys)
            (let gen ((p pattern))
               (if (identifier? p)
                   (if (key? p keys)
                       (cons (syntax free-id) (key-index p keys))
                       (syntax (any)))
                   (syntax-case p ()
                      ((x dots)
                       (ellipsis? (syntax dots))
                       (cons (syntax each) (gen (syntax x))))
                      ((x . y)
                       (cons (syntax pair)
                             (cons (gen (syntax x)) (gen (syntax y)))))
                      (#(x ...) (cons (syntax vector) (gen (syntax (x ...)))))
                      (x (cons (syntax atom) p)))))))

      (define valid-keyword?
         (lambda (k)
            (and (identifier? k)
                 (not (free-identifier=? k (syntax (... ...)))))))

      (define valid-syntax-pattern?
         (lambda (pattern keys)
            (define (check? p ids)
               (if (identifier? p)
                   (if (eq? ids 'no)
                       ids
                       (if (key? p keys)
                           ids
                           (if (and (not (ellipsis? p))
                                    (not (memid p ids)))
                               (cons p ids)
                               'no)))
                   (syntax-case p ()
                      ((x dots)
                       (ellipsis? (syntax dots))
                       (check? (syntax x) ids))
                      ((x . y) (check? (syntax x) (check? (syntax y) ids)))
                      (#(x ...) (check? (syntax (x ...)) ids))
                      (x ids))))
            (not (eq? (check? pattern '()) 'no))))

      (define extract-bound-syntax-ids
         (lambda (pattern keys)
            (let gen ((p pattern) (n 0) (ids '()))
               (if (identifier? p)
                   (if (key? p keys) ids (cons (list p n) ids))
                   (syntax-case p ()
                      ((x dots)
                       (ellipsis? (syntax dots))
                       (gen (syntax x) (+ n 1) ids))
                      ((x . y) (gen (syntax x) n (gen (syntax y) n ids)))
                      (#(x ...) (gen (syntax (x ...)) n ids))
                      (x ids))))))

      (define build-dispatch-call
         (lambda (args body val)
            (syntax-case args ()
               (() body)
               ((arg1)
                (with-syntax ((body body) (val val))
                   (syntax
                      ((syntax-lambda (arg1) body)
                       (car val)))))
               ((arg1 arg2)
                (with-syntax ((body body) (val val))
                   (syntax
                      ((syntax-lambda (arg1 arg2) body)
                       (car val) (cadr val)))))
               ((arg1 arg2 arg3)
                (with-syntax ((body body) (val val))
                   (syntax
                      ((syntax-lambda (arg1 arg2 arg3) body)
                       (car val) (cadr val) (caddr val)))))
               ((arg1 arg2 arg3 arg4)
                (with-syntax ((body body) (val val))
                   (syntax
                      ((syntax-lambda (arg1 arg2 arg3 arg4) body)
                       (car val) (cadr val) (caddr val) (cadddr val)))))
               ((arg ...)
                (with-syntax ((body body) (val val))
                   (syntax (apply (syntax-lambda (arg ...) body) val)))))))

      (lambda (x)
         (syntax-case x () ; isn't bootstrapping fun?
            ((_ val (key ...))
             (andmap valid-keyword? (syntax (key ...)))
             (syntax (syntax-error val)))
            ((_ val (key ...) (pat exp))
             (and (identifier? (syntax pat))
                  (andmap valid-keyword? (syntax (key ...)))
                  (andmap (lambda (x) (not (free-identifier=? (syntax pat) x)))
                          (syntax ((... ...) key ...))))
             (syntax ((syntax-lambda ((pat 0)) exp) val)))
            ((_ val (key ...) (pat exp) (e1 e2 e3 ...) ...)
             (and (andmap valid-keyword? (syntax (key ...)))
                  (valid-syntax-pattern? (syntax pat) (syntax (key ...))))
             (with-syntax ((pattern (convert-syntax-dispatch-pattern
                                       (syntax pat)
                                       (syntax (key ...))))
                           (y (syntax y))
                           (call (build-dispatch-call
                                    (extract-bound-syntax-ids
                                       (syntax pat)
                                       (syntax (key ...)))
                                    (syntax exp)
                                    (syntax y))))
                (syntax (let ((x val))
                           (let ((y (syntax-dispatch x 'pattern
                                                       (syntax #(key ...)))))
                              (if (not (eq? y 'no))
                                  call
                                  (syntax-case x (key ...)
                                     (e1 e2 e3 ...) ...)))))))
            ((_ val (key ...) (pat fender exp) (e1 e2 e3 ...) ...)
             (and (andmap valid-keyword? (syntax (key ...)))
                  (valid-syntax-pattern? (syntax pat) (syntax (key ...))))
             (with-syntax ((pattern (convert-syntax-dispatch-pattern
                                       (syntax pat)
                                       (syntax (key ...))))
                           (y (syntax y))
                           (dorest (syntax dorest))
                           (call (build-dispatch-call
                                    (extract-bound-syntax-ids
                                       (syntax pat)
                                       (syntax (key ...)))
                                    (syntax (if fender exp (dorest)))
                                    (syntax y))))
                (syntax (let ((x val))
                           (let ((dorest (lambda ()
                                            (syntax-case x (key ...)
                                               (e1 e2 e3 ...) ...))))
                              (let ((y (syntax-dispatch x 'pattern
                                                          (syntax #(key ...)))))
                                 (if (not (eq? y 'no)) call (dorest))))))))))))

)
