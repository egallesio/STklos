;;;
;;;; Simple calculator in Scheme
;;;
;;
;;   This program illustrates the use of the lalr-scm parser generator
;; for Scheme. It is NOT robust, since calling a function with 
;; the wrong number of arguments may generate an error that will
;; cause the calculator to crash.


;;;
;;;;   The LALR(1) parser
;;;


(define calc-parser
  (lalr-parser

   ;; --- Options 
   ;; output a parser, called calc-parser, in a separate file - calc.yy.scm, 
   ;; STklos:
   ;;(output:    calc-parser "calc.yy.scm")
   ;; output the LALR table to calc.out
   ;;STklos
   ;;(out-table: "calc.out")
   ;; there should be no conflict
   (expect:    5)

   ;; --- token definitions
   (ID NUM = LPAREN RPAREN NEWLINE COMMA
       (left: + -)
       (left: * /)
       (nonassoc: uminus))

   (lines    (lines line) : (display-result $2)
             (line)       : (display-result $1))


   ;; --- rules
   (line     (assign NEWLINE)        : $1
             (expr   NEWLINE)        : $1
	     (NEWLINE)               : #f
             (error  NEWLINE)        : #f)

   (assign   (ID = expr)             : (add-binding $1 $3))

   (expr     (expr + expr)           : (+ $1 $3)
             (expr - expr)           : (- $1 $3)
             (expr * expr)           : (* $1 $3)
             (expr / expr)           : (/ $1 $3)
             (- expr (prec: uminus)) : (- $2)
             (ID)                    : (get-binding $1)
             (ID LPAREN args RPAREN) : (invoke-proc $1 $3)
             (NUM)                   : $1
             (LPAREN expr RPAREN)    : $2)

   (args     ()                      : '()
             (expr arg-rest)         : (cons $1 $2))

   (arg-rest (COMMA expr arg-rest)   : (cons $2 $3)
             ()                      : '())))


;;;
;;;;   The lexer
;;;


(cond-expand
 (gambit
  (define port-line input-port-line)
  (define port-column input-port-column))
 
 (chicken
  (define (force-output) #f)
  (define (port-line port) 
    (let-values (((line _) (port-position port)))
      line))
  
  (define (port-column port)
    (let-values (((_ column) (port-position port)))
      column)))

 (guile
  (define (port-line port) '??)
  (define (port-column port) '??))
 
 (else
  (define (force-output) #f)
  (define (port-line port) '??)
  (define (port-column port) '??)))


(define (make-lexer errorp)
  (lambda ()
    (letrec ((skip-spaces
              (lambda ()
                (let loop ((c (peek-char)))
                  (if (and (not (eof-object? c))
                           (or (char=? c #\space) (char=? c #\tab)))
                      (begin
                        (read-char)
                        (loop (peek-char)))))))
             (read-number
              (lambda (l)
                (let ((c (peek-char)))
                  (if (char-numeric? c)
                      (read-number (cons (read-char) l))
                      (string->number (apply string (reverse l)))))))
             (read-id
              (lambda (l)
                (let ((c (peek-char)))
                  (if (char-alphabetic? c)
                      (read-id (cons (read-char) l))
                      (string->symbol (apply string (reverse l))))))))

      ;; -- skip spaces
      (skip-spaces)
      ;; -- read the next token
      (let loop ()
        (let* ((location (make-source-location "*stdin*" (port-line (current-input-port)) (port-column (current-input-port)) -1 -1))
               (c (read-char)))
          (cond ((eof-object? c)      '*eoi*)
                ((char=? c #\newline) (make-lexical-token 'NEWLINE location #f))
                ((char=? c #\+)       (make-lexical-token '+       location #f))
                ((char=? c #\-)       (make-lexical-token '-       location #f))
                ((char=? c #\*)       (make-lexical-token '*       location #f))
                ((char=? c #\/)       (make-lexical-token '/       location #f))
                ((char=? c #\=)       (make-lexical-token '=       location #f))
                ((char=? c #\,)       (make-lexical-token 'COMMA   location #f))
                ((char=? c #\()       (make-lexical-token 'LPAREN  location #f))
                ((char=? c #\))       (make-lexical-token 'RPAREN  location #f))
                ((char-numeric? c)    (make-lexical-token 'NUM     location (read-number (list c))))
                ((char-alphabetic? c) (make-lexical-token 'ID      location (read-id (list c))))
                (else
                 (errorp "PARSE ERROR : illegal character: " c)
                 (skip-spaces)
                 (loop))))))))



;;;
;;;;   Environment management
;;;


(define *env* (list (cons '$$ 0)))


(define (init-bindings)
  (set-cdr! *env* '())
  (add-binding 'cos cos)
  (add-binding 'sin sin)
  (add-binding 'tan tan)
  (add-binding 'expt expt)
  (add-binding 'sqrt sqrt))


(define (add-binding var val)
  (set! *env* (cons (cons var val) *env*))
  val)


(define (get-binding var)
  (let ((p (assq var *env*)))
    (if p
        (cdr p)
        0)))


(define (invoke-proc proc-name args)
  (let ((proc (get-binding proc-name)))
    (if (procedure? proc)
        (apply proc args)
        (begin
          (display "ERROR: invalid procedure:")
          (display proc-name)
          (newline)
          0))))


;;;
;;;;   The main program
;;;


(define (display-result v)
  (if v
      (begin
        (display v)
        (newline)))
  (display-prompt))


(define (display-prompt)
  (display "[calculator]> ")
  (force-output))


(define calc
  (lambda ()
    (call-with-current-continuation
     (lambda (k)
       (display "********************************") (newline)
       (display "*  Mini calculator in Scheme   *") (newline)
       (display "*                              *") (newline)
       (display "* Enter expressions followed   *") (newline)
       (display "* by [RETURN] or 'quit()' to   *") (newline)
       (display "* exit.                        *") (newline)
       (display "********************************") (newline)
       (init-bindings)
       (add-binding 'quit (lambda () (k #t)))
       (letrec ((errorp
                 (lambda (message . args)
                   (display message)
                   (if (and (pair? args) 
                            (lexical-token? (car args)))
                       (let ((token (car args)))
                         (display (or (lexical-token-value token)
                                      (lexical-token-category token)))
                         (let ((source (lexical-token-source token)))
                           (if (source-location? source)
                               (let ((line (source-location-line source))   
				     (column (source-location-column source)))
				 (if (and (number? line) (number? column))
				     (begin
				       (display " (at line ")
				       (display line)
				       (display ", column ")
				       (display (+ 1 column))
				       (display ")")))))))
                       (for-each display args))
                   (newline)))
                (start
                 (lambda ()
                   (calc-parser (make-lexer errorp) errorp))))
	 (display-prompt)
         (start))))))
;; STklos
;; (calc)

