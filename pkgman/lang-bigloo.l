;;;           Author: Erick Gallesio [eg@essi.fr]
;;;    Creation date: 21-Mar-2007 13:45 (eg)
;;; Last file update: 25-Apr-2007 18:49 (eg)


%%
;; Character "
#\\\"			"#\\\""

;; Strings
\"([^\\\"]|\\.)*\"	yytext		;|; 

;;Comment
\;.*			yytext

;; Type indication
::[A-Za-z0-9]+		(yycontinue)

;; #undefined
#unspecified		"#void"

;; Ascii characters notation #aXXX
#a[0-9][0-9][0-9]	(format "(integer->char ~a)" 
				(substring yytext 2 5))

;; Other characters
[^:#\"\;]+		yytext

;; A # or : which is not undefined or #aXXX
[#:]			yytext


<<EOF>>			'eof
<<ERROR>>		(error 'bigloo-preprocessor "parse error ~S (line ~a)" 
			       yytext (lexer-get-line))
