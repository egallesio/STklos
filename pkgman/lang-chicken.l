;;;           Author: Erick Gallesio [eg@essi.fr]
;;;    Creation date: 21-Mar-2007 13:45 (eg)
;;; Last file update:  5-May-2007 11:51 (eg)


%%
;; Character "
#\\\"			"#\\\""

;; Strings
\"([^\\\"]|\\.)*\"	yytext		;|;

;;Comment
\;.*			yytext

;; #!eof
#!eof			"#eof"

;; Other characters
[^#\"\;]+		yytext

;; A #
[#]			yytext


<<EOF>>			'eof
<<ERROR>>		(error 'chicken-preprocessor "parse error ~S (line ~a)"
			       yytext (lexer-get-line))
