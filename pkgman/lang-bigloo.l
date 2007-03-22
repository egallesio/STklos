;;;           Author: Erick Gallesio [eg@essi.fr]
;;;    Creation date: 21-Mar-2007 13:45 (eg)
;;; Last file update: 22-Mar-2007 13:56 (eg)


%%
;; Strings
\"[^\"]*\"		yytext

;;Comment
\;.*			yytext

;; #undefined
#unspecified		"#void"

;; Ascii characters notation #aXXX
#a[0-9][0-9][0-9]	(format "(integer->char ~a)" 
				(substring yytext 2 5))

;; Other characters
[^#\"\;]+		yytext

;; A # which is not undefined or #aXXX
\#			"#"

<<EOF>>			'eof
<<ERROR>>		(error 'bigloo-preprocessor "Parse error" yytext)

