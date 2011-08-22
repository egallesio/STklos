;;;           Author: Erick Gallesio [eg@essi.fr]
;;;    Creation date: 21-Mar-2007 13:45 (eg)
;;; Last file update: 31-May-2007 14:44 (eg)


%%
;; Character "
#\\\"			"#\\\""

;; Strings
\"([^\\\"]|\\.)*\"	yytext		;|;

;;Comment
\;.*			yytext

;; require provide
\(require		"(mzrequire"
\(provide		"(mzprovide"


;; Other characters
[^#\"\;\(]+		yytext

;; A #
[#\(]			yytext


<<EOF>>			'eof
<<ERROR>>		(error 'mzscheme-preprocessor "parse error ~S (line ~a)"
			       yytext (lexer-get-line))
