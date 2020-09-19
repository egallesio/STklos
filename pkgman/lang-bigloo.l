;;;;                                            -*- mode: scheme -*-
;;;; lang-bigloo.l
;;;;
;;;; Copyright © 2007-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
;;;;
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;;; USA.
;;;;           Author: Erick Gallesio [eg@essi.fr]
;;;;    Creation date: 21-Mar-2007 13:45 (eg)
;;;; Last file update: 19-Sep-2020 20:37 (eg)


%%
;; Character "
#\\\"           "#\\\""

;; Strings
\"([^\\\"]|\\.)*\"  yytext      ;|;

;;Comment
\;.*            yytext

;; Type indication
::[A-Za-z0-9]+      (yycontinue)

;; #undefined
#unspecified        "#void"

;; Ascii characters notation #aXXX
#a[0-9][0-9][0-9]   (format "(integer->char ~a)"
                (substring yytext 2 5))

;; Other characters
[^:#\"\;]+      yytext

;; A # or : which is not undefined or #aXXX
[#:]            yytext


<<EOF>>         'eof
<<ERROR>>       (error 'bigloo-preprocessor "parse error ~S (line ~a)"
                   yytext (lexer-get-line))
