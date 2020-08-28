# Makefile.am for STklos lib
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 26-Nov-2001 10:10 (eg)
# Last file update: 28-Aug-2020 16:50 (eg)

scheme_matchdir   = $(prefix)/share/@PACKAGE@/@VERSION@/Match.d
#scheme_match_DATA = descr.scm compiler.scm  mexpand.scm normalize.scm s2cfun.scm
SRC = descr.scm compiler.scm  mexpand.scm normalize.scm s2cfun.scm

install-sources:
	mkdir -p  $(scheme_matchdir)
	cp $(SRC) $(scheme_matchdir)



