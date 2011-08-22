# Makefile for stklos-pkgman
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 22-Dec-2006 12:00 (eg)
# Last file update: 23-Oct-2010 11:53 (eg)


makefiledir=  $(prefix)/etc/stklos
makefile_DATA= etc/Makefile-scmpkg.tmpl

bin_SCRIPTS = stklos-pkg

LEX     = lang-bigloo.l lang-chicken.l lang-mzscheme.l
LEXOBJS = lang-bigloo.inc lang-chicken.inc lang-mzscheme.inc

SRC     = ../lib/http.stk \
	  main.stk add.stk copy.stk extract.stk install.stk lang.stk \
	  params.stk pkgball.stk \
	  repository.stk rewrite.stk misc.stk tune.stk types.stk
SFLAGS  =
RM      = /bin/rm
SCC	= ../utils/stklos-compile

lang-bigloo.inc: lang-bigloo.l
	stklos-genlex lang-bigloo.l lang-bigloo.inc lang-bigloo

lang-chicken.inc: lang-chicken.l
	stklos-genlex lang-chicken.l lang-chicken.inc lang-chicken

lang-mzscheme.inc: lang-mzscheme.l
	stklos-genlex lang-mzscheme.l lang-mzscheme.inc lang-mzscheme


$(bin_SCRIPTS): $(LEXOBJS) $(SRC)
	../utils/tmpcomp main.stk $(bin_SCRIPTS)
#	$(SCC) $(SFLAGS) -o $(bin_SCRIPTS) main.stk

clean:
	$(RM) -f $(bin_SCRIPTS) *~

distclean: clean
	$(RM) -f Makefile

egclean: distclean
	$(RM) -f  $(LEXOBJS)
