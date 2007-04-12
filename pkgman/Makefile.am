# Makefile for stklos-pkgman
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 22-Dec-2006 12:00 (eg)
# Last file update: 12-Apr-2007 12:03 (eg)


makefiledir=  $(prefix)/etc/stklos
makefile_DATA= etc/Makefile-scmpkg.tmpl

bin_SCRIPTS = stklos-pkg

LEX     = lang-bigloo.l
LEXOBJS = lang-bigloo.inc

SRC     = main.stk add.stk extract.stk http.stk lang.stk lang-bigloo.stk \
	  params.stk pkgball.stk \
	  repository.stk rewrite.stk misc.stk tune.stk types.stk
SFLAGS  = -l 
RM      = /bin/rm
SCC	= ../utils/stklos-compile

lang-bigloo.inc: lang-bigloo.l
	stklos-genlex lang-bigloo.l lang-bigloo.inc lang-bigloo


$(bin_SCRIPTS): $(LEXOBJS) $(SRC)
	../utils/tmpcomp main.stk $(bin_SCRIPTS)
#	$(SCC) $(SFLAGS) -o $(bin_SCRIPTS) main.stk

clean:
	$(RM) -f $(bin_SCRIPTS) *~

distclean: clean


egclean: distclean
	$(RM) -f  $(LEXOBJS)
