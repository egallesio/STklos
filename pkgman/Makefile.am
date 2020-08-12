# Makefile for stklos-pkgman
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 22-Dec-2006 12:00 (eg)
# Last file update: 12-Aug-2020 19:38 (eg)


makefiledir  = $(prefix)/share/@PACKAGE@/@VERSION@/etc
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
COMP	?= ../utils/tmpcomp
GENLEX  ?= ../utils/tmpgenlex

all: $(bin_SCRIPTS)

lang-bigloo.inc: lang-bigloo.l
	$(GENLEX) lang-bigloo.l lang-bigloo.inc lang-bigloo

lang-chicken.inc: lang-chicken.l
	$(GENLEX) lang-chicken.l lang-chicken.inc lang-chicken

lang-mzscheme.inc: lang-mzscheme.l
	$(GENLEX) lang-mzscheme.l lang-mzscheme.inc lang-mzscheme

$(bin_SCRIPTS):  $(LEXOBJS) $(SRC)
	$(COMP) -o $(bin_SCRIPTS) main.stk
	rm -f  $(LEXOBJS)

clean:
	$(RM) -f $(bin_SCRIPTS) $(LEXOBJS) *~

distclean: clean
	$(RM) -f Makefile
