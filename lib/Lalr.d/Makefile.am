# Makefile for lib/Lalr
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 25-Oct-2004 16:43 (eg)
# Last file update: 23-Oct-2010 11:59 (eg)

schemedir   = $(prefix)/share/@PACKAGE@/@VERSION@
scheme_DATA = lalr.stk lalr.ostk

COMP ?= ../utils/tmpcomp

#======================================================================
SUFFIXES = .stk .ostk .scm
.stk.ostk:
	echo "COMP = $(COMP)"
	(cd ..; $(COMP) Lalr.d/$*.stk -o Lalr.d/$*.ostk)
#======================================================================
all: lalr.ostk calc

calc: calc.stk
	echo "#!/bin/sh" > calc
	echo "../../src/stklos -b ../../src/boot.img -f calc.stk" >> calc
	chmod a+x calc

clean: 
	/bin/rm -f *.ostk calc *~

