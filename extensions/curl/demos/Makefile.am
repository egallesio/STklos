# Makefile.in for stklos-fuse examples
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 19-Sep-2009 20:29 (eg)
# Last file update:  8-Feb-2022 19:19 (eg)

SRC= imap.stk
EXE=$(SRC:.stk=)
COMP=../../../utils/tmpcomp

SUFFIXES= .stk

.stk:
	$(COMP) -o $* $<

all: 	$(EXE)


install:
	@echo "Demos are not installed"

clean:
	/bin/rm -f $(EXE) *~ 

distclean-am: clean
	/bin/rm -f Makefile

