# Makefile.in for stklos-fuse examples
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 19-Sep-2009 20:29 (eg)
# Last file update: 13-Jul-2021 19:15 (eg)

SRC= hellofs hashfs
EXE=$(SRC:.stk=)
COMP=../../../utils/stklos-compile

SUFFIXES= .stk

.stk:
	$(COMP) -c -o $* $<

all: 	$(EXE)


install:
	@echo "Demos are not installed"

clean:
	/bin/rm -f $(EXE) *~ 

distclean-am: clean
	/bin/rm -f stklos-fuse.so
	/bin/rm -f Makefile

