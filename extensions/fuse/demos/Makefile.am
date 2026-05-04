# Makefile.in for stklos-fuse examples
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 19-Sep-2009 20:29 (eg)

SRC= hellofs hashfs
EXE=$(SRC:.stk=)
COMP=../../../utils/tmpcomp

SUFFIXES= .stk

.stk:
	$(COMP) -o $* $<

all: 	$(EXE)


install:
	@echo "Demos are not installed"

clean:
	rm -f $(EXE) *~ 

distclean-am: clean
	rm -f stklos-fuse.so
	rm -f Makefile

