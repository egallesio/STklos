# Makefile.am for STklos lib
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 20-Dec-2006 10:09 (eg)
# Last file update: 14-Feb-2007 12:20 (eg)

SRCS =  split-define.stk    \
	split-exception.stk \
	split-interface.stk \
	split-languages.stk \
	split-parameter.stk \
	split-record.stk    \
	split-runtime.stk   

OBJ =   split-support.ostk



scheme_splitdir   = $(prefix)/share/@PACKAGE@/@VERSION@
scheme_split_DATA = split-support.stk $(SRCS) $(OBJ)

#======================================================================
SUFFIXES = .stk .ostk .scm
.stk.ostk:
	(cd ..; ../utils/tmpcomp Split.d/$*.stk Split.d/$*.ostk)
#======================================================================

all: $(OBJ)

$(OBJ): split-support.stk $(SRCS)


clean: 
	/bin/rm -f *.ostk *~

distclean: clean
