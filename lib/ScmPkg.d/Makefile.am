# Makefile.am for STklos lib
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 20-Dec-2006 10:09 (eg)
# Last file update: 28-Feb-2007 22:58 (eg)

SRCS =  scmpkg-define.stk    \
	scmpkg-exception.stk \
	scmpkg-interface.stk \
	scmpkg-languages.stk \
	scmpkg-parameter.stk \
	scmpkg-record.stk    \
	scmpkg-runtime.stk   

OBJ =   scmpkg-support.ostk



scheme_libdir      = $(prefix)/share/@PACKAGE@/@VERSION@
scheme_lib_DATA    = scmpkg-support.stk $(OBJ)

scheme_scmpkgdir   = $(scheme_libdir)/ScmPkg.d
scheme_scmpkg_DATA = $(SRCS)

#======================================================================
SUFFIXES = .stk .ostk .scm
.stk.ostk:
	(cd ..; ../utils/tmpcomp ScmPkg.d/$*.stk ScmPkg.d/$*.ostk)
#======================================================================

all: $(OBJ)

$(OBJ): scmpkg-support.stk $(SRCS)


clean: 
	/bin/rm -f *.ostk *~

distclean: clean
