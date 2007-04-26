# Makefile.am for STklos lib
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 20-Dec-2006 10:09 (eg)
# Last file update: 26-Apr-2007 16:07 (eg)

SRCS =  scmpkg-interface.stk scmpkg-languages.stk
OBJ  =  ../scmpkg-support.ostk



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
	/bin/rm -f $(OBJ) *~

distclean: clean
