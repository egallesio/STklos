# Makefile.am for STklos lib
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 20-Dec-2006 10:09 (eg)
# Last file update: 26-Apr-2007 17:29 (eg)

SRCS =  scmpkg-interface.stk scmpkg-languages.stk
OBJ  =  ../scmpkg-support.ostk



scheme_libdir      = $(prefix)/share/@PACKAGE@/@VERSION@
scheme_lib_DATA    = scmpkg-support.stk $(OBJ)

scheme_scmpkgdir   = $(scheme_libdir)/ScmPkg.d
scheme_scmpkg_DATA = $(SRCS)

all: $(OBJ)

$(OBJ): scmpkg-support.stk $(SRCS)
	(cd ..; ../utils/tmpcomp ScmPkg.d/scmpkg-support.stk scmpkg-support.ostk)

clean: 
	/bin/rm -f $(OBJ) *~

distclean: clean
