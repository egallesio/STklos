# Makefile.am for STklos lib
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 20-Dec-2006 10:09 (eg)
# Last file update: 28-Aug-2020 17:07 (eg)

SRCS =  scmpkg-interface.stk scmpkg-languages.stk
OBJ  =  ../scmpkg-support.ostk

COMP ?= ../utils/tmpcomp

scheme_libdir      = $(prefix)/share/@PACKAGE@/@VERSION@
scheme_lib_DATA    = scmpkg-support.stk $(OBJ)

scheme_scmpkgdir   = $(scheme_libdir)/ScmPkg.d
scheme_scmpkg_DATA = $(SRCS)

all: $(OBJ)

install-sources:

$(OBJ): scmpkg-support.stk $(SRCS)
	(cd ..; $(COMP) -o scmpkg-support.ostk ScmPkg.d/scmpkg-support.stk )

clean: 
	/bin/rm -f $(OBJ) *~
