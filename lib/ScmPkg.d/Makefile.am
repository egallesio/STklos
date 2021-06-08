# Makefile.am for STklos lib
#
# Copyright © 2006-2021 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 20-Dec-2006 10:09 (eg)
# Last file update:  1-Jun-2021 18:37 (eg)

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

uninstall-hook:
	(cd  $(scheme_libdir)/ScmPkg.d && rm -f $(SRCS) )
	(rmdir $(scheme_libdir)/ScmPkg.d || true)

clean: 
	/bin/rm -f $(OBJ) *~
