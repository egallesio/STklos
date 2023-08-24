# Makefile for the STklos specific  libraries
#
# Copyright © 2021-2023 Erick Gallesio <eg@stklos.net>
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
#    Creation date:  2-Jan-2022 15:26 (eg)

#======================================================================

COMP ?= ../../utils/tmpcomp
STKLOS_BINARY ?= ../../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive
SO = @SH_SUFFIX@

#
# Libraries written in Scheme only
#
SRC_STK  = primitive.stk derived.stk
SRC_OSTK = primitive.ostk derived.ostk

#
# Libraries written in C and Scheme
#
SRC_C     =
SRC_C_STK =
SRC_SHOBJ =

ALL_OBJS = $(SRC_OSTK) $(SRC_SHOBJ)

DOCDB  = ../DOCDB
BASEDIR= ../..
SOURCES= $(SRC_C) $(SRC_STK) $(SRC_C_STK)

#======================================================================
schemedir       = @SCMDIR@/streams
schemelibdir    = @EXECDIR@/streams
scheme_DATA     = $(SRC_OSTK)
schemelib_DATA  = $(SRC_SHOBJ)
scheme_sources  = $(SRC_STK)
#======================================================================
SUFFIXES = .stk .ostk .stk -incl.c .$(SO) .c

.stk.ostk:
	$(COMP) -o $*.ostk $*.stk

.stk-incl.c:
	$(COMP) -C -o $*-incl.c $*.stk

.c.$(SO) :
	@CC@ @CFLAGS@ @STKCFLAGS@ @SH_COMP_FLAGS@ -I../../src @GCINC@ @GMPINC@ \
	-c -o $*.o $*.c
	@SH_LOADER@ @SH_LOAD_FLAGS@  $*.$(SO) $*.o @DLLIBS@
	/bin/rm -f $*.o
#======================================================================
# Dependencies
derived.ostk: primitive.ostk ../scheme/list.$(SO)
../scheme/list.$(SO):
	(cd ../scheme; $(MAKE) list.$(SO))

# ======================================================================
install-sources:
	install $(scheme_sources) $(schemedir)

clean:
	rm -f $(ALL_OBJS) *-incl.c  *~

distclean: clean
	/bin/rm -f Makefile

uninstall-hook:
	(cd $(schemedir); rm -f $(scheme_sources))
	rmdir $(schemedir) || true

doc:
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ../../src/boot.img \
		-f ../../doc/extract-doc $(SOURCES) >> $(DOCDB)
