# Makefile for STklos srfi-175 library
#
# Copyright © 2021 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
#    Creation date: 12-May-2021 06:54 (eg)
# Last file update:  1-Jun-2021 17:48 (eg)

SRFI=srfi-175

#======================================================================
OUT            = $(SRFI).@SH_SUFFIX@
schemedir      = $(prefix)/share/@PACKAGE@/@VERSION@
schemelibdir   = $(prefix)/lib/@PACKAGE@/@VERSION@
schemelib_DATA = $(OUT)


#======================================================================
COMP ?= ../../utils/tmpcomp
STKLOS_BINARY ?= ../../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive
# ======================================================================
SOURCES=$(SRFI).c $(SRFI).stk
DOCDB  = ../DOCDB
BASEDIR= ../..

SUFFIXES = .stk -incl.c .@SH_SUFFIX@ .c

.stk-incl.c:
	$(COMP) -C -o $*-incl.c $*.stk

.c.@SH_SUFFIX@ :
	@CC@ @CFLAGS@ @STKCFLAGS@ @SH_COMP_FLAGS@ -I../../src @GCINC@ @GMPINC@ \
	-c -o $*.o $*.c
	@SH_LOADER@ @SH_LOAD_FLAGS@  $*.@SH_SUFFIX@ $*.o @DLLIBS@
	/bin/rm -f $*.o


all: $(OUT)

$(OUT): $(SRFI)-incl.c $(SRFI).c


$(SRFI)-incl.c: $(SRFI).stk

# Scheme source is not complete => do not install
install-sources:
	@echo "No source file to install for $(SRFI)"

doc:
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ../../src/boot.img \
		-f ../../doc/extract-doc $(SOURCES) >> $(DOCDB)

#install-exec-hook:
#	install -d $(DESTDIR)$(schemelibdir)
#	install $(OUT) $(DESTDIR)$(schemelibdir)

clean:
	rm -f $(OUT) $(SRFI)-incl.c *~
