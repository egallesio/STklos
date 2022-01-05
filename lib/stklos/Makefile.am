# Makefile for the STklos specific  libraries
#
# Copyright © 2021-2022 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
# Last file update:  5-Jan-2022 11:57 (eg)

#======================================================================

COMP ?= ../../utils/tmpcomp
STKLOS_BINARY ?= ../../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive
SO = @SH_SUFFIX@

SRC_STK = markdown.stk
SRC_C   =

scheme_OBJS = markdown.ostk


DOCDB  = ../DOCDB
SOURCES= $(SRC_C) $(SRC_STK)

#======================================================================
schemedir       = $(prefix)/share/@PACKAGE@/@VERSION@/stklos
scheme_sources  = $(SRC_STK)
scheme_DATA     = $(scheme_OBJS)

#======================================================================
SUFFIXES = .stk .ostk
.stk.ostk:
	$(COMP) -o $*.ostk $*.stk

.scm.ostk:
	$(COMP) -o $*.ostk $*.scm
#======================================================================

install-sources:
	install $(scheme_sources) $(schemedir)

clean:
	rm -f $(scheme_OBJS)  *~

distclean: clean
	/bin/rm -f Makefile

uninstall-hook:
	(cd $(schemedir); rm -f $(scheme_sources))
	rmdir $(schemedir) || true

doc:
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ../../src/boot.img \
		-f ../../doc/extract-doc $(SOURCES) >> $(DOCDB)