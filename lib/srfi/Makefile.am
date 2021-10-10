# Makefile for the R7RS SRFI standard libraries
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
#    Creation date:  8-Oct-2021 11:54 (eg)
# Last file update: 10-Oct-2021 20:05 (eg)

#======================================================================

COMP ?= ../../utils/tmpcomp
STKLOS_BINARY ?= ../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive

SRC_STK   = 1.stk 2.stk 4.stk 5.stk 6.stk 7.stk 8.stk

srfi_OBJS = 1.ostk 2.ostk 4.ostk 5.ostk 6.ostk 7.ostk 8.ostk


#======================================================================
srfidir       = $(prefix)/share/@PACKAGE@/@VERSION@/srfi
srfi_sources  = $(SRC_STK)
srfi_DATA     = $(srfi_OBJS)

#======================================================================
SUFFIXES = .stk .ostk

.stk.ostk:
	$(COMP) -o $*.ostk $*.stk


#======================================================================

install-sources:
	install $(srfi_sources) $(srfidir)

clean:
	rm -f $(srfi_OBJS)  *~

distclean: clean
	/bin/rm -f Makefile

uninstall-hook:
	(cd $(srfidir); rm -f $(srfi_sources))
	rmdir $(srfidir) || true
