# Makefile for the SRFI 225
#
# Copyright © 2022 Jeronimo Pellegrini <j_p@aleph0.info>
# Based on the SRFI Makefiles by Erick Gallesio.
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
#           Author: Jeronimo Pellegrini [j_p@aleph0.info]
#    Creation date: 21-Jul-2025 15:53 (jpellegrini)

#-======================================================================

COMP ?= ../../../utils/tmpcomp
STKLOS_BINARY ?= ../../../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive
SO = @SH_SUFFIX@

SRC_STK = alist-impl.stk \
           srfi-125-impl.stk \
           default-impl.stk \
           indexes.stk \
           core-impl.stk \
           core.stk

SRC_OSTK = alist-impl.ostk \
           srfi-125-impl.ostk \
           default-impl.ostk \
           indexes.ostk \
           core-impl.ostk \
           core.ostk

srfi_OBJS = $(SRC_OSTK)


DOCDB  = ../../DOCDB
BASEDIR= ../../..
SOURCES= $(SRC_C) $(SRC_STK) $(SRC_C_STK)


#======================================================================
srfidir       = @SCMDIR@/srfi/225
srfilibdir    = @EXECDIR@/srfi/225
srfi_DATA     = $(SRC_OSTK)
srfilib_DATA  = $(SRC_SHOBJ)
srfi_sources  = $(SRC_STK)
#======================================================================
SUFFIXES = .stk .ostk .stk -incl.c .$(SO) .c

#======================================================================

.stk.ostk:
	$(COMP) -o $*.ostk $*.stk

#======================================================================
# Dependencies
core.ostk:  core-impl.ostk

#======================================================================

install-sources:
	mkdir -p $(DESTDIR)$(srfidir)
	cp $(srfi_sources) $(DESTDIR)$(srfidir)

clean:
	rm -f $(srfi_OBJS) *-incl.c *~

distclean: clean
	/bin/rm -f Makefile

uninstall-hook:
	(cd $(PREFIX)$(srfidir) && rm -f $(srfi_sources) ) || true
	rmdir $(srfidir) || true

# Local Variables:
# tab-width: 8
# End:
