# Makefile for the SRFI 160
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
#    Creation date: 20-Jun-2022 09:24 (jpellegrini)

#-======================================================================


COMP ?= ../../../utils/tmpcomp
STKLOS_BINARY ?= ../../../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive
SO = @SH_SUFFIX@

SRC_STK = base.stk \
          s8.stk   \
          u8.stk   \
          s16.stk  \
          u16.stk  \
          s32.stk  \
          u32.stk  \
          s64.stk  \
          u64.stk  \
          f32.stk  \
          f64.stk  \
          c64.stk  \
          c128.stk


SRC_OSTK = base.ostk \
           s8.ostk   \
           u8.ostk   \
           s16.ostk  \
           u16.ostk  \
           s32.ostk  \
           u32.ostk  \
           s64.ostk  \
           u64.ostk  \
           f32.ostk  \
           f64.ostk  \
           c64.ostk  \
           c128.ostk

srfi_OBJS = $(SRC_OSTK)


DOCDB  = ../../DOCDB
BASEDIR= ../../..
SOURCES= $(SRC_C) $(SRC_STK) $(SRC_C_STK)


#======================================================================
srfidir       = @SCMDIR@/srfi/160
srfilibdir    = @EXECDIR@/srfi/160
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

base.ostk: ../../scheme/vector/base.$(SO)
s8.ostk:   ../../scheme/vector/s8.ostk
u8.ostk:   ../../scheme/vector/u8.ostk
s16.ostk:  ../../scheme/vector/s16.ostk
u16.ostk:  ../../scheme/vector/u16.ostk
s32.ostk:  ../../scheme/vector/s32.ostk
u32.ostk:  ../../scheme/vector/u32.ostk
s64.ostk:  ../../scheme/vector/s64.ostk
u64.ostk:  ../../scheme/vector/u64.ostk
f32.ostk:  ../../scheme/vector/f32.ostk
f64.ostk:  ../../scheme/vector/f64.ostk
c64.ostk:  ../../scheme/vector/c64.ostk
c128.ostk: ../../scheme/vector/c128.ostk

../../scheme/vector/base.$(SO):
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/s8.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/u8.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/s16.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/u16.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/s32.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/u32.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/s64.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/u64.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/f32.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/f64.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/c64.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)

../../scheme/vector/c128.ostk:
	(cd ../../scheme/vector; $(MAKE) $@)


#======================================================================

install-sources:
	mkdir -p $(srfidir)
	cp $(srfi_sources) $(srfidir)

clean:
	rm -f $(srfi_OBJS) *-incl.c *~

distclean: clean
	/bin/rm -f Makefile

uninstall-hook:
	(cd $(srfidir); rm -f $(srfi_sources) )
	rmdir $(srfidir) || true
##
## # no docs...
## # doc:
## # 	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ../../src/boot.img \
## # 		-f ../../doc/extract-doc $(SOURCES) >> $(DOCDB)

# Local Variables:
# tab-width: 8
# End:
