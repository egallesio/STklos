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
# Last file update: 21-Jun-2022 08:47 (jpellegrini)

#-======================================================================


COMP ?= ../../../utils/tmpcomp
STKLOS_BINARY ?= ../../../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive
SO = @SH_SUFFIX@

# ORGANIZATION OF SRFI-160 IMPLEMENTATION:
#
# - base.c, base.stk: implementation of the base library,
#   (srfi 160 base), where there's also much code that is
#   used by the specific s8, u8, etc, libraries.
#
# - s8.stk, u8,stk, ..., c128.stk: the specific libraries
#   (srfi 160 s8), and so on. They have thin wrappers over
#   procedures defined in base.c
#
# - ../160.stk (in the directory above this one): the non-standard
#   (srfi 160) library. It is useful for the tests.
#
# the s8.stk, etc files are generated (with sed) from the file
# tagvector-template.stk.


SRC_STK = base.stk \
          tagvector-template.stk

SRC_OSTK = base.ostk \
           s8.ostk \
           u8.ostk \
           s16.ostk \
           u16.ostk \
           s32.ostk \
           u32.ostk \
           s64.ostk \
           u64.ostk \
           f32.ostk \
           f64.ostk \
           c64.ostk \
           c128.ostk

SRC_C     = base.c
SRC_C_STK = base.stk
SRC_SHOBJ = base.$(SO)

srfi_OBJS = $(SRC_OSTK) $(SRC_SHOBJ)

srfi_interm = s8.stk u8.stk s16.stk u16.stk \
	      s32.stk u32.stk s64.stk u64.stk \
	      f32.stk f64.stk c64.stk c128.stk


DOCDB  = ../../DOCDB
BASEDIR= ../../..
SOURCES= $(SRC_C) $(SRC_STK) $(SRC_C_STK)


#======================================================================
srfidir       = $(prefix)/share/@PACKAGE@/@VERSION@/srfi/160
srfilibdir    = $(prefix)/lib/@PACKAGE@/@VERSION@/srfi/160
srfi_DATA     = $(SRC_OSTK)
srfilib_DATA  = $(SRC_SHOBJ)
srfi_sources  = $(SRC_STK)
#======================================================================
SUFFIXES = .stk .ostk .stk -incl.c .$(SO) .c

# The procedures defined are exactly the same for all data types,
# only the type changes. So we generate them from a template.
#
# We use a multiple-target rule, which calls sed to change the
# string "TAG" in the template into s8, u8, etc, writing it to
# a new file "s8.stk", "u8.stk" etc.
s8.stk  u8.stk  s16.stk u16.stk \
s32.stk u32.stk s64.stk u64.stk \
f32.stk f64.stk c64.stk c128.stk: tagvector-template.stk base.so
	sed 's/TAG/$(basename $@)/g'   tagvector-template.stk > $@

#======================================================================

.stk.ostk:
	$(COMP) -o $*.ostk $*.stk

.stk-incl.c:
	$(COMP) -C -o $*-incl.c $*.stk

.c.$(SO) :
	@CC@ @CFLAGS@ @STKCFLAGS@ @SH_COMP_FLAGS@ -I../../../src @GCINC@ @GMPINC@ -c -o $*.o $*.c
	@SH_LOADER@ @SH_LOAD_FLAGS@  $*.$(SO) $*.o @DLLIBS@
	@/bin/rm -f $*.o

#======================================================================
# Dependencies

base.$(SO): base-incl.c base.c

#======================================================================


install-sources:
	install $(srfi_sources) $(srfidir)

# we also clean the TAG.stk files -- $(srfi-interm) --, which are NOT
# srfi_OBJS (should not be installed with other objects):
clean:
	rm -f $(srfi_OBJS) *-incl.c *~ $(srfi_interm)

distclean: clean
	/bin/rm -f Makefile

uninstall-hook:
	(cd $(srfidir); rm -f $(srfi_sources))
	rmdir $(srfidir) || true

# no docs...
# doc:
# 	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ../../src/boot.img \
# 		-f ../../doc/extract-doc $(SOURCES) >> $(DOCDB)
