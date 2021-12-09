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
# Last file update:  9-Dec-2021 14:24 (eg)

#======================================================================

COMP ?= ../../utils/tmpcomp
STKLOS_BINARY ?= ../../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive
SO = @SH_SUFFIX@

#
# SRFI written in Scheme
#
SRC_STK   = 1.stk   \
            2.stk   \
            4.stk   \
            5.stk   \
            6.stk   \
            7.stk   \
            8.stk   \
            9.stk   \
            10.stk  \
            11.stk  \
            13.stk  \
            14.stk  \
            16.stk  \
            17.stk  \
            18.stk  \
            22.stk  \
            23.stk  \
            26.stk  \
            28.stk  \
            29.stk  \
            31.stk  \
            60.stk  \
            64.stk  \
            66.stk  \
            74.stk  \
            88.stk  \
            111.stk \
            113.stk \
            118.stk \
            127.stk \
            128.stk \
            141.stk \
            161.stk \
            173.stk \
            174.stk \
            180.stk \
            185.stk \
            189.stk \
            195.stk \
            196.stk \
            208.stk \
            214.stk \
            215.stk \
            223.stk

SRC_OSTK =  1.ostk   \
            2.ostk   \
            4.ostk   \
            5.ostk   \
            6.ostk   \
            7.ostk   \
            8.ostk   \
            9.ostk   \
            10.ostk  \
            11.ostk  \
            13.ostk  \
            14.ostk  \
            16.ostk  \
            17.ostk  \
            18.ostk  \
            22.ostk  \
            23.ostk  \
            26.ostk  \
            28.ostk  \
            29.ostk  \
            31.ostk  \
            60.ostk  \
            64.ostk  \
            66.ostk  \
            74.ostk  \
            88.ostk  \
            111.ostk \
            113.ostk \
            118.ostk \
            127.ostk \
            128.ostk \
            141.ostk \
            161.ostk \
            173.ostk \
            174.ostk \
            180.ostk \
            185.ostk \
            189.ostk \
            195.ostk \
            196.ostk \
            208.ostk \
            214.ostk \
            215.ostk \
            223.ostk

#
# SRFIs written in C and Scheme
#

SRC_C     = 25.c     27.c     132.c     133.c      144.c      170.c      175.c
SRC_C_STK = 25.stk   27.stk   132.stk   133.stk    144.stk    170.stk    175.stk
SRC_SHOBJ = 25.$(SO) 27.$(SO) 132.$(SO) 133.$(SO)  144.$(SO)  170.$(SO)  175.$(SO)

srfi_OBJS = $(SRC_OSTK) $(SRC_SHOBJ)


DOCDB  = ../../DOCDB
BASEDIR= ../..
SOURCES= $(SRC_C) $(SRC_STK)

#======================================================================
srfidir       = $(prefix)/share/@PACKAGE@/@VERSION@/srfi
srfi_sources  = $(SRC_STK)
srfi_DATA     = $(srfi_OBJS)

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
13.ostk: 14.ostk
113.ostk:  128.ostk

25.$(SO):  25-incl.c  25.c
27.$(SO):  27-incl.c  27.c
132.$(SO): 132-incl.c 132.c
133.$(SO): 133-incl.c 133.c
144.$(SO): 144-incl.c 144.c
170.$(SO): 170-incl.c 170.c
175.$(SO): 175-incl.c 175.c

#======================================================================

install-sources:
	install $(srfi_sources) $(srfidir)

clean:
	rm -f $(srfi_OBJS) *-incl.c *~

distclean: clean
	/bin/rm -f Makefile

uninstall-hook:
	(cd $(srfidir); rm -f $(srfi_sources))
	rmdir $(srfidir) || true

doc:
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ../../src/boot.img \
		-f ../../doc/extract-doc $(SOURCES) >> $(DOCDB)
