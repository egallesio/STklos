# Makefile for the R7RS SRFI standard libraries
#
# Copyright © 2021-2022 Erick Gallesio <eg@stklos.net>
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
# Last file update: 11-Nov-2022 16:00 (eg)

#-======================================================================

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
            15.stk  \
            16.stk  \
            17.stk  \
            18.stk  \
            22.stk  \
            23.stk  \
            26.stk  \
            28.stk  \
            29.stk  \
            31.stk  \
            34.stk  \
            35.stk  \
            36.stk  \
            37.stk  \
            38.stk  \
            39.stk  \
            41.stk  \
            45.stk  \
            48.stk  \
            51.stk  \
            54.stk  \
            55.stk  \
            59.stk  \
            60.stk  \
            61.stk  \
            62.stk  \
            64.stk  \
            66.stk  \
            69.stk  \
            70.stk  \
            74.stk  \
            87.stk  \
            88.stk  \
            89.stk  \
            94.stk  \
            95.stk  \
            96.stk  \
            100.stk \
            111.stk \
            113.stk \
            116.stk \
            117.stk \
            118.stk \
            125.stk \
            127.stk \
            128.stk \
            129.stk \
            130.stk \
            132.stk \
            133.stk \
            134.stk \
            135.stk \
            137.stk \
            141.stk \
            143.stk \
            151.stk \
            152.stk \
            154.stk \
            156.stk \
            158.stk \
            160.stk \
            161.stk \
            169.stk \
            171.stk \
            173.stk \
            174.stk \
            176.stk \
            180.stk \
            185.stk \
            189.stk \
            190.stk \
            193.stk \
            195.stk \
            196.stk \
            207.stk \
            208.stk \
            214.stk \
            215.stk \
            216.stk \
            217.stk \
            219.stk \
            221.stk \
            223.stk \
            224.stk \
            229.stk \
            230.stk \
            233.stk

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
            15.ostk  \
            16.ostk  \
            17.ostk  \
            18.ostk  \
            22.ostk  \
            23.ostk  \
            26.ostk  \
            28.ostk  \
            29.ostk  \
            31.ostk  \
            34.ostk  \
            35.ostk  \
            36.ostk  \
            37.ostk  \
            38.ostk  \
            39.ostk  \
            41.ostk  \
            45.ostk  \
            48.ostk  \
            51.ostk  \
            54.ostk  \
            55.ostk  \
            59.ostk  \
            60.ostk  \
            61.ostk  \
            62.ostk  \
            64.ostk  \
            66.ostk  \
            69.ostk  \
            70.ostk  \
            74.ostk  \
            87.ostk  \
            88.ostk  \
            89.ostk  \
            94.ostk  \
            95.ostk  \
            96.ostk  \
            100.ostk \
            111.ostk \
            116.ostk \
            117.ostk \
            118.ostk \
            113.ostk \
            125.ostk \
            127.ostk \
            128.ostk \
            129.ostk \
            130.ostk \
            132.ostk \
            133.ostk \
            134.ostk \
            135.ostk \
            137.ostk \
            141.ostk \
            143.ostk \
            151.ostk \
            152.ostk \
            154.ostk \
            156.ostk \
            158.ostk \
            160.ostk \
            161.ostk \
            169.ostk \
            171.ostk \
            173.ostk \
            174.ostk \
            176.ostk \
            180.ostk \
            185.ostk \
            189.ostk \
            190.ostk \
            193.ostk \
            195.ostk \
            196.ostk \
            207.ostk \
            208.ostk \
            214.ostk \
            215.ostk \
            216.ostk \
            217.ostk \
            219.ostk \
            221.ostk \
            223.ostk \
            224.ostk \
            229.ostk \
            230.ostk \
            233.ostk

#
# SRFIs written in C and Scheme
#
SRC_C     = 25.c     27.c     144.c      170.c      175.c
SRC_C_STK = 25.stk   27.stk   144.stk    170.stk    175.stk
SRC_SHOBJ = 25.$(SO) 27.$(SO) 144.$(SO)  170.$(SO)  175.$(SO)

srfi_OBJS = $(SRC_OSTK) $(SRC_SHOBJ)


DOCDB  = ../DOCDB
BASEDIR= ../..
SOURCES= $(SRC_C) $(SRC_STK) $(SRC_C_STK)

#======================================================================
srfidir       = $(prefix)/share/@PACKAGE@/@VERSION@/srfi
srfilibdir    = $(prefix)/lib/@PACKAGE@/@VERSION@/srfi
srfi_DATA     = $(SRC_OSTK)
srfilib_DATA  = $(SRC_SHOBJ)
srfi_sources  = $(SRC_STK)
#======================================================================
SUFFIXES = .stk .ostk .stk -incl.c .$(SO) .c

.stk.ostk:
	$(COMP) -o $*.ostk $*.stk

.stk-incl.c:
	$(COMP) -C -o $*-incl.c $*.stk

.c.$(SO) :
	@CC@ @CFLAGS@ @STKCFLAGS@ @SH_COMP_FLAGS@ -I../../src @GCINC@ @GMPINC@ -c -o $*.o $*.c
	@SH_LOADER@ @SH_LOAD_FLAGS@  $*.$(SO) $*.o @DLLIBS@
	@/bin/rm -f $*.o

#======================================================================
# Dependencies
1.ostk: ../scheme/list.ostk
13.ostk: 14.ostk
14.ostk: ../scheme/charset.ostk
37.ostk: 9.ostk 13.ostk
41.ostk: ../streams/derived.ostk
51.ostk: 1.ostk
64.ostk: 9.ostk 35.ostk
74.ostk: 4.ostk 26.ostk 60.ostk 66.ostk
94.ostk: 60.ostk
95.ostk:  ../scheme/sort.$(SO)
113.ostk: ../scheme/set.ostk
116.ostk: ../scheme/ilist.$(SO)
117.ostk: 1.ostk 9.ostk ../scheme/list-queue.ostk
125.ostk: 128.ostk ../scheme/hash-table.ostk
127.ostk: ../scheme/lseq.ostk
128.ostk: ../scheme/comparator.ostk
130.ostk: 13.ostk
132.ostk: ../scheme/sort.$(SO)
133.ostk: ../scheme/vector.$(SO)
134.ostk: 1.ostk 9.ostk 158.ostk
135.ostk: 9.ostk 129.ostk
151.ostk: 60.ostk
152.ostk: 13.ostk
154.ostk: 9.ostk
160.ostk: 160-subdir
161.ostk: 9.ostk
171.ostk: 1.ostk 9.ostk 69.ostk
173.ostk: 9.ostk
180.ostk: 9.ostk
189.ostk: 1.ostk 9.ostk
190.ostk: 158.ostk
195.ostk: 35.ostk
196.ostk: 1.ostk 9.ostk
207.ostk: 1.ostk 9.ostk 13.ostk 151.ostk 158.ostk
216.ostk: 27.$(SO)
217.ostk: ../stklos/itrie.$(SO)
221.ostk: 41.ostk 158.ostk
224.ostk: ../scheme/comparator.ostk
230.ostk: 9.ostk


25.$(SO):  25-incl.c  25.c
27.$(SO):  27-incl.c  27.c
116.$(SO): 128.ostk 116-incl.c 116.c
144.$(SO): 144-incl.c 144.c
170.$(SO): 170-incl.c 170.c
175.$(SO): 175-incl.c 175.c

../streams/derived.ostk:
	(cd ../streams; $(MAKE) derived.ostk)
../scheme/list.ostk:
	(cd ../scheme; $(MAKE) list.ostk)
../scheme/charset.ostk:
	(cd ../scheme; $(MAKE) charset.ostk)
../scheme/set.ostk:
	(cd ../scheme; $(MAKE) set.ostk)
../scheme/ilist.$(SO):
	(cd ../scheme; $(MAKE) ilist.$(SO))
../scheme/list-queue.ostk:
	(cd ../scheme; $(MAKE) list-queue.ostk)
../scheme/hash-table.ostk:
	(cd ../scheme; $(MAKE) hash-table.ostk)
../scheme/lseq.ostk:
	(cd ../scheme; $(MAKE) lseq.ostk)
../scheme/sort.$(SO):
	(cd ../scheme; $(MAKE) sort.$(SO))


160-subdir: 128.ostk
	(cd 160; $(MAKE))

#======================================================================

install-data-hook:
	(cd 160; $(MAKE) install)

install-sources:
	(cd 160; $(MAKE) install-sources)
	cp $(srfi_sources) $(srfidir)

clean:
	rm -f $(srfi_OBJS) *-incl.c *~
	(cd 160; $(MAKE) clean)

distclean: clean
	/bin/rm -f Makefile
	(cd 160; $(MAKE) distclean)

uninstall-hook:
	(cd 160; $(MAKE) uninstall)
	(cd $(srfidir); rm -f $(srfi_sources))
	rmdir $(srfidir) || true

doc:
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ../../src/boot.img \
		-f ../../doc/extract-doc $(SOURCES) >> $(DOCDB)
