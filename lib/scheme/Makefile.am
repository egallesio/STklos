# Makefile for the R7RS Scheme standard libraries
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
#    Creation date:  8-Oct-2021 11:54 (eg)

#======================================================================

COMP ?= ../../utils/tmpcomp
STKLOS_BINARY ?= ../../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive
SO = @SH_SUFFIX@

DOCDB  = ../DOCDB
BASEDIR= ../..

# ----------------------------------------------------------------------
# R7RS Small Support
# ----------------------------------------------------------------------
R7_STK=    base.stk            \
           case-lambda.stk     \
           char.stk            \
           complex.stk         \
           cxr.stk             \
           eval.stk            \
           file.stk            \
           fixnum.stk          \
           inexact.stk         \
           lazy.stk            \
           load.stk            \
           process-context.stk \
           r5rs.stk            \
           read.stk            \
           repl.stk            \
           time.stk            \
           write.stk

R7_OBJS =  base.ostk            \
           case-lambda.ostk     \
           char.ostk            \
           complex.ostk         \
           cxr.ostk             \
           eval.ostk            \
           file.ostk            \
           fixnum.ostk          \
           inexact.ostk         \
           lazy.ostk            \
           load.ostk            \
           process-context.ostk \
           r5rs.ostk            \
           read.ostk            \
           repl.ostk            \
           time.ostk            \
           write.ostk

# ----------------------------------------------------------------------
# R7RS Large Support
# ----------------------------------------------------------------------
LARGE_STK  = charset.stk    \
             stream.stk     \
             box.stk        \
             set.stk        \
             list-queue.stk \
             hash-table.stk \
             lseq.stk       \
             comparator.stk \
             generator.stk  \
             ideque.stk     \
             text.stk       \
             division.stk   \
             bitwise.stk


LARGE_OBJS = charset.ostk    \
             stream.ostk     \
             box.ostk        \
             set.ostk        \
             list-queue.ostk \
             hash-table.ostk \
             lseq.ostk       \
             comparator.ostk \
             generator.ostk  \
             ideque.ostk     \
             text.ostk       \
             division.ostk   \
             bitwise.ostk

LARGE_C     = list.c      ilist.c     flonum.c     sort.c     vector.c     bytevector.c
LARGE_C_STK = list.stk    ilist.stk   flonum.stk   sort.stk   vector.stk   bytevector.stk
LARGE_SHOBJ = list.$(SO)  ilist.$(SO) flonum.$(SO) sort.$(SO) vector.$(SO) bytevector.$(SO)


#----------------------------------------------------------------------
SRC_STK     = $(R7_STK) $(LARGE_STK)   $(LARGE_C_STK)
SRC_C_STK   = $(LARGE_C_STK)
scheme_OBJS = $(R7_OBJS) $(LARGE_OBJS) 

SOURCES = $(SRC_STK) $(LARGE_C)

#======================================================================
schemedir       = $(prefix)/share/@PACKAGE@/@VERSION@/scheme
schemelibdir    = $(prefix)/lib/@PACKAGE@/@VERSION@/scheme
scheme_sources  = $(SRC_STK)
scheme_DATA     = $(scheme_OBJS)
schemelib_DATA  = $(LARGE_SHOBJ)

#======================================================================
SUFFIXES = .stk .ostk .stk -incl.c .$(SO) .c
.stk.ostk:
	$(COMP) -o $*.ostk $*.stk

.stk-incl.c:
	$(COMP) -C -o $*-incl.c $*.stk

.c.$(SO) :
	@CC@ @CFLAGS@ @STKCFLAGS@ @SH_COMP_FLAGS@ -I../../src @GCINC@ @GMPINC@ -c -o $*.o $*.c
	@SH_LOADER@ @SH_LOAD_FLAGS@ -o $*.$(SO) $*.o @DLLIBS@
	@/bin/rm -f $*.o


#======================================================================
# Dependencies
#======================================================================
# DEPS= ../srfi/1.ostk    ../srfi/14.ostk  ../srfi/41.ostk   \
#        ../srfi/111.ostk ../srfi/113.ostk ../srfi/116.$(SO) \
#        ../srfi/117.ostk ../srfi/125.ostk
#
#
# $(DEPS):
# 	(cd ../srfi; $(MAKE)  `basename $@`)


stream.ostk:     list.$(SO) ../streams/derived.ostk
../streams/derived.ostk:
	(cd ../streams; $(MAKE) derived.ostk)

set.ostk: ../srfi/69.ostk comparator.ostk

../srfi/69.ostk:
	(cd ../srfi; $(MAKE) 69.ostk)
../srfi/128.ostk:
	(cd ../srfi; $(MAKE) 128.ostk)
../srfi/129.ostk:
	(cd ../srfi; $(MAKE) 129.ostk)
../srfi/60.ostk:
	(cd ../srfi; $(MAKE) 60.ostk)

bytevector.$(SO): bytevector-incl.c bytevector.c

list.$(SO): list-incl.c
list-incl.c: list.stk

ilist.$(SO): ilist-incl.c
ilist-incl.c: comparator.ostk

flonum.$(SO): flonum-incl.c
flonum-incl.c: flonum.stk

sort.$(SO): sort-incl.c
sort-incl.c: sort.stk

vector.$(SO): vector-incl.c
vector-incl.c: vector.stk

hash-table.ostk: comparator.ostk
ideque.ostk: generator.ostk list.$(SO)
text.ostk: ../srfi/129.ostk
bitwise.ostk: ../srfi/60.ostk

# ======================================================================
install-sources:
	cp $(scheme_sources) $(schemedir)

clean:
	rm -f $(scheme_OBJS) *-incl.c *~

distclean: clean
	/bin/rm -f Makefile

uninstall-hook:
	(cd $(schemedir); rm -f $(scheme_sources))
	rmdir $(schemedir) || true

doc:
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -f ../../doc/extract-doc \
			 $(SOURCES) >> $(DOCDB)

# Local Variables:
# tab-width: 8
# End:
