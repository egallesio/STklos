# Makefile for the R7RS Scheme standard libraries
#
# Copyright © 2021-2026 Erick Gallesio <eg@stklos.net>
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
SUBDIRS  = vector regex
COMP ?= ../../utils/tmpcomp
STKLOS_BINARY ?= ../../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive
SO = @SH_SUFFIX@

DOCDB   = ../DOCDB
BASEDIR = ../..
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
LARGE_STK  = stream.stk     \
             box.stk        \
             set.stk        \
             list-queue.stk \
             hash-table.stk \
             lseq.stk       \
             comparator.stk \
             generator.stk  \
             ideque.stk     \
             regex.stk      \
             text.stk       \
             division.stk   \
             bitwise.stk    \
             rlist.stk


LARGE_OBJS = stream.ostk     \
             box.ostk        \
             set.ostk        \
             list-queue.ostk \
             hash-table.ostk \
             lseq.ostk       \
             comparator.ostk \
             generator.ostk  \
             ideque.ostk     \
             regex.ostk     \
             text.ostk       \
             division.ostk   \
             bitwise.ostk

LARGE_C     = list.c      ilist.c     flonum.c     sort.c     vector.c     bytevector.c \
              charset.c   rlist.c
LARGE_C_STK = list.stk    ilist.stk   flonum.stk   sort.stk   vector.stk   bytevector.stk \
              charset.stk rlist.stk
LARGE_SHOBJ = list.$(SO)  ilist.$(SO) flonum.$(SO) sort.$(SO) vector.$(SO) bytevector.$(SO) \
              charset.$(SO) rlist.$(SO)



#----------------------------------------------------------------------
SRC_STK     = $(R7_STK) $(LARGE_STK)   $(LARGE_C_STK)
SRC_C_STK   = $(LARGE_C_STK)
scheme_OBJS = $(R7_OBJS) $(LARGE_OBJS) 

SOURCES = $(SRC_STK) $(LARGE_C)

#======================================================================
schemedir       = @SCMDIR@/scheme
schemelibdir    = @EXECDIR@/scheme
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
	@CC@ @CFLAGS@ @CPPFLAGS@ @STKCFLAGS@ @SH_COMP_FLAGS@ \
		-I../../src @GCINC@ @GMPINC@ -c -o $*.o $*.c
	@SH_LOADER@ @SH_LOAD_FLAGS@ @LDFLAGS@ -o $*.$(SO) $*.o @DLLIBS@
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
# 	(cd ../srfi && $(MAKE)  `basename $@`)


stream.ostk:     ../streams/primitive.ostk ../streams/derived.ostk

../streams/derived.ostk:
	(cd ../streams && $(MAKE) derived.ostk)

../streams/primitive.ostk:
	(cd ../streams && $(MAKE) primitive.ostk)

set.ostk: ../srfi/69.ostk comparator.ostk

charset.stk: ../srfi/217.ostk
charset.$(SO): charset-incl.c
charset-incl.c: charset.stk


../srfi/69.ostk:
	(cd ../srfi && $(MAKE) 69.ostk)
../srfi/128.ostk:
	(cd ../srfi && $(MAKE) 128.ostk)
../srfi/129.ostk:
	(cd ../srfi && $(MAKE) 129.ostk)
../srfi/60.ostk:
	(cd ../srfi && $(MAKE) 60.ostk)
../srfi/217.ostk:
	(cd ../srfi && $(MAKE) 217.ostk)
../stklos/itrie.$(SO):
	(cd ../stklos && $(MAKE) itrie.$(SO))


bytevector.$(SO): bytevector-incl.c bytevector.c

list.$(SO): list-incl.c
list-incl.c: list.stk

ilist.$(SO): ilist-incl.c
ilist-incl.c: comparator.ostk

flonum.$(SO): flonum-incl.c
flonum-incl.c: flonum.stk

rlist.$(SO): rlist-incl.c
rlist-incl.c: rlist.stk


sort.$(SO): sort-incl.c
sort-incl.c: sort.stk

vector.$(SO): vector-incl.c
vector-incl.c: vector.stk

charset.$(SO): charset-incl.c
charset-incl.c: charset.stk

rlist.$(SO): rlist-incl.c
rlist-incl.c: rlist.stk


hash-table.ostk: comparator.ostk
ideque.ostk: generator.ostk list.$(SO)
text.ostk: ../srfi/129.ostk
bitwise.ostk: ../srfi/60.ostk
regex.ostk: regex/boundary.stk hash-table.ostk list.$(SO) \
            charset.$(SO) bitwise.ostk


list-inc.c:       list.stk
ilist-inc.c:      ilist.stk
flonum-inc.c:     flonum.stk
sort-inc.c:       sort.stk
vector-inc.c:     vector.stk
bytevector-inc.c: bytevector.stk
charset-inc.c:    charset.stk


# ======================================================================
install-sources:
	@for i in $(SUBDIRS) ;do \
	   (cd $$i && $(MAKE) install-sources)\
	done
	cp $(scheme_sources) $(schemedir)

install-data-hook:
	(cd regex && $(MAKE) install)
clean:
	(cd vector && $(MAKE) clean)
	(cd regex && $(MAKE) clean)
	rm -f $(scheme_OBJS) *-incl.c *~

distclean: clean
	(cd vector && $(MAKE) distclean)
	(cd regex && $(MAKE) distclean)
	/bin/rm -f Makefile

uninstall-hook:
	(cd vector && $(MAKE) uninstall)
	(cd regex && $(MAKE) uninstall)
	(cd $(schemedir) && rm -f $(scheme_sources)) || true
	rmdir $(schemedir) || true

doc:
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -f ../../doc/extract-doc \
			 $(SOURCES) >> $(DOCDB)

# Local Variables:
# tab-width: 8
# End:
