# Makefile for STklos lib
#
#
# Copyright © 2000-2023 Erick Gallesio <eg@stklos.net>
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
#    Creation date: 11-Apr-2000 10:30 (eg)

SUBDIRS = Match.d SILex.d Lalr.d   \
	      scheme stklos streams srfi

COMP ?= ../utils/tmpcomp
STKLOS_BINARY ?= ../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive

scheme_BOOT = assembler.stk \
          bb.stk            \
          bonus.stk         \
          boot.stk          \
          callcc.stk        \
          compiler.stk      \
          compfile.stk      \
          compflags.stk     \
          computils.stk     \
          equiv.stk         \
          expand.pp         \
          ffi.stk           \
          library.stk       \
          load.stk          \
          logical.stk       \
          mbe.stk           \
          module.stk        \
          object.stk        \
          obsolete.stk      \
          peephole.stk      \
          process.stk       \
          r5rs.stk          \
          r7rs.stk          \
          regexp.stk        \
          readline.stk      \
          repl.stk          \
          repl-readline.stk \
          runtime.stk       \
          runtime-macros.stk\
          srfis-data.scm    \
          srfi-0.stk        \
          str.stk           \
          struct.stk        \
          time.stk          \
          thread.stk

scheme_extra = STklos.init

SRC_STK = bigmatch.stk        \
          conditions.stk      \
          describe.stk        \
          env.stk             \
          expand.ss           \
          full-syntax.stk     \
          getopt.stk          \
          help.stk            \
          lex-rt.stk          \
          make-C-boot.stk     \
          pretty-print.stk    \
          slib.stk            \
          trace.stk


scheme_OBJS = bigmatch.ostk     \
          conditions.ostk       \
          full-syntax.ostk      \
          describe.ostk         \
          env.ostk              \
          getopt.ostk           \
          help.ostk             \
          lex-rt.ostk           \
          pretty-print.ostk     \
          slib.ostk             \
          trace.ostk

SRC_C         = readline-complete.c
SRC_C_STK     =
scheme_SOLIBS = readline-complete.@SH_SUFFIX@


DOCDB       = DOCDB
BASEDIR     = ..


#======================================================================
schemedir       = $(prefix)/share/@PACKAGE@/@VERSION@
schemelibdir    = $(prefix)/lib/@PACKAGE@/@VERSION@
scheme_DATA     = $(scheme_extra) $(scheme_OBJS)
schemelib_DATA  = $(scheme_SOLIBS)
scheme_sources  = $(scheme_BOOT) $(scheme_extra) $(SRC_STK) $(SRC_C)
#======================================================================

SUFFIXES = .stk .ostk .scm  -incl.c  .$(SH_SUFFIX) .c
.stk.ostk:
	$(COMP) -o $*.ostk $*.stk

.scm.ostk:
	$(COMP) -o $*.ostk $*.scm

.stk-incl.c:
	$(COMP) -C -o $*-incl.c $*.stk

.c.@SH_SUFFIX@ :
	@CC@ @CFLAGS@ @STKCFLAGS@ @SH_COMP_FLAGS@ -I../src @GCINC@ \
	-c -o $*.o $*.c
	@SH_LOADER@ @SH_LOAD_FLAGS@ -o $*.@SH_SUFFIX@ $*.o @DLLIBS@
	/bin/rm -f $*.o

#======================================================================

all-recursive: boot generate-git-info

boot:	../src/boot.img

generate-git-info:
	@(cd ../src; ../utils/generate-git-info && $(MAKE) all)

#install-exec-hook:
#	mkdir -p $(DESTDIR)$(schemelibdir)
#	install $(scheme_SOLIBS) $(DESTDIR)$(schemelibdir)

../src/boot.img: $(scheme_BOOT)
	@echo "*** Boot 0"; \
	(export STKLOS_BUILDING=1; \
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -f bb.stk boot.img0 instr0)
	@echo "*** Boot 1"; \
	(export STKLOS_BUILDING=1; \
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ./boot.img0     -f bb.stk boot.img1 instr1)
	@echo "*** Boot 2"; \
	(export STKLOS_BUILDING=1; \
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ./boot.img1    -f bb.stk boot.img2 instr2)
	@echo "*** Boot 3"; \
	(export STKLOS_BUILDING=1; \
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ./boot.img2     -f bb.stk boot.img3 instr3)
	@if cmp ./boot.img2 ./boot.img3 ;then                      \
	   echo "*** New boot file created";                       \
	   cp ./boot.img3 ../src/boot.img;                         \
	   cp ./instr3    ../src/vm-instr.h;                       \
	   echo "*** Create new boot.c";                           \
	   (export STKLOS_BUILDING=1;                              \
		$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ../src/boot.img    \
		-f make-C-boot.stk -- boot.img3 ../src/boot.c);        \
	   echo "*** Recompile STklos";                            \
	   (cd ../src; $(MAKE) stklos);                            \
	   echo "*** Cleaning useless images";                     \
	   /bin/rm boot.img[0-3] instr[0-3];                       \
	else                                                       \
	   echo "*** Boot file creation failed";                   \
	   exit 1;                                                 \
	fi;

# Installation of *.stk files
install-sources:
	@for i in $(SUBDIRS) ;do \
	   (cd $$i; $(MAKE) install-sources)\
	done
	cp $(SRC_STK) $(scheme_BOOT) $(DESTDIR)/$(schemedir)


# Explicit some dependencies here
$(SCHEME_OBJS): ../src/boot.img
ffi.ostk: bigmatch.ostk
bigmatch.ostk: Match.d/compiler.scm Match.d/descr.scm Match.d/mexpand.scm \
               Match.d/normalize.scm Match.d/s2cfun.scm

# SLIB
slib.ostk: slib.stk STklos.init
STklos.init: srfi/59.stk
srfi/59.ostk:
	(cd srfi; make 59.ostk)


lib/scheme: lib
lib/stklos: lib
lib/streams: lib
lib/srfi: lib/scheme lib/stklos lib/streams

readline-complete.@SH_SUFFIX@: readline-complete.c
	@CC@ @CFLAGS@ @STKCFLAGS@ @SH_COMP_FLAGS@ -I../src @GCINC@ \
	-c -o $*.o $*.c
	@SH_LOADER@ @SH_LOAD_FLAGS@ -o $*.@SH_SUFFIX@ $*.o @DLLIBS@ @RDLINE@
	/bin/rm -f $*.o

#
# SRFIs support
#
# If ./srfis.stk we must rebuild
#     - srfis-data.scm (used by SRFI0 and features)
srfis-data.scm: srfis.stk getopt.ostk
	$(STKLOS_BINARY) -q -l srfis.stk \
	                 -f ../utils/update-srfi-list.stk \
                     -- --internal > $@
	(cd ..; $(MAKE) SUPPORTED-SRFIS)

doc: $(DOCDB)
	@for i in $(SUBDIRS) ;do \
	   (cd $$i; $(MAKE) doc)\
	done


$(DOCDB): $(scheme_sources)
	$(STKLOS_BINARY) -q -c -f ../doc/extract-doc $(scheme_sources) > $(DOCDB)

clean:
	/bin/rm -f $(scheme_OBJS) $(scheme_SOLIBS) ./srfis-data.scm
	@for i in $(SUBDIRS) ;do \
	   (cd $$i; $(MAKE) clean)\
	done

distclean: clean
	/bin/rm -f Makefile
	touch boot.stk
	@for i in $(SUBDIRS) ;do \
	   (cd $$i; $(MAKE) distclean)\
	done

uninstall-hook:
	(cd $(schemedir) && rm -f $(DOCDB) $(scheme_sources))
	rmdir $(schemedir) $(schemelibdir) || true


# Local Variables:
# tab-width: 4
# End:
