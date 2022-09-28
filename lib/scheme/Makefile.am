# Makefile for the R7RS Scheme standard libraries
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
# Last file update: 28-Sep-2022 16:04 (eg)

#======================================================================

COMP ?= ../../utils/tmpcomp
STKLOS_BINARY ?= ../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive

SRC_STK = base.stk            \
          case-lambda.stk     \
          char.stk            \
          complex.stk         \
          cxr.stk             \
          eval.stk            \
          file.stk            \
          inexact.stk         \
          lazy.stk            \
          load.stk            \
          process-context.stk \
          r5rs.stk            \
          read.stk            \
          repl.stk            \
          time.stk            \
          write.stk           \
          list.stk            \
          hash-table.stk

scheme_OBJS = base.ostk            \
              case-lambda.ostk     \
              char.ostk            \
              complex.ostk         \
              cxr.ostk             \
              eval.ostk            \
              file.ostk            \
              inexact.ostk         \
              lazy.ostk            \
              load.ostk            \
              process-context.ostk \
              r5rs.ostk            \
              read.ostk            \
              repl.ostk            \
              time.ostk            \
              write.ostk           \
              list.ostk            \
              hash-table.ostk

#======================================================================
schemedir       = $(prefix)/share/@PACKAGE@/@VERSION@/scheme
scheme_sources  = $(SRC_STK)
scheme_DATA     = $(scheme_OBJS)

#======================================================================
SUFFIXES = .stk .ostk
.stk.ostk:
	$(COMP) -o $*.ostk $*.stk

.scm.ostk:
	$(COMP) -o $*.ostk $*.scm

#======================================================================
# Dependencies
#======================================================================
# LIST library
list.ostk: ../srfi/1.ostk
../srfi/1.ostk:
	(cd ../srfi; $(MAKE)  $@)


# HASH-TABLE library 
hash-table.ostk: ../srfi/125.ostk

../srfi/125.ostk:
	(cd ../srfi; $(MAKE)  $@)

# ======================================================================

install-sources:
	install $(scheme_sources) $(schemedir)

clean:
	rm -f $(scheme_OBJS)  *~

distclean: clean
	/bin/rm -f Makefile

uninstall-hook:
	(cd $(schemedir); rm -f $(scheme_sources))
	rmdir $(schemedir) || true
