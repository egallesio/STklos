# Makefile for STklos GTklos extension
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
#    Creation date: 12-May-2021 06:54 (eg)
# Last file update: 11-Jun-2021 14:36 (eg)

EXTENSION=gtklos

SCM_SRCS = sys/events.stk sys/libs.stk sys/utils.stk                 \
           classes/meta.stk classes/container.stk classes/widget.stk \
           widgets/box.stk widgets/button.stk widgets/entry.stk      \
           widgets/label.stk widgets/window.stk

#======================================================================
OUT            = $(EXTENSION).@SH_SUFFIX@
schemedir      = $(prefix)/share/@PACKAGE@/@VERSION@
schemelibdir   = $(prefix)/lib/@PACKAGE@/@VERSION@

GTKFLAGS       = `pkg-config --cflags gtk+-3.0`

#======================================================================
COMP ?= ../../utils/tmpcomp
STKLOS_BINARY ?= ../../src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive
# ======================================================================
SOURCES= $(EXTENSION).c $(EXTENSION).stk
DOCDB  = ../DOCDB
BASEDIR= ../..

SUFFIXES = .stk -incl.c .@SH_SUFFIX@ .c

.stk-incl.c:
	$(COMP) -C -o $*-incl.c $*.stk

.c.@SH_SUFFIX@ :
	@CC@ @CFLAGS@ $(GTKFLAGS) @STKCFLAGS@ @SH_COMP_FLAGS@ \
         -I../../src @GCINC@ @GMPINC@ \
	     -c -o $*.o $*.c
	@SH_LOADER@ @SH_LOAD_FLAGS@  $*.@SH_SUFFIX@ $*.o @DLLIBS@
	/bin/rm -f $*.o

$(EXTENSION)-incl.c: $(EXTENSION).stk $(SCM_SRCS)


all: $(OUT)

$(OUT): $(EXTENSION)-incl.c $(EXTENSION).c


$(EXTENSION)-incl.c: $(EXTENSION).stk

# Scheme source is not complete => do not install
install-sources:
	@echo "No source file to install for $(EXTENSION)"

doc:
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b ../../src/boot.img \
		-f ../../doc/extract-doc $(SOURCES) >> $(DOCDB)

install-exec-hook:
	install -d $(DESTDIR)$(schemelibdir)
	install $(OUT) $(DESTDIR)$(schemelibdir)

clean:
	rm -f $(OUT) $(EXTENSION)-incl.c *~
