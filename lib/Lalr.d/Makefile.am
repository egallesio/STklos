# Makefile for lib/Lalr
#
# Copyright © 2004-2023 Erick Gallesio <eg@stklos.net>
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
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 25-Oct-2004 16:43 (eg)

schemedir   = @SCMDIR@
scheme_DATA = lalr.ostk

COMP ?= ../utils/tmpcomp

#======================================================================
SUFFIXES = .stk .ostk .scm
.stk.ostk:
	(cd ..; $(COMP) -o Lalr.d/$*.ostk Lalr.d/$*.stk)
#======================================================================
all: lalr.ostk calc

install-sources: install
	mkdir -p $(schemedir)
	cp lalr.stk $(DESTDIR)/$(schemedir)

uninstall-hook:
	rm  -f $(DESTDIR)/$(schemedir)/lalr.stk

calc: calc.stk
	echo "#!/bin/sh" > calc
	echo "../../src/stklos -f calc.stk" >> calc
	chmod a+x calc

clean: 
	/bin/rm -f *.ostk calc *~

doc:
