# Makefile for stklos-pkgman
#
# Copyright © 2006-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
#    Creation date: 22-Dec-2006 12:00 (eg)
# Last file update: 21-Sep-2020 11:53 (eg)


makefiledir  = $(prefix)/share/@PACKAGE@/@VERSION@/etc
makefile_DATA= etc/Makefile-scmpkg.tmpl

bin_SCRIPTS = stklos-pkg

LEX     = lang-bigloo.l lang-chicken.l lang-mzscheme.l
LEXOBJS = lang-bigloo.inc lang-chicken.inc lang-mzscheme.inc
SRC     = ../lib/http.stk \
	  main.stk add.stk copy.stk extract.stk install.stk lang.stk \
	  params.stk pkgball.stk \
	  repository.stk rewrite.stk misc.stk tune.stk types.stk
SFLAGS  =
RM      = /bin/rm
COMP	?= ../utils/tmpcomp
GENLEX  ?= ../utils/tmpgenlex

all: $(bin_SCRIPTS)

install-sources:


lang-bigloo.inc: lang-bigloo.l
	$(GENLEX) lang-bigloo.l lang-bigloo.inc lang-bigloo

lang-chicken.inc: lang-chicken.l
	$(GENLEX) lang-chicken.l lang-chicken.inc lang-chicken

lang-mzscheme.inc: lang-mzscheme.l
	$(GENLEX) lang-mzscheme.l lang-mzscheme.inc lang-mzscheme

$(bin_SCRIPTS):  $(LEXOBJS) $(SRC)
	$(COMP) -o $(bin_SCRIPTS) main.stk
	rm -f  $(LEXOBJS)

clean:
	$(RM) -f $(bin_SCRIPTS) $(LEXOBJS) *~

distclean: clean
	$(RM) -f Makefile
