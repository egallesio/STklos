# Makefile.am for STklos documentation
#
# Copyright © 1999-2023 Erick Gallesio <eg@stklos.net>
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
#r
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)

.PHONY: subdoc docdb extradoc

# mandir      = $(prefix)/share/man
man_MANS      = stklos.1 stklos-config.1 stklos-compile.1 stklos-genlex.1 \
	        stklos-pp.1 stklos-script.1
EXTRA_DIST    = $(man_MANS)

DOCDB       = DOCDB
OTHERDB	    = ../src/$(DOCDB) ../lib/$(DOCDB)
ALLDB       = $(OTHERDB) ./$(DOCDB)

schemedir   = $(prefix)/share/@PACKAGE@/@VERSION@
scheme_DATA = DOCDB

#doclocation = $(prefix)/share/doc/@PACKAGE@-@VERSION@
doclocation = $(docdir)/@VERSION@

docpdfdir   = $(doclocation)/pdf
docpdf_DATA = PDF/stklos-ref.pdf

dochtmldir  = $(doclocation)/html
dochtml_DATA= HTML/stklos-ref.html

docimgdir   = $(dochtmldir)/images
docimg_DATA = HTML/images/hierarchy.png

SUBDOCS     = refman vm hacking pp

all: $(DOCDB) ../utils/stklos-pp
	for d in $(SUBDOCS); do (cd $$d; $(MAKE) $@); done

extradoc: $(DOCDB)
	for d in $(SUBDOCS); do (cd $$d; $(MAKE) $@); done

../utils/stklos-pp:
	(cd ../utils; $(MAKE) stklos-pp)

#
# Database construction
#
$(DOCDB): $(OTHERDB)
	cat $(OTHERDB) > $(DOCDB)

../src/$(DOCDB):
	(cd ../src; $(MAKE) doc)

../lib/$(DOCDB):
	(cd ../lib; $(MAKE) doc)


#
# Documentation
#
clean:
	/bin/rm -f $(ALLDB)  stklos*.1 *~

distclean: clean
	/bin/rm -f Makefile

uninstall-hook:
	rmdir $(docpdfdir) $(docimgdir) $(dochtmldir) $(doclocation) || true


# Local Variables:
# tab-width: 8
# End:
