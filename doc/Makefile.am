# Makefile.am for STklos documentation
#
# Copyright © 1999-2022 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
# Last file update: 26-Jan-2022 11:59 (eg)

SUBDIRS      = refman

mandir	      = $(prefix)/share/man
man_MANS      = stklos.1 stklos-config.1 stklos-compile.1 stklos-genlex.1 \
	        stklos-pkg.1 stklos-script.1
EXTRA_DIST    = $(man_MANS)

DOCDB       = DOCDB
OTHERDB	    = ../src/$(DOCDB) ../lib/$(DOCDB)
ALLDB       = $(OTHERDB) ./$(DOCDB)

schemedir   = $(prefix)/share/@PACKAGE@/@VERSION@
scheme_DATA = DOCDB

doclocation = $(prefix)/share/doc/@PACKAGE@-@VERSION@

docpdfdir   = $(doclocation)/pdf
docpdf_DATA = pdf/stklos-ref.pdf

dochtmldir  = $(doclocation)/html
dochtml_DATA= html/stklos-ref.html



docimgdir   = $(doclocation)/html/images
docimg_DATA = html/images/hierarchy.png


all: docdb

docdb: $(DOCDB)

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
