# Makefile.am for STklos documentation
#
# Copyright © 1999-2021 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
# Last file update:  1-Jun-2021 19:13 (eg)

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
dochtml_DATA= html/index.html		\
	      html/stklos-ref.html	\
	      html/stklos-ref-1.html	\
	      html/stklos-ref-2.html	\
	      html/stklos-ref-3.html	\
	      html/stklos-ref-4.html	\
	      html/stklos-ref-5.html	\
	      html/stklos-ref-6.html	\
	      html/stklos-ref-7.html	\
	      html/stklos-ref-8.html	\
	      html/stklos-ref-9.html	\
	      html/stklos-ref-10.html	\
              html/stklos-ref-11.html	\
	      html/stklos-ref-12.html	\
	      html/stklos-ref-13.html	\
	      html/stklos-ref-14.html	\
	      html/stklos-ref-15.html	\
	      html/stklos-ref-16.html	\
	      html/doc-style.css

docimgdir   = $(doclocation)/html/images
docimg_DATA = html/images/hierarchy.jpg


all: $(DOCDB)

skribe:
	(cd skb; $(MAKE) $(HTML) $(PDF))

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
	(cd skb; $(MAKE) distclean)
	/bin/rm -f Makefile

uninstall-hook:
	rmdir $(docpdfdir) $(docimgdir) $(dochtmldir) $(doclocation) || true
# Local Variables:
# tab-width: 8
# End:
