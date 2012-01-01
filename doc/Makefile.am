# Makefile.am for STklos documentation
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)
# Last file update:  1-Jan-2012 18:28 (eg)

mandir	      = $(prefix)/share/man
man_MANS      = stklos.1 stklos-config.1 stklos-compile.1 stklos-genlex.1 \
	        stklos-pkg.1
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
dochtml_DATA= $(wildcard html/stklos-ref*.html) $(wildcard html/*.css)

docimgdir   = $(doclocation)/html/images
docimg_DATA = $(wildcard html/images/*)


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
	/bin/rm -f Makefile

