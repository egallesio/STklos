# Makefile for producing STklos documentation (not using autoconf stuff)
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date:  1-Sep-2004 16:19 (eg)
# Last file update: 14-Mar-2022 19:12 (eg)


ADOC=@ASCIIDOCTOR@

SOURCE  = hacking.adoc
DOCDB   =
SPP     = ../../src/stklos -q -I ../../lib -f ../../utils/stklos-pp.stk --

HTMLMAIN  = hacking.html
PDF       = hacking.pdf

all-recursive: all

all: ../HTML/$(HTMLMAIN)
pdf: ../PDF/$(PDF)
extradoc: pdf

# ----------------------------------------------------------------------
../HTML/$(HTMLMAIN): tmp-html.txt
	$(ADOC) -v -o $@ tmp-html.txt

# ----------------------------------------------------------------------
../PDF/$(PDF): tmp-pdf.txt
	mkdir -p ../pdf
	$(ADOC) -v -r asciidoctor-pdf -b pdf -o $@ tmp-pdf.txt

# ----------------------------------------------------------------------
tmp-html.txt: $(SOURCE) $(DOCDB)
	$(SPP) -D "doc-fmt: 'html" $(SOURCE) > $@

tmp-pdf.txt: $(SOURCE) $(DOCDB)
	$(SPP) -D "doc-fmt: 'pdf" $(SOURCE) > $@

# ----------------------------------------------------------------------
clean:
	rm -f  tmp-html.txt tmp-pdf.txt  *~

distclean: clean
	rm -f  ../HTML/$(HTMLMAIN) 
	rm -f ../PDF/$(PDF)
