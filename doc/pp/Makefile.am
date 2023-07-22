# Makefile for producing STklos documentation (not using autoconf stuff)
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date:  1-Sep-2004 16:19 (eg)


ADOC=@ASCIIDOCTOR@

SOURCE  = pp.adoc
DOCDB   =
SPP     = ../../src/stklos -q -I ../../lib -f ../../utils/stklos-pp.stk --

SAMPLE    = pp-demo.html
HTMLMAIN  = pp.html
PDF       = pp.pdf

all-recursive: all

all: ../HTML/$(HTMLMAIN) ../HTML/$(SAMPLE)
pdf: ../PDF/$(PDF)
extradoc: pdf

# ----------------------------------------------------------------------
../HTML/$(HTMLMAIN): tmp-html.txt
	mkdir -p ../HTML
	$(ADOC) -v -o $@ tmp-html.txt

../HTML/$(SAMPLE): Resources/mymd-prelude.templ  Resources/mymd-postlude.templ \
                   Resources/pp-demo.md
	mkdir -p ../HTML
	(cd Resources; build-pp-demo.sh)

# ----------------------------------------------------------------------
../PDF/$(PDF): tmp-pdf.txt
	mkdir -p ../PDF
	$(ADOC) -v -r asciidoctor-pdf -b pdf -o $@ tmp-pdf.txt

# ----------------------------------------------------------------------
tmp-html.txt: $(SOURCE) $(DOCDB)
	$(SPP) -D "doc-fmt: 'html" $(SOURCE) > $@

tmp-pdf.txt: $(SOURCE) $(DOCDB)
	$(SPP) -D "doc-fmt: 'pdf" $(SOURCE) > $@

# ----------------------------------------------------------------------
clean:
	rm -f  tmp-html.txt tmp-pdf.txt  Resources/*~ *~

distclean: clean
	rm -f ../HTML/$(HTMLMAIN) ../HTML/$(SAMPLE)
	rm -f ../PDF/$(PDF) ../PDF/$(SAMPLE)
