# Makefile for producing STklos documentation (not using autoconf stuff)
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date:  1-Sep-2004 16:19 (eg)


ADOC=@ASCIIDOCTOR@

SOURCE  = vm.adoc
DOCDB   =
SPP     = ../../src/stklos -q -I ../../lib -f ../../utils/stklos-pp.stk --

HTMLMAIN  = vm.html
PDF       = vm.pdf

all-recursive: all

all: ../HTML/$(HTMLMAIN)
pdf: ../PDF/$(PDF)
extradoc: pdf

# ----------------------------------------------------------------------
../HTML/$(HTMLMAIN): tmp-html.txt
	mkdir -p ../HTML
	$(ADOC) -v -o $@ tmp-html.txt

# ----------------------------------------------------------------------
../PDF/$(PDF): tmp-pdf.txt
	mkdir -p ../PDF
	$(ADOC) -v -r asciidoctor-pdf -b pdf -o tmp.pdf tmp-pdf.txt
	@echo "Reducing size of generated PDF file"
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress \
		-dNOPAUSE -dQUIET -dBATCH -sOutputFile=$@ tmp.pdf
	rm -f tmp.pdf

# ----------------------------------------------------------------------
tmp-html.txt: $(SOURCE) $(DOCDB)
	$(SPP) -D "doc-fmt: 'html" $(SOURCE) > $@

tmp-pdf.txt: $(SOURCE) $(DOCDB)
	$(SPP) -D "doc-fmt: 'pdf" $(SOURCE) > $@

# ----------------------------------------------------------------------
clean:
	rm -f  tmp-html.txt tmp-pdf.txt tmp.pdf *~

distclean: clean
	rm -f ../HTML/$(HTMLMAIN)
	rm -f ../PDF/$(PDF)
