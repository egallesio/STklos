# Makefile for producing STklos documentation (not using autoconf stuff)
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date:  9-Feb-2022 11:46

ADOC=@ASCIIDOCTOR@

TOP   =../../..
SPP   = $(TOP)/src/stklos -q -I $(TOP)/lib -I ../lib -f $(TOP)/utils/stklos-pp.stk --
MAIN  = gtklos
DOCS  = buttons.adoc canvases.adoc containers.adoc dialogs.adoc \
        displays.adoc events.adoc gtklos.adoc intro.adoc misc.adoc texts.adoc
SCM   = gendoc.stk
SRC   = $(DOCS) $(SCM)


all: $(MAIN).html  $(MAIN).pdf

$(MAIN).html: $(SRC)
	$(SPP) -D "doc-fmt: html" $(MAIN).adoc > $(MAIN).tmp
	$(ADOC) -v -o $@ $(MAIN).tmp
	rm -f $(MAIN).tmp

$(MAIN).pdf: $(SRC)
	$(SPP) -D "doc-fmt: pdf" $(MAIN).adoc > $(MAIN).tmp
	$(ADOC) -r asciidoctor-pdf -b pdf -v -o $@ $(MAIN).tmp
	rm -f $(MAIN).tmp

clean:
	rm -f $(MAIN).pdf $(MAIN).html $(MAIN).tmp *~

distclean: clean
