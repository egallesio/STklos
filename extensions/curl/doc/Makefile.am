# Makefile for producing STklos documentation (not using autoconf stuff)
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date:  9-Feb-2022 11:46

ADOC=@ASCIIDOCTOR@

TOP   =../../..
SPP   = $(TOP)/src/stklos -q -I $(TOP)/lib -f $(TOP)/utils/stklos-pp.stk --
FILE  = curl-extension

all: $(FILE).html  $(FILE).pdf

$(FILE).html: $(FILE).adoc
	$(SPP) $(FILE).adoc > $(FILE).tmp
	$(ADOC) -v -o $@ $(FILE).tmp
	rm -f $(FILE).tmp

$(FILE).pdf: $(FILE).adoc
	$(SPP) $(FILE).adoc > $(FILE).tmp
	$(ADOC) -r asciidoctor-pdf -b pdf -v -o $@ $(FILE).tmp
	rm -f $(FILE).tmp

clean:
	rm -f $(FILE).pdf $(FILE).html $(FILE).tmp *~

distclean: clean
