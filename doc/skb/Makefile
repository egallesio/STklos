# Makefile for producing STklos documentation
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date:  1-Sep-2004 16:19 (eg)
# Last file update:  3-Nov-2005 18:19 (eg)


SOURCES  = biblio.skb cond.skb custom.skb expr.skb index.skb 		\
	   intro.skb match.skb object.skb	 			\
	   overview.skb progstruct.skb regexp.skb slib.skb srfi.skb	\
	   stdproc.skb stklos.skb
BIB	 = biblio.skbib
DOCDB    = ../DOCDB

PDF      = stklos-ref.pdf
HTMLMAIN = stklos-ref.html
HTML     = ./html/$(HTMLMAIN)
CTEX	 = stklos-ref.ctex
CSS	 = doc-style.css
IMG	 = images/hierarchy.jpg

all:  Html
Html: $(HTML)
Pdf:  $(PDF)


$(PDF): $(CTEX)
	texexec $(CTEX)

$(CTEX): $(SOURCES) $(DOCDB) $(BIB) $(IMG)
	skribe -o $(CTEX) stklos.skb


$(HTML): $(SOURCES) $(DOCDB) $(BIB) $(IMG)
	skribe  -t html-color -b ./html -o $(HTML) stklos.skb
	cp -a $(CSS) html
	cp -a $(IMG) html/images
	(cd html; ln -s $(HTMLMAIN) index.html)

$(DOCDB):
	(cd ..; make)

clean:
	rm -rf ./stklos-ref.* *~ *.mp html/* $(PDF)

install: Html Pdf
	rm -rf ../html/*.html ../html/images/*; 
	cp -a html ../html
	cp -a $(PDF) ../pdf
