# Makefile for producing STklos documentation (not using autoconf stuff)
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date:  1-Sep-2004 16:19 (eg)
# Last file update: 16-Jan-2022 18:45 (eg)


SOURCES  = biblio.adoc cond.adoc custom.adoc expr.adoc ffi.adoc \
           intro.adoc match.adoc object.adoc progstruct.adoc    \
           regexp.adoc scmpkg.adoc slib.adoc srfi.adoc          \
           stdproc.adoc stklos.adoc threads.adoc
DOCDB    = ../DOCDB
SPP      = ../../utils/stklos-pp

HTMLMAIN  = stklos-ref.html
HTMLMULTI = stklos-ref-multi.html
PDF       = stklos-ref.pdf
IMG	      = images/hierarchy.jpg

all-recursive: all

all:  html/$(HTMLMAIN) html/$(HTMLMULTI) $(PDF)

# ----------------------------------------------------------------------
html/$(HTMLMAIN): $(SOURCES) $(DOCDB) 
	mkdir -p html/images
	$(SPP) -D "doc-fmt: 'html" stklos.adoc | \
          asciidoctor -o html/$(HTMLMAIN) -
	(cd html; ln -s $(HTMLMAIN) index.html 2>/dev/null || true)
	cp -a $(IMG) html/images

# ----------------------------------------------------------------------
# For the Multiple file version of the manual, we need to install
# https://github.com/owenh000/asciidoctor-multipage
#
# The STklos prepreocessor creates a file (instead of passing doc as a pipe)
# to resolve back links to the main page
html/$(HTMLMULTI): $(SOURCES) $(DOCDB)
	mkdir -p html/images
	$(SPP) -D "doc-fmt: 'html" stklos.adoc > stklos-ref-multi.adoc
	asciidoctor -r asciidoctor-multipage -b multipage_html5 \
		  -o html/$(HTMLMULTI) stklos-ref-multi.adoc
	rm -f stklos-ref-multi.adoc

# ----------------------------------------------------------------------
$(PDF):  $(SOURCES) $(DOCDB)
	$(SPP) -D "doc-fmt: 'pdf" stklos.adoc | \
          asciidoctor-pdf -o $(PDF) -

# ----------------------------------------------------------------------
$(DOCDB):
	(cd ..; $(MAKE) docdb)

clean:
	rm -rf  html/* $(PDF) *~

distclean: clean
	rm -rf html

copy: all
	rm -rf ../html/*.html ../html/images/*;
	cp -a html/* ../html
	cp -a $(PDF) ../pdf
