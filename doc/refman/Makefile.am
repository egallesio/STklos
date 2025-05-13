# Makefile for producing STklos documentation (not using autoconf stuff)
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date:  1-Sep-2004 16:19 (eg)


ADOC=@ASCIIDOCTOR@
STKLOS_BINARY ?= ../../src/stklos

SOURCES  = biblio.adoc cond.adoc custom.adoc expr.adoc ffi.adoc          \
           idiosync.adoc intro.adoc licence.adoc match.adoc object.adoc  \
           progstruct.adoc regexp.adoc slib.adoc srfi.adoc stdproc.adoc  \
           stklos.adoc threads.adoc
DOCDB    = ../DOCDB
SPP      = $(STKLOS_BINARY) -c -q -I ../../lib -f ../../utils/stklos-pp.stk --

HTMLMAIN  = stklos-ref.html
HTMLMULTI = stklos-ref-multi.html
MULTI_AUX = FDL.html                                           \
            doc_bibliography.html                              \
            doc_customizations.html                            \
            doc_exceptions_and_conditions.html                 \
            doc_expressions.html                               \
            doc_foreign_function_interface.html                \
            doc_idiosync.html                                  \
            doc_introduction.html                              \
            doc_pattern_matching.html                          \
            doc_preface.html                                   \
            doc_program_structure.html                         \
            doc_regular_expressions.html                       \
            doc_srfis.html                                     \
            doc_standard_procedures.html                       \
            doc_stklos_object_system.html                      \
            doc_threads_mutexes_and_condition_variables.html   \
            doc_using_the_slib_package.html

PDF       = stklos-ref.pdf
IMG	      = images/hierarchy.png

.PHONY: single multi pdf

all-recursive: all

all: single

# ----------------------------------------------------------------------
# Various entries. Only the single HTML page is done automatically.
# Multiple HTML files and PDF file generations must be called by hand
# since they demand special asciidoctor modules

single : ../HTML/$(HTMLMAIN)
multi:   ../HTML/$(HTMLMULTI)
pdf:     ../PDF/$(PDF)
extradoc: multi pdf

# ----------------------------------------------------------------------
# Don't use pipes here, because if we have a Scheme error in the original
# document, it  will not be caught by make (set -o pipefile doesn't work
# everywhere)


# ----------------------------------------------------------------------
../HTML/$(HTMLMAIN): tmp-html.txt
	mkdir -p ../HTML/images
	cp -a $(IMG) ../HTML/images
	$(ADOC) -v -o $@ tmp-html.txt && rm -f tmp-html.txt

# ----------------------------------------------------------------------
# For the Multiple file version of the manual, we need to install
# https://github.com/owenh000/asciidoctor-multipage
# The backreferences are always on the name of the source file.
# So during the build of this version of the manual we create a link on
# tmp-html.txt
../HTML/$(HTMLMULTI): tmp-html.txt
	mkdir -p ../HTML/images
	cp -a $(IMG) ../HTML/images
	ln -s tmp-html.txt stklos-ref-multi.txt 2>/dev/stderr || true
	$(ADOC) -v -r asciidoctor-multipage -b multipage_html5 \
			-o $@ stklos-ref-multi.txt && \
		rm -f stklos-ref-multi.txt

# ----------------------------------------------------------------------
../PDF/$(PDF): tmp-pdf.txt
	mkdir -p ../PDF
	$(ADOC) -v -r asciidoctor-pdf -b pdf -o tmp.pdf tmp-pdf.txt
	@echo "Reducing size of generated PDF file"
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress \
		-dNOPAUSE -dQUIET -dBATCH -sOutputFile=$@ tmp.pdf
	rm -f tmp.pdf


# ----------------------------------------------------------------------
tmp-html.txt: $(SOURCES) $(DOCDB)
	$(SPP) -D "doc-fmt: html" stklos.adoc > $@

tmp-pdf.txt: $(SOURCES) $(DOCDB)
	$(SPP) -D "doc-fmt: pdf" stklos.adoc > $@
# ----------------------------------------------------------------------
$(DOCDB):
	(cd .. && $(MAKE) docdb)

clean:
	rm -f  tmp-html.txt tmp-pdf.txt stklos-ref-multi.txt tmp.pdf *~

distclean: clean
	rm -f  ../HTML/$(HTMLMAIN) ../HTML/$(HTMLMULTI)
	for i in $(MULTI_AUX); do rm -f ../HTML/$$i ; done
	rm -f ../HTML/$(IMG)
	rm -f ../PDF/$(PDF)
	rm -f ./Makefile
