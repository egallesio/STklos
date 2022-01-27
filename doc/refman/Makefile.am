# Makefile for producing STklos documentation (not using autoconf stuff)
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date:  1-Sep-2004 16:19 (eg)
# Last file update: 27-Jan-2022 10:38 (eg)


ADOC=@ASCIIDOCTOR@

SOURCES  = biblio.adoc cond.adoc custom.adoc expr.adoc ffi.adoc \
           intro.adoc licence.adoc match.adoc object.adoc       \
           progstruct.adoc regexp.adoc scmpkg.adoc slib.adoc    \
           srfi.adoc stdproc.adoc stklos.adoc threads.adoc
DOCDB    = ../DOCDB
SPP      = ../../src/stklos -q -I ../../lib -f ../../utils/stklos-pp.stk --

HTMLMAIN  = stklos-ref.html
HTMLMULTI = stklos-ref-multi.html
MULTI_AUX = FDL.html                                        \
            _bibliography.html                              \
            _customizations.html                            \
            _exceptions_and_conditions.html                 \
            _expressions.html                               \
            _foreign_function_interface.html                \
            _introduction.html                              \
            _pattern_matching.html                          \
            _preface.html                                   \
            _program_structure.html                         \
            _regular_expressions.html                       \
            _srfis.html                                     \
            _standard_procedures.html                       \
            _stklos_object_system.html                      \
            _the_scmpkg_package_system.html                 \
            _threads_mutexes_and_condition_variables.html   \
            _using_the_slib_package.html

PDF       = stklos-ref.pdf
IMG	      = images/hierarchy.png

.PHONY: single multi pdf

all-recursive: all

all: single

# ----------------------------------------------------------------------
# Various entries. Only the single HTML page is done automatically.
# Multiple HTML files and PDF file generations must be called by hand
# since they demand special asciidoctor modules

single : ../html/$(HTMLMAIN)
multi:   ../html/$(HTMLMULTI)
pdf:     ../pdf/$(PDF)


# ----------------------------------------------------------------------
../html/$(HTMLMAIN): $(SOURCES) $(DOCDB)
	mkdir -p ../html/images
	$(SPP) -D "doc-fmt: 'html" stklos.adoc | $(ADOC) -v -o $@ -
	cp -a $(IMG) ../html/images

# ----------------------------------------------------------------------
# For the Multiple file version of the manual, we need to install
# https://github.com/owenh000/asciidoctor-multipage
#
# The STklos prepreocessor creates a file (instead of passing doc as a pipe)
# to resolve back links to the main page
../html/$(HTMLMULTI): $(SOURCES) $(DOCDB)
	mkdir -p ../html/images
	$(SPP) -D "doc-fmt: 'html" stklos.adoc > stklos-ref-multi.adoc
	$(ADOC) -v -r asciidoctor-multipage -b multipage_html5 \
		  -o $@ stklos-ref-multi.adoc
	rm -f stklos-ref-multi.adoc

# ----------------------------------------------------------------------
../pdf/$(PDF):  $(SOURCES) $(DOCDB)
	$(SPP) -D "doc-fmt: 'pdf" stklos.adoc | \
           $(ADOC) -v -r asciidoctor-pdf -b pdf -o $@ -

# ----------------------------------------------------------------------
$(DOCDB):
	(cd ..; $(MAKE) docdb)

clean:
	rm -f  stklos-ref-multi.adoc *~

distclean: clean
	rm -f  ../html/$(HTMLMAIN) ../html/$(HTMLMULTI) 
	for i in $(MULTI_AUX); do rm -f ../html/$$i ; done
	rm -f ../html/$(IMG)
	rm -f ../pdf/$(PDF)
