# Makefile.am for STklos
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)
# Last file update:  1-Sep-2020 09:18 (eg)


EXTRA_DIST   =
# INSTDIRS contains the dirs which need to be installed
INSTDIRS     =  @GC@ src utils lib examples pkgman
SUBDIRS      =  @PCRE@ @GMP@ @FFI@ $(INSTDIRS)

.PHONY:  test tests

install: install-base install-sources install-doc
	@echo "Full install completed"

install-base:
	@for i in $(INSTDIRS) ;do \
	  echo "***** Installing directory $$i"; \
	  (cd $$i && $(MAKE) $(AM_MAKEFLAGS) install); \
	done

install-sources: install-base
	(cd lib && $(MAKE)  $(AM_MAKEFLAGS) install-sources)

install-doc: install-base
	(cd doc && $(MAKE) $(AM_MAKEFLAGS) install)


test tests: all
	(cd tests; $(MAKE) test)
