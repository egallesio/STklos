# Makefile.am for STklos
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)
# Last file update:  9-Aug-2020 17:00 (eg)


EXTRA_DIST   =
# INSTDIRS contains the dirs which need to be installed
INSTDIRS     =  @GC@ src utils lib examples pkgman tests doc
SUBDIRS      =  @PCRE@ @GMP@ @FFI@ $(INSTDIRS)
VERSION_TAG  = @PACKAGE@-@VERSION@
VERSION_BETA = $(VERSION_TAG)-beta
GIT_META     = ./.git_meta

.PHONY:  test tests push

install:
	@for i in $(INSTDIRS) ;do \
	  echo "***** Installing directory $$i"; \
	  (cd $$i && $(MAKE) $(AM_MAKEFLAGS) install); \
	done

test tests: all
	(cd tests; $(MAKE) test)
