# Makefile.am for STklos
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)
# Last file update: 26-Sep-2009 16:55 (eg)

EXTRA_DIST   = 
# INSTDIRS contains the dirs which need to be installed
INSTDIRS     =  @GC@ src utils lib examples pkgman tests doc
SUBDIRS      =  @PCRE@ @GMP@ @FFI@ $(INSTDIRS)
VERSION_TAG  = @PACKAGE@-@VERSION@
VERSION_BETA = $(VERSION_TAG)-beta

install: 
	@for i in $(INSTDIRS) ;do \
	  echo "***** Installing directory $$i"; \
	  (cd $$i && $(MAKE) $(AM_MAKEFLAGS) install); \
	done

test:
	(cd tests; $(MAKE) test)

commit-beta:
	eval "expr `cat .beta-number` + 1" > .beta-number
	hg commit -m "Preparing version $(VERSION_BETA)`cat .beta-number`"

commit-version:
	echo 0 > .beta-number	
	hg commit -m "Commit version $(VERSION_TAG)"

prep-version:
	rm -rf /usr/tmp/$(VERSION_TAG) /usr/tmp/$(VERSION_BETA)* /usr/tmp/beta
	hg clone . /usr/tmp/$(VERSION_TAG)
	rm -rf /usr/tmp/$(VERSION_TAG)/.hg
	hgclonedates `pwd` /usr/tmp/$(VERSION_TAG)

beta: commit-beta prep-version
	(mv /usr/tmp/$(VERSION_TAG) /usr/tmp/$(VERSION_BETA)`cat .beta-number`;  \
	 echo "$(VERSION_BETA)`cat .beta-number`" > /usr/tmp/beta ; \
	 cd /usr/tmp; tar cvfz `cat beta`.tar.gz `cat beta` )
	ls -ls /usr/tmp/$(VERSION_BETA)*.tar.gz

version: commit-version prep-version
	hg tag $(VERSION_TAG)
	( cd /usr/tmp; tar cvfz $(VERSION_TAG).tar.gz $(VERSION_TAG) )
	ls -ls /usr/tmp/$(VERSION_TAG).tar.gz
