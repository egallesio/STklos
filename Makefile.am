# Makefile.am for STklos
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)
# Last file update: 17-Jun-2019 16:03 (eg)

EXTRA_DIST   =
# INSTDIRS contains the dirs which need to be installed
INSTDIRS     =  @GC@ src utils lib examples pkgman tests doc
SUBDIRS      =  @PCRE@ @GMP@ @FFI@ $(INSTDIRS)
VERSION_TAG  = @PACKAGE@-@VERSION@
VERSION_BETA = $(VERSION_TAG)-beta

.PHONY:  test push 

install:
	@for i in $(INSTDIRS) ;do \
	  echo "***** Installing directory $$i"; \
	  (cd $$i && $(MAKE) $(AM_MAKEFLAGS) install); \
	done

test tests: all
	(cd tests; $(MAKE) test)

push: _git-pre-push
	git push

_git-pre-push:
	./utils/make-push-id
	git add src/extraconf.h.in
	git commit --amend


#commit-beta:
#	eval "expr `cat .beta-number` + 1" > .beta-number
#	hg commit -m "Preparing version $(VERSION_BETA)`cat .beta-number`"
#
#commit-version:
#	echo 0 > .beta-number
#	hg commit -m "Commit version $(VERSION_TAG)"
#
#prep-version:
#	rm -rf /tmp/$(VERSION_TAG) /tmp/$(VERSION_BETA)* /tmp/beta
#	hg clone . /tmp/$(VERSION_TAG)
#	rm -rf /tmp/$(VERSION_TAG)/.hg*
#	hgclonedates `pwd` /tmp/$(VERSION_TAG)
#
#beta: commit-beta prep-version
#	(mv /tmp/$(VERSION_TAG) /tmp/$(VERSION_BETA)`cat .beta-number`;  \
#	 echo "$(VERSION_BETA)`cat .beta-number`" > /tmp/beta ; \
#	 cd /tmp; tar cvfz `cat beta`.tar.gz `cat beta` )
#	ls -ls /tmp/$(VERSION_BETA)*.tar.gz
#
#version: commit-version prep-version
#	hg tag $(VERSION_TAG)
#	( cd /tmp; tar cvfz $(VERSION_TAG).tar.gz $(VERSION_TAG) )
#	ls -ls /tmp/$(VERSION_TAG).tar.gz
