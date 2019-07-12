# Makefile.am for STklos
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)
# Last file update: 12-Jul-2019 17:52 (eg)


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

push: pre-push
	-git commit
	git push

pre-push:
	./utils/make-push-id
	git add src/extraconf.h.in
	find `git ls-files`             \
	     -printf 'chmod %#m "%p"; ' \
	     -printf 'touch -c -d "%TY-%Tm-%Td %TT" "%p"\n' | \
	  sed -E -e "s/[0-9]{2}:\.[0-9]+//" > $(GIT_META)
	git add $(GIT_META)


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
