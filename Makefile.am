# Makefile.am for STklos
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)
# Last file update: 28-Oct-2006 18:12 (eg)

EXTRA_DIST   = 
SUBDIRS      =  @PCRE@ @GC@ @GMP@ src utils lib @GTKLOS@ @EXAMPLES@ \
	       extensions tests doc
SVN_URL      = @SVN_URL@/STklos
VERSION_TAG  = @PACKAGE@-@VERSION@
VERSION_BETA = $(VERSION_TAG)-beta

test:
	(cd tests; $(MAKE) test)

##prep-version: 
##	svn commit 
##	svn update
##	rm -rf /usr/tmp/$(VERSION_BETA)
##	svn export $(SVN_URL)/trunk /usr/tmp/$(VERSION_BETA)
##	svnclonedates `pwd` /usr/tmp/$(VERSION_BETA)
##
##beta: prep-version
##	( cd /usr/tmp; tar cvfz $(VERSION_BETA).tar.gz $(VERSION_BETA) )
##	ls -ls /usr/tmp/$(VERSION_BETA).tar.gz
##
##version: prep-version
##	-svn rm  $(SVN_URL)/tags/$(VERSION_TAG) -m ''
##	svn copy . $(SVN_URL)/tags/$(VERSION_TAG) -m ''
##	rm -rf /usr/tmp/$(VERSION_TAG)
##	cp -a /usr/tmp/$(VERSION_BETA) /usr/tmp/$(VERSION_TAG)
##	( cd /usr/tmp; tar cvfz $(VERSION_TAG).tar.gz $(VERSION_TAG) )
##	ls -ls /usr/tmp/$(VERSION_TAG).tar.gz
##

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


# oldversion: 
# 	svn commit 
# 	svn update
# 	-svn rm  $(SVN_URL)/tags/$(VERSION_TAG) -m ''
# 	svn copy . $(SVN_URL)/tags/$(VERSION_TAG) -m ''
# 	rm -rf /usr/tmp/$(VERSION_TAG)
# 	svn export $(SVN_URL)/tags/$(VERSION_TAG) /usr/tmp/$(VERSION_TAG)
# 	clonedates `pwd` /usr/tmp/$(VERSION_TAG)
# 	( cd /usr/tmp; tar cvfz $(VERSION_TAG).tar.gz $(VERSION_TAG) )
# 	ls -ls /usr/tmp/$(VERSION_TAG).tar.gz
