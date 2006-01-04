# Makefile.am for STklos
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)
# Last file update:  4-Jan-2006 14:42 (eg)

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
prep-version:
	hg commit -m "preparing version $(VERSION_TAG)"
	rm -rf /usr/tmp/$(VERSION_TAG) /usr/tmp/$(VERSION_BETA)
	hg clone . /usr/tmp/$(VERSION_TAG)
	rm -rf /usr/tmp/$(VERSION_TAG)/.hg
	hgclonedates `pwd` /usr/tmp/$(VERSION_TAG)

beta: prep-version
	( cd /usr/tmp; mv $(VERSION_TAG) $(VERSION_BETA) \
	  tar cvfz $(VERSION_BETA).tar.gz $(VERSION_BETA) )
	ls -ls /usr/tmp/$(VERSION_BETA).tar.gz

version: beta
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
