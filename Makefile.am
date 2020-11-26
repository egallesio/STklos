# Makefile.am for STklos
#
# Copyright © 1999-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)
# Last file update: 26-Nov-2020 11:16 (eg)


EXTRA_DIST   =
# INSTDIRS contains the dirs which need to be installed
INSTDIRS     =  @GC@ src utils lib examples pkgman
SUBDIRS      =  @PCRE@ @GMP@ @FFI@ $(INSTDIRS) doc tests

.PHONY:  test tests

install: install-base install-sources install-doc
	@echo "Full install completed"

install-base:
	@for i in $(INSTDIRS) ;do \
	  echo "***** Installing directory $$i"; \
	  (cd $$i && $(MAKE) $(AM_MAKEFLAGS) install); \
	done
install-base-no-strip:
	@env $(MAKE) install-base STRIP=no

install-sources: install-base
	(cd lib && $(MAKE)  $(AM_MAKEFLAGS) install-sources)

install-doc: install-base
	(cd doc && $(MAKE) $(AM_MAKEFLAGS) install)


# Update the list of supported SRFIs
# This target is used by lib/Makefile. It needs a running STklos
# Dont call it directly
SUPPORTED-SRFIS: lib/srfis.stk
	@echo "Updating list of supported SRFIs ... "
	sed -e '/- SRFI/d' $@ > $@.tmp                                          && \
	./src/stklos -l ./lib/srfis.stk -f utils/update-srfi-list.stk >> $@.tmp && \
	mv -f  $@.tmp $@

clean-am:
	for i in doc tests utils;do (cd $$i; $(MAKE) clean) ;done
	rm -f SUPPORTED-SRFIS.tmp

distclean: clean
	for i in $(SUBDIRS) ;do (cd $$i; $(MAKE) distclean); done
	rm -rf Makefile config.log config.status autom4te.cache

test tests: all
	(cd tests; $(MAKE) test)

# Local Variables:
# tab-width: 4
# End:
