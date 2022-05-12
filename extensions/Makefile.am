#
#  Makefile 	-- Makefile for the STklos extensions
#
# Copyright © 2021-2022 Erick Gallesio - Polytech Nice-Sophia <eg@unice.fr>
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date:  6-Jul-2021 11:53
# Last file update: 19-Feb-2022 18:10 (eg)
#


subdirs= curl fuse gtklos 

all:
	@echo "***"
	@echo "*** The extensions must be built in their own directory"
	@echo "***"

clean-am:
	for d in $(subdirs) ;do (cd $$d; $(MAKE) $@) ;done
	rm -f *~

distclean-am:
	for d in $(subdirs) ;do (cd $$d; $(MAKE) $@) ;done
	rm -f Makefile
