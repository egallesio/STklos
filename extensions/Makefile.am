#
#  Makefile 	-- Makefile for the STklos extensions
#
# Copyright © 2021-2022 Erick Gallesio - Polytech Nice-Sophia <eg@unice.fr>
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date:  6-Jul-2021 11:53
# Last file update:  2-Feb-2022 16:13 (eg)
#


subdirs=gtklos stklos-fuse

all:
	@echo "***"
	@echo "*** The extensions must be built in their own directory"
	@echo "***"

clean-am:
	for d in $(subdirs) ;do (cd $$d; make $@) ;done
	rm -f *~

distclean-am:
	for d in $(subdirs) ;do (cd $$d; make $@) ;done
	rm -f Makefile
