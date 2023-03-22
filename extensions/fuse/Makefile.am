#
#  Makefile.in 	-- Makefile for GTklos Extension
#
# Copyright © 2021-2023 Erick Gallesio <eg@stklos.net>
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 18-Jun-2021 08:18
# Last file update: 22-Mar-2023 15:03 (eg)
#

SUBDIRS=lib/stklos demos

clean-am:
	rm -f *~

distclean-am: clean-am
	(cd demos; make distclean)
	rm -f Makefile
