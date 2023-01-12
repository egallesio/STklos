# Makefile.am for STklos/utils source file
#
# Copyright © 2000-2023 Erick Gallesio <eg@stklos.net>
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
#    Creation date: 14-Aug-2000 13:13 (eg)
# Last file update: 12-Jan-2023 12:20 (eg)

scmbin      = stklos-compile stklos-genlex stklos-pp
bin_SCRIPTS = stklos-config  stklos-script $(scmbin)

COMP ?= ./tmpcomp

all-am:
	chmod a+x stklos-config stklos-script

stklos-compile: stklos-compile.stk
	$(COMP) -o stklos-compile stklos-compile.stk

stklos-genlex: stklos-genlex.stk
	$(COMP) -o stklos-genlex stklos-genlex.stk

stklos-pp: stklos-pp.stk ../lib/stklos/preproc.ostk
	$(COMP) -o stklos-pp stklos-pp.stk

../lib/stklos/preproc.ostk:
	(cd ../lib/stklos; $(MAKE)  preproc.ostk)

clean-am:
	rm -f $(scmbin) $(localscript) *~
