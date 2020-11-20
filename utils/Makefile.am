# Makefile.am for STklos/utils source file
#
# Copyright © 2000-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
# Last file update: 20-Nov-2020 17:05 (eg)

scmbin      = stklos-compile stklos-genlex
bin_SCRIPTS = stklos-config  stklos-script $(scmbin)

# HACK: We define localscript as a dependence of the scripts
# stklos-compile and stklos-genlex to ensure that it is compiled
# Since it is not in bin_SCRIPTS, it will not be installed
localscript = update-srfi-list

COMP ?= ./tmpcomp

stklos-compile: $(localscript) stklos-compile.stk
	$(COMP) -o stklos-compile stklos-compile.stk

stklos-genlex: $(localscript) stklos-genlex.stk
	$(COMP) -o stklos-genlex stklos-genlex.stk

update-srfi-list: update-srfi-list.stk
	$(COMP) -o update-srfi-list update-srfi-list.stk

clean:
	rm -f $(scmbin) $(localscript) *~
