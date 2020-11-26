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
# Last file update: 26-Nov-2020 10:50 (eg)

scmbin      = stklos-compile stklos-genlex
bin_SCRIPTS = stklos-config  stklos-script $(scmbin)

COMP ?= ./tmpcomp

stklos-compile: stklos-compile.stk
	$(COMP) -o stklos-compile stklos-compile.stk

stklos-genlex: stklos-genlex.stk
	$(COMP) -o stklos-genlex stklos-genlex.stk


clean-am:
	rm -f $(scmbin) $(localscript) *~
