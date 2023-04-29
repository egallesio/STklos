# Makefile.am for STklos lib
#
# Copyright © 2001-2022 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
#    Creation date: 26-Nov-2001 10:10 (eg)

scheme_matchdir   = $(prefix)/share/@PACKAGE@/@VERSION@/Match.d
scheme_match_DATA = descr.scm compiler.scm  mexpand.scm normalize.scm s2cfun.scm

# SRC = descr.scm compiler.scm  mexpand.scm normalize.scm s2cfun.scm
#
# install-sources:
# 	mkdir -p  $(DESTDIR)/$(scheme_matchdir)
# 	cp $(SRC) $(DESTDIR)/$(scheme_matchdir)
#

uninstall-hook:
	rmdir $(scheme_matchdir) || true

doc:
