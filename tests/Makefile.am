# Makefile.am for testing STklos
#
# Copyright © 2005-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 23-May-2005 14:52 (eg)

all:
	@echo "Use \"make test\" for testing STklos."

check: test

test tests:
	@../src/stklos --no-init-file --utf8-encoding=yes -f do-test.stk

clean-am:
	rm -f TEST.LOG SRFI-64_Meta-Test_Suite.log data *~

distclean: clean
	rm -f Makefile

install:
