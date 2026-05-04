# Makefile.am for STklos/gmp
#
# Copyright © 2009-2026 Erick Gallesio <eg@stklos.net>
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
#    Creation date: ???? 2009


noinst_LIBRARIES = libgmp.a
libgmp_a_SOURCES =  mini-gmp.c

all-local: libgmp.a
	@mkdir -p .libs
	cp libgmp.a ./.libs

check:

test tests: check

clean:
	rm -f *.o *~ libgmp.a .libs/libgmp.a

distclean: clean
	rm -f config.h stamp-h1
	rm -f config.status config.cache config.log configure.lineno \
	      config.status.lineno
	rm -rf ./.deps ./libs ./autom4te.cache
	rm -f Makefile
