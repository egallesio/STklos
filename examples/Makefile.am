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
#    Creation date: ??? 

schemedemodir       = $(prefix)/share/@PACKAGE@/@VERSION@/Demos.d
schemedemo_DATA	    = fork-test.stk hello.stk secho.stk \
		      socket-server.stk socket-client.stk \
		      socket-server-fork.stk socket-server-thread.stk \
		      threads.stk

schemedemo_SCRIPTS  = fork-test hello secho \
		      socket-server socket-client \
		      socket-server-fork socket-server-thread \
		      threads

SUFFIXES            = .stk

COMP ?= ../utils/tmpcomp

.stk:
	$(COMP) -o $* $*.stk 

# Override the install target to avoid intallation in destination dir.
install:

clean:
	   /bin/rm -f $(schemedemo_SCRIPTS) *~

distclean: clean
	/bin/rm -f Makefile
	/bin/rm -rf .deps
