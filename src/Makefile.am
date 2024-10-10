# Makefile.am for STklos source file
#
# Copyright © 2000-2024 Erick Gallesio <eg@stklos.net>
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
#    Creation date: 11-Apr-2000 10:30 (eg)

# Use AM_CFLAGS instead of CFLAGS since automake issue a warning when
# CFLAGS is used here (anyway CFLAGS is implied here)
CC	   	 = @CC@
AM_CFLAGS	 = @STKCFLAGS@

STKLOS_BINARY ?= ./stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive


extraincdir     = @INCLUDEDIR@/stklos
extrainc_DATA   = stklos.h extraconf.h stklosconf.h socket.h fport.h

bin_PROGRAMS    = stklos


DOCDB	        = DOCDB
BASEDIR         = ..

THREADS     = @THREADS@

# what thread support do we put in?
if PTHREADS
  THREAD_FILES = thread-common.c thread-pthreads.c mutex-common.c mutex-pthreads.c
endif
if NO_THREAD
  THREAD_FILES = thread-none.c mutex-none.c
endif

stklos_SOURCES = base64.c boolean.c boot.c box.c char.c \
 cond.c  cpointer.c dynload.c env.c error.c extend.c ffi.c fixnum.c \
 fport.c gnu-getopt.c hash.c keyword.c lib.c \
 list.c misc.c md5.c number.c object.c parameter.c \
 path.c port.c print.c proc.c process.c promise.c read.c regexp.c  \
 signal.c sio.c socket.c sport.c stklos.c str.c struct.c symbol.c \
 syntax.c system.c utf8.c uvector.c vector.c vm.c vport.c $(THREAD_FILES)

# HACK:  Normally BUILT_SOURCES contains the files that must be generated before
# starting the construction of the main target. We use it with a fictive target
# which runs a script which may update the git-info.h file. If it is updated, 
# normal rules apply and a new global link is done.
.PHONY: generate-git-info
BUILT_SOURCES = generate-git-info

### # The STklos library
### lib_LTLIBRARIES      = libstklos.la
### libstklos_la_SOURCES = boolean.c char.c dynload.c env.c error.c \
### extend.c fport.c hash.c keyword.c lib.c list.c misc.c number.c \
### object.c path.c port.c print.c proc.c process.c promise.c read.c \
### regexp.c signal.c sio.c sport.c  str.c symbol.c system.c \
### uvector.c vector.c  vm.c
###
###
### stklos_SOURCES = stklos.c


# GC LIBRARY
gc              = @GC@
gclib           = @GCLIB@
gcinc           = @GCINC@

# GMP LIBRARY
gmp	        = @GMP@
gmplib          = @GMPLIB@
gmpinc	        = @GMPINC@

# PCRE2 LIBRARY
pcre2		= @PCRE2@
pcre2lib	= @PCRE2LIB@
pcre2inc	= @PCRE2INC@

# FFI LIBRARY
ffi		= @FFI@
ffilib		= @FFILIB@
ffiinc		= @FFIINC@

stklos_LDADD   = $(gmplib) $(pcre2lib) $(ffilib) $(gclib) -lm
stklos_LDFLAGS = @SH_MAIN_LOAD_FLAGS@
AM_CPPFLAGS    = $(gmpinc) $(pcre2inc) $(ffiinc) $(gcinc)


struct.o cond.o: struct.h
chars.c: utf8-tables.in

generate-git-info:
	@../utils/generate-git-info


install-exec-hook:
	mv $(bindir)/stklos $(bindir)/stklos-@VERSION@
	ln -s stklos-@VERSION@ $(bindir)/stklos


doc:  $(DOCDB)

$(DOCDB): $(stklos_SOURCES)
	$(STKLOS_BINARY) $(STKLOS_FLAGS) -b boot.img -f ../doc/extract-doc $(stklos_SOURCES) > $(DOCDB)

clean-am:
	/bin/rm -f *.o $(DOCDB) git-info.h boot.ok stklos

distclean: clean
	/bin/rm -f boot.img Makefile extraconf.h stklos stamp-h1 stklosconf.h


uninstall-hook:
	rm -f $(DESTDIR)$(bindir)/stklos-@VERSION@
	rmdir $(extraincdir) || true




# Local Variables:
# tab-width: 8
# End:
