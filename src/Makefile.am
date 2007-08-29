# Makefile.am for STklos source file
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)
# Last file update: 28-Aug-2007 15:00 (eg)

CC	    = @CC@
CFLAGS	    = @CFLAGS@ @STKCFLAGS@

schemedir   = $(prefix)/share/@PACKAGE@/@VERSION@
extraincdir = $(prefix)/include/@PACKAGE@


bin_PROGRAMS    = stklos
#scheme_DATA     = boot.img
extrainc_DATA   = stklos.h extraconf.h stklosconf.h socket.h fport.h

DOCDB	        = DOCDB

THREADS     = @THREADS@

# what thread support do we put in?
if LURC
  THREAD_FILES = thread-common.c thread-lurc.c mutex-common.c mutex-lurc.c
endif
if PTHREADS
  THREAD_FILES = thread-common.c thread-pthreads.c mutex-common.c mutex-pthreads.c
endif
if NO_THREAD
  THREAD_FILES = thread-none.c mutex-none.c
endif

stklos_SOURCES = base64.c boolean.c boot.c char.c cond.c cpointer.c dynload.c \
 env.c error.c extend.c ffi.c fixnum.c fport.c gnu-getopt.c gnu-glob.c \
 hash.c keyword.c lib.c list.c misc.c md5.c number.c object.c parameter.c \
 path.c port.c print.c proc.c process.c promise.c read.c regexp.c  \
 signal.c sio.c socket.c sport.c stklos.c str.c struct.c \
 symbol.c system.c uvector.c vector.c vm.c vport.c $(THREAD_FILES)


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

# gtklib		= @GTK_CONFIG_LIBS@

# COMPAT LIB 
#compatlib       = -L$(top_srcdir)/compat -lcompat

# GC LIBRARY
gc              = @GC@
gclib           = @GCLIB@
gcinc           = @GCINC@

# GMP LIBRARY 
gmp	        = @GMP@
gmplib          = @GMPLIB@
gmpinc	        = @GMPINC@

# PCRE LIBRARY
pcre		= @PCRE@
pcrelib		= @PCRELIB@
pcreinc		= @PCREINC@

stklos_LDADD   = $(compatlib) $(gmplib) $(pcrelib) $(gclib) -lm 
stklos_LDFLAGS = @SH_MAIN_LOAD_FLAGS@
INCLUDES       = $(gmpinc) $(pcreinc) $(gcinc)


struct.o cond.o: struct.h

doc:  $(DOCDB)

$(DOCDB): $(stklos_SOURCES)	
	./stklos -b boot.img -c -q -f ../doc/extract-doc $(stklos_SOURCES) > $(DOCDB)

clean:
	/bin/rm -f *.o $(DOCDB)

