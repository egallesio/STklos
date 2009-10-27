noinst_LIBRARIES = libgmp.a
libgmp_a_SOURCES =  mpi.c gmp.c

all: mpi-types.h libgmp.a
	@mkdir -p .libs
	cp libgmp.a ./.libs

mpi-types.h: types
	types > ./mpi-types.h

types: types.c
	$(CC) $(CFLAGS) -o types types.c

test: all
	(cd tests; $(MAKE) test)

check: test

clean: 
	/bin/rm -f *.o *~ libgmp.a .libs/libgmp.a
	(cd tests; make clean)

distclean: clean
	(cd tests; make distclean)
	/bin/rm -f types mpi-types.h
	/bin/rm -f config.h stamp-h1
	/bin/rm -f config.status config.cache config.log configure.lineno \
		   config.status.lineno
	/bin/rm -rf ./.deps
	/bin/rm -f Makefile
