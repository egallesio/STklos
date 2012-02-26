# Makefile.am for testing STklos
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 23-May-2005 14:52 (eg)
# Last file update: 26-Feb-2012 19:04 (eg)

all:
	@echo "Use \"make test\" for testing STklos."

check: test

test:
	@../src/stklos --utf8-encoding=yes -f do-test.stk

clean:
	rm -f TEST.LOG data *~

distclean: clean
	rm -f Makefile

install:
