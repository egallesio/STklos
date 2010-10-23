# Makefile.am for testing STklos
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 23-May-2005 14:52 (eg)
# Last file update: 23-Oct-2010 11:54 (eg)

all:
	@echo "Use \"make test\" for testing STklos."

check: test

test:
	@../src/stklos -f do-test.stk

clean:
	rm -f TEST.LOG data *~

distclean: clean
	rm -f Makefile

install: 


