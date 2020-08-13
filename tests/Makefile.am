# Makefile.am for testing STklos
#
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 23-May-2005 14:52 (eg)
# Last file update: 13-Aug-2020 17:28 (eg)

all:
	@echo "Use \"make test\" for testing STklos."

check: test

test:
	@../src/stklos --no-init-file --utf8-encoding=yes -f do-test.stk

clean:
	rm -f TEST.LOG SRFI-64_Meta-Test_Suite.log data *~

distclean: clean
	rm -f Makefile

install:
