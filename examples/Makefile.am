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
