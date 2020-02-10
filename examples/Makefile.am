schemedemodir       = $(prefix)/share/@PACKAGE@/@VERSION@/Demos.d
schemedemo_DATA	    = fork-test.stk hello.stk secho.stk \
		      socket-server.stk socket-client.stk \
		      threads.stk

schemedemo_SCRIPTS  = fork-test hello secho \
		      socket-server socket-client \
		      threads

SUFFIXES            = .stk

COMP ?= ../utils/tmpcomp

.stk:
	$(COMP) -o $* $*.stk 

clean:
	   /bin/rm -f $(schemedemo_SCRIPTS) *~

distclean: clean
	/bin/rm -f Makefile
	/bin/rm -rf .deps
