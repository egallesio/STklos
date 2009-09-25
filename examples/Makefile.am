schemedemodir       = $(prefix)/share/@PACKAGE@/@VERSION@/Demos.d
schemedemo_DATA	    = fork-test.stk hello.stk secho.stk \
		      socket-server.stk socket-client.stk

schemedemo_SCRIPTS  = fork-test hello secho \
		      socket-server socket-client

SUFFIXES            = .stk

.stk:
	../utils/tmpcomp $*.stk $*

clean:
	   /bin/rm -f $(schemedemo_SCRIPTS) *~ 

distclean: clean
	/bin/rm -f Makefile 
	/bin/rm -rf .deps
