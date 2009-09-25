schemedemodir       = $(prefix)/share/@PACKAGE@/@VERSION@/Demos.d
schemedemo_DATA	    = fork-test.stk hello.stk secho.stk \
<<<<<<< local
		      socket-server.stk socket-client.stk
=======
		      socket-server.stk socket-client.stk \
		      threads.stk
>>>>>>> other

schemedemo_SCRIPTS  = fork-test hello secho \
<<<<<<< local
		      socket-server socket-client
=======
		      socket-server socket-client \
		      threads
>>>>>>> other

SUFFIXES            = .stk

.stk:
	../utils/tmpcomp $*.stk $*

clean:
	   /bin/rm -f $(schemedemo_SCRIPTS) *~ 

distclean: clean
	/bin/rm -f Makefile 
	/bin/rm -rf .deps
