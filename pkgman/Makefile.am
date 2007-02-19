# Makefile for stklos-pkgman
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 22-Dec-2006 12:00 (eg)
# Last file update: 16-Feb-2007 23:34 (eg)


makefiledir=  $(prefix)/etc/stklos
makefile_DATA= etc/Makefile-stklospkg.tmpl

bin_SCRIPTS = stklos-pkg

SRC     = main.stk add.stk extract.stk http.stk params.stk pkgball.stk \
	  repository.stk misc.stk tune.stk types.stk
SFLAGS  = -l 
RM      = /bin/rm
SCC	= ../utils/stklos-compile

$(bin_SCRIPTS): $(SRC)
	$(SCC) $(SFLAGS) -o $(bin_SCRIPTS) main.stk

clean:
	$(RM) -f $(bin_SCRIPTS) *~

distclean: clean

