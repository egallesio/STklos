# Makefile for stklos-snowman
#           Author: Erick Gallesio [eg@essi.fr]
#    Creation date: 22-Dec-2006 12:00 (eg)
# Last file update: 18-Jan-2007 16:41 (eg)


makefiledir=  $(prefix)/etc/stklos
makefile_DATA= etc/Makefile-snow.tmpl

bin_SCRIPTS = stklos-snowman 

SRC     = main.stk extract.stk params.stk repository.stk \
	  snowball.stk misc.stk types.stk
SFLAGS  = -l 
RM      = /bin/rm
SCC	= ../utils/stklos-compile

$(bin_SCRIPTS): $(SRC)
	$(SCC) $(SFLAGS) -o $(bin_SCRIPTS) main.stk

clean:
	$(RM) -f $(bin_SCRIPTS) *~

distclean: clean
	$(RM) -f config.stk

