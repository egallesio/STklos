# Makefile.am for STklos/utils source file
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 14-Aug-2000 13:13 (eg)
# Last file update: 10-Feb-2020 16:32 (eg)

scmbin      = stklos-compile stklos-genlex
bin_SCRIPTS = stklos-config  stklos-script $(scmbin)

COMP ?= ./tmpcomp

stklos-compile: stklos-compile.stk
	$(COMP) -o stklos-compile stklos-compile.stk 

stklos-genlex: stklos-genlex.stk
	$(COMP) -o stklos-genlex stklos-genlex.stk 

clean:
	rm -f $(scmbin)
