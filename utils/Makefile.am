# Makefile.am for STklos/utils source file
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 14-Aug-2000 13:13 (eg)
# Last file update: 10-Aug-2010 10:42 (eg)

scmbin      = stklos-compile stklos-genlex
bin_SCRIPTS = stklos-config  stklos-script $(scmbin)

COMP ?= ./tmpcomp

stklos-compile: stklos-compile.stk
	$(COMP) stklos-compile.stk -o stklos-compile

stklos-genlex: stklos-genlex.stk
	$(COMP) stklos-genlex.stk -o stklos-genlex

clean:
	rm -f $(scmbin)
