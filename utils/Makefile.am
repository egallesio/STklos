# Makefile.am for STklos/utils source file
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 14-Aug-2000 13:13 (eg)
# Last file update: 10-Aug-2010 10:42 (eg)

scmbin      = stklos-compile stklos-genlex
bin_SCRIPTS = stklos-config  stklos-script $(scmbin)


stklos-compile: stklos-compile.stk
	./tmpcomp stklos-compile.stk stklos-compile

stklos-genlex: stklos-genlex.stk
	./tmpcomp stklos-genlex.stk stklos-genlex

clean:
	rm -f $(scmbin)
