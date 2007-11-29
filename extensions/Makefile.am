# Makefile.am for STklos extension directory
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date:  4-Jul-2001 11:36 (eg)
# Last file update: 29-Nov-2007 22:30 (eg)


STKCONF	  = ../utils/stklos-config
INCLUDES  = -I../src $(COMPFLAGS)
CC	  = @CC@
CFLAGS	  = @CFLAGS@ @SH_COMP_FLAGS@ @GTK_CONFIG_CFLAGS@ @GCINC@ $(INCLUDES)


shobjectdir	    = $(libdir)/@PACKAGE@/@VERSION@
shobject_SCRIPTS    = ldap.@SH_SUFFIX@

#======================================================================
# Scheme code 
#
scheme_SRCS = ldap.stk
scheme_OBJS = ldap.ostk

schemedir   = $(datadir)/@PACKAGE@/@VERSION@
scheme_DATA = $(scheme_SRCS) $(scheme_OBJS) 


##############################################################################
SUFFIXES  = .@SH_SUFFIX@ .o .c .stk .ostk .scm

.o.@SH_SUFFIX@: 
	@SH_LOADER@ @SH_LOAD_FLAGS@ $*.@SH_SUFFIX@ $<
	if test -f a.out  ;then mv a.out $*.@SH_SUFFIX@; fi

.stk.ostk:
	../utils/tmpcomp $*.stk $*.ostk

.scm.ostk:
	../utils/tmpcomp $*.scm $*.ostk


##############################################################################
clean:
	/bin/rm -f *.o *.ostk  *.@SH_SUFFIX@ *.a *~ 

##############################################################################
# Specific rules for ldap.so
ldap.@SH_SUFFIX@: ldap.o
	@SH_LOADER@ @SH_LOAD_FLAGS@ $*.@SH_SUFFIX@ $< @LDAP_LIB@
	if test -f a.out  ;then mv a.out $*.@SH_SUFFIX@; fi
