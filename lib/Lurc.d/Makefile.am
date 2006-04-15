# Makefile.am for STklos Lurc directory
#
#           Author: Stephane Epardaud [Stephane.Epardaud@inria.fr]
#    Creation date:  3-Apr-2006 10:30 (se)
# Last file update: 14-Apr-2006 19:22 (eg)


CC	  = @CC@
CFLAGS	  = @CFLAGS@

MGCINC     = $(subst ../,../../,@GCINC@)
STKCONF	  = ../../utils/stklos-config
INCLUDES  = -I../../src $(COMPFLAGS) $(MGCINC)
COMPILE   = @CC@ @CFLAGS@ @SH_COMP_FLAGS@ $(INCLUDES)

shobjectdir=$(prefix)/lib/@PACKAGE@/@VERSION@

shobject_SCRIPTS    = stklurc.@SH_SUFFIX@
noinst_LIBRARIES    = libstklurc.a
libstklurc_a_SOURCES = lurc.c


# Scheme part
schemedir    = $(prefix)/share/@PACKAGE@/@VERSION@
scheme_files = lurc.stk

scheme_DATA = $(scheme_files) lurc.ostk

stklurc.@SH_SUFFIX@: libstklurc.a 
	@CC@ @SH_LOAD_FLAGS@ stklurc.@SH_SUFFIX@ $(libstklurc_a_OBJECTS)
	chmod 0755 stklurc.@SH_SUFFIX@

lurc.ostk: $(scheme_files)
	../../utils/tmpcomp lurc.stk lurc.ostk

clean:
	/bin/rm -f *.o *.ostk *.@SH_SUFFIX@ *.a *~ 

distclean: clean 
