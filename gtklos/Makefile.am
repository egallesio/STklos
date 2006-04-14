# Makefile.am for STklos GTKlos directory
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 18-Jul-2000 10:30 (eg)
# Last file update: 13-Sep-2005 22:45 (eg)


CC	  = @CC@
CFLAGS	  = @CFLAGS@

STKCONF	  = ../utils/stklos-config
INCLUDES  = -I../src $(COMPFLAGS) @GCINC@
COMPILE   = @CC@ @CFLAGS@ @SH_COMP_FLAGS@ @GTK_CONFIG_CFLAGS@ $(INCLUDES)

shobjectdir=$(prefix)/lib/@PACKAGE@/@VERSION@

shobject_SCRIPTS    = gtklos.@SH_SUFFIX@
noinst_LIBRARIES    = libgtklos.a
libgtklos_a_SOURCES = gtk-glue.c gtk-canvas.c gtk-cont.c gtk-editable.c \
		      gtk-image.c gtk-label.c gtk-misc.c gtk-list.c \
		      gtk-signal.c gtk-event.c gtk-menu.c


# Scheme part
schemedir    = $(prefix)/share/@PACKAGE@/@VERSION@
scheme_files = gtklos.stk gtk-basic.stk \
	       \
	       box.stk button.stk canvas.stk canvitem.stk checkbutton.stk \
	       colorselector.stk combobox.stk container.stk \
	       dialog.stk entry.stk event.stk \
	       fileselector.stk fontselector.stk frame.stk gauge.stk \
	       handle.stk image.stk label.stk listbox.stk \
	       grid.stk layout.stk menu.stk object.stk \
	       radiobutton.stk scale.stk scroll.stk separator.stk \
	       text.stk toolbar.stk widget.stk window.stk

scheme_DATA = $(scheme_files) gtklos.ostk

# gtklos.@SH_SUFFIX@: libgtklos.a 
# 	@SH_LOADER@ @SH_LOAD_FLAGS@ gtklos.@SH_SUFFIX@ $(libgtklos_a_OBJECTS) \
# 	@GTK_CONFIG_LIBS@
# 	chmod 0755 gtklos.@SH_SUFFIX@

gtklos.@SH_SUFFIX@: libgtklos.a 
	@CC@ @SH_LOAD_FLAGS@ gtklos.@SH_SUFFIX@ $(libgtklos_a_OBJECTS) \
	@GTK_CONFIG_LIBS@
	chmod 0755 gtklos.@SH_SUFFIX@

gtklos.ostk: $(scheme_files)
	../utils/tmpcomp gtklos.stk gtklos.ostk


clean:
	/bin/rm -f *.o *.ostk *.@SH_SUFFIX@ *.a *~ 

distclean: clean 
