# Makefile for STklos GTklos extension
#
# Copyright © 2021-2023 Erick Gallesio <eg@stklos.net>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 12-May-2021 06:54 (eg)

EXTENSION=gtklos

SCM_CANVAS = canvas/canvas.stk canvas/canvitem.stk                       \
             canvas/canvellipse.stk canvas/canvimage.stk                 \
             canvas/canvline.stk canvas/canvpath.stk canvas/canvrect.stk \
             canvas/canvtext.stk

SCM_SRCS = lib/events.stk lib/libs.stk lib/utils.stk                     \
           classes/meta.stk classes/container.stk classes/widget.stk     \
           widgets/box.stk widgets/button.stk widgets/checkbutton.stk    \
           widgets/color-dialog.stk widgets/combobox.stk                 \
           widgets/dialog.stk widgets/entry.stk widgets/file-dialog.stk  \
           widgets/font-dialog.stk widgets/frame.stk                     \
           widgets/grid.stk widgets/header.stk widgets/image.stk         \
           widgets/label.stk          \
           widgets/menu.stk widgets/progress.stk widgets/radiobutton.stk \
           widgets/scale.stk widgets/scroll.stk widgets/separator.stk    \
           widgets/text.stk widgets/toolbar.stk widgets/window.stk       \
           $(SCM_CANVAS)

#======================================================================
OUT            = $(EXTENSION).@SH_SUFFIX@
schemedir      = $(prefix)/share/@PACKAGE@/@VERSION@/stklos
schemelibdir   = $(prefix)/lib/@PACKAGE@/@VERSION@/stklos

GTKFLAGS       = @GTK_CFLAGS@

#======================================================================
BASEDIR= ../../../..
COMP ?= $(BASEDIR)/utils/tmpcomp
STKLOS_BINARY ?= $(BASEDIR)/src/stklos
STKLOS_FLAGS   = --no-init-file --case-sensitive
# ======================================================================
SOURCES= $(EXTENSION).c $(EXTENSION).stk

SUFFIXES = .stk -incl.c .@SH_SUFFIX@ .c

.stk-incl.c:
	$(COMP) -C -o $*-incl.c $*.stk

.c.@SH_SUFFIX@ :
	@CC@ @CFLAGS@  @STKCFLAGS@ @SH_COMP_FLAGS@ \
         $(GTKFLAGS) \
         -I$(BASEDIR)/src @GCINC@ @GMPINC@ \
         -c -o $*.o $*.c
	@SH_LOADER@ @SH_LOAD_FLAGS@  -o $*.@SH_SUFFIX@ $*.o @DLLIBS@
	/bin/rm -f $*.o

$(EXTENSION)-incl.c: $(EXTENSION).stk $(SCM_SRCS)


all: $(OUT)

$(OUT): $(EXTENSION)-incl.c $(EXTENSION).c


$(EXTENSION)-incl.c: $(EXTENSION).stk

install-exec-hook:
	install -d $(DESTDIR)$(schemelibdir)
	install $(OUT) $(DESTDIR)$(schemelibdir)

clean-am:
	rm -f $(OUT) $(EXTENSION)-incl.c  *~

distclean-am:
	rm -f Makefile gtklos.stk gtklos-config.h

