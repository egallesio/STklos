# Makefile.in for GTklos examples
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 19-Sep-2009 20:29 (eg)

SRC= demos.stk \
     button.stk canvas1.stk canvas2.stk canvas3.stk    \
     canvas4.stk canvas5.stk checkbutton.stk           \
     colorchooser.stk                                  \
     combobox.stk dialog.stk entry1.stk entry2.stk     \
     entry3.stk event.stk filechooser.stk              \
     fontchooser.stk frame.stk grid1.stk grid2.stk     \
     header.stk image1.stk image2.stk label.stk        \
     menu1.stk menu2.stk menu3.stk progress.stk        \
     radiobutton.stk scale.stk scroll1.stk scroll2.stk \
     sedit.stk separator.stk text.stk toolbar.stk


COMP ?= ../../../utils/tmpcomp

EXE=$(SRC:.stk=)

SUFFIXES= .stk

.stk:
	$(COMP) -o $* $<

all: 	$(EXE)
	@echo $(EXE) > all-demos

install:
	@echo "Demos are not installed"

clean:
	/bin/rm -f $(EXE) *~ all-demos

distclean-am:
	/bin/rm -f stklos-gtklos.so
	/bin/rm -f Makefile

demo: all
	./demos
