schemedemodir       = $(prefix)/share/@PACKAGE@/@VERSION@/Demos.d
schemedemo_DATA	    = button.stk canvas.stk checkbutton.stk \
		      colorselector.stk combobox.stk dialog.stk \
		      entry1.stk entry2.stk event.stk \
		      fileselector.stk fontselector.stk frame.stk \
		      gauge.stk grid1.stk grid1 grid2.stk gtklos-demo.stk \
		      handle.stk image1.stk image2.stk label.stk listbox.stk  \
		      menu.stk radiobutton.stk scale.stk sedit.stk \
		      scroll1.stk scroll2.stk text.stk toolbar.stk \
			\
		      socket-server.stk socket-client.stk

schemedemo_SCRIPTS  = button canvas checkbutton colorselector combobox dialog \
		      entry1 entry2 event fileselector fontselector frame \
		      gauge grid1 grid2 handle image1 image2 label listbox \
		      menu radiobutton scale sedit scroll1 scroll2 text \
		      toolbar \
			\
		      fork-test socket-server socket-client

SUFFIXES            = .stk

.stk:
	../utils/tmpcomp $*.stk $*

clean:
	   /bin/rm -f $(schemedemo_SCRIPTS) *~ 

distclean: clean
	/bin/rm -f Makefile 
	/bin/rm -rf .deps
