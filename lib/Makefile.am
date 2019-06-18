# Makefile.am for STklos lib
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 11-Apr-2000 10:30 (eg)
# Last file update: 13-Jun-2019 09:28 (eg)

SUBDIRS = Match.d SILex.d Lalr.d ScmPkg.d

SFLAGS=

scheme_BOOT = assembler.stk		\
	      bb.stk 			\
	      bonus.stk 		\
	      boot.stk 			\
	      callcc.stk		\
	      compiler.stk 		\
	      computils.stk		\
	      date.stk			\
	      equiv.stk			\
	      expand.pp			\
	      ffi.stk			\
	      load.stk			\
	      logical.stk		\
	      mbe.stk			\
	      module.stk		\
	      object.stk 		\
	      obsolete.stk		\
	      peephole.stk		\
	      process.stk		\
	      r5rs.stk 			\
	      r7rs.stk			\
	      regexp.stk		\
	      readline.stk		\
	      repl.stk 			\
	      repl-readline.stk		\
	      runtime.stk		\
	      str.stk			\
	      srfi-0.stk		\
	      struct.stk		\
	      thread.stk


scheme_SRCS = STklos.init		\
	      bigloo-support.stk	\
	      bigmatch.stk		\
	      compfile.stk 		\
	      describe.stk		\
	      env.stk			\
	      expand.ss			\
	      full-syntax.stk		\
	      full-conditions.stk 	\
	      getopt.stk		\
	      help.stk			\
	      http.stk			\
	      lex-rt.stk		\
	      make-C-boot.stk		\
	      pretty-print.stk		\
	      r7rs-unicode.stk		\
	      recette.stk		\
	      slib.stk			\
	      srfi-1.stk		\
	      srfi-2.stk		\
	      srfi-4.stk		\
	      srfi-7.stk		\
	      srfi-9.stk		\
	      srfi-11.stk		\
	      srfi-13.stk		\
	      srfi-14.stk		\
	      srfi-17.stk		\
	      srfi-26.stk		\
	      srfi-27.stk		\
	      srfi-31.stk		\
	      srfi-34.stk		\
	      srfi-35.stk		\
	      srfi-36.stk		\
	      srfi-45.stk		\
	      srfi-48.stk		\
	      srfi-59.stk		\
	      srfi-60.stk		\
	      srfi-66.stk		\
	      srfi-69.stk		\
	      srfi-70.stk		\
	      srfi-74.stk		\
	      srfi-89.stk		\
	      srfi-96.stk		\
	      srfi-100.stk		\
	      tar.stk			\
	      trace.stk

scheme_OBJS = compfile.ostk		\
	      bigmatch.ostk		\
	      full-syntax.ostk		\
	      full-conditions.ostk 	\
	      describe.ostk		\
	      env.ostk			\
	      getopt.ostk		\
	      help.ostk			\
	      http.ostk			\
	      lex-rt.ostk		\
	      pretty-print.ostk		\
	      r7rs-unicode.ostk		\
	      recette.ostk		\
	      slib.ostk			\
	      srfi-1.ostk 		\
	      srfi-2.ostk		\
	      srfi-4.ostk		\
	      srfi-7.ostk		\
	      srfi-9.ostk		\
	      srfi-11.ostk		\
	      srfi-13.ostk		\
	      srfi-14.ostk		\
	      srfi-17.ostk		\
	      srfi-26.ostk		\
	      srfi-27.ostk		\
	      srfi-31.ostk		\
	      srfi-34.ostk		\
	      srfi-35.ostk		\
	      srfi-36.ostk		\
	      srfi-45.ostk		\
	      srfi-48.ostk		\
	      srfi-59.ostk		\
	      srfi-60.ostk		\
	      srfi-66.ostk		\
	      srfi-69.ostk		\
	      srfi-70.ostk		\
	      srfi-74.ostk		\
	      srfi-89.ostk		\
	      srfi-96.ostk		\
	      srfi-100.ostk		\
	      tar.ostk			\
	      trace.ostk

DOCDB	    = DOCDB

#======================================================================
schemedir   = $(prefix)/share/@PACKAGE@/@VERSION@
scheme_DATA = $(scheme_SRCS) $(scheme_OBJS) $(scheme_BOOT)

#======================================================================

SUFFIXES = .stk .ostk .scm
.stk.ostk:
	../utils/tmpcomp $*.stk $*.ostk

.scm.ostk:
	../utils/tmpcomp $*.scm $*.ostk

#======================================================================

all-recursive: boot compfile.ostk

boot:	../src/boot.img

../src/boot.img: $(scheme_BOOT)
	@echo "*** Boot 0"; \
	(export STKLOS_BUILDING=1; \
	../src/stklos -q -c -b ../src/boot.img -f bb.stk boot.img0 instr0)
	@echo "*** Boot 1"; \
	(export STKLOS_BUILDING=1; \
	../src/stklos $(SFLAGS) -q -c -b ./boot.img0     -f bb.stk boot.img1 instr1)
	@echo "*** Boot 2"; \
	(export STKLOS_BUILDING=1; \
	 ../src/stklos $(SFLAGS) -q -c -b ./boot.img1    -f bb.stk boot.img2 instr2)
	@echo "*** Boot 3"; \
	(export STKLOS_BUILDING=1; \
	../src/stklos $(SFLAGS) -q -c -b ./boot.img2     -f bb.stk boot.img3 instr3)
	@if cmp ./boot.img2 ./boot.img3 ;then 				\
	   echo "*** New boot file created";  				\
	   cp ../src/boot.img ../src/boot.ok; 				\
	   cp ./boot.img3 ../src/boot.img;    				\
	   cp ./instr3    ../src/vm-instr.h;  				\
	   echo "*** Create new boot.c";				\
	   (export STKLOS_BUILDING=1;					\
	    ../src/stklos $(SFLAGS) -q -c -b ../src/boot.img  		\
		-f make-C-boot.stk -- boot.img3 ../src/boot.c);		\
	   echo "*** Recompile STklos";					\
	   (cd ../src; $(MAKE) stklos);					\
	   echo "*** Cleaning useless images";				\
	   /bin/rm boot.img[0-3] instr[0-3];  				\
	else 								\
	   echo "*** Boot file creation failed"; 			\
	   exit 1; 							\
	fi;

# Compile SRFI13 before SRFI14 to avoid 2 warnings

$(SCHEME_OBJS): ../src/boot.img
compfile.ostk: getopt.ostk
ffi.ostk: bigmatch.ostk
full-conditions: srfi-35.ostk srfi-36.stk
recette.ostk: pretty-print.ostk
srfi-13.ostk: srfi-14.ostk
srfi-66.ostk: srfi-4.ostk
slib.ostk: slib.stk STklos.init


doc: $(DOCDB)

$(DOCDB): $(scheme_SRCS) $(scheme_BOOT)
	../src/stklos -q -c -b ../src/boot.img -f ../doc/extract-doc \
	$(scheme_SRCS) $(scheme_BOOT) > $(DOCDB)

clean:
	/bin/rm -f $(scheme_OBJS)
	@for i in $(SUBDIRS) ;do \
	   (cd $$i; $(MAKE) clean)\
	done

distclean: clean
	/bin/rm -f Makefile
	@for i in $(SUBDIRS) ;do \
	   (cd $$i; $(MAKE) distclean)\
	done
