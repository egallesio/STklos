### configure.in for STklos
###
###           Author: Erick Gallesio [eg@unice.fr]
###    Creation date: 28-Dec-1999 21:19 (eg)
### Last file update:  2-Feb-2023 10:58 (eg)

AC_PREREQ([2.69])
AC_INIT([stklos],[1.70])

#
# Grab configure options before it is too late
#
CONF_OPT=""; HAS_CC=""; HAS_CFLAGS=""

for opt in "$@"
do
  case $opt in
     CC=*)     HAS_CC=1 ;;
     CFLAGS=*) HAS_CFLAGS=1;;
  esac

  if test "X$opt" = "X--no-create" -o "X$opt" = "X--no-recursion"
  then
     # These options are added when configure is older than configure.ac
     true
  else
     CONF_OPT="$CONF_OPT \\\"$opt\\\""
  fi
done

AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR(src/stklos.c)
AC_CONFIG_HEADERS(src/stklosconf.h)
AC_CANONICAL_HOST


### Checks for programs.
AC_PROG_CC
AC_PROG_CPP
AC_PROG_AWK
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB
# AC_PROG_LEX
AC_PROG_MKDIR_P
AC_CHECK_PROG(PKGCONFIG, pkg-config, "pkg-config", "false")
AC_CHECK_PROG(ASCIIDOCTOR, asciidoctor, "asciidoctor", "../adoc-fake.sh")
### Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h fcntl.h limits.h malloc.h memory.h netdb.h netinet/in.h nlist.h stdlib.h string.h strings.h sys/param.h sys/socket.h sys/time.h syslog.h unistd.h execinfo.h])



### Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_C_BIGENDIAN

### Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_FUNC_MKTIME
AC_FUNC_REALLOC
AC_FUNC_STRCOLL
AC_FUNC_STRTOD
AC_CHECK_FUNCS([atexit dup2 endpwent floor getcwd gethostbyaddr gethostbyname gethostname getpass gettimeofday glob inet_ntoa isascii memchr memmove memset mkdir modf putenv rmdir select socket sqrt strcasecmp strchr strerror strrchr strstr strtoul])

AC_CHECK_FUNCS(backtrace, , AC_CHECK_LIB(execinfo, backtrace))


### ======================================================================
### Various stuff done by hand
### ======================================================================

### Initialize prefix
if test X$prefix = XNONE ;then  prefix=$ac_default_prefix; fi

### Determine the options necessary for dynamic loading (even if disbaled later)
if test X$LD = X
then
   LD="ld"
fi

# DLLIBS is used for dynamically loaded libraries on broken systems
DLLIBS=""

eval `env CC=$CC LD=$LD /bin/sh ./utils/shlib-options`

case $OS_NAME_VERSION in
  SUNOS*)
    CFLAGS="$CFLAGS -DSOLARIS";
    LIBS="$LIBS -lsocket -lrt -lnsl -lpthread"
    ;;
  DARWIN_10)
     GCINC="GCINC -D_XOPEN_SOURCE"
     EXTRA_COMP_FLAGS="-I/opt/local/include"
     ;;
  UNKNOWN*)
    echo ""
    echo "**** STklos has no support for your system (`uname -s`)"
    echo "**** Please modify the script \"utils/shlib-options\" to add"
    echo "**** support for your system (it's easy) and restart installation process."
    echo "**** Thanks for sending your modifications to <bugs _at_ stklos.net>"
    exit 1
    ;;
esac


### Initilize the extra conf file in src
SCMDIR=$prefix/share/$PACKAGE/$VERSION
EXECDIR=$prefix/lib/$PACKAGE/$VERSION
PREFIX=$prefix

### Determine the kind of the machine which is running this script
BUILD_OS=$os-$version
BUILD_ARCH=`uname -m`
OS_NAME=$OS
case $OS in
   LINUX)
      v=`echo $version | sed -e 's/\([[0-9]]*\)\.\([[0-9\]]*\).*/\1_\2/'`
      OS_NAME_VERSION=LINUX_$v
      ;;
   DARWIN)
      case $version in
         9*)  OS_NAME_VERSION=DARWIN_9;;
         10*) OS_NAME_VERSION=DARWIN_10;;
         11*) OS_NAME_VERSION=DARWIN_11;;
         *)   OS_NAME_VERSION=DARWIN_OLD;;
      esac
      ;;
  *)
      v=`echo $version | sed -e 's/[[^0-9]]/_/g'`
      OS_NAME_VERSION=$OS\_$v;
esac


###
### Checks for libraries.
###
SYST_LIBS=""
COMP_LIBS=""


### Check for dynamic loading
AC_CHECK_FUNC(dlopen, HAVE_DLOPEN="yes", AC_CHECK_LIB(dl, dlopen, LIBS="-ldl $LIBS"&&HAVE_DLOPEN="yes"))

if test "X$HAVE_DLOPEN" = "Xyes"
then
  AC_DEFINE(HAVE_DLOPEN, 1, [System provides dlopen])
fi

###
### Be case sensitive or not
###
CASE_SENSITIVE=true

AC_ARG_ENABLE(case-insensitive,
  [AS_HELP_STRING([--enable-case-insensitive],[Be case insensitive by default as in R5RS])],
  CASE_SENSITIVE=false,
  CASE_SENSITIVE=true)

case "$CASE_SENSITIVE" in
  false | 0 |off| yes)
     DEFAULT_CS=0
     ;;
  *)
     DEFAULT_CS=1
     ;;
esac


###
### To thread or not to thread ?
###
AC_CHECK_LIB(pthread, pthread_create, deflt_threads=pthreads, deflt_threads=none)

AC_ARG_ENABLE(threads,
  [AS_HELP_STRING([--enable-threads=TYPE],[choose threading package (none,pthreads)])],
  THREADS=$enableval,
  THREADS=$deflt_threads)

AC_LANG(C)

### Now look at what threads we want
case "$THREADS" in
 no | none | single)
   THREADS=none
   AC_DEFINE(THREADS_NONE, [], [No thread support])
   ;;
 posix | pthreads)
   ### Check for pthreads
   AC_CHECK_LIB(pthread, pthread_create, LIBS="-lpthread $LIBS",
                [AC_MSG_ERROR([Missing PThread library])])
   THREADS=pthreads
   AC_DEFINE(THREADS_PTHREADS, [], [PThread support])
   AC_CHECK_FUNCS([pthread_cancel])
   ;;
 *)
   AC_MSG_ERROR([Unknown thread system: $THREADS])
   ;;
esac

#XX
#XX ### Add the options _REENTRANT and GC_LINUX_THREADS if we are on Linux
#XX case "$THREADS" in
#XX   none) true ;;
#XX   *) case `uname -s` in
#XX         Linux*) STKCFLAGS="-D_REENTRANT=1 -DGC_LINUX_THREADS=1";;
#XX      esac
#XX esac
#XX




### Automake nightmare
AM_CONDITIONAL(PTHREADS, test "$THREADS" = pthreads)
AM_CONDITIONAL(NO_THREAD, test "$THREADS" = none)

# ----------------------------------------------------------------------
# GC support
# ----------------------------------------------------------------------
echo "*****"
echo "***** GC support"
echo "*****"

# Patch code for OSX Lion
case $OS_NAME_VERSION in
     DARWIN_11)
       if test ! -f gc/stklos-patch-applied
       then
              echo "**** Patching GC library";
              patch -p1 < etc/lion.patch
              touch  gc/stklos-patch-applied
       fi
       ;;
esac


# Try to determine if libgc is installed
if ${PKGCONFIG} --exists bdw-gc ;then
   HAVE_GC="yes"
else
   AC_CHECK_LIB(gc, GC_malloc, HAVE_GC="yes", HAVE_GC="no", $LIBS)
fi

# Test if the user want to force with our GC lib
AC_ARG_WITH(provided-gc,
       [  --with-provided-gc      use the provided Boehm GC library],
       PROV_GC="$withval", PROV_GC="no")

if test "$HAVE_GC" = "no" -o "$PROV_GC" = "yes"
then
     echo "... Configuring libgc"

     case $OS_NAME_VERSION in
       DARWIN_10)
          cflags="${CFLAGS} -D_XOPEN_SOURCE";;
       *)
          cflags="${CFLAGS}";;
     esac
     (cd gc; CC=${CC} CFLAGS=${cflags} ./configure --prefix=$prefix --enable-static \
              --includedir=$prefix/include/stklos \
              --enable-threads=$THREADS) || { echo "Cannot configure the GC"; exit; }
     GC=gc
     GCINC='-I$(BASEDIR)/gc/include'
     GCLIB="../gc/.libs/libgc.a"
     COMP_LIBS="libgc $COMP_LIBS"
else
     echo "... Using system libgc library"
     GC=""
     if $PKGCONFIG --exists bdw-gc ;then
        GCINC=$($PKGCONFIG  bdw-gc --cflags)
        GCLIB=$($PKGCONFIG  bdw-gc --libs)
     else
        GCINC=""
        GCLIB="-lgc"
     fi
     DLLIBS="$DLLIBS -lgc"
     AC_DEFINE(HAVE_GC, 1, [Use system GC])
     SYST_LIBS="libgc $SYST_LIBS"
fi


# ----------------------------------------------------------------------
# GMP support
# ----------------------------------------------------------------------
echo "*****"
echo "***** GMP support"
echo "*****"

# Try to determine if libgmp is installed
# No pkg-config for gmp yet

# check for GMP pre-version 3.0
AC_CHECK_LIB(gmp, mpz_init_set, HAVE_GMP="yes", HAVE_GMP="no")

# check for GMP version post 3.0
if test "$HAVE_GMP" = "no" ;then
  AC_CHECK_LIB(gmp, __gmpz_init_set, HAVE_GMP="yes", HAVE_GMP="no")
fi

# Test if the user want to force with our GMP lib
# (2 names available for that  --with-provided-bignum or (legacy) --with-gmp-light
AC_ARG_WITH(provided-bignum,
       [  --with-provided-bignum  use the provided Bignum (GMPlite) library],
       PROV_GMP="$withval", PROV_GMP="no")

if test "$PROV_GMP" = "no" ;then
  AC_ARG_WITH(gmp-light,
         [  --with-gmp-light        a synonym for --with-provided-bignum],
         PROV_GMP="$withval", PROV_GMP="no")
fi

if test "$HAVE_GMP" = "no" -o "$PROV_GMP" = "yes"
then
     echo "... Configuring libgmp"
     (cd gmp; CC=${CC} CFLAGS="${cflags} -I../src -DFOR_STKLOS=1" ./configure \
              --prefix=$prefix) || { echo "Cannot configure the GMP"; exit; }
     GMP=gmp
     GMPINC='-I$(BASEDIR)/gmp'
     GMPLIB="../gmp/.libs/libgmp.a"
     COMP_LIBS="libgmp $COMP_LIBS"
else
     echo "... Using system libgmp library"
     GMP=""
     GMPINC=""
     GMPLIB="-lgmp"
     DLLIBS="$DLLIBS $GMPLIB"
     AC_DEFINE(HAVE_GMP, 1, [Use system GMP])
     SYST_LIBS="libgmp $SYST_LIBS"
fi


# ----------------------------------------------------------------------
# PCRE support
# ----------------------------------------------------------------------
echo "*****"
echo "***** Regexp support"
echo "*****"

# Try to determine if libpcre is installed
if ${PKGCONFIG} --exists libpcre ;then
   HAVE_PCRE="yes"
   PCREPKG="present"
else
   AC_CHECK_LIB(pcre, pcre_fullinfo, HAVE_PCRE="yes", HAVE_PCRE="no", $LIBS)
fi

# Test if the user want to force with our PCRE lib
AC_ARG_WITH(provided-regexp,
       [  --with-provided-regexp  use the provided Regexp (PCRE) library],
       PROV_PCRE="$withval", PROV_PCRE="no")

if test "$HAVE_PCRE" = "no" -o "$PROV_PCRE" = "yes"
then
     echo "... Configuring libpcre"
     PCRE_FLAGS="-Dregexec=PCRE_regexec -Dregcomp=PCRE_regcomp \
                 -Dregerror=PCRE_regerror -Dregfree=PCRE_regfree"
     (cd pcre; CC=${CC} CFLAGS="${CFLAGS} $PCRE_FLAGS" ./configure \
           --enable-utf8 \
           --prefix=$prefix --disable-shared --disable-cpp) || \
           { echo "Cannot configure PCRE"; exit; }
     PCRE=pcre
     PCREINC="-I../pcre"
     PCRELIB="../pcre/.libs/libpcreposix.a ../pcre/.libs/libpcre.a"
     DEF_PCRE=""
     COMP_LIBS="libpcre $COMP_LIBS"
else
     echo "... Using system libpcre library"
     PCRE=""
     if $PKGCONFIG --exists libpcre ;then
        PCREINC="$($PKGCONFIG --cflags libpcre)"
        PCRELIB="$($PKGCONFIG --libs libpcre) -lpcreposix"
     else
        PCREINC=""
        PCRELIB="-lpcre -lpcreposix"
     fi
     DLLIBS="$DLLIBS $PCRELIB"
     AC_DEFINE(HAVE_PCRE, 1 , [We use our version of pcre])
     SYST_LIBS="libpcre $SYST_LIBS"
fi


# ----------------------------------------------------------------------
# FFI support
# ----------------------------------------------------------------------
echo "*****"
echo "***** FFI support"
echo "*****"

# Try to determine if libffi is installed
if ${PKGCONFIG} --exists libffi ;then
   HAVE_FFI="yes"
else
  AC_CHECK_LIB(ffi, ffi_prep_cif, HAVE_FFI="yes", HAVE_FFI="no", $LIBS)
fi

# Test if the user want to force with our FFI lib
AC_ARG_WITH(provided-ffi,
       [  --with-provided-ffi     use the provided FFI library],
       PROV_FFI="$withval", PROV_FFI="no")

if test "$HAVE_FFI" = "no" -o "$PROV_FFI" = "yes"
then
     echo "... Configuring libffi"
     (cd ffi; CC=${CC} CFLAGS=${CFLAGS} ./configure \
           --disable-structs --disable-raw-api --disable-shared \
           --prefix=$prefix) || { echo "Cannot configure libffi"; exit; }
     FFI="ffi"
     FFIINC="-I../ffi/$(./config.guess)/include"
     FFILIB="../ffi/$(./config.guess)/.libs/libffi.a"
     COMP_LIBS="libffi $COMP_LIBS"
else
     echo "... Using system libffi library"
     FFI=""
     if $PKGCONFIG --exists libffi ;then
       FFIINC=$($PKGCONFIG libffi --cflags)
       FFILIB=$($PKGCONFIG libffi --libs)
     else
       FFIINC="-I/usr/include/ffi"
       FFILIB="-lffi"
       DLLIBS="$DLLIBS $FFILIB"
     fi
     DLLIBS="$DLLIBS $FFILIB"
     SYST_LIBS="libffi $SYST_LIBS"
fi
# We always want FFI. Should be modifiable in the future
AC_DEFINE(HAVE_FFI, 1, [System provides FFI])


# ----------------------------------------------------------------------
# Control fixnum and flonum parameters
# ----------------------------------------------------------------------
AC_ARG_WITH(control-fast-math,
       [  --with-control-fast-math  verify that arguments to fx and fl functions are fixnums and flonums],
       CONTROL_FX="$withval", CONTROL_FX="yes")

if test "$CONTROL_FX" = "yes"
then
    CONTROL_FX_PARAMETERS=1
else
    CONTROL_FX_PARAMETERS=0
fi

###
### See in what direction the stack grows (code stolen from Sawfish)
###
AC_RUN_IFELSE([AC_LANG_SOURCE([[ void inner (char *foo) { char bar; exit (!(foo >= &bar)); }
             void main () { char foo; inner (&foo); } ]])],[STACK_DIRECTION="DOWN"],[STACK_DIRECTION="UP"],[echo "Stack direction is not detected when cross compiling for now"])



###
### Configuration for extensions
###

## --- GTKLOS

GTK_CFLAGS=""
HAVE_GTK=0
HAVE_CANVAS=0
GOOCANV=absent
GOOVERS=""

# Do we have GTK+?
if ${PKGCONFIG} --exists gtk+-3.0
then
  GTK_CFLAGS=`${PKGCONFIG} --cflags gtk+-3.0`
  HAVE_GTK=1
fi

# Do we have goocanvas?
if ${PKGCONFIG} --exists goocanvas-3.0
then
   GOOVERS=goocanvas-3.0
else
   if ${PKGCONFIG} --exists goocanvas-2.0
   then
      GOOVERS=goocanvas-2.0
   fi
fi

if test "X${GOOVERS}" != "X"
then
   GTK_CFLAGS=`${PKGCONFIG} --cflags ${GOOVERS}`
   HAVE_GTK=1
   HAVE_CANVAS=1
   GOOCANV=present
fi

###
### SFI-176 stuff
###
C_VERSION=`(${CC} --version | head -n 1) 2>/dev/null || ${CC}`
C_COMPILE=""
C_LINK=""

# c.compile property
for arg in ${CC} ${CFLAGS} ${SH_MAIN_LOAD_FLAGS}
do
    C_COMPILE="${C_COMPILE}\\\"${arg}\\\" "
done

# c.link property
for arg in ${CC} ${LDLAGS} ${GMPLIB} ${PCRELIB} ${FFILIB} ${GCLIB} -lm
do
    C_LINK="${C_LINK}\\\"${arg}\\\" "
done

SH_LIB_COMPILE=""
SH_LIB_LINK=""

# stklos.shlib.compile property
inc=-I${prefix}/include/$PACKAGE
for arg in ${CC} ${SH_COMP_FLAGS} ${inc} ${inc}/gc ${EXTRA_COMP_FLAGS}
do
    SH_LIB_COMPILE="${SH_LIB_COMPILE}\\\"${arg}\\\" "
done

# stklos.shlib.link property
for arg in ${SH_LOADER} ${SH_LOAD_FLAGS}
do
    SH_LIB_LINK="${SH_LIB_LINK}\\\"${arg}\\\" "
done


###
### Finishing ...
###

# See if we must add CC and CFLAGS to the arguments stored in $CONF_OPT
if test "X$HAS_CC" = "X" ;then
   CONF_OPT="$CONF_OPT \\\"CC=$CC\\\""
fi

if test "X$HAS_CFLAGS" = "X" ;then
   CONF_OPT="$CONF_OPT \\\"CFLAGS=$CFLAGS\\\""
fi

CONF_SUMMARY="(:system ($SYST_LIBS) :compiled ($COMP_LIBS) :configure ($CONF_OPT))"

# Determmine the STRIP command to use
if test "X$STRIP" = "X" ;then
   STRIPCMD=strip
else
   STRIPCMD=$STRIP
fi

UNAME_O=""
case `uname -s` in
  *Linux*)
    UNAME_O=`uname -o | sed -e 's=/=_='`
    ;;
  *) # Darwin, for instance, does not support -o. Use the result of uname -s
    UNAME_O=`uname -s`
    ;;
esac

# Adapt libraries that must be loaded for systems with broken RTLD_GLOBAL (as Android)
RDLINE="";        # Necessary for Android
case $UNAME_O in
  Android) # Test if we have readline lib or libedit
           DLLIBS="$DLLIBS -lc -lm"
           AC_CHECK_LIB(readline, readline, HAVE_READLINE="yes", HAVE_READLINE="no", $LIBS)
           if test "$HAVE_READLINE" = "yes" ;then
              RDLINE="-lreadline";
            else
                AC_CHECK_LIB(edit, readline, HAVE_LIBEDIT="yes", HAVE_LIBEDIT="no", $LIBS)
                if test "$HAVE_LIBEDIT" = "yes" ;then
                   RDLINE="-ledit";
                fi
            fi;;
  *) DLLIBS="";;
esac

### Output files
AC_SUBST(STKCFLAGS)
AC_SUBST(VERSION)
AC_SUBST(HAVE_DLOPEN)
AC_SUBST(OS_FLAVOUR)
AC_SUBST(UNAME_O)
AC_SUBST(GMP)
AC_SUBST(GMPLIB)
AC_SUBST(GMPINC)
AC_SUBST(PREFIX)
AC_SUBST(SCMDIR)
AC_SUBST(EXECDIR)
AC_SUBST(BUILD_OS)
AC_SUBST(BUILD_ARCH)
AC_SUBST(OS_NAME)
AC_SUBST(OS_NAME_VERSION)
AC_SUBST(EXTRA_COMP_FLAGS)
AC_SUBST(SH_COMP_FLAGS)
AC_SUBST(SH_LOAD_FLAGS)
AC_SUBST(SH_LOADER)
AC_SUBST(SH_SUFFIX)
AC_SUBST(SH_LIB_SUFFIX)
AC_SUBST(SH_MAIN_LOAD_FLAGS)
AC_SUBST(COMPOBJ)
AC_SUBST(COMPSRC)
AC_SUBST(STACK_DIRECTION)
AC_SUBST(HAVE_GNOME)
AC_SUBST(LDAP_LIB)
AC_SUBST(STRIPCMD)
#GC
AC_SUBST(GC)
AC_SUBST(GCLIB)
AC_SUBST(GCINC)
# PCRE
AC_SUBST(PCRE)
AC_SUBST(PCRELIB)
AC_SUBST(PCREINC)
AC_SUBST(PCREPKG)
# FFI
AC_SUBST(FFI)
AC_SUBST(FFILIB)
AC_SUBST(FFIINC)
# readline completion support
AC_SUBST(RDLINE)

AC_SUBST(THREADS)
AC_SUBST(CONF_SUMMARY)
AC_SUBST(DEFAULT_CS)
AC_SUBST(CONTROL_FX_PARAMETERS)
AC_SUBST(DLLIBS)
AC_SUBST(SYST_LIBS)
AC_SUBST(COMP_LIBS)
AC_SUBST(C_VERSION)
AC_SUBST(C_COMPILE)
AC_SUBST(C_LINK)
AC_SUBST(SH_LIB_COMPILE)
AC_SUBST(SH_LIB_LINK)

AC_SUBST(C_LINK)

# DOC
AC_SUBST(ASCIIDOCTOR)

# Extensions
AC_SUBST(HAVE_GTK)
AC_SUBST(HAVE_CANVAS)
AC_SUBST(GTK_CFLAGS)
AC_SUBST(GOOCANV)
AC_SUBST(GOOVERS)

AC_CONFIG_FILES([Makefile src/Makefile src/extraconf.h doc/Makefile
          doc/refman/Makefile doc/vm/Makefile doc/hacking/Makefile
          lib/Makefile utils/Makefile utils/stklos-config
          utils/stklos-script examples/Makefile
          lib/Match.d/Makefile lib/SILex.d/Makefile lib/Lalr.d/Makefile
          lib/scheme/Makefile lib/srfi/Makefile
          lib/stklos/Makefile lib/streams/Makefile
          tests/Makefile
          doc/stklos.1 doc/stklos-config.1 doc/stklos-compile.1
          doc/stklos-genlex.1 doc/stklos-pkg.1 doc/stklos-script.1

          lib/srfi/160/Makefile

          extensions/Makefile

          extensions/gtklos/Makefile
          extensions/gtklos/lib/stklos/Makefile
          extensions/gtklos/lib/stklos/gtklos-config.h
          extensions/gtklos/lib/stklos/gtklos.stk
          extensions/gtklos/demos/Makefile

          extensions/curl/Makefile
          extensions/curl/lib/stklos/Makefile
          extensions/curl/demos/Makefile
          extensions/curl/doc/Makefile

          extensions/fuse/Makefile
          extensions/fuse/lib/stklos/Makefile
          extensions/fuse/demos/Makefile
])
AC_OUTPUT

# Add a src/git-config.h if we dont have one
(cd src; ../utils/generate-git-info)

chmod 0755 utils/stklos-config utils/stklos-script


echo " "
echo " "
echo "SUMMARY"
echo "*******"
echo "               System: " $BUILD_OS
echo "              OS nick: " $OS_NAME_VERSION
echo "              OS type: " $OS_FLAVOUR
echo "      Install prefix : " $prefix
echo "           C compiler: " $CC
echo "    Compilation flags: " $CFLAGS
echo "               Loader: " $LD
echo "       Thread support: " $THREADS
echo "     Case sensitivity: " $CASE_SENSITIVE '(by default)'
echo "Control fx parameters: " $CONTROL_FX
echo "System libraries used: " $SYST_LIBS
echo "   Compiled libraries: " $COMP_LIBS
echo " "

echo "If this is correct, you can just type 'make' now at your shell prompt."
echo "Otherwise, re-run 'configure' with correct options."
echo " "
