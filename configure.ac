### configure.ac for STklos
###
###           Author: Erick Gallesio [eg@unice.fr]
###    Creation date: 28-Dec-1999 21:19 (eg)

AC_PREREQ([2.69])
AC_INIT([stklos],[2.00])   # Set also VERSION_STATUS to stable when changing version
VERSION_STATUS=unstable    # and reset it to unstable after releasing

#
# Grab configure options before it is too late
#
CONF_OPT=""; HAS_CC=""; HAS_CFLAGS=""; HAS_LDFLAGS=""; HAS_CPPFLAGS=""
PDFDIR="";HTMLDIR=""; DOCDIR=""; LIBDIR=""; DATADIR="";

for opt in "$@"
do
  case $opt in
     CC=*)        HAS_CC=1 ;;
     CFLAGS=*)    HAS_CFLAGS=1;;
     LDLAGS=*)    HAS_LDLAGS=1;;
     CPPFLAGS=*)  HAS_CPPFLAGS=1;;
     --docdir=*)  DOCDIR=${docdir};;
     --pdfdir=*)  PDFDIR=${pdfdir};;
     --htmldir=*) HTMLDIR=${htmldir};;
     --libdir=*)  LIBDIR=${libdir};;
     --datadir=*) DATADIR=${datadir};;
  esac

  if test "X$opt" = "X--no-create" -o "X$opt" = "X--no-recursion"
  then
     # These options are added when configure is older than configure.ac
     true
  else
     CONF_OPT="$CONF_OPT \\\"$opt\\\""
  fi
done

# export CC, CFLAGS and all for configure scripts launched by this configure script
export CC CFLAGS LDFLAGS CPPFLAGS

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
if test "$ASCIIDOCTOR" = "asciidoctor"
then
    REPORT_ASCIIDOCTOR="yes (since Asciidoctor is installed)"
else
    REPORT_ASCIIDOCTOR="no (install Asciidoctor to rebuild documentation)"
fi
### Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h fcntl.h limits.h malloc.h memory.h netdb.h netinet/in.h nlist.h stdlib.h string.h strings.h sys/param.h sys/socket.h sys/time.h syslog.h unistd.h execinfo.h])



### Checks for typedefs, structures, and compiler characteristics.
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
AC_CHECK_FUNCS([atexit dup2 endpwent floor getcwd gethostbyaddr gethostbyname gethostname getpass clock_gettime glob inet_ntoa isascii memchr memmove memset mkdir modf putenv rmdir select socket sqrt strcasecmp strchr strerror strrchr strstr strtoul utimensat])

AC_CHECK_FUNCS(backtrace, , AC_CHECK_LIB(execinfo, backtrace))


### ======================================================================
### Various stuff done by hand
### ======================================================================

### Initialize prefix
if test X$prefix = XNONE ;then  prefix=$ac_default_prefix; fi

### Determine the options necessary for dynamic loading (even if disabled later)
if test X$LD = X
then
   LD="ld"
fi

# DLLIBS is used for dynamically loaded libraries on broken systems
DLLIBS=""

eval `env CC=$CC LD=$LD /bin/sh ./utils/shlib-options`

case $OS in
  SUNOS)
    CFLAGS="$CFLAGS -DSOLARIS";
    LIBS="$LIBS -lsocket -lrt -lnsl -lpthread"
    ;;
  UNKNOWN)
    echo ""
    echo "**** STklos has no support for your system (`uname -s`)."
    echo "**** Please modify the script \"utils/shlib-options\" to add support"
    echo "**** for your system (it's easy) and rerun the configure script."
    echo "**** Thanks for sending your modifications to <bugs _at_ stklos.net>"
    exit 1
    ;;
esac

# Manage standard options --docdir, --pdfdir, --htmldir
# By default, autoconf uses
#   docdir = ${prefix}/share/doc/stklos
#   pdfdir = ${docdir}
#   htmldir = ${docdir}
# To permit to have several versions of STklos installed, and to avoid
# cluttering the doc directory, we use
#   docdir = ${prefix}/share/doc/stklos/${VERSION}
#   pdfdir = ${docdir}/pdf
#   htmldir = ${docdir}/html
# Of course, if the user specify any option, we use the value he/she gave
if test "$DOCDIR" == "" ; then
    # user has not given a "--docdir" option. Use our own convention
    docdir="${prefix}/share/doc/${PACKAGE_TARNAME}/${VERSION}"
fi

if test "$PDFDIR" == ""  ; then
   # user has not given a "--pdfdir" option. Use our own convention
   pdfdir="${docdir}/pdf"
fi

if test "$HTMLDIR" == ""  ; then
   # user has not given a "--htmldir" option. Use our own convention
   htmldir="${docdir}/html"
fi

# Manage standard options --libdir and --datadir
# If the user doesn't specify these option, we fix
#   libdir to ${prefix}/lib/
#   datadir to ${prefix}/share
# Otherwise, we take the user provided values.
#
# In any case, we will place our files in the stklos/${VERSION} subdir

if test "$LIBDIR" == ""  ; then
   # user has not given a "--libdir" option. Use ${prefix}/lib/stklos/${VERSION}
   libdir="${prefix}/lib/${PACKAGE_TARNAME}/${VERSION}"
fi

if test "$DATADIR" == ""  ; then
   # user has not given a "--datadir" option. Use ${prefix}/share/stklos/${VERSION}
   datadir="${prefix}/share/${PACKAGE_TARNAME}/${VERSION}"
fi

### Initialize the extra conf file in src
PREFIX=${prefix}
SCMDIR=${datadir}
EXECDIR=${libdir}
INCLUDEDIR=${includedir}


### Determine the kind of the machine which is running this script
BUILD_OS=$os-$version
BUILD_ARCH=`uname -m`
OS_NAME=$OS
case $OS in
   LINUX)
      v=`echo $version | sed -e 's/^\([[0-9]]*\)\.\([[0-9\]]*\).*/\1_\2/'`
      ;;
   DARWIN)
      v=`echo $version | sed -e 's/^\([[0-9]]*\).*/\1/'`
      ;;
  *)
      v=`echo $version | sed -e 's/[[^0-9]]/_/g'`
esac
OS_NAME_VERSION=${OS}_$v;


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
### To thread or not to thread?
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

# Try to determine if libgc is installed
if ${PKGCONFIG} --exists bdw-gc ;then
   HAVE_GC="yes"
else
   AC_CHECK_LIB(gc, GC_malloc, HAVE_GC="yes", HAVE_GC="no", $LIBS)
fi

# Test if the user wants to force the use of our GC lib
AC_ARG_WITH(provided-gc,
       [  --with-provided-gc      use the provided Boehm GC library],
       PROV_GC="$withval", PROV_GC="no")

if test "$HAVE_GC" = "no" -o "$PROV_GC" = "yes"
then
     echo "... Configuring libgc"
     (cd gc; ./configure --prefix=$prefix --enable-static \
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
if ${PKGCONFIG} --exists gmp ;then
   HAVE_GMP="yes"
   GMPKG="present"
else
   # check for GMP pre-version 3.0
   AC_CHECK_LIB(gmp, mpz_init_set, HAVE_GMP="yes", HAVE_GMP="no")
   # check for GMP version post 3.0
   if test "$HAVE_GMP" = "no" ;then
      AC_CHECK_LIB(gmp, __gmpz_init_set, HAVE_GMP="yes", HAVE_GMP="no")
   fi
fi

# Test if the user wants to force the use of our GMP lib
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
     (cd gmp; CC=${CC} CFLAGS="${CFLAGS} -I../src -DFOR_STKLOS=1" ./configure \
              --prefix=$prefix) || { echo "Cannot configure the GMP"; exit; }
     GMP=gmp
     GMPINC='-I$(BASEDIR)/gmp'
     GMPLIB="../gmp/.libs/libgmp.a"
     COMP_LIBS="libgmp $COMP_LIBS"
else
     echo "... Using system libgmp library"
     GMP=""
     if $PKGCONFIG --exists gmp ;then
        GMPINC="$($PKGCONFIG --cflags gmp)"
        GMPLIB="$($PKGCONFIG --libs gmp)"
     else
        GMPINC=""
        GMPLIB="-lgmp"
     fi
     DLLIBS="$DLLIBS $GMPLIB"
     AC_DEFINE(HAVE_GMP, 1, [Use system GMP])
     SYST_LIBS="libgmp $SYST_LIBS"
fi


# ----------------------------------------------------------------------
# PCRE2 support
# ----------------------------------------------------------------------
echo "*****"
echo "***** Regexp support"
echo "*****"

# Try to determine if libpcre2 is installed
if ${PKGCONFIG} --exists libpcre2-8 libpcre2-posix ;then
   HAVE_PCRE2="yes"
   PCRE2PKG="present"
else
    AC_CHECK_LIB(pcre2-8, pcre2_config_8, HAVE_PCRE2="yes", HAVE_PCRE2="no", $LIBS)
fi

# Test if the user wants to force the use of our PCRE2 lib
AC_ARG_WITH(provided-regexp,
       [  --with-provided-regexp  use the provided Regexp (PCRE2) library],
       PROV_PCRE2="$withval", PROV_PCRE2="no")

if test "$HAVE_PCRE2" = "no" -o "$PROV_PCRE2" = "yes"
then
     echo "... Configuring libpcre2"
     (cd pcre2; ./configure --prefix=$prefix --disable-shared --disable-cpp) || \
           { echo "Cannot configure PCRE2"; exit; }
     PCRE2=pcre2
     PCRE2INC="-I../pcre2/src"
     PCRE2LIB="../pcre2/.libs/libpcre2-posix.a ../pcre2/.libs/libpcre2-8.a"
     COMP_LIBS="libpcre2 $COMP_LIBS"
else
     echo "... Using system libpcre2 library"
     if $PKGCONFIG --exists libpcre2-8 libpcre2-posix ;then
        PCRE2INC="$($PKGCONFIG --cflags libpcre2-8 libpcre2-posix) "
        PCRE2LIB="$($PKGCONFIG --libs   libpcre2-8 libpcre2-posix)"
     else
        PCRE2INC=""
        PCRE2LIB="-lpcre-8 -lpcre2-posix"
     fi
     DLLIBS="$DLLIBS $PCRE2LIB"
     AC_DEFINE(HAVE_PCRE2, 1 , [We use our version of pcre])
     SYST_LIBS="libpcre2 $SYST_LIBS"
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

# Test if the user wants to force the use of our FFI lib
AC_ARG_WITH(provided-ffi,
       [  --with-provided-ffi     use the provided FFI library],
       PROV_FFI="$withval", PROV_FFI="no")

if test "$HAVE_FFI" = "no" -o "$PROV_FFI" = "yes"
then
     echo "... Configuring libffi"
     (cd ffi; ./configure --disable-structs --disable-raw-api --disable-shared \
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
AC_RUN_IFELSE([AC_LANG_SOURCE([[ int inner (char *foo) { char bar; return !(foo >= &bar); }
             int main (void) { char foo; return inner (&foo); } ]])],[STACK_DIRECTION="DOWN"],[STACK_DIRECTION="UP"],[echo "Stack direction is not detected when cross compiling for now"])



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
for arg in ${CC} ${LDLAGS} ${GMPLIB} ${PCRE2LIB} ${FFILIB} ${GCLIB} -lm
do
    C_LINK="${C_LINK}\\\"${arg}\\\" "
done

SH_LIB_COMPILE=""
SH_LIB_LINK=""

# stklos.shlib.compile property
inc=-I${prefix}/include/$PACKAGE
for arg in ${CC} ${SH_COMP_FLAGS} ${inc} ${inc}/gc
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

# See if we must add CC, {C,LD,CPP}FLAGS to the arguments stored in $CONF_OPT
if test "X$HAS_CC" = "X" ;then
   CONF_OPT="$CONF_OPT \\\"CC=$CC\\\""
fi

if test "X$HAS_CFLAGS" = "X" ;then
   CONF_OPT="$CONF_OPT \\\"CFLAGS=$CFLAGS\\\""
fi

if test "X$HAS_LDFLAGS" = "X" ;then
   CONF_OPT="$CONF_OPT \\\"LDFLAGS=$LDFLAGS\\\""
fi

if test "X$HAS_CFLAGS" = "X" ;then
   CONF_OPT="$CONF_OPT \\\"CPPFLAGS=$CPPFLAGS\\\""
fi

# Build the CONF_SUMMARY variable
confdirs="("
confdirs="${confdirs} #:libdir \\\"$libdir\\\" #:datadir \\\"$datadir\\\""
confdirs="${confdirs} #:docdir \\\"$docdir\\\" #:htmldir \\\"$htmldir\\\" #:pdfdir \\\"$pdfdir\\\""
confdirs="${confdirs})"

confsum="("
confsum="${confsum} #:system ($SYST_LIBS) #:compiled ($COMP_LIBS) #:configure ($CONF_OPT)"
confsum="${confsum} #:dirs ${confdirs}"
confsum="${confsum})"

CONF_SUMMARY="${confsum}"

# Determine the STRIP command to use
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

# Get configure/compilation directory
COMP_DIR=`pwd`


# Add information to VERSION
FULL_VERSION=$VERSION

if test $VERSION_STATUS = "unstable"
then
  # This code duplicates some code from "utils/generate-git-info"
  # but it makes things simpler
  git_version=`git --version 2>/dev/null || true`

  if test "$git_version" != ""
  then
    # Git is installed
    git_patch="`git describe --tags | sed -Ee 's/.*-(.*)-.*/\1/'`"
    if test "X$git_patch" != "X"
    then
      FULL_VERSION="$VERSION.$git_patch"
    fi
  else
    FULL_VERSION=$VERSION-unstable
  fi
fi




### Output files
AC_SUBST(STKCFLAGS)
AC_SUBST(VERSION)
AC_SUBST(FULL_VERSION)
AC_SUBST(VERSION_STATUS)
AC_SUBST(HAVE_DLOPEN)
AC_SUBST(OS_FLAVOUR)
AC_SUBST(UNAME_O)
AC_SUBST(GMP)
AC_SUBST(GMPLIB)
AC_SUBST(GMPINC)
AC_SUBST(PREFIX)
AC_SUBST(SCMDIR)
AC_SUBST(EXECDIR)
AC_SUBST(INCLUDEDIR)
AC_SUBST(BUILD_OS)
AC_SUBST(BUILD_ARCH)
AC_SUBST(OS_NAME)
AC_SUBST(OS_NAME_VERSION)
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
AC_SUBST(COMP_DIR)
#GC
AC_SUBST(GC)
AC_SUBST(GCLIB)
AC_SUBST(GCINC)
# PCRE2
AC_SUBST(PCRE2)
AC_SUBST(PCRE2LIB)
AC_SUBST(PCRE2INC)
AC_SUBST(PCRE2PKG)
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
          doc/pp/Makefile
          lib/Makefile utils/Makefile utils/stklos-config
          utils/stklos-script examples/Makefile
          lib/Match.d/Makefile lib/SILex.d/Makefile lib/Lalr.d/Makefile
          lib/scheme/Makefile
          lib/scheme/regex/Makefile lib/scheme/vector/Makefile
          lib/srfi/Makefile lib/stklos/Makefile lib/streams/Makefile
          tests/Makefile
          doc/stklos.1 doc/stklos-config.1 doc/stklos-compile.1
          doc/stklos-genlex.1 doc/stklos-pp.1 doc/stklos-script.1

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

# Add a src/git-config.h if we don't have one
(cd src; ../utils/generate-git-info)

chmod 0755 utils/stklos-config utils/stklos-script


echo " "
echo " "
echo "SUMMARY"
echo "*******"
echo "               System: " $BUILD_OS
echo "              OS nick: " $OS_NAME_VERSION
echo "              OS type: " $OS_FLAVOUR
echo "       Install prefix: " $prefix
echo "           C compiler: " $CC
echo "    Compilation flags: " $CFLAGS
echo "               Loader: " $LD
echo "       Thread support: " $THREADS
echo "     Case sensitivity: " $CASE_SENSITIVE '(by default)'
echo "Control fx parameters: " $CONTROL_FX
echo "System libraries used: " $SYST_LIBS
echo "   Compiled libraries: " $COMP_LIBS
echo " Documentation update: " $REPORT_ASCIIDOCTOR
echo " "

echo "If this is correct, you can just type 'make' now at your shell prompt."
echo "Otherwise, re-run 'configure' with correct options."
echo " "
