### configure.in for STklos
###
###           Author: Erick Gallesio [eg@unice.fr]
###    Creation date: 28-Dec-1999 21:19 (eg)
### Last file update: 26-Sep-2009 09:23 (eg)

AC_PREREQ(2.64)
AC_INIT([stklos], [0.99dev])
AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR(src/stklos.c)
AC_CONFIG_HEADERS(src/stklosconf.h)

### Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_AWK
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AC_PROG_LEX
AC_PROG_MKDIR_P

### Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h fcntl.h libintl.h limits.h malloc.h memory.h netdb.h netinet/in.h nlist.h stdlib.h string.h strings.h sys/param.h sys/socket.h sys/time.h syslog.h unistd.h])

### Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T

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
AC_CHECK_FUNCS([atexit dup2 endpwent floor getcwd gethostbyaddr gethostbyname gethostname getpass gettimeofday inet_ntoa isascii memchr memmove memset mkdir modf putenv rmdir select socket sqrt strcasecmp strchr strerror strrchr strstr strtoul])

### Initialize prefix
if test X$prefix = XNONE ;then  prefix=$ac_default_prefix; fi

### use -O3 if user has not specified something else and we are using GCC
USER_CFLAGS=$CFLAGS
if test "X$USER_CFLAGS" = "X" -a "$CC" = "gcc"
then
  CFLAGS="-O3 -fomit-frame-pointer"
fi


###
### Checks for libraries.
###

### Check for dynamic loading
AC_CHECK_FUNC(dlopen, HAVE_DLOPEN="yes", AC_CHECK_LIB(dl, dlopen, LIBS="-ldl $LIBS"&&HAVE_DLOPEN="yes"))

if test "X$HAVE_DLOPEN" = "Xyes"
then
  HAVE_DLOPEN="#define HAVE_DLOPEN 1"
fi


###
### To thread or not to thread ?
###
AC_CHECK_LIB(pthread, pthread_create, deflt_threads=pthreads, deflt_threads=none)

AC_ARG_ENABLE(threads,
  [AS_HELP_STRING([--enable-threads=TYPE],[choose threading package (none,pthread,lurc)])],
  THREADS=$enableval,
  THREADS=$deflt_threads)

AC_LANG(C)
### This checks for the presence of a given CPP define in a fiven header file
AC_DEFUN(GC_CHECK_DEFINE, [
 ### $1: header file
 ### $2: CPP variable
 ### $3: then
 ### $4: else
 AC_PREPROC_IFELSE([
 #include <$1>
 #ifndef $2
 # error "$2 undefined"
 #else
  ok
 #endif
 ],
 [$3], [$4])
])


### Now look at what threads we want
case "$THREADS" in
 no | none | single)
   THREADS=none
   CALLCC_STK='"callcc.stk"'
   AC_DEFINE(THREADS_NONE, [], [No thread support])
   ;;
 posix | pthreads)
   ### Check for pthreads
   AC_CHECK_LIB(pthread, pthread_create, LIBS="-lpthread $LIBS", 
                [AC_MSG_ERROR([Missing PThread library])])
   THREADS=pthreads
   CALLCC_STK='"callcc.stk"'
   AC_DEFINE(THREADS_PTHREADS, [], [PThread support])
   ;;
 lurc)
   ### Check for lurc
   AC_CHECK_LIB(lurc, lurc_thread_create, LIBS="-llurc $LIBS", 
                [AC_MSG_ERROR([Missing LURC library])])
   AC_CHECK_HEADER([lurc.h], [], [AC_MSG_ERROR([Missing LURC header file])])
   ### Now check if it is compiled with GC support
   AC_MSG_CHECKING([for LURC GC support])
   GC_CHECK_DEFINE([lurc.h], [LURC_ENABLE_GC], 
                   [AC_MSG_RESULT([ok])],
                   [AC_MSG_ERROR([Missing GC support in LURC])])
   ### That should be good
   AC_DEFINE(THREADS_LURC, [], [LURC threads support])
   CALLCC_STK='"dyn-wind-lurc.stk"'
   ;;
 *)
   AC_MSG_ERROR([Unknown thread system: $THREADS])
   ;;
esac

### Add the options _REENTRANT and GC_LINUX_THREADS if we are on Linux
case "$THREADS" in
  none) true ;;
  lurc) true ;;
  *) case `uname -s` in
        Linux*) STKCFLAGS="-D_REENTRANT=1 -DGC_LINUX_THREADS=1";;
     esac
esac



### Automake nightmare
AM_CONDITIONAL(PTHREADS, test "$THREADS" = pthreads)
AM_CONDITIONAL(LURC, test "$THREADS" = lurc)
AM_CONDITIONAL(NO_THREAD, test "$THREADS" = none)

### check for GMP pre-version 3.0
AC_CHECK_LIB(gmp, mpz_init_set, HAVE_GMP="yes", HAVE_GMP="no")

### check for GMP version post 3.0
if test "$HAVE_GMP" = "no" ;then
  AC_CHECK_LIB(gmp, __gmpz_init_set, HAVE_GMP="yes", HAVE_GMP="no")
fi


# Eventually force our gmp usage
withval=""
AC_ARG_WITH(gmp-light,
	[  --with-gmp-light		use our GMP version even the system provides one])

case $withval in
  "yes") HAVE_GMP="no";;
  "no")  HAVE_GMP="yes";;
esac



###
### check for GC
###
if test "$THREADS" = lurc
then
  AC_MSG_WARN([Using the bundled GC for LURC support])
  HAVE_GC="no"
else
  AC_CHECK_LIB(gc, GC_malloc, HAVE_GC="yes", HAVE_GC="no", $LIBS)
fi

# Test if the user want to force with our GC
withval=""
AC_ARG_WITH(provided-gc,
       [  --with-provided-gc		use our GC version even the system provides one])

case $withval in
  "yes") HAVE_GC="no";;
  "no")  HAVE_GC="yes";;
esac

# On Mac OS X use the given GC (to compile it without -Ox)
case `uname -s` in
  Darwin*) echo "Force '--with-provided-gc' on MacOS";
           HAVE_GC="no" ;;
  *) true;;
esac


###
### check for PCRE
###
AC_CHECK_LIB(pcre, pcre_fullinfo, HAVE_PCRE="yes", HAVE_PCRE="no", $LIBS)

# Test if the user want to force with our PCRE lib
withval=""
AC_ARG_WITH(provided-regexp,
       [  --with-provided-regexp	use our PCRE regexp version])

case $withval in
  "yes") HAVE_PCRE="no";;
  "no")  HAVE_PCRE="yes";;
esac

###
### configure libraries
###

if test "$HAVE_GMP" = "no"
then 
  echo "No GMP library. Using the one provided with the package"
  cmd="CC=\"$CC\" CFLAGS=\"$CFLAGS\" ./configure --prefix=$prefix"
  echo "Configuring GMP"	
  echo $cmd		
  (cd gmp; eval $cmd)
  GMP=gmp
  GMPLIB="../gmp/libgmp.a"
  GMPINC="-I../gmp"
else
  echo "Using the system GMP library"
  GMP=""
  GMPLIB="-lgmp"
  GMPINC=""
fi

if test "$HAVE_GC" = "no"
then
  echo "No Boehm-Demers-Weiser GC library. Using the one provided with the package"
  (cd gc; CC="$CC" CFLAGS="$CFLAGS" sh ./configure --prefix=$prefix \
          --includedir=$prefix/include/stklos \
          --enable-threads=$THREADS) || { echo "Cannot configure the GC"; exit; }
  
  GC=gc
  GCLIB="../gc/.libs/libgc.a"
  GCINC="-I../gc/include"
  # Eventually add a distclean target to the GC Makefile
else
  echo "Using the system Boehm-Demers-Weiser library"
  GC=""
  GCLIB="-lgc"
  GCINC=""
  AC_DEFINE(HAVE_GC, [], [Use system GC])
fi

if test "$HAVE_PCRE" = "no"
then 
   echo "No PCRE library. Using the one provided with the package"
   PCRE_FLAGS="-Dregexec=PCRE_regexec -Dregcomp=PCRE_regcomp \
   -Dregerror=PCRE_regerror -Dregfree=PCRE_regfree"

   (cd pcre; CC=$CC CFLAGS="$CFLAGS $PCRE_FLAGS" sh ./configure \
    --prefix=$prefix --disable-shared --disable-cpp)
    PCRE=pcre
    PCRELIB="../pcre/.libs/libpcreposix.a ../pcre/.libs/libpcre.a"
    PCREINC="-I../pcre"
    DEF_PCRE=""
else 
      echo "Using the system PCRE library"
     PCRE=""
     PCRELIB="-lpcre -lpcreposix"
     PCREINC=""
     DEF_PCRE="#define HAVE_PCRE 1"
fi


#PCRELIBS="../pcre/.libs/libpcreposix.a ../pcre/.libs/libpcre.a"
#
# case `uname -s` in
#   CYGWIN*) PCRETARGET="libpcre.la";;
#   *)       PCRETARGET="libpcre.la libpcreposix.la";;
# esac


#
# FFI support 
#
AC_ARG_ENABLE(ffi, 
		[  --disable-ffi			disable FFI support],
		USER_FFI="no", 
		USER_FFI="yes")

if test "$USER_FFI" = "yes" ;then
  echo "Configuring libffi"
  (cd ffi; CC=${CC} CFLAGS=${CFLAGS} ./configure \
           --disable-structs --disable-raw-api --disable-shared \
	   --prefix=$prefix)
  LIBS="$LIBS ../ffi/.libs/libffi.a"
  CFLAGS="$CFLAGS -I../ffi/include"
  LIBFFI="ffi"
  HAVE_FFI="#define HAVE_FFI 1"
  FFI="yes"
else
  echo "Disabling FFI support"
  LIBFFI=""
  HAVE_FFI=""
  FFI="no"
fi


### ======================================================================
### Various stuff done by hand
### ======================================================================


### Determine the options necessary for dynamic loading
eval `CC=$CC /bin/sh ./utils/shlib-options`

case $OS in
  SUNOS) 
    CFLAGS="$CFLAGS -DSOLARIS";
    LIBS="$LIBS -lsocket -lrt -lnsl -lpthread" ;;
  UNKNOWN)
    echo ""
    echo "**** STklos has no support for your system (`uname -s`)"
    echo "**** Please modify the script \"utils/shlib-options\" to add"
    echo "**** support for your system (it's easy) and restart installation process."
    echo "**** Thanks for sending your modifications to <bugs _at_ stklos.net>"
    exit 1 ;;
esac


### Initilize the extra conf file in src
SCMDIR=$prefix/share/$PACKAGE/$VERSION
EXECDIR=$prefix/lib/$PACKAGE/$VERSION
PREFIX=$prefix

### Determine the kind of the machine which is running this script
BUILD_OS=$os-$version
BUILD_ARCH=`uname -m`
OS_NAME=$OS

### Place version number in VERSION file
### echo "VERSION=${VERSION}">VERSION

#1.0 ### GTK+/GNOME support
#1.0 HAVE_GNOME="undef"
#1.0 
#1.0 if test "$USE_GTK" = "no" ;then
#1.0    USE_GNOME="no"
#1.0 fi
#1.0 
#1.0 
#1.0 if test "$USE_GTK" = "yes"
#1.0 then
#1.0   GTKLOS=gtklos	
#1.0   EXAMPLES="examples examples/images"
#1.0 
#1.0   if test "$USE_GNOME" = "yes"
#1.0   then
#1.0     HAVE_GNOME="define"
#1.0     GTK_CONFIG_LIBS=`$GNOME_CONFIG --libs  gnomecanvaspixbuf`
#1.0     GTK_CONFIG_CFLAGS=`$GNOME_CONFIG --cflags gnomeui gnomecanvaspixbuf`
#1.0   else
#1.0     GTK_CONFIG_LIBS=`$GTK_CONFIG --libs`
#1.0     GTK_CONFIG_CFLAGS=`$GTK_CONFIG --cflags`
#1.0   fi
#1.0 else
#1.0   GTKLOS=
#1.0   EXAMPLES=
#1.0   GTK_CONFIG_LIBS=
#1.0   GTK_CONFIG_CFLAGS=
#1.0 fi

###
### display some informations for the user
### 

if test "$HAVE_GMP" = "no"
then
  biglib="compile provided library (better perf. can be achieved with a recent GMP)"
else 
  biglib="use the system library"
fi

if test "$HAVE_GC" = "no"
then
  gclib="compile provided library"
else 
  gclib="use the system library"
fi

if test "$HAVE_PCRE" = "no"
then
  pcrelib="compile provided library"
else
  pcrelib="use the system library"
fi

#1.0 # Compute SVN_URL 
#1.0 SVN_URL=$SVN_REPOSITORY
#1.0 if test "$SVN_REPOSITORY" = "" ;then
#1.0    SVN_URL="NOT_AVAILABLE"
#1.0 fi

# LURC adjustment
LURCDIR=""
if test "$THREADS" = "lurc" ;then
   LURCDIR=Lurc.d
fi



### Output files 
AC_SUBST(STKCFLAGS)
AC_SUBST(VERSION)
AC_SUBST(HAVE_DLOPEN)
AC_SUBST(HAVE_FFI)
AC_SUBST(OS_FLAVOUR)
AC_SUBST(GC)
AC_SUBST(GCLIB)
AC_SUBST(GCINC)
AC_SUBST(GMP)
AC_SUBST(GMPLIB)
AC_SUBST(GMPINC)
AC_SUBST(PREFIX)
AC_SUBST(SCMDIR)
AC_SUBST(EXECDIR)
AC_SUBST(BUILD_OS)
AC_SUBST(BUILD_ARCH)
AC_SUBST(OS_NAME)
AC_SUBST(SH_COMP_FLAGS)
AC_SUBST(SH_LOAD_FLAGS)
AC_SUBST(SH_LOADER)
AC_SUBST(SH_SUFFIX)
AC_SUBST(SH_MAIN_LOAD_FLAGS)
AC_SUBST(COMPOBJ)
AC_SUBST(COMPSRC)
AC_SUBST(STACK_DIRECTION)
#1.0 AC_SUBST(GTKLOS)
#1.0 AC_SUBST(GTK_CONFIG_LIBS)
#1.0 AC_SUBST(GTK_CONFIG_CFLAGS)
#1.0 AC_SUBST(EXAMPLES)
AC_SUBST(HAVE_GNOME)
#1.0 AC_SUBST(HAVE_LDAP)
AC_SUBST(LDAP_LIB)
AC_SUBST(PCRETARGET)
AC_SUBST(PCRE)
AC_SUBST(PCRELIB)
AC_SUBST(PCREINC)
AC_SUBST(DEF_PCRE)
#1.0 AC_SUBST(SVN_URL)
AC_SUBST(THREADS)
AC_SUBST(LURCDIR)
AC_SUBST(CALLCC_STK)
AC_SUBST(LIBFFI)
	
AC_CONFIG_FILES([Makefile src/Makefile src/extraconf.h doc/Makefile
          lib/Makefile utils/Makefile lib/boot-callcc.stk
	  utils/stklos-config utils/stklos-script
	  examples/Makefile lib/Match.d/Makefile 
	  lib/SILex.d/Makefile lib/Lalr.d/Makefile lib/Lurc.d/Makefile
	  lib/ScmPkg.d/Makefile tests/Makefile  
	  doc/stklos.1 doc/stklos-config.1 doc/stklos-compile.1 
	  doc/stklos-genlex.1 doc/stklos-install.1 doc/stklos-pkg.1 
	  doc/skb/stklos-version.stk
	  pkgman/Makefile])
AC_OUTPUT

chmod 0755 utils/stklos-config utils/stklos-script

echo " "
echo " "
echo "SUMMARY"
echo "*******"
echo "            System: " $BUILD_OS
echo "           OS type: " $OS_FLAVOUR
echo "   Install prefix : " $prefix
echo "        C compiler: " $CC
echo " Compilation flags: " $CFLAGS
echo "    Bignum library: " $biglib
echo "    Regexp library: " $pcrelib
echo "        GC library: " $gclib
#1.0 echo "      LDAP support: " $USE_LDAP
#1.0 echo "      GTK+ support: " $USE_GTK 
#1.0 echo "     GNOME support: " $USE_GNOME 
echo "       FFI support: " $FFI
echo "    Thread support: " $THREADS 
echo " "
if test "$THREADS" = "lurc" 
then 
     echo "*** WARNING: To use LURC threads, you need to replace (or install) "
     echo "*** a special GC package (available at http://www.stklos.net/download)."
     echo "*** You have to: "
     echo "***    - Remove the current gc directory"
     echo "***    - untar this package in this directory."
     echo "***    - re-run configure"
     echo "*** The GC for LURC probably doesn't work on MacOS 10.5"
fi
echo "If this is correct, you can just type 'make' now at your shell prompt."
echo "Otherwise, re-run 'configure' with correct options." 
echo " "

