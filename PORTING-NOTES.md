Porting Notes
=============

Most of the time, Version 1.4X can be compiled with the standard invocation trio

```bash
configure && make && sudo make install
```

For more information on the way to build STklos from the source distribution,
you can look at https://stklos.bet/documentation.html.


Main Linux distributions
------------------------

STklos is developed on a Linux distribution (Arch) and compiling it on any
general purpose Linux distribution should be easy.

You can compile STklos with `gcc` and `clang`. It is known to work on 32 and
64 bits architectures.

### x86_64 platform

STklos has been successfully built on the following distributions:
- Arch Linux (64 bits gcc/clang, kernel 4.4 → 5.7)
- Debian 10 (Buster) (64 bits gcc, kernel 4.19)

### ARM architecture

STklos has been successfully built on the following distribution:
- Raspbian 8.3.0 on a Raspberry Pi 2 (ARM v7l) (32 bits gcc, kernel 4.19)

Apple MacOs
-----------

STklos can be compiled on MacOs. By default, some of the needed libraries for
STklos construction are available, but without the associated header
files. This can mislead the `configure` script. In this case, you can
configure STklos by forcing it to use the embedded libraries. For instance, to
compile STklos on macOS 10.14 Mojave (YMMV):

```bash
configure --with-provided-ffi --with-provided-regexp && make all tests && sudo make install
```

STklos has been successfully built on the following distribution:

-  macOS 10.14 Mojave (clang 64 bits)


Microsoft Windows
-----------------

STklos used to be compiled on Cygwin, but it is easier now to built it using
WSL. It has been been successfully built on the following distribution:

- Windows 10 running WSL (version 1) on an Ubuntu 16.04 LTS — Xenial Xerus
  (64 bits gcc, kernel 4.4)


========================================================================

The following text is kept here for historical purpose.
You should not need the information it contains.

=======================================================================

(Obsolete) Information for version 1.x
======================================


Version 1.x is known to compile with the standard invocation
     configure && make && sudo make install
on the following architectures

    - Linux 2.6    (gcc-4.x 32 and 64 bits)
    - MacOS 10.5+  (gcc-4.x 32 and 64 bits)
    - FreeBSD 8.x  (gcc-4.x 32 bits)
    - Win32        (cygwin 1.7.x)

MacOs X Lion

   To compile stklos on MacOs X Lion, the following
   configuration line is known to work:

   $ CFLAGS="-Wl,-no_pie,-no_compact_unwind" ./configure \
                 --with-provided-gc --with-provided-regexp \
         --with-provided-ffi --with-provided-bignum

Win32

    The Cygwin version of Boehm GC doesn't work with STklos. Consequently,


on.  the following architectures

    - Linux 2.6    (gcc-4.x 32 and 64 bits)
    - MacOS 10.5+  (gcc-4.x 32 and 64 bits)
    - FreeBSD 8.x  (gcc-4.x 32 bits)
    - Win32        (cygwin 1.7.x)

MacOs X Lion

   To compile stklos on MacOs X Lion, the following
   configuration line is known to work:

   $ CFLAGS="-Wl,-no_pie,-no_compact_unwind" ./configure \
                 --with-provided-gc --with-provided-regexp \
         --with-provided-ffi --with-provided-bignum

Win32

    The Cygwin version of Boehm GC doesn't work with STklos. Consequently,
    you need at least a configuration such as

    $ ./configure --with-provided-gc

    to have a working version of STklos.
    Note that the current version is a rather direct port of STklos and
    has rough edges (in particular thers is no real support for the DOS
    drives and the functions using pathnames use a Unixy syntax, rather
    than the more conventional DOS one). Dynamic loading works, but GTK
    support has not been tested.


======================================================================

(More Obsolete) Information for pre-version 1.x
===============================================

All the information given below are for old versions (pre-1.0) of
STklos. They are here for the record.


This file contains a set of informations for compiling STklos on
various architectures.

If you experience a new port or confirm/infirm/add informations which
are specified here please send a mail to Erick Gallesio
'eg(at)unice.fr'

For each entry below, you'll find the OS and the version of the C
compiler used for building the system. The entry Gtklos, tells if the
GTklos has been built on this architecture. Canvases are set apart
because, they are implemented in Gnome rather than GTk. Since Gnome,
is a large library, it is likely that it is not present on a given
box.



GNU/Linux (x86) ------------------------
----------------------------------------

     OS Version: Any "recent" version of the OS
       Compiler: gcc-2.95 or gcc-3.x
    Compilation: CC=gcc
             CFLAGS=-03 -fomit-frame-pointer
            Who: Erick Gallesio  'eg(at)essi.fr'
     GTklos: Yes
     Canvas: Yes
    Remarks: This is the STklos main development machine.


GNU/Linux (alpha) ----------------------
----------------------------------------

     OS Version: Microway Linux release 6.2 (redhat based)
       Compiler: egcs-2.91
    Compilation: CC=gcc
                 CFLAGS=-O2 --disable-gtklos
            Who: Erick Gallesio  'eg(at)essi.fr'
     GTklos: Yes
     Canvas: No (see below)
    Remarks: I was not able to test canvases, because the machine I
         on which I have tested use an old distribution.

GNU/Linux (itanium) --------------------
----------------------------------------

     OS Version: Red Hat Ent Linux AS 2.1
       Compiler: gcc-3.2
    Compilation: CC=gcc
            Who: Erick Gallesio  'eg(at)essi.fr'
     GTklos: Yes
     Canvas: Yes
    Remarks: Thanks to HP Test Drive Program for providing the machine

FreeBSD (x86) -------------------------
----------------------------------------

     OS Version: version 4.2
       Compiler: gcc
    Compilation:
             CC=gcc
             CFLAGS=-O2
     GTklos: No (see below)
     Canvas: No (see below)
            Who: Erick Gallesio  'eg(at)essi.fr'
        Remarks: I have no more access to such a machine. Last version
         tested was STklos v0.51. GTklos and Canvas where not
         tested because the machine didn't have X11 installed.

         A more recent log (04/08/03) of a port done by Kimura
         Fuyuki  is available at
         http://bento.freebsd.org/errorlogs/i386-5-full-logs/stklos-0.54.log
         Port works on x86 only.

Win32/Cygwin ---------------------------
----------------------------------------

     OS Version: Win32 (XP pro)
       Compiler: gcc 3.2.1
    Compilation: CC=gcc
             CFLAGS=-O2 --disable-gtk --disable-gnome
     GTklos: No (see below)
     Canvas: No (see below)
            Who: Erick Gallesio  'eg(at)essi.fr'
        Remarks: I have not even tried to compile GTklos on this
         OS because I don't know how to use the GTk on Win32
         and I'm not really interested to do it.
         Any taker?

Apple OS-X -----------------------------
----------------------------------------

     OS Version: Darwin Kernel Version 6.3
       Compiler: Apple Computer, Inc. GCC version 1161, based on gcc 3.1
    Compilation: CC=gcc
             CFLAGS=-O2 --disable-gnome
     GTklos: Yes
     Canvas: No (see below)
            Who: Erick Gallesio  'eg(at)essi.fr'
        Remarks: Gnome didn't work on the platform I have used, so I
         was not able to test, but since (nearly) everything
         went smoothly on this OS, it should work.


Sun Solaris ----------------------------
----------------------------------------

     OS Version: Solaris 6
       Compiler: gcc 3.0.1
    Compilation: CC=gcc
             CFLAGS=-O2 --disable-gnome --disable-gtk
     GTklos: No (libs not present)
     Canvas: No (libs not present)
            Who: Erick Gallesio  'eg(at)essi.fr'
        Remarks: I had problems with gmp, which did not work in
         generic mode. I had to configure STklos and then
         I went in the gmp directory and did
             configure --disable-shared --target=sparc64
         and then, you can go back in the main directory and
         do a standard "make" and "make install"


SGI Irix -- ----------------------------
----------------------------------------

     OS Version: Irix 6.5.20f (Octane, MIPS R10,000)
       Compiler: gcc 3.3
    Compilation: CC=gcc
             CFLAGS=-O2 --disable-gnome
     GTklos: Yes
     Canvas: No (libs not present)
            Who: Joshua Boyd jdboyd(at)jdboyd.net
        Remarks: Some problems with the provided GMP, but it works
             with the one available from SGI

Open Darwin  (PPC) ---------------------
----------------------------------------

     OS Version: MacOS-X 10.3/OpenDarwin 7.1/Darwin 7.0
       Compiler: gcc 3.3
    Compilation: --disable-gnome --disable-gtk
     GTklos: No
     Canvas: No
            Who: Markus W.Weissmann (mww (at) opendarwin.org)
        Remarks: The portfile is available from
                 http://darwinports.opendarwin.org/
