STklos (version 2.00)
=====================


**Erick Gallesio** <eg(at)stklos.net>


Purpose
-------
*STklos*** (pronounced [/ˈɛs.'ti.kl'ɔss/](https://itinerarium.github.io/phoneme-synthesis/?w=ˈɛs.'ti.kl'ɔss)) is a free [Scheme](http://www.schemers.org) system mostly compliant
with the language features defined in [R⁷RS small](http://www.scheme-reports.org/2015/working-group-1.html). The aim of this implementation
is to be fast as well as light. The implementation is based on an ad-hoc
*Virtual Machine*.


<!-- ***STklos*** can also be compiled as a library and embedded
in an application. -->

The salient points of ***STklos*** are:

-   an efficient and powerful object system based on [CLOS](https://en.wikipedia.org/wiki/Common_Lisp_Object_System) (Common Lisp Object System) providing
    -   Multiple Inheritance,
    -   Generic Functions,
    -   Multi-methods
    -   an efficient MOP (Meta Object Protocol)
-   a simple to use module system, as well as R⁷RS libraries
-   a full [Numerical tower](https://en.wikipedia.org/wiki/Numerical_tower), as defined in *R⁷RS*,
-   a simple [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface) (Foreign Function Interface),
-   an easy connection to the [GTK+](http://www.gtk.org) toolkit,
-   a Perl compatible regular expressions thanks to the [PCRE](http://www.pcre.org) package,
-   [Unicode](https://en.wikipedia.org/wiki/Unicode) support,
-   native threads built on top of [Posix theads](https://en.wikipedia.org/w/index.php?title=Pthreads),
-   [Tail Call Optimization](https://en.wikipedia.org/w/index.php?title=Tail_call), as required by R⁷RS,
-   support for a large number (> 120) of [SRFIs](https://en.wikipedia.org/wiki/Scheme_Requests_for_Implementation).


History
-------

STklos is the successor of STk, a Scheme interpreter tightly connected
to the Tk toolkit, that I have developed since September 1993.

STklos is very similar to STk for the user but very different in the
implementation. Everything has been rewritten and STklos now uses a
compiler to a byte-code machine rather than direct interpretation.
As a consequence, it is a lot faster than STk.

<!-- STklos is also better
designed and should be easily embedded in an application. (This was not
completely the case with STk.) -->

Whereas STk used the Tk toolkit, STklos uses the GTK+ graphical
toolkit. STklos is not completely compatible with STk but it should be
easy to port old STk graphical applications (provided that they use
objects). The GTK+ extension can be loaded at run time and does not
require a special interpreter as was the case with STk.

**August 2020**
The 1.50 version has been ported to various architectures (in
particular to "small" 32-bit little- and big-endian systems)
and a port to Android.  This version also brings support
of 23 new SRFIs. For a complete list of supported SRFIs, see
https://www.stklos.net/srfi.html

**February 2021**
Support for 16 new [**SRFIs**](https://www.stklos.net/srfi.html).

**November 2021**
The 1.70 version supports 14 new [**SRFIs**](https://www.stklos.net/srfi.html).
(We now have 98 SRFIs supported). The (new) `extensions` directory contains
an OO binding to the [GTK+3](https://docs.gtk.org/gtk3/) toolkit.

**August 2023**
The 2.00 version is available. This version is conforming to R^7^RS
(previous versions of STklos used to implement the R^5^RS standard).
It adds support for 24 new [**SRFIs**](https://www.stklos.net/srfi.html) giving a complete
support for 122 SRFIs. You can now use the [cURL](https://curl.se/libcurl/)
library thanks to the new **curl** extension.

SRFI Support
------------

The current version of STklos supports mre than 120 final SRFIs (a
complete list of implemented SRFIs is available in the file
SUPPORTED-SRFIS in the distribution). The goal is to support as many
final SRFIs as possible.  Any help on implementing other SRFIs on
STklos is welcome.

IDEs
----

There are two recommended IDEs that can be used with STklos:

* Quack by Neil van Dyke, which still works fine, but it's not being updated:
  https://www.neilvandyke.org/quack/

* Geiser:
  http://www.nongnu.org/geiser/
  The STklos support package can be installed from MELPA: the package
  name is `geiser-stklos`.
  The source code is here: https://gitlab.com/emacs-geiser/stklos/

Supported architectures
-----------------------

The current version of STklos works on:

- GNU/Linux 4.x and 5.0 (64 bits).
- macOS 10.13 High Sierra
- Win32 (using WSL - Windows Subsystem for Linux)


The previous version of STklos (1.10) was known to work on the following
architectures:

- GNU/Linux 1.6.x (i386/gcc 32 and 64 bits)
- Mac OS X 10.5+ (i386/gcc 32 and 64 bits)
- FreeBSD 8.x (i386/gcc 32 bits and 64 bits)
- Win32 (using Cygwin 1.7.x)

Very old versions of STklos have run on various architectures. All
these ports are pretty old now, but if a port worked a day on a given
architecture, it should be not too hard to port recent versions of
STklos to it. The old ports were effective on:

- Win-NT/Win-XP (i386/gcc(Cygwin))
- GNU/Linux >= 2.2 (i386/gcc and alpha/gcc)
- FreeBSD 4.2 (i386/gcc)
- Mac OS X (ppc & Intel/gcc)
- Solaris 8 (ultrasparc/gcc)
- Irix 6.5.20 (Octane, mips R10,000)
- Open Darwin 7.0 (PPC/gcc)

More information on supported platforms is available in the
PORTING-NOTES file in the distribution.

STklos Installation
-------------------

See the file ./QUICK-INSTALL.md for instructions on how to build STklos. A
more complete document is available in ./INSTALL.

If you want to build or run a Docker image of STklos, you can find some information
in the ./etc/Docker/README.md

Project Home
------------

The STklos home page is located at http://stklos.net
