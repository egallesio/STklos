STklos (version 1.70)
=====================


**Erick Gallesio** <eg(at)unice.fr>


Purpose
-------

STklos is a free Scheme system compliant with the languages features
defined in R7RS. The aim of this implementation is to be fast as well
as light. The implementation is based on an ad-hoc Virtual
Machine. STklos can also be compiled as a library and embedded in an
application.

The salient points of STklos are:

 - an efficient and powerful object system based on CLOS providing
    - Multiple Inheritance,
    - Generic Functions,
    - Multi-methods
    - an efficient MOP (Meta Object Protocol)
 - a simple to use module system
 - a full tower of numbers implementation, as defined in R5RS,
 - easy connection to the GTK+ toolkit,
 - a Perl compatible regular expressions thanks to the PCRE package,
 - it implements properly tail recursive calls.

History
-------

STklos is the successor of STk, a Scheme interpreter tightly connected
to the Tk toolkit, that I have developed since September 1993.

STklos is very similar to STk, for the user, but very different in the
implementation. Everything has been rewritten and STklos uses now a
compiler to a byte-code machine, rather than direct interpretation.
As a consequence, it is a lot faster than STk.  STklos is also better
designed and should be easily embedded in an application (it was not
completely the case with STk).

Whereas STk used the Tk toolkit, STklos uses the GTK+ graphical
toolkit. STklos is not completely compatible with STk but it should be
easy to port old STk graphical applications (provided that they use
objects). The GTK+ extension can be loaded at run time and does not
require a special interpreter as it was the case with STk.

**June 2020**
The 1.40 version continues to add R7RS traits to STklos.

**August 2020**
The 1.50 version has been ported on various architectures (in
particular on `small" systems running on 32 bits little as well as big
endian) and a port on Android.  This version brings also the support
of 23 new SRFIs. For a complete list of supported SRFIs, see
https://www.stklos.net/srfi.html


**February 2021**
Support for 16 new [**SRFIs**](https://www.stklos.net/srfi.html).

**November 2021**
The 1.70 version supports 14 new [**SRFIs**](https://www.stklos.net/srfi.html)
(we have now 98 SRFIs supported). The (new) `extensions` directory contains 
an OO binding to the [GTK+3](https://docs.gtk.org/gtk3/) toolkit. 


SRFI Support
------------

Current version of STklos supports several SRFIs (a complete list of
implemented SRFIs is available in the file SUPPORTED-SRFIS in the
distribution). The goal is to support as much as possible final
SRFIs. Any help on implementing other SRFIs on STklos is welcome.

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
----------------------

STklos current version works on
- GNU/Linux 4.x and 5.0  (64 bits).
- Mac OS X 10.13 High Sierra
- Win32 (using WSL - Windows Subsystem for Linux)


The previous version of STklos (1.10) was known to work on the following
architectures:

- GNU/Linux 1.6.x (i386/gcc 32 and 64bits)
- Mac OS X 10.5+ (i386/gcc 32 and 64 bits)
- FreeBSD 8.x (i386/gcc 32 bits and 64 bits)
- Win32 (using Cygwin 1.7.x)

Very old versions of STklos have run on various architectures. All
these ports are pretty old now, but if a port worked a day on a given
architecture, it should be not too hard to port recent version of
STklos on it. The old ports were effective on:

- Win-NT/Win-XP (i386/gcc(Cygwin))
- GNU/Linux >= 2.2 (i386/gcc and alpha/gcc)
- FreeBSD 4.2 (i386/gcc)
- Mac OS X (ppc & Intel/gcc)
- Solaris 8 (ultrasparc/gcc)
- Irix 6.5.20 (Octane, mips R10,000)
- Open Darwin 7.0 (PPC/gcc)

More informations on supported platforms is available in the
PORTING-NOTES file in the distribution.

STklos Installation
-------------------

See the file ./QUICK-INSTALL.md for instructions on how to build STklos (a
more complete document is available in ./INSTALL

If you want to build or run a Docker image of STklos, you can find some information
in the ./etc/Docker/README.md

Project Home
------------

STklos home page is is located at http://stklos.net

License
-------

STklos: Copyright (C) Erick Gallesio. Most STklos source files are distributed under the GNU General Public License version 2 or any later version (SPDX GPL-2.0-or-later); some are distributed under different licenses -- please see each file for its specific license.

STklos documentation: Copyright (C) Erick Gallesio. The documentation is distributed under the GNU Free Documentation License version 1.3 or any later version (SPDX GFDL-1.3-or-later).

HACKING amd vm.md: Copyright (C) 2020 Jerônimo Pellegrini. These documents are distributed under the GNU Free Documentation License version 1.3 or any later version (SPDX GFDL-1.3-or-later).

