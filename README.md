STklos   (version 1.3x)
=======================

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
 - easily extensible with its ScmPkg mechanism,
 - it implements properly tail recursive calls.

History
-------

STklos is the successor of STk, a Scheme interpreter tightly connected
to the Tk toolkit, that I have developed since September 1993.

STklos is very similar to STk, for the user, but very different in he
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

**March 2018:**
Development of STklos was on hold for several years, due to heavy
administrative tasks in my University. However, Ι still use it for
personal scripts and, since Ι was asked for, the source tree is now
publicly available. The DVCS used is now **git** instead of mercurial.

**July 2019**
The 1.3x versions try to be more R7RS compliant. All the
R5RS functions which have been extended in R7RS are now conform to
R7RS (for instance `assoc` and `member` accept now an optional
parameter which is a compare function, vector->list accepts the
`start` and `end` parameters, ...)

SRFI Support
------------

Current version of STklos supports some SRFIs (a complete list of
implemented SRFIs is available in the file SUPPORTED-SRFIS in the
distribution). The goal is to support as much as possible final
SRFIs. Any help on implementing other SRFIs on STklos is welcome.

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

Project Home
------------

STklos home page is is located at http://stklos.net

