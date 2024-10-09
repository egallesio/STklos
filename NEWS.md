NEWS
====
## Version 2.10

This version of ***STklos*** mostly enhances the 2.00 version released a year ago. As usual, this version could not have be finalized without the help of Jeronimo Pellegrini ([@jpellegrini](https://github.com/jpellegrini)).

**Contributors for this version**:

- Akinori Hattori ([@hattya](https://github.com/hattya))
- Jeronimo Pellegrini ([@jpellegrini](https://github.com/jpellegrini))


**Enhancements:**

- Corrected the result of `stklos-config --compile`
- Added the second (optional) parameter to R7RS `load` (and `try-load`)
- Enhanced various things about ***STklos*** compilation/installation
- Added information in the summary of configuration script
- Builds are now reproducible
- FFI support can be completely disabled, if needed.
- Updated GTklos, the  GTK+ extension
- Optimize `expt` primitive.
- Avoid unneeded boxing of numbers in the `+`, `-`, `*` and `/` operations
- Added `!` as a shell escape under the REPL
- Added statistics to the VM
- Permit to build ***STklos*** with the `tcc` C compiler.
- Added an simple hybrid (C and Scheme) module in the examples directory
- Correct/enhance  implementation of numerical functions in some corner cases (in particular with NaNs and infinities).
- Added an API to add expressions rewriting rules before their compilation.
- Location given in error message is more precise
- Error messages don't loop anymore when displaying circular structures
- Added `list-set!` as the setter function of `list-ref`
- Added the number of allocations and collections to the output of the `time` form
- Added special instructions in the VM for `member`, `memq` and `memv`
- Added special instructions in the VM for `assoc`, `assq` and `assv`
- Do constant folding before in-lining usual functions
- Better UTF-8 support and updated Unicode tables to version 16.0.0
- Enhance the `apropos` primitive
- New REPL variables: `@*`, `@1`, `@2`, `@3`, `@4` and `@5`
- New REPL commands: `time`, `describe`, `expand`, `import`, `require-feature`, 
  `open`, `browse`, `manual`, `apropos`
- Documentation of a function can be accessed directly into the HTML Reeference Manual
- Length of symbols is no more limited.

**New primitives / parameters**:

- `read-ci`
- `eval-form-string-ci`
- `read-from-sring-ci`
- `compiler:source-rewrite`
- `compiler:peephole-optimizer`
- `1+` and `1-`
- `push!` and `pop`
- `inc!` and `dec!`
- `dolist`
- `apropos/alist`
- `apropos/pp`
- `default-browser`
- `open-in-browser`
- `manual`
- `install-path`
- `define-parameter`

**Updated embedded libraries**

- `libgc` updated to version 8.2.8
- `libffi` updated to version 3.4.6
- `libpcre2` updated to version 10.44

**New supported SRFI**

- SRFI-115: Scheme Regular Expressions
- SRFI-178: Bitvector library
- SRFI-232: Flexible curried procedures

**Misc:**

- Updated documentation
- Added tests
- Code cleaning an optimizations
- Bug fixes



Version 2.00
------------

This is a major version of ***STklos***, bringing it from R^5^RS to R^7^RS. It is
the fruit of nearly two years of development with the help of the following
persons (in alphabetical order):
   - Amirouche Boubekki ([@amirouche](https://github.com))
   - Lassi Kortela ([@lassik](https://github.com/lassik))
   - Ivan Maidanski ([@ivmai](https://github.com/ivmai))
   - Tom Niget ([@zdimension](https://github.com/zdimension))
   - Jeronimo Pellegrini ([@jpellegrini](https://github.com/jpellegrini))
   - Ryan Schmidt ([@ryandesign](https://github.com/ryandesign))
   - Ben Taca ([@bentaca](https://github.com/bentaca))
   - Robby Zambito ([@Zambito1](https://github.com/Zambito1))

The main changes in this version are:

- Support for R⁷RS libraries
- Macro system has been rewritten
- Support for ScmPkg packages has been suppressed
- New options for the commands *stklos(1)* and  *stklos-compile(1)*
- Optimizations on numerical operations
- Documentation is rewritten in `asciidoctor` and has been greatly enhanced
- Updated the Docker build files
- New instructions in the VM
- Fixed some issues in the compilation/installation procedure
- New extension: `curl` to access the *multiprotocol file transfer library*
- Better support for macOS
- `describe` has been enhanced for some type of objects
- REPL has now tab completion
- Bash and zsh completions for `stklos` and `stklos-compile` commands
- Better `SLIB` support
- Use PCRE2 library instead of legacy PCRE
- New-command `stklos-pp(1)`
- The preferred path for configuration files follows now [XDG Base
  Directory Specification](https://specifications.freedesktop.org/basedir-spec).
- Better error messages
- New SRFIS supported
  - SRFI-19:  Time Data Types and Procedures
  - SRFI 43: Vector Library
  - SRFI-95: Sorting and Merging
  - SRFI-116: Immutable List Library
  - SRFI-125: Intermediate hash tables
  - SRFI-138: Compiling Scheme programs to executables
  - SRFI-152: String Library (reduced)
  - SRFI-162: Comparators sublibrary
  - SRFI-154: First-class dynamic extents
  - SRFI-215: Central Log Exchange
  - SRFI-217: Integer Sets
  - SRFI-222: Compound objects
  - SRFI-224: Integer Mappings
  - SRFI-227: Optional Arguments
  - SRFI-228: Composing Comparators
  - SRFI-229: Tagged Procedures
  - SRFI-230: Atomic Operations
  - SRFI-235: Combinators
  - SRFI-236: Evaluating expressions in an unspecified order
  - SRFI-238: Codesets
  - SRFI-244: Multiple-value Definitions
- Added support for some R7RS-large libraries
   - supported libraries of the *Red edition* are
       - `(scheme bytevector)`
       - `(scheme box)`
       - `(scheme charset)`
       - `(scheme comparator)`
       - `(scheme generator)`
       - `(scheme hash-table)`
       - `(scheme ideque)`
       - `(scheme ilist)`
       - `(scheme list)`
       - `(scheme list-queue)`
       - `(scheme lseq)`
       - `(scheme set)`
       - `(scheme sort)`
       - `(scheme stream)`
       - `(scheme text)`
       - `(scheme vector)`
    - supported libraries of the *Tangerine edition* are
      - `(scheme bitwise)`
      - `(scheme bytevector)`
      - `(scheme division)`
      - `(scheme fixnum)`
      - `(scheme flonum)`
      - `(scheme generator)`
      - `(scheme vector @)`
- New primitives
  - `library-name`
  - `library-list`
  - `module-list`
  - `module-lock!`
  - `module-locked?`
  - `symbol-mutable?`
  - `define-constant`
  - `cpointer-null?`
  - `syntax-error`
  - `hash-table-clear!`
  - `environement`
  - `send-signal`
  - `set-signal-handler!`
  - `get-signal-handler`
  - `void?`
  - `procedure-source`
  - `procedure-formals`
  - `short-version`
  - `radians->degrees`
  - `degrees->radians`
  - `sinh`, `cosh`, ...
- New compiler options (also parameter objects):
   - `compiler:gen-line-number`
   - `compiler:show-assembly-code`
   - `compiler:time-display`
   - `compiler:keep-formals`
   - `compiler-keep-source`
   - `compiler:inline-common-functions`
   - `compiler:unroll-iterations`
   - `repl-show-startup-message`
- New predefined classes
  - `<uvector>`
  - `<bytevector>`
  - `<hash-table>`
  - `<port>`
  - `<input-port>`
  - `<output-port>`
- Code cleaning an optimizations
- Bug fixes



Version 1.70
------------

This version further improves compliance with R7RS and brings some new SRFIs

The main changes in this version are:

   - Added the some comma-commands to the REPL (,pwd ,cd and ,shell)
   - Use system Posix glob instead of the provided gnu-glob
   - Improved reader (`|.|` is wow  valid symbol as well as numbers with bars)
   - New primitive/syntaxes
       - `tagbody`
       - `repeat`
       - `keyword-colon-position` is a parameter object specifying the places
         where the colon can be used in keyword.
   - Suppressed the calls to insecure C function (such as `strcpy` or `strcat`)
   - List reading is tail recursive now
   - Added an extension directory, which contains for now:
      - **GTKlos** which gives access to the GTK+ toolkit using the OO
        layer available in STklos. This version uses the GTK+3
        toolkit.  It also defines a canvas widget. if the to have the
        *goocanvas* library is installed.
      - an extension which permits to make a file system in Scheme,
        using *FUSE* (Filesystem in Userspace).
   - Use gmp-mini (which is provided) if gmp is not available, instead
     of our version which was based on old unmaintained version of
     mpi.
   - Support of new SRFIs:
       - SRFI-25: Multi-dimensional Arrays
       - SRFI-27: Source of random bits
       - SRFI-29: Localization
       - SRFI-94: Type-Restricted Numerical Functions
       - SRFI-132: Sort Libraries
       - SRFI-133: Vector Library (R7RS-compatible)
       - SRFI-143: Fixnums
       - SRFI-144: Flonums
       - SRFI-170: POSIX API
       - SRFI-208: NaN procedures
       - SRFI-214: Flexvectors
       - SRFI-217: Integer Sets
       - SRFI-219: Define higher-order lambda
       - SRFI-223: Generalized binary search procedures
   - SRFI can be implemented in C and Scheme
   - Various optimizations
   - Documentation update
   - Added tests
   - Bug fixes

Version 1.60
------------

As the previous version, R7RS compliance is enhanced and a bunch of
new SRFIS has been implemented (thanks again to [Jeronimo Pellegrini —
**@jpellegrini**](https://github.com/jpellegrini)).

Changes in this version are:

   - Added option -I and -A to prepend and append to the loadpath
   - New primitives/syntaxes
       - `require-feature`
       - `define-values`
   - Added makefile targets to install only subparts of STklos on constrained
     environments
   - Reader accepts now `#i` for rationals
   - Changed the version of LALR-SCM to version 2.5.0
   - Added scripts and documentation in the `exemples` directory
   - Definition of a new framework for implementing SRFIs
       - Documentation of SRFI is now automatically built
       - SRFI tests are automatically done with `make tests`
       - Automatic update of the SUPPORTED-SRFIS file
   - Support of new SRFIs
       - SRFI-5: A compatible let form with signatures and rest arguments
       - SRFI-41: Streams
       - SRFI-61: A more general COND clause
       - SRFI-113: Sets and Bags
       - SRFI-127: Lazy Sequences
       - SRFI-128: Comparators (reduced)
       - SRFI-130: Cursor-based string library
       - SRFI-134: Immutable Deques
       - SRFI-135: Immutable Texts
       - SRFI-137: Minimal Unique Types
       - SRFI-192: Port Positioning
       - SRFI-193: Command line
       - SRFI-195: Multiple-value boxes
       - SRFI-196: Range Objects
       - SRFI-207: String-notated bytevectors
       - SRFI-216: SICP Prerequisites (Portable)
   - Documentations update
   - Added tests
   - Bug fixes

Version 1.50
------------

This version enhances R7RS compliance and a lot of new SRFIs (thanks to the awesome work of [Jeronimo Pellegrini  — **@jpellegrini**](https://github.com/jpellegrini))

Changes in this version are:

   - Exceptions behavior is now R7RS compliant
   - `current{input,ouput,error}-port` are now parameters as required by R7RS.
   - Socket ports are now both binary and textual.
   - Added support for Android.
   - Better support of 32 bits big-endian architectures
   - Support of more architectures. STklos can be run now on
       - GNU/Linux on x86_64  (64 bits)
       - GNU/Linux on ARM v7l (32 bits)
       - GNU/Linux on MIPS (32 bits little and big endian)
       - FreeBSD
       - OPenBSD
       - MacOS X86_64
       - Windows 10 (on WSL)
       - Android on ARM 64
   - Support of new SRFIs (most of them were implemented by @jpellegrini):
       - SRFI-37: args-fold: a program argument processor
       - SRFI-51: Handling rest list
       - SRFI-54: Formatting
       - SRFI-64: A Scheme API for test suites
       - SRFI-117: Queues based on lists
       - SRFI-118: Simple adjustable-size strings
       - SRFI-129: Titlecase procedures
       - SRFI-141: Integer Division
       - SRFI-145: Assumptions
       - SRFI-151: Bitwise Operations
       - SRFI-156: Syntactic combiners for binary predicates
       - SRFI-158: Generators and Accumulators
       - SRFI-161: Unifiable Boxes
       - SRFI-169: Underscores in numbers
       - SRFI-171: Transducers
       - SRFI-173: Hooks
       - SRFI-174: POSIX Timespecs
       - SRFI-175: ASCII character library
       - SRFI-176: Version flag
       - SRFI-180: JSON
       - SRFI-185: Linear adjustable-length strings
       - SRFI-189: Maybe and Either: optional container types
       - SRFI-190: Coroutines Generators
   - Better documentation (thanks to @jpellegrini again! — need to be completed)
       - description of the internals of STklos
       - description of the VM instructions
       - instructions to compile STklos on various OS
       - updated manual's bibliography
   - Bug fixes

Version 1.40
------------

Changes in this version are:

   - Updated the build tools
   - Now STklos is case sensitive by default as required by R7RS\
     the R5RS case insensitivity can be enable with the
     `--case-insensitive` option
   - New parameter: `repl-theme` permits to customize REPL prompt colors.
   - Added more tests
   - Added the `#:xxx` syntax for keywords for compatibility with
     other implementations
   - Added the R7RS syntaxes letrec*, let-values & let*-values
   - Various bug fixes


version 1.31
------------

Bug fix version.

Changes in this version are:

  - Updated the version of the libraries included in the distribution
  - Bug fix of various problems when installing STklos
  - Bug fix for MacOs UTF8 detection
  - Added SRFI-112
  - Updated documentation


version 1.30
------------

The goal of this version consists to be more R7RS compliant. All the
R5RS functions which have been extended in R7RS are now conform to
R7RS (for instance `assoc` and `member` accept now an optional
parameter which is a compare function, vector->list accepts the
`start` and `end` parameters, ...)


Changes in this version are:
  - Implementations R7RS I/O
      - Ports can now be binary or textual (standard ports are both)
      - New functions:
          - `write-string`
          - `wite-shared` & `write-simple`
          - functions to input and output bytevectors
          - `read-u8` `peek-u8` and `u8-ready?`
          - `call-with-port`
          - `textual-port?`
          - `binary-port?`
          - `open-binary-input-file`
          - `open-binary-output-file`
          - `utf8->string` & `string->utf8`
 - Updated various functions to be R7RS compliant.
 - Added the R7RS functions on bytevectors
 - Added R7RS `#true` and `#false` constants
 - Added R7RS exceptions
 - Added R7RS system functions
 - Upgraded the libraries embedded in STklos source tree
 - Optimization of circular structure reading & writing
 - New primitives
       - `display-simple` & `display-shared`
       - R7RS `symbol=?`
       - R7RS `boolean=?`
 - Updated documentation
 - Added SRFI-111
 - Bug fixes


version 1.20
------------

Even if previous version was released more than seven years ago,
STklos is not completely dead.

Now the developement tree of STklos is available on
[Gitlab](https://gitlab.com/Gallesio/STklos) and
[Github](https://github.com/egallesio/STklos).

Changes in this version are:

  - Corrected some problems on UTF8 encoding
  - Bug fix of a long standing bug on `call/cc`.
  - Function can now be created with the UTF8 symbol `λ`
  - Added the #!fold-case and #!no-fold-case  special notations
    of R7RS
  - Updated documentation
  - Bug fixes

version 1.10
------------

This version brings the support of UTF-8 encoding and begins to support some (future) R7RS traits:

 - Added full support for UTF-8 strings and symbols
 - Added support for Win32 support under Cygwin
 - Added some support for R7RS traits:
     - bytesvectors
     - hexadecimal chars in strings and symbols
     - continuations lines in strings
     - New primitives
         - `char-foldcase`
         - `string-foldcase`
         - `string-foldcase!`
         - `make-list`
         - `string-map`
         - `string-for-each`
         - `vector-for-each`
         - `vector-map`
 - Added support for MacOs X Lion
 - Added back support for Win32 using Cygwin (this support has still
   rough edges. See the file PORTING-NOTES in the distribution
   directory for details)
 - Bug Fixes

version 1.01 (2010 12 29)
------------------------

This is mainly a maintenance version.

 - Documentation update
 - Modifications to support ScmPkg on MacOs MacPorts
 - Fix incompatibilities in FFI introduced in 1.00
 - Better x86_64 support
 - Bug fixes

version 1.00 (2010 08 11)
-------------------------

The version 1.00 is finally here (work on STklos started 10 years before ...).
The main difference with this version and the pre-1.0
ones is that it does not integrate GTK+ 1.x support. In fact, GTK+
support is now available through several ScmPkg packages. Otherwise:

 - Complete rewriting of GMP-lite, the provided **gmp** compatible package
   for system which does not provide it
 - Remove the old extension installation mechanism (stklos-install).
   Use the ScmPkg mechanism for extending STklos now
 - functions, generic functions and methods cans now have documentation strings
 - Added GNU readline support in REPL if the readline lib can be
   loaded dynamically
 - Configuration files location can now be changed
 - Added an interactive help system
 - libffi uptated to version 3.0.9
 - New implemented SRFIs
     - SRFI-74 (Octet-Addressed Binary Blocks)
     - SRFI-96 (SLIB Prerequisites )
     - SRFI-98 (Interface to access environment variables)
     - SRFI-100 (define-lambda-object)
 - New primitives
     - `port?`
     - `help`
     - `make-directories`
     - `ensure-directories-exist`
 - Bug fixes

version 0.98 (2008 04 15)
-------------------------

 - Replaced the C/invoke FFI library by libffi: Now GTk2 ScmPkg
   packages work on MacOs
 - Added option --build-sync-file to ease personal ScmPkg repository maintenance
 - Bug fixes

version 0.97 (2007 12 16)
-------------------------

 - Fixed support for recent versions of SLIB
 - Added boxes: they were used by the system but unavailable to the user.
 - Improved the stklos-pkg command
 - Fixed several mutexes bugs
 - Minor improvements of the FFI
 - Updated PCRE to version 7.4
 - New implemented SRFIs
     - SRFI-45 (Primitives for expressing iterative lazy algorithms)
     - SRFI-59 (Vicinity)
     - SRFI-88 (Keyword Objects)
     - SRFI-89 (Optional and named parameters)
 - Bug fixes

version 0.96 (2007 06 30)
-------------------------

This release introduces a simple FFI (Foreign Function Interface).

 - Better support for FreeBSD
 - Fixed a bug when using the -j option of make during bootstrap
 - Fixed several problems with ScmPkg packages installation

version 0.95 (2007 06 08)
-------------------------

This is a major version introducing support for the ScmPkg source packaging system.

 - Better support for lexical analyzer generation
 - Added some support for fixnum operations
 - Added here-strings support
 - Added partial module import
 - Macros can be local
 - Added some peephole optimizations
 - Added new options to the compiler
 - Added support for reading the tar files
 - void and eof objects are now printed back as #void and #eof and can
   be read back
 - New primitives
     - signal-error
     - md5sum
     - md5sum-file
     - file-prefix
     - file-suffix
     - condition-set!
 - Bug fixes

version 0.82 (2006 12 19)
-------------------------

 - Added the --debug option to the interpreter
 - Better error messages when in debug mode and more correct line numbers
 - Updated Dominique Boucher lalr parser to the latest version
 - Better error messages for POSIX functions
 - Internal macro definition are allowed now
 - New parameters for controlling compilation
 - New primitives
     - null-environment
     - scheme-report-environment
     - interaction-environment
     - directory-files
     - make-directory
     - delete-directory
 - Bug fixes


version 0.81 (2006 11 06)
------------------------


This is a bug-fix version. Previous release had a bug which prevent to
compile it on some architectures/systems.

 - Corrected a deadlock when compiling the system
 - Corrected bugs in SRFI-0 and SRFI-13

version 0.80 (2006 10 27)
-------------------------

This is a version with important changes to support
multi-threading. This version provides two kinds of threads: LURC
threads and Posix threads (aka pthreads). LURC threads are implemented
using the LURC library, the Light ULM/Reactive library for C developed
at INRIA. LURC supports the creation of synchronous cooperative
threads, synchronizing and communicating with each other using signals
in a deterministic scheduler (see
http://www-sop.inria.fr/mimosa/Stephane.Epardaud/lurc for details).


 - Thread support
 - Better error messages
 - Added the possibility to compile byte-codes to a C file.
 - New optimizations
 - More customizable REPL
 - New primitives
     - read-byte
     - write-byte
     - printf
     - fprintf
     - base64-{en|de}code
     - current-second
 - New implemented SRFIs
     - SRFI-18 (Multithreading support)
 - Bug fixes


version 0.72 (2006 01 04)
-------------------------

This is a minor release.

- New primitives
    -  string-blit!
    - print
    - printerr
- New implemented SRFIS
    - SRFI-66 (Octet Vectors)
- Bug fixes

version 0.71 (2005 11 03)
-------------------------

 - Added virtual ports (ports defined by user functions)
 - The reader accepts now #eof to denote the eend of file object
 - Fix problems with SunOS support
 - Fix problems with gcc4 compilation
 - Hash tables are now compliant to SRFI-69 (Basic Hash Tables).  This
   implies some (compatible) changes in the hash-tables function
   names.
 - New primitives
     -  finite?
     -  infinite?
     -  unsetenv!
     -  file-size
 - New implemented SRFIs
     -  SRFI-62 (S-expression comments)
     -  SRFI-69 (Basic Hash Tables)
     -  SRFI-70 (Numbers)
 - Bug fixes

version 0.70 (2005 05 27)
-------------------------

The main changes in this release are: a new back-trace system, a
conform call/cc/dynamic-wind implementation and some performance
enhancements.

 - Added a way to have a back-trace when an error occurs
 - call/cc has been completely implemented. Its interaction with
   dynamic-wind is now conform to R5RS.
 - Perfomance improvements
 - Added some infrastructure for auto-testing a newly built system.
 - New primitives
     - port-rewind
     - port-seek
     - current-loading-file
     - decode-float
     - read/ss
     - write/ss
     - repl
 - Multiple EOF can be seen now on an input file to allow staged REPLs
 - Added the configure options `--with-provided-gc` and
   `--with-provided-regexp` to force the usage of our GC and PCRE
   versions, even if there are some version already installed on the
   system
 - The boot file is now integrated in the VM instead of being read
   from a file. This enhances the loading time of the interpreter
   a bit.
 - Describe shows more information for structures types and
   conditions.
 - The object system takes now into account conditions and
   conditions types
 - Documentation updated
 - Bug fixes


version 0.61 (2005 04 05)
-------------------------

 - Documentation updated
 - Distribution uses now PCRE 5.0 for regexp
 - Build process has changed
 - Performance improvement thanks to VM and I/O optimizations
 - Added a way to download and install STklos extensions
 - Better error signaling on undefined variables when compiling files
 - Minor enhancements:
    - Some corrections in the reader
    - Parameter objects SRFI-39 (Parameters objects) can be used now
      in generalized set! SRFI-17 (Generalized set!)
    - Reader can be case-sensitive now (the system can be bootstraped
      using case sensitive read)
    - ...
 - New primitives
    - eval-from-string
    - require/provide
    - read-chars
    - read-chars!
    - write-chars
    - get-password
 - Implementation of SRFI-60 (Integers as bits)
 - Bug fixes

version 0.60 (2004 11 29)
------------------------

This version is mostly a correcting version which fixes a serious bug
on file loading introduced in release 0.59. Very minor changes since
previous release

 - SRFIs can also have symbolic names
 - Minor modifications to allow the compilation of Skribe with STklos
   on Win32
 - A bunch of functions on dates
 - Implementation of SRFI-55 (Require-extension)
 - Bug fixes

Supported SRFIs: 0, 1, 2, 4, 6, 7, 8, 9, 10, 11, 13, 14, 16, 22, 23,
          26, 27, 28, 30, 31, 34, 35, 36, 38, 39, 48, 55.


version 0.59 (2004 11 04)
-------------------------

 - Documentation has been rewritten in Skribe
   (http://www-sop.inria.fr/mimosa/fp/Skribe)
 - Added Dominique Boucher LALR(1) parser generator.
   (http://www.iro.umontreal.ca/~boucherd/Lalr/documentation/lalr.html)
   Thanks to Dominique for allowing me to do so
 - Added the future R6RS comment syntax #;
 - New primitives
      - read-from-string
      - load-path
      - load-verbose
      - load-suffixes
      - fork
      - port-current-position
      - port-closed?
 - New implemented SRFIs
      - SRFI-10 (Sharp Comma External Form)
 - Bug fixes

Supported SRFIs: 0, 1, 2, 4, 6, 7, 8, 9, 10, 11, 13, 14, 16, 22, 23,
          26, 27, 28, 30, 31, 34, 35, 36, 38, 39, 48.

version 0.58
------------

 - Better Win32 port (was no more fully functional since 0.55)
 - Added a way to configure the system with our version of GMP even
 - if GMP is installed
 - Added structure types
 - Stack size can be changed when the VM is launched
 - New port on Octane/SGI Irix (by Joshua Boyd) and Itanium
 - Added the --interactive option for embedded VM
 - Compiler can add line information to the generated code to ease debugging
 - error locations are more accurate now
 - New primitives
      - winify-file-name (for Win32 only)
      - sleep
 - New implemented SRFIs
      - SRFI-34 (Exception Handling for Programs)
      - SRFI-35 (Conditions)
      - SRFI-36 (I/O Conditions)
      - SRFI-48 (Intermediate Format Strings)
 - Bug fixes

Supported SRFIs: 0, 1, 2, 4, 6, 7, 8, 9, 11, 13, 14, 16, 22, 23, 26,
          27, 28, 30, 31, 34, 35, 36, 38, 39, 48

version 0.57
------------

 - Added support for non Finked Mac-OS X
 - Added the Danny Dub� SIlex lexical analyser generator
   to the distribution. Thanks to Danny for allowing me to do so
 - New primitives
     - fork
     - register-exit-function!
     - html->string
     - uri-parse
     - chmod
 - New implemented SRFIs
     - SRFI-16 (Syntax for procedures of variable arity)
     - SRFI-26 (Notation for Specializing Parameters without Currying)
 - Bug corrections

Supported SRFIs: 0, 1, 2, 4, 6, 7, 8, 9, 11, 13, 14, 16, 22, 23, 26,
          27, 28, 30, 31, 38, 39

version 0.56
------------

- The reader recognizes now the DSSL keywords #!rest,
  #!optional and #!key
- New implemented SRFIs: 27 (random), 38 (external
  representation of shared structures) and 39 (parameter
  objects)
- New parameter: REAL-PRECISION allow to change the precision
  used when displaying a real number
- New primitives: HOSTNAME, REGEXP-QUOTE, MAKE-PATH,
  FILE-SEPARATOR, HASH-TABLE-UPDATE!, DATE,
          {READ,WRITE}-WITH-SHARED-STRUCTURE
- Modified primitives: GENSYM, PARSE-ARGUMENTS
- Added the definition of feature 'stklos' for SRFI-0
- Bug corrections

Supported SRFIs: 0, 1, 2, 4, 6, 7, 8, 9, 11, 13, 14, 22, 23, 27
          28, 30, 31, 38, 39

 version 0.55
 -----------

- New PCRE vesion (4.3)
- New primitive: COPY-FILE
- Bug corrections

version 0.54
------------

- Evaluation of parameters is now from left to right rather than the opposite.
- New primitives: PORT-IDLE-REGISTER!,  PORT-IDLE-UNREGISTER!,
  PORT-IDLE-RESET!, GLOB, CANONICAL-PATH-NAME,
  EXPAND-FILE-NAME, BASENAME, DIRNAME, DECOMPOSE-FILE-NAME
- Socket Support
- Added support for SRFI-23 (Error)
- Added support for SRFI-30 (Nested comments)
- Added support for SRFI-31 (REC form)
- Changed the Boehm GC version to 6.1
- Code cleaning (again) to be accepted by gcc-3.x
- New Ports: Solaris, Win32, Mac OS X
- Bug corrections
- ...

Supported SRFIs: 0, 1, 2, 4, 6, 7, 8, 9, 11, 13, 14, 22, 23, 28,
          30, 31

version 0.53
------------

- Added support for SRFI-7
- Added support for SRFI-11
- Added support for SRFI-14
- Added support for SRFI-28
- Better LDAP support
- Added PRETTY-PRINTER
- Added TRACE
- Added APROPOS
- A bunch of new primitives: DIE, PAIR-MUTABLE?,
  VECTOR-MUTABLE?, STRING-MUTABLE?, STRING-TITLECASE,
  STRING-TITLECASE!, SYMBOL-VALUE*
- New syntax: IN-MODULE
- Changed the Boehm GC version to 6.1 alpha5
- Code cleaning to be accepted by gcc-3.x
- Bug corrections
- ...

Supported SRFIs: 0, 1, 2, 4, 6, 7, 8, 9, 11, 13, 14, 22, 28

version 0.52
------------

- Compiler has been rewritten and deliver code which is more
  efficient
- New command 'compile-file' to compile a stklos file to byte-codes
- Complete rewriting of GTklos (support of GTK+ in STklos)
  the rewriting is still incomplete and needs some work
      - support of the Canvas widget (nearly completely finished)
      - more general event management
- All the demos have been rewritten
- Better error reporting
- Errors can now be browsed with Emacs next-error previous-error
- Added support for SRFI-2
- Added support for SRFI-4
- Added support for SRFI-8
- Added support for SRFI-9
- Added support for SRFI-22
- new option: "--no-init-file"
- Added bitwise operations on bignums
- Virtual slots can now have an initial value
- New primitives: KEY-DELETE!, KEY-DELETE, PROGRAM-NAME,
  PORT-FILE-NAME, FULL-CURRENT-TIME, SECONDS->DATE
- Added the Bigloo MATCH-CASE and MATCH-LAMBDA
- New form PARSE-ARGUMENTS to simplify argument parsing
- Better support of dynamic loading.
- Minimal support of LDAP.
- Support of active-slots (as in old STk)
- PCRE package used is now 3.7
- GC package used is now 6.0
- More documentation
- There are now man pages for stklos and utilities
- Many small improvements
- Bug corrections
- ...

version 0.51
------------

- Added R5RS hygienic macros
- Added compatibility with the Aubrey Jaffer's SLIB
- New port on Alpha/Linux
- Added SRFI 0
- Added SRFI 1
- Some parts of the VM have been rewritten to integrate full CALL/CC
  (not yet finished)
- New primitives: CURRENT-TIME, RUNNING-OS, FIND-PATH, CALL/EC,
  LOAD-PATH, SET-LOAD-PATH!, LOAD-SUFFIXES and SET-LOAD-SUFFIXES!
- Exact reals can be read now (e.g #e3.14 => 157/50)
- New demo: edit.stk
- Bug corrections

version 0.50 (released on 2001-01-17)
-------------------------------------

- First public release



<!-- Local Variables: -->
<!-- mode: markdown -->
<!-- End: -->
