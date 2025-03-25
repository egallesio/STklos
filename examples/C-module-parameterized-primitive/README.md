# Writing STklos modules in C and Scheme

This is an example of a STklos module written part in C and part in Scheme.

What we illustrate with this example:

* Combining C and Scheme to build an STklos module;
* Selectively exporting symbols from the C file;
* Referencing, in the Scheme file, symbols defined in the C file;
* Writing help for procedures, not only in Scheme files, but also
  in C files.

NOTE: It is probably simpler to understand the file `Makefile-sample` than the
GNU autotools generated `Makefile`. The main difference is that the generated
`Makefile` can be run without installing STklos, whereas `Makefile-sample` needs a
prior installation of STklos. 

## Caveats

* The `Makefile-sample` was built for Linux, but can be adapted for other systems.
* In order to write STklos modules in C, it is necessary to understand
  how typical STklos C code works. This can be found in the `hacking` file.

## How it works

The idea is that, from two files:

* `some-module.c`
* `some-module.stk`

we build a single shared object

* `some-module.so`

Then, if the module name is "some-module", and the shared object is
in the STklos load path, one can simply (import some-module).

Of course, if the module name defined internlaly in the C file is
`a/b/c/some-module`, then the file `some-module.so` should be in that
subpath:

```
stklos> ,ls a/b/c
some-module.so
stklos> (import (a b c some-module))
stklos>
```

See the Scheme and C files, and also the Makefile in this directory for
an in-depth understanding of how this works.

## How to use the compiled module

* Run `make` in this directory
* See that `string-obfuscate.so` was created in the current directory
* start STklos

```
stklos> (import string-obfuscate)
stklos> (string-obfuscate "ABC" 3)
"DEF"
stklos> (string-obfuscate-set! string-xor)
stklos> (string-obfuscate "ABC" 3)
"BA@"
```

inspecting the C file, one can see that a procedure was added to the
module, but not exported:

```
stklos> string-obfuscate-hidden
**** Error:
%execute: symbol 'string-obfuscate-hidden' unbound in module 'stklos'
	(type ",help" for more information)
stklos> (in-module string-obfuscate string-obfuscate-hidden)
#[primitive string-obfuscate-hidden]
stklos> ((in-module string-obfuscate string-obfuscate-hidden))
"Hello, there from the hidden part of the module!"
```
