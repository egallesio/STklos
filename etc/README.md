
This directory contains the following directories:

  - **Docker**: A directory containing two Docker files for STklos (one
    for building STklos with the sources coming from the git tree and
    another one to built it from a stable release).

  - **Unicode**: A directory containing some files to build the UT8 tables
  needed by STklos

  - **completions**: A directory which contains code for *bash(1)* and *zsh(1)*
  completions.

It also contains two files, which are not complete examples (it is why
they are not in the `examples` directory). They can be used as
templates, for some advanced work in C with STklos.
  - **simple-module.c** : A simple module which adds some primitives
    written in C to the interpreter. This module must be compiled and
    dynamically loaded with the interpreter (documentation can be
    found in the source file).
  - **simple-stklos.c**: A simple main program which uses the STklos
    library)

    **IMPORTANT**: this program must be updated to be run
    on recent versions of STklos.
