Quick Installation guide
=======================

**STklos** is configured/built thanks to the *GNU autotools*. Installing STklos is three (or
four) step process:

1.  You first need to run the `configure` script. By default, this script will try to use the
    libraries installed on your system. When a library is absent, a version of the library embedded
    with the source distribution will be used.

    The main options of the `configure` script are

    - `--prefix=PATH`: place where the files will be installed (default to `/usr/local`)
    - `--with-provided-gc`: use the provided Boehm GC library
    - `--with-provided-bignum`: use the provided Bignum (GMPlite) library
    - `--with-provided-regexp`: use the provided Regexp (PCRE) library
    - `--with-provided-ffi:` use the provided FFI library
    - `--with-gmp-light`: a synonym for `--with-provided-bignum`

    By default the following options are enabled, but can be disabled with the following `configure` options

    - `--disable-case-sensitive`: symbols are case insensitive by default (as in R5RS)
    - `--disable-threads`: disable Posix threads support
    - `--disable-control-fast-math`: do not verify that parameters of fx/fl functions are correct fixnums/flonums
    - `--disable-ffi`: disable FFI (Foreign Function Interface) support


    There are also environment variables that will have effect on how STklos is compiled. These should be passed to the configure script in the `CFLAGS` variable:

    - `STK_DEBUG`: enable debugging code for STklos
    - `VM_DEBUG`: enable debugging code for the VM
    - `STAT_VM`: enable code for collecting timing statistics for the virtual machine (useful for profiling user programs and for working on optimizing the VM itself

    You can also choose the compiler and the compiler options to use for building **STklos** with the `CC` and `CFLAGS` parameters:

    ```bash
    $ ./configure --prefix=/opt --with-provided-gc CC=clang-18 CFLAGS="-O3 -Wall -DSTAT_VM"
    ```

    A summary will be printed at the end of the execution of the script as shown below:

    ```
    SUMMARY
    *******
                    System:  Linux-6.10.9-arch1-1
                   OS nick:  LINUX_6_10
                   OS type:  unix
            Install prefix:  /opt
         C compiler binary:  clang-18
        C compiler version:  clang version 18.1.8
         Compilation flags:  -O3 -Wall -DSTAT_VM
                    Loader:  ld
            Thread support:  yes
               FFI support:  yes
    Case sensitive symbols:  yes
     Control fx parameters:  yes
     System libraries used:  ffi (3.4.6) pcre2 (10.44) gmp (6.3.0)
        Compiled libraries:  gc
      Documentation update:  yes (since Asciidoctor is installed)

    If this is correct, you can just type 'make' now at your shell prompt.
    Otherwise, re-run 'configure' with correct options.
    ```


2. If everything is correct, you can just type `make` now at your shell prompt. Note that
   STklos supports [parallel execution](https://www.gnu.org/software/make/manual/html_node/Parallel.html)
   for faster builds. For instance, you can enter `make -j 8` to build 8 recipes simultaneously.

3. **Optionally**, you can type `make tests` to run some internal tests

4. To install the version just compiled in the configured place, type `make install` for a full install. The size of a full installation is approximately 18Mb (on a x86 64 bits architecture).
On machines with a more limited space, you can use the following Makefile installation targets:
    - `install-base` (or `install-base-no-strip`) to install only the VM and all the compiled Scheme files (size is ~5Mb). The installation is fully functional (except the `help` function which will yield an error).
    - `install-base-strip` is identical to `install-base`, except that the `stklos` binary is stripped to save some disk space. This permits to save ~1Mb of disk space.
    - `install-sources` to install the Scheme source files used whence building the system (adds ~3Mb to the installation)
    - `install-doc` to install the documentation (for the help command, manual pages, and reference manual in HTML and PDF formats). This adds ~11Mb) to the installation).

**Note**: `make install` is equivalent to `make install-base install-sources install-docs`.



For a more complete description of the build process, you can refer to the
INSTALL file at the top of the distribution tree.
