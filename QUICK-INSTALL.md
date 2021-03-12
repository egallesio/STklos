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
    - `--without-control-fixnums`: do not verify that parameters of fx functions are correct fixnums
    - `--enable-case-insensitive`: be case insensitive by default as in R5RS
    - `--enable-threads=TYPE`: choose threading package (value can be 'none' or 'pthreads', the default)

    You can also choose the compiler and the compiler options to use for building **STklos** with the `CC` and `CFLAGS` parameters:

     ```bash
     $ ./configure --prefix=/opt --with-provided-gc CC=clang CFLAGS="-O3 -Wall"
    ```
    A summary will be printed at the end of the execution of the script as shown below:

    ```
    SUMMARY
    *******
                   System:  Linux-5.6.15-1-MANJARO
                  OS nick:  LINUX_5_6
                  OS type:  unix
          Install prefix :  /opt
               C compiler:  clang
        Compilation flags:  -O3 -Wall
                   Loader:  ld
           Thread support:  pthreads
         Case sensitivity:  true (by default)
    System libraries used:  libffi libpcre libgmp
       Compiled libraries:  libgc
   ```


2. If everything is correct, you can just type `make` now at your shell prompt. Note that
   STklos supports [parallel execution](https://www.gnu.org/software/make/manual/html_node/Parallel.html)
   for faster builds. For instance, you can enter `make -j 8` to build 8 recipes simultaneously.

3. **Optionally**, you can type `make tests` to run some internal tests

4. To install the version just compiled in the configured place, type `make install` for a full install. The size of a full installation is approximately 6.5Mb (on a x86 64 bits architecture).  
On machines with a more limited space, you can use the following Makefile installation targets:
    - `install-base` to install only the VM and all the compiled Scheme files (size is ~2.0Mb). The installation is fully functional (except the `help` function which will yield an error).
    - `install-base-no-strip` is identical to `install-base`, except that the `stklos` binary is not stripped (this could be useful when cross compiling **STklos** or when generating packages with debug symbols).
    - `install-sources` to install the Scheme source files used whence building the system (adds ~1.5Mb to the installation)
    - `install-doc` to install the documentation (for the help command, manual pages, and reference manual in HTML and PDF formats). This adds ~3.0Mb) to the installation).

**Note**: `make install` is equivalent to `make install-base install-sources install-docs`



For a more complete description of the build process, you can refer to the
INSTALL file at the top of the distribution tree.
