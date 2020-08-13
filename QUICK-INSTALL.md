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
    - `--enable-case-insensitive`: be case insensitive by default as in R5RS
    - `--enable-threads=TYPE`: choose threading package (value can be 'none' or 'pthreads', the default)

    You can also choose the compiler and the compiler options to use for building **STklos** with the `CC` and `CFLAGS` parameters:

     ```bash
     $ ./configure --prefix=/opt --with-provided-gc CC=clang CFLAGS="-O3 -Wall"
    ```
    A summary will be printed at the end of the execution of the script as shown below:

    ```alertindent
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

4. To install the version you just compiled in the place you have chosen previously (or in
   `/usr/local`), type `make install`.

For a more complete description of the build process, you can refer to the
INSTALL file at the top of the distribution tree.
