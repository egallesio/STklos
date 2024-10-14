# Cross-compiling STklos

## How to cross-compile

Cross-compilation is just like compilation

```
./configure prefix=some-tmp-dir CC=some-C-compiler CFLAGS=your-c-flags ...
make
make install
```

The relevant environment variables are described in the output
of `configure --help`.

***However:***

* You *need* a STklos binary in order to compile STklos. When not
  cross-compiling,
  - the binary is generated as `src/stklos`
  - the build proceeds using `src/stklos` as the STklos binary that
    can be used to compile `.stk` files
  However, when cross-compiling `src/stklos` will be a binary for a
  different (target) architecture, and the build will fail, unless we
  have a native STklos in the system that can be used instead.

* Put the full path of the STklos binary in the `STKLOS_BINARY`
  variable when calling `make`:

```
./configure --prefix=/usr/some/place
make `STKLOS_BINARY=/usr/local/bin/stklos`
make install
```

Now you can copy the `usr/some/place` directory to the target machine.
