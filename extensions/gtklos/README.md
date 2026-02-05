# GTKlos extension


This extension gives access to the GTK+ toolkit using the OO layer available in STklos. You need to have GTK+3 installed to use it. This extension also defines a canvas widget. You need to have the *goocanvas* library installed to use canvases (this library is often not installed by default).


To compile GTklos:

```sh
$ make all      # compile GTklos and build the demos
```

## Requirement: GTK Libraries

The GTK libraries are searched in standard places (that is `/usr/lib` and
`/usr/local/lib`). If your libraries are located elsewhere, the configuration
script guess a plausible directory. Normally, the libraries should be found
and you don't need to do anything special. Anyway, if the libraries are not found,
you can use the `locate` command to have a hint  (search for  `libgtk-3`).
Once you have found this directory, you can set the `STKLOS_GTK_DIR` environment variable.

For instance, on a machine running Debian or Ubuntu (on a x86_64
architecture), this directory is `/usr/lib/x86_64-linux-gnu`. Normally, the
`./configure` for **STklos** should have found this directory by itself on these
distributions. However, if this not the case, you can set the variable
`STKLOS_GTK_DIR` with:

```shell
$ export STKLOS_GTK_DIR=/usr/lib/x86_64-linux-gnu
```

If you are on macOS you'll probably install gtk with `brew` so you have to do

```shell
brew install gtk+3
export STKLOS_GTK_DIR=/usr/local/opt/gtk+3/
```

After that, you have to run `./configure` from the project root directory and then return in `extensions/gtklos` and run `make`

## Demos

The `demos` directory contains some small programs which can be run individually or by using the `run-demos` script in the `gtklos` directory, particularly if you have never installed STklos before:

```sh
$ ./run-demos &
```

Have fun.
