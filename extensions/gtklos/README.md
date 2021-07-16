# GTKlos extension


This extension gives access to the GTK+ toolkit using the OO layer available in STklos. You need to have GTK+3 installed to use it. This extension also defines a canvas widget. You need to have the *goocanvas* library installed to use canvases (this library is often not installed by default).


To compile GTklos:

```sh
$ make all      # compile GTklos and build the demos
```

**Important note:** The GTK libraries are searched in standard places (that is
`/usr/lib` and `/usr/local/lib`). If your libraries are located elsewhere (use the `locate` command to have a hint), you can set the `STKLOS_GTK_DIR` environment variable. For instance, for a machine running Debian or Ubuntu on a x86_64 architecture, you'll probably have to do

```shell
$ export STKLOS_GTK_DIR=/usr/lib/x86_64-linux-gnu
```


The `demos` directory contains some small programs which can be run individually or by using the `run-demos` script in the `gtklos` directory, particularly if you have never installed STklos before:

```sh
$ ./run-demos &
```

Have fun.
