# Extensions directory

This directory contains extensions which are not compiled/installed by default.

To compile an extension, you have to go in its directory and construct it with the `make` command.

To install an extension, you just need to enter `make install` at the command prompt.

Each extension contains a `demos` directory, with code examples.

For now, the available extensions are

  - **gtklos**: an extension which gives access to the GTK+ toolkit using the OO layer available in STklos. You need to have GTK+3 installed to use it. This extension also defines a canvas widget. You need to have the *goocanvas3* library installed to use canvases (this library is often not installed by default).

  - **stklos-fuse**: an extension which permit to make a file system in Scheme. This extension needs that you have *FUSE* (Filesystem in Userspace) installed.

