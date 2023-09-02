# Some examples in STklos

The files in this directory show some examples in STklos:

- `hello.stk` is a simple hello world. This file can be run directly
  as a script without compiling it by entering `./hello.stk` in a
  terminal.  Running `make` in this directory will also build the file
  `hello`. It is the compiled version of `hello.stk` which can be run
  with `./hello`. As a rule of thumb, any STklos program can be used
  as a script by adding the line

        #!/usr/bin/env stklos

  at the top of the file, and adding the execution bit on the file.
  You can also compile the file with the `stklos-compile` command.
  For instance

        stklos-compile -o hello hello.stk

  compiles the file `hello.stk` and pots the result in file `hello`.

- `secho.stk` is a simple echo program written is STklos. Use the
  `--help` option to see the available options.

- `fork-test.stk` is a simple program using the Unix `fork(2)`
  primitive.

- `threads.stk` is a program with 3 threads. It shows how to use
  threads and mutexes.

- `socket-server.stk` is a simple TCP server. Running it displays the
  port to use for communicating with it. This server accepts only one
  connection and it answers its client by returning the entered lines
  in uppercase.

- `socket-server-fork.stk` is a TCP server which accepts multiple
  clients. When a new connection is detected, a new process is
  launched (using fork) to handle the new connection

- `socket-server-thread.stk` another multiple client TCP server (using
  thread this time).

- `socket-client.stk` is a simple TCP client.
