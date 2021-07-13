# STklos-fuse demo programs


In this directory you can find two file systems written in STklos:

-   **hellofs**: A simple file system which contains only one file named
    `"hello"`. You cannot do a lot with this file system and most actions
    produce errors.

    To mount the file system you can for instance type

    ```sh
    $ hellofs -f -s ~/fuse     # Directory must exists
    ```

     This will mount the **hellofs** on the (already existing and empty)
     `~/fuse` directory. The `-f` option  permits to stay in foreground
     and the `-s` option uses a single thread to avoid GC problems.
     If you want to go in background, you can use the `nohup` command.

     For instance,

    ```sh
    $ nohup hellofs -s ~/fuse
    ```

-  **hashfs**: This is an implementation of a file system in STklos hash
   tables. This implementation is rather complete and can be used as
   a starting point to develop a new file system. To use it just type

   ```sh
    $ hashfs -f -s ~/fuse
   ```


Note that to *unmount* a previously mounted file system, you have to use the
`fusermount` command

```sh
    $ fusermount -u ~/fuse
```
