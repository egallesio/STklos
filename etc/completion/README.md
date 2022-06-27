## Shell completion functions

This directory contains the following completions functions for the commands `stklos` and `stklos-compile`. 

- `stklos` is a completion function for `bash`
* `_stklos` is a completion function for `zsh`


### Adding completion for  bash

Source the file `stklos` to get tab completion for the STklos command in `bash`.

For a permanent effect, either:

- source this from your .bash_profile, or

- put it in your user's local bash completion directory. In  Debian this is `$XDG_DATA_HOME/bash-completion/completions`  or

- put it with the other bash completion files in your system --  for example, in Debian and Debian-based systems, this will be `/usr/share/bash-completion/completions/`

It is recommended that the completion script have the same name as the
command, so a good choice is to install the script under the name 'stklos' and
create a symbolic link from 'stklos-compile' to 'stklos'

-rw-r--r-- 1 root root 9380 May 16 10:35  stklos
lrwxrwxrwx 1 root root    6 May 16 10:37  stklos-compile -> stklos



## Adding completion for zsh

To add completion for `zsh` place the file `_stklos` in one of the directories accessible in the `$fpath` environment variable. Eventually run the `compinit` function if it is not called in your `~/.zhsrc`. Et voilà. 
