# Contributing to STklos

Contributions to STklos are warmly appreciated. Whether it is reporting or fixing a bug, implementing a new feature, correcting or adding documentation, or any other enhancement, your involvement is valuable.

## Preparing Your Environment

To contribute, start by cloning the latest `master` branch from GitHub or GitLab:

```shell
git clone https://github.com/egallesio/STklos  # Clone from GitHub
```

or

```
git clone https://gitlab.com/Gallesio/STklos   # Clone from GitLab
```

Before compiling, ensure you have the following dependencies installed:
*   A `make` command;
*   A C compiler (GNU GCC, Clang, or TCC are supported);
*   GNU Autotools (`autoconf`, `automake`, etc.) if you plan to modify `Makefile`s only.

Compiling STklos is straightforward:

```shell
$ ./configure
$ make
$ make tests     # Run tests on your local build
$ make install   # Install the compiled version (optional)
```

Various options allow you to adapt the compilation and installation process. For a detailed list of `configure` options, please refer to the `QUICK-INSTALL.md` and `INSTALL` files.

## File Tree Structure / Virtual Machine

For an overview of the directory structure, consult `doc/hacking.html` (also available in PDF as  `doc/hacking.pdf`). Documentation regarding the STklos Virtual Machine is available in `doc/vm.html` (also available in PDF as `doc/vm.pdf`).

## Submitting Your Contribution

> **STklos doesn't use any AI generated code or documentation and for numerous reasons we want to keep this as is. As a consequence, and for the sake of project integrity, we do not accept Large Language Models (LLM) generated contributions**

For now, development takes place on GitHub (http://github.com/egallesio/STklos). Please submit your contributions as Pull Requests. Problems or bugs should be reported via GitHub Issues.

This document is intentionally concise. If you require further clarification or if using GitHub is a concern for you, send an email at the address displayed in the STklos banner.
