# myte
[![tests](https://github.com/Hans-Halverson/myte/actions/workflows/ci.yml/badge.svg)](https://github.com/Hans-Halverson/myte/actions/workflows/ci.yml)

Myte is a modern, general-purpose programming language. This repository contains Myte's compiler, standard library, and tooling.

## Building and testing the compiler

The Myte compiler is written in OCaml (version 4.09.0). To build the compiler from source and run its test suite, check out this repository and run the following commands from the `myte` repository's root directory.

### Initial install

First [install opam](https://opam.ocaml.org/doc/Install.html), the OCaml package manager. Then navigate to the checked out `myte` directory and run the following commands, which will set up opam and install the correct version of the compiler and dependencies for Myte.

```
opam init
opam switch create 4.09.0
opam install . --deps-only --yes
```

The initial install only needs to be completed once (unless dependencies are updated, in which case run the `opam install` command again).

### Building and testing

Scripts are included for building and testing the Myte compiler. To build the Myte compiler, run:

```
./scripts/build
```

And to run the compiler's test suite, after building run:

```
./scripts/run_tests
```

You may also run the `myte` compiler binary directory with:

```
./scripts/myte
```