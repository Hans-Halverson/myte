# myte
[![tests](https://github.com/Hans-Halverson/myte/actions/workflows/ci.yml/badge.svg)](https://github.com/Hans-Halverson/myte/actions/workflows/ci.yml)

Myte is an in-progress modern, general-purpose programming language. This repository contains Myte's compiler, standard library, and tooling.

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

## Architecture

The following is a description of the overall architecture of the Myte compiler. The compiler is located within the `src` directory and consists of four main phases - parsing, type checking and static analysis, compilation to an intermediate representation, and generation of assembly. Phases are separated as much as possible, and overall compilation is run by the compiler driver in [`src/driver`](src/driver).

### Parsing

The Myte parser is a handwritten recursive descent parser contained in the [`src/parser`](src/parser) directory. Tokenization is performed using a lexer generated using the [`sedlex`](https://github.com/ocaml-community/sedlex) library (the sole library dependency of the Myte compiler). The parser directory also contains the definition for the [Myte AST](src/parser/ast.ml), and utilities for traversing and transforming the AST.

### Type Checking and Static Analysis

[`src/analyze`](src/analyze)

### Intermediate Representation and Optimization

[`src/mir`](src/mir)

### Assembly Generation

[`src/asm/x86`](src/asm/x86)

## Tests

The Myte test suite is run by a custom test harness (`./scripts/run_tests`), and primarily consists of snapshot tests that test multiple stages of the compiler. Specific tests can be run with the `--filter` option, and snapshot tests can be re-recorded with the `--record` option. Run `./scripts/run_tests --help` for more options.

The compiler is built and all tests are run on both MacOS and Linux via Github Actions on each commit.

The following is an overview of the tests contained within the `test` directory:

- [`test/analyze`](test/analyze) Tests for various static analysis and type checking passes. Snapshot tests for the transformed AST after name resolution are generated with the `--dump-resolved-ast` option, all other analysis tests check generated errors (or a lack of errors).
- [`test/asm/x86`](test/asm/x86) Tests for x86_64 assembly generation. Snapshot tests contain a textual representation of x86_64 assembly generated with the `--dump-asm` option.
- [`test/mir`](test/mir) Tests for MIR (Myte Intermediate Representation) generation and optimization. Snapshot tests contain a textual representation of MIR and are generated with the `--dump-ir` and `--dump-ir-transforms` options.
- [`test/parser`](test/parser) Tests for the parser and lexer. Snapshot tests contain a textual representation of the AST generated from the `--dump-ast` option.
- [`test/program`](test/program) Tests for full, end to end compilation and running of Myte programs to verify run time functionality.
- [`test/cli`](test/cli) Miscellaneous tests for CLI usage of the `myte` binary.
- [`test/test`](test/test) Contains the implementation of Myte's custom test harness.
