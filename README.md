# myte

[![tests](https://github.com/Hans-Halverson/myte/actions/workflows/ci.yml/badge.svg)](https://github.com/Hans-Halverson/myte/actions/workflows/ci.yml)

Myte is an in-progress modern, general-purpose programming language. This repository contains Myte's compiler, standard library, and tooling.

You can play around with the Myte language on the Myte Playground at https://mytelang.org/playground/.

## Building and testing the compiler

The Myte compiler is written in OCaml (version 4.09.1) with some runtime components in C. To build the compiler from source and run its test suite, check out this repository and run the following commands from the `myte` repository's root directory.

### Initial install

First install the OCaml package manager [opam](https://opam.ocaml.org/doc/Install.html) and the C compiler [clang](https://releases.llvm.org/download.html). Then navigate to the checked out `myte` directory and run the following commands, which will set up opam and install the correct version of the compiler and dependencies for Myte.

```
opam init
opam switch create 4.09.1
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

The Myte parser is a handwritten recursive descent parser contained in the [`src/parser`](src/parser) directory. The parser directory also contains the definition for the [Myte AST](src/parser/ast.ml) along with utilities for traversing and transforming the AST. Errors are reported after parsing, and only if there are no parse or tokenization errors does the compiler proceed to the next phase.

### Type Checking and Static Analysis

The next phase of the Myte compiler is determining correctness of successfully parsed programs via static analysis, contained in the [`src/analyze`](src/analyze) directory. There are many correctness checks that occur during this phase, but the three most significant parts are name resolution (resolving uses of identifiers to their definitions), type checking with inference, and pattern matching exhaustiveness and usefulness analysis.

Name resolution begins with collecting all modules and their exported items, and verifying that there are no conflicting modules or exported names so that every import statement can be unambiguously resolved. Next, all imports are resolved and every identifier in the program is matched with its definition. This process also transforms the AST so that ambiguous chains of accesses are split into the portion that is a module reference and the portion that is an accessed field (e.g. `foo.bar.baz` could refer to field `baz` of global variable `bar` in module `foo`, or could refer to subfield `bar.baz` of the variable `foo` which is in scope). Errors are reported for any issues during name resolution, and only if there are errors does the compiler proceed to type checking.

At a high level, type checking and inference works by creating a lattice of type variables for all typed items (declarations, expression nodes, etc.), then applying unification and subtype relations between types based on how they are used. This allows for basic local type inference so that annotations for local variables often do not need to be supplied. Type checking begins by constructing the hierarchy of traits and determining which methods are available for each trait and type due to successful implementation of super traits. Type checking then proceeds to create and check constraints across all AST nodes, generating errors for any incompatible uses of types that are present. This phase also resolves names that depend on type information, such as methods. Finally, type checking infers the type of integer literals if they could not already be inferred, and verifies that the type of all expressions could be determined.

The last analysis phase is pattern matching exhaustiveness and reachability checking. Exhaustiveness analysis verifies that every possible argument to pattern matching (of the determined type) will be matched by at least one of the cases. Reachability analysis verifies that every case (and subpattern) is able to match at least one possible input, meaning an earlier case (or subpattern) does not entirely shadow that case. For instance, any pattern after a wildcard pattern is unreachable (e.g. the second clause of `_ -> 1 | true -> 2`), or in more complicated patterns like `(true, _) | (_, false) | (true, false) -> 1` where the third subpattern in the or-pattern is unreachable. Note these same checks are also applied to destructuring during variable declaration or assignment statements.

Other analysis passes include control flow analysis to verify that all statements are reachable and all control flow paths end in a return statement when necessary. This concludes the front end of the compiler - a program that passes all analysis checks is considered correct and may proceed to code generation.

### Intermediate Representation and Optimization

The first phase of the compiler back end is to lower the program into Myte Intermediate Representation (MIR), contained in the[`src/mir`](src/mir) directory. MIR is in single static assignment (SSA) form and is very similar to LLVM IR, essentially being a simplified subset of LLVM IR needed for the Myte compiler.

The first phase is lowering the Myte AST to MIR. This is mostly a straightforward translation, with the main sources of complexity occurring in the handling of layouts for algebraic data types (in particular variant types), the compilation of pattern matching, and monomorphization of generics.

To lower pattern matching, a decision tree is first built using heuristics to minimize the number of tests and resulting tree size. This decision tree is then visited with MIR generated for each test and branch.

Myte supports generics in the form of parametric polymorphism with trait bounds. Generic types and methods are monomorphized when lowering to MIR, meaning separate copies are created for each instantiation of the generic type or method with a different set of type arguments.

The initial MIR that is emitted allocates space on the stack for each local variable. Immediately after creation this version of MIR is "SSA-ified", where these stack slots are promoted to SSA variables within MIR that are joined using phi nodes (corresponding to LLVM's `mem2reg` pass). MIR will remain in SSA form until right before assembly generation, at which point the SSA is "destructed", where phi nodes are replaced with copies to a shared variable in previous blocks. SSA destruction makes sure to insert blocks on critical edges if necessary, and to correctly sequence copies (possibly with the addition of temporary variables) to avoid clobbering the result of other phi nodes as all phi nodes execute in parallel.

Optimizations on MIR can be run once the SSA pass has been completed. At the moment the only notable optimization is constant folding and propagation with branch pruning. The MIR is also simplified whenever possible to consolidate blocks and eliminate unnecessary variables. Once all optimizations and simplification have completed, the MIR is ready to be lowered to assembly.

### Assembly Generation

The final phase of the compiler is lowering destructed MIR to assembly. Myte currently supports generating `x86_64` assembly for both MacOS and Linux, contained in the [`src/asm/x86`](src/asm/x86) directory.

Assembly generation begins with a pass to generate "virtual assembly" from MIR, which is identical to the target assembly language but with unlimited virtual registers, only some of which are mapped directly to physical registers. Once virtual assembly has been generated, liveness information for each virtual register is determined and an interference graph between virtual registers is built. The register allocator then assigns physical registers to each virtual register using the strategy of iterated register coalescing, which attempts to eliminate as many copy instructions as possible from the program. When spills are determined, virtual registers are mapped to virtual stack slots and the program is rewritten to read/write from memory on uses of that virtual register.

Once all registers have been either assigned a physical register or mapped to a virtual stack slot, we then perform stack slot coloring to minimize the stack space used for spilled register in each function. This pass first calculates liveness information and interferences for all virtual stack slots, then greedily maps non-interfering virtual stack slots to the same phyiscal stack slot. At this point every variable from MIR has been assigned a physical location, whether it be in a register or in memory.

The resulting assembly is then simplified, peephole optimizations are run, and unnecessary instructions are removed. This leaves the assembly in its final form.

Finally, the generated assembly is written to a file then assembled and linked to the minimal Myte runtime with the system's assembler and linker, generating an executable.

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

## Tools

Myte has a VSCode plugin contained within the [`tools/vscode-extensions`](tools/vscode-extensions) directory that adds support for the Myte language to VSCode. This plugin includes a basic language configuation along with a TextMate grammar that adds syntax highlighting for the Myte language to VSCode. This plugin has not yet been published, so to use it locally copy the `tools/vscode-extensions/myte` directory to your VSCode extensions directory.
