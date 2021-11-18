const Commands = {
  Execute: {
    id: "execute",
    label: "Execute",
    description: "Compile and run the program, printing the program's output.",
  },
  Compile: {
    id: "compile",
    label: "Compile",
    description: "Compile the program without running it, printing errors.",
  },
  AST: {
    id: "dump-ast",
    label: "Show AST",
    description: "Print the program's AST, a tree of its syntactic structure.",
  },
  MIR: {
    id: "mir",
    label: "Show MIR",
    description:
      "Print the program's MIR, the Myte intermediate representation.",
  },
  Asm: {
    id: "asm",
    label: "Show Assembly",
    description: "Print the program's resulting assembly code.",
  },
};

module.exports = Commands;
