type t = {
  check: bool ref;
  dump_ast: bool ref;
  dump_resolved_ast: bool ref;
  dump_ir: bool ref;
  print_plain: bool ref;
}

let opts =
  {
    check = ref false;
    dump_ast = ref false;
    dump_resolved_ast = ref false;
    dump_ir = ref false;
    print_plain = ref false;
  }

let spec =
  [
    ("--check", Arg.Set opts.check, " Check program for errors but do not compile");
    ("--dump-ast", Arg.Set opts.dump_ast, " Print the AST to stdout");
    ("--dump-resolved-ast", Arg.Set opts.dump_resolved_ast, " Print the resolved AST to stdout");
    ("--dump-ir", Arg.Set opts.dump_ir, " Print the IR to stdout");
    ("--no-pretty-print", Arg.Set opts.print_plain, " Do not pretty print output");
  ]
  |> Arg.align

let check () = !(opts.check)

let dump_ast () = !(opts.dump_ast)

let dump_resolved_ast () = !(opts.dump_resolved_ast)

let dump_ir () = !(opts.dump_ir)

let print_plain () = !(opts.print_plain)
