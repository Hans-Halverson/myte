type t = {
  dump_ast: bool ref;
  dump_resolved_ast: bool ref;
  print_plain: bool ref;
}

let opts = { dump_ast = ref false; dump_resolved_ast = ref false; print_plain = ref false }

let spec =
  [
    ("--dump-ast", Arg.Set opts.dump_ast, " Print the AST to stdout");
    ("--dump-resolved-ast", Arg.Set opts.dump_resolved_ast, " Print the resolved AST to stdout");
    ("--no-pretty-print", Arg.Set opts.print_plain, " Do not pretty print output");
  ]
  |> Arg.align

let dump_ast () = !(opts.dump_ast)

let dump_resolved_ast () = !(opts.dump_resolved_ast)

let print_plain () = !(opts.print_plain)
