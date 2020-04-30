type t = {
  show_ast: bool ref;
  print_plain: bool ref;
}

let opts = {
  show_ast = ref false;
  print_plain = ref false;
}

let spec = [
  ("--show-ast", Arg.Set opts.show_ast, " Print the AST to stdout");
  ("--no-pretty-print", Arg.Set opts.print_plain, " Do not pretty print output");
]
  |> Arg.align

let show_ast () = !(opts.show_ast)

let print_plain () = !(opts.print_plain)
