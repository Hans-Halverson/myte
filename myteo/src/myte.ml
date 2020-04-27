module SSet = Set.Make(String)

type opts = {
  show_ast: bool ref;
}

let opts = { show_ast = ref false }

let spec = [
  ("--show-ast", Arg.Set opts.show_ast, "Print the AST to stdout")
]
  |> Arg.align

let absolute_root = Sys.getcwd ()

let normalize path =
  let path_length = String.length path in
  let root_length = String.length absolute_root + 1 in
  String.sub path root_length (path_length - root_length)

let show_ast files =
  SSet.iter
    (fun file ->
       let (ast, errors) = Parser.parse_file file in
       if errors = [] then
         let pp_ast = Ast_pp.pp_program ast in
         Printf.printf "%s\n%s" (normalize file) pp_ast
       else
         List.iter (fun err -> Printf.printf "%s\n" (Parse_error.print err)) errors)
    !files

let () =
  let files = ref SSet.empty in
  Arg.parse spec (fun file -> files := SSet.add file !files) "Myte programming language";
  if !(opts.show_ast) then
    show_ast files