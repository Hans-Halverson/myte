module SSet = Set.Make (String)

let show_ast files =
  SSet.iter
    (fun file ->
      let (ast, errors) = Parser.parse_file file in
      if errors = [] then
        let pp_ast = Ast_pp.pp_program ast in
        Printf.printf "%s\n%s" (Files.strip_root file) pp_ast
      else
        List.iter (fun err -> Printf.printf "%s\n\n" (Parse_error.print err)) errors)
    !files

let () =
  let files = ref SSet.empty in
  Arg.parse Opts.spec (fun file -> files := SSet.add file !files) "Myte programming language";
  if Opts.show_ast () then show_ast files
