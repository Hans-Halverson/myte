module SSet = Set.Make(String)

let () =
  let files = ref SSet.empty in
  Arg.parse [] (fun file -> files := SSet.add file !files) "Myte programming language";
  SSet.iter
    (fun file ->
       let (ast, errors) = Parser.parse_file file in
       if errors = [] then
         let pp_ast = Ast_pp.pp_program ast in
         Printf.printf "%s\n%s" file pp_ast
       else
         List.iter (fun err -> Printf.printf "%s\n" (Parse_error.print err)) errors)
    !files