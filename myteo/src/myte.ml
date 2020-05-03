module SSet = Set.Make (String)

let print_errors errors =
  Printf.printf
    "%s"
    (String.concat "\n\n" (List.map (fun (loc, err) -> Error_pp.pp loc err) errors))

let print_parse_errors errors =
  print_errors (List.map (fun (loc, err) -> (loc, Parse_error.to_string err)) errors)

let print_analyze_errors errors =
  print_errors (List.map (fun (loc, err) -> (loc, Analyze_error.to_string err)) errors)

let show_ast files =
  SSet.iter
    (fun file ->
      let (ast, errors) = Parser.parse_file file in
      if errors <> [] then (
        print_parse_errors errors;
        exit 1
      );
      let pp_ast = Ast_pp.pp_program ast in
      Printf.printf "%s\n%s" (Files.strip_root file) pp_ast)
    files

let compile files =
  SSet.iter
    (fun file ->
      let (ast, errors) = Parser.parse_file file in
      if errors <> [] then begin
        print_parse_errors errors;
        exit 1
      end;
      let errors = Lex_analyze.analyze_program ast in
      if errors <> [] then print_analyze_errors errors)
    files

let () =
  let files = ref SSet.empty in
  Arg.parse Opts.spec (fun file -> files := SSet.add file !files) "Myte programming language";
  if Opts.show_ast () then
    show_ast !files
  else
    compile !files
