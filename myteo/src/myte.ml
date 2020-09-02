open Basic_collections

let print_errors errors =
  Printf.printf "%s" (String.concat "\n" (List.map (fun (loc, err) -> Error_pp.pp loc err) errors))

let print_parse_errors errors =
  print_errors (List.map (fun (loc, err) -> (loc, Parse_error.to_string err)) errors)

let print_analyze_errors errors =
  print_errors (List.map (fun (loc, err) -> (loc, Analyze_error.to_string err)) errors)

let parse_files files =
  let (asts, errors) =
    SSet.fold
      (fun file (asts, errors) ->
        let (ast, parse_errors) = Parser.parse_file file in
        ((file, ast) :: asts, parse_errors :: errors))
      files
      ([], [])
  in
  let asts = List.rev asts in
  let errors = List.flatten (List.rev errors) in
  if errors <> [] then (
    print_parse_errors errors;
    exit 1
  ) else
    asts

let pp_asts asts =
  let ast_strings =
    List.map
      (fun (file, ast) ->
        let pp_ast = Ast_pp.pp_module ast in
        Printf.sprintf "%s\n%s" (Files.strip_root file) pp_ast)
      asts
  in
  Printf.printf "%s" (String.concat "\n" ast_strings)

let compile files =
  let asts = parse_files files in
  if Opts.dump_ast () then (
    pp_asts asts;
    exit 0
  ) else
    let (resolved_asts, errors) = Lex_analyze.analyze_modules asts in
    if errors <> [] then (
      print_analyze_errors errors;
      exit 1
    );
    if Opts.dump_resolved_ast () then pp_asts resolved_asts

let () =
  let files = ref SSet.empty in
  Arg.parse Opts.spec (fun file -> files := SSet.add file !files) "Myte programming language";
  compile !files
