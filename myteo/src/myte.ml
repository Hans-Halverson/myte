module SSet = Set.Make (String)

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

let show_ast files =
  let asts = parse_files files in
  let pp_asts =
    List.map
      (fun (file, ast) ->
        let pp_ast = Ast_pp.pp_program ast in
        Printf.sprintf "%s\n%s" (Files.strip_root file) pp_ast)
      asts
  in
  Printf.printf "%s" (String.concat "\n" pp_asts)

let compile files =
  let asts = parse_files files |> List.map snd in
  let errors = Lex_analyze.analyze_programs asts in
  if errors <> [] then (
    print_analyze_errors errors;
    exit 1
  )

let () =
  let files = ref SSet.empty in
  Arg.parse Opts.spec (fun file -> files := SSet.add file !files) "Myte programming language";
  if Opts.show_ast () then
    show_ast !files
  else
    compile !files
