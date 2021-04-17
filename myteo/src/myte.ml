open Basic_collections

let print_errors errors =
  Printf.printf "%s" (String.concat "\n" (List.map (fun (loc, err) -> Error_pp.pp loc err) errors))

let print_error_message msg = print_string (Error_pp.print_message_line msg)

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

let pp_irs irs =
  let ir_strings = List.map Mir_pp.pp_program irs in
  String.concat "\n" ir_strings

let rec compile files =
  (* Parse files *)
  let asts = parse_files files in
  if Opts.dump_ast () then (
    pp_asts asts;
    exit 0
  );
  (* Perform analysis passes *)
  match Lex_analyze.analyze_modules asts with
  | Error errors ->
    print_analyze_errors errors;
    exit 1
  | Ok program_cx ->
    if Opts.dump_resolved_ast () then begin
      Lex_analyze.(pp_asts program_cx.modules);
      exit 0
    end;
    if Opts.check () then exit 0;
    (* Lower to IR *)
    let program_cf_ir = Emit.emit_control_flow_ir program_cx in
    let program_ssa_ir = Ssa.control_flow_ir_to_ssa program_cx program_cf_ir in
    if Opts.dump_ir () then dump_ir program_ssa_ir;
    let ir =
      if Opts.optimize () then
        Mir_optimize.optimize program_ssa_ir
      else
        Mir_optimize.transform_for_assembly program_ssa_ir
    in
    let destructed_ir = Ssa_destruction.destruct_ssa ir in
    (* Generate executable *)
    let executable = X86_gen.gen_x86_executable destructed_ir in
    let executable_file = X86_pp.pp_x86_executable executable in
    if Opts.dump_asm () then begin
      print_string executable_file;
      exit 0
    end;
    let output_file =
      match Opts.output_file () with
      | None ->
        print_error_message "Must specify output file with -o option";
        exit 1
      | Some output_file -> output_file
    in
    (* Write executable file *)
    (try
       let out_chan = open_out output_file in
       output_string out_chan executable_file;
       close_out out_chan
     with Sys_error err ->
       print_error_message err;
       exit 1);
    (* Compile with assembler *)
    let quoted_output_file = Filename.quote output_file in
    let ret = Sys.command (Printf.sprintf "as -o %s %s" quoted_output_file quoted_output_file) in
    if ret <> 0 then (
      print_error_message (Printf.sprintf "Assembler failed with exit code %d" ret);
      exit 1
    );
    (* Link *)
    let ret =
      Sys.command (Printf.sprintf "ld -lSystem -o %s %s" quoted_output_file quoted_output_file)
    in
    if ret <> 0 then (
      print_error_message (Printf.sprintf "Linker failed with exit code %d" ret);
      exit 1
    )

and dump_ir ir =
  let open Mir_optimize in
  let transforms =
    Opts.dump_ir_transforms () |> List.filter_map MirTransform.of_string |> MirTransformSet.of_list
  in
  let ir =
    if not (MirTransformSet.is_empty transforms) then
      Mir_optimize.apply_transforms ir transforms
    else if Opts.optimize () then
      Mir_optimize.optimize ir
    else
      ir
  in
  print_string (Mir_pp.pp_program ir);
  exit 0

let () =
  let files = ref SSet.empty in
  Arg.parse Opts.spec (fun file -> files := SSet.add file !files) Opts.usage_message;
  if SSet.is_empty !files then (
    Arg.usage Opts.spec Opts.usage_message;
    exit 1
  );
  compile !files
