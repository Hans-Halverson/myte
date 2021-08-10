open Basic_collections

let print_errors errors =
  let errors = List.sort (fun (loc1, _) (loc2, _) -> Loc.compare loc1 loc2) errors in
  Printf.printf
    "%s"
    (String.concat
       "\n"
       (List.map
          (fun (loc, err) ->
            if loc <> Loc.none then
              Error_pp.pp loc err
            else
              Error_pp.print_message_line err)
          errors))

let print_error_message msg = print_string (Error_pp.print_message_line msg)

let print_parse_errors errors =
  print_errors (List.map (fun (loc, err) -> (loc, Parse_error.to_string err)) errors)

let print_analyze_errors errors =
  print_errors (List.map (fun (loc, err) -> (loc, Analyze_error.to_string err)) errors)

let parse_files files =
  let (asts, errors) =
    SSet.fold
      (fun file (asts, errors) ->
        match Parser.parse_file file with
        | Ok ast -> ((file, ast) :: asts, errors)
        | Error parse_errors -> (asts, parse_errors :: errors))
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

let rec parse_and_check_stdlib () =
  match Std_lib.get_stdlib_path () with
  | Error err ->
    print_error_message (Std_lib.pp_error err);
    exit 1
  | Ok stdlib_path ->
    let pcx = Program_context.mk_pcx () in
    let stdlib_files = Std_lib.get_stdlib_files stdlib_path in
    parse_and_check ~pcx ~is_stdlib:true stdlib_files;
    pcx

and parse_and_check ~pcx ~is_stdlib files =
  (* Parse files *)
  let asts = parse_files files in
  if (not is_stdlib) && Opts.dump_ast () then (
    pp_asts asts;
    exit 0
  );
  (* Perform analysis passes *)
  match Lex_analyze.analyze_modules ~pcx ~is_stdlib asts with
  | Error errors ->
    print_analyze_errors errors;
    exit 1
  | Ok resolved_modules ->
    if (not is_stdlib) && Opts.dump_resolved_ast () then (
      pp_asts resolved_modules;
      exit 0
    )

let dump_ir ir =
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
      Mir_optimize.transform_for_dump_ir ir
  in
  print_string (Mir_pp.pp_program ir);
  exit 0

let lower_to_asm pcx =
  (* Lower to IR *)
  let (ecx, program_cf_ir) = Mir_emit.emit_control_flow_ir pcx in
  let program_ssa_ir = Mir_ssa.control_flow_ir_to_ssa pcx ecx program_cf_ir in
  if Opts.dump_ir () then dump_ir program_ssa_ir;
  let ir =
    if Opts.optimize () then
      Mir_optimize.optimize program_ssa_ir
    else
      Mir_optimize.transform_for_assembly program_ssa_ir
  in
  let destructed_ir = Mir_ssa_destruction.destruct_ssa ir in
  (* Generate x86 program  *)
  let gcx = X86_gen.gen_x86_program destructed_ir in
  let x86_program_file = X86_pp.pp_x86_program ~gcx in
  let output_file =
    match Opts.output_file () with
    | None ->
      print_error_message "Must specify output file with -o option";
      exit 1
    | Some output_file -> output_file
  in
  (* Write x86 file *)
  try
    let out_chan = open_out output_file in
    output_string out_chan x86_program_file;
    close_out out_chan;
    output_file
  with Sys_error err ->
    print_error_message err;
    exit 1

let compile_and_link_asm output_file =
  let assemble_file input_file output_file =
    let ret = Sys.command (Printf.sprintf "as -o %s %s" output_file input_file) in
    if ret <> 0 then (
      print_error_message (Printf.sprintf "Assembler failed with exit code %d" ret);
      exit 1
    )
  in
  (* Compile program and runtime with assembler *)
  let program_output_file = Filename.quote output_file in
  assemble_file program_output_file program_output_file;
  let runtime_output_file = Filename.quote (output_file ^ ".runtime") in
  assemble_file (Filename.quote X86_runtime.macos_runtime_file_path) runtime_output_file;
  (* Link *)
  let ret =
    Sys.command
      (Printf.sprintf
         "ld -lSystem -e _start -o %s %s %s"
         program_output_file
         program_output_file
         runtime_output_file)
  in
  ignore (Sys.command (Printf.sprintf "rm -f %s" runtime_output_file));
  if ret <> 0 then (
    print_error_message (Printf.sprintf "Linker failed with exit code %d" ret);
    exit 1
  )

let compile files =
  let pcx = parse_and_check_stdlib () in
  parse_and_check ~pcx ~is_stdlib:false files;
  if Opts.check () then exit 0;
  let out_file = lower_to_asm pcx in
  compile_and_link_asm out_file

let () =
  let files = ref SSet.empty in
  Arg.parse Opts.spec (fun file -> files := SSet.add file !files) Opts.usage_message;
  if SSet.is_empty !files then (
    Arg.usage Opts.spec Opts.usage_message;
    exit 1
  );
  compile !files
