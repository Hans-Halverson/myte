open Basic_collections
open Driver_utils

(* Top level driver for Myte. Parsers, analyzes, generates MIR, optimizes, and then generates
   textual x86 assembly for all hosts. After textual x86 assembly has been generated, defers to
   host-specific drivers for final calls to host's assembler and linker. *)

(* Utilities for printing output at intermediate stages of the compiler *)

let dump_ast asts =
  let ast_strings =
    List.map
      (fun (file, ast) ->
        let pp_ast = Ast_pp.pp_module ast in
        Printf.sprintf "%s\n%s" (Files.strip_root file) pp_ast)
      asts
  in
  Printf.printf "%s" (String.concat "\n" ast_strings)

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

(* Stages of compilation *)

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

and parse_files files =
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
    print_errors (List.map (fun (loc, err) -> (loc, Parse_error.to_string err)) errors);
    exit 1
  ) else
    asts

and parse_and_check ~pcx ~is_stdlib files =
  (* Parse files *)
  let asts = parse_files files in
  if (not is_stdlib) && Opts.dump_ast () then (
    dump_ast asts;
    exit 0
  );

  (* Perform analysis passes *)
  match Lex_analyze.analyze_modules ~pcx ~is_stdlib asts with
  | Error errors ->
    print_errors (List.map (fun (loc, err) -> (loc, Analyze_error.to_string err)) errors);
    exit 1
  | Ok resolved_modules ->
    if (not is_stdlib) && Opts.dump_resolved_ast () then (
      dump_ast resolved_modules;
      exit 0
    )

let lower_to_ir pcx =
  let ir = Mir_emit.emit pcx in
  if Opts.dump_pre_ssa_ir () then dump_ir ir;
  let ir = Mir_ssa.promote_variables_to_registers ir in
  if Opts.dump_ir () then dump_ir ir;
  if Opts.optimize () then
    Mir_optimize.optimize ir
  else
    Mir_optimize.transform_for_assembly ir

let lower_to_asm ir =
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
  let output_file = output_file ^ ".S" in

  (* Write textual x86 assembly to output file *)
  try
    let out_chan = open_out output_file in
    output_string out_chan x86_program_file;
    close_out out_chan;
    output_file
  with Sys_error err ->
    print_error_message err;
    exit 1

let gen_executable asm_file =
  match Target.system () with
  | Target.Darwin -> Driver_mac.gen_executable asm_file
  | Linux -> Driver_linux.gen_executable asm_file

let compile files =
  let pcx = parse_and_check_stdlib () in
  parse_and_check ~pcx ~is_stdlib:false files;
  if Opts.check () then exit 0;
  let ir = lower_to_ir pcx in
  let asm_file = lower_to_asm ir in
  gen_executable asm_file
