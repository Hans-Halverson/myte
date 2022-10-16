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

(* Stages of compilation *)

let rec parse_and_check_stdlib () =
  let pcx = Program_context.mk_pcx () in
  let stdlib_files = Std_lib.get_stdlib_files (Installation.get_stdlib_path ()) in
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
  | Ok () -> ()

let lower_to_ir pcx = Mir_emit.emit pcx

let transform_ir ~pcx program =
  let dump_ir () =
    print_string (Mir_pp.pp_program program);
    exit 0
  in

  if Opts.dump_untransformed_ir () then dump_ir ();

  (* Dump IR when sequence of transforms is specified *)
  let transforms =
    Opts.dump_transformed_ir () |> List.filter_map Mir_transforms.MirTransform.of_string
  in
  if transforms <> [] then (
    Mir_transforms.apply_transforms ~program ~pcx transforms;
    dump_ir ()
  );

  (* Generic IR dumps *)
  if Opts.dump_ir () then (
    if Opts.optimize () then
      Mir_transforms.optimize ~program ~pcx
    else
      Mir_transforms.transform_for_dump_ir ~program ~pcx;

    dump_ir ()
  );

  if Opts.optimize () then
    Mir_transforms.optimize ~program ~pcx
  else
    Mir_transforms.non_optimized_transforms ~program ~pcx;

  Mir_transforms.transform_for_assembly ~program ~pcx

let lower_to_asm ir =
  (* Generate textual assembly for program *)
  let assembly_text =
    match Target.target_architecture () with
    | X86_64 ->
      let gcx = X86_64_gen.gen_program ir in
      X86_64_pp.pp_program ~gcx
    | AArch64 -> ".text\n_stub:\nret\n"
  in
  let output_file =
    match Opts.output_file () with
    | None ->
      print_error_message "Must specify output file with -o option";
      exit 1
    | Some output_file -> output_file
  in
  let output_file = output_file ^ ".S" in

  (* Write textual assembly to output file *)
  try
    let out_chan = open_out output_file in
    output_string out_chan assembly_text;
    close_out out_chan;
    output_file
  with
  | Sys_error err ->
    print_error_message err;
    exit 1

let gen_executable asm_file =
  match Target.target_system () with
  | Target.Darwin -> Driver_mac.gen_executable asm_file
  | Linux -> Driver_linux.gen_executable asm_file

let compile files =
  let pcx = parse_and_check_stdlib () in
  parse_and_check ~pcx ~is_stdlib:false files;
  if Opts.check () then exit 0;

  let ir = lower_to_ir pcx in
  transform_ir ~pcx ir;

  let asm_file = lower_to_asm ir in
  gen_executable asm_file
