open Driver_utils

let mk_assembler_command asm_file obj_file =
  Printf.sprintf "as -o %s %s" (Filename.quote asm_file) (Filename.quote obj_file)

let mk_linker_command output_file program_obj_file runtime_obj_file =
  Printf.sprintf
    "ld -e _start -o %s %s %s"
    (Filename.quote output_file)
    (Filename.quote program_obj_file)
    (Filename.quote runtime_obj_file)

let gen_executable program_asm_file =
  let output_file = Option.get (Opts.output_file ()) in
  let program_obj_file = output_file ^ ".program" in

  (* Compile program and runtime with system assembler *)
  let assemble_file input_asm_file output_obj_file =
    let ret = Sys.command (mk_assembler_command output_obj_file input_asm_file) in
    if ret <> 0 then (
      print_error_message (Printf.sprintf "Assembler failed with exit code %d" ret);
      exit 1
    )
  in
  assemble_file program_asm_file program_obj_file;
  Sys.remove program_asm_file;

  let runtime_asm_file = X86_runtime.linux_runtime_file () in
  let runtime_obj_file = output_file ^ ".runtime" in
  assemble_file runtime_asm_file runtime_obj_file;

  (* Link using system linker *)
  let ret = Sys.command (mk_linker_command output_file program_obj_file runtime_obj_file) in
  Sys.remove program_obj_file;
  Sys.remove runtime_obj_file;
  Sys.remove runtime_asm_file;
  if ret <> 0 then (
    print_error_message (Printf.sprintf "Linker failed with exit code %d" ret);
    exit 1
  )
