open Driver_utils

let mk_assembler_command asm_file obj_file =
  Printf.sprintf "%s -o %s %s" assembler_path (Filename.quote asm_file) (Filename.quote obj_file)

let mk_linker_command output_file program_obj_file lib_myte_file lib_gc_file =
  (* On macOS Big Sur (11.0) and later system library is at /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib/
     For earlier versions this directory does not exist, so always specify it. *)
  Printf.sprintf
    "%s -L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib/ -lSystem -e _start -o %s %s %s %s"
    linker_path
    (Filename.quote output_file)
    (Filename.quote program_obj_file)
    (Filename.quote lib_myte_file)
    (Filename.quote lib_gc_file)

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

  let lib_myte_file = Installation.lib_myte_file () in
  let lib_gc_file = Installation.lib_gc_file () in

  (* Link using system linker *)
  let ret =
    Sys.command (mk_linker_command output_file program_obj_file lib_myte_file lib_gc_file)
  in
  Sys.remove program_obj_file;
  if ret <> 0 then (
    print_error_message (Printf.sprintf "Linker failed with exit code %d" ret);
    exit 1
  )
