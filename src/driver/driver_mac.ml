open Driver_utils

let gen_executable output_file =
  (* Compile program and runtime with system assembler *)
  let assemble_file input_file output_file =
    let ret = Sys.command (Printf.sprintf "as -o %s %s" output_file input_file) in
    if ret <> 0 then (
      print_error_message (Printf.sprintf "Assembler failed with exit code %d" ret);
      exit 1
    )
  in
  let program_output_file = Filename.quote output_file in
  assemble_file program_output_file program_output_file;
  let runtime_output_file = Filename.quote (output_file ^ ".runtime") in
  assemble_file (Filename.quote X86_runtime.macos_runtime_file_path) runtime_output_file;

  (* Link using system linker *)
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
