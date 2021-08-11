type system = Darwin

(* MacOS *)

type architecture = X86_64

type target = {
  architecture: architecture;
  system: system;
}

let target = ref None

let architecture () = (Option.get !target).architecture

let system () = (Option.get !target).system

let error msg =
  print_string (Error_pp.print_message_line msg);
  exit 1

let detect_architecture () =
  let uname_output = Unix.open_process_in "uname -m" in
  let architecture = input_line uname_output in
  let () = close_in uname_output in
  match architecture with
  | "x86_64" -> X86_64
  | _ -> error (Printf.sprintf "Unsupported architecture %s" architecture)

let detect_system () =
  let uname_output = Unix.open_process_in "uname" in
  let system = input_line uname_output in
  let () = close_in uname_output in
  match system with
  | "Darwin" -> Darwin
  | _ -> error (Printf.sprintf "Unsupported system %s" system)

let detect () =
  if Sys.os_type <> "Unix" then error (Printf.sprintf "Unsupported OS type %s" Sys.os_type);
  let architecture = detect_architecture () in
  let system = detect_system () in
  target := Some { architecture; system }
