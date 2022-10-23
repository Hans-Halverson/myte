type system =
  (* MacOS *)
  | Darwin
  | Linux

type architecture =
  | X86_64
  | AArch64

type machine = {
  architecture: architecture;
  system: system;
}

let all_architectures = [AArch64; X86_64]

let empty_machine = { architecture = X86_64; system = Linux }

let equal (m1 : machine) (m2 : machine) : bool =
  m1.architecture == m2.architecture && m1.system == m2.system

let host = ref empty_machine

let target = ref empty_machine

let host_architecture () = !host.architecture

let target_architecture () = !target.architecture

let target_system () = !target.system

let error msg =
  print_string (Error_pp.print_message_line msg);
  exit 1

let detect_host_architecture () =
  let uname_output = Unix.open_process_in "uname -m" in
  let architecture = input_line uname_output in
  let () = close_in uname_output in
  match architecture with
  | "x86_64" -> X86_64
  | "arm64"
  | "aarch64" ->
    AArch64
  | _ -> error (Printf.sprintf "Unsupported architecture %s" architecture)

let detect_host_system () =
  let uname_output = Unix.open_process_in "uname" in
  let system = input_line uname_output in
  let () = close_in uname_output in
  match system with
  | "Darwin" -> Darwin
  | "Linux" -> Linux
  | _ -> error (Printf.sprintf "Unsupported system %s" system)

let init_host () =
  if Sys.os_type <> "Unix" then error (Printf.sprintf "Unsupported OS type %s" Sys.os_type);
  let architecture = detect_host_architecture () in
  let system = detect_host_system () in
  host := { architecture; system }

let parse_target_string target_string =
  match target_string with
  | "x86-64-apple-darwin" -> { architecture = X86_64; system = Darwin }
  | "x86-64-linux-gnu" -> { architecture = X86_64; system = Linux }
  | "aarch64-linux-gnu" -> { architecture = AArch64; system = Linux }
  | _ -> error (Printf.sprintf "Unsupported target %s" target_string)

let init_target () =
  match Opts.target () with
  | None -> target := !host
  | Some target_string -> target := parse_target_string target_string

let init () =
  init_host ();
  init_target ()

let gcc_target_triple machine =
  match machine with
  | { architecture = X86_64; system = Darwin } -> "x86-64-apple-darwin"
  | { architecture = X86_64; system = Linux } -> "x86-64-linux-gnu"
  | { architecture = AArch64; system = Linux } -> "aarch64-linux-gnu"
  | _ -> failwith "Unsupported machine"

let llvm_target_triple machine =
  match machine with
  | { architecture = X86_64; system = Darwin } -> "x86_64-apple-darwin"
  | { architecture = X86_64; system = Linux } -> "x86_64-linux-gnu"
  | { architecture = AArch64; system = Linux } -> "aarch64-linux-gnu"
  | _ -> failwith "Unsupported machine"

let target_triple machine = gcc_target_triple machine
