type error =
  | NoInstallationFound
  | MalformedInstallation of string * string

let mytepath_env_variable = "MYTEPATH"

let installation_error_help_message =
  "\n\nThere may have been an error during installation. Reinstall Myte or provide a path to a valid Myte installation by setting the `$MYTEPATH` environment variable or the `--installation` command line option."

let pp_error err =
  match err with
  | NoInstallationFound ->
    "Could not find Myte installation on system." ^ installation_error_help_message
  | MalformedInstallation (path, message) ->
    Printf.sprintf
      "Myte installation at %s is malformed. %s.%s"
      path
      message
      installation_error_help_message

let error err =
  print_string (Error_pp.print_message_line (pp_error err));
  exit 1

let installation_path = ref "<uninitialized>"

let build_stdlib_path installation_path = Files.join_parts [installation_path; "lib"; "stdlib"]

let build_runtime_path installation_path =
  Files.join_parts [installation_path; "lib"; Target.(target_triple !target); "runtime"]

let build_gc_path installation_path =
  Files.join_parts [installation_path; "lib"; Target.(target_triple !target); "gc"]

let get_stdlib_path () = build_stdlib_path !installation_path

let get_runtime_path () = build_runtime_path !installation_path

let get_gc_path () = build_gc_path !installation_path

let lib_myte_file () = Filename.concat (get_runtime_path ()) "libmyte.a"

let lib_gc_file () = Filename.concat (get_gc_path ()) "libgc.a"

let check_installation_directories path directories =
  let is_directory path = Sys.file_exists path && Sys.is_directory path in
  List.fold_left
    (fun acc directory ->
      match acc with
      | None when not (is_directory directory) ->
        let message = Printf.sprintf "Expected a directory at %s" directory in
        Some (MalformedInstallation (path, message))
      | _ -> acc)
    None
    directories

let init () =
  let check_is_valid_installation path =
    (* Verify that all necessary directories exist *)
    let directories = [path; Filename.concat path "lib"; build_stdlib_path path] in
    let err_opt = check_installation_directories path directories in
    match err_opt with
    | None -> installation_path := path
    | Some err -> error err
  in

  match Opts.installation_path () with
  | Some path -> check_is_valid_installation path
  | None ->
    (match Sys.getenv_opt mytepath_env_variable with
    | Some path -> check_is_valid_installation path
    | None -> error NoInstallationFound)

let init_runtime () =
  (* Verify that all the necessary runtime directories exist *)
  let path = !installation_path in
  let directories = [build_runtime_path path; build_gc_path path] in
  let err_opt = check_installation_directories path directories in
  Option.iter error err_opt
