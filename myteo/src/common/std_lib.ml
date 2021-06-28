open Basic_collections

type error =
  | NoStdlibPath
  | StdlibPathNotDirectory of string

let mytepath_env_variable = "MYTEPATH"

let myte_file_extension = ".myte"

let stdlib_error_help_msg =
  "Provide a path to the standard library by setting the `$MYTEPATH` environment variable or the `--stdlib` command line option."

let pp_error err =
  match err with
  | NoStdlibPath -> "Could not determine path to standard library. " ^ stdlib_error_help_msg
  | StdlibPathNotDirectory path ->
    Printf.sprintf
      "Path to standard library determined to be `%s`, but there is no directory at that path. %s"
      path
      stdlib_error_help_msg

let has_stdlib_prefix name = String.length name >= 4 && String.sub name 0 4 = "std."

let get_stdlib_path () =
  let check_is_directory path =
    if (not (Sys.file_exists path)) || not (Sys.is_directory path) then
      Error (StdlibPathNotDirectory path)
    else
      Ok path
  in
  match Opts.stdlib_path () with
  | Some path -> check_is_directory path
  | None ->
    (match Sys.getenv_opt mytepath_env_variable with
    | Some path -> check_is_directory path
    | None -> Error NoStdlibPath)

let get_stdlib_files stdlib_path =
  let myte_files = ref SSet.empty in
  let rec gather_myte_files_in_directory dir =
    let files = Sys.readdir dir |> Array.to_list in
    List.iter
      (fun file ->
        let file = Filename.concat dir file in
        if Sys.is_directory file then
          gather_myte_files_in_directory file
        else if Filename.check_suffix file myte_file_extension then
          myte_files := SSet.add file !myte_files)
      files
  in
  gather_myte_files_in_directory stdlib_path;
  !myte_files

(* Stdlib names *)

let std_array_array = "std.array.Array"

let std_array_new = "std.array.new"

let std_io_write = "std.io.write"

let std_string_string = "std.string.String"

let all_stdlib_names =
  SSet.of_list [std_array_array; std_array_new; std_io_write; std_string_string]

(* Stdlib types *)

let string_adt_sig = ref Types.empty_adt_sig

let mk_string_type () = Types.ADT { adt_sig = !string_adt_sig; type_args = [] }

(* Stdlib registration *)

let stdlib_builtin_decl_locs = ref LocMap.empty

let stdlib_builtin_name_to_decl_loc = ref SMap.empty

let lookup_stdlib_name decl_loc = LocMap.find_opt decl_loc !stdlib_builtin_decl_locs

let lookup_stdlib_decl_loc name = SMap.find name !stdlib_builtin_name_to_decl_loc

let register_stdlib_decl full_name loc =
  if SSet.mem full_name all_stdlib_names then (
    stdlib_builtin_decl_locs := LocMap.add loc full_name !stdlib_builtin_decl_locs;
    stdlib_builtin_name_to_decl_loc := SMap.add full_name loc !stdlib_builtin_name_to_decl_loc
  )

let register_stdlib_type loc adt_sig =
  match lookup_stdlib_name loc with
  | Some name when name = std_string_string -> string_adt_sig := adt_sig
  | _ -> ()
