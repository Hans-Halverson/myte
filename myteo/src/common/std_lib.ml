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

let std_array_array_new = "std.array.Array.new"

let std_bool_bool = "std.bool.Bool"

let std_byte_byte = "std.byte.Byte"

let std_int_int = "std.int.Int"

let std_io_write = "std.io.write"

let std_long_long = "std.long.Long"

let std_string_string = "std.string.String"

let std_unit_unit = "std.unit.Unit"

let std_vec_vec = "std.vec.Vec"

let all_stdlib_names =
  SSet.of_list
    [
      std_array_array;
      std_array_array_new;
      std_bool_bool;
      std_byte_byte;
      std_io_write;
      std_int_int;
      std_long_long;
      std_string_string;
      std_unit_unit;
      std_vec_vec;
    ]

(* Stdlib types *)

let bool_adt_sig = ref Types.AdtSig.empty

let byte_adt_sig = ref Types.AdtSig.empty

let int_adt_sig = ref Types.AdtSig.empty

let long_adt_sig = ref Types.AdtSig.empty

let string_adt_sig = ref Types.AdtSig.empty

let unit_adt_sig = ref Types.AdtSig.empty

let mk_string_type () = Types.Type.ADT { adt_sig = !string_adt_sig; type_args = [] }

let get_primitive_adt_sig (ty : Types.Type.t) =
  match ty with
  | Unit -> !unit_adt_sig
  | Bool -> !bool_adt_sig
  | Byte -> !byte_adt_sig
  | Int -> !int_adt_sig
  | Long -> !long_adt_sig
  | _ -> failwith "Can only be called on primitive type"

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
  | Some name when name = std_bool_bool -> bool_adt_sig := adt_sig
  | Some name when name = std_byte_byte -> byte_adt_sig := adt_sig
  | Some name when name = std_int_int -> int_adt_sig := adt_sig
  | Some name when name = std_long_long -> long_adt_sig := adt_sig
  | Some name when name = std_string_string -> string_adt_sig := adt_sig
  | Some name when name = std_unit_unit -> unit_adt_sig := adt_sig
  | _ -> ()
