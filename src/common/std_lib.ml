open Basic_collections

let myte_file_extension = ".myte"

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

let has_std_lib_prefix name = String.length name >= 4 && String.sub name 0 4 = "std."

(* Stdlib names *)

let std_bool_bool = "std.bool.Bool"

let std_bool_bool_equals = "std.bool.Bool.equals"

let std_byte_byte = "std.byte.Byte"

let std_byte_byte_equals = "std.byte.Byte.equals"

let std_byte_byte_toInt = "std.byte.Byte.toInt"

let std_byte_byte_toLong = "std.byte.Byte.toLong"

let std_gc_getHeapSize = "std.gc.getHeapSize"

let std_int_int = "std.int.Int"

let std_int_int_equals = "std.int.Int.equals"

let std_int_int_toByte = "std.int.Int.toByte"

let std_int_int_toLong = "std.int.Int.toLong"

let std_io_file_builtin_close = "std.io.file.File.__builtin_close"

let std_io_file_builtin_open = "std.io.file.File.__builtin_open"

let std_io_file_builtin_read = "std.io.file.File.__builtin_read"

let std_io_file_builtin_unlink = "std.io.file.File.__builtin_unlink"

let std_io_file_builtin_write = "std.io.file.File.__builtin_write"

let std_iterator_iterable = "std.iterator.Iterable"

let std_long_long = "std.long.Long"

let std_long_long_equals = "std.long.Long.equals"

let std_long_long_toByte = "std.long.Long.toByte"

let std_long_long_toInt = "std.long.Long.toInt"

let std_map_map = "std.map.Map"

let std_map_map_add = "std.map.Map.add"

let std_map_map_new = "std.map.Map.new"

let std_map_map_reserve = "std.map.Map.reserve"

let std_memory_array = "std.memory.Array"

let std_memory_array_copy = "std.memory.Array.copy"

let std_memory_array_isNull = "std.memory.Array.isNull"

let std_memory_array_new = "std.memory.Array.new"

let std_never_never = "std.never.Never"

let std_ops_equatable = "std.ops.Equatable"

let std_option_none = "std.option.None"

let std_option_option = "std.option.Option"

let std_option_some = "std.option.Some"

let std_result_error = "std.result.Error"

let std_result_ok = "std.result.Ok"

let std_result_result = "std.result.Result"

let std_set_set = "std.set.Set"

let std_set_set_add = "std.set.Set.add"

let std_set_set_new = "std.set.Set.new"

let std_set_set_reserve = "std.set.Set.reserve"

let std_string_string = "std.string.String"

let std_string_tostring = "std.string.ToString"

let std_sys_exit = "std.sys.exit"

let std_sys_init = "std.sys.init"

let std_unit_unit = "std.unit.Unit"

let std_vec_vec = "std.vec.Vec"

let all_stdlib_names =
  SSet.of_list
    [
      std_bool_bool;
      std_bool_bool_equals;
      std_byte_byte;
      std_byte_byte_equals;
      std_byte_byte_toInt;
      std_byte_byte_toLong;
      std_gc_getHeapSize;
      std_io_file_builtin_close;
      std_io_file_builtin_open;
      std_io_file_builtin_read;
      std_io_file_builtin_unlink;
      std_io_file_builtin_write;
      std_iterator_iterable;
      std_int_int;
      std_int_int_equals;
      std_int_int_toByte;
      std_int_int_toLong;
      std_long_long;
      std_long_long_equals;
      std_long_long_toByte;
      std_long_long_toInt;
      std_map_map;
      std_map_map_add;
      std_map_map_new;
      std_map_map_reserve;
      std_memory_array;
      std_memory_array_copy;
      std_memory_array_isNull;
      std_memory_array_new;
      std_never_never;
      std_ops_equatable;
      std_option_none;
      std_option_option;
      std_option_some;
      std_result_error;
      std_result_ok;
      std_result_result;
      std_set_set;
      std_set_set_add;
      std_set_set_new;
      std_set_set_reserve;
      std_string_string;
      std_string_tostring;
      std_sys_exit;
      std_sys_init;
      std_unit_unit;
      std_vec_vec;
    ]

(* Stdlib types and traits *)

let array_adt_sig = ref Types.AdtSig.empty

let bool_adt_sig = ref Types.AdtSig.empty

let byte_adt_sig = ref Types.AdtSig.empty

let int_adt_sig = ref Types.AdtSig.empty

let long_adt_sig = ref Types.AdtSig.empty

let map_adt_sig = ref Types.AdtSig.empty

let never_adt_sig = ref Types.AdtSig.empty

let option_adt_sig = ref Types.AdtSig.empty

let result_adt_sig = ref Types.AdtSig.empty

let set_adt_sig = ref Types.AdtSig.empty

let string_adt_sig = ref Types.AdtSig.empty

let vec_adt_sig = ref Types.AdtSig.empty

let unit_adt_sig = ref Types.AdtSig.empty

let mk_option_type type_arg = Types.Type.ADT { adt_sig = !option_adt_sig; type_args = [type_arg] }

let mk_result_type ok_ty err_ty =
  Types.Type.ADT { adt_sig = !result_adt_sig; type_args = [ok_ty; err_ty] }

let mk_string_type () = Types.Type.ADT { adt_sig = !string_adt_sig; type_args = [] }

let equatable_trait_sig = ref Types.TraitSig.empty

let iterable_trait_sig = ref Types.TraitSig.empty

let to_string_trait_sig = ref Types.TraitSig.empty

let get_primitive_adt_sig (ty : Types.Type.t) =
  match ty with
  | Unit -> !unit_adt_sig
  | Bool -> !bool_adt_sig
  | Byte -> !byte_adt_sig
  | Int -> !int_adt_sig
  | Long -> !long_adt_sig
  | _ -> failwith "Can only be called on primitive type"

let get_primitive_type_for_adt_sig adt_sig =
  if adt_sig == !unit_adt_sig then
    Some Types.Type.Unit
  else if adt_sig == !bool_adt_sig then
    Some Bool
  else if adt_sig == !byte_adt_sig then
    Some Byte
  else if adt_sig == !int_adt_sig then
    Some Int
  else if adt_sig == !long_adt_sig then
    Some Long
  else
    None

(* Stdlib registration *)

let stdlib_builtin_decl_locs = ref LocMap.empty

let stdlib_builtin_name_to_decl_loc = ref SMap.empty

let lookup_stdlib_name decl_loc = LocMap.find_opt decl_loc !stdlib_builtin_decl_locs

let lookup_stdlib_decl_loc name = SMap.find name !stdlib_builtin_name_to_decl_loc

let register_stdlib_decl full_name loc =
  (* Only register the first name encountered, in the case of multiple names (namely type traits) *)
  if
    SSet.mem full_name all_stdlib_names && not (SMap.mem full_name !stdlib_builtin_name_to_decl_loc)
  then (
    stdlib_builtin_decl_locs := LocMap.add loc full_name !stdlib_builtin_decl_locs;
    stdlib_builtin_name_to_decl_loc := SMap.add full_name loc !stdlib_builtin_name_to_decl_loc
  )

let register_stdlib_type loc adt_sig =
  match lookup_stdlib_name loc with
  | Some name when name = std_bool_bool -> bool_adt_sig := adt_sig
  | Some name when name = std_byte_byte -> byte_adt_sig := adt_sig
  | Some name when name = std_int_int -> int_adt_sig := adt_sig
  | Some name when name = std_long_long -> long_adt_sig := adt_sig
  | Some name when name = std_map_map -> map_adt_sig := adt_sig
  | Some name when name = std_memory_array -> array_adt_sig := adt_sig
  | Some name when name = std_never_never -> never_adt_sig := adt_sig
  | Some name when name = std_option_option -> option_adt_sig := adt_sig
  | Some name when name = std_result_result -> result_adt_sig := adt_sig
  | Some name when name = std_set_set -> set_adt_sig := adt_sig
  | Some name when name = std_string_string -> string_adt_sig := adt_sig
  | Some name when name = std_vec_vec -> vec_adt_sig := adt_sig
  | Some name when name = std_unit_unit -> unit_adt_sig := adt_sig
  | _ -> ()

let register_stdlib_trait loc trait_sig =
  match lookup_stdlib_name loc with
  | Some name when name = std_iterator_iterable -> iterable_trait_sig := trait_sig
  | Some name when name = std_ops_equatable -> equatable_trait_sig := trait_sig
  | Some name when name = std_string_tostring -> to_string_trait_sig := trait_sig
  | _ -> ()
