open Types

let cast_to_tuple_type (ty : Types.t) : Types.t list =
  match ty with
  | Tuple elements -> elements
  | _ -> failwith "Expected tuple type"

let cast_to_function_type (ty : Types.t) : tvar_id list * Types.t list * Types.t =
  match ty with
  | Function { type_args; params; return } -> (type_args, params, return)
  | _ -> failwith "Expected function type"

let cast_to_adt_type (ty : Types.t) : Types.t list * Types.adt_sig =
  match ty with
  | ADT { type_args; adt_sig } -> (type_args, adt_sig)
  | _ -> failwith "Expected ADT type"
