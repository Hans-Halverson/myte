open Types

let cast_to_tuple_type (ty : Type.t) : Type.t list =
  match ty with
  | Tuple elements -> elements
  | _ -> failwith "Expected tuple type"

let cast_to_function_type (ty : Type.t) : Type.t list * Type.t list * Type.t =
  match ty with
  | Function { type_args; params; return } -> (type_args, params, return)
  | _ -> failwith "Expected function type"

let cast_to_adt_type (ty : Type.t) : Type.t list * AdtSig.t =
  match ty with
  | ADT { type_args; adt_sig } -> (type_args, adt_sig)
  | _ -> failwith "Expected ADT type"
