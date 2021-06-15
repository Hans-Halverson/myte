open Types

let cast_to_function_type (ty : Types.t) : Types.t list * Types.t =
  match ty with
  | Function { params; return } -> (params, return)
  | _ -> failwith "Expected function type"
