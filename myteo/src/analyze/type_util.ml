open Types

let cast_to_function_type ty =
  match ty with
  | Function { tparams; params; return } -> (tparams, params, return)
  | _ -> failwith "Expected function type"
