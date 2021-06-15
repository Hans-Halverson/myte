open Types

let cast_to_function_type ty =
  match ty with
  | Function { params; return } -> (params, return)
  | _ -> failwith "Expected function type"
