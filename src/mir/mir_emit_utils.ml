let mk_value_binding_name binding =
  let open Bindings.ValueBinding in
  match binding.context with
  | Module
  | Function _ ->
    String.concat "." (binding.module_ @ [binding.name])
  | Trait trait_name -> String.concat "." (binding.module_ @ [trait_name; binding.name])
  | GlobalInit -> failwith "Cannot refer to binding in global init context by name"

let mk_type_binding_name binding =
  let open Bindings.TypeBinding in
  String.concat "." (binding.module_ @ [binding.name])
