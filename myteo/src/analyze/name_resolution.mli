val analyze :
  is_stdlib:bool ->
  Module_tree.t ->
  (string * Ast.Module.t) list ->
  (string * Ast.Module.t) list * Bindings.Bindings.t * Analyze_error.errors
