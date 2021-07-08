val analyze :
  is_stdlib:bool ->
  Bindings.Bindings.t ->
  Module_tree.t ->
  (string * Ast.Module.t) list ->
  (string * Ast.Module.t) list * Analyze_error.errors
