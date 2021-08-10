val analyze :
  is_stdlib:bool ->
  Bindings.Bindings.t ->
  Module_tree.t ->
  (string * Ast.Module.t) list ->
  (string * Ast.Module.t) list * Ast.TraitDeclaration.t list * Analyze_error.errors
