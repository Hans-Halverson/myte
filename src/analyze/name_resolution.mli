val analyze :
  is_stdlib:bool ->
  Bindings.Bindings.t ->
  Module_tree.t ->
  (string * Ast.Module.t) list ->
  Ast.TraitDeclaration.t list * Ast.TypeDeclaration.t list * Analyze_error.errors
