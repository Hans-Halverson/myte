val analyze :
  pcx:Program_context.t ->
  is_stdlib:bool ->
  (string * Ast.Module.t) list ->
  Ast.TraitDeclaration.t list * Ast.TypeDeclaration.t list * Analyze_error.errors
