val analyze_modules :
  is_stdlib:bool ->
  pcx:Program_context.t ->
  (string * Ast.Module.t) list ->
  ((string * Ast.Module.t) list, Analyze_error.errors) result
