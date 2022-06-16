val analyze_modules :
  is_stdlib:bool ->
  pcx:Program_context.t ->
  (string * Ast.Module.t) list ->
  (unit, Analyze_error.errors) result
