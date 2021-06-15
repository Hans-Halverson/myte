exception CyclicTypeAliasesException of Loc.t * string

val order_type_aliases : cx:Type_context.t -> Ast.Module.t list -> Ast.TypeDeclaration.t list
