type t = module_tree_node Basic_collections.SMap.t

and module_tree_node =
  | Empty of string * t
  | Module of string * t
  | Export of export_info

and export_info = {
  value: (value_export_kind * Ast.Identifier.t) option;
  ty: (type_export_kind * Ast.Identifier.t) option;
}

and value_export_kind =
  | VarDecl of Ast.Statement.VariableDeclaration.kind
  | FunDecl
  | CtorDecl

and type_export_kind =
  | TypeDecl
  | TypeAlias of Bindings.TypeAliasDeclaration.t

type lookup_result =
  | LookupResultExport of export_info
  | LookupResultModule of string option * t
  | LookupResultError of Loc.t * Analyze_error.t

val lookup : Ast.Identifier.t list -> t -> lookup_result

val get_all_exports :
  t ->
  (value_export_kind * Ast.Identifier.t * string list) list
  * (type_export_kind * Ast.Identifier.t * string list) list

val analyze : t -> Ast.Module.t list -> t * (Loc.t * Analyze_error.t) list
