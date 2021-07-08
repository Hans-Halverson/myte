type t = module_tree_node Basic_collections.SMap.t

and module_tree_node =
  | Empty of string * t
  | Module of string * t
  | Export of export_info

and export_info = export_kind * Ast.Identifier.t

and export_kind =
  | VarDecl of Ast.Statement.VariableDeclaration.kind
  | FunDecl of bool
  | CtorDecl
  | TypeDecl of (* Is ctor decl *) bool
  | TypeAlias of Bindings.TypeAliasDeclaration.t
  | TraitDecl

type lookup_result =
  | LookupResultExport of export_info
  | LookupResultModule of string option * t
  | LookupResultError of Analyze_error.error

val lookup : Ast.Identifier.t list -> t -> lookup_result

val get_all_exports : t -> (export_kind * Ast.Identifier.t * string list) list

val analyze : t -> Ast.Module.t list -> t * Analyze_error.errors
