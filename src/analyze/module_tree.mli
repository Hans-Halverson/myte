type t = module_tree_node Basic_collections.SMap.t

and module_tree_node =
  | Empty of string * t
  | Module of string * t
  | Export of Ast.Identifier.t

type lookup_result =
  | LookupResultExport of Ast.Identifier.t
  | LookupResultModule of string option * t
  | LookupResultError of Analyze_error.error

val lookup : Ast.Identifier.t list -> t -> lookup_result

val get_all_exports : t -> (Ast.Identifier.t * string list) list

val analyze : t -> Ast.Module.t list -> t * Analyze_error.errors
