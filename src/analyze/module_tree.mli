type t = module_tree_node Basic_collections.SMap.t

and module_tree_node =
  | Empty of string * t
  | Module of string * t
  | Decl of decl

and decl = {
  name: Ast.Identifier.t;
  is_public: bool;
}

type lookup_result =
  | LookupResultDecl of decl
  | LookupResultModule of string option * t
  | LookupResultError of Analyze_error.error

val lookup : Ast.Identifier.t list -> t -> lookup_result

val get_all_decls : t -> (decl * string list) list

val analyze : t -> Ast.Module.t list -> t * Analyze_error.errors
