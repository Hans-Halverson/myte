open Basic_collections
open Bindings
open Types

type t

val mk : bindings:Bindings.t -> t

(* Getters and Setters *)

val add_error : cx:t -> Loc.t -> Analyze_error.t -> unit

val get_errors : cx:t -> Analyze_error.errors

val set_errors : cx:t -> Analyze_error.errors -> unit

val add_return_type : cx:t -> Loc.t -> Type.t -> unit

val get_return_types : cx:t -> Type.t LocMap.t

val get_unresolved_int_literals : cx:t -> LocSet.t

val add_method_use : cx:t -> Loc.t -> unit

val is_method_use : cx:t -> Loc.t -> bool

val set_main_loc : cx:t -> Loc.t -> unit

val is_main_loc : cx:t -> Loc.t -> bool

val get_ordered_traits : cx:t -> Ast.TraitDeclaration.t list

val set_ordered_traits : cx:t -> Ast.TraitDeclaration.t list -> unit

(* Binding getters *)

val get_value_binding : cx:t -> Loc.t -> ValueBinding.t

val get_type_binding : cx:t -> Loc.t -> TypeBinding.t

val get_type_binding_from_decl : cx:t -> Loc.t -> TypeBinding.t

(* Type constraints *)

val unify : cx:t -> Type.t -> Type.t -> bool

val is_subtype : cx:t -> Type.t -> Type.t -> bool

val assert_unify : cx:t -> Loc.t -> Type.t -> Type.t -> unit

val assert_is_subtype : cx:t -> Loc.t -> Type.t -> Type.t -> unit

val type_satisfies_trait_bounds : cx:t -> Type.t -> Types.TraitSig.instance list -> bool

(* Type utilities *)

val find_rep_type : cx:t -> Type.t -> Type.t

val get_tvar_from_loc : cx:t -> Loc.t -> TVar.t

val get_tvar_from_loc_opt : cx:t -> Loc.t -> TVar.t option

val mk_tvar_id : cx:t -> loc:Loc.t -> TVar.t

val resolve_int_literal : cx:t -> IntLiteral.t -> Type.t -> unit

val resolve_int_literal_from_values : cx:t -> IntLiteral.t -> Type.t

val add_incompatible_types_error : cx:t -> Loc.t -> Type.t -> Type.t -> unit

val mk_int_literal_ty : cx:t -> Loc.t -> string -> Integers.base -> Type.t

val get_implemented_trait : Type.t -> TraitSig.t -> TraitSig.instance option
