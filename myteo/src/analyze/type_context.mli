open Basic_collections
open Bindings

type t

val mk : bindings:Bindings.t -> t

(* Getters and Setters *)

val add_error : cx:t -> Loc.t -> Analyze_error.t -> unit

val get_errors : cx:t -> Analyze_error.errors

val set_errors : cx:t -> Analyze_error.errors -> unit

val add_return_type : cx:t -> Loc.t -> Types.t -> unit

val get_return_types : cx:t -> Types.t LocMap.t

val get_unresolved_int_literals : cx:t -> LocSet.t

(* Binding getters *)

val get_value_binding : cx:t -> Loc.t -> ValueBinding.t

val get_type_binding : cx:t -> Loc.t -> TypeBinding.t

(* Type constraints *)

val unify : cx:t -> Types.t -> Types.t -> bool

val is_subtype : cx:t -> Types.t -> Types.t -> bool

val assert_unify : cx:t -> Loc.t -> Types.t -> Types.t -> unit

val assert_is_subtype : cx:t -> Loc.t -> Types.t -> Types.t -> unit

(* Type utilities *)

val find_rep_type : cx:t -> Types.t -> Types.t

val get_tvar_from_loc : cx:t -> Loc.t -> Types.tvar_id

val get_tvar_from_loc_opt : cx:t -> Loc.t -> Types.tvar_id option

val mk_tvar_id : cx:t -> loc:Loc.t -> Types.tvar_id

val resolve_int_literal : cx:t -> Types.int_literal -> Types.t -> unit

val add_incompatible_types_error : cx:t -> Loc.t -> Types.t -> Types.t -> unit

val mk_int_literal_ty : cx:t -> Loc.t -> string -> Integers.base -> Types.t
