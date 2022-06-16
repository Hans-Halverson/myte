open Basic_collections
open Bindings
open Types

type t

val mk : bindings:Bindings.t -> t

module MethodUse : sig
  type t = {
    method_sig: MethodSig.t;
    is_super_call: bool;
  }
end

module FunctionContext : sig
  type t = {
    func: func;
    return_type: Type.t;
  }

  and func =
    | Named of FunctionDeclaration.t
    | Anonymous of Loc.t
end

(* Getters and Setters *)

val add_error : cx:t -> Loc.t -> Analyze_error.t -> unit

val get_errors : cx:t -> Analyze_error.errors

val set_errors : cx:t -> Analyze_error.errors -> unit

val push_current_function : cx:t -> FunctionContext.func -> Type.t -> unit

val pop_current_function : cx:t -> unit

val get_current_function : cx:t -> FunctionContext.t

val get_function_context_stack : cx:t -> FunctionContext.t list

val is_in_function : cx:t -> bool

val get_unresolved_int_literals : cx:t -> LocSet.t

val add_method_use : cx:t -> Loc.t -> MethodUse.t -> unit

val get_method_use : cx:t -> Loc.t -> MethodUse.t

val is_method_use : cx:t -> Loc.t -> bool

val set_main_loc : cx:t -> Loc.t -> unit

val is_main_loc : cx:t -> Loc.t -> bool

val get_ordered_traits : cx:t -> Ast.TraitDeclaration.t list

val set_ordered_traits : cx:t -> Ast.TraitDeclaration.t list -> unit

val get_trait_object_promotion : cx:t -> Loc.t -> Types.TraitSig.instance option

val add_unchecked_trait_object_use : cx:t -> Types.TraitSig.t -> Loc.t -> unit

val resolve_unchecked_trait_object_uses : cx:t -> unit

val enter_loop : cx:t -> unit

val exit_loop : cx:t -> unit

val in_loop : cx:t -> bool

val add_anonymous_function_capture : cx:t -> Loc.t -> ValueBinding.t -> unit

val get_anonymous_function_captures : cx:t -> Loc.t -> LBVMMap.VSet.t

(* Binding getters *)

val get_value_binding : cx:t -> Loc.t -> ValueBinding.t

val get_type_binding : cx:t -> Loc.t -> TypeBinding.t

val get_this_binding : cx:t -> ValueBinding.id -> ValueBinding.t

val is_scope_named_access : cx:t -> Loc.t -> bool

(* Type constraints *)

val unify : cx:t -> Type.t -> Type.t -> bool

val is_subtype : cx:t -> trait_object_promotion_loc:Loc.t option -> Type.t -> Type.t -> bool

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

val mk_int_literal_ty : cx:t -> Loc.t -> Int64.t option -> Type.t

val get_implemented_trait : Type.t -> TraitSig.t -> TraitSig.instance option
