open Basic_collections

module VariableDeclaration : sig
  type t = {
    kind: Ast.Statement.VariableDeclaration.kind;
    tvar_id: Types.tvar_id;
  }

  val mk : Ast.Statement.VariableDeclaration.kind -> t
end

module FunctionParamDeclaration : sig
  type t = { tvar_id: Types.tvar_id }

  val mk : unit -> t
end

module ConstructorDeclaration : sig
  type t

  val mk : unit -> t

  val get : t -> Types.adt_sig

  val set : t -> Types.adt_sig -> unit
end

module FunctionDeclaration : sig
  type t = {
    mutable type_params: Types.TypeParam.t list;
    mutable params: Types.t list;
    mutable return: Types.t;
    is_builtin: bool;
  }

  val mk : bool -> t
end

module TypeAliasDeclaration : sig
  type t = {
    mutable type_params: Types.TypeParam.t list;
    mutable body: Types.t;
  }

  val mk : unit -> t
end

module TypeParamDeclaration : sig
  type t

  val mk : unit -> t

  val get : t -> Types.TypeParam.t

  val set : t -> Types.TypeParam.t -> unit
end

module TypeDeclaration : sig
  type t

  val mk : unit -> t

  val get_adt_sig : t -> Types.adt_sig

  val set_adt_sig : t -> Types.adt_sig -> unit

  val add_trait : t -> Traits.Trait.t -> unit

  val get_traits : t -> Traits.Trait.t list
end

module TraitDeclaration : sig
  type t = Traits.Trait.t

  val mk : name:string -> loc:Loc.t -> t
end

type value_declaration =
  | VarDecl of VariableDeclaration.t
  | FunDecl of FunctionDeclaration.t
  | CtorDecl of ConstructorDeclaration.t
  | FunParamDecl of FunctionParamDeclaration.t

type type_declaration =
  | TypeDecl of TypeDeclaration.t
  | TypeParam of TypeParamDeclaration.t
  | TypeAlias of TypeAliasDeclaration.t
  | TraitDecl of TraitDeclaration.t

module ValueBinding : sig
  type t = {
    name: string;
    loc: Loc.t;
    declaration: value_declaration;
    mutable uses: LocSet.t;
    is_global: bool;
    module_: string list;
  }

  val mk :
    name:string ->
    loc:Loc.t ->
    declaration:value_declaration ->
    is_global:bool ->
    module_:string list ->
    t
end

module TypeBinding : sig
  type t = {
    name: string;
    loc: Loc.t;
    declaration: type_declaration;
    mutable uses: LocSet.t;
    module_: string list;
  }

  val mk : name:string -> loc:Loc.t -> declaration:type_declaration -> module_:string list -> t
end

module Bindings : sig
  type t = {
    mutable value_bindings: ValueBinding.t LocMap.t;
    mutable type_bindings: TypeBinding.t LocMap.t;
    mutable value_use_to_decl: Loc.t LocMap.t;
    mutable type_use_to_decl: Loc.t LocMap.t;
  }

  val mk : unit -> t

  val add_value_binding : t -> ValueBinding.t -> unit

  val add_type_binding : t -> TypeBinding.t -> unit

  val add_value_use : t -> Loc.t -> Loc.t -> unit

  val add_type_use : t -> Loc.t -> Loc.t -> unit
end

val get_value_binding : Bindings.t -> Loc.t -> ValueBinding.t

val get_type_binding : Bindings.t -> Loc.t -> TypeBinding.t

val get_decl_loc_from_value_use : Bindings.t -> Loc.t -> Loc.t

val is_global_decl : Bindings.t -> Loc.t -> bool

val get_var_decl : ValueBinding.t -> VariableDeclaration.t

val get_func_decl : ValueBinding.t -> FunctionDeclaration.t

val get_ctor_decl : ValueBinding.t -> ConstructorDeclaration.t

val get_func_param_decl : ValueBinding.t -> FunctionParamDeclaration.t

val get_type_decl : TypeBinding.t -> TypeDeclaration.t

val get_type_alias_decl : TypeBinding.t -> TypeAliasDeclaration.t

val get_type_param_decl : TypeBinding.t -> TypeParamDeclaration.t
