open Basic_collections

module VariableDeclaration : sig
  type t = {
    kind: Ast.Statement.VariableDeclaration.kind;
    tvar: Types.TVar.t;
  }

  val mk : Ast.Statement.VariableDeclaration.kind -> t
end

module FunctionParamDeclaration : sig
  type t = { tvar: Types.TVar.t }

  val mk : unit -> t
end

module FunctionDeclaration : sig
  type t = {
    name: string;
    loc: Loc.t;
    is_builtin: bool;
    is_static: bool;
    is_override: bool;
    is_signature: bool;
    mutable type_params: Types.TypeParam.t list;
    mutable params: Types.Type.t list;
    mutable return: Types.Type.t;
  }

  val mk :
    name:string ->
    loc:Loc.t ->
    is_builtin:bool ->
    is_static:bool ->
    is_override:bool ->
    is_signature:bool ->
    t
end

module TypeAliasDeclaration : sig
  type t = {
    mutable type_params: Types.TypeParam.t list;
    mutable body: Types.Type.t;
  }

  val mk : unit -> t
end

module TypeParamDeclaration : sig
  type t

  val mk : unit -> t

  val get : t -> Types.TypeParam.t

  val set : t -> Types.TypeParam.t -> unit
end

module TraitDeclaration : sig
  type t = {
    trait_sig: Types.TraitSig.t;
    name: string;
    loc: Loc.t;
    mutable methods: FunctionDeclaration.t SMap.t;
    mutable implemented: implemented_trait LocMap.t;
  }

  and implemented_trait = {
    mutable implemented_trait: t;
    mutable implemented_type_args: Types.Type.t list;
  }

  val mk : name:string -> loc:Loc.t -> t
end

module TypeDeclaration : sig
  type t = {
    adt_sig: Types.AdtSig.t;
    mutable traits: TraitDeclaration.t list;
  }

  val mk : name:string -> t

  val add_trait : t -> TraitDeclaration.t -> unit
end

type value_declaration =
  | VarDecl of VariableDeclaration.t
  | FunDecl of FunctionDeclaration.t
  | CtorDecl of TypeDeclaration.t
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
    context: context;
    module_: string list;
  }

  and context =
    | Module
    | Trait of string
    | Function

  val mk :
    name:string ->
    loc:Loc.t ->
    declaration:value_declaration ->
    context:context ->
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

val get_type_binding_from_decl : Bindings.t -> Loc.t -> TypeBinding.t

val get_decl_loc_from_value_use : Bindings.t -> Loc.t -> Loc.t

val is_module_decl : Bindings.t -> Loc.t -> bool

val get_var_decl : ValueBinding.t -> VariableDeclaration.t

val get_func_decl : ValueBinding.t -> FunctionDeclaration.t

val get_ctor_decl : ValueBinding.t -> TypeDeclaration.t

val get_func_param_decl : ValueBinding.t -> FunctionParamDeclaration.t

val get_type_decl : TypeBinding.t -> TypeDeclaration.t

val get_trait_decl : TypeBinding.t -> TraitDeclaration.t

val get_type_alias_decl : TypeBinding.t -> TypeAliasDeclaration.t

val get_type_param_decl : TypeBinding.t -> TypeParamDeclaration.t
