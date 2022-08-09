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

module MatchCaseVariableDeclaration : sig
  type t = { tvar: Types.TVar.t }

  val mk : unit -> t
end

module ThisDeclaration : sig
  type t = { tvar: Types.TVar.t }

  val mk : unit -> t
end

module FunctionDeclaration : sig
  type t = {
    name: string;
    loc: Loc.t;
    is_public: bool;
    is_static: bool;
    is_override: bool;
    is_signature: bool;
    mutable type_params: Types.TypeParam.t list;
    mutable params: Types.Type.t list;
    mutable return: Types.Type.t;
    mutable this_binding_id: int option;
    mutable captures: LocSet.t;
  }

  val mk :
    name:string ->
    loc:Loc.t ->
    is_public:bool ->
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
    is_public: bool;
    mutable methods: FunctionDeclaration.t SMap.t;
    mutable implemented: t LocMap.t;
  }

  val mk : name:string -> loc:Loc.t -> module_:Module.t -> is_public:bool -> t
end

module TypeDeclaration : sig
  type t = {
    adt_sig: Types.AdtSig.t;
    is_public: bool;
    mutable traits: TraitDeclaration.t list;
  }

  val mk : name:string -> loc:Loc.t -> module_:Module.t -> is_public:bool -> t

  val add_trait : t -> TraitDeclaration.t -> unit
end

type value_declaration =
  | VarDecl of VariableDeclaration.t
  | FunDecl of FunctionDeclaration.t
  | CtorDecl of TypeDeclaration.t
  | FunParamDecl of FunctionParamDeclaration.t
  | MatchCaseVarDecl of MatchCaseVariableDeclaration.t
  | ThisDecl of ThisDeclaration.t

type type_declaration =
  | TypeDecl of TypeDeclaration.t
  | TypeParam of TypeParamDeclaration.t
  | TypeAlias of TypeAliasDeclaration.t
  | TraitDecl of TraitDeclaration.t

module ValueBinding : sig
  type t = {
    id: id;
    name: string;
    loc: Loc.t;
    declaration: value_declaration;
    mutable uses: LocSet.t;
    mutable is_captured: bool;
    context: context;
    module_: Module.t;
  }

  and id = int

  and context =
    | Module
    | Trait of string
    | Function of Loc.t * bool
    | GlobalInit

  val mk :
    name:string ->
    loc:Loc.t ->
    declaration:value_declaration ->
    context:context ->
    module_:Module.t ->
    t
end

module TypeBinding : sig
  type t = {
    id: id;
    name: string;
    loc: Loc.t;
    declaration: type_declaration;
    mutable uses: LocSet.t;
    module_: Module.t;
  }

  and id = int

  val mk : name:string -> loc:Loc.t -> declaration:type_declaration -> module_:Module.t -> t
end

module Bindings : sig
  type t = {
    mutable value_use_to_binding: ValueBinding.t LocMap.t;
    mutable type_use_to_binding: TypeBinding.t LocMap.t;
    mutable this_bindings: ValueBinding.t IMap.t;
    mutable scope_named_access_locs: LocSet.t;
  }

  val mk : unit -> t

  val add_value_use : t -> Loc.t -> ValueBinding.t -> unit

  val add_type_use : t -> Loc.t -> TypeBinding.t -> unit

  val add_this_binding : t -> ValueBinding.t -> unit

  val add_scope_named_access : t -> Loc.t -> unit

  val is_value_decl_loc : t -> Loc.t -> bool

  val is_type_decl_loc : t -> Loc.t -> bool
end

module BVSet : Set.S with type elt = ValueBinding.t

module BVMap : Map.S with type key = ValueBinding.t

module LBVMMap : MultiMap.S with type key = Loc.t and type value = ValueBinding.t

module BTSet : Set.S with type elt = TypeBinding.t

module BTMap : Map.S with type key = TypeBinding.t

val get_value_binding : Bindings.t -> Loc.t -> ValueBinding.t

val get_type_binding : Bindings.t -> Loc.t -> TypeBinding.t

val get_this_binding : Bindings.t -> ValueBinding.id -> ValueBinding.t

val is_scope_named_access : Bindings.t -> Loc.t -> bool

val get_decl_loc_from_value_use : Bindings.t -> Loc.t -> Loc.t

val is_module_decl : ValueBinding.t -> bool

val is_std_lib_value : ValueBinding.t -> bool

val is_mutable_variable : ValueBinding.t -> bool

val get_var_decl : ValueBinding.t -> VariableDeclaration.t

val get_func_decl : ValueBinding.t -> FunctionDeclaration.t

val get_ctor_decl : ValueBinding.t -> TypeDeclaration.t

val get_func_param_decl : ValueBinding.t -> FunctionParamDeclaration.t

val get_this_decl : ValueBinding.t -> ThisDeclaration.t

val get_type_decl : TypeBinding.t -> TypeDeclaration.t

val get_trait_decl : TypeBinding.t -> TraitDeclaration.t

val get_type_alias_decl : TypeBinding.t -> TypeAliasDeclaration.t

val get_type_param_decl : TypeBinding.t -> TypeParamDeclaration.t
