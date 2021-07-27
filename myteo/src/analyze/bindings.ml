open Ast
open Basic_collections
open Types

module VariableDeclaration = struct
  type t = {
    kind: Statement.VariableDeclaration.kind;
    tvar: TVar.t;
  }

  let mk kind = { kind; tvar = TVar.mk () }
end

module FunctionParamDeclaration = struct
  type t = { tvar: TVar.t }

  let mk () = { tvar = TVar.mk () }
end

module MatchCaseVariableDeclaration = struct
  type t = { tvar: TVar.t }

  let mk () = { tvar = TVar.mk () }
end

module FunctionDeclaration = struct
  type t = {
    name: string;
    loc: Loc.t;
    is_builtin: bool;
    is_static: bool;
    is_override: bool;
    is_signature: bool;
    (* Type parameters for this function. During type instantiation for the function (where it is
       identified - whether at a direct call or another use) type arguments will be bound to these
       type parameters in the param and return signatures of the type. *)
    mutable type_params: Types.TypeParam.t list;
    (* Parameter types for the function. If the function has type params, this is a signature *)
    mutable params: Type.t list;
    (* Return types for the function. If the function has type params, this is a signature *)
    mutable return: Type.t;
  }

  let mk ~name ~loc ~is_builtin ~is_static ~is_override ~is_signature =
    {
      name;
      loc;
      is_builtin;
      is_static;
      is_override;
      is_signature;
      type_params = [];
      params = [];
      return = Any;
    }
end

module TypeAliasDeclaration = struct
  type t = {
    (* Type parameters for this alias. During type instantiation type arguments will be bound
       to these type parameters in the body of the type. *)
    mutable type_params: Types.TypeParam.t list;
    (* The body signature of the type alias *)
    mutable body: Type.t;
  }

  let mk () = { type_params = []; body = Any }
end

module TypeParamDeclaration = struct
  type t = { mutable type_param: TypeParam.t option }

  let mk () = { type_param = None }

  let get decl = Option.get decl.type_param

  let set decl type_param = decl.type_param <- Some type_param
end

module TraitDeclaration = struct
  type t = {
    trait_sig: TraitSig.t;
    name: string;
    loc: Loc.t;
    mutable methods: FunctionDeclaration.t SMap.t;
    mutable implemented: t LocMap.t;
  }

  let mk ~name ~loc =
    { trait_sig = TraitSig.mk ~name; name; loc; methods = SMap.empty; implemented = LocMap.empty }
end

module TypeDeclaration = struct
  type t = {
    adt_sig: AdtSig.t;
    mutable traits: TraitDeclaration.t list;
  }

  let mk ~name = { adt_sig = AdtSig.mk ~name; traits = [] }

  let add_trait decl trait = decl.traits <- trait :: decl.traits
end

type value_declaration =
  | VarDecl of VariableDeclaration.t
  | FunDecl of FunctionDeclaration.t
  | CtorDecl of TypeDeclaration.t
  | FunParamDecl of FunctionParamDeclaration.t
  | MatchCaseVarDecl of MatchCaseVariableDeclaration.t

type type_declaration =
  | TypeDecl of TypeDeclaration.t
  | TypeParam of TypeParamDeclaration.t
  | TypeAlias of TypeAliasDeclaration.t
  | TraitDecl of TraitDeclaration.t

module ValueBinding = struct
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

  let mk ~name ~loc ~declaration ~context ~module_ =
    { name; loc; declaration; uses = LocSet.singleton loc; context; module_ }
end

module TypeBinding = struct
  type t = {
    name: string;
    loc: Loc.t;
    declaration: type_declaration;
    mutable uses: LocSet.t;
    module_: string list;
  }

  let mk ~name ~loc ~declaration ~module_ =
    { name; loc; declaration; uses = LocSet.singleton loc; module_ }
end

module Bindings = struct
  type t = {
    mutable value_bindings: ValueBinding.t LocMap.t;
    mutable type_bindings: TypeBinding.t LocMap.t;
    mutable value_use_to_decl: Loc.t LocMap.t;
    mutable type_use_to_decl: Loc.t LocMap.t;
  }

  let mk () =
    {
      value_bindings = LocMap.empty;
      type_bindings = LocMap.empty;
      value_use_to_decl = LocMap.empty;
      type_use_to_decl = LocMap.empty;
    }

  let add_value_binding bindings binding =
    let open ValueBinding in
    bindings.value_bindings <- LocMap.add binding.loc binding bindings.value_bindings;
    bindings.value_use_to_decl <- LocMap.add binding.loc binding.loc bindings.value_use_to_decl

  let add_type_binding bindings binding =
    let open TypeBinding in
    bindings.type_bindings <- LocMap.add binding.loc binding bindings.type_bindings;
    bindings.type_use_to_decl <- LocMap.add binding.loc binding.loc bindings.type_use_to_decl

  let add_value_use bindings use_loc decl_loc =
    bindings.value_use_to_decl <- LocMap.add use_loc decl_loc bindings.value_use_to_decl

  let add_type_use bindings use_loc decl_loc =
    bindings.type_use_to_decl <- LocMap.add use_loc decl_loc bindings.type_use_to_decl
end

let get_value_binding bindings use_loc =
  let open Bindings in
  let decl_loc = LocMap.find use_loc bindings.value_use_to_decl in
  LocMap.find decl_loc bindings.value_bindings

let get_type_binding bindings use_loc =
  let open Bindings in
  let decl_loc = LocMap.find use_loc bindings.type_use_to_decl in
  LocMap.find decl_loc bindings.type_bindings

let get_type_binding_from_decl bindings decl_loc =
  let open Bindings in
  LocMap.find decl_loc bindings.type_bindings

let get_decl_loc_from_value_use bindings use_loc =
  let binding = get_value_binding bindings use_loc in
  binding.loc

let is_module_decl bindings decl_loc =
  let open Bindings in
  let binding = LocMap.find decl_loc bindings.value_bindings in
  binding.context = ValueBinding.Module

let is_std_lib_value binding = List.hd binding.ValueBinding.module_ = "std"

let get_var_decl binding =
  match binding.ValueBinding.declaration with
  | VarDecl var_decl -> var_decl
  | _ -> failwith "Expected variable"

let get_func_decl binding =
  match binding.ValueBinding.declaration with
  | FunDecl func_decl -> func_decl
  | _ -> failwith "Expected function"

let get_ctor_decl binding =
  match binding.ValueBinding.declaration with
  | CtorDecl ctor_decl -> ctor_decl
  | _ -> failwith "Expected constructor"

let get_func_param_decl binding =
  match binding.ValueBinding.declaration with
  | FunParamDecl param_decl -> param_decl
  | _ -> failwith "Expected function parameter"

let get_type_decl binding =
  match binding.TypeBinding.declaration with
  | TypeDecl type_decl -> type_decl
  | _ -> failwith "Expected type declaration"

let get_trait_decl binding =
  match binding.TypeBinding.declaration with
  | TraitDecl trait_decl -> trait_decl
  | _ -> failwith "Expected trait declaration"

let get_type_alias_decl binding =
  match binding.TypeBinding.declaration with
  | TypeAlias alias_decl -> alias_decl
  | _ -> failwith "Expected type alias"

let get_type_param_decl binding =
  match binding.TypeBinding.declaration with
  | TypeParam param_decl -> param_decl
  | _ -> failwith "Expected type parameter"
