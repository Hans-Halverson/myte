module ModuleDef = Module
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

module ThisDeclaration = struct
  type t = { tvar: TVar.t }

  let mk () = { tvar = TVar.mk () }
end

module FunctionDeclaration = struct
  type t = {
    name: string;
    loc: Loc.t;
    is_public: bool;
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
    (* Id of the implicit `this` binding for this function if it is a method *)
    mutable this_binding_id: int option;
    (* Decl locs for all values this function captures *)
    mutable captures: LocSet.t;
  }

  let mk ~name ~loc ~is_public ~is_builtin ~is_static ~is_override ~is_signature =
    {
      name;
      loc;
      is_public;
      is_builtin;
      is_static;
      is_override;
      is_signature;
      type_params = [];
      params = [];
      return = Types.any;
      this_binding_id = None;
      captures = LocSet.empty;
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

  let mk () = { type_params = []; body = Types.any }
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

  let mk ~name ~loc ~module_ =
    {
      trait_sig = TraitSig.mk ~name ~loc ~module_;
      name;
      loc;
      methods = SMap.empty;
      implemented = LocMap.empty;
    }
end

module TypeDeclaration = struct
  type t = {
    adt_sig: AdtSig.t;
    mutable traits: TraitDeclaration.t list;
  }

  let mk ~name ~loc ~module_ = { adt_sig = AdtSig.mk ~name ~loc ~module_; traits = [] }

  let add_trait decl trait = decl.traits <- trait :: decl.traits
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

module ValueBinding = struct
  type t = {
    id: id;
    name: string;
    loc: Loc.t;
    declaration: value_declaration;
    mutable uses: LocSet.t;
    mutable is_captured: bool;
    context: context;
    module_: ModuleDef.t;
  }

  and id = int

  and context =
    | Module
    | Trait of string
    (* Loc of the named function id, or the anonymous function node. Boolean is whether context is
       for anonymous function. *)
    | Function of Loc.t * bool
    | GlobalInit

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id

  let mk ~name ~loc ~declaration ~context ~module_ =
    {
      id = mk_id ();
      name;
      loc;
      declaration;
      uses = LocSet.singleton loc;
      is_captured = false;
      context;
      module_;
    }

  let compare b1 b2 = Int.compare b1.id b2.id
end

module TypeBinding = struct
  type t = {
    id: id;
    name: string;
    loc: Loc.t;
    declaration: type_declaration;
    mutable uses: LocSet.t;
    module_: ModuleDef.t;
  }

  and id = int

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id

  let mk ~name ~loc ~declaration ~module_ =
    { id = mk_id (); name; loc; declaration; uses = LocSet.singleton loc; module_ }

  let compare b1 b2 = Int.compare b1.id b2.id
end

module Bindings = struct
  type t = {
    mutable value_use_to_binding: ValueBinding.t LocMap.t;
    mutable type_use_to_binding: TypeBinding.t LocMap.t;
    (* Collection of all "this" bindings indexed by their binding id *)
    mutable this_bindings: ValueBinding.t IMap.t;
    (* Locs of all named access nodes where the entire target is a scope chain, and the target is
       the actual value that is qualified by the scope chain. *)
    mutable scope_named_access_locs: LocSet.t;
  }

  let mk () =
    {
      value_use_to_binding = LocMap.empty;
      type_use_to_binding = LocMap.empty;
      this_bindings = IMap.empty;
      scope_named_access_locs = LocSet.empty;
    }

  let add_value_use bindings use_loc binding =
    bindings.value_use_to_binding <- LocMap.add use_loc binding bindings.value_use_to_binding

  let add_type_use bindings use_loc binding =
    bindings.type_use_to_binding <- LocMap.add use_loc binding bindings.type_use_to_binding

  let add_this_binding bindings binding =
    bindings.this_bindings <- IMap.add binding.ValueBinding.id binding bindings.this_bindings

  let add_scope_named_access bindings loc =
    bindings.scope_named_access_locs <- LocSet.add loc bindings.scope_named_access_locs

  let is_value_decl_loc bindings decl_loc =
    match LocMap.find_opt decl_loc bindings.value_use_to_binding with
    | Some { ValueBinding.loc; _ } when Loc.compare loc decl_loc = 0 -> true
    | _ -> false

  let is_type_decl_loc bindings decl_loc =
    match LocMap.find_opt decl_loc bindings.type_use_to_binding with
    | Some { TypeBinding.loc; _ } when Loc.compare loc decl_loc = 0 -> true
    | _ -> false
end

module ValueBindingCollection = MakeCollection (ValueBinding)
module TypeBindingCollection = MakeCollection (TypeBinding)

module BVSet = ValueBindingCollection.Set
module BVMap = ValueBindingCollection.Map
module LBVMMap = MultiMap.Make (Loc) (ValueBindingCollection)
module BTSet = TypeBindingCollection.Set
module BTMap = TypeBindingCollection.Map

let get_value_binding bindings use_loc =
  let open Bindings in
  LocMap.find use_loc bindings.value_use_to_binding

let get_type_binding bindings use_loc =
  let open Bindings in
  LocMap.find use_loc bindings.type_use_to_binding

let get_this_binding bindings this_binding_id =
  let open Bindings in
  IMap.find this_binding_id bindings.this_bindings

let is_scope_named_access bindings loc =
  let open Bindings in
  LocSet.mem loc bindings.scope_named_access_locs

let get_decl_loc_from_value_use bindings use_loc =
  let binding = get_value_binding bindings use_loc in
  binding.loc

let is_module_decl binding = binding.ValueBinding.context = ValueBinding.Module

let is_std_lib_value binding = List.hd binding.ValueBinding.module_.name = "std"

let is_mutable_variable binding =
  match binding.ValueBinding.declaration with
  | VarDecl { kind = Mutable; _ } -> true
  | _ -> false

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

let get_this_decl binding =
  match binding.ValueBinding.declaration with
  | ThisDecl this_decl -> this_decl
  | _ -> failwith "Expected this"

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
