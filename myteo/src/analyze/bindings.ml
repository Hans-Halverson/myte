open Ast
open Basic_collections

module TypeAliasDeclaration = struct
  type t = {
    (* Type parameters for this alias. During type instantiation type arguments will be bound
       to these type parameters in the body of the type. *)
    mutable type_params: Types.TParam.t list;
    (* The body signature of the type alias *)
    mutable body: Types.t;
  }

  let mk () = { type_params = []; body = Any }
end

module TypeParamDeclaration = struct
  type t = { mutable type_param: Types.TParam.t option }

  let mk () = { type_param = None }

  let get decl = Option.get decl.type_param

  let set decl type_param = decl.type_param <- Some type_param
end

module TypeDeclaration = struct
  type t = { mutable adt_sig: Types.adt_sig option }

  let mk () = { adt_sig = None }

  let get decl = Option.get decl.adt_sig

  let set decl adt_sig = decl.adt_sig <- Some adt_sig
end

type value_declaration =
  | VarDecl of Statement.VariableDeclaration.kind
  | FunDecl
  | CtorDecl
  | FunParam

type type_declaration =
  | TypeDecl of TypeDeclaration.t
  | TypeParam of TypeParamDeclaration.t
  | TypeAlias of TypeAliasDeclaration.t

module ValueBinding = struct
  type t = {
    name: string;
    loc: Loc.t;
    declaration: value_declaration;
    uses: LocSet.t;
    tvar_id: Types.tvar_id;
    is_global: bool;
    module_: string list;
  }
end

module TypeBinding = struct
  type t = {
    name: string;
    loc: Loc.t;
    declaration: type_declaration;
    uses: LocSet.t;
    module_: string list;
  }
end

module Bindings = struct
  type t = {
    value_bindings: ValueBinding.t LocMap.t;
    type_bindings: TypeBinding.t LocMap.t;
    value_use_to_decl: Loc.t LocMap.t;
    type_use_to_decl: Loc.t LocMap.t;
  }

  let empty =
    {
      value_bindings = LocMap.empty;
      type_bindings = LocMap.empty;
      value_use_to_decl = LocMap.empty;
      type_use_to_decl = LocMap.empty;
    }

  let merge b1 b2 =
    let union a b = LocMap.union (fun _ v1 _ -> Some v1) a b in
    {
      value_bindings = union b1.value_bindings b2.value_bindings;
      type_bindings = union b1.type_bindings b2.type_bindings;
      value_use_to_decl = union b1.value_use_to_decl b2.value_use_to_decl;
      type_use_to_decl = union b1.type_use_to_decl b2.type_use_to_decl;
    }
end

let get_value_binding bindings use_loc =
  let open Bindings in
  let decl_loc = LocMap.find use_loc bindings.value_use_to_decl in
  LocMap.find decl_loc bindings.value_bindings

let get_type_binding bindings use_loc =
  let open Bindings in
  let decl_loc = LocMap.find use_loc bindings.type_use_to_decl in
  LocMap.find decl_loc bindings.type_bindings

let get_tvar_id_from_value_decl bindings decl_loc =
  let open Bindings in
  let open ValueBinding in
  let binding = LocMap.find decl_loc bindings.value_bindings in
  binding.tvar_id

let get_decl_loc_from_value_use bindings use_loc =
  let binding = get_value_binding bindings use_loc in
  binding.loc

let get_tvar_id_from_value_use bindings use_loc =
  let binding = get_value_binding bindings use_loc in
  binding.tvar_id

let is_global_decl bindings decl_loc =
  let open Bindings in
  let binding = LocMap.find decl_loc bindings.value_bindings in
  binding.is_global

let get_type_decl binding =
  match binding.TypeBinding.declaration with
  | TypeDecl type_decl -> type_decl
  | _ -> failwith "Expected type decl"

let get_type_alias_decl binding =
  match binding.TypeBinding.declaration with
  | TypeAlias alias_decl -> alias_decl
  | _ -> failwith "Expected type alias"

let get_type_param_decl binding =
  match binding.TypeBinding.declaration with
  | TypeParam param_decl -> param_decl
  | _ -> failwith "Expected type parameter"
