open Ast
open Basic_collections

type value_declaration =
  | VarDecl
  | FunDecl
  | ImportedValue of Identifier.t
  | ImportedModule of Module_tree.module_tree
  | FunParam

type type_declaration =
  | TypeDecl
  | ImportedType of Identifier.t
  | ImportedModule of Module_tree.module_tree

module ValueBinding = struct
  type t = {
    name: string;
    declaration: Loc.t * value_declaration;
    uses: LocSet.t;
    tvar_id: Types.tvar_id;
  }
end

module TypeBinding = struct
  type t = {
    name: string;
    declaration: Loc.t * type_declaration;
    uses: LocSet.t;
    tvar_id: Types.tvar_id;
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

let get_source_value_binding bindings use_loc =
  let open Bindings in
  let local_decl_loc = LocMap.find use_loc bindings.value_use_to_decl in
  let local_binding = LocMap.find local_decl_loc bindings.value_bindings in
  match local_binding.ValueBinding.declaration with
  | (_, ImportedValue { Ast.Identifier.loc = source_decl_loc; _ }) ->
    LocMap.find source_decl_loc bindings.value_bindings
  | _ -> local_binding

let get_source_type_binding bindings use_loc =
  let open Bindings in
  let local_decl_loc = LocMap.find use_loc bindings.type_use_to_decl in
  let local_binding = LocMap.find local_decl_loc bindings.type_bindings in
  match local_binding.TypeBinding.declaration with
  | (_, ImportedType { Ast.Identifier.loc = source_decl_loc; _ }) ->
    LocMap.find source_decl_loc bindings.type_bindings
  | _ -> local_binding

let get_tvar_id_from_value_decl bindings decl_loc =
  let open Bindings in
  let open ValueBinding in
  let binding = LocMap.find decl_loc bindings.value_bindings in
  binding.tvar_id

let get_tvar_id_from_type_decl bindings decl_loc =
  let open Bindings in
  let binding = LocMap.find decl_loc bindings.type_bindings in
  binding.tvar_id

let get_source_decl_loc_from_value_use bindings use_loc =
  let binding = get_source_value_binding bindings use_loc in
  fst binding.declaration

let get_tvar_id_from_value_use bindings use_loc =
  let binding = get_source_value_binding bindings use_loc in
  binding.tvar_id

let get_tvar_id_from_type_use bindings use_loc =
  let binding = get_source_type_binding bindings use_loc in
  binding.tvar_id
