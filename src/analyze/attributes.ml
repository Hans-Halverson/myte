open Basic_collections

module Attribute = struct
  type t =
    | Builtin
    | Inline
    | NoInline
end

module AttributeStore = struct
  type t = {
    mutable attributes: Attribute.t list LocMap.t;
    mutable errors: Analyze_error.error list;
  }

  let empty = { attributes = LocMap.empty; errors = [] }
end

let add_attribute ~(store : AttributeStore.t) (loc : Loc.t) (attribute : Attribute.t) =
  let new_attributes =
    match LocMap.find_opt loc store.attributes with
    | None -> [attribute]
    | Some attributes -> attribute :: attributes
  in
  store.attributes <- LocMap.add loc new_attributes store.attributes

let add_error ~(store : AttributeStore.t) (loc : Loc.t) (error : Analyze_error.t) =
  store.errors <- (loc, error) :: store.errors

let visit_builtin_attribute ~(store : AttributeStore.t) ~add_attribute ~module_ ~loc =
  if Module.in_stdlib module_ then
    add_attribute ~store Attribute.Builtin
  else
    add_error ~store loc BuiltinOutsideStdlib

let add_function_attributes
    ~(store : AttributeStore.t) ~(module_ : Module.t) (func : Ast.Function.t) =
  let add_attribute ~store attribute = add_attribute ~store func.name.loc attribute in
  List.iter
    (fun attr ->
      let open Ast.Attribute in
      match attr with
      | { name = { loc; name = "Builtin" }; _ } ->
        visit_builtin_attribute ~store ~add_attribute ~module_ ~loc
      | { name = { name = "Inline"; _ }; _ } -> add_attribute ~store Attribute.Inline
      | { name = { name = "NoInline"; _ }; _ } -> add_attribute ~store Attribute.NoInline
      | _ -> ())
    func.attributes

let add_general_attributes
    ~(store : AttributeStore.t)
    ~(module_ : Module.t)
    (loc : Loc.t)
    (attributes : Ast.Attribute.t list) =
  let add_attribute ~store attribute = add_attribute ~store loc attribute in
  List.iter
    (fun attr ->
      let open Ast.Attribute in
      match attr with
      | { name = { loc; name = "Builtin" }; _ } ->
        visit_builtin_attribute ~store ~add_attribute ~module_ ~loc
      | _ -> ())
    attributes

let get_attributes ~(store : AttributeStore.t) (loc : Loc.t) : Attribute.t list =
  match LocMap.find_opt loc store.attributes with
  | None -> []
  | Some attributes -> attributes

let has_attribute ~(store : AttributeStore.t) (loc : Loc.t) (target : Attribute.t) : bool =
  List.exists (fun attribute -> attribute == target) (get_attributes ~store loc)

let is_builtin ~(store : AttributeStore.t) (loc : Loc.t) : bool = has_attribute ~store loc Builtin

let has_inline ~(store : AttributeStore.t) (loc : Loc.t) : bool = has_attribute ~store loc Inline

let has_no_inline ~(store : AttributeStore.t) (loc : Loc.t) : bool =
  has_attribute ~store loc NoInline
