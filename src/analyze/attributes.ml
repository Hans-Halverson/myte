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

let iter_attribute_items (attributes : Ast.Attribute.t list) (f : Ast.Attribute.item -> unit) =
  List.iter (fun { Ast.Attribute.items; _ } -> List.iter f items) attributes

let visit_builtin_attribute ~(store : AttributeStore.t) ~add_attribute ~module_ ~loc =
  if Module.in_stdlib module_ then
    add_attribute ~store Attribute.Builtin
  else
    add_error ~store loc BuiltinOutsideStdlib

let add_function_attributes
    ~(store : AttributeStore.t) ~(module_ : Module.t) (func : Ast.Function.t) =
  let add_attribute ~store attribute = add_attribute ~store func.name.loc attribute in
  iter_attribute_items func.attributes (fun item ->
      match item with
      | Ast.Attribute.Identifier { name = "Builtin"; loc } ->
        visit_builtin_attribute ~store ~add_attribute ~module_ ~loc
      | Identifier { name = "Inline"; _ } -> add_attribute ~store Attribute.Inline
      | Identifier { name = "NoInline"; _ } -> add_attribute ~store Attribute.NoInline
      | _ -> ())

let add_general_attributes
    ~(store : AttributeStore.t)
    ~(module_ : Module.t)
    (loc : Loc.t)
    (attributes : Ast.Attribute.t list) =
  let add_attribute ~store attribute = add_attribute ~store loc attribute in
  iter_attribute_items attributes (fun item ->
      match item with
      | Ast.Attribute.Identifier { name = "Builtin"; loc } ->
        visit_builtin_attribute ~store ~add_attribute ~module_ ~loc
      | _ -> ())

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
