open Basic_collections
open Graph

(* Build a directed graph between type aliases. Add incoming dependency edges to a type alias
   from all other type aliases that appear in its body. *)
class type_alias_graph_builder ~bindings ~graph =
  object (this)
    inherit Ast_visitor.visitor as super

    val mutable current_node = Loc.none

    val mutable node_to_alias = LocMap.empty

    method node_to_alias = node_to_alias

    method add_node loc alias =
      current_node <- loc;
      node_to_alias <- LocMap.add loc alias node_to_alias;
      LocGraph.add_node ~graph loc

    method! type_declaration decl =
      match decl with
      | { name; decl = Alias ty; _ } as alias ->
        this#add_node name.loc alias;
        this#type_ ty
      | _ -> ()

    method! identifier_type id =
      let open Ast.Type.Identifier in
      let { name = { name = { loc; _ }; _ }; _ } = id in
      let type_binding_opt = LocMap.find_opt loc bindings.Bindings.Bindings.type_use_to_binding in
      (match type_binding_opt with
      | Some { loc; declaration = TypeAlias _; _ } -> LocGraph.add_edge ~graph loc current_node
      | _ -> ());
      super#identifier_type id
  end

let order_type_aliases ~bindings modules =
  (* Build graph of type aliases *)
  let graph = LocGraph.mk () in
  let alias_graph_builder = new type_alias_graph_builder ~bindings ~graph in
  List.iter
    (fun { Ast.Module.toplevels; _ } ->
      List.iter
        (fun toplevel ->
          match toplevel with
          | Ast.Module.TypeDeclaration decl -> alias_graph_builder#type_declaration decl
          | _ -> ())
        toplevels)
    modules;
  let node_to_alias = alias_graph_builder#node_to_alias in

  (* Topologically sort type alias locs, then reconstruct ordered aliases from locs *)
  try
    let sorted_nodes = LocGraph.topological_sort ~graph in
    let sorted_aliases = List.map (fun node -> LocMap.find node node_to_alias) sorted_nodes in
    Ok sorted_aliases
  with
  | LocGraph.CycleException node ->
    let alias = LocMap.find node node_to_alias in
    Error alias
