open Basic_collections

(* Checking type aliases
 * Type alias definitions may be out of order but may not be cyclic.
 *)

exception CyclicTypeAliasesException of Loc.t * string

module LLMMap = MultiMap.Make (Loc) (Loc)

(* Build a directed graph between type aliases. Add incoming dependency edges to a type alias
   from all other type aliases that appear in its body. *)
class type_alias_graph_builder ~cx =
  object (this)
    inherit [unit] Ast_visitor.visitor as super

    val mutable outgoing_edges = LLMMap.empty

    val mutable current_node = Loc.none

    method outgoing_edges = outgoing_edges

    method set_current_node loc = current_node <- loc

    method add_outgoing_edge incoming_node_loc =
      outgoing_edges <- LLMMap.add incoming_node_loc current_node outgoing_edges

    method! identifier_type acc id =
      let open Ast.Type.Identifier in
      let { name = { name = { loc; _ }; _ }; _ } = id in
      let type_binding = Type_context.get_source_type_binding ~cx loc in
      let (decl_loc, kind) = type_binding.declaration in
      if kind = TypeAlias then this#add_outgoing_edge decl_loc;
      super#identifier_type acc id
  end

let order_type_aliases ~cx modules =
  let alias_graph_builder = new type_alias_graph_builder ~cx in
  let node_to_alias = ref LocMap.empty in
  let unvisited_nodes = ref LocSet.empty in

  (* Gather all type alias nodes and collect all incoming edges between them *)
  List.iter
    (fun { Ast.Module.toplevels; _ } ->
      List.iter
        (fun toplevel ->
          let open Ast.TypeDeclaration in
          match toplevel with
          | Ast.Module.TypeDeclaration
              ({ name = { Ast.Identifier.loc; _ }; decl = Alias ty; _ } as alias) ->
            alias_graph_builder#set_current_node loc;
            alias_graph_builder#type_ () ty;
            node_to_alias := LocMap.add loc alias !node_to_alias;
            unvisited_nodes := LocSet.add loc !unvisited_nodes
          | _ -> ())
        toplevels)
    modules;
  let outgoing_edges = alias_graph_builder#outgoing_edges in

  let in_progress_nodes = ref LocSet.empty in
  let finished_nodes = ref LocSet.empty in
  let topological_sorted_aliases = ref [] in

  let move_sets node old_set new_set =
    old_set := LocSet.remove node !old_set;
    new_set := LocSet.add node !new_set
  in

  (* Visit all nodes in depth first order, sorting topologically and erroring on cycles *)
  let rec visit node =
    if LocSet.mem node !finished_nodes then
      ()
    else if LocSet.mem node !in_progress_nodes then
      let alias = LocMap.find node !node_to_alias in
      raise (CyclicTypeAliasesException (alias.loc, alias.name.name))
    else (
      move_sets node unvisited_nodes in_progress_nodes;
      let next_nodes = LLMMap.find_all node outgoing_edges in
      LocSet.iter visit next_nodes;
      move_sets node in_progress_nodes finished_nodes;
      topological_sorted_aliases := LocMap.find node !node_to_alias :: !topological_sorted_aliases
    )
  in

  while not (LocSet.is_empty !unvisited_nodes) do
    visit (LocSet.choose !unvisited_nodes)
  done;

  !topological_sorted_aliases
