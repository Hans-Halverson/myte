type program_context = {
  modules: (string * Ast.Module.t) list;
  bindings: Bindings.Bindings.t;
  module_tree: Module_tree.module_tree_node Basic_collections.SMap.t;
  main_loc: Loc.t;
  type_ctx: Type_context.t;
}

let analyze_module mod_ =
  let errors = Exhaustive_returns.analyze mod_ in
  errors

let analyze_modules mods_and_files =
  let mods = List.map snd mods_and_files in
  let (main_loc, main_errors) = Main_function.analyze mods in
  let (module_tree, module_tree_errors) = Module_tree.analyze mods in
  let (resolved_mods, bindings, resolution_errors) =
    Name_resolution.analyze mods_and_files module_tree
  in
  let single_module_errors =
    List.flatten (List.map (fun (_, mod_) -> analyze_module mod_) resolved_mods)
  in
  let pre_typecheck_errors =
    List.concat [module_tree_errors; main_errors; resolution_errors; single_module_errors]
  in
  if List.length pre_typecheck_errors <> 0 then
    Error pre_typecheck_errors
  else
    let type_ctx = Type_check.analyze resolved_mods bindings in
    let type_check_errors = type_ctx.errors in
    if List.length type_check_errors <> 0 then
      Error type_check_errors
    else
      Ok
        { modules = resolved_mods; bindings; module_tree; main_loc = Option.get main_loc; type_ctx }
