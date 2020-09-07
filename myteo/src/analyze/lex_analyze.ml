let analyze_module mod_ =
  let errors = Exhaustive_returns.analyze mod_ in
  errors

let analyze_modules mods_and_files =
  let mods = List.map snd mods_and_files in
  let main_errors = Main_function.analyze mods in
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
    (resolved_mods, pre_typecheck_errors)
  else
    let type_check_errors = Type_check.analyze resolved_mods bindings in
    (resolved_mods, type_check_errors)
