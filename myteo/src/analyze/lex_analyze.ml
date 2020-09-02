let analyze_module mod_ =
  let errors = Exhaustive_returns.analyze mod_ in
  errors

let analyze_modules mods_and_files =
  let mods = List.map snd mods_and_files in
  let main_errors = Main_function.analyze mods in
  let (module_tree, module_tree_errors) = Module_tree.analyze mods in
  let (resolved_mods, _, resolution_errors) = Name_resolution.analyze mods_and_files module_tree in
  let single_module_errors =
    List.flatten (List.map (fun (_, mod_) -> analyze_module mod_) resolved_mods)
  in
  ( resolved_mods,
    List.concat [module_tree_errors; main_errors; resolution_errors; single_module_errors] )
