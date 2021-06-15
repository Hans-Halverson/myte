open Program_context

let analyze_modules ~is_stdlib ~pcx mods_and_files =
  let mods = List.map snd mods_and_files in

  (* Check for main function for non-library compilation units *)
  let main_errors =
    if is_stdlib then
      []
    else
      let (main_loc, main_errors) = Main_function.analyze mods in
      Program_context.set_main_loc ~pcx main_loc;
      main_errors
  in

  (* Update module tree with modules in this compilation unit *)
  let (module_tree, module_tree_errors) = Module_tree.analyze pcx.module_tree mods in
  Program_context.set_module_tree ~pcx module_tree;

  (* Resolve symbols in this compilation unit. Updates the set of all bindings and store
     the newly resolved ASTs *)
  let (new_resolved_mods, new_bindings, resolution_errors) =
    Name_resolution.analyze ~is_stdlib pcx.module_tree mods_and_files
  in
  Program_context.add_resolved_modules ~pcx new_resolved_mods;
  Program_context.add_bindings ~pcx new_bindings;

  (* Perform other single-module structural analyses, collecting errors *)
  let single_module_errors =
    List.flatten
      (List.map
         (fun (_, mod_) ->
           let exhaustive_returns_errors = Exhaustive_returns.analyze mod_ in
           let reachability_errors = Reachability.analyze mod_ in
           exhaustive_returns_errors @ reachability_errors)
         new_resolved_mods)
  in

  (* Only proceed to type checking if there are no pre-typechecking errors *)
  let pre_typecheck_errors =
    List.concat [module_tree_errors; main_errors; resolution_errors; single_module_errors]
  in
  if List.length pre_typecheck_errors <> 0 then
    Error pre_typecheck_errors
  else (
    Type_check.analyze ~cx:pcx.type_ctx new_resolved_mods;
    let type_check_errors = Type_context.get_errors ~cx:pcx.type_ctx in
    if List.length type_check_errors <> 0 then
      Error type_check_errors
    else
      Ok new_resolved_mods
  )
