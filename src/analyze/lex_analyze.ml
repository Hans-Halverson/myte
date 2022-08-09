open Program_context

let analyze_modules ~is_stdlib ~pcx mods_and_files =
  Program_context.add_modules ~pcx mods_and_files;
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
  let (ordered_traits, ordered_type_aliases, resolution_errors) =
    Name_resolution.analyze ~pcx ~is_stdlib mods_and_files
  in
  Type_context.set_ordered_traits ~cx:pcx.type_ctx ordered_traits;
  Type_context.set_ordered_type_aliases ~cx:pcx.type_ctx ordered_type_aliases;

  (* Only proceed to type checking if there are no pre-typechecking errors *)
  let pre_typecheck_errors =
    List.concat [module_tree_errors; main_errors; resolution_errors; pcx.attribute_store.errors]
  in
  if List.length pre_typecheck_errors <> 0 then
    Error pre_typecheck_errors
  else (
    Type_check.analyze ~cx:pcx.type_ctx mods_and_files;
    let type_check_errors = Type_context.get_errors ~cx:pcx.type_ctx in
    if type_check_errors <> [] then
      Error type_check_errors
    else
      (* Only proceed to match exhaustiveness checking if there are no typechecking errors *)
      let match_errors = Match.analyze ~cx:pcx.type_ctx mods_and_files in
      if match_errors <> [] then
        Error match_errors
      else
        Ok ()
  )
