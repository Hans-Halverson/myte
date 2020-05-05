let analyze_module mod_ =
  let errors = Exhaustive_returns.analyze mod_ in
  errors

let analyze_modules mods =
  let main_errors = Main_function.analyze mods in
  let single_module_errors = List.flatten (List.map analyze_module mods) in
  main_errors @ single_module_errors
