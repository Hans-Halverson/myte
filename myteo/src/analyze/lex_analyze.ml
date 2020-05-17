let analyze_module mod_ =
  let errors = Exhaustive_returns.analyze mod_ in
  errors

let analyze_modules mods =
  let main_errors = Main_function.analyze mods in
  let (_, resolution_errors) = Name_resolution.analyze mods in
  let single_module_errors = List.flatten (List.map analyze_module mods) in
  List.concat [main_errors; resolution_errors; single_module_errors]
