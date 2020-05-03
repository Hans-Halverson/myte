let analyze_program prog =
  let errors = Exhaustive_returns.analyze prog in
  errors

let analyze_programs progs =
  let main_errors = Main_function.analyze progs in
  let single_program_errors = List.flatten (List.map analyze_program progs) in
  main_errors @ single_program_errors
