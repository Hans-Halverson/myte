let analyze_program prog =
  let errors = Exhaustive_returns.analyze prog in
  errors
