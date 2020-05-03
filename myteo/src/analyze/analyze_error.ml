type t = InexhaustiveReturn

let to_string error =
  match error with
  | InexhaustiveReturn -> "All branches must contain a return statement"
