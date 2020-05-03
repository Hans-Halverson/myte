open Ast

type t =
  | InexhaustiveReturn of unit Identifier.t
  | UnreachableStatementAfterReturn
  | MissingMainFunction
  | MultipleMainFunctions

let to_string error =
  match error with
  | InexhaustiveReturn { Identifier.name; _ } ->
    Printf.sprintf "All branches of function %s must end in a return statement" name
  | UnreachableStatementAfterReturn -> "Unreachable statement after return"
  | MissingMainFunction -> "No main function found in program"
  | MultipleMainFunctions -> "Main function has already been declared"
