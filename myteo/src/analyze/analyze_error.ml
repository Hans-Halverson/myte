open Ast

type t =
  | InexhaustiveReturn of unit Identifier.t
  | UnreachableStatementAfterReturn
  | MissingMainFunction
  | MultipleMainFunctions
  | UnresolvedName of string
  | DuplicateToplevelNames of string
  | DuplicateParameterNames of string * string
  | DuplicateModuleNames of string

let to_string error =
  match error with
  | InexhaustiveReturn { Identifier.name; _ } ->
    Printf.sprintf "All branches of function %s must end in a return statement" name
  | UnreachableStatementAfterReturn -> "Unreachable statement after return"
  | MissingMainFunction -> "No main function found in modules"
  | MultipleMainFunctions -> "Main function has already been declared"
  | UnresolvedName name -> Printf.sprintf "Could not resolve name \"%s\"" name
  | DuplicateToplevelNames name -> Printf.sprintf "Name \"%s\" already bound in module" name
  | DuplicateParameterNames (param, func) ->
    Printf.sprintf "Name \"%s\" already bound in parameters of function \"%s\"" param func
  | DuplicateModuleNames name -> Printf.sprintf "Module already declared with name \"%s\"" name
