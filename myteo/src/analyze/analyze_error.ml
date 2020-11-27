open Ast
open Basic_collections

type t =
  | InexhaustiveReturn of Identifier.t
  | UnreachableStatement of unreachable_statement_reason option
  | BreakOutsideLoop
  | ContinueOutsideLoop
  | MissingMainFunction
  | MultipleMainFunctions
  | UnresolvedName of string * bool
  | DuplicateToplevelNames of string * bool
  | DuplicateParameterNames of string * string
  | DuplicateModuleNames of string
  | ModuleAndExportDuplicateNames of string * string
  | ImportNonexist of string * string list
  | ImportChildOfExport of string * string list
  | ModuleInvalidPosition of string list * bool
  | NoExportInModule of string * string list * bool
  | NoModuleWithName of string list * bool
  | RecursiveTypeAlias of string * Types.tvar_id * Types.t
  | ToplevelVarWithoutAnnotation
  | IncompatibleTypes of Types.t * Types.t list
  | VarDeclNeedsAnnotation of string * (Types.t * Types.tvar_id) option
  | NonFunctionCalled of Types.t
  | IncorrectFunctionArity of int * int

and unreachable_statement_reason =
  | AfterReturn
  | AfterBreak
  | AfterContinue

let plural n str =
  if n = 1 then
    str
  else
    str ^ "s"

let to_string error =
  let value_or_type is_value =
    if is_value then
      "value"
    else
      "type"
  in
  match error with
  | InexhaustiveReturn { Identifier.name; _ } ->
    Printf.sprintf "All branches of function %s must end in a return statement" name
  | UnreachableStatement reason ->
    let reason_string =
      match reason with
      | None -> ""
      | Some AfterReturn -> " after return"
      | Some AfterBreak -> " after break"
      | Some AfterContinue -> " after continue"
    in
    "Unreachable statement" ^ reason_string
  | BreakOutsideLoop -> "Break cannot appear outside a loop"
  | ContinueOutsideLoop -> "Continue cannot appear outside a loop"
  | MissingMainFunction -> "No main function found in modules"
  | MultipleMainFunctions -> "Main function has already been declared"
  | UnresolvedName (name, is_value) ->
    Printf.sprintf "Could not resolve name \"%s\" to %s" name (value_or_type is_value)
  | DuplicateToplevelNames (name, is_value) ->
    Printf.sprintf
      "%s with name \"%s\" already bound in module"
      ( if is_value then
        "Value"
      else
        "Type" )
      name
  | DuplicateParameterNames (param, func) ->
    Printf.sprintf "Name \"%s\" already bound in parameters of function \"%s\"" param func
  | DuplicateModuleNames name -> Printf.sprintf "Module already declared with name \"%s\"" name
  | ModuleAndExportDuplicateNames (name, module_) ->
    Printf.sprintf "Module and export with same name \"%s\" in module \"%s\"" name module_
  | ImportNonexist (name, []) -> Printf.sprintf "No toplevel module with name \"%s\" found" name
  | ImportNonexist (name, module_parts) ->
    Printf.sprintf
      "No module or export with name \"%s\" found in module \"%s\""
      name
      (String.concat "." module_parts)
  | ImportChildOfExport (name, module_parts) ->
    let module_parts_string = String.concat "." module_parts in
    Printf.sprintf
      "\"%s\" is an export of module \"%s\", there are no items beneath \"%s\""
      name
      module_parts_string
      (module_parts_string ^ "." ^ name)
  | ModuleInvalidPosition (module_parts, is_value) ->
    Printf.sprintf
      "Module \"%s\" cannot be used as a %s"
      (String.concat "." module_parts)
      (value_or_type is_value)
  | NoExportInModule (export, module_parts, is_value) ->
    let module_parts_string = String.concat "." module_parts in
    Printf.sprintf
      "Could not resolve \"%s.%s\". Module \"%s\" does not have exported %s with name \"%s\"."
      module_parts_string
      export
      module_parts_string
      (value_or_type is_value)
      export
  | NoModuleWithName (module_parts, is_value) ->
    Printf.sprintf
      "No module or exported %s with name \"%s\" found"
      (value_or_type is_value)
      (String.concat "." module_parts)
  | RecursiveTypeAlias (name, id_tvar, ty) ->
    Printf.sprintf
      "Type aliases cannot be recursive. %s cannot be defined as %s"
      name
      (Types.pp ~tvar_to_name:(IMap.singleton id_tvar name) ty)
  | ToplevelVarWithoutAnnotation -> Printf.sprintf "Toplevel variables must have type annotations"
  | IncompatibleTypes (actual, expecteds) ->
    let type_strings = Types.pps (expecteds @ [actual]) in
    let expected_strings = List_utils.drop_last type_strings in
    let actual_string = List_utils.last type_strings in
    let expected_string =
      match expected_strings with
      | [expected_string] -> "type " ^ expected_string
      | _ -> "types " ^ String.concat " or " expected_strings
    in
    Printf.sprintf "Expected %s but found %s" expected_string actual_string
  | VarDeclNeedsAnnotation (name, partial) ->
    let partial_string =
      match partial with
      | None -> ""
      | Some (partial_type, unresolved_tvar_id) ->
        let (partial_type_string, tvar_to_name) = Types.pps_with_tvar_map [partial_type] in
        let unresolved_tvar_name = IMap.find unresolved_tvar_id tvar_to_name in
        Printf.sprintf
          "Partially inferred %s but was unable to resolve %s. "
          (List.hd partial_type_string)
          unresolved_tvar_name
    in
    Printf.sprintf
      "Cannot infer type for \"%s\". %sPlease provide additional type hints such as a type annotation."
      name
      partial_string
  | IncorrectFunctionArity (actual, expected) ->
    Printf.sprintf
      "Incorrect number of arguments supplied to function. Expected %d %s but found %d."
      expected
      (plural expected "argument")
      actual
  | NonFunctionCalled ty ->
    Printf.sprintf
      "Only functions can be called, but this expression is inferred to have type %s."
      (Types.pp ty)
