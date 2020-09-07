open Ast
open Basic_collections

type t =
  | InexhaustiveReturn of Identifier.t
  | UnreachableStatementAfterReturn
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
  | UnreachableStatementAfterReturn -> "Unreachable statement after return"
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
