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
  | ModuleAndExportDuplicateNames of string * string
  | ImportNonexist of string * string list
  | ImportChildOfExport of string * string list
  | ModuleInValuePosition of string list
  | NoExportInModule of string * string list
  | NoModuleWithName of string list

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
  | ModuleInValuePosition module_parts ->
    Printf.sprintf "Module \"%s\" cannot be used as a value" (String.concat "." module_parts)
  | NoExportInModule (export, module_parts) ->
    let module_parts_string = String.concat "." module_parts in
    Printf.sprintf
      "Could not resolve \"%s.%s\". Module \"%s\" does not have export with name \"%s\"."
      module_parts_string
      export
      module_parts_string
      export
  | NoModuleWithName module_parts ->
    Printf.sprintf "No module or export with name \"%s\" found" (String.concat "." module_parts)
