open Ast

type t =
  | InexhaustiveReturn of Identifier.t
  | UnreachableStatement of unreachable_statement_reason option
  | BreakOutsideLoop
  | ContinueOutsideLoop
  | MissingMainFunction
  | MultipleMainFunctions
  | UnresolvedName of string * name_position_type
  | MethodDeclarationsInSameModule of string * string list
  | InvalidWildcardIdentifier
  | BuiltinNotFound of string
  | InvalidAssignment of string * invalid_assignment_kind
  | InvalidLValue of invalid_lvalue_kind
  | DuplicateToplevelNames of string
  | DuplicateParameterNames of string * string
  | DuplicatePatternNames of string
  | DuplicateTypeParameterNames of string * name_source
  | DuplicateModuleNames of string
  | DuplicateMethodNames of string * string * string list
  | DuplicateRecordFieldNames of string
  | DuplicateRecordFieldAndMethodNames of string * string * string option
  | ModuleAndExportDuplicateNames of string * string
  | ImportNonexist of string * string list
  | ReferenceChildOfExport of string * string list
  | ModuleInvalidPosition of string list * name_position_type
  | NoExportInModule of string * string list * bool
  | NoModuleWithName of string list * bool
  | NoStaticMethod of string * string * trait_type
  | ReferenceChildOfStaticMethod of string * string * trait_type
  | ExpectedTrait of string
  | OverrideNonexistentMethod of string
  | OverrideMultipleMethods of string * string * string
  | CyclicTrait of string
  | TypeWithAccess of string list
  | CyclicTypeAlias of string
  | ToplevelVarWithoutAnnotation
  | IncompatibleTypes of Types.t * Types.t list
  | CannotInferType of cannot_infer_type_kind * (Types.t * Types.tvar_id list) option
  | NonFunctionCalled of Types.t
  | RecordConstructorCalled of string
  | ExpectedRecordConstructor
  | ExpectedTupleConstructor
  | IncorrectFunctionArity of int * int
  | IncorrectTupleConstructorArity of int * int
  | IncorrectTypeParametersArity of int * int
  | MissingRecordConstructorFields of string list
  | UnexpectedRecordConstructorField of string * string
  | NonIndexableIndexed of Types.t
  | NonAccessibleAccessed of string * Types.t
  | TupleIndexIsNotLiteral
  | TupleIndexOutOfBounds of int
  | NamedAccessNonexistentField of string * string
  | IntLiteralOutOfRange of Types.t
  | IndexIsNotInteger of Types.t

and unreachable_statement_reason =
  | AfterReturn
  | AfterBreak
  | AfterContinue

and name_position_type =
  | NamePositionValue
  | NamePositionType
  | NamePositionCtor

and trait_type =
  | TraitTrait
  | TraitType

and invalid_assignment_kind =
  | InvalidAssignmentImmutableVariable
  | InvalidAssignmentFunction
  | InvalidAssignmentFunctionParam
  | InvalidAssignmentConstructor

and invalid_lvalue_kind = InvalidLValueTuple

and cannot_infer_type_kind =
  | CannotInferTypeVariableDeclaration
  | CannotInferTypeExpression

and name_source =
  | FunctionName of string
  | TypeName of string

type error = Loc.t * t

type errors = error list

let plural n str =
  if n = 1 then
    str
  else
    str ^ "s"

let string_of_name_source source =
  match source with
  | FunctionName name -> Printf.sprintf "function `%s`" name
  | TypeName name -> Printf.sprintf "type `%s`" name

let value_or_type is_value =
  if is_value then
    "value"
  else
    "type"

let string_of_name_position position =
  match position with
  | NamePositionValue -> "value"
  | NamePositionType -> "type"
  | NamePositionCtor -> "constructor"

let string_of_trait_type trait_or_type =
  match trait_or_type with
  | TraitTrait -> "trait"
  | TraitType -> "type"

let concat_with_and strs =
  match strs with
  | [] -> ""
  | [str] -> str
  | [str1; str2] -> str1 ^ " and " ^ str2
  | strs ->
    let (strs, last_str) = List_utils.split_last strs in
    String.concat ", " strs ^ ", and " ^ last_str

let to_string error =
  match error with
  | InexhaustiveReturn { Identifier.name; _ } ->
    Printf.sprintf "All branches of function `%s` must end in a return statement" name
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
  | UnresolvedName (name, position) ->
    Printf.sprintf "Could not resolve name `%s` to %s" name (string_of_name_position position)
  | MethodDeclarationsInSameModule (type_name, module_parts) ->
    Printf.sprintf
      "Method declarations must appear in same module as type declaration. Type `%s` is declared in module `%s`."
      type_name
      (String.concat "." module_parts)
  | InvalidWildcardIdentifier ->
    Printf.sprintf "`_` is the wildcard pattern and cannot be used as an identifier"
  | BuiltinNotFound name -> Printf.sprintf "No declaration found for builtin `%s`" name
  | InvalidAssignment (name, kind) ->
    let kind_string =
      match kind with
      | InvalidAssignmentImmutableVariable -> "immutable variable"
      | InvalidAssignmentFunction -> "function"
      | InvalidAssignmentFunctionParam -> "function parameter"
      | InvalidAssignmentConstructor -> "constructor"
    in
    Printf.sprintf "Cannot reassign %s `%s`" kind_string name
  | InvalidLValue kind ->
    let kind_string =
      match kind with
      | InvalidLValueTuple -> "Tuples are immutable and cannot have their elements reassigned."
    in
    Printf.sprintf "Invalid left hand side of assignment. %s" kind_string
  | DuplicateToplevelNames name -> Printf.sprintf "Name `%s` already bound in module" name
  | DuplicateParameterNames (param, func) ->
    Printf.sprintf "Name `%s` already bound in parameters of function `%s`" param func
  | DuplicatePatternNames name -> Printf.sprintf "Name `%s` already bound in pattern" name
  | DuplicateTypeParameterNames (param, source) ->
    Printf.sprintf
      "Name `%s` already bound in type parameters of %s"
      param
      (string_of_name_source source)
  | DuplicateModuleNames name -> Printf.sprintf "Module already declared with name `%s`" name
  | DuplicateMethodNames (method_name, trait_name, super_traits) ->
    (match super_traits with
    | [] ->
      Printf.sprintf "Multiple methods with name `%s` declared for `%s`" method_name trait_name
    | [super] ->
      Printf.sprintf
        "Multiple methods with name `%s` declared for `%s`. Method `%s` is already declared in super trait `%s`."
        method_name
        trait_name
        method_name
        super
    | [super1; super2] ->
      Printf.sprintf
        "Multiple methods with name `%s` declared for `%s`. Method `%s` is declared in super traits `%s` and `%s`."
        method_name
        trait_name
        method_name
        super1
        super2
    | _ -> failwith "At most two supertraits")
  | DuplicateRecordFieldNames name ->
    Printf.sprintf "Field with name `%s` already declared in record" name
  | DuplicateRecordFieldAndMethodNames (method_name, trait_name, super_trait_opt) ->
    (match super_trait_opt with
    | None -> Printf.sprintf "Field with name `%s` already declared for `%s`" method_name trait_name
    | Some super_trait ->
      Printf.sprintf
        "Method with name `%s` from super trait `%s` conflicts with field of the same name for `%s`"
        method_name
        super_trait
        trait_name)
  | ModuleAndExportDuplicateNames (name, module_) ->
    Printf.sprintf "Module and export with same name `%s` in module `%s`" name module_
  | ImportNonexist (name, []) -> Printf.sprintf "No toplevel module with name `%s` found" name
  | ImportNonexist (name, module_parts) ->
    Printf.sprintf
      "No module or export with name `%s` found in module `%s`"
      name
      (String.concat "." module_parts)
  | ReferenceChildOfExport (name, module_parts) ->
    let module_parts_string = String.concat "." module_parts in
    Printf.sprintf
      "`%s` is an export of module `%s`, there are no items beneath `%s`"
      name
      module_parts_string
      (module_parts_string ^ "." ^ name)
  | ModuleInvalidPosition (module_parts, position) ->
    Printf.sprintf
      "Module `%s` cannot be used as a %s"
      (String.concat "." module_parts)
      (string_of_name_position position)
  | NoExportInModule (export, module_parts, is_value) ->
    let module_parts_string = String.concat "." module_parts in
    Printf.sprintf
      "Could not resolve `%s.%s`. Module `%s` does not have exported %s with name `%s`."
      module_parts_string
      export
      module_parts_string
      (value_or_type is_value)
      export
  | NoModuleWithName (module_parts, is_value) ->
    Printf.sprintf
      "No module or exported %s with name `%s` found"
      (value_or_type is_value)
      (String.concat "." module_parts)
  | NoStaticMethod (method_name, trait_name, trait_type) ->
    Printf.sprintf
      "No static method with name `%s` for %s `%s`"
      method_name
      (string_of_trait_type trait_type)
      trait_name
  | ReferenceChildOfStaticMethod (method_name, trait_name, trait_type) ->
    Printf.sprintf
      "`%s` is a static method for %s `%s`, there are no items beneath it"
      method_name
      (string_of_trait_type trait_type)
      trait_name
  | ExpectedTrait name -> Printf.sprintf "Expected `%s` to be a trait" name
  | OverrideNonexistentMethod name ->
    Printf.sprintf "No parent method with name `%s` to override" name
  | OverrideMultipleMethods (method_name, super1, super2) ->
    Printf.sprintf
      "Overriding multiple methods with name `%s`, but only a single method can be overridden. Method `%s` is declared in super traits `%s` and `%s`."
      method_name
      method_name
      super1
      super2
  | CyclicTrait name -> Printf.sprintf "Cycle detected in super traits of `%s`" name
  | TypeWithAccess type_name_parts ->
    Printf.sprintf
      "Could not resolve access on type `%s`. Types do not have members that can be accessed."
      (String.concat "." type_name_parts)
  | CyclicTypeAlias name -> Printf.sprintf "Cycle detected in definition of type `%s`" name
  | ToplevelVarWithoutAnnotation -> Printf.sprintf "Toplevel variables must have type annotations"
  | IncompatibleTypes (actual, expecteds) ->
    let type_strings = Types.pps (expecteds @ [actual]) |> List.map (fun str -> "`" ^ str ^ "`") in
    let expected_strings = List_utils.drop_last type_strings in
    let actual_string = List_utils.last type_strings in
    let expected_string =
      match expected_strings with
      | [expected_string] -> "type " ^ expected_string
      | _ -> "types " ^ String.concat " or " expected_strings
    in
    Printf.sprintf "Expected %s but found %s" expected_string actual_string
  | CannotInferType (kind, partial) ->
    let kind_string =
      match kind with
      | CannotInferTypeVariableDeclaration -> "variable declaration"
      | CannotInferTypeExpression -> "expression"
    in
    let partial_string =
      match partial with
      | None -> ""
      | Some (partial_type, unresolved_tvar_ids) ->
        let type_strings =
          Types.pps (partial_type :: List.map (fun id -> Types.TVar id) unresolved_tvar_ids)
        in
        let partial_type_string = List.hd type_strings in
        let unresolved_tvar_names = List.tl type_strings |> List.map (fun s -> "`" ^ s ^ "`") in
        let unresolved_tvars = Error_utils.concat_with_or unresolved_tvar_names in
        Printf.sprintf
          "Partially inferred `%s` but was unable to resolve %s. "
          partial_type_string
          unresolved_tvars
    in
    Printf.sprintf
      "Cannot infer type for %s. %sPlease provide additional type annotations."
      kind_string
      partial_string
  | IncorrectFunctionArity (actual, expected) ->
    Printf.sprintf
      "Incorrect number of arguments supplied to function. Expected %d %s but found %d."
      expected
      (plural expected "argument")
      actual
  | IncorrectTupleConstructorArity (actual, expected) ->
    Printf.sprintf
      "Incorrect number of arguments supplied to tuple constructor. Expected %d %s but found %d."
      expected
      (plural expected "argument")
      actual
  | IncorrectTypeParametersArity (actual, expected) ->
    Printf.sprintf
      "Incorrect number of type parameters passed to type. Expected %d %s but found %d."
      expected
      (plural expected "type parameter")
      actual
  | MissingRecordConstructorFields field_names ->
    let field_names = List.map (fun name -> "`" ^ name ^ "`") field_names in
    Printf.sprintf
      "Record is missing %s %s"
      (plural (List.length field_names) "field")
      (concat_with_and field_names)
  | UnexpectedRecordConstructorField (record_name, field_name) ->
    Printf.sprintf "Record `%s` does not have a field named `%s`" record_name field_name
  | NonFunctionCalled ty ->
    Printf.sprintf
      "Only functions can be called, but this expression is inferred to have type `%s`"
      (Types.pp ty)
  | RecordConstructorCalled name ->
    Printf.sprintf "`%s` is a record constructor and cannot be called as a function" name
  | ExpectedRecordConstructor -> Printf.sprintf "Expected a record constructor"
  | ExpectedTupleConstructor -> Printf.sprintf "Expected a tuple constructor"
  | NonIndexableIndexed ty -> Printf.sprintf "Cannot index into value of type `%s`" (Types.pp ty)
  | NonAccessibleAccessed (field_name, ty) ->
    Printf.sprintf "Cannot access field `%s` on non-record type `%s`" field_name (Types.pp ty)
  | TupleIndexIsNotLiteral -> Printf.sprintf "Tuple indices must be int literals"
  | TupleIndexOutOfBounds actual_size ->
    Printf.sprintf
      "Tuple index out of range. Tuple has %d elements so only indices 0 through %d are allowed."
      actual_size
      (actual_size - 1)
  | NamedAccessNonexistentField (record_name, field_name) ->
    Printf.sprintf "Record `%s` does not have a field named `%s`" record_name field_name
  | IntLiteralOutOfRange ty ->
    Printf.sprintf "Integer literal out of range for type `%s`" (Types.pp ty)
  | IndexIsNotInteger ty ->
    Printf.sprintf "Array index must be an integer but found `%s`" (Types.pp ty)
