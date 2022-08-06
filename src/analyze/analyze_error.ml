open Types

type t =
  | InexhaustiveReturn of string option
  | UnreachableStatement
  | BreakOutsideLoop
  | ContinueOutsideLoop
  | MissingMainFunction
  | MultipleMainFunctions
  | InvalidMainFunctionType
  | UnresolvedName of string * name_position_type
  | MethodDeclarationsInSameModule of string * string list
  | InvalidWildcardIdentifier
  | BuiltinNotFound of string
  | InvalidAssignment of string * invalid_assignment_kind
  | InvalidLValue of invalid_lvalue_kind
  | DuplicateToplevelNames of string
  | DuplicateParameterNames of string * string option
  | DuplicatePatternNames of string
  | DuplicateTypeParameterNames of string * name_source
  | DuplicateModuleNames of string
  | DuplicateMethodNames of string * string * string list
  | DuplicateRecordFieldNames of string
  | DuplicateRecordFieldAndMethodNames of string * string * string option
  | ModuleAndDeclDuplicateNames of string * string
  | ImportNonexist of string * string list
  | ReferenceChildOfDecl of string * string list
  | ModuleInvalidPosition of string list * name_position_type
  | NoDeclInModule of string * string list * bool
  | ImportPrivateDecl of string
  | AccessPrivateDecl of string
  | PublicMethodInPrivateTrait of trait_type
  | PublicFieldInPrivateType
  | NoModuleWithName of string list * bool
  | NoStaticMethod of string * string * trait_type
  | ReferenceChildOfStaticMethod of string * string * trait_type
  | MismatchedOrPatternName of string
  | ExpectedTrait of string
  | StaticMethodOverride
  | StaticMethodSignature
  | OverrideNonexistentMethod of string
  | OverrideMultipleMethods of string * string * string
  | MissingOverrideKeyword of string * string
  | CyclicTrait of string
  | SuperWithoutAccess
  | SuperNonOverridenMethod of string
  | NoImplementedSuperMethod of string
  | TypeWithAccess of string list
  | ConstructorWithAccess of string list
  | CyclicTypeAlias of string
  | NonTraitAsBound
  | TypeAliasWithBounds
  | ImplicitTypeParamOutsideFunction
  | ImplicitTypeParamInAnonymousFunction
  | TraitObjectExpectsTrait
  | ToplevelVarWithoutAnnotation
  | ToplevelVarWithPattern
  | IncompatibleTypes of Type.t * Type.t list
  | CannotInferType of cannot_infer_type_kind * (Type.t * Type.t list) option
  | OperatorRequiresTrait of operator_requires_trait_kind * Type.t
  | InterpolatedExpressionRequiresToString of Type.t
  | ForLoopRequiresIterable of Type.t
  | NonFunctionCalled of Type.t
  | RecordConstructorCalled of string
  | ExpectedConstructorKind of (* Tuple *) bool * (* Record *) bool
  | IncorrectFunctionArity of int * int
  | IncorrectTupleConstructorArity of int * int
  | IncorrectTypeParametersArity of int * int
  | MissingRecordConstructorFields of string list
  | UnexpectedRecordConstructorField of string * string
  | RecordExpressionWithRest
  | NonIndexableIndexed of Type.t
  | UnresolvedNamedAccess of string * Type.t
  | UnresolvedSuperNamedAccess of string * Type.t
  | InvalidTraitObject
  | TupleIndexIsNotLiteral
  | TupleIndexOutOfBounds of int
  | IntLiteralOutOfRange of Type.t
  | IndexIsNotInteger of Type.t
  | UnimplementedMethodSignature of string * string
  | IncompatibleOverridenMethodType of string * string * Type.t * Type.t
  | IncorrectOverridenMethodTypeParametersArity of string * string * int * int
  | InvalidMultipleArgumentsPattern
  | IfExpressionMissingElse
  | ExpressionStatementMissingSemicolon
  | UnwrapOutsideFunction
  | InvalidTypeUnwrapped of Type.t
  | InvalidUnwrappedReturnType of invalid_unwrapped_return_type_kind * Type.t
  | InexhaustiveMatch of string
  | UnreachablePattern

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
  | InvalidAssignmentMethod
  | InvalidAssignmentThis
  | InvalidAssignmentConstructor
  | InvalidAssignmentMatchCaseVariable
  | InvalidAssignmentImmutableField

and invalid_lvalue_kind = InvalidLValueTuple

and cannot_infer_type_kind =
  | CannotInferTypeVariableDeclaration
  | CannotInferTypeExpression
  | CannotInferTypeAnonFuncParam of string
  | CannotInferTypeAnonFuncReturn

and operator_requires_trait_kind =
  | OperatorRequiresTraitEquals
  | OperatorRequiresTraitNotEquals

and invalid_unwrapped_return_type_kind =
  | InvalidUnwrappedReturnTypeOption
  | InvalidUnwrappedReturnTypeResult of Type.t

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

let string_of_func_name_opt func_name_opt =
  match func_name_opt with
  | None -> "anonymous function"
  | Some func_name -> Printf.sprintf "function `%s`" func_name

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

let operator_requires_trait_info kind =
  match kind with
  | OperatorRequiresTraitEquals -> ("==", "Equatable")
  | OperatorRequiresTraitNotEquals -> ("!=", "Equatable")

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
  | InexhaustiveReturn func_name_opt ->
    let func_name = string_of_func_name_opt func_name_opt in
    Printf.sprintf "All branches of %s must end in a return statement" func_name
  | UnreachableStatement -> "Unreachable statement"
  | BreakOutsideLoop -> "Break cannot appear outside a loop"
  | ContinueOutsideLoop -> "Continue cannot appear outside a loop"
  | MissingMainFunction -> "No main function found in modules"
  | MultipleMainFunctions -> "Main function has already been declared"
  | InvalidMainFunctionType ->
    "Invalid type for main function. The main function takes no parameters and must return either an `Int` or `Unit`."
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
    let with_name kind_string = Printf.sprintf "Cannot reassign%s `%s`" kind_string name in
    (match kind with
    | InvalidAssignmentImmutableVariable -> with_name " immutable variable"
    | InvalidAssignmentFunction -> with_name " function"
    | InvalidAssignmentFunctionParam -> with_name " function parameter"
    | InvalidAssignmentMethod -> "Cannot reassign methods"
    | InvalidAssignmentThis -> with_name ""
    | InvalidAssignmentConstructor -> with_name " constructor"
    | InvalidAssignmentMatchCaseVariable -> with_name " match case variable"
    | InvalidAssignmentImmutableField -> with_name " immutable field")
  | InvalidLValue kind ->
    let kind_string =
      match kind with
      | InvalidLValueTuple -> "Tuples are immutable and cannot have their elements reassigned."
    in
    Printf.sprintf "Invalid left hand side of assignment. %s" kind_string
  | DuplicateToplevelNames name -> Printf.sprintf "Name `%s` already bound in module" name
  | DuplicateParameterNames (param, func_name_opt) ->
    let func_name = string_of_func_name_opt func_name_opt in
    Printf.sprintf "Name `%s` already bound in parameters of %s" param func_name
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
  | ModuleAndDeclDuplicateNames (name, module_) ->
    Printf.sprintf
      "Module and top level declaration with same name `%s` in module `%s`"
      name
      module_
  | ImportNonexist (name, []) -> Printf.sprintf "No top level module with name `%s` found" name
  | ImportNonexist (name, module_parts) ->
    Printf.sprintf
      "No module or declaration with name `%s` found in module `%s`"
      name
      (String.concat "." module_parts)
  | ReferenceChildOfDecl (name, module_parts) ->
    let module_parts_string = String.concat "." module_parts in
    Printf.sprintf
      "`%s` is a top level declaration of module `%s`, there are no items beneath `%s`"
      name
      module_parts_string
      (module_parts_string ^ "." ^ name)
  | ModuleInvalidPosition (module_parts, position) ->
    Printf.sprintf
      "Module `%s` cannot be used as a %s"
      (String.concat "." module_parts)
      (string_of_name_position position)
  | NoDeclInModule (name, module_parts, is_value) ->
    let module_parts_string = String.concat "." module_parts in
    Printf.sprintf
      "Could not resolve `%s.%s`. Module `%s` does not have a %s with name `%s`."
      module_parts_string
      name
      module_parts_string
      (value_or_type is_value)
      name
  | ImportPrivateDecl name -> Printf.sprintf "Cannot import private `%s`" name
  | AccessPrivateDecl name -> Printf.sprintf "Cannot access private `%s`" name
  | PublicMethodInPrivateTrait trait_type ->
    Printf.sprintf "Public method cannot appear in private %s" (string_of_trait_type trait_type)
  | PublicFieldInPrivateType -> Printf.sprintf "Public field cannot appear in private type"
  | NoModuleWithName (module_parts, is_value) ->
    Printf.sprintf
      "No module or %s with name `%s` found"
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
  | MismatchedOrPatternName name ->
    Printf.sprintf "`%s` must appear in all branches of or pattern" name
  | ExpectedTrait name -> Printf.sprintf "Expected `%s` to be a trait" name
  | StaticMethodOverride -> Printf.sprintf "Static methods cannot be overridden"
  | StaticMethodSignature -> Printf.sprintf "Static methods must have an implementation"
  | OverrideNonexistentMethod name ->
    Printf.sprintf "No parent method with name `%s` to override" name
  | OverrideMultipleMethods (method_name, super1, super2) ->
    Printf.sprintf
      "Overriding multiple methods with name `%s`, but only a single method can be overridden. Method `%s` is declared in super traits `%s` and `%s`."
      method_name
      method_name
      super1
      super2
  | MissingOverrideKeyword (method_name, trait_name) ->
    Printf.sprintf
      "Method `%s` overrides method in trait `%s`, but method is not marked as `override`"
      method_name
      trait_name
  | CyclicTrait name -> Printf.sprintf "Cycle detected in super traits of `%s`" name
  | SuperWithoutAccess -> Printf.sprintf "`super` can only be used when accessing a method"
  | SuperNonOverridenMethod method_name ->
    Printf.sprintf
      "Cannot access super method since `%s` is not overridden by this type"
      method_name
  | NoImplementedSuperMethod method_name ->
    Printf.sprintf "No implemented method with name `%s` found in super traits" method_name
  | TypeWithAccess type_name_parts ->
    Printf.sprintf
      "Could not resolve access on type `%s`. Types do not have members that can be accessed."
      (String.concat "." type_name_parts)
  | ConstructorWithAccess ctor_name_parts ->
    Printf.sprintf
      "Could not resolve access on constructor `%s`. Constructors do not have members that can be accessed."
      (String.concat "." ctor_name_parts)
  | CyclicTypeAlias name -> Printf.sprintf "Cycle detected in definition of type `%s`" name
  | NonTraitAsBound -> "Type parameter bound is not a trait"
  | TypeAliasWithBounds -> "Type parameters cannot have bounds in type aliases"
  | ImplicitTypeParamOutsideFunction ->
    "Implicit trait type parameters can only appear in function parameter types"
  | ImplicitTypeParamInAnonymousFunction ->
    "Implicit trait type parameters cannot appear in anonymous function parameter types"
  | TraitObjectExpectsTrait -> "Trait object expects trait"
  | ToplevelVarWithoutAnnotation -> Printf.sprintf "Top level variables must have type annotations"
  | ToplevelVarWithPattern ->
    Printf.sprintf "Top level variable declarations cannot contain patterns"
  | IncompatibleTypes (actual, expecteds) ->
    let type_strings = Types.pps (expecteds @ [actual]) |> List.map (fun str -> "`" ^ str ^ "`") in
    let expected_strings = List_utils.drop_last type_strings in
    let actual_string = List_utils.last type_strings in
    let expected_string =
      match expected_strings with
      | [expected_string] -> "type " ^ expected_string
      | _ -> "types " ^ Error_utils.concat_with_conjunction "or" expected_strings
    in
    Printf.sprintf "Expected %s but found %s" expected_string actual_string
  | CannotInferType (kind, partial) ->
    let kind_string =
      match kind with
      | CannotInferTypeVariableDeclaration -> "type for variable declaration"
      | CannotInferTypeExpression -> "type for expression"
      | CannotInferTypeAnonFuncParam param_name -> Printf.sprintf "type for `%s`" param_name
      | CannotInferTypeAnonFuncReturn -> "anonymous function's return type"
    in
    let partial_string =
      match partial with
      | None -> ""
      | Some (partial_type, unresolved_types) ->
        let type_strings = Types.pps (partial_type :: unresolved_types) in
        let partial_type_string = List.hd type_strings in
        let unresolved_type_names = List.tl type_strings |> List.map (fun s -> "`" ^ s ^ "`") in
        let unresolved_types = Error_utils.concat_with_or unresolved_type_names in
        Printf.sprintf
          "Partially inferred `%s` but was unable to resolve %s. "
          partial_type_string
          unresolved_types
    in
    Printf.sprintf
      "Cannot infer %s. %sPlease provide additional type annotations."
      kind_string
      partial_string
  | OperatorRequiresTrait (kind, ty) ->
    let (operator, trait) = operator_requires_trait_info kind in
    Printf.sprintf
      "Operator `%s` requires type `%s` to implement trait `%s`"
      operator
      (Types.pp ty)
      trait
  | InterpolatedExpressionRequiresToString ty ->
    Printf.sprintf
      "Type `%s` cannot be used in string interpolation since it does not implement trait `ToString`"
      (Types.pp ty)
  | ForLoopRequiresIterable ty ->
    Printf.sprintf
      "Type `%s` cannot be used in for loop since it does not implement trait `Iterable`"
      (Types.pp ty)
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
      "Incorrect number of type parameters. Expected %d %s but found %d."
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
  | RecordExpressionWithRest -> "`...` can only be present in record patterns"
  | NonFunctionCalled ty ->
    Printf.sprintf
      "Only functions can be called, but this expression is inferred to have type `%s`"
      (Types.pp ty)
  | RecordConstructorCalled name ->
    Printf.sprintf "`%s` is a record constructor and cannot be called as a function" name
  | ExpectedConstructorKind (expected_tuple, expected_record) ->
    let kind =
      if expected_tuple && expected_record then
        "tuple or record"
      else if expected_tuple then
        "tuple"
      else
        "record"
    in
    Printf.sprintf "Expected a %s constructor" kind
  | NonIndexableIndexed ty -> Printf.sprintf "Cannot index into value of type `%s`" (Types.pp ty)
  | UnresolvedNamedAccess (field_name, ty) ->
    Printf.sprintf
      "Cannot resolve field or method with name `%s` on type `%s`"
      field_name
      (Types.pp ty)
  | UnresolvedSuperNamedAccess (field_name, ty) ->
    Printf.sprintf
      "Cannot resolve method with name `%s` in super traits of type `%s`"
      field_name
      (Types.pp ty)
  | InvalidTraitObject ->
    Printf.sprintf
      "Trait cannot be used as a trait object since it contains a method with type parameters or the `This` type"
  | TupleIndexIsNotLiteral -> Printf.sprintf "Tuple indices must be int literals"
  | TupleIndexOutOfBounds actual_size ->
    Printf.sprintf
      "Tuple index out of range. Tuple has %d elements so only indices 0 through %d are allowed."
      actual_size
      (actual_size - 1)
  | IntLiteralOutOfRange ty ->
    Printf.sprintf "Integer literal out of range for type `%s`" (Types.pp ty)
  | IndexIsNotInteger ty ->
    Printf.sprintf "Array index must be an integer but found `%s`" (Types.pp ty)
  | UnimplementedMethodSignature (method_name, trait_name) ->
    Printf.sprintf "Method `%s` from trait `%s` not implemented" method_name trait_name
  | IncompatibleOverridenMethodType (method_name, trait_name, actual_ty, expected_ty) ->
    let type_strings = Types.pps [actual_ty; expected_ty] in
    let actual_ty_string = List.hd type_strings in
    let expected_ty_string = List_utils.last type_strings in
    Printf.sprintf
      "Method `%s` has incompatible type from declaration in trait `%s`. Declared as type `%s` but found `%s`."
      method_name
      trait_name
      expected_ty_string
      actual_ty_string
  | IncorrectOverridenMethodTypeParametersArity (method_name, trait_name, actual, expected) ->
    Printf.sprintf
      "Method `%s` has incorrect number of type parameters from declaration in trait `%s`. Expected %d %s but found %d."
      method_name
      trait_name
      expected
      (plural expected "type parameter")
      actual
  | InvalidMultipleArgumentsPattern ->
    "Invalid pattern for match with multiple arguments. The only allowed patterns are the wildcard `_` or a tuple with the same arity as the number of arguments."
  | IfExpressionMissingElse -> "If expression is missing an `else` block"
  | ExpressionStatementMissingSemicolon ->
    "Expression is in a statement position but semicolon is missing"
  | UnwrapOutsideFunction -> "Unwrap expressions can only appear in functions"
  | InvalidTypeUnwrapped ty ->
    Printf.sprintf
      "Expression must be an `Option` or `Result` to be unwrapped, but found `%s`"
      (Types.pp ty)
  | InvalidUnwrappedReturnType (expected, actual) ->
    let expected_string =
      match expected with
      | InvalidUnwrappedReturnTypeOption -> "Option"
      | InvalidUnwrappedReturnTypeResult err_ty -> Printf.sprintf "Result<_, %s>" (Types.pp err_ty)
    in
    Printf.sprintf
      "This unwrap expression can only be used in a function that returns `%s`, but found return type `%s`"
      expected_string
      (Types.pp actual)
  | InexhaustiveMatch witness ->
    Printf.sprintf
      "Inexhaustive pattern matching. For example the pattern `%s` is not matched."
      witness
  | UnreachablePattern -> "Unreachable pattern"
