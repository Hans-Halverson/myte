open Ast
open Token
module Env = Parser_env.Env

module ExpressionPrecedence = struct
  type t =
    | (* Binds tightest *) Group
    | Call
    | Access
    | Unary
    | Multiplication
    | Addition
    | Comparison
    | Equality
    | LogicalAnd
    | LogicalOr
    | Ternary (* Precedence for checking ternary operator *)
    | TernaryRightAssociative (* Precedence for parsing ternary operator *)
    | (* Binds weakest *) None

  let level = function
    | Group -> 11
    | Call -> 10
    | Access -> 10
    | Unary -> 8
    | Multiplication -> 8
    | Addition -> 7
    | Comparison -> 6
    | Equality -> 5
    | LogicalAnd -> 4
    | LogicalOr -> 3
    | Ternary -> 2
    | TernaryRightAssociative -> 1
    | None -> 0

  let is_tighter p1 p2 = level p1 > level p2
end

let mark_loc env =
  let start_loc = Env.loc env in
  (fun env -> Loc.between start_loc (Env.prev_loc env))

let rec parse_file file = parse (Parser_env.from_file file)

and parse_string str = parse (Parser_env.from_string str)

and parse env =
  let rec helper toplevels =
    match Env.token env with
    | T_EOF -> List.rev toplevels
    | _ ->
      let toplevel = parse_toplevel env in
      helper (toplevel :: toplevels)
  in
  let (module_, imports, toplevels, errors) =
    try
      let module_ = parse_module env in
      let imports = parse_imports env in
      let toplevels = helper [] in
      (module_, imports, toplevels, Env.errors env)
    with Parse_error.Fatal (loc, err) ->
      let dummy_module =
        {
          Module.Module.loc;
          name = { ScopedIdentifier.loc; name = { Identifier.loc; name = "module" }; scopes = [] };
        }
      in
      (dummy_module, [], [], [(loc, err)])
  in
  let loc = { (Env.loc env) with Loc.start = Loc.first_pos } in
  ({ Module.loc; module_; imports; toplevels }, errors)

and parse_module env =
  let open Module in
  let marker = mark_loc env in
  begin
    match Env.token env with
    | T_MODULE -> Env.advance env
    | token -> Parse_error.fatal (Env.loc env, MissingModule token)
  end;
  let name = parse_scoped_identifier env in
  let loc = marker env in
  { Module.loc; name }

and parse_imports env =
  let open Module.Import in
  let parse_import () =
    let marker = mark_loc env in
    Env.expect env T_IMPORT;
    match Env.token env with
    | T_LEFT_BRACE -> parse_complex_import env marker []
    | _ ->
      let first = parse_identifier env in
      let rec parse_scopes () =
        match Env.token env with
        | T_PERIOD ->
          Env.advance env;
          (match Env.token env with
          | T_LEFT_BRACE -> []
          | _ ->
            let scope = parse_identifier env in
            scope :: parse_scopes ())
        | _ -> []
      in
      let scopes = first :: parse_scopes () in
      (match Env.token env with
      | T_LEFT_BRACE -> parse_complex_import env marker scopes
      | _ ->
        let name = List_utils.last scopes in
        let scopes = List_utils.drop_last scopes in
        let loc = marker env in
        Simple { ScopedIdentifier.loc; name; scopes })
  in
  let rec parse_imports () =
    match Env.token env with
    | T_IMPORT ->
      let import = parse_import () in
      import :: parse_imports ()
    | _ -> []
  in
  parse_imports ()

and parse_complex_import env marker scopes =
  let open Module.Import in
  Env.expect env T_LEFT_BRACE;
  let rec parse_aliases () =
    let marker = mark_loc env in
    let name = parse_identifier env in
    let alias =
      match Env.token env with
      | T_AS ->
        Env.advance env;
        Some (parse_identifier env)
      | _ -> None
    in
    let loc = marker env in
    let alias = { Alias.loc; name; alias } in
    match Env.token env with
    | T_RIGHT_BRACE -> [alias]
    | T_COMMA ->
      Env.advance env;
      (match Env.token env with
      | T_RIGHT_BRACE -> [alias]
      | T_IDENTIFIER _ -> alias :: parse_aliases ()
      | _ -> [alias])
    | _ -> [alias]
  in
  let aliases = parse_aliases () in
  Env.expect env T_RIGHT_BRACE;
  let loc = marker env in
  Complex { Complex.loc; scopes; aliases }

and parse_toplevel env =
  let open Module in
  match Env.token env with
  | T_VAL
  | T_VAR ->
    VariableDeclaration (parse_variable_declaration ~is_toplevel:true env)
  | T_FUN -> FunctionDeclaration (parse_function env)
  | T_TYPE -> TypeDeclaration (parse_type_declaration env)
  | token -> Parse_error.fatal (Env.loc env, MalformedTopLevel token)

and parse_statement env =
  let open Statement in
  match Env.token env with
  | T_LEFT_BRACE -> Block (parse_block env)
  | T_IF -> parse_if env
  | T_WHILE -> parse_while env
  | T_RETURN -> parse_return env
  | T_BREAK -> parse_break env
  | T_CONTINUE -> parse_continue env
  | T_VAL
  | T_VAR ->
    VariableDeclaration (parse_variable_declaration ~is_toplevel:false env)
  | T_FUN -> FunctionDeclaration (parse_function env)
  | _ -> parse_assignment_or_expression_statement env

and parse_assignment_or_expression_statement env =
  let marker = mark_loc env in
  let expr = parse_expression env in
  match Env.token env with
  | T_EQUALS ->
    let pattern = reparse_expression_as_pattern expr in
    Env.advance env;
    let expr = parse_expression env in
    Env.expect env T_SEMICOLON;
    let loc = marker env in
    Statement.Assignment { loc; pattern; expr }
  | _ ->
    Env.expect env T_SEMICOLON;
    let loc = marker env in
    Statement.Expression (loc, expr)

and parse_expression ?(precedence = ExpressionPrecedence.None) env =
  let marker = mark_loc env in
  let expr = parse_expression_prefix env in
  let rec infix expr =
    let expr' = parse_expression_infix ~precedence env expr marker in
    if expr == expr' then
      expr
    else
      infix expr'
  in
  infix expr

and parse_expression_prefix env =
  let open Expression in
  match Env.token env with
  | T_LEFT_PAREN -> parse_parenthesized_expression env
  | T_PLUS
  | T_MINUS
  | T_LOGICAL_NOT ->
    parse_unary_expression env
  | T_IDENTIFIER _ -> Expression.Identifier (parse_identifier env)
  | T_INT_LITERAL (raw, base) ->
    let loc = Env.loc env in
    Env.advance env;
    IntLiteral { IntLiteral.loc; raw; base }
  | T_STRING_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    StringLiteral { StringLiteral.loc; value }
  | T_BOOL_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    BoolLiteral { BoolLiteral.loc; value }
  | token -> Parse_error.fatal (Env.loc env, UnexpectedToken { actual = token; expected = None })

and parse_expression_infix ~precedence env left marker =
  match Env.token env with
  | T_LEFT_PAREN when ExpressionPrecedence.(is_tighter Call precedence) ->
    parse_call env left marker
  | T_LEFT_BRACE when ExpressionPrecedence.(is_tighter Call precedence) ->
    parse_record env left marker
  | T_PERIOD when ExpressionPrecedence.(is_tighter Access precedence) ->
    parse_named_access env left marker
  | T_LEFT_BRACKET when ExpressionPrecedence.(is_tighter Access precedence) ->
    parse_indexed_access env left marker
  | T_PLUS
  | T_MINUS
    when ExpressionPrecedence.(is_tighter Addition precedence) ->
    parse_binary_operation env left marker
  | T_MULTIPLY
  | T_DIVIDE
    when ExpressionPrecedence.(is_tighter Multiplication precedence) ->
    parse_binary_operation env left marker
  | T_LESS_THAN
  | T_GREATER_THAN
  | T_LESS_THAN_OR_EQUAL
  | T_GREATER_THAN_OR_EQUAL
    when ExpressionPrecedence.(is_tighter Comparison precedence) ->
    parse_binary_operation env left marker
  | T_DOUBLE_EQUALS
  | T_NOT_EQUALS
    when ExpressionPrecedence.(is_tighter Equality precedence) ->
    parse_binary_operation env left marker
  | T_LOGICAL_AND when ExpressionPrecedence.(is_tighter LogicalAnd precedence) ->
    parse_logical_expression env left marker
  | T_LOGICAL_OR when ExpressionPrecedence.(is_tighter LogicalOr precedence) ->
    parse_logical_expression env left marker
  | T_QUESTION when ExpressionPrecedence.(is_tighter Ternary precedence) ->
    parse_ternary env left marker
  | _ -> left

and parse_parenthesized_expression env =
  let open Expression in
  let marker = mark_loc env in
  Env.expect env T_LEFT_PAREN;
  match Env.token env with
  | T_RIGHT_PAREN ->
    Env.advance env;
    let loc = marker env in
    Unit { Unit.loc }
  | _ ->
    let expr = parse_expression env in
    (match Env.token env with
    | T_COLON ->
      Env.advance env;
      let ty = parse_type env in
      Env.expect env T_RIGHT_PAREN;
      let loc = marker env in
      TypeCast { TypeCast.loc; expr; ty }
    | T_COMMA ->
      let comma_loc = Env.loc env in
      Env.advance env;
      (* Error if this would be parsed as a single element tuple with a trailing comma *)
      (match Env.token env with
      | T_RIGHT_PAREN ->
        Parse_error.fatal
          (comma_loc, UnexpectedToken { actual = T_COMMA; expected = Some T_RIGHT_PAREN })
      | _ -> ());
      parse_anonymous_tuple_expression env expr marker
    | _ ->
      Env.expect env T_RIGHT_PAREN;
      expr)

and parse_unary_expression env =
  let open Expression.UnaryOperation in
  let marker = mark_loc env in
  let op =
    match Env.token env with
    | T_PLUS -> Plus
    | T_MINUS -> Minus
    | T_LOGICAL_NOT -> LogicalNot
    | _ -> failwith "Invalid prefix operator"
  in
  let op_loc = Env.loc env in
  Env.advance env;
  let next_loc = Env.loc env in
  let no_whitespace_after_op = Loc.pos_equal op_loc._end next_loc.start in
  (* A minus sign directly in front of a numeric literal should be treated as part of that literal *)
  match Env.token env with
  | T_INT_LITERAL (raw, base) when op = Minus && no_whitespace_after_op ->
    Env.advance env;
    let loc = marker env in
    IntLiteral { Expression.IntLiteral.loc; raw = "-" ^ raw; base }
  | _ ->
    let operand = parse_expression ~precedence:Unary env in
    let loc = marker env in
    Expression.UnaryOperation { loc; operand; op }

and parse_binary_operation env left marker =
  let open Expression.BinaryOperation in
  let (op, precedence) =
    match Env.token env with
    | T_PLUS -> (Add, ExpressionPrecedence.Addition)
    | T_MINUS -> (Subtract, ExpressionPrecedence.Addition)
    | T_MULTIPLY -> (Multiply, ExpressionPrecedence.Multiplication)
    | T_DIVIDE -> (Divide, ExpressionPrecedence.Multiplication)
    | T_DOUBLE_EQUALS -> (Equal, ExpressionPrecedence.Equality)
    | T_NOT_EQUALS -> (NotEqual, ExpressionPrecedence.Equality)
    | T_LESS_THAN -> (LessThan, ExpressionPrecedence.Comparison)
    | T_GREATER_THAN -> (GreaterThan, ExpressionPrecedence.Comparison)
    | T_LESS_THAN_OR_EQUAL -> (LessThanOrEqual, ExpressionPrecedence.Comparison)
    | T_GREATER_THAN_OR_EQUAL -> (GreaterThanOrEqual, ExpressionPrecedence.Comparison)
    | _ -> failwith "Invalid binary operator"
  in
  Env.advance env;
  let right = parse_expression ~precedence env in
  let loc = marker env in
  Expression.BinaryOperation { loc; left; right; op }

and parse_logical_expression env left marker =
  let open Expression in
  match Env.token env with
  | T_LOGICAL_AND ->
    Env.advance env;
    let right = parse_expression ~precedence:LogicalAnd env in
    let loc = marker env in
    LogicalAnd { LogicalAnd.loc; left; right }
  | T_LOGICAL_OR ->
    Env.advance env;
    let right = parse_expression ~precedence:LogicalOr env in
    let loc = marker env in
    LogicalOr { LogicalOr.loc; left; right }
  | _ -> failwith "Invalid logical operator"

and parse_ternary env test marker =
  Env.expect env T_QUESTION;
  let conseq = parse_expression ~precedence:TernaryRightAssociative env in
  Env.expect env T_COLON;
  let altern = parse_expression ~precedence:TernaryRightAssociative env in
  let loc = marker env in
  Expression.Ternary { loc; test; conseq; altern }

and parse_anonymous_tuple_expression env first_element marker =
  let open Expression in
  let rec parse_elements () =
    match Env.token env with
    | T_RIGHT_PAREN ->
      Env.advance env;
      []
    | _ ->
      let element = parse_expression env in
      begin
        match Env.token env with
        | T_RIGHT_PAREN -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_PAREN
      end;
      element :: parse_elements ()
  in
  let elements = first_element :: parse_elements () in
  let loc = marker env in
  Tuple { Tuple.loc; elements }

and parse_call env left marker =
  let open Expression.Call in
  Env.expect env T_LEFT_PAREN;
  let rec args env =
    match Env.token env with
    | T_RIGHT_PAREN ->
      Env.advance env;
      []
    | _ ->
      let arg = parse_expression env in
      begin
        match Env.token env with
        | T_RIGHT_PAREN -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_PAREN
      end;
      arg :: args env
  in
  let args = args env in
  let loc = marker env in
  Expression.Call { loc; func = left; args }

and parse_record env name marker =
  let open Expression in
  Env.expect env T_LEFT_BRACE;
  let rec parse_fields () =
    let open Record in
    match Env.token env with
    | T_RIGHT_BRACE ->
      Env.advance env;
      []
    | T_IDENTIFIER _ ->
      let marker = mark_loc env in
      let name = parse_identifier env in
      let field =
        match Env.token env with
        | T_RIGHT_BRACE ->
          let loc = marker env in
          { Field.loc; name; value = None }
        | T_COMMA ->
          let loc = marker env in
          Env.advance env;
          { Field.loc; name; value = None }
        | _ ->
          Env.expect env T_COLON;
          let value = parse_expression env in
          let loc = marker env in
          (match Env.token env with
          | T_RIGHT_BRACE -> ()
          | T_COMMA -> Env.advance env
          | _ -> Env.expect env T_RIGHT_BRACE);
          { Field.loc; name; value = Some value }
      in
      field :: parse_fields ()
    | token ->
      Parse_error.fatal
        (Env.loc env, UnexpectedToken { actual = token; expected = Some T_RIGHT_BRACE })
  in
  let fields = parse_fields () in
  let loc = marker env in
  if fields = [] then Parse_error.fatal (loc, EmptyRecord);
  Record { loc; name; fields }

and parse_named_access env left marker =
  let open Expression.NamedAccess in
  Env.expect env T_PERIOD;
  let name = parse_identifier env in
  let loc = marker env in
  Expression.NamedAccess { loc; target = left; name }

and parse_indexed_access env left marker =
  let open Expression.IndexedAccess in
  Env.expect env T_LEFT_BRACKET;
  let index = parse_expression env in
  Env.expect env T_RIGHT_BRACKET;
  let loc = marker env in
  Expression.IndexedAccess { loc; target = left; index }

and parse_identifier env =
  match Env.token env with
  | T_IDENTIFIER name ->
    let loc = Env.loc env in
    Env.advance env;
    { Identifier.loc; name }
  | token ->
    Parse_error.fatal
      (Env.loc env, UnexpectedToken { actual = token; expected = Some (T_IDENTIFIER "") })

and parse_scoped_identifier env =
  let marker = mark_loc env in
  let rec parse_scoped_identifier () =
    match Env.token env with
    | T_PERIOD ->
      Env.advance env;
      let scope = parse_identifier env in
      scope :: parse_scoped_identifier ()
    | _ -> []
  in
  let first_scope = parse_identifier env in
  let scopes = first_scope :: parse_scoped_identifier () in
  let name = List_utils.last scopes in
  let scopes = List_utils.drop_last scopes in

  let loc = marker env in
  { ScopedIdentifier.loc; name; scopes }

and parse_pattern env =
  match Env.token env with
  | T_IDENTIFIER _ -> parse_identifier_pattern env
  | T_LEFT_PAREN -> parse_anonymous_tuple_pattern env
  | token -> Parse_error.fatal (Env.loc env, MalformedPattern token)

and parse_identifier_pattern env =
  let open Pattern in
  let marker = mark_loc env in
  let id = parse_identifier env in
  match Env.token env with
  | T_PERIOD
  | T_LEFT_PAREN
  | T_LEFT_BRACE ->
    let rec parse_parts () =
      match Env.token env with
      | T_PERIOD ->
        Env.advance env;
        let id = parse_identifier env in
        id :: parse_parts ()
      | _ -> []
    in
    let parts = id :: parse_parts () in
    let (scopes, name) = List_utils.split_last parts in
    let loc = marker env in
    let scoped_id = { Ast.ScopedIdentifier.loc; scopes; name } in
    (match Env.token env with
    | T_LEFT_PAREN -> parse_tuple_pattern env scoped_id marker
    | T_LEFT_BRACE -> parse_record_pattern env scoped_id marker
    | token -> Parse_error.fatal (Env.loc env, MalformedPattern token))
  | _ -> Identifier id

and parse_anonymous_tuple_pattern env =
  let open Pattern in
  let marker = mark_loc env in
  Env.expect env T_LEFT_PAREN;
  let first_element = parse_pattern env in
  Env.expect env T_COMMA;
  let rec parse_elements () =
    match Env.token env with
    | T_RIGHT_PAREN ->
      Env.advance env;
      []
    | _ ->
      let element = parse_pattern env in
      begin
        match Env.token env with
        | T_RIGHT_PAREN -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_PAREN
      end;
      element :: parse_elements ()
  in
  let elements = first_element :: parse_elements () in
  let loc = marker env in
  Tuple { loc; name = None; elements }

and parse_tuple_pattern env name marker =
  let open Pattern in
  Env.expect env T_LEFT_PAREN;
  let rec parse_elements () =
    match Env.token env with
    | T_RIGHT_PAREN ->
      Env.advance env;
      []
    | _ ->
      let element = parse_pattern env in
      begin
        match Env.token env with
        | T_RIGHT_PAREN -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_PAREN
      end;
      element :: parse_elements ()
  in
  let elements = parse_elements () in
  let loc = marker env in
  if elements = [] then Parse_error.fatal (loc, EmptyTuple);
  Tuple { loc; name = Some name; elements }

and parse_record_pattern env name marker =
  let open Pattern in
  Env.expect env T_LEFT_BRACE;
  let rec parse_fields () =
    let open Record in
    match Env.token env with
    | T_RIGHT_BRACE ->
      Env.advance env;
      []
    | T_IDENTIFIER _ ->
      let marker = mark_loc env in
      let name = parse_identifier env in
      let field =
        match Env.token env with
        | T_RIGHT_BRACE ->
          let loc = marker env in
          { Field.loc; name = None; value = Identifier name }
        | T_COMMA ->
          let loc = marker env in
          Env.advance env;
          { Field.loc; name = None; value = Identifier name }
        | _ ->
          Env.expect env T_COLON;
          let value = parse_pattern env in
          let loc = marker env in
          (match Env.token env with
          | T_RIGHT_BRACE -> ()
          | T_COMMA -> Env.advance env
          | _ -> Env.expect env T_RIGHT_BRACE);
          { Field.loc; name = Some name; value }
      in
      field :: parse_fields ()
    | token ->
      Parse_error.fatal
        (Env.loc env, UnexpectedToken { actual = token; expected = Some T_RIGHT_BRACE })
  in
  let fields = parse_fields () in
  let loc = marker env in
  if fields = [] then Parse_error.fatal (loc, EmptyRecord);
  Record { loc; name; fields }

and parse_block env =
  let open Statement in
  let marker = mark_loc env in
  Env.expect env T_LEFT_BRACE;
  let rec statements env =
    match Env.token env with
    | T_RIGHT_BRACE ->
      Env.advance env;
      []
    | _ ->
      let statement = parse_statement env in
      statement :: statements env
  in
  let statements = statements env in
  let loc = marker env in
  { Block.loc; statements }

and parse_if env =
  let open Statement.If in
  let marker = mark_loc env in
  Env.expect env T_IF;
  Env.expect env T_LEFT_PAREN;
  let test = parse_expression env in
  Env.expect env T_RIGHT_PAREN;
  let conseq = parse_statement env in
  let altern =
    match Env.token env with
    | T_ELSE ->
      Env.advance env;
      Some (parse_statement env)
    | _ -> None
  in
  let loc = marker env in
  Statement.If { loc; test; conseq; altern }

and parse_while env =
  let marker = mark_loc env in
  Env.expect env T_WHILE;
  Env.expect env T_LEFT_PAREN;
  let test = parse_expression env in
  Env.expect env T_RIGHT_PAREN;
  let body = parse_statement env in
  let loc = marker env in
  Statement.While { loc; test; body }

and parse_return env =
  let open Statement.Return in
  let marker = mark_loc env in
  Env.expect env T_RETURN;
  let arg =
    match Env.token env with
    | T_SEMICOLON -> None
    | _ -> Some (parse_expression env)
  in
  Env.expect env T_SEMICOLON;
  let loc = marker env in
  Statement.Return { loc; arg }

and parse_break env =
  let marker = mark_loc env in
  Env.expect env T_BREAK;
  Env.expect env T_SEMICOLON;
  let loc = marker env in
  Statement.Break { loc }

and parse_continue env =
  let marker = mark_loc env in
  Env.expect env T_CONTINUE;
  Env.expect env T_SEMICOLON;
  let loc = marker env in
  Statement.Continue { loc }

and parse_type_declaration env =
  let open TypeDeclaration in
  let marker = mark_loc env in
  Env.expect env T_TYPE;
  let parse_type_params_opt () =
    match Env.token env with
    | T_LESS_THAN -> parse_type_params env
    | _ -> []
  in
  match Env.token env with
  | T_ALIAS ->
    Env.advance env;
    let name = parse_identifier env in
    let type_params = parse_type_params_opt () in
    Env.expect env T_EQUALS;
    let alias = parse_type env in
    let loc = marker env in
    { loc; name; type_params; decl = Alias alias }
  | _ ->
    let name_marker = mark_loc env in
    let name = parse_identifier env in
    let type_params = parse_type_params_opt () in
    (match Env.token env with
    | T_LEFT_PAREN ->
      let tuple = parse_tuple_variant env name name_marker in
      let loc = marker env in
      { loc; name; type_params; decl = Tuple tuple }
    | T_LEFT_BRACE ->
      let record = parse_record_variant env name name_marker in
      let loc = marker env in
      { loc; name; type_params; decl = Record record }
    | T_EQUALS ->
      Env.advance env;
      parse_variant env name type_params marker
    | token -> Parse_error.fatal (Env.loc env, MalformedTypeDeclaration token))

and parse_variant env name type_params marker =
  (match Env.token env with
  | T_PIPE -> Env.advance env
  | _ -> ());
  let rec parse_variants () =
    let open TypeDeclaration in
    let marker = mark_loc env in
    let name = parse_identifier env in
    let variant =
      match Env.token env with
      | T_LEFT_BRACE -> RecordVariant (parse_record_variant env name marker)
      | T_LEFT_PAREN -> TupleVariant (parse_tuple_variant env name marker)
      | _ -> EnumVariant name
    in
    match Env.token env with
    | T_PIPE ->
      Env.advance env;
      variant :: parse_variants ()
    | _ -> [variant]
  in
  let variants = parse_variants () in
  let loc = marker env in
  if List.length variants = 1 then Parse_error.fatal (loc, SingleVariant);
  { loc; name; type_params; decl = Variant variants }

and parse_record_variant env name marker =
  let open TypeDeclaration.Record in
  Env.expect env T_LEFT_BRACE;
  let rec parse_fields () =
    match Env.token env with
    | T_RIGHT_BRACE ->
      Env.advance env;
      []
    | _ ->
      let name = parse_identifier env in
      Env.expect env T_COLON;
      let field =
        let ty = parse_type env in
        let loc = marker env in
        (match Env.token env with
        | T_RIGHT_BRACE -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_BRACE);
        { Field.loc; name; ty }
      in
      field :: parse_fields ()
  in
  let fields = parse_fields () in
  let loc = marker env in
  if fields = [] then Parse_error.fatal (loc, EmptyRecord);
  { loc; name; fields }

and parse_tuple_variant env name marker =
  let open TypeDeclaration.Tuple in
  Env.expect env T_LEFT_PAREN;
  let rec parse_elements () =
    match Env.token env with
    | T_RIGHT_PAREN ->
      Env.advance env;
      []
    | _ ->
      let element = parse_type env in
      begin
        match Env.token env with
        | T_RIGHT_PAREN -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_PAREN
      end;
      element :: parse_elements ()
  in
  let elements = parse_elements () in
  let loc = marker env in
  if elements = [] then Parse_error.fatal (loc, EmptyTuple);
  { loc; name; elements }

and parse_variable_declaration ~is_toplevel env =
  let open Statement in
  let marker = mark_loc env in
  let kind =
    match Env.token env with
    | T_VAL -> VariableDeclaration.Immutable
    | T_VAR -> VariableDeclaration.Mutable
    | _ -> failwith "Must be called on variable declaration"
  in
  Env.advance env;
  let pattern = parse_pattern env in
  let annot =
    match Env.token env with
    | T_COLON ->
      Env.advance env;
      Some (parse_type env)
    | _ -> None
  in
  Env.expect env T_EQUALS;
  let init = parse_expression env in
  if not is_toplevel then Env.expect env T_SEMICOLON;
  let loc = marker env in
  { VariableDeclaration.loc; kind; pattern; init; annot }

and parse_function env =
  let open Function in
  let marker = mark_loc env in
  Env.expect env T_FUN;
  let name = parse_identifier env in
  let type_params =
    if Env.token env = T_LESS_THAN then
      parse_type_params env
    else
      []
  in
  Env.expect env T_LEFT_PAREN;
  let rec params env =
    match Env.token env with
    | T_RIGHT_PAREN ->
      Env.advance env;
      []
    | _ ->
      let marker = mark_loc env in
      let name = parse_identifier env in
      Env.expect env T_COLON;
      let annot = parse_type env in
      let loc = marker env in
      begin
        match Env.token env with
        | T_RIGHT_PAREN -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_PAREN
      end;
      let param = { Param.loc; name; annot } in
      param :: params env
  in
  let params = params env in
  let return =
    match Env.token env with
    | T_COLON ->
      Env.advance env;
      Some (parse_type env)
    | _ -> None
  in
  let body =
    match Env.token env with
    | T_LEFT_BRACE -> Block (parse_block env)
    | T_EQUALS ->
      Env.advance env;
      Expression (parse_expression env)
    | token -> Parse_error.fatal (Env.loc env, MalformedFunctionBody token)
  in
  let loc = marker env in
  { loc; name; params; body; return; type_params }

and parse_type env =
  let marker = mark_loc env in
  let ty = parse_type_prefix env in
  match Env.token env with
  | T_ARROW -> parse_function_type env [ty] [] marker
  | _ -> ty

and parse_type_prefix env =
  let open Type in
  match Env.token env with
  | T_UNIT
  | T_BYTE
  | T_INT
  | T_LONG
  | T_STRING
  | T_BOOL ->
    Primitive (parse_primitive_type env)
  | T_LEFT_PAREN -> parse_parenthesized_type env
  | T_LESS_THAN -> parse_function_type_with_type_params env
  | T_IDENTIFIER _ -> Identifier (parse_identifier_type env)
  | token -> Parse_error.fatal (Env.loc env, MalformedType token)

and parse_primitive_type env =
  let open Type.Primitive in
  let kind =
    match Env.token env with
    | T_UNIT -> Unit
    | T_INT -> Int
    | T_BYTE -> Byte
    | T_LONG -> Long
    | T_STRING -> String
    | T_BOOL -> Bool
    | _ -> failwith "Must be called on primitive type"
  in
  let loc = Env.loc env in
  Env.advance env;
  { loc; kind }

and parse_parenthesized_type env =
  let open Type in
  let marker = mark_loc env in
  Env.expect env T_LEFT_PAREN;
  let (tys, trailing_comma_loc) = parse_parenthesized_type_or_params env in
  (* A parenthesized list followed by an arrow is a function type *)
  if Env.token env = T_ARROW then
    parse_function_type env tys [] marker
  (* A parenthesized list of two or more elements not followed by an arrow is a tuple *)
  else if List.length tys <> 1 then
    let loc = marker env in
    Tuple { Tuple.loc; elements = tys }
  else (
    (* Error if there was a trailing comma after a single parenthesized element *)
    (match trailing_comma_loc with
    | Some loc ->
      Parse_error.fatal (loc, UnexpectedToken { actual = T_COMMA; expected = Some T_RIGHT_PAREN })
    | _ -> ());
    List.hd tys
  )

and parse_parenthesized_type_or_params ?(trailing_comma_loc = None) env =
  match Env.token env with
  | T_RIGHT_PAREN ->
    Env.advance env;
    ([], trailing_comma_loc)
  | _ ->
    let ty = parse_type env in
    let trailing_comma_loc =
      match Env.token env with
      | T_RIGHT_PAREN -> None
      | T_COMMA ->
        let loc = Env.loc env in
        Env.advance env;
        Some loc
      | _ ->
        Env.expect env T_RIGHT_PAREN;
        None
    in
    let (tys, trailing_comma_loc) = parse_parenthesized_type_or_params ~trailing_comma_loc env in
    (ty :: tys, trailing_comma_loc)

and parse_identifier_type env =
  let open Type.Identifier in
  let marker = mark_loc env in
  let name = parse_scoped_identifier env in
  let type_params =
    match Env.token env with
    | T_LESS_THAN ->
      Env.advance env;
      (* List of type params must be nonempty *)
      if Env.token env = T_GREATER_THAN then (
        Env.expect env (T_IDENTIFIER "");
        []
      ) else
        let rec type_params env =
          match Env.token env with
          | T_GREATER_THAN ->
            Env.advance env;
            []
          | _ ->
            let ty = parse_type env in
            begin
              match Env.token env with
              | T_GREATER_THAN -> ()
              | T_COMMA -> Env.advance env
              | _ -> Env.expect env T_GREATER_THAN
            end;
            ty :: type_params env
        in
        type_params env
    | _ -> []
  in
  let loc = marker env in
  { loc; name; type_params }

and parse_function_type env params type_params marker =
  let open Ast.Type in
  Env.expect env T_ARROW;
  let return = parse_type env in
  let loc = marker env in
  Function { Function.loc; params; return; type_params }

and parse_function_type_with_type_params env =
  let marker = mark_loc env in
  let type_params = parse_type_params env in
  Env.expect env T_LEFT_PAREN;
  let (params, _) = parse_parenthesized_type_or_params env in
  parse_function_type env params type_params marker

and parse_type_params env =
  Env.expect env T_LESS_THAN;
  (* List of type params must be nonempty *)
  if Env.token env = T_GREATER_THAN then (
    Env.expect env (T_IDENTIFIER "");
    []
  ) else
    let rec type_params env =
      match Env.token env with
      | T_GREATER_THAN ->
        Env.advance env;
        []
      | _ ->
        let marker = mark_loc env in
        let name = parse_identifier env in
        let loc = marker env in
        begin
          match Env.token env with
          | T_GREATER_THAN -> ()
          | T_COMMA -> Env.advance env
          | _ -> Env.expect env T_GREATER_THAN
        end;
        { TypeParameter.loc; name } :: type_params env
    in
    type_params env

and reparse_expression_as_pattern expr =
  let open Expression in
  let invalid_pattern_error expr =
    Parse_error.fatal (Ast_utils.expression_loc expr, Parse_error.InvalidAssignmentPattern)
  in
  let reparse_name_parts name =
    let rec reparse_name_parts expr acc =
      match expr with
      | Identifier id -> id :: acc
      | NamedAccess { NamedAccess.target; name; _ } -> reparse_name_parts target (name :: acc)
      | _ -> invalid_pattern_error expr
    in
    let name_parts = reparse_name_parts name [] in
    let (scopes, name) = List_utils.split_last name_parts in
    let name_loc =
      match scopes with
      | [] -> name.loc
      | hd :: _ -> Loc.between hd.loc name.loc
    in
    { ScopedIdentifier.loc = name_loc; scopes; name }
  in
  match expr with
  | Identifier { loc; name } -> Pattern.Identifier { loc; name }
  | Tuple { loc; elements } ->
    Pattern.Tuple { loc; name = None; elements = List.map reparse_expression_as_pattern elements }
  | Call { loc; func; args } ->
    let name = reparse_name_parts func in
    let elements = List.map reparse_expression_as_pattern args in
    Pattern.Tuple { loc; name = Some name; elements }
  | Record { loc; name; fields } ->
    let name = reparse_name_parts name in
    let fields =
      List.map
        (fun { Record.Field.loc; name; value } ->
          match value with
          | None -> { Pattern.Record.Field.loc; name = None; value = Pattern.Identifier name }
          | Some value ->
            let value = reparse_expression_as_pattern value in
            { Pattern.Record.Field.loc; name = Some name; value })
        fields
    in
    Pattern.Record { loc; name; fields }
  | _ -> invalid_pattern_error expr
