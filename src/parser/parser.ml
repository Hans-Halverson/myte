open Ast
open Token
module Env = Parser_env.Env

module ExpressionPrecedence = struct
  type t =
    | (* Binds tightest *) Group
    | Call
    | Access
    | Unwrap
    | Unary
    | Multiplication
    | Addition
    | BitwiseShift
    | BitwiseAnd
    | BitwiseXor
    | BitwiseOr
    | Comparison
    | Equality
    | LogicalAnd
    | LogicalOr
    | (* Binds weakest *) None

  let level = function
    | Group -> 14
    | Call -> 13
    | Access -> 13
    | Unwrap -> 12
    | Unary -> 11
    | Multiplication -> 10
    | Addition -> 9
    | BitwiseShift -> 8
    | BitwiseAnd -> 7
    | BitwiseXor -> 6
    | BitwiseOr -> 5
    | Comparison -> 4
    | Equality -> 3
    | LogicalAnd -> 2
    | LogicalOr -> 1
    | None -> 0

  let is_tighter p1 p2 = level p1 > level p2
end

let mark_loc env =
  let start_loc = Env.loc env in
  (fun env -> Loc.between start_loc (Env.prev_loc env))

let rec parse_file file =
  let open Parse_error in
  if not (Sys.file_exists file) then
    Error [(Loc.none, FileDoesNotExist file)]
  else if Sys.is_directory file then
    Error [(Loc.none, FileIsDirectory file)]
  else
    try
      let env = Parser_env.from_file file in
      parse env
    with
    | Sys_error _ -> Error [(Loc.none, CannotOpenFile file)]

and parse_string str = parse (Parser_env.from_string str)

and parse env =
  let rec helper toplevels =
    match Env.token env with
    | T_EOF -> List.rev toplevels
    | _ ->
      let toplevel = parse_toplevel env in
      helper (toplevel :: toplevels)
  in
  try
    let name = parse_module_name env in
    let imports = parse_imports env in
    let toplevels = helper [] in
    let loc = { (Env.loc env) with Loc.start = Loc.first_pos } in
    let errors = Env.errors env in
    if errors = [] then
      Ok { Module.loc; name; imports; toplevels }
    else
      Error errors
  with
  | Parse_error.Fatal (loc, err) -> Error [(loc, err)]

and parse_module_name env =
  let marker = mark_loc env in
  begin
    match Env.token env with
    | T_MODULE -> Env.advance env
    | token -> Parse_error.fatal (Env.loc env, MissingModule token)
  end;
  let name = parse_scoped_identifier env in
  let loc = marker env in
  { Module.Name.loc; name }

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
  let marker = mark_loc env in
  let pub_loc = Env.loc env in
  let attributes = parse_attribute_annotations env in
  let is_public = maybe_parse_public env in
  match Env.token env with
  | T_VAL
  | T_VAR ->
    VariableDeclaration
      (parse_variable_declaration ~is_toplevel:true ~attributes ~is_public env marker)
  | T_FUN ->
    FunctionDeclaration
      (parse_function ~attributes ~is_public ~is_static:false ~is_override:false marker env)
  | T_TYPE -> TypeDeclaration (parse_type_declaration ~attributes ~is_public env marker)
  | T_METHODS when is_public -> Parse_error.fatal (pub_loc, PublicMethodsBlock)
  | T_METHODS ->
    TraitDeclaration
      (parse_trait_declaration
         ~kind:TraitDeclaration.Methods
         ~attributes
         ~is_public:false
         env
         marker)
  | T_TRAIT ->
    TraitDeclaration
      (parse_trait_declaration ~kind:TraitDeclaration.Trait ~attributes ~is_public env marker)
  | token -> Parse_error.fatal (Env.loc env, MalformedTopLevel token)

and maybe_parse_public env =
  if Env.token env == T_PUB then (
    Env.advance env;
    true
  ) else
    false

and parse_attribute_annotations env =
  let rec maybe_parse_attribute_annotation acc =
    match Env.token env with
    | T_AT ->
      Env.advance env;
      Env.expect env T_LEFT_PAREN;
      let rec parse_attributes acc =
        match Env.token env with
        | T_RIGHT_PAREN ->
          Env.advance env;
          acc
        | _ ->
          let attribute = parse_attribute env in
          begin
            match Env.token env with
            | T_RIGHT_PAREN -> ()
            | T_COMMA -> Env.advance env
            | _ -> Env.expect env T_RIGHT_PAREN
          end;
          parse_attributes (attribute :: acc)
      in
      maybe_parse_attribute_annotation (parse_attributes acc)
    | _ -> List.rev acc
  in
  maybe_parse_attribute_annotation []

and parse_attribute env =
  let marker = mark_loc env in
  let name = parse_identifier env in
  match Env.token env with
  | T_LEFT_PAREN ->
    Env.advance env;
    let rec parse_params acc =
      match Env.token env with
      | T_RIGHT_PAREN ->
        Env.advance env;
        List.rev acc
      | _ ->
        let param = parse_attribute_param env in
        (match Env.token env with
        | T_RIGHT_PAREN -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_PAREN);
        parse_params (param :: acc)
    in
    let params = parse_params [] in
    let loc = marker env in
    { Attribute.loc; name; params }
  | _ ->
    let loc = marker env in
    { Attribute.loc; name; params = [] }

and parse_attribute_param env =
  let marker = mark_loc env in
  let attribute = parse_attribute env in
  match (attribute, Env.token env) with
  | ({ name; params = []; _ }, T_EQUALS) ->
    Env.advance env;
    let value = parse_attribute_literal env in
    let loc = marker env in
    Attribute.Param.Pair { loc; key = name; value }
  | _ -> Attribute.Param.Attribute attribute

and parse_attribute_literal env =
  match Env.token env with
  (* Literals in declaration patterns are allowed *)
  | T_BOOL_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    Attribute.Literal.Bool { Expression.BoolLiteral.loc; value }
  | T_INT_LITERAL (raw, base) ->
    let loc = Env.loc env in
    Env.advance env;
    Attribute.Literal.Int { Expression.IntLiteral.loc; raw; base }
  | T_STRING_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    Attribute.Literal.String { Expression.StringLiteral.loc; value }
  | token -> Parse_error.fatal (Env.loc env, MalformedAttributeLiteral token)

and parse_statement env =
  let open Statement in
  match Env.token env with
  | T_LEFT_BRACE -> Block (parse_block env)
  | T_IF -> If (parse_if env)
  | T_MATCH -> Match (parse_match ~is_expr:false env)
  | T_WHILE -> parse_while env
  | T_FOR -> parse_for env
  | T_RETURN -> parse_return ~in_match_case:false env
  | T_BREAK -> parse_break ~in_match_case:false env
  | T_CONTINUE -> parse_continue ~in_match_case:false env
  | T_VAL
  | T_VAR ->
    let marker = mark_loc env in
    VariableDeclaration
      (parse_variable_declaration ~is_toplevel:false ~attributes:[] ~is_public:false env marker)
  | T_FUN ->
    let marker = mark_loc env in
    FunctionDeclaration
      (parse_function
         ~attributes:[]
         ~is_public:false
         ~is_static:false
         ~is_override:false
         marker
         env)
  | _ -> parse_assignment_or_expression_statement env

and parse_assignment_or_expression_statement env =
  let marker = mark_loc env in
  let expr = parse_expression env in
  let parse_assignment op =
    let lvalue =
      match op with
      | None -> reparse_expression_as_lvalue expr
      | Some _ -> reparse_expression_as_assignment_op_lvalue expr
    in
    Env.advance env;
    let expr = parse_expression env in
    Env.expect env T_SEMICOLON;
    let loc = marker env in
    Statement.Assignment { loc; op; lvalue; expr }
  in
  match Env.token env with
  (* If followed by an equals or equals operation, expression is actually the lvalue for an
     assignment statement *)
  | T_EQUALS -> parse_assignment None
  | T_PLUS_EQUALS -> parse_assignment (Some Add)
  | T_MINUS_EQUALS -> parse_assignment (Some Subtract)
  | T_MULTIPLY_EQUALS -> parse_assignment (Some Multiply)
  | T_DIVIDE_EQUALS -> parse_assignment (Some Divide)
  | T_PERCENT_EQUALS -> parse_assignment (Some Remainder)
  | T_AMPERSAND_EQUALS -> parse_assignment (Some BitwiseAnd)
  | T_PIPE_EQUALS -> parse_assignment (Some BitwiseOr)
  | T_CARET_EQUALS -> parse_assignment (Some BitwiseXor)
  | T_LEFT_SHIFT_EQUALS -> parse_assignment (Some LeftShift)
  | T_ARITHMETIC_RIGHT_SHIFT_EQUALS -> parse_assignment (Some ArithmeticRightShift)
  | T_LOGICAL_RIGHT_SHIFT_EQUALS -> parse_assignment (Some LogicalRightShift)
  (* If the expression is not followed by an equals then it must be an expression statement *)
  | _ ->
    (* An expression statement must be terminated by a semicolon unless it is the last statement
       in a block, in which case it is the value of the block. *)
    let is_value =
      match Env.token env with
      | T_SEMICOLON ->
        Env.advance env;
        false
      | T_RIGHT_BRACE -> true
      (* Provide a missing semicolon error if incorrect token is next *)
      | _ ->
        Env.expect env T_SEMICOLON;
        false
    in
    let loc = marker env in
    Statement.ExpressionStatement { loc; expr; is_value }

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
  | T_BANG ->
    parse_unary_expression env
  | T_MATCH -> Match (parse_match ~is_expr:true env)
  | T_IDENTIFIER _ -> Expression.Identifier (parse_identifier env)
  | T_WILDCARD ->
    let loc = Env.loc env in
    Env.advance env;
    Expression.Identifier { Identifier.loc; name = "_" }
  | T_INT_LITERAL (raw, base) ->
    let loc = Env.loc env in
    Env.advance env;
    IntLiteral { IntLiteral.loc; raw; base }
  | T_FLOAT_LITERAL raw ->
    let loc = Env.loc env in
    Env.advance env;
    FloatLiteral { FloatLiteral.loc; raw }
  | T_CHAR_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    CharLiteral { CharLiteral.loc; value }
  | T_STRING_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    StringLiteral { StringLiteral.loc; value }
  | T_BOOL_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    BoolLiteral { BoolLiteral.loc; value }
  | T_INTERPOLATED_STRING (first_string, is_end) ->
    parse_interpolated_string env first_string is_end
  | T_LEFT_BRACKET -> parse_vec_literal env
  | T_LEFT_BRACE -> parse_map_literal env
  | T_SET_OPEN -> parse_set_literal env
  | T_FN -> parse_anonymous_function env
  | T_IF -> If (parse_if env)
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
  | T_PERCENT
    when ExpressionPrecedence.(is_tighter Multiplication precedence) ->
    parse_binary_operation env left marker
  | T_AMPERSAND when ExpressionPrecedence.(is_tighter BitwiseAnd precedence) ->
    parse_binary_operation env left marker
  | T_PIPE when ExpressionPrecedence.(is_tighter BitwiseOr precedence) && Env.can_use_bitwise_or env
    ->
    parse_binary_operation env left marker
  | T_CARET when ExpressionPrecedence.(is_tighter BitwiseXor precedence) ->
    parse_binary_operation env left marker
  | T_LESS_THAN -> parse_less_than_infix_expression ~precedence env left marker
  | T_GREATER_THAN -> parse_greater_than_infix_expression ~precedence env left marker
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
  | T_QUESTION when ExpressionPrecedence.(is_tighter Unwrap precedence) ->
    parse_unwrap_expression env left marker
  | _ -> left

and parse_less_than_infix_expression ~precedence env left marker =
  let first_loc = Env.loc env in
  let (second_loc, second_token) = Env.peek env in
  if
    second_token = T_LESS_THAN
    && ExpressionPrecedence.(is_tighter BitwiseShift precedence)
    && Loc.are_adjacent first_loc second_loc
  then (
    (* Two adjacent less thans are a left shift *)
    Env.expect env T_LESS_THAN;
    Env.expect env T_LESS_THAN;
    let right = parse_expression ~precedence:BitwiseShift env in
    let loc = marker env in
    Expression.BinaryOperation { loc; left; right; op = LeftShift }
  ) else if ExpressionPrecedence.(is_tighter Comparison precedence) then (
    (* Otherwise this is a less than comparison *)
    Env.expect env T_LESS_THAN;
    let right = parse_expression ~precedence:Comparison env in
    let loc = marker env in
    Expression.BinaryOperation { loc; left; right; op = LessThan }
  ) else
    left

and parse_greater_than_infix_expression ~precedence env left marker =
  let first_loc = Env.loc env in
  let (second_loc, second_token) = Env.peek env in
  (* Two adjacent greater thans are a right shift of some form *)
  if
    second_token = T_GREATER_THAN
    && ExpressionPrecedence.(is_tighter BitwiseShift precedence)
    && Loc.are_adjacent first_loc second_loc
  then (
    Env.expect env T_GREATER_THAN;
    Env.expect env T_GREATER_THAN;
    let third_loc = Env.loc env in
    let third_token = Env.token env in
    if third_token = T_GREATER_THAN && Loc.are_adjacent second_loc third_loc then (
      Env.expect env T_GREATER_THAN;
      (* Three adjacent greater thans are a logical right shift *)
      let right = parse_expression ~precedence:BitwiseShift env in
      let loc = marker env in
      Expression.BinaryOperation { loc; left; right; op = LogicalRightShift }
    ) else
      (* Two adjacent greater thans are an arithmetic right shift *)
      let right = parse_expression ~precedence:BitwiseShift env in
      let loc = marker env in
      Expression.BinaryOperation { loc; left; right; op = ArithmeticRightShift }
  ) else if ExpressionPrecedence.(is_tighter Comparison precedence) then (
    (* Otherwise this is a greater than comparison *)
    Env.expect env T_GREATER_THAN;
    let right = parse_expression ~precedence:Comparison env in
    let loc = marker env in
    Expression.BinaryOperation { loc; left; right; op = GreaterThan }
  ) else
    left

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
      let tuple = parse_anonymous_tuple_expression env expr marker in
      tuple
    | _ ->
      Env.expect env T_RIGHT_PAREN;
      expr)

(* When current token is a minus, if next token is an int literal then consume the minus and
   int literal and return the negative int literal, otherwise consume the minus and return None. *)
and maybe_parse_negative_int_literal env =
  let marker = mark_loc env in
  let minus_loc = Env.loc env in
  Env.expect env T_MINUS;
  let next_loc = Env.loc env in
  let no_whitespace_after_op = Loc.pos_equal minus_loc._end next_loc.start in
  (* A minus sign directly in front of a numeric literal should be treated as part of that literal *)
  match Env.token env with
  | T_INT_LITERAL (raw, base) when no_whitespace_after_op ->
    Env.advance env;
    let loc = marker env in
    Some { Expression.IntLiteral.loc; raw = "-" ^ raw; base }
  | _ -> None

and parse_unary_expression env =
  let open Expression.UnaryOperation in
  let marker = mark_loc env in
  let op =
    match Env.token env with
    | T_PLUS -> Plus
    | T_MINUS -> Minus
    | T_BANG -> Not
    | _ -> failwith "Invalid prefix operator"
  in
  if op = Minus then
    match maybe_parse_negative_int_literal env with
    | Some lit -> IntLiteral lit
    | None ->
      let operand = parse_expression ~precedence:Unary env in
      let loc = marker env in
      Expression.UnaryOperation { loc; operand; op }
  else (
    Env.advance env;
    let operand = parse_expression ~precedence:Unary env in
    let loc = marker env in
    Expression.UnaryOperation { loc; operand; op }
  )

and parse_binary_operation env left marker =
  let open Expression.BinaryOperation in
  let (op, precedence) =
    match Env.token env with
    | T_PLUS -> (Add, ExpressionPrecedence.Addition)
    | T_MINUS -> (Subtract, ExpressionPrecedence.Addition)
    | T_MULTIPLY -> (Multiply, ExpressionPrecedence.Multiplication)
    | T_DIVIDE -> (Divide, ExpressionPrecedence.Multiplication)
    | T_PERCENT -> (Remainder, ExpressionPrecedence.Multiplication)
    | T_AMPERSAND -> (BitwiseAnd, ExpressionPrecedence.BitwiseAnd)
    | T_PIPE -> (BitwiseOr, ExpressionPrecedence.BitwiseOr)
    | T_CARET -> (BitwiseXor, ExpressionPrecedence.BitwiseXor)
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

and parse_unwrap_expression env operand marker =
  let open Expression in
  Env.advance env;
  let loc = marker env in
  Unwrap { Unwrap.loc; operand }

and parse_interpolated_string env first_string is_single_string =
  let open Expression.InterpolatedString in
  let marker = mark_loc env in
  let loc = Env.loc env in
  Env.advance env;
  if is_single_string then
    InterpolatedString { loc; parts = [String { loc; value = first_string }] }
  else
    (* String parts are only included if they are nonempty, e.g. `${expr1}${expr2}` has two
       expression parts and no string parts. *)
    let rec parse_parts env acc =
      let expr = parse_expression env in
      let acc = Expression expr :: acc in
      (* Enter interpolated string mode before call to Env.advance within Env.expect. This ensures
         the token after the right brace is lexed as an interpolated string token. *)
      Env.enter_interpolated_string env;
      Env.expect env T_RIGHT_BRACE;
      match Env.token env with
      | T_INTERPOLATED_STRING (value, is_end) ->
        Env.exit_interpolated_string env;
        let loc = Env.loc env in
        Env.advance env;
        let acc =
          if value = "" then
            acc
          else
            String { loc; value } :: acc
        in
        if is_end then
          List.rev acc
        else
          parse_parts env acc
      | _ -> failwith "Interpolated string is always returned when parsing interpolated strings"
    in
    let maybe_first_part =
      if first_string = "" then
        []
      else
        [String { loc; value = first_string }]
    in
    let parts = parse_parts env maybe_first_part in
    let loc = marker env in
    InterpolatedString { loc; parts }

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
  let rest = ref None in
  Env.expect env T_LEFT_BRACE;
  let rec parse_fields () =
    let open Record in
    match Env.token env with
    | T_RIGHT_BRACE ->
      Env.advance env;
      []
    | T_PERIOD ->
      (* `...`, followed by an optional trailing comma, followed by right brace *)
      let marker = mark_loc env in
      Env.expect env T_PERIOD;
      Env.expect env T_PERIOD;
      Env.expect env T_PERIOD;
      let loc = marker env in
      (match Env.token env with
      | T_COMMA -> Env.advance env
      | _ -> ());
      Env.expect env T_RIGHT_BRACE;
      rest := Some loc;
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
  Record { loc; name; fields; rest = !rest }

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

and parse_vec_literal env =
  let marker = mark_loc env in
  Env.expect env T_LEFT_BRACKET;
  let rec elements env =
    match Env.token env with
    | T_RIGHT_BRACKET ->
      Env.advance env;
      []
    | _ ->
      let element = parse_expression env in
      begin
        match Env.token env with
        | T_RIGHT_BRACKET -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_BRACKET
      end;
      element :: elements env
  in
  let elements = elements env in
  let loc = marker env in
  Expression.VecLiteral { loc; elements }

and parse_map_literal env =
  let marker = mark_loc env in
  Env.expect env T_LEFT_BRACE;
  let rec entries env =
    match Env.token env with
    | T_RIGHT_BRACE ->
      Env.advance env;
      []
    | _ ->
      let marker = mark_loc env in
      let key = parse_expression env in
      Env.expect env T_COLON;
      let value = parse_expression env in
      let loc = marker env in
      let entry = { Expression.MapLiteral.Entry.loc; key; value } in
      begin
        match Env.token env with
        | T_RIGHT_BRACE -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_BRACE
      end;
      entry :: entries env
  in
  let entries = entries env in
  let loc = marker env in
  Expression.MapLiteral { loc; entries }

and parse_set_literal env =
  let marker = mark_loc env in
  Env.expect env T_SET_OPEN;
  let rec elements env =
    match Env.token env with
    | T_SET_CLOSE ->
      Env.advance env;
      []
    | _ ->
      let element = parse_expression env in
      begin
        match Env.token env with
        | T_SET_CLOSE -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_SET_CLOSE
      end;
      element :: elements env
  in
  let elements = elements env in
  let loc = marker env in
  Expression.SetLiteral { loc; elements }

and parse_anonymous_function env =
  let open Expression.AnonymousFunction in
  let marker = mark_loc env in
  Env.expect env T_FN;
  let (params, return) = parse_anonymous_function_signature env in
  let body =
    match Env.token env with
    | T_LEFT_BRACE -> Block (parse_block env)
    | T_ARROW ->
      Env.advance env;
      Expression (parse_expression env)
    | token -> Parse_error.fatal (Env.loc env, MalformedFunctionBody token)
  in
  let loc = marker env in
  Expression.AnonymousFunction { loc; params; body; return }

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

and parse_pattern ?(precedence = ExpressionPrecedence.None) ~is_decl env =
  let marker = mark_loc env in
  let patt = parse_pattern_prefix ~is_decl env in
  let rec infix patt =
    let patt' = parse_pattern_infix ~precedence ~is_decl env patt marker in
    if patt == patt' then
      patt
    else
      infix patt'
  in
  infix patt

and parse_pattern_prefix ~is_decl env =
  let open Pattern in
  match Env.token env with
  | T_WILDCARD ->
    let loc = Env.loc env in
    Env.advance env;
    Wildcard loc
  | T_IDENTIFIER _ -> parse_identifier_pattern ~is_decl env
  | T_LEFT_PAREN -> parse_parenthesized_pattern ~is_decl env
  (* Literals are not allowed in declaration patterns *)
  | T_BOOL_LITERAL _
  | T_INT_LITERAL _
  | T_CHAR_LITERAL _
  | T_STRING_LITERAL _
    when is_decl ->
    Parse_error.fatal (Env.loc env, LiteralInPattern)
  (* Literals in declaration patterns are allowed *)
  | T_BOOL_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    Literal (Literal.Bool { Expression.BoolLiteral.loc; value })
  | T_INT_LITERAL (raw, base) ->
    let loc = Env.loc env in
    Env.advance env;
    Literal (Literal.Int { Expression.IntLiteral.loc; raw; base })
  | T_CHAR_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    Literal (Literal.Char { Expression.CharLiteral.loc; value })
  | T_STRING_LITERAL value ->
    let loc = Env.loc env in
    Env.advance env;
    Literal (Literal.String { Expression.StringLiteral.loc; value })
  (* Minus may be start of negative int literal if not in declaration *)
  | T_MINUS when not is_decl ->
    let minus_loc = Env.loc env in
    (match maybe_parse_negative_int_literal env with
    | Some lit -> Literal (Literal.Int lit)
    | None -> Parse_error.fatal (minus_loc, MalformedPattern T_MINUS))
  | token -> Parse_error.fatal (Env.loc env, MalformedPattern token)

and parse_pattern_infix ~precedence ~is_decl env left marker =
  match Env.token env with
  | T_PIPE when ExpressionPrecedence.(is_tighter BitwiseOr precedence) ->
    Env.advance env;
    let right = parse_pattern ~precedence:ExpressionPrecedence.BitwiseOr ~is_decl env in
    let loc = marker env in
    Or { loc; left; right }
  | T_AS ->
    Env.advance env;
    let name = parse_identifier env in
    let loc = marker env in
    Binding { loc; pattern = left; name }
  | _ -> left

and parse_identifier_pattern ~is_decl env =
  let open Pattern in
  let marker = mark_loc env in
  let id = parse_identifier env in
  match Env.token env with
  | T_PERIOD
  | T_WILDCARD
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
    | T_LEFT_PAREN -> parse_tuple_pattern ~is_decl env scoped_id marker
    | T_LEFT_BRACE -> parse_record_pattern ~is_decl env scoped_id marker
    | T_WILDCARD ->
      Env.advance env;
      let loc = marker env in
      NamedWildcard { loc; name = scoped_id }
    | _ -> Identifier scoped_id)
  | _ -> Identifier (id_to_scoped_id id)

and parse_parenthesized_pattern ~is_decl env =
  let open Pattern in
  let marker = mark_loc env in
  Env.expect env T_LEFT_PAREN;
  match Env.token env with
  (* A unit literal can appear in non-declaration patterns *)
  | T_RIGHT_PAREN ->
    Env.advance env;
    let loc = marker env in
    if is_decl then
      Parse_error.fatal (loc, LiteralInPattern)
    else
      Literal (Literal.Unit { Expression.Unit.loc })
  (* Otherwise this is a parenthesized or anonymous tuple pattern *)
  | _ ->
    let rec parse_elements () =
      match Env.token env with
      | T_RIGHT_PAREN ->
        Env.advance env;
        []
      | _ ->
        let element = parse_pattern ~is_decl env in
        begin
          match Env.token env with
          | T_RIGHT_PAREN -> ()
          | T_COMMA -> Env.advance env
          | _ -> Env.expect env T_RIGHT_PAREN
        end;
        element :: parse_elements ()
    in
    let elements = parse_elements () in
    (match elements with
    | [element] -> element
    | elements ->
      let loc = marker env in
      Tuple { loc; name = None; elements })

and parse_tuple_pattern ~is_decl env name marker =
  let open Pattern in
  Env.expect env T_LEFT_PAREN;
  let rec parse_elements () =
    match Env.token env with
    | T_RIGHT_PAREN ->
      Env.advance env;
      []
    | _ ->
      let element = parse_pattern ~is_decl env in
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

and parse_record_pattern ~is_decl env name marker =
  let open Pattern in
  Env.expect env T_LEFT_BRACE;
  let rest = ref false in
  let rec parse_fields () =
    let open Record in
    match Env.token env with
    | T_RIGHT_BRACE ->
      Env.advance env;
      []
    | T_PERIOD ->
      (* `...`, followed by an optional trailing comma, followed by right brace *)
      Env.expect env T_PERIOD;
      Env.expect env T_PERIOD;
      Env.expect env T_PERIOD;
      (match Env.token env with
      | T_COMMA -> Env.advance env
      | _ -> ());
      Env.expect env T_RIGHT_BRACE;
      rest := true;
      []
    | T_IDENTIFIER _ ->
      let marker = mark_loc env in
      let name = parse_identifier env in
      let field =
        match Env.token env with
        | T_RIGHT_BRACE ->
          let loc = marker env in
          { Field.loc; name = None; value = Identifier (id_to_scoped_id name) }
        | T_COMMA ->
          let loc = marker env in
          Env.advance env;
          { Field.loc; name = None; value = Identifier (id_to_scoped_id name) }
        | _ ->
          Env.expect env T_COLON;
          let value = parse_pattern ~is_decl env in
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
  Record { loc; name; fields; rest = !rest }

and parse_match ~is_expr env =
  let open Match in
  let marker = mark_loc env in
  Env.expect env T_MATCH;
  (* Parse arguments *)
  Env.expect env T_LEFT_PAREN;
  let rec parse_args () =
    let arg = parse_expression env in
    match Env.token env with
    | T_RIGHT_PAREN -> [arg]
    | T_COMMA ->
      Env.advance env;
      (* Optionally accept trailing comma *)
      (match Env.token env with
      | T_RIGHT_PAREN -> [arg]
      | _ -> arg :: parse_args ())
    | _ ->
      Env.expect env T_RIGHT_PAREN;
      []
  in
  let args = parse_args () in
  Env.expect env T_RIGHT_PAREN;
  (* Parse match cases *)
  Env.expect env T_LEFT_BRACE;
  Env.enter_match env;
  (* Pipe for first variant is optional *)
  (match Env.token env with
  | T_PIPE -> Env.advance env
  | _ -> ());
  let rec parse_cases () =
    let marker = mark_loc env in
    let pattern = parse_pattern ~is_decl:false env in
    let guard =
      match Env.token env with
      | T_WHEN ->
        Env.advance env;
        Some (parse_expression env)
      | _ -> None
    in
    Env.expect env T_ARROW;
    let right =
      match Env.token env with
      (* Only certain statements are allowed as right hand side *)
      | T_LEFT_BRACE -> Case.Statement (Statement.Block (parse_block env))
      | T_WHILE -> Case.Statement (parse_while env)
      | T_FOR -> Case.Statement (parse_for env)
      | T_RETURN -> Case.Statement (parse_return ~in_match_case:true env)
      | T_BREAK -> Case.Statement (parse_break ~in_match_case:true env)
      | T_CONTINUE -> Case.Statement (parse_continue ~in_match_case:true env)
      | T_IF ->
        if is_expr then
          Case.Expression (If (parse_if env))
        else
          Case.Statement (If (parse_if env))
      | T_MATCH ->
        let match_ = parse_match ~is_expr env in
        if is_expr then
          Case.Expression (Match match_)
        else
          Case.Statement (Match match_)
      | _ -> Case.Expression (parse_expression env)
    in
    let loc = marker env in
    let case = { Case.loc; pattern; guard; right } in
    match Env.token env with
    | T_PIPE ->
      Env.advance env;
      case :: parse_cases ()
    | T_RIGHT_BRACE -> [case]
    | _ ->
      Env.expect env T_RIGHT_BRACE;
      []
  in
  let cases = parse_cases () in
  Env.expect env T_RIGHT_BRACE;
  Env.exit_match env;
  let loc = marker env in
  { loc; args; cases }

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
  let open If in
  let marker = mark_loc env in
  Env.expect env T_IF;
  Env.expect env T_LEFT_PAREN;
  let test = parse_test env in
  Env.expect env T_RIGHT_PAREN;
  let conseq = parse_block env in
  let altern =
    match Env.token env with
    | T_ELSE ->
      Env.advance env;
      (match Env.token env with
      | T_IF -> If (parse_if env)
      | _ -> Block (parse_block env))
    | _ -> None
  in
  let loc = marker env in
  { loc; test; conseq; altern }

and parse_test env =
  let open Test in
  let marker = mark_loc env in
  let expr = parse_expression env in
  if Env.token env == T_MATCH then (
    Env.advance env;
    let pattern = parse_pattern ~is_decl:false env in
    let guard =
      if Env.token env == T_WHEN then (
        Env.advance env;
        Some (parse_expression env)
      ) else
        None
    in

    let loc = marker env in
    Match { loc; expr; pattern; guard }
  ) else
    Expression expr

and parse_while env =
  let marker = mark_loc env in
  Env.expect env T_WHILE;
  Env.expect env T_LEFT_PAREN;
  let test = parse_test env in
  Env.expect env T_RIGHT_PAREN;
  let body = parse_block env in
  let loc = marker env in
  Statement.While { loc; test; body }

and parse_for env =
  let marker = mark_loc env in
  Env.expect env T_FOR;
  Env.expect env T_LEFT_PAREN;
  let pattern = parse_pattern ~is_decl:true env in
  let annot =
    match Env.token env with
    | T_COLON ->
      Env.advance env;
      Some (parse_type env)
    | _ -> None
  in
  Env.expect env T_IN;
  let iterator = parse_expression env in
  Env.expect env T_RIGHT_PAREN;
  let body = parse_block env in
  let loc = marker env in
  Statement.For { loc; pattern; annot; iterator; body }

and parse_return ~in_match_case env =
  let open Statement.Return in
  let marker = mark_loc env in
  Env.expect env T_RETURN;
  let arg =
    match Env.token env with
    | T_SEMICOLON -> None
    (* Handle the following cases:
       match (x) {
         | a -> return
       }

       match (x) {
         | a -> return
         | b -> c
       } *)
    | T_PIPE
    | T_RIGHT_BRACE
      when in_match_case ->
      None
    | _ -> Some (parse_expression env)
  in
  if not in_match_case then Env.expect env T_SEMICOLON;
  let loc = marker env in
  Statement.Return { loc; arg }

and parse_break ~in_match_case env =
  let marker = mark_loc env in
  Env.expect env T_BREAK;
  if not in_match_case then Env.expect env T_SEMICOLON;
  let loc = marker env in
  Statement.Break { loc }

and parse_continue ~in_match_case env =
  let marker = mark_loc env in
  Env.expect env T_CONTINUE;
  if not in_match_case then Env.expect env T_SEMICOLON;
  let loc = marker env in
  Statement.Continue { loc }

and parse_type_declaration ~attributes ~is_public env marker =
  let open TypeDeclaration in
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
    { loc; name; type_params; decl = Alias alias; attributes; is_public }
  | _ ->
    let name_marker = mark_loc env in
    let name = parse_identifier env in
    let type_params = parse_type_params_opt () in
    (match Env.token env with
    | T_LEFT_PAREN ->
      let tuple = parse_tuple_variant env name name_marker in
      let loc = marker env in
      { loc; name; type_params; decl = Tuple tuple; attributes; is_public }
    | T_LEFT_BRACE ->
      let record = parse_record_variant env ~is_variant:false name name_marker in
      let loc = marker env in
      { loc; name; type_params; decl = Record record; attributes; is_public }
    | T_EQUALS ->
      Env.advance env;
      parse_variant env ~attributes ~is_public name type_params marker
    (* Some types, such as builtins, do not have a decl *)
    | _ ->
      let loc = marker env in
      { loc; name; type_params; decl = None; attributes; is_public })

and parse_variant env ~attributes ~is_public name type_params marker =
  (match Env.token env with
  | T_PIPE -> Env.advance env
  | _ -> ());
  let rec parse_variants () =
    let open TypeDeclaration in
    let marker = mark_loc env in
    let name = parse_identifier env in
    let variant =
      match Env.token env with
      | T_LEFT_BRACE -> RecordVariant (parse_record_variant env ~is_variant:true name marker)
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
  { loc; name; type_params; decl = Variant variants; attributes; is_public }

and parse_record_variant env ~is_variant name marker =
  let open TypeDeclaration.Record in
  Env.expect env T_LEFT_BRACE;
  let rec parse_fields () =
    match Env.token env with
    | T_RIGHT_BRACE ->
      Env.advance env;
      []
    | _ ->
      let marker = mark_loc env in
      (* Non-variant types can mark public fields by prefixing field with pub *)
      let is_public =
        if not is_variant then
          maybe_parse_public env
        else
          false
      in

      (* Non-variant types can mark mutable fields by prefixing field with var *)
      let is_mutable =
        if (not is_variant) && Env.token env == T_VAR then (
          Env.advance env;
          true
        ) else
          false
      in

      let name = parse_identifier env in
      Env.expect env T_COLON;
      let field =
        let ty = parse_type env in
        let loc = marker env in
        (match Env.token env with
        | T_RIGHT_BRACE -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_BRACE);
        { Field.loc; name; ty; is_public; is_mutable }
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

and parse_variable_declaration ~is_toplevel ~attributes ~is_public env marker =
  let open Statement in
  let kind =
    match Env.token env with
    | T_VAL -> VariableDeclaration.Immutable
    | T_VAR -> VariableDeclaration.Mutable
    | _ -> failwith "Must be called on variable declaration"
  in
  Env.advance env;
  let pattern = parse_pattern ~is_decl:true env in
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
  { VariableDeclaration.loc; kind; pattern; init; annot; attributes; is_public }

and parse_function ~attributes ~is_public ~is_static ~is_override marker env =
  let open Function in
  Env.expect env T_FUN;
  let name = parse_identifier env in
  let type_params =
    if Env.token env = T_LESS_THAN then
      parse_type_params env
    else
      []
  in
  let (params, return) = parse_function_signature env in
  let body =
    match Env.token env with
    | T_LEFT_BRACE -> Block (parse_block env)
    | T_EQUALS ->
      Env.advance env;
      Expression (parse_expression env)
    | _ -> Signature
  in
  let loc = marker env in
  { loc; name; params; body; return; type_params; attributes; is_public; is_static; is_override }

and parse_function_signature env =
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
      let param = { Function.Param.loc; name; annot } in
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
  (params, return)

and parse_anonymous_function_signature env =
  Env.expect env T_LEFT_PAREN;
  let rec params env =
    match Env.token env with
    | T_RIGHT_PAREN ->
      Env.advance env;
      []
    | _ ->
      let marker = mark_loc env in
      let name = parse_identifier env in
      let annot =
        match Env.token env with
        | T_COLON ->
          Env.advance env;
          Some (parse_type env)
        | _ -> None
      in
      let loc = marker env in
      begin
        match Env.token env with
        | T_RIGHT_PAREN -> ()
        | T_COMMA -> Env.advance env
        | _ -> Env.expect env T_RIGHT_PAREN
      end;
      let param = { Expression.AnonymousFunction.Param.loc; name; annot } in
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
  (params, return)

and parse_trait_declaration ~kind ~attributes ~is_public env marker =
  (match kind with
  | Methods -> Env.expect env T_METHODS
  | Trait -> Env.expect env T_TRAIT);
  let name = parse_identifier env in
  let type_params =
    if Env.token env = T_LESS_THAN then
      parse_type_params env
    else
      []
  in
  Env.expect env T_LEFT_BRACE;
  let rec parse_implemented acc =
    match (Env.token env, kind) with
    | (T_EXTENDS, Trait)
    | (T_IMPLEMENTS, Methods) ->
      let marker = mark_loc env in
      Env.advance env;
      let name = parse_scoped_identifier env in
      let type_args = parse_type_args env in
      let loc = marker env in
      let trait = { Type.Identifier.loc; name; type_args } in
      parse_implemented (trait :: acc)
    | _ -> acc
  in
  let implemented = parse_implemented [] |> List.rev in
  let rec parse_methods acc =
    let marker = mark_loc env in
    let attributes = parse_attribute_annotations env in
    let is_public = maybe_parse_public env in
    let is_override =
      match Env.token env with
      | T_OVERRIDE ->
        Env.advance env;
        true
      | _ -> false
    in
    let is_static =
      match Env.token env with
      | T_STATIC ->
        Env.advance env;
        true
      | _ -> false
    in
    match Env.token env with
    | T_FUN ->
      let func = parse_function ~attributes ~is_public ~is_static ~is_override marker env in
      parse_methods (func :: acc)
    | T_RIGHT_BRACE -> acc
    | token -> Parse_error.fatal (Env.loc env, MalformedMethodsItem token)
  in
  let methods = parse_methods [] |> List.rev in
  Env.expect env T_RIGHT_BRACE;
  let loc = marker env in
  { loc; kind; name; type_params; implemented; methods; attributes; is_public }

and parse_type env =
  match Env.token env with
  | T_LEFT_PAREN -> parse_parenthesized_type env
  | T_IDENTIFIER _ -> Identifier (parse_identifier_type env)
  | T_TRAIT -> parse_trait_type env
  | token -> Parse_error.fatal (Env.loc env, MalformedType token)

and parse_parenthesized_type env =
  let open Type in
  let marker = mark_loc env in
  Env.expect env T_LEFT_PAREN;
  let (tys, trailing_comma_loc) = parse_parenthesized_type_or_params env in
  (* A parenthesized list followed by an arrow is a function type *)
  if Env.token env = T_ARROW then
    parse_function_type env tys marker
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

and parse_type_args env =
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

and parse_identifier_type env =
  let open Type.Identifier in
  let marker = mark_loc env in
  let name = parse_scoped_identifier env in
  let type_args = parse_type_args env in
  let loc = marker env in
  { loc; name; type_args }

and parse_function_type env params marker =
  let open Ast.Type in
  Env.expect env T_ARROW;
  let return = parse_type env in
  let loc = marker env in
  Function { Function.loc; params; return }

and parse_trait_type env =
  let open Ast.Type in
  let marker = mark_loc env in
  Env.expect env T_TRAIT;
  let trait = parse_identifier_type env in
  let loc = marker env in
  Trait { loc; trait }

and parse_type_param_bounds env =
  let bound = parse_identifier_type env in
  let rec parse_bounds env acc =
    match Env.token env with
    | T_AMPERSAND ->
      Env.advance env;
      (match Env.token env with
      | T_COMMA
      | T_GREATER_THAN ->
        acc
      | _ ->
        let bound = parse_identifier_type env in
        parse_bounds env (bound :: acc))
    | _ -> acc
  in
  let bounds = parse_bounds env [bound] in
  List.rev bounds

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
        let bounds =
          match Env.token env with
          | T_COLON ->
            Env.advance env;
            parse_type_param_bounds env
          | _ -> []
        in
        let loc = marker env in
        begin
          match Env.token env with
          | T_GREATER_THAN -> ()
          | T_COMMA -> Env.advance env
          | _ -> Env.expect env T_GREATER_THAN
        end;
        { TypeParameter.loc; name; bounds } :: type_params env
    in
    type_params env

and reparse_expression_as_lvalue expr =
  let open Expression in
  match expr with
  | IndexedAccess _
  | NamedAccess _ ->
    assert_expression_is_lvalue_expression expr;
    Statement.Assignment.Expression expr
  | _ -> Statement.Assignment.Pattern (reparse_expression_as_lvalue_pattern expr)

and reparse_expression_as_assignment_op_lvalue expr =
  let open Expression in
  let error () =
    Parse_error.fatal (Ast_utils.expression_loc expr, Parse_error.InvalidAssignmentPattern)
  in
  match expr with
  | Identifier { name = "_"; _ } -> error ()
  | Identifier _
  | IndexedAccess _
  | NamedAccess _ ->
    reparse_expression_as_lvalue expr
  | _ -> error ()

and assert_expression_is_lvalue_expression expr =
  let open Expression in
  match expr with
  | Identifier _ -> ()
  | NamedAccess { target; _ }
  | IndexedAccess { target; _ } ->
    assert_expression_is_lvalue_expression target
  | _ -> Parse_error.fatal (Ast_utils.expression_loc expr, Parse_error.InvalidAssignmentPattern)

and reparse_expression_as_lvalue_pattern expr =
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
  | Identifier { loc; name = "_" } -> Pattern.Wildcard loc
  | Identifier { loc; name } -> Pattern.Identifier { loc; name = { loc; name }; scopes = [] }
  | BinaryOperation { loc; op = BitwiseOr; left; right } ->
    let left = reparse_expression_as_lvalue_pattern left in
    let right = reparse_expression_as_lvalue_pattern right in
    Pattern.Or { loc; left; right }
  | Tuple { loc; elements } ->
    Pattern.Tuple
      { loc; name = None; elements = List.map reparse_expression_as_lvalue_pattern elements }
  | Call { loc; func; args } ->
    let name = reparse_name_parts func in
    let elements = List.map reparse_expression_as_lvalue_pattern args in
    Pattern.Tuple { loc; name = Some name; elements }
  | Record { loc; name; fields; rest } ->
    let name = reparse_name_parts name in
    let fields =
      List.map
        (fun { Record.Field.loc; name; value } ->
          match value with
          | None ->
            {
              Pattern.Record.Field.loc;
              name = None;
              value = Pattern.Identifier (id_to_scoped_id name);
            }
          | Some value ->
            let value = reparse_expression_as_lvalue_pattern value in
            { Pattern.Record.Field.loc; name = Some name; value })
        fields
    in
    Pattern.Record { loc; name; fields; rest = rest <> None }
  | _ -> invalid_pattern_error expr

and id_to_scoped_id id =
  { Ast.ScopedIdentifier.loc = id.Ast.Identifier.loc; name = id; scopes = [] }
