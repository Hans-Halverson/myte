open Ast

type pp_node =
  | None
  | Skip
  | Int of Int32.t
  | String of string
  | Bool of bool
  | Raw of string
  | List of pp_node list
  | Map of (string * pp_node) list

let pp node =
  let buf = Buffer.create 16 in
  let add_string str = Buffer.add_string buf str in
  let add_strings strs = List.iter add_string strs in
  let add_indent indent = add_string (String.make (2 * indent) ' ') in
  let rec pp_node node indent =
    match node with
    | None -> add_string "None"
    | Skip -> ()
    | Int int -> add_string (Int32.to_string int)
    | String str -> add_strings ["\""; str; "\""]
    | Bool bool ->
      add_string
        (if bool then
          "true"
        else
          "false")
    | Raw raw -> add_string raw
    | List [] -> add_string "[]"
    | List nodes ->
      add_string "[\n";
      List.iter
        (fun node ->
          match node with
          | Skip -> ()
          | _ ->
            add_indent (indent + 1);
            pp_node node (indent + 1);
            add_string ",\n")
        nodes;
      add_indent indent;
      add_string "]"
    | Map [] -> add_string "{}"
    | Map attrs ->
      add_string "{\n";
      List.iter
        (fun (name, node) ->
          match node with
          | Skip -> ()
          | _ ->
            add_indent (indent + 1);
            add_strings [name; ": "];
            pp_node node (indent + 1);
            add_string ",\n")
        attrs;
      add_indent indent;
      add_string "}"
  in
  pp_node node 0;
  add_string "\n";
  Buffer.contents buf

let string_of_unary_op op =
  let open Expression.UnaryOperation in
  match op with
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Not -> "Not"

let string_of_binary_op op =
  let open Expression.BinaryOperation in
  match op with
  | Add -> "Add"
  | Subtract -> "Subtract"
  | Multiply -> "Multiply"
  | Divide -> "Divide"
  | Remainder -> "Remainder"
  | Equal -> "Equal"
  | NotEqual -> "NotEqual"
  | LessThan -> "LessThan"
  | GreaterThan -> "GreaterThan"
  | LessThanOrEqual -> "LessThanOrEqual"
  | GreaterThanOrEqual -> "GreaterThanOrEqual"
  | BitwiseAnd -> "BitwiseAnd"
  | BitwiseOr -> "BitwiseOr"
  | BitwiseXor -> "BitwiseXor"
  | LeftShift -> "LeftShift"
  | ArithmeticRightShift -> "ArithmeticRightShift"
  | LogicalRightShift -> "LogicalRightShift"

let string_of_assignment_op op =
  let open Statement.Assignment in
  match op with
  | Add -> "Add"
  | Subtract -> "Subtract"
  | Multiply -> "Multiply"
  | Divide -> "Divide"
  | Remainder -> "Remainder"
  | BitwiseAnd -> "BitwiseAnd"
  | BitwiseOr -> "BitwiseOr"
  | BitwiseXor -> "BitwiseXor"
  | LeftShift -> "LeftShift"
  | ArithmeticRightShift -> "ArithmeticRightShift"
  | LogicalRightShift -> "LogicalRightShift"

let rec node_of_loc loc =
  let pp_pos pos =
    let { Loc.line; col } = pos in
    Printf.sprintf "%d:%d" line col
  in
  let { Loc.start; _end; _ } = loc in
  let raw_loc = Printf.sprintf "%s-%s" (pp_pos start) (pp_pos _end) in
  Raw raw_loc

and node name loc attributes = Map (("node", Raw name) :: ("loc", node_of_loc loc) :: attributes)

and opt : 'a. ('a -> 'b) -> 'a option -> 'b =
 fun f x ->
  match x with
  | Option.None -> None
  | Some x -> f x

and node_of_module mod_ =
  let { Module.loc; name; imports; toplevels } = mod_ in
  node
    "Module"
    loc
    [
      ("name", node_of_module_name name);
      ("imports", List (List.map node_of_import imports));
      ("toplevels", List (List.map node_of_toplevel toplevels));
    ]

and node_of_module_name module_ =
  let { Module.Name.loc; name } = module_ in
  node "ModuleName" loc [("name", node_of_scoped_identifier name)]

and node_of_toplevel toplevel =
  let open Module in
  match toplevel with
  | VariableDeclaration decl -> node_of_variable_decl decl
  | FunctionDeclaration decl -> node_of_function decl
  | TypeDeclaration decl -> node_of_type_decl decl
  | TraitDeclaration decl -> node_of_trait_decl decl

and node_of_statement stmt =
  let open Statement in
  match stmt with
  | VariableDeclaration decl -> node_of_variable_decl decl
  | FunctionDeclaration decl -> node_of_function decl
  | ExpressionStatement expr -> node_of_expression_stmt expr
  | Block block -> node_of_block block
  | If if_ -> node_of_if ~is_expr:false if_
  | While while_ -> node_of_while while_
  | For for_ -> node_of_for for_
  | Return ret -> node_of_return ret
  | Break break -> node_of_break break
  | Continue continue -> node_of_continue continue
  | Assignment assign -> node_of_assignment assign
  | Match match_ -> node_of_match ~is_expr:false match_

and node_of_expression expr =
  let open Expression in
  match expr with
  | Unit unit -> node_of_unit unit
  | Identifier id -> node_of_identifier id
  | IntLiteral lit -> node_of_int_literal lit
  | FloatLiteral lit -> node_of_float_literal lit
  | CharLiteral lit -> node_of_char_literal lit
  | StringLiteral lit -> node_of_string_literal lit
  | BoolLiteral lit -> node_of_bool_literal lit
  | InterpolatedString str -> node_of_interpolated_string str
  | Record record -> node_of_record_expression record
  | Tuple tuple -> node_of_tuple_expression tuple
  | TypeCast cast -> node_of_type_cast cast
  | UnaryOperation unary -> node_of_unary_operation unary
  | BinaryOperation binary -> node_of_binary_operation binary
  | LogicalAnd logical -> node_of_logical_and logical
  | LogicalOr logical -> node_of_logical_or logical
  | If if_ -> node_of_if ~is_expr:true if_
  | Call call -> node_of_call call
  | IndexedAccess access -> node_of_indexed_access access
  | NamedAccess access -> node_of_named_access access
  | Match match_ -> node_of_match ~is_expr:true match_
  | VecLiteral lit -> node_of_vec_literal lit
  | MapLiteral lit -> node_of_map_literal lit
  | SetLiteral lit -> node_of_set_literal lit
  | AnonymousFunction func -> node_of_anonymous_function func
  | Unwrap unwrap -> node_of_unwrap unwrap

and node_of_pattern pat =
  let open Pattern in
  match pat with
  | Wildcard loc -> node_of_wildcard_pattern loc
  | Identifier id -> node_of_scoped_identifier id
  | NamedWildcard named -> node_of_named_wildcard_pattern named
  | Binding b -> node_of_binding_pattern b
  | Or o -> node_of_or_pattern o
  | Tuple t -> node_of_tuple_pattern t
  | Record r -> node_of_record_pattern r
  | Literal l -> node_of_literal_pattern l

and node_of_type ty =
  let open Type in
  match ty with
  | Identifier id -> node_of_identifier_type id
  | Tuple tuple -> node_of_tuple_type tuple
  | Function func -> node_of_function_type func
  | Trait trait -> node_of_trait_type trait

and node_of_expression_stmt stmt =
  let { Statement.ExpressionStatement.loc; expr; is_value } = stmt in
  let nodes = [("expression", node_of_expression expr)] in
  let nodes =
    if is_value then
      nodes @ [("is_value", Bool true)]
    else
      nodes
  in
  node "ExpressionStatement" loc nodes

and node_of_identifier id =
  let { Identifier.loc; name } = id in
  node "Identifier" loc [("name", String name)]

and node_of_scoped_identifier id =
  let { ScopedIdentifier.loc; name; scopes } = id in
  node
    "ScopedIdentifier"
    loc
    [("scopes", List (List.map node_of_identifier scopes)); ("name", node_of_identifier name)]

and node_of_attributes attributes =
  if attributes == [] then
    Skip
  else
    List (List.map node_of_attribute attributes)

and node_of_attribute attribute =
  let { Attribute.loc; name; params } = attribute in
  node
    "Attribute"
    loc
    [("name", node_of_identifier name); ("params", List (List.map node_of_attribute_param params))]

and node_of_attribute_param param =
  let open Attribute.Param in
  match param with
  | Attribute attribute -> node_of_attribute attribute
  | Pair pair -> node_of_attribute_param_pair pair

and node_of_attribute_param_pair pair =
  let { Attribute.Pair.loc; key; value } = pair in
  node "Pair" loc [("key", node_of_identifier key); ("value", node_of_attribute_literal value)]

and node_of_attribute_literal lit =
  let open Attribute.Literal in
  match lit with
  | Bool lit -> node_of_bool_literal lit
  | Int lit -> node_of_int_literal lit
  | String lit -> node_of_string_literal lit

and node_of_import import =
  let open Module.Import in
  match import with
  | Simple i -> node_of_scoped_identifier i
  | Complex i -> node_of_complex_import i

and node_of_complex_import import =
  let { Module.Import.Complex.loc; scopes; aliases } = import in
  node
    "ComplexImport"
    loc
    [
      ("scopes", List (List.map node_of_identifier scopes));
      ("aliases", List (List.map node_of_import_alias aliases));
    ]

and node_of_import_alias alias =
  let { Module.Import.Alias.loc; name; alias } = alias in
  node
    "ImportAlias"
    loc
    [("name", node_of_identifier name); ("alias", opt node_of_identifier alias)]

and node_of_unit unit =
  let { Expression.Unit.loc } = unit in
  node "Unit" loc []

and node_of_int_literal lit =
  let { Expression.IntLiteral.loc; raw; base } = lit in
  let base_node =
    match base with
    | Dec -> []
    | Bin -> [("base", String "Bin")]
    | Hex -> [("base", String "Hex")]
  in
  node "IntLiteral" loc ([("raw", String raw)] @ base_node)

and node_of_float_literal lit =
  let { Expression.FloatLiteral.loc; raw } = lit in
  node "FloatLiteral" loc [("raw", String raw)]

and node_of_char_literal lit =
  let { Expression.CharLiteral.loc; value } = lit in
  node "CharLiteral" loc [("value", String (Integers.char_to_string value))]

and node_of_string_literal lit =
  let { Expression.StringLiteral.loc; value } = lit in
  node "StringLiteral" loc [("value", String value)]

and node_of_bool_literal lit =
  let { Expression.BoolLiteral.loc; value } = lit in
  node "BoolLiteral" loc [("value", Bool value)]

and node_of_interpolated_string str =
  let open Expression.InterpolatedString in
  let { loc; parts } = str in
  let parts =
    List.map
      (fun part ->
        match part with
        | String lit -> node_of_string_literal lit
        | Expression expr -> node_of_expression expr)
      parts
  in
  node "InterpolatedString" loc [("parts", List parts)]

and node_of_record_expression record =
  let { Expression.Record.loc; name; fields; rest = _ } = record in
  node
    "RecordExpression"
    loc
    [
      ("name", node_of_expression name);
      ("fields", List (List.map node_of_record_expression_field fields));
    ]

and node_of_record_expression_field field =
  let { Expression.Record.Field.loc; name; value } = field in
  node
    "RecordExpressionField"
    loc
    [("name", node_of_identifier name); ("value", opt node_of_expression value)]

and node_of_tuple_expression tuple =
  let { Expression.Tuple.loc; elements } = tuple in
  node "TupleExpression" loc [("elements", List (List.map node_of_expression elements))]

and node_of_type_cast cast =
  let { Expression.TypeCast.loc; expr; ty } = cast in
  node "TypeCast" loc [("expr", node_of_expression expr); ("type", node_of_type ty)]

and node_of_unary_operation unary =
  let { Expression.UnaryOperation.loc; operand; op } = unary in
  node
    "UnaryOperation"
    loc
    [("op", Raw (string_of_unary_op op)); ("operand", node_of_expression operand)]

and node_of_binary_operation binary =
  let { Expression.BinaryOperation.loc; left; right; op } = binary in
  node
    "BinaryOperation"
    loc
    [
      ("op", Raw (string_of_binary_op op));
      ("left", node_of_expression left);
      ("right", node_of_expression right);
    ]

and node_of_logical_and logical =
  let { Expression.LogicalAnd.loc; left; right } = logical in
  node "LogicalAnd" loc [("left", node_of_expression left); ("right", node_of_expression right)]

and node_of_logical_or logical =
  let { Expression.LogicalOr.loc; left; right } = logical in
  node "LogicalOr" loc [("left", node_of_expression left); ("right", node_of_expression right)]

and node_of_call call =
  let { Expression.Call.loc; func; args } = call in
  node
    "Call"
    loc
    [("func", node_of_expression func); ("args", List (List.map node_of_expression args))]

and node_of_indexed_access access =
  let { Expression.IndexedAccess.loc; target; index } = access in
  node
    "IndexedAccess"
    loc
    [("target", node_of_expression target); ("index", node_of_expression index)]

and node_of_named_access access =
  let { Expression.NamedAccess.loc; target; name } = access in
  node "NamedAccess" loc [("target", node_of_expression target); ("name", node_of_identifier name)]

and node_of_vec_literal lit =
  let { Expression.VecLiteral.loc; elements } = lit in
  node "VecLiteral" loc [("elements", List (List.map node_of_expression elements))]

and node_of_map_literal lit =
  let { Expression.MapLiteral.loc; entries } = lit in
  node "MapLiteral" loc [("entries", List (List.map node_of_map_literal_entry entries))]

and node_of_map_literal_entry entry =
  let { Expression.MapLiteral.Entry.loc; key; value } = entry in
  node "MapLiteralEntry" loc [("key", node_of_expression key); ("value", node_of_expression value)]

and node_of_set_literal lit =
  let { Expression.SetLiteral.loc; elements } = lit in
  node "SetLiteral" loc [("elements", List (List.map node_of_expression elements))]

and node_of_anonymous_function func =
  let open Expression.AnonymousFunction in
  let { loc; params; body; return } = func in
  let body =
    match body with
    | Block block -> node_of_block block
    | Expression expr -> node_of_expression expr
  in
  node
    "AnonymousFunction"
    loc
    [
      ("params", List (List.map node_of_anonymous_function_param params));
      ("return", opt node_of_type return);
      ("body", body);
    ]

and node_of_anonymous_function_param param =
  let open Expression.AnonymousFunction.Param in
  let { loc; name; annot } = param in
  node "Param" loc [("name", node_of_identifier name); ("annot", opt node_of_type annot)]

and node_of_unwrap unwrap =
  let { Expression.Unwrap.loc; operand } = unwrap in
  node "Unwrap" loc [("operand", node_of_expression operand)]

and node_of_wildcard_pattern loc = node "Wildcard" loc []

and node_of_named_wildcard_pattern named =
  let { Pattern.NamedWildcard.loc; name } = named in
  node "NamedWildcard" loc [("name", node_of_scoped_identifier name)]

and node_of_binding_pattern binding =
  let { Pattern.Binding.loc; pattern; name } = binding in
  node "Binding" loc [("pattern", node_of_pattern pattern); ("name", node_of_identifier name)]

and node_of_or_pattern or_ =
  let { Pattern.Or.loc; left; right } = or_ in
  node "Or" loc [("left", node_of_pattern left); ("right", node_of_pattern right)]

and node_of_tuple_pattern tuple =
  let { Pattern.Tuple.loc; name; elements } = tuple in
  node
    "TuplePattern"
    loc
    [
      ("name", opt node_of_scoped_identifier name);
      ("elements", List (List.map node_of_pattern elements));
    ]

and node_of_record_pattern record =
  let { Pattern.Record.loc; name; fields; rest } = record in
  node
    "RecordPattern"
    loc
    [
      ("name", node_of_scoped_identifier name);
      ("fields", List (List.map node_of_record_pattern_field fields));
      ("rest", Bool rest);
    ]

and node_of_record_pattern_field field =
  let { Pattern.Record.Field.loc; name; value } = field in
  node
    "RecordPatternField"
    loc
    [("name", opt node_of_identifier name); ("value", node_of_pattern value)]

and node_of_literal_pattern lit =
  let open Pattern.Literal in
  match lit with
  | Unit unit -> node_of_unit unit
  | Bool lit -> node_of_bool_literal lit
  | Int lit -> node_of_int_literal lit
  | Char lit -> node_of_char_literal lit
  | String lit -> node_of_string_literal lit

and node_of_block block =
  let { Statement.Block.loc; statements } = block in
  node "Block" loc [("statements", List (List.map node_of_statement statements))]

and node_of_while while_ =
  let { Statement.While.loc; test; body } = while_ in
  node "While" loc [("test", node_of_test test); ("body", node_of_block body)]

and node_of_for for_ =
  let { Statement.For.loc; pattern; annot; iterator; body } = for_ in
  node
    "For"
    loc
    [
      ("pattern", node_of_pattern pattern);
      ("annot", opt node_of_type annot);
      ("iterator", node_of_expression iterator);
      ("body", node_of_block body);
    ]

and node_of_return ret =
  let { Statement.Return.loc; arg } = ret in
  node "Return" loc [("arg", opt node_of_expression arg)]

and node_of_break break =
  let { Statement.Break.loc } = break in
  node "Break" loc []

and node_of_continue continue =
  let { Statement.Continue.loc } = continue in
  node "Continue" loc []

and node_of_assignment assign =
  let { Statement.Assignment.loc; op; lvalue; expr } = assign in
  let op =
    match op with
    | None -> None
    | Some op -> String (string_of_assignment_op op)
  in
  node
    "Assignment"
    loc
    [
      ("op", op);
      ( "lvalue",
        match lvalue with
        | Pattern pattern -> node_of_pattern pattern
        | Expression expr -> node_of_expression expr );
      ("expr", node_of_expression expr);
    ]

and node_of_if ~is_expr if_ =
  let { If.loc; test; conseq; altern } = if_ in
  let name =
    if is_expr then
      "IfExpression"
    else
      "IfStatement"
  in
  let altern =
    match altern with
    | Block block -> node_of_block block
    | If if_ -> node_of_if ~is_expr if_
    | None -> None
  in
  node name loc [("test", node_of_test test); ("conseq", node_of_block conseq); ("altern", altern)]

and node_of_test test =
  let open Test in
  match test with
  | Expression expr -> node_of_expression expr
  | Match match_ -> node_of_match_test match_

and node_of_match_test match_ =
  let { Test.Match.loc; expr; pattern; guard } = match_ in
  node
    "MatchTest"
    loc
    [
      ("expr", node_of_expression expr);
      ("pattern", node_of_pattern pattern);
      ("guard", opt node_of_expression guard);
    ]

and node_of_match match_ ~is_expr =
  let { Match.loc; args; cases } = match_ in
  let name =
    if is_expr then
      "MatchExpression"
    else
      "MatchStatement"
  in
  node
    name
    loc
    [
      ("args", List (List.map node_of_expression args));
      ("cases", List (List.map node_of_match_case cases));
    ]

and node_of_match_case case =
  let open Match.Case in
  let { loc; pattern; guard; right } = case in
  node
    "MatchCase"
    loc
    [
      ("pattern", node_of_pattern pattern);
      ("guard", opt node_of_expression guard);
      ( "right",
        match right with
        | Expression expr -> node_of_expression expr
        | Statement stmt -> node_of_statement stmt );
    ]

and node_of_variable_decl decl =
  let { Statement.VariableDeclaration.loc; kind; pattern; init; annot; attributes; is_public } =
    decl
  in
  let kind =
    match kind with
    | Immutable -> "Immutable"
    | Mutable -> "Mutable"
  in
  node
    "VariableDeclaration"
    loc
    [
      ("kind", Raw kind);
      ("pattern", node_of_pattern pattern);
      ("init", node_of_expression init);
      ("annot", opt node_of_type annot);
      ("attributes", node_of_attributes attributes);
      ("is_public", Bool is_public);
    ]

and node_of_trait_decl decl =
  let open TraitDeclaration in
  let { loc; kind; name; type_params; implemented; methods; attributes; is_public } = decl in
  let kind =
    match kind with
    | Methods -> "Methods"
    | Trait -> "Trait"
  in
  node
    "TraitDeclaration"
    loc
    [
      ("kind", Raw kind);
      ("name", node_of_identifier name);
      ("type_params", List (List.map node_of_type_parameter type_params));
      ("implemented", List (List.map node_of_identifier_type implemented));
      ("methods", List (List.map node_of_function methods));
      ("attributes", node_of_attributes attributes);
      ("is_public", Bool is_public);
    ]

and node_of_type_decl decl =
  let { TypeDeclaration.loc; name; type_params; decl; attributes; is_public } = decl in
  let decl =
    match decl with
    | None -> ("skip", Skip)
    | Alias alias -> ("alias", node_of_type alias)
    | Record record -> ("record", node_of_record_variant record)
    | Tuple tuple -> ("tuple", node_of_tuple_variant tuple)
    | Variant variants ->
      ( "variants",
        List
          (List.map
             (fun variant ->
               match variant with
               | TypeDeclaration.RecordVariant record -> node_of_record_variant record
               | TupleVariant tuple -> node_of_tuple_variant tuple
               | EnumVariant id -> node_of_identifier id)
             variants) )
  in
  node
    "TypeDeclaration"
    loc
    [
      ("name", node_of_identifier name);
      ("type_params", List (List.map node_of_type_parameter type_params));
      decl;
      ("attributes", node_of_attributes attributes);
      ("is_public", Bool is_public);
    ]

and node_of_record_variant record =
  let open TypeDeclaration.Record in
  let { loc; name; fields } = record in
  node
    "RecordVariant"
    loc
    [
      ("name", node_of_identifier name);
      ("fields", List (List.map node_of_record_variant_field fields));
    ]

and node_of_record_variant_field field =
  let open TypeDeclaration.Record.Field in
  let { loc; name; ty; is_public; is_mutable } = field in
  node
    "RecordVariantField"
    loc
    [
      ("name", node_of_identifier name);
      ("type", node_of_type ty);
      ("is_public", Bool is_public);
      ("is_mutable", Bool is_mutable);
    ]

and node_of_tuple_variant tuple =
  let open TypeDeclaration.Tuple in
  let { loc; name; elements } = tuple in
  node
    "TupleVariant"
    loc
    [("name", node_of_identifier name); ("elements", List (List.map node_of_type elements))]

and node_of_function func =
  let open Function in
  let {
    loc;
    name;
    type_params;
    params;
    body;
    return;
    attributes;
    is_public;
    is_static;
    is_override;
  } =
    func
  in
  let body =
    match body with
    | Block block -> node_of_block block
    | Expression expr -> node_of_expression expr
    | Signature -> None
  in
  node
    "Function"
    loc
    [
      ("name", node_of_identifier name);
      ("params", List (List.map node_of_function_param params));
      ("body", body);
      ("return", opt node_of_type return);
      ("type_params", List (List.map node_of_type_parameter type_params));
      ("attributes", node_of_attributes attributes);
      ("is_public", Bool is_public);
      ("is_static", Bool is_static);
      ("is_override", Bool is_override);
    ]

and node_of_function_param param =
  let open Function.Param in
  let { loc; name; annot } = param in
  node "Param" loc [("name", node_of_identifier name); ("annot", node_of_type annot)]

and node_of_type_parameter param =
  let open TypeParameter in
  let { loc; name; bounds } = param in
  node
    "TypeParameter"
    loc
    [("name", node_of_identifier name); ("bounds", List (List.map node_of_identifier_type bounds))]

and node_of_identifier_type id =
  let { Type.Identifier.loc; name; type_args } = id in
  node
    "IdentifierType"
    loc
    [
      ("name", node_of_scoped_identifier name); ("type_args", List (List.map node_of_type type_args));
    ]

and node_of_tuple_type tuple =
  let { Type.Tuple.loc; elements } = tuple in
  node "TupleType" loc [("elements", List (List.map node_of_type elements))]

and node_of_function_type func =
  let { Type.Function.loc; params; return } = func in
  node
    "FunctionType"
    loc
    [("params", List (List.map node_of_type params)); ("return", node_of_type return)]

and node_of_trait_type trait =
  let { Type.Trait.loc; trait } = trait in
  node "TraitType" loc [("trait", node_of_identifier_type trait)]

and pp_module mod_ =
  let node = node_of_module mod_ in
  pp node
