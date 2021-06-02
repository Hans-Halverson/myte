open Ast

type pp_node =
  | None
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
    | Int int -> add_string (Int32.to_string int)
    | String str -> add_strings ["\""; str; "\""]
    | Bool bool ->
      add_string
        ( if bool then
          "true"
        else
          "false" )
    | Raw raw -> add_string raw
    | List [] -> add_string "[]"
    | List nodes ->
      add_string "[\n";
      List.iter
        (fun node ->
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
  | LogicalNot -> "LogicalNot"

let string_of_binary_op op =
  let open Expression.BinaryOperation in
  match op with
  | Add -> "Add"
  | Subtract -> "Subtract"
  | Multiply -> "Multiply"
  | Divide -> "Divide"
  | Equal -> "Equal"
  | NotEqual -> "NotEqual"
  | LessThan -> "LessThan"
  | GreaterThan -> "GreaterThan"
  | LessThanOrEqual -> "LessThanOrEqual"
  | GreaterThanOrEqual -> "GreaterThanOrEqual"

let string_of_primitive_type kind =
  let open Type.Primitive in
  match kind with
  | Int -> "Int"
  | String -> "String"
  | Bool -> "Bool"
  | Unit -> "Unit"

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
  let { Module.loc; module_; imports; toplevels } = mod_ in
  node
    "Module"
    loc
    [
      ("module", node_of_module_module module_);
      ("imports", List (List.map node_of_import imports));
      ("toplevels", List (List.map node_of_toplevel toplevels));
    ]

and node_of_module_module module_ =
  let { Module.Module.loc; name } = module_ in
  node "Module" loc [("name", node_of_scoped_identifier name)]

and node_of_toplevel toplevel =
  let open Module in
  match toplevel with
  | VariableDeclaration decl -> node_of_variable_decl decl
  | FunctionDeclaration decl -> node_of_function decl
  | TypeDeclaration decl -> node_of_type_decl decl

and node_of_statement stmt =
  let open Statement in
  match stmt with
  | VariableDeclaration decl -> node_of_variable_decl decl
  | FunctionDeclaration decl -> node_of_function decl
  | Expression expr -> node_of_expression_stmt expr
  | Block block -> node_of_block block
  | If if_ -> node_of_if if_
  | While while_ -> node_of_while while_
  | Return ret -> node_of_return ret
  | Break break -> node_of_break break
  | Continue continue -> node_of_continue continue
  | Assignment assign -> node_of_assignment assign

and node_of_expression expr =
  let open Expression in
  match expr with
  | Unit unit -> node_of_unit unit
  | Identifier id -> node_of_identifier id
  | ScopedIdentifier id -> node_of_scoped_identifier id
  | IntLiteral lit -> node_of_int_literal lit
  | StringLiteral lit -> node_of_string_literal lit
  | BoolLiteral lit -> node_of_bool_literal lit
  | Record record -> node_of_record_expression record
  | Tuple tuple -> node_of_tuple_expression tuple
  | TypeCast cast -> node_of_type_cast cast
  | UnaryOperation unary -> node_of_unary_operation unary
  | BinaryOperation binary -> node_of_binary_operation binary
  | LogicalAnd logical -> node_of_logical_and logical
  | LogicalOr logical -> node_of_logical_or logical
  | Call call -> node_of_call call
  | IndexedAccess access -> node_of_indexed_access access
  | NamedAccess access -> node_of_named_access access

and node_of_pattern pat =
  let open Pattern in
  match pat with
  | Identifier id -> node_of_identifier id

and node_of_type ty =
  let open Type in
  match ty with
  | Primitive prim -> node_of_primitive_type prim
  | Custom custom -> node_of_custom_type custom
  | Tuple tuple -> node_of_tuple_type tuple
  | Function func -> node_of_function_type func

and node_of_expression_stmt (loc, expr) =
  node "ExpressionStatement" loc [("expression", node_of_expression expr)]

and node_of_identifier id =
  let { Identifier.loc; name } = id in
  node "Identifier" loc [("name", String name)]

and node_of_scoped_identifier id =
  let { ScopedIdentifier.loc; name; scopes } = id in
  node
    "ScopedIdentifier"
    loc
    [("scopes", List (List.map node_of_identifier scopes)); ("name", node_of_identifier name)]

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
  let { Expression.IntLiteral.loc; raw; value } = lit in
  node "IntLiteral" loc [("value", Int value); ("raw", String raw)]

and node_of_string_literal lit =
  let { Expression.StringLiteral.loc; value } = lit in
  node "StringLiteral" loc [("value", String value)]

and node_of_bool_literal lit =
  let { Expression.BoolLiteral.loc; value } = lit in
  node "BoolLiteral" loc [("value", Bool value)]

and node_of_record_expression record =
  let { Expression.Record.loc; name; fields } = record in
  node
    "RecordExpression"
    loc
    [
      ("name", node_of_identifier name);
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

and node_of_block block =
  let { Statement.Block.loc; statements } = block in
  node "Block" loc [("statements", List (List.map node_of_statement statements))]

and node_of_if if_ =
  let { Statement.If.loc; test; conseq; altern } = if_ in
  node
    "If"
    loc
    [
      ("test", node_of_expression test);
      ("conseq", node_of_statement conseq);
      ("altern", opt node_of_statement altern);
    ]

and node_of_while while_ =
  let { Statement.While.loc; test; body } = while_ in
  node "While" loc [("test", node_of_expression test); ("body", node_of_statement body)]

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
  let { Statement.Assignment.loc; pattern; expr } = assign in
  node "Assignment" loc [("pattern", node_of_pattern pattern); ("expr", node_of_expression expr)]

and node_of_variable_decl decl =
  let { Statement.VariableDeclaration.loc; kind; pattern; init; annot } = decl in
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
    ]

and node_of_type_decl decl =
  let open TypeDeclaration in
  let { loc; name; type_params; decl } = decl in
  node
    "TypeDeclaration"
    loc
    [
      ("name", node_of_identifier name);
      ("type_params", List (List.map node_of_type_parameter type_params));
      (match decl with
      | Alias alias -> ("alias", node_of_type alias)
      | Record record -> ("record", node_of_record_variant record)
      | Tuple tuple -> ("tuple", node_of_tuple_variant tuple)
      | Variant variants ->
        ( "variants",
          List
            (List.map
               (fun variant ->
                 match variant with
                 | RecordVariant record -> node_of_record_variant record
                 | TupleVariant tuple -> node_of_tuple_variant tuple
                 | EnumVariant id -> node_of_identifier id)
               variants) ));
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
  let { loc; name; ty } = field in
  node "RecordVariantField" loc [("name", node_of_identifier name); ("type", node_of_type ty)]

and node_of_tuple_variant tuple =
  let open TypeDeclaration.Tuple in
  let { loc; name; elements } = tuple in
  node
    "TupleVariant"
    loc
    [("name", node_of_identifier name); ("elements", List (List.map node_of_type elements))]

and node_of_function func =
  let open Function in
  let { loc; name; type_params; params; body; return } = func in
  let body =
    match body with
    | Block block -> node_of_block block
    | Expression expr -> node_of_expression expr
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
    ]

and node_of_function_param param =
  let open Function.Param in
  let { loc; name; annot } = param in
  node "Param" loc [("name", node_of_identifier name); ("annot", node_of_type annot)]

and node_of_type_parameter param =
  let open TypeParameter in
  let { loc; name } = param in
  node "TypeParameter" loc [("name", node_of_identifier name)]

and node_of_primitive_type p =
  let { Type.Primitive.loc; kind } = p in
  node "PrimitiveType" loc [("kind", Raw (string_of_primitive_type kind))]

and node_of_custom_type named =
  let { Type.Custom.loc; name } = named in
  node "CustomType" loc [("name", node_of_scoped_identifier name)]

and node_of_tuple_type tuple =
  let { Type.Tuple.loc; elements } = tuple in
  node "TupleType" loc [("elements", List (List.map node_of_type elements))]

and node_of_function_type func =
  let { Type.Function.loc; params; return; type_params } = func in
  node
    "FunctionType"
    loc
    [
      ("params", List (List.map node_of_type params));
      ("return", node_of_type return);
      ("type_params", List (List.map node_of_type_parameter type_params));
    ]

and pp_module mod_ =
  let node = node_of_module mod_ in
  pp node
