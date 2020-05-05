open Ast

type pp_node =
  | None
  | Int of int
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
    | Int int -> add_string (string_of_int int)
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
  let { Module.loc; name; imports; toplevels; t = _ } = mod_ in
  node
    "Module"
    loc
    [
      ("name", node_of_scoped_identifier name);
      ("imports", List (List.map node_of_import imports));
      ("toplevels", List (List.map node_of_toplevel toplevels));
    ]

and node_of_toplevel toplevel =
  let open Module in
  match toplevel with
  | VariableDeclaration decl -> node_of_variable_decl decl
  | FunctionDeclaration decl -> node_of_function decl

and node_of_statement stmt =
  let open Statement in
  match stmt with
  | Expression expr -> node_of_expression_stmt expr
  | Block block -> node_of_block block
  | If if_ -> node_of_if if_
  | Return ret -> node_of_return ret
  | VariableDeclaration decl -> node_of_variable_decl decl
  | FunctionDeclaration decl -> node_of_function decl

and node_of_expression expr =
  let open Expression in
  match expr with
  | Unit unit -> node_of_unit unit
  | Identifier id -> node_of_identifier id
  | IntLiteral lit -> node_of_int_literal lit
  | StringLiteral lit -> node_of_string_literal lit
  | BoolLiteral lit -> node_of_bool_literal lit
  | UnaryOperation unary -> node_of_unary_operation unary
  | BinaryOperation binary -> node_of_binary_operation binary
  | LogicalAnd logical -> node_of_logical_and logical
  | LogicalOr logical -> node_of_logical_or logical
  | Call call -> node_of_call call
  | Access access -> node_of_access access

and node_of_pattern pat =
  let open Pattern in
  match pat with
  | Identifier id -> node_of_identifier id

and node_of_type ty =
  let open Type in
  match ty with
  | Primitive prim -> node_of_primitive_type prim
  | Function func -> node_of_function_type func

and node_of_expression_stmt (loc, expr) =
  node "ExpressionStatement" loc [("expression", node_of_expression expr)]

and node_of_identifier id =
  let { Identifier.loc; name; t = _ } = id in
  node "Identifier" loc [("name", String name)]

and node_of_scoped_identifier id =
  let { ScopedIdentifier.loc; name; scopes; t = _ } = id in
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
  let { Module.Import.Complex.loc; scopes; aliases; t = _ } = import in
  node
    "ComplexImport"
    loc
    [
      ("scopes", List (List.map node_of_identifier scopes));
      ("aliases", List (List.map node_of_import_alias aliases));
    ]

and node_of_import_alias alias =
  let { Module.Import.Alias.loc; name; alias; t = _ } = alias in
  node
    "ImportAlias"
    loc
    [("name", node_of_identifier name); ("alias", opt node_of_identifier alias)]

and node_of_unit unit =
  let { Expression.Unit.loc; t = _ } = unit in
  node "Unit" loc []

and node_of_int_literal lit =
  let { Expression.IntLiteral.loc; raw; value; t = _ } = lit in
  node "IntLiteral" loc [("value", Int value); ("raw", String raw)]

and node_of_string_literal lit =
  let { Expression.StringLiteral.loc; value; t = _ } = lit in
  node "StringLiteral" loc [("value", String value)]

and node_of_bool_literal lit =
  let { Expression.BoolLiteral.loc; value; t = _ } = lit in
  node "BoolLiteral" loc [("value", Bool value)]

and node_of_unary_operation unary =
  let { Expression.UnaryOperation.loc; operand; op; t = _ } = unary in
  node
    "UnaryOperation"
    loc
    [("op", Raw (string_of_unary_op op)); ("operand", node_of_expression operand)]

and node_of_binary_operation binary =
  let { Expression.BinaryOperation.loc; left; right; op; t = _ } = binary in
  node
    "BinaryOperation"
    loc
    [
      ("op", Raw (string_of_binary_op op));
      ("left", node_of_expression left);
      ("right", node_of_expression right);
    ]

and node_of_logical_and logical =
  let { Expression.LogicalAnd.loc; left; right; t = _ } = logical in
  node "LogicalAnd" loc [("left", node_of_expression left); ("right", node_of_expression right)]

and node_of_logical_or logical =
  let { Expression.LogicalOr.loc; left; right; t = _ } = logical in
  node "LogicalOr" loc [("left", node_of_expression left); ("right", node_of_expression right)]

and node_of_call call =
  let { Expression.Call.loc; func; args; t = _ } = call in
  node
    "Call"
    loc
    [("func", node_of_expression func); ("args", List (List.map node_of_expression args))]

and node_of_access binary =
  let { Expression.Access.loc; left; right; t = _ } = binary in
  node "Access" loc [("left", node_of_expression left); ("right", node_of_identifier right)]

and node_of_block block =
  let { Statement.Block.loc; statements; t = _ } = block in
  node "Block" loc [("statements", List (List.map node_of_statement statements))]

and node_of_if if_ =
  let { Statement.If.loc; test; conseq; altern; t = _ } = if_ in
  node
    "If"
    loc
    [
      ("test", node_of_expression test);
      ("conseq", node_of_statement conseq);
      ("altern", opt node_of_statement altern);
    ]

and node_of_return ret =
  let { Statement.Return.loc; arg; t = _ } = ret in
  node "Return" loc [("arg", node_of_expression arg)]

and node_of_variable_decl decl =
  let { Statement.VariableDeclaration.loc; kind; pattern; init; annot; t = _ } = decl in
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

and node_of_function func =
  let open Function in
  let { loc; name; params; body; return; t = _ } = func in
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
    ]

and node_of_function_param param =
  let open Function.Param in
  let { loc; name; annot; t = _ } = param in
  node "Param" loc [("name", node_of_identifier name); ("annot", node_of_type annot)]

and node_of_primitive_type p =
  let { Type.Primitive.loc; kind; t = _ } = p in
  node "PrimitiveType" loc [("kind", Raw (string_of_primitive_type kind))]

and node_of_function_type func =
  let { Type.Function.loc; params; return; t = _ } = func in
  node
    "FunctionType"
    loc
    [("params", List (List.map node_of_type params)); ("return", node_of_type return)]

and pp_module mod_ =
  let node = node_of_module mod_ in
  pp node
