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

let rec node_of_loc loc =
  let pp_pos pos =
    let { Loc.line; col } = pos in
    Printf.sprintf "%d:%d" line col
  in
  let { Loc.start; _end; _ } = loc in
  let raw_loc = Printf.sprintf "%s-%s" (pp_pos start) (pp_pos _end) in
  Raw raw_loc

and node name loc attributes = Map (("node", Raw name) :: ("loc", node_of_loc loc) :: attributes)

and node_of_program program =
  let { Program.loc; statements; t = _ } = program in
  node "Program" loc [("statements", List (List.map node_of_statement statements))]

and node_of_statement stmt =
  let open Statement in
  match stmt with
  | Expression expr -> node_of_expression_stmt expr
  | Block block -> node_of_block block

and node_of_expression_stmt (loc, expr) =
  node "ExpressionStatement" loc [("expression", node_of_expression expr)]

and node_of_expression expr =
  let open Expression in
  match expr with
  | Identifier id -> node_of_identifier id
  | IntLiteral lit -> node_of_int_literal lit
  | StringLiteral lit -> node_of_string_literal lit
  | BoolLiteral lit -> node_of_bool_literal lit
  | UnaryOperation unary -> node_of_unary_operation unary
  | BinaryOperation binary -> node_of_binary_operation binary
  | LogicalAnd logical -> node_of_logical_and logical
  | LogicalOr logical -> node_of_logical_or logical

and node_of_identifier id =
  let { Identifier.loc; name; t = _ } = id in
  node "Identifier" loc [("name", String name)]

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

and node_of_block block =
  let { Statement.Block.loc; statements; t = _ } = block in
  node "Block" loc [("statements", List (List.map node_of_statement statements))]

and pp_program program =
  let node = node_of_program program in
  pp node
