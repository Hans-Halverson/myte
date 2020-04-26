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
    | String str -> add_strings ["\""; str ;"\""]
    | Bool bool -> add_string (if bool then "true" else "false")
    | Raw raw -> add_string raw
    | List nodes -> add_string "[\n";
      List.iter
        (fun node ->
           add_indent (indent + 1);
           pp_node node (indent + 1);
           add_string ",\n")
        nodes;
      add_indent indent;
      add_string "]"
    | Map attrs -> add_string "{\n";
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

let rec node_of_loc loc =
  let pp_pos pos =
    let { Loc.line; col } = pos in
    Printf.sprintf "%d:%d" line col
  in
  let { Loc.start; _end; _ } = loc in
  let raw_loc = Printf.sprintf "%s-%s" (pp_pos start) (pp_pos _end) in
  Raw raw_loc

and node name loc attributes =
  Map (("node", Raw name) :: ("loc", node_of_loc loc) :: attributes)

and node_of_program program =
  let { Program.loc; statements; t = _ } = program in
  node "Program" loc [ "statements", List (List.map node_of_statement statements) ]

and node_of_statement stmt =
  let open Statement in
  match stmt with
  | Expression expr -> node_of_expression expr

and node_of_expression expr =
  let open Expression in
  match expr with
  | Identifier id -> node_of_identifier id

and node_of_identifier id =
  let { Identifier.loc; name; t = _ } = id in
  node "Identifier" loc [ ("name", String name) ]

and pp_program program =
  let node = node_of_program program in
  pp node

