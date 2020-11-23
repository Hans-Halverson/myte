open Basic_collections
open Mir

module Context = struct
  type t = {
    mutable print_var_id_map: int IMap.t;
    mutable max_print_var_id: int;
    mutable print_block_id_map: int IMap.t;
    mutable max_print_block_id: int;
    global_names: string IMap.t;
  }

  let mk program =
    let global_names =
      LocMap.fold
        (fun _ { Global.var_id; name; _ } globals -> IMap.add var_id name globals)
        program.Program.globals
        IMap.empty
    in
    {
      print_var_id_map = IMap.empty;
      max_print_var_id = 0;
      print_block_id_map = IMap.empty;
      max_print_block_id = 0;
      global_names;
    }
end

let rec pp_program program =
  let open Program in
  let cx = Context.mk program in
  let get_loc_keys m = LocMap.fold (fun loc _ locs -> loc :: locs) m [] in
  let block_locs = get_loc_keys program.globals @ get_loc_keys program.funcs in
  let sorted_block_locs = List.sort Loc.compare block_locs in
  let blocks_strings =
    List.map
      (fun loc ->
        match LocMap.find_opt loc program.globals with
        | Some global -> pp_global ~cx ~program global
        | None -> pp_func ~cx ~program (LocMap.find loc program.funcs))
      sorted_block_locs
  in
  String.concat "\n" blocks_strings

and pp_global ~cx ~program global =
  let open Global in
  let global_label = Printf.sprintf "global %s %%%s {" (pp_value_type global.ty) global.name in
  calc_print_block_ids ~cx (List.tl global.init);
  let init_strings =
    List.mapi
      (fun i block_id ->
        let block = IMap.find block_id program.Program.blocks in
        pp_block ~cx ~label:(i <> 0) block)
      global.init
  in
  String.concat "\n" ((global_label :: init_strings) @ ["}\n"])

and pp_func ~cx ~program func =
  let open Function in
  let func_params =
    func.params
    |> List.map (fun (var_id, ty) ->
           Printf.sprintf "%s %s" (pp_value_type ty) (pp_var_id ~cx var_id))
    |> String.concat ", "
  in
  let func_label =
    Printf.sprintf "func %s @%s(%s) {" (pp_value_type func.return_ty) func.name func_params
  in
  calc_print_block_ids ~cx (List.tl func.Function.body);
  let body_strings =
    List.mapi
      (fun i block_id ->
        let block = IMap.find block_id program.Program.blocks in
        pp_block ~cx ~label:(i <> 0) block)
      func.Function.body
  in
  String.concat "\n" ((func_label :: body_strings) @ ["}\n"])

and pp_block ~cx ~label block =
  let open Block in
  let lines =
    if label then
      [Printf.sprintf "label %s:" (pp_block_id ~cx block.id)]
    else
      []
  in
  let lines = lines @ List.map (pp_instruction ~cx) block.instructions in
  let lines =
    match block.next with
    | Halt -> lines
    | Continue block_id -> lines @ [Printf.sprintf "  continue %s" (pp_block_id ~cx block_id)]
    | Branch { test; jump; continue } ->
      lines
      @ [
          Printf.sprintf
            "  branch %s, %s, %s"
            (pp_bool_value ~cx test)
            (pp_block_id ~cx continue)
            (pp_block_id ~cx jump);
        ]
  in
  String.concat "\n" lines

and pp_var_id ~cx var_id =
  let open Context in
  match IMap.find_opt var_id cx.global_names with
  | Some global_name -> "%" ^ global_name
  | None ->
    let print_id =
      match IMap.find_opt var_id cx.print_var_id_map with
      | Some print_id -> print_id
      | None ->
        let print_id = cx.max_print_var_id in
        cx.print_var_id_map <- IMap.add var_id print_id cx.print_var_id_map;
        cx.max_print_var_id <- print_id + 1;
        print_id
    in
    Printf.sprintf "%%%d" print_id

and calc_print_block_ids ~cx block_ids =
  List.iter
    (fun block_id ->
      match IMap.find_opt block_id cx.print_block_id_map with
      | Some _ -> ()
      | None ->
        let print_id = cx.max_print_block_id in
        cx.print_block_id_map <- IMap.add block_id print_id cx.print_block_id_map;
        cx.max_print_block_id <- print_id + 1)
    block_ids

and pp_block_id ~cx block_id =
  let open Context in
  let print_id =
    match IMap.find_opt block_id cx.print_block_id_map with
    | Some print_id -> print_id
    | None ->
      let print_id = cx.max_print_block_id in
      cx.print_block_id_map <- IMap.add block_id print_id cx.print_block_id_map;
      cx.max_print_block_id <- print_id + 1;
      print_id
  in
  Printf.sprintf "@%d" print_id

and pp_unit_value ~cx v =
  let open Instruction.UnitValue in
  match v with
  | Lit -> "()"
  | Var var_id -> pp_var_id ~cx var_id

and pp_bool_value ~cx v =
  let open Instruction.BoolValue in
  match v with
  | Lit true -> "true"
  | Lit false -> "false"
  | Var var_id -> pp_var_id ~cx var_id

and pp_string_value ~cx v =
  let open Instruction.StringValue in
  match v with
  | Lit s -> "\"" ^ s ^ "\""
  | Var var_id -> pp_var_id ~cx var_id

and pp_numeric_value ~cx v =
  let open Instruction.NumericValue in
  match v with
  | IntLit i -> string_of_int i
  | IntVar var_id -> pp_var_id ~cx var_id

and pp_value ~cx v =
  let open Instruction.Value in
  match v with
  | Unit v -> pp_unit_value ~cx v
  | Bool v -> pp_bool_value ~cx v
  | String v -> pp_string_value ~cx v
  | Numeric v -> pp_numeric_value ~cx v

and pp_value_type ty =
  let open ValueType in
  match ty with
  | Unit -> "unit"
  | Int -> "int"
  | Bool -> "bool"
  | String -> "string"

and pp_type_of_value v = pp_value_type (type_of_value v)

and pp_type_of_numeric_value v =
  let open Instruction.NumericValue in
  match v with
  | IntLit _
  | IntVar _ ->
    "int"

and pp_instruction ~cx (_, instr) =
  let pp_instr var_id instr = Printf.sprintf "%s := %s" (pp_var_id ~cx var_id) instr in
  let instr_string =
    match instr with
    | Mov (var_id, right) ->
      pp_instr var_id (Printf.sprintf "Mov %s %s" (pp_type_of_value right) (pp_value ~cx right))
    | Ret val_opt ->
      "Ret"
      ^
      (match val_opt with
      | Some v -> " " ^ pp_value ~cx v
      | None -> "")
    | Phi (var_id, args) ->
      let args = String.concat ", " (List.map (pp_var_id ~cx) args) in
      pp_instr var_id (Printf.sprintf "Phi %s" args)
    | LogNot (var_id, arg) -> pp_instr var_id (Printf.sprintf "LogNot %s" (pp_bool_value ~cx arg))
    | LogAnd (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf "LogAnd %s, %s" (pp_bool_value ~cx left) (pp_bool_value ~cx right))
    | LogOr (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf "LogAnd %s, %s" (pp_bool_value ~cx left) (pp_bool_value ~cx right))
    | Neg (var_id, arg) ->
      pp_instr
        var_id
        (Printf.sprintf "Neg %s %s" (pp_type_of_numeric_value arg) (pp_numeric_value ~cx arg))
    | Add (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Add %s %s, %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Sub (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Sub %s %s, %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Mul (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Mul %s %s, %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Div (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Div %s %s, %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Eq (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Eq %s %s, %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Neq (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Neq %s %s, %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Lt (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Lt %s %s, %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | LtEq (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "LtEq %s %s, %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Gt (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Gt %s %s, %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | GtEq (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "GtEq %s %s, %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
  in
  "  " ^ instr_string
