open Basic_collections
open Mir

module Context = struct
  type t = {
    mutable print_id_map: int IMap.t;
    mutable max_print_id: int;
  }

  let empty = { print_id_map = IMap.empty; max_print_id = 0 }
end

let rec pp_program program =
  let open Program in
  let cx = Context.empty in
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
  let global_label = Printf.sprintf "global %s %s:" (pp_value_type global.ty) global.name in
  let init_strings =
    List.map
      (fun block_id ->
        let block = IMap.find block_id program.Program.blocks in
        pp_block ~cx block)
      global.init
  in
  String.concat "\n" (global_label :: List.rev init_strings)

and pp_func ~cx ~program func =
  let open Function in
  let func_params =
    func.params
    |> List.map (fun (var_id, ty) -> Printf.sprintf "%s %d" (pp_value_type ty) (prid ~cx var_id))
    |> String.concat ", "
  in
  let func_label =
    Printf.sprintf "func %s %s(%s):" (pp_value_type func.return_ty) func.name func_params
  in
  let body_strings =
    List.map
      (fun block_id ->
        let block = IMap.find block_id program.Program.blocks in
        pp_block ~cx block)
      func.Function.body
  in
  String.concat "\n" (func_label :: List.rev body_strings)

and pp_block ~cx block =
  let open Block in
  let instr_lines = List.map (pp_instruction ~cx) block.instructions in
  let lines =
    match block.next with
    | Halt -> instr_lines
    | Branch _ -> failwith "Unimplemented"
  in
  String.concat "\n" lines

and prid ~cx var_id =
  let open Context in
  match IMap.find_opt var_id cx.print_id_map with
  | Some print_id -> print_id
  | None ->
    let print_id = cx.max_print_id in
    cx.print_id_map <- IMap.add var_id print_id cx.print_id_map;
    cx.max_print_id <- print_id + 1;
    print_id

and pp_numeric_type ty =
  let open Instruction.NumericType in
  match ty with
  | Int -> "int"

and pp_value_type ty =
  let open ValueType in
  match ty with
  | Unit -> "unit"
  | Int -> "int"
  | Bool -> "bool"
  | String -> "string"

and pp_instruction ~cx (_, instr) =
  let prid = prid ~cx in
  let pp_instr var_id instr = Printf.sprintf "%d := %s" (prid var_id) instr in
  let instr_string =
    match instr with
    | Lit (var_id, value) ->
      let type_and_value =
        let open Instruction in
        match value with
        | LitValue.Unit -> "unit"
        | LitValue.Int i -> Printf.sprintf "int %d" i
        | LitValue.String s -> Printf.sprintf "string \"%s\"" s
        | LitValue.Bool b ->
          Printf.sprintf
            "bool %s"
            ( if b then
              "true"
            else
              "false" )
      in
      pp_instr var_id ("Lit " ^ type_and_value)
    | Ret var_id_opt ->
      "Ret"
      ^
      (match var_id_opt with
      | Some var_id -> Printf.sprintf " %d" (prid var_id)
      | None -> "")
    | LogNot (var_id, arg_id) -> pp_instr var_id (Printf.sprintf "LogNot %d" (prid arg_id))
    | LogAnd (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "LogAnd %d %d" (prid left_id) (prid right_id))
    | LogOr (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "LogAnd %d %d" (prid left_id) (prid right_id))
    | Neg (ty, var_id, arg_id) ->
      pp_instr var_id (Printf.sprintf "Neg %s %d" (pp_numeric_type ty) (prid arg_id))
    | Add (ty, var_id, left_id, right_id) ->
      pp_instr
        var_id
        (Printf.sprintf "Add %s %d %d" (pp_numeric_type ty) (prid left_id) (prid right_id))
    | Sub (ty, var_id, left_id, right_id) ->
      pp_instr
        var_id
        (Printf.sprintf "Sub %s %d %d" (pp_numeric_type ty) (prid left_id) (prid right_id))
    | Mul (ty, var_id, left_id, right_id) ->
      pp_instr
        var_id
        (Printf.sprintf "Mul %s %d %d" (pp_numeric_type ty) (prid left_id) (prid right_id))
    | Div (ty, var_id, left_id, right_id) ->
      pp_instr
        var_id
        (Printf.sprintf "Div %s %d %d" (pp_numeric_type ty) (prid left_id) (prid right_id))
    | Eq (ty, var_id, left_id, right_id) ->
      pp_instr
        var_id
        (Printf.sprintf "Eq %s %d %d" (pp_numeric_type ty) (prid left_id) (prid right_id))
    | Neq (ty, var_id, left_id, right_id) ->
      pp_instr
        var_id
        (Printf.sprintf "Neq %s %d %d" (pp_numeric_type ty) (prid left_id) (prid right_id))
    | Lt (ty, var_id, left_id, right_id) ->
      pp_instr
        var_id
        (Printf.sprintf "Lt %s %d %d" (pp_numeric_type ty) (prid left_id) (prid right_id))
    | LtEq (ty, var_id, left_id, right_id) ->
      pp_instr
        var_id
        (Printf.sprintf "LtEq %s %d %d" (pp_numeric_type ty) (prid left_id) (prid right_id))
    | Gt (ty, var_id, left_id, right_id) ->
      pp_instr
        var_id
        (Printf.sprintf "Gt %s %d %d" (pp_numeric_type ty) (prid left_id) (prid right_id))
    | GtEq (ty, var_id, left_id, right_id) ->
      pp_instr
        var_id
        (Printf.sprintf "GtEq %s %d %d" (pp_numeric_type ty) (prid left_id) (prid right_id))
  in
  "  " ^ instr_string
