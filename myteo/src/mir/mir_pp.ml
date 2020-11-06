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
  let init_strings =
    List.map
      (fun block_id ->
        let block = IMap.find block_id program.Program.blocks in
        pp_block ~cx block)
      global.Global.init
  in
  String.concat "\n" (List.rev init_strings)

and pp_func ~cx ~program func =
  let body_strings =
    List.map
      (fun block_id ->
        let block = IMap.find block_id program.Program.blocks in
        pp_block ~cx block)
      func.Function.body
  in
  String.concat "\n" (List.rev body_strings)

and pp_block ~cx block =
  let open Block in
  let label =
    match block.label with
    | GlobalLabel label -> "GLOBAL__" ^ label
    | FuncLabel label -> "FUNC__" ^ label
  in
  let label_line = label ^ ":" in
  let instr_lines = List.map (pp_instruction ~cx) block.instructions in
  let lines = label_line :: instr_lines in
  let lines =
    match block.next with
    | Halt -> lines
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

and pp_instruction ~cx (_, instr) =
  let prid = prid ~cx in
  let pp_instr var_id instr = Printf.sprintf "%d := %s" (prid var_id) instr in
  let instr_string =
    match instr with
    | LoadUnit var_id -> pp_instr var_id "LoadUnit"
    | LoadInt (var_id, int) -> pp_instr var_id (Printf.sprintf "LoadInt %d" int)
    | LoadString (var_id, string) -> pp_instr var_id (Printf.sprintf "LoadString \"%s\"" string)
    | LoadBool (var_id, bool) ->
      pp_instr
        var_id
        (Printf.sprintf
           "LoadBool %s"
           ( if bool then
             "true"
           else
             "false" ))
    | Ret var_id -> Printf.sprintf "Ret %d" (prid var_id)
    | LogNot (var_id, arg_id) -> pp_instr var_id (Printf.sprintf "LogNot %d" (prid arg_id))
    | LogAnd (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "LogAnd %d %d" (prid left_id) (prid right_id))
    | LogOr (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "LogAnd %d %d" (prid left_id) (prid right_id))
    | NegInt (var_id, arg_id) -> pp_instr var_id (Printf.sprintf "NegInt %d" (prid arg_id))
    | AddInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "AddInt %d %d" (prid left_id) (prid right_id))
    | SubInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "SubInt %d %d" (prid left_id) (prid right_id))
    | MulInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "MulInt %d %d" (prid left_id) (prid right_id))
    | DivInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "DivInt %d %d" (prid left_id) (prid right_id))
    | EqInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "EqInt %d %d" (prid left_id) (prid right_id))
    | NeqInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "NeqInt %d %d" (prid left_id) (prid right_id))
    | LtInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "LtInt %d %d" (prid left_id) (prid right_id))
    | LteqInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "LteqInt %d %d" (prid left_id) (prid right_id))
    | GtInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "GtInt %d %d" (prid left_id) (prid right_id))
    | GteqInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "GteqInt %d %d" (prid left_id) (prid right_id))
  in
  "  " ^ instr_string
