open Basic_collections
open Mir

let rec pp_program program =
  let open Program in
  let blocks_strings =
    IMap.fold (fun _ block block_strings -> pp_block block :: block_strings) program.blocks []
  in
  String.concat "\n" (List.rev blocks_strings)

and pp_block block =
  let open Block in
  let label =
    match block.label with
    | Label label -> label
    | DebugLabel label -> "DEBUG__" ^ label
  in
  let label_line = label ^ ":" in
  let instr_lines = List.map pp_instruction block.instructions in
  let lines = label_line :: instr_lines in
  let lines =
    match block.next with
    | Halt -> lines
    | Branch _ -> failwith "Unimplemented"
  in
  String.concat "\n" lines

and pp_var_id var_id = string_of_int var_id

and pp_instruction (_, instr) =
  let pp_instr var_id instr = pp_var_id var_id ^ " := " ^ instr in
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
    | LogNot (var_id, arg_id) -> pp_instr var_id (Printf.sprintf "LogNot %d" arg_id)
    | LogAnd (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "LogAnd %d %d" left_id right_id)
    | LogOr (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "LogAnd %d %d" left_id right_id)
    | NegInt (var_id, arg_id) -> pp_instr var_id (Printf.sprintf "NegInt %d" arg_id)
    | AddInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "AddInt %d %d" left_id right_id)
    | SubInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "SubInt %d %d" left_id right_id)
    | MulInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "MulInt %d %d" left_id right_id)
    | DivInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "DivInt %d %d" left_id right_id)
    | EqInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "EqInt %d %d" left_id right_id)
    | NeqInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "NeqInt %d %d" left_id right_id)
    | LtInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "LtInt %d %d" left_id right_id)
    | LteqInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "LteqInt %d %d" left_id right_id)
    | GtInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "GtInt %d %d" left_id right_id)
    | GteqInt (var_id, left_id, right_id) ->
      pp_instr var_id (Printf.sprintf "GteqInt %d %d" left_id right_id)
  in
  "  " ^ instr_string
