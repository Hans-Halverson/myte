open Basic_collections
open Mir
open Mir_type

module Context = struct
  type t = {
    mutable print_var_id_map: int IMap.t;
    mutable max_print_var_id: int;
    mutable print_block_id_map: string IMap.t;
    mutable max_print_block_id: int;
    program: ssa_program;
  }

  let mk program =
    {
      print_var_id_map = IMap.empty;
      max_print_var_id = 0;
      print_block_id_map = IMap.empty;
      max_print_block_id = 0;
      program;
    }
end

let rec pp_program program =
  let open Program in
  let cx = Context.mk program in
  (* Filter items to print depending on dump_stdlib settings *)
  let dump_stdlib = Opts.dump_stdlib () in
  let dump_stdlib_prefix = Opts.dump_stdlib_prefix () in
  let filter_stdlib =
    if not dump_stdlib then
      fun name ->
    Std_lib.has_std_lib_prefix name || Mir.has_std_lib_string_prefix name
    else
      match dump_stdlib_prefix with
      | None -> (fun _ -> false)
      | Some prefix ->
        let prefix_length = String.length prefix in
        fun name ->
          Std_lib.has_std_lib_prefix name
          && not (String.length name >= prefix_length && String.sub name 0 prefix_length = prefix)
  in
  (* Collect printed blocks along with their source locations *)
  let blocks =
    SMap.fold
      (fun name global blocks ->
        if filter_stdlib name then
          blocks
        else
          (global.Global.loc, (fun _ -> pp_global ~cx global)) :: blocks)
      program.globals
      []
  in
  let blocks =
    SMap.fold
      (fun name func blocks ->
        if filter_stdlib name then
          blocks
        else
          (func.Function.loc, (fun _ -> pp_func ~cx ~program func)) :: blocks)
      program.funcs
      blocks
  in
  let blocks =
    SMap.fold
      (fun name type_ blocks ->
        if filter_stdlib name then
          blocks
        else
          (type_.Aggregate.loc, (fun _ -> pp_type_decl type_)) :: blocks)
      program.types
      blocks
  in
  (* Sort by block source location *)
  let sorted_blocks = List.sort (fun (l1, _) (l2, _) -> Loc.compare l1 l2) blocks in
  String.concat "\n" (List.map (fun (_, mk_block) -> mk_block ()) sorted_blocks)

and pp_global ~cx global =
  let init =
    match global.init_val with
    | None -> "uninitialized"
    | Some init_val -> pp_value ~cx init_val
  in
  let open Global in
  let global_label = Printf.sprintf "global %s @%s = %s\n" (pp_type global.ty) global.name init in
  global_label

and pp_func ~cx ~program func =
  let open Function in
  let func_params =
    func.params
    |> List.map (fun (_, var_id, ty) -> Printf.sprintf "%s %s" (pp_type ty) (pp_var_id ~cx var_id))
    |> String.concat ", "
  in
  let func_label =
    Printf.sprintf "func %s @%s(%s) {" (pp_type func.return_ty) func.name func_params
  in
  cx.print_block_id_map <- IMap.add func.body_start_block func.name cx.print_block_id_map;
  let body_blocks = Block_ordering.order_blocks ~program func.body_start_block in
  calc_print_block_ids ~cx (List.tl body_blocks);
  let body_strings =
    List.mapi
      (fun i block_id ->
        let block = IMap.find block_id program.Program.blocks in
        pp_block ~cx ~label:(i <> 0) block)
      body_blocks
  in
  String.concat "\n" ((func_label :: body_strings) @ ["}\n"])

and pp_type_decl type_ =
  let open Aggregate in
  let element_strings = List.map (fun (_, element_ty) -> pp_type element_ty) type_.elements in
  let elements_string = String.concat ", " element_strings in
  Printf.sprintf "type %s {%s}\n" type_.name elements_string

and pp_block ~cx ~label block =
  let open Block in
  let label_lines =
    let debug_id =
      if Opts.dump_debug () then
        Printf.sprintf "(Block #%d) " block.id
      else
        ""
    in
    if label then
      [Printf.sprintf "%slabel %s:" debug_id (pp_block_id ~cx block.id)]
    else if Opts.dump_debug () then
      [debug_id]
    else
      []
  in
  let phi_lines =
    List.map
      (fun (value_type, var_id, args) ->
        Printf.sprintf
          "  %s := Phi %s %s"
          (pp_var_id ~cx var_id)
          (pp_type value_type)
          (String.concat
             ", "
             (List.map
                (fun (prev_block_id, arg_var_id) ->
                  pp_block_id ~cx prev_block_id ^ ":" ^ pp_var_id ~cx arg_var_id)
                (IMap.bindings args))))
      block.phis
  in
  let instruction_lines = List.map (pp_instruction ~cx) block.instructions in
  let next_lines =
    match block.next with
    | Halt -> []
    | Continue block_id ->
      let debug_id =
        if Opts.dump_debug () then
          Printf.sprintf "(Block #%d) " block_id
        else
          ""
      in
      [Printf.sprintf "  continue %s%s" debug_id (pp_block_id ~cx block_id)]
    | Branch { test; jump; continue } ->
      [
        Printf.sprintf
          "  branch %s, %s, %s"
          (pp_bool_value ~cx test)
          (pp_block_id ~cx continue)
          (pp_block_id ~cx jump);
      ]
  in
  let lines = List.concat [label_lines; phi_lines; instruction_lines; next_lines] in
  String.concat "\n" lines

and pp_var_id ~cx var_id =
  let open Context in
  let print_id =
    if Opts.dump_debug () then
      var_id
    else
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
        cx.max_print_block_id <- print_id + 1;
        cx.print_block_id_map <- IMap.add block_id (string_of_int print_id) cx.print_block_id_map)
    block_ids

and pp_block_id ~cx block_id =
  let open Context in
  let print_id =
    match IMap.find_opt block_id cx.print_block_id_map with
    | Some print_id -> print_id
    | None ->
      let print_id = cx.max_print_block_id in
      cx.print_block_id_map <- IMap.add block_id (string_of_int print_id) cx.print_block_id_map;
      cx.max_print_block_id <- print_id + 1;
      string_of_int print_id
  in
  "@" ^ print_id

and pp_value ~cx v =
  match v with
  | `UnitL -> "()"
  | `BoolL true -> "true"
  | `BoolL false -> "false"
  | `ByteL i -> string_of_int i
  | `IntL i -> Int32.to_string i
  | `LongL i -> Int64.to_string i
  | `FunctionL label
  | `PointerL (_, label) ->
    "@" ^ label
  | `ArrayL (_, _, str) -> Printf.sprintf "\"%s\"" str
  | `UnitV var_id
  | `BoolV var_id
  | `ByteV var_id
  | `IntV var_id
  | `LongV var_id
  | `FunctionV var_id
  | `PointerV (_, var_id)
  | `AggregateV (_, var_id)
  | `ArrayV (_, _, var_id) ->
    pp_var_id ~cx var_id

and pp_bool_value ~cx v = pp_value ~cx (v :> ssa_value)

and pp_long_value ~cx v = pp_value ~cx (v :> ssa_value)

and pp_numeric_value ~cx v = pp_value ~cx (v :> ssa_value)

and pp_function_value ~cx v = pp_value ~cx (v :> ssa_value)

and pp_pointer_value ~cx v = pp_value ~cx (v :> ssa_value)

and pp_type ty = type_to_string ty

and pp_type_of_value v = pp_type (type_of_value v)

and pp_type_of_numeric_value v = pp_type_of_value (v :> ssa_value)

and pp_instruction ~cx (_, instr) =
  let open Instruction in
  let pp_instr var_id instr = Printf.sprintf "%s := %s" (pp_var_id ~cx var_id) instr in
  let instr_string =
    match instr with
    | Mov (var_id, right) ->
      pp_instr var_id (Printf.sprintf "Mov %s %s" (pp_type_of_value right) (pp_value ~cx right))
    | Call (var_id, ret_ty, func, args) ->
      let args_string = List.map (pp_value ~cx) args |> String.concat ", " in
      pp_instr
        var_id
        (Printf.sprintf "Call %s %s(%s)" (pp_type ret_ty) (pp_function_value ~cx func) args_string)
    | CallBuiltin (var_id, ret_ty, { Builtin.name; _ }, args) ->
      let args_string = List.map (pp_value ~cx) args |> String.concat ", " in
      pp_instr var_id (Printf.sprintf "CallBuiltin %s %s(%s)" (pp_type ret_ty) name args_string)
    | Ret val_opt ->
      "Ret"
      ^
      (match val_opt with
      | Some v -> " " ^ pp_value ~cx v
      | None -> "")
    | Load (var_id, ptr) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Load %s %s"
           (pp_type (pointer_value_element_type ptr))
           (pp_pointer_value ~cx ptr))
    | Store (ptr, right) ->
      Printf.sprintf
        "Store %s %s, %s"
        (pp_type (pointer_value_element_type ptr))
        (pp_pointer_value ~cx ptr)
        (pp_value ~cx right)
    | GetPointer { GetPointer.var_id; return_ty; pointer; pointer_offset; offsets } ->
      let pp_pointer_offset pointer_offset =
        Printf.sprintf
          "[%s %s]"
          (pp_type_of_numeric_value pointer_offset)
          (pp_numeric_value ~cx pointer_offset)
      in

      let pointer_offset_str =
        Option_utils.value_map pp_pointer_offset ~default:"" pointer_offset
      in
      let offset_strs =
        List.map
          (fun offset ->
            match offset with
            | GetPointer.PointerIndex value -> pp_pointer_offset value
            | GetPointer.FieldIndex field -> "." ^ string_of_int field)
          offsets
      in
      pp_instr
        var_id
        (Printf.sprintf
           "GetPointer %s %s%s%s"
           (pp_type return_ty)
           (pp_pointer_value ~cx pointer)
           pointer_offset_str
           (String.concat "" offset_strs))
    | LogNot (var_id, arg) -> pp_instr var_id (Printf.sprintf "LogNot %s" (pp_bool_value ~cx arg))
    | LogAnd (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf "LogAnd %s, %s" (pp_bool_value ~cx left) (pp_bool_value ~cx right))
    | LogOr (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf "LogAnd %s, %s" (pp_bool_value ~cx left) (pp_bool_value ~cx right))
    | BitNot (var_id, arg) ->
      pp_instr
        var_id
        (Printf.sprintf "BitNot %s %s" (pp_type_of_numeric_value arg) (pp_numeric_value ~cx arg))
    | BitAnd (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "BitAnd %s, %s %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | BitOr (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "BitOr %s, %s %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | BitXor (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "BitXor %s, %s %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Shl (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Shl %s, %s %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Shr (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Shr %s, %s %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Shrl (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Shrl %s, %s %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
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
    | Rem (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Rem %s %s, %s"
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
