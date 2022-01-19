open Basic_collections
open Mir
open Mir_type

module Context = struct
  type t = {
    mutable print_value_id_map: int IMap.t;
    mutable max_print_value_id: int;
    mutable print_block_id_map: string BlockMap.t;
    mutable max_print_block_id: int;
    program: Program.t;
  }

  let mk program =
    {
      print_value_id_map = IMap.empty;
      max_print_value_id = 0;
      print_block_id_map = BlockMap.empty;
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
          (Std_lib.has_std_lib_prefix name || Mir.has_std_lib_string_prefix name)
          && not (String.length name >= prefix_length && String.sub name 0 prefix_length = prefix)
  in
  (* Collect printed blocks along with their source locations *)
  let blocks =
    SMap.fold
      (fun name type_ blocks ->
        if filter_stdlib name then
          blocks
        else
          (Aggregate.(type_.loc, type_.name, 0), (fun _ -> pp_type_decl type_)) :: blocks)
      program.types
      []
  in
  let blocks =
    SMap.fold
      (fun name global blocks ->
        if filter_stdlib name then
          blocks
        else
          (Global.(global.loc, global.name, 1), (fun _ -> pp_global ~cx global)) :: blocks)
      program.globals
      blocks
  in
  let blocks =
    SMap.fold
      (fun name func blocks ->
        if filter_stdlib name then
          blocks
        else
          (Function.(func.loc, func.name, 2), (fun _ -> pp_func ~cx func)) :: blocks)
      program.funcs
      blocks
  in
  (* Sort by block source location, break ties by name *)
  let sorted_blocks =
    List.sort
      (fun ((loc1, name1, order1), _) ((loc2, name2, order2), _) ->
        match Loc.compare loc1 loc2 with
        | 0 ->
          (match Int.compare order1 order2 with
          | 0 -> String.compare name1 name2
          | other -> other)
        | other -> other)
      blocks
  in
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

and pp_func ~cx func =
  let open Function in
  (* Each function's values and labels have their own print id space *)
  cx.max_print_value_id <- 0;
  cx.max_print_block_id <- 0;
  let func_params =
    func.params
    |> List.map (fun { Argument.id; type_; _ } ->
           Printf.sprintf "%s %s" (pp_type type_) (pp_value_id ~cx id))
    |> String.concat ", "
  in
  let return_ty =
    match func.return_type with
    | None -> "void"
    | Some return_ty -> pp_type return_ty
  in
  let func_label = Printf.sprintf "func %s @%s(%s) {" return_ty func.name func_params in
  cx.print_block_id_map <- BlockMap.add func.start_block func.name cx.print_block_id_map;
  let body_blocks = Mir_block_ordering.order_blocks func.start_block in
  calc_print_block_ids ~cx (List.tl body_blocks);
  let body_strings = List.mapi (fun i block -> pp_block ~cx ~label:(i <> 0) block) body_blocks in
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
      [Printf.sprintf "%slabel %s:" debug_id (pp_block_id ~cx block)]
    else if Opts.dump_debug () then
      [debug_id]
    else
      []
  in
  let instruction_lines =
    fold_instructions block [] (fun instr acc -> pp_instruction ~cx instr :: acc) |> List.rev
  in
  let next_lines =
    match block.next with
    | Halt -> []
    | Continue block ->
      let debug_id =
        if Opts.dump_debug () then
          Printf.sprintf "(Block #%d) " block.id
        else
          ""
      in
      [Printf.sprintf "  continue %s%s" debug_id (pp_block_id ~cx block)]
    | Branch { test; jump; continue } ->
      [
        Printf.sprintf
          "  branch %s, %s, %s"
          (pp_value ~cx test)
          (pp_block_id ~cx continue)
          (pp_block_id ~cx jump);
      ]
  in
  let lines = List.concat [label_lines; instruction_lines; next_lines] in
  String.concat "\n" lines

and pp_value_id ~cx value_id =
  let open Context in
  let print_id =
    if Opts.dump_debug () then
      value_id
    else
      match IMap.find_opt value_id cx.print_value_id_map with
      | Some print_id -> print_id
      | None ->
        let print_id = cx.max_print_value_id in
        cx.print_value_id_map <- IMap.add value_id print_id cx.print_value_id_map;
        cx.max_print_value_id <- print_id + 1;
        print_id
  in
  Printf.sprintf "%%%d" print_id

and calc_print_block_ids ~cx blocks =
  List.iter
    (fun block ->
      match BlockMap.find_opt block cx.print_block_id_map with
      | Some _ -> ()
      | None ->
        let print_id = cx.max_print_block_id in
        cx.max_print_block_id <- print_id + 1;
        cx.print_block_id_map <- BlockMap.add block (string_of_int print_id) cx.print_block_id_map)
    blocks

and pp_block_id ~cx block =
  let open Context in
  let print_id =
    match BlockMap.find_opt block cx.print_block_id_map with
    | Some print_id -> print_id
    | None ->
      let print_id = cx.max_print_block_id in
      cx.print_block_id_map <- BlockMap.add block (string_of_int print_id) cx.print_block_id_map;
      cx.max_print_block_id <- print_id + 1;
      string_of_int print_id
  in
  "@" ^ print_id

and pp_value ~cx value =
  match value with
  | Instr { id; _ }
  | Argument { id; _ } ->
    pp_value_id ~cx id
  | Lit lit -> pp_literal lit

and pp_literal lit =
  match lit with
  | Literal.Bool true -> "true"
  | Bool false -> "false"
  | Byte i -> string_of_int i
  | Int i -> Int32.to_string i
  | Long i -> Int64.to_string i
  | Function label
  | Pointer (_, label) ->
    "@" ^ label
  | NullPointer _ -> "null"
  | ArrayString str -> "\"" ^ str ^ "\""
  | ArrayVtable (_, funcs) ->
    let funcs = List.map (fun func -> "@" ^ func) funcs in
    "[" ^ String.concat ", " funcs ^ "]"

and pp_type ty = type_to_string ty

and pp_numeric_type ty = type_to_string (ty :> Type.t)

and pp_type_of_value v = pp_type (type_of_value v)

and pp_unary_operation unary_operation =
  match unary_operation with
  | Instruction.Neg -> "Neg"
  | Not -> "Not"

and pp_binary_operation binary_operation =
  match binary_operation with
  | Instruction.Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Rem -> "Rem"
  | And -> "And"
  | Or -> "Or"
  | Xor -> "Xor"
  | Shl -> "Shl"
  | Shr -> "Shr"
  | Shrl -> "Shrl"

and pp_comparison comparison =
  match comparison with
  | Instruction.Eq -> "Eq"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | LtEq -> "LtEq"
  | Gt -> "Gt"
  | GtEq -> "GtEq"

and pp_instruction ~cx instr =
  let open Instruction in
  let pp_instr str = Printf.sprintf "%s := %s" (pp_value_id ~cx instr.id) str in
  let instr_string =
    match instr.instr with
    | Mov right ->
      pp_instr (Printf.sprintf "Mov %s %s" (pp_type_of_value right) (pp_value ~cx right))
    | Phi { args } ->
      let args_string =
        List.map
          (fun (prev_block, arg) -> pp_block_id ~cx prev_block ^ ":" ^ pp_value ~cx arg)
          (BlockMap.bindings args)
        |> String.concat ", "
      in
      pp_instr (Printf.sprintf "Phi %s %s" (pp_type instr.type_) args_string)
    | Call { func; args; has_return } ->
      let func_string =
        match func with
        | Value func -> pp_value ~cx func
        | Builtin { name; _ } -> name
      in
      let args_string = List.map (pp_value ~cx) args |> String.concat ", " in
      if has_return then
        pp_instr (Printf.sprintf "Call %s %s(%s)" (pp_type instr.type_) func_string args_string)
      else
        Printf.sprintf "Call void %s(%s)" func_string args_string
    | Ret val_opt ->
      "Ret"
      ^
      (match val_opt with
      | Some v -> " " ^ pp_value ~cx v
      | None -> "")
    | StackAlloc ty -> pp_instr (Printf.sprintf "StackAlloc %s" (pp_type ty))
    | Load ptr ->
      pp_instr
        (Printf.sprintf "Load %s %s" (pp_type (pointer_value_element_type ptr)) (pp_value ~cx ptr))
    | Store (ptr, right) ->
      Printf.sprintf
        "Store %s %s, %s"
        (pp_type (pointer_value_element_type ptr))
        (pp_value ~cx ptr)
        (pp_value ~cx right)
    | GetPointer { GetPointer.pointer; pointer_offset; offsets } ->
      let pointer_ty = type_of_value pointer in
      let pp_pointer_offset pointer_offset =
        Printf.sprintf "[%s %s]" (pp_type_of_value pointer_offset) (pp_value ~cx pointer_offset)
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
      let ptr_element_type = cast_to_pointer_type instr.type_ in
      pp_instr
        (Printf.sprintf
           "GetPointer %s, %s %s%s%s"
           (pp_type ptr_element_type)
           (pp_type pointer_ty)
           (pp_value ~cx pointer)
           pointer_offset_str
           (String.concat "" offset_strs))
    | Unary (op, arg) ->
      pp_instr
        (Printf.sprintf
           "%s %s %s"
           (pp_unary_operation op)
           (pp_type_of_value arg)
           (pp_value ~cx arg))
    | Binary (op, left, right) ->
      pp_instr
        (Printf.sprintf
           "%s %s %s, %s"
           (pp_binary_operation op)
           (pp_type_of_value left)
           (pp_value ~cx left)
           (pp_value ~cx right))
    | Cmp (cmp, left, right) ->
      pp_instr
        (Printf.sprintf
           "%s %s %s, %s"
           (pp_comparison cmp)
           (pp_type_of_value left)
           (pp_value ~cx left)
           (pp_value ~cx right))
    | Cast arg ->
      pp_instr
        (Printf.sprintf
           "Cast %s %s to %s"
           (pp_type_of_value arg)
           (pp_value ~cx arg)
           (pp_type instr.type_))
    | Trunc arg ->
      pp_instr
        (Printf.sprintf
           "Trunc %s %s to %s"
           (pp_type_of_value arg)
           (pp_value ~cx arg)
           (pp_numeric_type instr.type_))
    | SExt arg ->
      pp_instr
        (Printf.sprintf
           "SExt %s %s to %s"
           (pp_type_of_value arg)
           (pp_value ~cx arg)
           (pp_numeric_type instr.type_))
  in
  "  " ^ instr_string
