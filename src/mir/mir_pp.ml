open Basic_collections
open Mir
open Mir_type

module Context = struct
  type t = {
    mutable print_var_id_map: int IMap.t;
    mutable max_print_var_id: int;
    mutable print_block_id_map: string IMap.t;
    mutable max_print_block_id: int;
    program: Program.t;
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
          (Function.(func.loc, func.name, 2), (fun _ -> pp_func ~cx ~program func)) :: blocks)
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

and pp_func ~cx ~program func =
  let open Function in
  (* Each function's variables and labels have their own print id space *)
  cx.max_print_var_id <- 0;
  cx.max_print_block_id <- 0;
  let func_params =
    func.params
    |> List.map (fun (_, var_id, ty) -> Printf.sprintf "%s %s" (pp_type ty) (pp_var_id ~cx var_id))
    |> String.concat ", "
  in
  let return_ty =
    match func.return_ty with
    | None -> "void"
    | Some return_ty -> pp_type return_ty
  in
  let func_label = Printf.sprintf "func %s @%s(%s) {" return_ty func.name func_params in
  cx.print_block_id_map <- IMap.add func.body_start_block func.name cx.print_block_id_map;
  let body_blocks = Mir_block_ordering.order_blocks ~program func.body_start_block in
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
                (fun (prev_block_id, arg) -> pp_block_id ~cx prev_block_id ^ ":" ^ pp_value ~cx arg)
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
  | `BoolL true -> "true"
  | `BoolL false -> "false"
  | `ByteL i -> string_of_int i
  | `IntL i -> Int32.to_string i
  | `LongL i -> Int64.to_string i
  | `FunctionL label
  | `PointerL (_, label) ->
    "@" ^ label
  | `ArrayStringL str -> "\"" ^ str ^ "\""
  | `ArrayVtableL (_, funcs) ->
    let funcs = List.map (fun func -> pp_value ~cx (func :> Value.t)) funcs in
    "[" ^ String.concat ", " funcs ^ "]"
  | `BoolV var_id
  | `ByteV var_id
  | `IntV var_id
  | `LongV var_id
  | `FunctionV var_id
  | `PointerV (_, var_id)
  | `ArrayV (_, _, var_id) ->
    pp_var_id ~cx var_id

and pp_bool_value ~cx v = pp_value ~cx (v :> Value.t)

and pp_long_value ~cx v = pp_value ~cx (v :> Value.t)

and pp_numeric_value ~cx v = pp_value ~cx (v :> Value.t)

and pp_function_value ~cx v = pp_value ~cx (v :> Value.t)

and pp_pointer_value ~cx v = pp_value ~cx (v :> Value.t)

and pp_comparable_value ~cx v = pp_value ~cx (v :> Value.t)

and pp_type ty = type_to_string ty

and pp_numeric_type ty = type_to_string (ty :> Type.t)

and pp_type_of_value v = pp_type (type_of_value v)

and pp_type_of_numeric_value v = pp_type_of_value (v :> Value.t)

and pp_type_of_comparable_value v = pp_type_of_value (v :> Value.t)

and pp_comparison comparison =
  match comparison with
  | Instruction.Eq -> "Eq"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | LtEq -> "LtEq"
  | Gt -> "Gt"
  | GtEq -> "GtEq"

and pp_instruction ~cx (_, instr) =
  let open Instruction in
  let pp_instr var_id instr = Printf.sprintf "%s := %s" (pp_var_id ~cx var_id) instr in
  let instr_string =
    match instr with
    | Mov (var_id, right) ->
      pp_instr var_id (Printf.sprintf "Mov %s %s" (pp_type_of_value right) (pp_value ~cx right))
    | Call { return; func; args } ->
      let args_string = List.map (pp_value ~cx) args |> String.concat ", " in
      let func_string = pp_function_value ~cx func in
      (match return with
      | None -> Printf.sprintf "Call void %s(%s)" func_string args_string
      | Some (var_id, ret_ty) ->
        pp_instr var_id (Printf.sprintf "Call %s %s(%s)" (pp_type ret_ty) func_string args_string))
    | CallBuiltin { return; func = { Builtin.name; _ }; args } ->
      let args_string = List.map (pp_value ~cx) args |> String.concat ", " in
      (match return with
      | None -> Printf.sprintf "CallBuiltin void %s(%s)" name args_string
      | Some (var_id, ret_ty) ->
        pp_instr var_id (Printf.sprintf "CallBuiltin %s %s(%s)" (pp_type ret_ty) name args_string))
    | Ret val_opt ->
      "Ret"
      ^
      (match val_opt with
      | Some v -> " " ^ pp_value ~cx v
      | None -> "")
    | StackAlloc (var_id, ty) -> pp_instr var_id (Printf.sprintf "StackAlloc %s" (pp_type ty))
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
      let pointer_ty = type_of_value (pointer :> Value.t) in
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
           "GetPointer %s, %s %s%s%s"
           (pp_type return_ty)
           (pp_type pointer_ty)
           (pp_pointer_value ~cx pointer)
           pointer_offset_str
           (String.concat "" offset_strs))
    | Not (var_id, arg) ->
      pp_instr
        var_id
        (Printf.sprintf "Not %s %s" (pp_type_of_numeric_value arg) (pp_numeric_value ~cx arg))
    | And (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "And %s, %s %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Or (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Or %s, %s %s"
           (pp_type_of_numeric_value left)
           (pp_numeric_value ~cx left)
           (pp_numeric_value ~cx right))
    | Xor (var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Xor %s, %s %s"
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
    | Cmp (comparison, var_id, left, right) ->
      pp_instr
        var_id
        (Printf.sprintf
           "%s %s %s, %s"
           (pp_comparison comparison)
           (pp_type_of_comparable_value left)
           (pp_comparable_value ~cx left)
           (pp_comparable_value ~cx right))
    | Trunc (var_id, arg, ty) ->
      pp_instr
        var_id
        (Printf.sprintf
           "Trunc %s %s to %s"
           (pp_type_of_numeric_value arg)
           (pp_numeric_value ~cx arg)
           (pp_numeric_type ty))
    | SExt (var_id, arg, ty) ->
      pp_instr
        var_id
        (Printf.sprintf
           "SExt %s %s to %s"
           (pp_type_of_numeric_value arg)
           (pp_numeric_value ~cx arg)
           (pp_numeric_type ty))
  in
  "  " ^ instr_string
