open Aarch64_gen_context
open Asm
open Asm_builders
open Basic_collections
open Mir
open Mir_builders

let imm_0 = mk_imm ~imm:(Imm8 (Int8.of_int 0))

let imm_1 = mk_imm ~imm:(Imm8 (Int8.of_int 1))

let imm_16 = mk_imm ~imm:(Imm8 (Int8.of_int 16))

let imm_32 = mk_imm ~imm:(Imm8 (Int8.of_int 32))

let imm_48 = mk_imm ~imm:(Imm8 (Int8.of_int 48))

let mk_vreg = mk_virtual_register

let rec gen ~(gcx : Gcx.t) (ir : Program.t) =
  let should_filter_stdlib = Asm_gen.should_filter_stdlib () in

  (* Calculate initial function info for all functions *)
  Mir_builders.program_iter_funcs ir (fun func -> preprocess_function ~gcx ~ir func);

  (* Generate all functions in program *)
  SMap.iter
    (fun name func ->
      if not (should_filter_stdlib && has_std_lib_prefix name) then gen_function ~gcx ~ir func)
    ir.funcs

and preprocess_function ~gcx ~ir mir_func =
  let calling_convention = Gcx.mir_function_calling_convention mir_func in
  let param_mir_types = List.map (fun param -> type_of_value param) mir_func.params in
  let param_types = calling_convention#calculate_param_types param_mir_types in
  Gcx.add_function ~gcx ~ir mir_func param_types

and gen_function ~gcx ~ir mir_func =
  let func = Gcx.get_func_from_mir_func ~gcx mir_func in
  (* Create function prologue which copies all params from physical registers or stack slots to
     temporaries *)
  Gcx.start_function ~gcx func;
  Gcx.start_block ~gcx ~label:(Some func.label) ~func ~mir_block:None;
  func.prologue <- Option.get gcx.current_block;

  func.params <-
    List.mapi
      (fun i param ->
        let param_mir_type = type_of_value param in
        let arg_id = param.id in
        let { Argument.type_; _ } = cast_to_argument param in
        match func.param_types.(i) with
        | ParamOnStack _ -> mk_function_stack_argument ~arg_id ~type_:param_mir_type
        | ParamInRegister reg ->
          let size = register_size_of_mir_value_type type_ in
          let param_op = mk_virtual_register_of_value_id ~value_id:arg_id ~type_:param_mir_type in
          Gcx.emit ~gcx (`MovR size) [| mk_precolored_of_operand reg param_op; param_op |];
          param_op)
      mir_func.params;

  (* Jump to function start and gen function body *)
  Gcx.emit ~gcx `B [| block_op_of_mir_block ~gcx mir_func.start_block |];
  Gcx.finish_block ~gcx;
  gen_blocks ~gcx ~ir mir_func.start_block None func;
  Gcx.finish_function ~gcx

and gen_blocks ~gcx ~ir start_block label func =
  let ordered_blocks = Mir_graph_ordering.get_ordered_cfg start_block in
  List.iteri
    (fun i mir_block ->
      let label =
        if i = 0 then
          label
        else
          None
      in
      Gcx.start_block ~gcx ~label ~func ~mir_block:(Some mir_block);
      let instructions =
        fold_instructions mir_block [] (fun instr_val _ acc -> instr_val :: acc) |> List.rev
      in
      gen_instructions ~gcx ~ir ~block:mir_block instructions;
      Gcx.finish_block ~gcx)
    ordered_blocks

and gen_instructions ~gcx ~ir:_ ~block:_ instructions =
  match instructions with
  | [] -> ()
  (*
   * ===========================================
   *                   Ret
   * ===========================================
   *)
  | [{ value = Instr { instr = Ret value; _ }; _ }] ->
    (match value with
    | None -> ()
    | Some value ->
      let size = register_size_of_mir_use value in
      let calling_convention = (Option.get gcx.current_func).calling_convention in
      let return_reg = calling_convention#calculate_return_register (type_of_use value) in
      let return_reg_op = mk_precolored ~type_:(type_of_use value) return_reg in
      let value_vreg = gen_value ~gcx value in
      Gcx.emit ~gcx (`MovR size) [| return_reg_op; value_vreg |]);
    Gcx.emit ~gcx `Ret [||]
  (*
   * ===========================================
   *                Terminators
   * ===========================================
   *)
  | [{ value = Instr { instr = Unreachable; _ }; _ }] -> ()
  | [{ value = Instr { instr = Continue continue; _ }; _ }] ->
    Gcx.emit ~gcx `B [| block_op_of_mir_block ~gcx continue |]
  | { value = Instr { instr = Ret _ | Continue _ | Branch _ | Unreachable; _ }; _ } :: _ ->
    failwith "Terminator instructions must be last instruction"
  | { value = Instr { instr = Mir.Instruction.Phi _; _ }; _ } :: _ ->
    failwith "Phi nodes must be removed before asm gen"
  | { value = Instr { instr = Mir.Instruction.StackAlloc _; _ }; _ } :: _ ->
    failwith "StackAlloc instructions removed before asm gen"
  | { value = Instr _; _ } :: _ -> failwith "Unimplemented MIR instruction"
  | { value = Lit _ | Argument _; _ } :: _ -> failwith "Expected instruction value"

and block_op_of_mir_block ~gcx mir_block =
  mk_block_op ~block:(Gcx.get_block_from_mir_block ~gcx mir_block)

(* Generate an immediate value loaded to a register. Only 16 bits can be moved in a single
   instruction, if more are required then additional movk instructions must be used. *)
and gen_mov_immediate ~gcx (lit : Literal.t) : Operand.t =
  let type_ = type_of_literal lit in
  if is_zero_literal lit then
    mk_precolored ~type_ `ZR
  else
    let n = int64_of_literal lit in
    let bytes = Bytes.create 8 in
    Bytes.set_int64_le bytes 0 n;

    (* Extract an unsigned 16 bit chunk from `n` starting at the given byte offset *)
    let get_imm16_chunk bytes byte_offset =
      let imm16 = Bytes.get_uint16_le bytes byte_offset in
      mk_imm ~imm:(Imm16 imm16)
    in

    let register_size = register_size_of_mir_value_type type_ in
    let vreg = mk_vreg ~type_ in

    let is_greater_than x y = Int64.compare x y != -1 in
    let is_less_than x y = Int64.compare x y == -1 in

    (if is_greater_than n 0L then (
      (* First 16 bit chunk is always loaded with a movz *)
      Gcx.emit ~gcx (`MovI (register_size, Z)) [| vreg; get_imm16_chunk bytes 0; imm_0 |];

      (* n >= 2^16 *)
      if is_greater_than n 65536L then (
        Gcx.emit ~gcx (`MovI (register_size, K)) [| vreg; get_imm16_chunk bytes 2; imm_16 |];
        (* n >= 2^32 *)
        if is_greater_than n 4294967296L then (
          Gcx.emit ~gcx (`MovI (register_size, K)) [| vreg; get_imm16_chunk bytes 4; imm_32 |];
          (* n >= 2^48 *)
          if is_greater_than n 281474976710656L then
            Gcx.emit ~gcx (`MovI (register_size, K)) [| vreg; get_imm16_chunk bytes 6; imm_48 |]
        )
      )
    ) else
      (* First 16 bit chunk is always inverted and loaded with a movn to set high bytes to ones *)
      let inverted_bytes = Bytes.create 8 in
      Bytes.set_int64_le inverted_bytes 0 (Int64.lognot n);
      Gcx.emit ~gcx (`MovI (register_size, N)) [| vreg; get_imm16_chunk inverted_bytes 0; imm_0 |];

      (* n < -2^16 *)
      if is_less_than n (-65536L) then (
        Gcx.emit ~gcx (`MovI (register_size, K)) [| vreg; get_imm16_chunk bytes 2; imm_16 |];
        (* n < -2^32 *)
        if is_less_than n (-4294967296L) then (
          Gcx.emit ~gcx (`MovI (register_size, K)) [| vreg; get_imm16_chunk bytes 4; imm_32 |];
          (* n < -2^48 *)
          if is_less_than n (-281474976710656L) then
            Gcx.emit ~gcx (`MovI (register_size, K)) [| vreg; get_imm16_chunk bytes 6; imm_48 |]
        )
      ));

    vreg

and gen_value ~gcx (use : Use.t) =
  match use.value.value with
  | Lit ((Bool _ | Byte _ | Int _ | Long _) as lit) -> gen_mov_immediate ~gcx lit
  | Lit (Double _) -> failwith "TODO: Support floating point literals"
  | Lit (Function _)
  | Lit (Global _) ->
    failwith "TODO: Support functions and globals"
  | Lit (NullPointer _) -> mk_precolored ~type_:(Pointer Byte) `ZR
  | Lit (ArrayString _)
  | Lit (ArrayVtable _) ->
    failwith "TODO: Cannot compile array literals"
  | Lit (AggregateClosure _) -> failwith "TODO: Cannot compile aggregate literals"
  | Instr { type_; _ }
  | Argument { type_; _ } ->
    mk_virtual_register_of_value_id ~value_id:use.value.id ~type_

and register_size_of_mir_value_type value_type =
  match value_type with
  | Bool
  | Byte
  | Short
  | Int ->
    Size32
  | Long
  | Double
  | Function
  | Pointer _ ->
    Size64
  | Aggregate _ -> failwith "TODO: Cannot compile aggregate structure literals"
  | Array _ -> failwith "TODO: Cannot compile array literals"

and register_size_of_mir_use mir_use = register_size_of_mir_value_type (type_of_use mir_use)
