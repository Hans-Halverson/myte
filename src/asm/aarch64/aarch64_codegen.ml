open Aarch64_asm
open Aarch64_calling_convention
open Aarch64_gen_context
open Asm
open Asm_builders
open Asm_calling_convention
open Asm_instruction_definition.AArch64
open Basic_collections
open Mir
open Mir_builders

let imm_0 = mk_imm ~imm:(Imm8 (Int8.of_int 0))

let imm_1 = mk_imm ~imm:(Imm8 (Int8.of_int 1))

let imm_12 = mk_imm ~imm:(Imm8 (Int8.of_int 12))

let imm_16 = mk_imm ~imm:(Imm8 (Int8.of_int 16))

let imm_32 = mk_imm ~imm:(Imm8 (Int8.of_int 32))

let imm_48 = mk_imm ~imm:(Imm8 (Int8.of_int 48))

let mk_vreg = mk_virtual_register

let mk_vreg_of_value_id value_id = mk_virtual_register_of_value_id ~value_id

let mk_vreg_of_op (op : Operand.t) = mk_virtual_register ~type_:op.type_

let rec gen ~(gcx : Gcx.t) (ir : Program.t) =
  let should_filter_stdlib = Asm_gen.should_filter_stdlib () in

  (* Calculate initial function info for all functions *)
  Mir_builders.program_iter_funcs ir (fun func -> preprocess_function ~gcx ~ir func);

  (* Generate all functions in program *)
  SMap.iter
    (fun name func ->
      if not (should_filter_stdlib && has_std_lib_prefix name) then gen_function ~gcx func)
    ir.funcs

and preprocess_function ~gcx ~ir mir_func =
  let calling_convention = Gcx.mir_function_calling_convention mir_func in
  let param_mir_types = List.map (fun param -> type_of_value param) mir_func.params in
  let param_types = calling_convention#calculate_param_types param_mir_types in
  Gcx.add_function ~gcx ~ir mir_func param_types

and gen_function ~gcx mir_func =
  let func = Gcx.get_func_from_mir_func ~gcx mir_func in
  (* Create function prologue which copies all params from physical registers or stack slots to
     temporaries *)
  Gcx.start_function ~gcx func;
  Gcx.start_block ~gcx ~label:(Some func.label) ~func ~mir_block:None;
  func.prologue <- gcx.current_block;

  func.params <-
    List.mapi
      (fun i param ->
        let param_mir_type = type_of_value param in
        let arg_id = param.id in
        let { Argument.type_; _ } = cast_to_argument param in
        match func.param_types.(i) with
        | ParamOnStack index -> mk_function_stack_argument ~arg_id ~index ~type_:param_mir_type
        | ParamInRegister reg ->
          let size = register_size_of_mir_value_type type_ in
          let param_op = mk_virtual_register_of_value_id ~value_id:arg_id ~type_:param_mir_type in
          Gcx.emit ~gcx (`MovR size) [| param_op; mk_precolored_of_operand reg param_op |];
          param_op)
      mir_func.params;

  (* Jump to function start and gen function body *)
  Gcx.emit ~gcx `B [| block_op_of_mir_block ~gcx mir_func.start_block |];
  Gcx.finish_block ~gcx;
  gen_blocks ~gcx mir_func.start_block None func;
  Gcx.finish_function ~gcx

and gen_blocks ~gcx start_block label func =
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
      gen_instructions ~gcx instructions;
      Gcx.finish_block ~gcx)
    ordered_blocks

and gen_instructions ~gcx instructions =
  let gen_instructions = gen_instructions ~gcx in
  let gen_call_arguments param_types arg_vals =
    (* First move function arguments that are passed in stack slots *)
    List.iteri
      (fun i arg_val ->
        match param_types.(i) with
        | ParamInRegister _ -> ()
        | ParamOnStack stack_slot_idx ->
          let arg_type = type_of_use arg_val in
          let arg_stack_slot_op =
            mk_function_argument_stack_slot ~func:gcx.current_func ~i:stack_slot_idx ~type_:arg_type
          in
          let arg_size = register_size_of_mir_value_type arg_type in
          let arg_op = gen_value ~gcx arg_val in
          Gcx.emit ~gcx (`MovR arg_size) [| arg_stack_slot_op; arg_op |])
      arg_vals;
    (* Then move function arguments that are passed in registers​ *)
    List.iteri
      (fun i arg_val ->
        match param_types.(i) with
        | ParamOnStack _ -> ()
        | ParamInRegister _ when is_zero_size_global arg_val -> ()
        | ParamInRegister reg ->
          let arg_op = gen_value ~gcx arg_val in
          let arg_size = register_size_of_mir_use arg_val in
          let precolored_reg = mk_precolored ~type_:(type_of_use arg_val) reg in
          Gcx.emit ~gcx (`MovR arg_size) [| precolored_reg; arg_op |])
      arg_vals
  in
  match instructions with
  | [] -> ()
  (*
   * ===========================================
   *                   Mov
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Mov arg; type_; _ }; _ } :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let arg_op = gen_value ~gcx arg in
    Gcx.emit ~gcx (`MovR size) [| result_op; arg_op |];
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Call
   * ===========================================
   *)
  | {
      id = result_id;
      value =
        Instr { type_; instr = Call { func = Value func_val; args = arg_vals; has_return }; _ };
      _;
    }
    :: rest_instructions ->
    gcx.current_func.is_leaf <- false;

    let calling_convention =
      match func_val.value.value with
      | Lit (Function mir_func) -> Gcx.mir_function_calling_convention mir_func
      | _ ->
        (* TODO: Annotate calling convention on MIR call instructions and enforce single calling
           convention for all functions that flow to a single call instruction. *)
        aapcs64
    in

    (* Emit arguments for call *)
    let param_mir_types = List.map type_of_use arg_vals in
    let param_types = calling_convention#calculate_param_types param_mir_types in
    gen_call_arguments param_types arg_vals;

    (* Emit call instruction *)
    (match func_val.value.value with
    | Lit (Function mir_func) ->
      let func = Gcx.get_func_from_mir_func ~gcx mir_func in
      Gcx.emit ~gcx (`BL (param_types, calling_convention)) [| mk_function_op ~func |]
    | _ ->
      let func_op = gen_value ~gcx func_val in
      Gcx.emit ~gcx (`BLR (param_types, calling_convention)) [| func_op |]);
    (* Move result from return register to return operand *)
    (if has_return then
      let return_size = register_size_of_mir_value_type type_ in
      let return_reg = calling_convention#calculate_return_register type_ in
      let return_reg_op = mk_precolored ~type_ return_reg in
      let result_op = mk_vreg_of_value_id ~type_ result_id in
      Gcx.emit ~gcx (`MovR return_size) [| result_op; return_reg_op |]);
    gen_instructions rest_instructions
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
      let calling_convention = gcx.current_func.calling_convention in
      let return_reg = calling_convention#calculate_return_register (type_of_use value) in
      let return_reg_op = mk_precolored ~type_:(type_of_use value) return_reg in
      let value_vreg = gen_value ~gcx value in
      Gcx.emit ~gcx (`MovR size) [| return_reg_op; value_vreg |]);
    Gcx.emit ~gcx `Ret [||]
  (*
   * ===========================================
   *              Cmp then Branch
   * ===========================================
   *)
  | [
   { id = result_id; value = Instr { instr = Cmp (cmp, left_val, right_val); _ }; _ };
   { value = Instr { instr = Branch { test; continue; jump }; _ }; _ };
  ]
    when result_id == test.value.id ->
    let cond = cond_of_mir_comparison cmp in
    let cond =
      (* If the only use of the comparison is in this branch instruction, only need to generate
         a comparison instruction and use the current flags. *)
      if value_has_single_use left_val.user then
        let swapped = gen_cmp ~gcx left_val right_val in
        if swapped then
          swap_cond_order cond
        else
          cond
      (* Otherwise the result of the comparison is used elsewhere, so we must load to a register
         with a CSet instruction. We can still emit a BCond directly off the current flags though. *)
      else
        gen_cmp_cset ~gcx cond result_id left_val right_val
    in
    (* Note that the condition code is inverted as we emit a BCond to the false branch *)
    let cond = invert_cond cond in
    Gcx.emit ~gcx (`BCond cond) [| block_op_of_mir_block ~gcx jump |];
    Gcx.emit ~gcx `B [| block_op_of_mir_block ~gcx continue |]
  (*
   * ===========================================
   *                    Cmp
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Cmp (cmp, left_val, right_val); _ }; _ }
    :: rest_instructions ->
    let cond = cond_of_mir_comparison cmp in
    ignore (gen_cmp_cset ~gcx cond result_id left_val right_val);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Terminators
   * ===========================================
   *)
  | [{ value = Instr { instr = Unreachable; _ }; _ }] -> ()
  | [{ value = Instr { instr = Continue continue; _ }; _ }] ->
    Gcx.emit ~gcx `B [| block_op_of_mir_block ~gcx continue |]
  | [{ value = Instr { instr = Branch { test; continue; jump }; _ }; _ }] ->
    let test =
      match test.value.value with
      | Lit _ -> failwith "Dead branch pruning must have already occurred"
      | _ -> test
    in
    let test_op = gen_value ~gcx test in
    Gcx.emit ~gcx (`Cbz Size32) [| test_op; block_op_of_mir_block ~gcx jump |];
    Gcx.emit ~gcx `B [| block_op_of_mir_block ~gcx continue |]
  | { value = Instr { instr = Ret _ | Continue _ | Branch _ | Unreachable; _ }; _ } :: _ ->
    failwith "Terminator instructions must be last instruction"
  (*
   * ===========================================
   *                   Add
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Add, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    (match (gen_add_sub_value ~gcx left_val, gen_add_sub_value ~gcx right_val) with
    | ({ value = Immediate _; _ }, { value = Immediate _; _ }) ->
      failwith "Constants must be folded before codegen"
    | ({ value = Immediate imm; _ }, reg_op)
    | (reg_op, { value = Immediate imm; _ }) ->
      let n = int64_of_immediate imm in
      if Integers.int64_less_than n 0L then
        gen_add_sub_i ~gcx (`SubI size) result_op reg_op (Int64.neg n)
      else
        gen_add_sub_i ~gcx (`AddI size) result_op reg_op n
    | (reg_op1, reg_op2) -> Gcx.emit ~gcx (`AddR size) [| result_op; reg_op1; reg_op2 |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Sub
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Sub, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let left_op = gen_value ~gcx left_val in
    (match gen_add_sub_value ~gcx right_val with
    | { value = Immediate imm; _ } ->
      let n = int64_of_immediate imm in
      if Integers.int64_less_than n 0L then
        gen_add_sub_i ~gcx (`AddI size) result_op left_op (Int64.neg n)
      else
        gen_add_sub_i ~gcx (`SubI size) result_op left_op n
    | right_op -> Gcx.emit ~gcx (`SubR size) [| result_op; left_op; right_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Mul
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Mul, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let left_op = gen_value ~gcx left_val in
    let right_op = gen_value ~gcx right_val in
    Gcx.emit ~gcx (`Mul size) [| result_op; left_op; right_op |];
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Div
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Div, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    ignore (gen_sdiv ~gcx ~type_ ~result_op ~left_val ~right_val);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Rem
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Rem, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let div_result_op = mk_vreg ~type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let (left_op, right_op) = gen_sdiv ~gcx ~type_ ~result_op:div_result_op ~left_val ~right_val in
    Gcx.emit ~gcx (`MSub size) [| result_op; div_result_op; right_op; left_op |];
    gen_instructions rest_instructions
    (*
   * ===========================================
   *                   Neg
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Unary (Neg, arg); type_; _ }; _ } :: rest_instructions
    ->
    let size = register_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let arg_op = gen_value ~gcx arg in
    Gcx.emit ~gcx (`Neg size) [| result_op; arg_op |];
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Not
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Unary (Not, arg); type_; _ }; _ } :: rest_instructions
    ->
    let size = register_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let arg_op = gen_value ~gcx arg in
    if is_bool_value arg.value then (
      Gcx.emit ~gcx (`CmpI size) [| arg_op; imm_0 |];
      Gcx.emit ~gcx (`CSet (size, EQ)) [| result_op |]
    ) else
      Gcx.emit ~gcx (`Mvn size) [| result_op; arg_op |];
    gen_instructions rest_instructions
  | { value = Instr { instr = Mir.Instruction.Phi _; _ }; _ } :: _ ->
    failwith "Phi nodes must be removed before asm gen"
  | { value = Instr { instr = Mir.Instruction.StackAlloc _; _ }; _ } :: _ ->
    failwith "StackAlloc instructions removed before asm gen"
  | {
      value =
        Instr
          {
            instr =
              ( Binary ((And | Or | Xor | Shl | Shr | Shrl), _, _)
              | Call { func = MirBuiltin _; _ }
              | GetPointer _ | Load _ | Store _ | Cast _ | Trunc _ | SExt _ | ZExt _ | IntToFloat _
              | FloatToInt _ );
            _;
          };
      _;
    }
    :: _ ->
    failwith "Unimplemented MIR instruction"
  | { value = Lit _ | Argument _; _ } :: _ -> failwith "Expected instruction value"

(* Generate a cmp instruction between two arguments. Return whether order was swapped.
   All registers must be sign extended before comparison. *)
and gen_cmp ~gcx left_val right_val =
  let mir_type = type_of_use left_val in

  let (left_op, left_is_sext) = gen_cmp_value ~gcx left_val in
  let (right_op, right_is_sext) = gen_cmp_value ~gcx right_val in

  match (left_op.value, right_op.value) with
  | (Immediate _, Immediate _) -> failwith "Constants must be folded before codegen"
  (* Comparison to immediate - swap operands if necessary *)
  | (_, Immediate _) ->
    gen_cmp_i ~gcx left_op right_op mir_type;
    false
  | (Immediate _, _) ->
    gen_cmp_i ~gcx right_op left_op mir_type;
    true
  (* Comparison of registers - swap operands if only one needs sign extension *)
  | (_, _) ->
    let size = register_size_of_mir_value_type mir_type in
    let extend =
      match subregister_size_of_mir_value_type mir_type with
      | B -> SXTB
      | H -> SXTH
      | W
      | X ->
        SXTX
    in
    if (not left_is_sext) && right_is_sext then (
      Gcx.emit ~gcx (`CmpR (size, extend)) [| right_op; left_op |];
      true
    ) else
      let left_sext_op =
        if left_is_sext then
          left_op
        else
          gen_sign_extended_op ~gcx ~type_:mir_type left_op
      in
      Gcx.emit ~gcx (`CmpR (size, extend)) [| left_sext_op; right_op |];
      false

and gen_cmp_i ~gcx reg_op imm_op mir_type =
  let size = register_size_of_mir_value_type mir_type in
  let n = int64_of_immediate (cast_to_immediate imm_op) in
  let (instr, imm_op) =
    if Integers.int64_less_than n 0L then
      (`CmnI size, mk_imm ~imm:(Imm64 (Int64.neg n)))
    else
      (`CmpI size, imm_op)
  in
  let reg_sext_op = gen_sign_extended_op ~gcx ~type_:mir_type reg_op in
  Gcx.emit ~gcx instr [| reg_sext_op; imm_op |]

(* Generate an operand that can be used in cmp instructions. 12-bit immediates are allowed, larger
   immediates must be loaded to a register. Return operand and whether it is sign extended. *)
and gen_cmp_value ~gcx (use : Use.t) : Operand.t * bool =
  match use.value.value with
  | Lit ((Bool _ | Byte _ | Int _ | Long _) as lit) ->
    let n = int64_of_literal lit in
    (* -2^12 < n < 2^12 *)
    let op =
      if Integers.int64_less_than (-4096L) n && Integers.int64_less_than n 4096L then
        mk_imm ~imm:(Imm64 n)
      else
        gen_mov_immediate ~gcx lit
    in
    (* Loaded immediates are always sign extended *)
    (op, true)
  | _ ->
    let subregister_size = subregister_size_of_mir_value_type (type_of_use use) in
    let is_sext =
      match subregister_size with
      | B
      | H ->
        false
      | W
      | X ->
        true
    in
    (gen_value ~gcx use, is_sext)

(* Generate a comparison and CSet instruction to load the result of the comparison to a register.
   Return the condition that was used in the cmp (may be different than the input condition code, as
   order of arguments may have been swapped). *)
and gen_cmp_cset ~gcx cond result_id left_val right_val =
  let result_op = mk_vreg_of_value_id ~type_:Bool result_id in
  let swapped = gen_cmp ~gcx left_val right_val in
  let cond =
    if swapped then
      swap_cond_order cond
    else
      cond
  in
  Gcx.emit ~gcx (`CSet (Size32, cond)) [| result_op |];
  cond

and gen_add_sub_i ~gcx instr dest_op reg_op n =
  (* n < 2^12 fits in a single add or sub instruction *)
  if Integers.int64_less_than n 4096L then
    Gcx.emit ~gcx instr [| dest_op; reg_op; mk_imm ~imm:(Imm64 n); imm_0 |]
  else
    (* n >= 2^12 is split into two add or sub instructions for low and high 12-bit chunks *)
    let low_12_bits = Int64.logand n 4095L in
    let high_12_bits = Int64.shift_right n 12 |> Int64.logand 4095L in
    Gcx.emit ~gcx instr [| dest_op; reg_op; mk_imm ~imm:(Imm64 high_12_bits); imm_12 |];
    Gcx.emit ~gcx instr [| dest_op; reg_op; mk_imm ~imm:(Imm64 low_12_bits); imm_0 |]

(* Generate an operand that can be used in add or sub instructions. 24-bit immediates are allowed,
   but larger immediates must be loaded to a register. *)
and gen_add_sub_value ~gcx (use : Use.t) : Operand.t =
  match use.value.value with
  | Lit ((Bool _ | Byte _ | Int _ | Long _) as lit) ->
    let n = int64_of_literal lit in
    (* -2^24 < n < 2^24 can be encoded as immediate in 1 or 2 add/sub instructions *)
    if Integers.int64_less_than (-16777216L) n && Integers.int64_less_than n 16777216L then
      mk_imm ~imm:(Imm64 n)
    else
      gen_mov_immediate ~gcx lit
  | _ -> gen_value ~gcx use

and gen_sign_extended_op ~gcx ~type_ op =
  let subregister_size = subregister_size_of_mir_value_type type_ in
  match subregister_size with
  | B
  | H ->
    let size = register_size_of_mir_value_type type_ in
    let sext_op = mk_vreg_of_op op in
    Gcx.emit ~gcx (`Sxt (size, subregister_size)) [| sext_op; op |];
    sext_op
  | W
  | X ->
    op

and gen_sdiv ~gcx ~type_ ~result_op ~left_val ~right_val =
  let size = register_size_of_mir_value_type type_ in
  let subregister_size = subregister_size_of_mir_value_type type_ in
  (* Subregister immediates are always sign extended to register size when loaded. Non-immediate
     operands must be explictly sign extended to register size. *)
  let gen_subregister_value use =
    let op = gen_value ~gcx use in
    match use.Use.value.value with
    | Lit _ -> op
    | _ ->
      let sext_op = mk_vreg_of_op op in
      Gcx.emit ~gcx (`Sxt (size, subregister_size)) [| sext_op; op |];
      sext_op
  in
  let (left_op, right_op) =
    match subregister_size with
    | B
    | H ->
      let left_op = gen_subregister_value left_val in
      let right_op = gen_subregister_value right_val in
      (left_op, right_op)
    | W
    | X ->
      let left_op = gen_value ~gcx left_val in
      let right_op = gen_value ~gcx right_val in
      (left_op, right_op)
  in
  Gcx.emit ~gcx (`SDiv size) [| result_op; left_op; right_op |];
  (left_op, right_op)

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
      if Integers.int64_less_than n (-65536L) then (
        Gcx.emit ~gcx (`MovI (register_size, K)) [| vreg; get_imm16_chunk bytes 2; imm_16 |];
        (* n < -2^32 *)
        if Integers.int64_less_than n (-4294967296L) then (
          Gcx.emit ~gcx (`MovI (register_size, K)) [| vreg; get_imm16_chunk bytes 4; imm_32 |];
          (* n < -2^48 *)
          if Integers.int64_less_than n (-281474976710656L) then
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

and subregister_size_of_mir_value_type value_type =
  match value_type with
  | Bool
  | Byte ->
    B
  | Short -> H
  | Int -> W
  | Long
  | Double
  | Function
  | Pointer _ ->
    X
  | Aggregate _ -> failwith "TODO: Cannot compile aggregate structure literals"
  | Array _ -> failwith "TODO: Cannot compile array literals"

and cond_of_mir_comparison cmp =
  match cmp with
  | Mir.Instruction.Eq -> EQ
  | Neq -> NE
  | Lt -> LT
  | LtEq -> LE
  | Gt -> GT
  | GtEq -> GE
