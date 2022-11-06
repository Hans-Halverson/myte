open Aarch64_asm
open Aarch64_calling_convention
open Aarch64_gen_context
open Asm
open Asm_builders
open Asm_calling_convention
open Asm_codegen
open Asm_instruction_definition.AArch64
open Asm_layout
open Basic_collections
open Mir
open Mir_builders
open Mir_type

type get_pointer_offset =
  | ImmediateOffset of Int64.t
  | LabelOffset of Operand.t

let imm_0 = mk_imm ~imm:(Imm8 (Int8.of_int 0))

let imm_1 = mk_imm ~imm:(Imm8 (Int8.of_int 1))

let imm_2 = mk_imm ~imm:(Imm8 (Int8.of_int 2))

let imm_3 = mk_imm ~imm:(Imm8 (Int8.of_int 3))

let imm_12 = mk_imm ~imm:(Imm8 (Int8.of_int 12))

let imm_16 = mk_imm ~imm:(Imm8 (Int8.of_int 16))

let imm_32 = mk_imm ~imm:(Imm8 (Int8.of_int 32))

let imm_48 = mk_imm ~imm:(Imm8 (Int8.of_int 48))

let imm_byte_mask = mk_imm64 ~n:255L

let imm_half_word_mask = mk_imm64 ~n:65535L

let imm_word_mask = mk_imm64 ~n:4294967295L

let mk_vreg = mk_virtual_register

let mk_vreg_of_value_id value_id = mk_virtual_register_of_value_id ~value_id

let mk_vreg_of_op (op : Operand.t) = mk_virtual_register ~type_:op.type_

let rec gen ~(gcx : Gcx.t) (ir : Program.t) =
  let should_filter_stdlib = Asm_gen.should_filter_stdlib () in

  (* Calculate layout of all aggregate types *)
  SMap.iter (fun _ agg -> add_agg_layout ~agg_cache:gcx.agg_cache agg) ir.types;

  (* Calculate initial function info for all functions *)
  Mir_builders.program_iter_funcs ir (fun func -> preprocess_function ~gcx ~ir func);

  (* Generate all globals in program *)
  gen_globals ~gcx ~ir;

  (* Generate all functions in program *)
  SMap.iter
    (fun name func ->
      if not (should_filter_stdlib && has_std_lib_prefix name) then gen_function ~gcx func)
    ir.funcs;

  Gcx.finish_builders ~gcx

and gen_globals ~gcx ~ir =
  let should_filter_stdlib = Asm_gen.should_filter_stdlib () in
  let builder = new globals_builder ~agg_cache:gcx.agg_cache in

  SMap.iter
    (fun name global ->
      if not (should_filter_stdlib && has_std_lib_prefix name) then builder#gen_global global)
    ir.globals;

  gcx.data <- builder#data;
  gcx.bss <- builder#bss

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
          let param_op = mk_virtual_register_of_value_id ~value_id:arg_id ~type_:param_mir_type in
          gen_mov ~gcx ~type_ ~dest:param_op ~src:(mk_precolored_of_operand reg param_op);
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
          let arg_op = gen_value ~gcx arg_val in
          gen_mov ~gcx ~type_:arg_type ~dest:arg_stack_slot_op ~src:arg_op)
      arg_vals;
    (* Then move function arguments that are passed in registers​ *)
    List.iteri
      (fun i arg_val ->
        match param_types.(i) with
        | ParamOnStack _ -> ()
        | ParamInRegister _ when is_zero_size_global arg_val -> ()
        | ParamInRegister reg ->
          let arg_op = gen_value ~gcx arg_val in
          let arg_type = type_of_use arg_val in
          let precolored_reg = mk_precolored ~type_:arg_type reg in
          gen_mov ~gcx ~type_:arg_type ~dest:precolored_reg ~src:arg_op)
      arg_vals
  in
  match instructions with
  | [] -> ()
  (*
   * ===========================================
   *    Copy Instructions (Mov, Cast, Trunc)
   * ===========================================
   *)
  | {
      id = result_id;
      value = Instr { instr = Mov arg_val | Cast arg_val | Trunc arg_val; type_; _ };
      _;
    }
    :: rest_instructions ->
    gen_mov_to_id ~gcx ~type_ result_id arg_val;
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
      let return_reg = calling_convention#calculate_return_register type_ in
      let return_reg_op = mk_precolored ~type_ return_reg in
      let result_op = mk_vreg_of_value_id ~type_ result_id in
      gen_mov ~gcx ~type_ ~dest:result_op ~src:return_reg_op);
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
      let calling_convention = gcx.current_func.calling_convention in
      let return_reg_type = type_of_use value in
      let return_reg = calling_convention#calculate_return_register return_reg_type in
      let return_reg_op = mk_precolored ~type_:return_reg_type return_reg in
      let value_vreg = gen_value ~gcx value in
      gen_mov ~gcx ~type_:return_reg_type ~dest:return_reg_op ~src:value_vreg);
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
    let is_float_cmp = type_of_use left_val == Double in
    let cond =
      (* If the only use of the comparison is in this branch instruction, only need to generate
         a comparison instruction and use the current flags. *)
      if value_has_single_use left_val.user then
        if is_float_cmp then
          gen_float_cmp ~gcx ~cmp left_val right_val
        else
          gen_int_cmp ~gcx ~cmp left_val right_val
      else
        (* Otherwise the result of the comparison is used elsewhere, so we must load to a register
           with a CSet instruction. We can still emit a BCond directly off the current flags though. *)
        gen_cmp_cset ~gcx cmp result_id left_val right_val
    in
    if is_float_cmp then (
      (* Float conditions are not inverted *)
      Gcx.emit ~gcx (`BCond cond) [| block_op_of_mir_block ~gcx continue |];
      Gcx.emit ~gcx `B [| block_op_of_mir_block ~gcx jump |]
    ) else
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
    ignore (gen_cmp_cset ~gcx cmp result_id left_val right_val);
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
   *                GetPointer
   * ===========================================
   *)
  (* | ({ value = Instr { instr = GetPointer gp_inner_instr; _ }; _ } as gp_instr)
       :: { value = Instr { instr = Load { value = load_ptr_value; _ }; _ }; _ }
       :: rest_instructions
       when load_ptr_value == gp_instr ->
       gen_get_pointer ~gcx gp_instr gp_inner_instr;
       gen_instructions rest_instructions
     | ({ value = Instr { instr = GetPointer gp_inner_instr; _ }; _ } as gp_instr)
       :: { value = Instr { instr = Store ({ value = store_ptr_value; _ }, store_arg_val); _ }; _ }
       :: rest_instructions
       when store_ptr_value == gp_instr ->
       gen_get_pointer ~gcx gp_instr gp_inner_instr;
       gen_instructions rest_instructions *)
  | { id = result_id; value = Instr { instr = GetPointer gp_inner_instr; type_; _ }; _ }
    :: rest_instructions ->
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let gp_op = gen_get_pointer ~gcx gp_inner_instr in
    gen_mov ~gcx ~type_ ~dest:result_op ~src:gp_op;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Load
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Load pointer; _ }; _ } :: rest_instructions ->
    let type_ = pointer_value_element_type pointer.value in
    let size = register_size_of_mir_value_type type_ in
    let subregister_size = subregister_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    (match pointer.value.value with
    | Lit (Function { name; _ })
    | Lit (Global { name; _ }) ->
      let (high_bits_reg, low_bits_offset) =
        gen_adrp_and_low_bits ~gcx ~type_:(type_of_use pointer) name
      in
      let ldr = `LdrI (size, subregister_size, true, Offset) in
      Gcx.emit ~gcx ldr [| result_op; high_bits_reg; low_bits_offset |]
    | _ ->
      let pointer_op = gen_value ~gcx ~allow_zr:false pointer in
      Gcx.emit ~gcx (`LdrR (size, subregister_size, true, LSL)) [| result_op; pointer_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Store
   * ===========================================
   *)
  | { value = Instr { instr = Store (pointer, arg); _ }; _ } :: rest_instructions ->
    let type_ = pointer_value_element_type pointer.value in
    let subregister_size = subregister_size_of_mir_value_type type_ in
    let arg_op = gen_value ~gcx arg in
    (match pointer.value.value with
    | Lit (Function { name; _ })
    | Lit (Global { name; _ }) ->
      let (high_bits_reg, low_bits_offset) =
        gen_adrp_and_low_bits ~gcx ~type_:(type_of_use pointer) name
      in
      let str = `StrI (subregister_size, Offset) in
      Gcx.emit ~gcx str [| arg_op; high_bits_reg; low_bits_offset |]
    | _ ->
      let pointer_op = gen_value ~gcx ~allow_zr:false pointer in
      Gcx.emit ~gcx (`StrR (subregister_size, LSL)) [| arg_op; pointer_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Add
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Add, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    (if type_ == Double then
      let left_op = gen_value ~gcx ~allow_zr:false left_val in
      let right_op = gen_value ~gcx ~allow_zr:false right_val in
      Gcx.emit ~gcx `FAdd [| result_op; left_op; right_op |]
    else
      match (left_val, right_val) with
      | ({ value = { value = Lit lit; _ }; _ }, other_val)
      | (other_val, { value = { value = Lit lit; _ }; _ }) ->
        let reg_op = gen_value ~gcx ~allow_zr:false other_val in
        gen_add_n ~gcx ~result_op ~type_ reg_op (int64_of_literal lit)
      | _ ->
        let left_op = gen_value ~gcx ~allow_zr:false left_val in
        let right_op = gen_value ~gcx ~allow_zr:false right_val in
        Gcx.emit ~gcx (`AddR (size, noop_extend)) [| result_op; left_op; right_op |]);
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
    let left_op = gen_value ~gcx ~allow_zr:false left_val in
    (if type_ == Double then
      let right_op = gen_value ~gcx right_val in
      Gcx.emit ~gcx `FSub [| result_op; left_op; right_op |]
    else
      match right_val.value.value with
      | Lit lit -> gen_sub_n ~gcx ~result_op ~type_ left_op (int64_of_literal lit)
      | _ ->
        let right_op = gen_value ~gcx ~allow_zr:false right_val in
        Gcx.emit ~gcx (`SubR size) [| result_op; left_op; right_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Mul
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Mul, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    (if type_ == Double then
      let left_op = gen_value ~gcx left_val in
      let right_op = gen_value ~gcx right_val in
      Gcx.emit ~gcx `FMul [| result_op; left_op; right_op |]
    else
      match (left_val, right_val) with
      | ({ value = { value = Lit lit; _ }; _ }, other_val)
      | (other_val, { value = { value = Lit lit; _ }; _ }) ->
        let n = int64_of_literal lit in
        let reg_op = gen_value ~gcx other_val in
        gen_mul_i ~gcx ~type_ ~result_op reg_op n
      | _ ->
        let size = register_size_of_mir_value_type type_ in
        let left_op = gen_value ~gcx left_val in
        let right_op = gen_value ~gcx right_val in
        Gcx.emit ~gcx (`Mul size) [| result_op; left_op; right_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Div
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Div, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    if type_ == Double then
      let left_op = gen_value ~gcx left_val in
      let right_op = gen_value ~gcx right_val in
      Gcx.emit ~gcx `FDiv [| result_op; left_op; right_op |]
    else
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
    if type_ == Double then
      Gcx.emit ~gcx `FNeg [| result_op; arg_op |]
    else
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
    (if is_bool_value arg.value then (
      let arg_op = gen_value ~gcx ~allow_zr:false arg in
      Gcx.emit ~gcx (`CmpI size) [| arg_op; imm_0 |];
      Gcx.emit ~gcx (`CSet (size, EQ)) [| result_op |]
    ) else
      let arg_op = gen_value ~gcx arg in
      Gcx.emit ~gcx (`Mvn size) [| result_op; arg_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    And
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (And, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let left_op = gen_bitmask_value ~gcx left_val in
    let right_op = gen_bitmask_value ~gcx right_val in
    (match (left_op, right_op) with
    | ({ Operand.value = Immediate _; _ }, { value = Immediate _; _ }) ->
      failwith "Constants must be folded before codegen"
    | (({ value = Immediate _; _ } as imm_op), reg_op)
    | (reg_op, ({ value = Immediate _; _ } as imm_op)) ->
      Gcx.emit ~gcx (`AndI size) [| result_op; reg_op; imm_op |]
    | _ -> Gcx.emit ~gcx (`AndR size) [| result_op; left_op; right_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Or
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Or, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let left_op = gen_bitmask_value ~gcx left_val in
    let right_op = gen_bitmask_value ~gcx right_val in
    (match (left_op, right_op) with
    | ({ value = Immediate _; _ }, { value = Immediate _; _ }) ->
      failwith "Constants must be folded before codegen"
    | (({ value = Immediate _; _ } as imm_op), reg_op)
    | (reg_op, ({ value = Immediate _; _ } as imm_op)) ->
      Gcx.emit ~gcx (`OrrI size) [| result_op; reg_op; imm_op |]
    | _ -> Gcx.emit ~gcx (`OrrR size) [| result_op; left_op; right_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Xor
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Xor, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let left_op = gen_bitmask_value ~gcx left_val in
    let right_op = gen_bitmask_value ~gcx right_val in
    (match (left_op, right_op) with
    | ({ value = Immediate _; _ }, { value = Immediate _; _ }) ->
      failwith "Constants must be folded before codegen"
    | (({ value = Immediate _; _ } as imm_op), reg_op)
    | (reg_op, ({ value = Immediate _; _ } as imm_op)) ->
      Gcx.emit ~gcx (`EorI size) [| result_op; reg_op; imm_op |]
    | _ -> Gcx.emit ~gcx (`EorR size) [| result_op; left_op; right_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  Shl
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Shl, target_val, shift_val); type_; _ }; _ }
    :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let target_op = gen_value ~gcx target_val in
    let shift_op = gen_shift_value ~gcx ~size shift_val in
    let instr =
      match shift_op.Operand.value with
      | Immediate _ -> `LslI size
      | _ -> `LslR size
    in
    Gcx.emit ~gcx instr [| result_op; target_op; shift_op |];
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  Shr
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Shr, target_val, shift_val); type_; _ }; _ }
    :: rest_instructions ->
    (* Arithmetic right shift is a no-op on bools and cannot be represented by shr instruction *)
    (if is_bool_value target_val.value then
      gen_mov_to_id ~gcx ~type_ result_id target_val
    else
      let size = register_size_of_mir_value_type type_ in
      let result_op = mk_vreg_of_value_id ~type_ result_id in
      let shift_op = gen_shift_value ~gcx ~size shift_val in
      match shift_op.Operand.value with
      | Immediate imm ->
        let subregister_size = subregister_size_of_mir_value_type type_ in
        (match subregister_size with
        (* Right shift by a known constant is modeled as a signed bitfield extract that extracts
           just the bits in the shifted subregister range. *)
        | B ->
          let n = int64_of_immediate imm in
          let num_bits_remaining = mk_imm64 ~n:(Int64.sub 8L n) in
          let target_op = gen_value ~gcx target_val in
          Gcx.emit ~gcx (`Sbfx size) [| result_op; target_op; shift_op; num_bits_remaining |]
        | H ->
          let n = int64_of_immediate imm in
          let num_bits_remaining = mk_imm64 ~n:(Int64.sub 16L n) in
          let target_op = gen_value ~gcx target_val in
          Gcx.emit ~gcx (`Sbfx size) [| result_op; target_op; shift_op; num_bits_remaining |]
        | W
        | X ->
          let target_op = gen_value ~gcx target_val in
          Gcx.emit ~gcx (`AsrI size) [| result_op; target_op; shift_op |])
      | _ ->
        let target_op = gen_sign_extended_value ~gcx target_val in
        Gcx.emit ~gcx (`AsrR size) [| result_op; target_op; shift_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  Shrl
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Shrl, target_val, shift_val); type_; _ }; _ }
    :: rest_instructions ->
    (let size = register_size_of_mir_value_type type_ in
     let result_op = mk_vreg_of_value_id ~type_ result_id in
     let shift_op = gen_shift_value ~gcx ~size shift_val in
     match shift_op.Operand.value with
     | Immediate imm ->
       let subregister_size = subregister_size_of_mir_value_type type_ in
       (match subregister_size with
       (* Right shift by a known constant is modeled as an unsigned bitfield extract that extracts
          just the bits in the shifted subregister range. *)
       | B ->
         let n = int64_of_immediate imm in
         let num_bits_remaining = mk_imm64 ~n:(Int64.sub 8L n) in
         let target_op = gen_value ~gcx target_val in
         Gcx.emit ~gcx (`Ubfx size) [| result_op; target_op; shift_op; num_bits_remaining |]
       | H ->
         let n = int64_of_immediate imm in
         let num_bits_remaining = mk_imm64 ~n:(Int64.sub 16L n) in
         let target_op = gen_value ~gcx target_val in
         Gcx.emit ~gcx (`Ubfx size) [| result_op; target_op; shift_op; num_bits_remaining |]
       | W
       | X ->
         let target_op = gen_value ~gcx target_val in
         Gcx.emit ~gcx (`LsrR size) [| result_op; target_op; shift_op |])
     | _ ->
       let target_op = gen_zero_extended_value ~gcx target_val in
       Gcx.emit ~gcx (`LsrI size) [| result_op; target_op; shift_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  SExt
   * ===========================================
   *)
  | { id = result_id; value = Instr { type_; instr = SExt arg_val; _ }; _ } :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let subregister_size = subregister_size_of_mir_value_type (type_of_use arg_val) in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let arg_op = gen_value ~gcx arg_val in
    Gcx.emit ~gcx (`Sxt (size, subregister_size)) [| result_op; arg_op |];
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  ZExt
   * ===========================================
   *)
  | { id = result_id; value = Instr { type_; instr = ZExt arg_val; _ }; _ } :: rest_instructions ->
    let size = register_size_of_mir_value_type type_ in
    let subregister_size = subregister_size_of_mir_value_type (type_of_use arg_val) in
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let arg_op = gen_value ~gcx arg_val in
    (match subregister_size with
    | B -> Gcx.emit ~gcx (`AndI size) [| result_op; arg_op; imm_byte_mask |]
    | H -> Gcx.emit ~gcx (`AndI size) [| result_op; arg_op; imm_half_word_mask |]
    | W -> Gcx.emit ~gcx (`AndI size) [| result_op; arg_op; imm_word_mask |]
    | X -> gen_mov ~gcx ~type_ ~dest:result_op ~src:arg_op);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Call Builtin
   * ===========================================
   *)
  | {
      id = result_id;
      value =
        Instr { type_ = return_type; instr = Call { func = MirBuiltin mir_builtin; args; _ }; _ };
      _;
    }
    :: rest_instructions ->
    let open Mir_builtin in
    gcx.current_func.is_leaf <- false;
    let builtin_func = Aarch64_builtin.get_asm_builtin mir_builtin in
    let calling_convention = builtin_func.calling_convention in

    (* Emit arguments for call *)
    let param_types =
      if mir_builtin.name = myte_alloc.name then (
        (* Special case myte_alloc as it must calculate the size to allocation from the type *)
        let element_mir_ty = cast_to_pointer_type return_type in
        let param_mir_types = [Type.Int] in
        let param_types = calling_convention#calculate_param_types param_mir_types in
        gen_size_from_count_and_type
          ~gcx
          (List.hd args)
          param_types.(0)
          (List.hd param_mir_types)
          element_mir_ty;
        param_types
      ) else if mir_builtin.name = myte_copy.name then (
        (* Special case myte_copy as it must calculate the size to copy from the type *)
        let element_mir_ty = cast_to_pointer_type (type_of_use (List.hd args)) in
        let (pointer_args, count_arg) = List_utils.split_last args in
        let count_mir_type = Type.Int in
        let param_mir_types = List.map type_of_use pointer_args @ [count_mir_type] in
        let param_types = calling_convention#calculate_param_types param_mir_types in
        gen_call_arguments param_types pointer_args;
        gen_size_from_count_and_type ~gcx count_arg param_types.(2) count_mir_type element_mir_ty;
        param_types
      ) else
        (* Generic case for all other builtins *)
        let param_mir_types = List.map type_of_use args in
        let param_types = calling_convention#calculate_param_types param_mir_types in
        gen_call_arguments param_types args;
        param_types
    in

    (* Call builtin function *)
    Gcx.emit ~gcx (`BL (param_types, calling_convention)) [| mk_function_op ~func:builtin_func |];

    (* Move return value to result register *)
    (match builtin_func.return_type with
    | None -> ()
    | Some return_mir_type ->
      let return_reg = calling_convention#calculate_return_register return_mir_type in
      let return_reg_op = mk_precolored ~type_:return_type return_reg in
      let return_op = mk_vreg_of_value_id ~type_:return_type result_id in
      gen_mov ~gcx ~type_:return_mir_type ~dest:return_op ~src:return_reg_op);

    gen_instructions rest_instructions
  (*
   * ===========================================
   *                IntToFloat
   * ===========================================
   *)
  | { id = result_id; value = Instr { type_; instr = IntToFloat arg_val; _ }; _ }
    :: rest_instructions ->
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let int_reg_size = register_size_of_mir_use arg_val in
    let arg_op = gen_sign_extended_value ~gcx arg_val in
    Gcx.emit ~gcx (`SCvtF int_reg_size) [| result_op; arg_op |];
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                FloatToInt
   * ===========================================
   *)
  | { id = result_id; value = Instr { type_; instr = FloatToInt arg_val; _ }; _ }
    :: rest_instructions ->
    let result_op = mk_vreg_of_value_id ~type_ result_id in
    let int_reg_size = register_size_of_mir_value_type type_ in
    let arg_op = gen_value ~gcx arg_val in
    Gcx.emit ~gcx (`FCvtZS int_reg_size) [| result_op; arg_op |];
    gen_instructions rest_instructions
  | { value = Instr { instr = Mir.Instruction.Phi _; _ }; _ } :: _ ->
    failwith "Phi nodes must be removed before asm gen"
  | { value = Instr { instr = Mir.Instruction.StackAlloc _; _ }; _ } :: _ ->
    failwith "StackAlloc instructions removed before asm gen"
  | { value = Lit _ | Argument _; _ } :: _ -> failwith "Expected instruction value"

and gen_mov ~gcx ~type_ ~dest ~src =
  let size = register_size_of_mir_value_type type_ in
  let mov_instr =
    if type_ == Double then
      `FMovR
    else
      `MovR size
  in
  Gcx.emit ~gcx mov_instr [| dest; src |]

and gen_mov_to_id ~gcx ~type_ result_id arg_val =
  let arg_op = gen_value ~gcx arg_val in
  let result_op = mk_vreg_of_value_id ~type_ result_id in
  gen_mov ~gcx ~type_ ~dest:result_op ~src:arg_op

(* Generate a cmp instruction between two integer arguments. Return the condition code that should
   be checked. May be different than the input MIR comparison as operands may have been swapped.
   All registers must be sign extended before comparison. *)
and gen_int_cmp ~gcx ~cmp left_val right_val =
  let mir_type = type_of_use left_val in

  let (left_op, left_is_sext) = gen_int_cmp_value ~gcx left_val in
  let (right_op, right_is_sext) = gen_int_cmp_value ~gcx right_val in

  let cond_code ~swap =
    let cond = int_cond_of_mir_comparison cmp in
    if swap then
      swap_cond_order cond
    else
      cond
  in

  match (left_op.value, right_op.value) with
  | (Immediate _, Immediate _) -> failwith "Constants must be folded before codegen"
  (* Comparison to immediate - swap operands if necessary *)
  | (_, Immediate _) ->
    gen_int_cmp_i ~gcx left_op right_op mir_type;
    cond_code ~swap:false
  | (Immediate _, _) ->
    gen_int_cmp_i ~gcx right_op left_op mir_type;
    cond_code ~swap:true
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
      cond_code ~swap:true
    ) else
      let left_sext_op =
        if left_is_sext then
          left_op
        else
          gen_sign_extended_op ~gcx ~type_:mir_type left_op
      in
      Gcx.emit ~gcx (`CmpR (size, extend)) [| left_sext_op; right_op |];
      cond_code ~swap:false

and gen_int_cmp_i ~gcx reg_op imm_op mir_type =
  let size = register_size_of_mir_value_type mir_type in
  let n = int64_of_immediate (cast_to_immediate imm_op) in
  let (instr, imm_op) =
    if Integers.int64_less_than n 0L then
      (`CmnI size, mk_imm64 ~n:(Int64.neg n))
    else
      (`CmpI size, imm_op)
  in
  let reg_sext_op = gen_sign_extended_op ~gcx ~type_:mir_type reg_op in
  Gcx.emit ~gcx instr [| reg_sext_op; imm_op |]

(* Generate an operand that can be used in cmp instructions. 12-bit immediates are allowed, larger
   immediates must be loaded to a register. Return operand and whether it is sign extended. *)
and gen_int_cmp_value ~gcx (use : Use.t) : Operand.t * bool =
  match use.value.value with
  | Lit ((Bool _ | Byte _ | Int _ | Long _) as lit) ->
    let n = int64_of_literal lit in
    (* -2^12 < n < 2^12 *)
    let op =
      if Integers.int64_less_than (-4096L) n && Integers.int64_less_than n 4096L then
        mk_imm64 ~n
      else
        gen_mov_immediate ~gcx lit
    in
    (* Loaded immediates are always sign extended *)
    (op, true)
  | Lit (NullPointer _) -> (imm_0, true)
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

(* Generate a cmp instruction between two float arguments. Return the condition code that should
   be checked. May be different than the input MIR comparison as operands may have been swapped. *)
and gen_float_cmp ~gcx ~cmp left_val right_val =
  match (left_val.value.value, right_val.value.value) with
  | (Lit (Double f), _) when Float.equal f 0.0 ->
    let right_op = gen_value ~gcx right_val in
    Gcx.emit ~gcx `FCmpZ [| right_op |];
    float_cond_of_mir_comparison (swap_comparison_order cmp)
  | (_, Lit (Double f)) when Float.equal f 0.0 ->
    let left_op = gen_value ~gcx left_val in
    Gcx.emit ~gcx `FCmpZ [| left_op |];
    float_cond_of_mir_comparison cmp
  | (_, _) ->
    let left_op = gen_value ~gcx left_val in
    let right_op = gen_value ~gcx right_val in
    Gcx.emit ~gcx `FCmpR [| left_op; right_op |];
    float_cond_of_mir_comparison cmp

(* Generate a comparison and CSet instruction to load the result of the comparison to a register.
   Return the condition that was used in the cmp (may be different than the input condition code, as
   order of arguments may have been swapped). *)
and gen_cmp_cset ~gcx cmp result_id left_val right_val =
  let result_op = mk_vreg_of_value_id ~type_:Bool result_id in
  let cond =
    if type_of_use left_val == Double then
      gen_float_cmp ~gcx ~cmp left_val right_val
    else
      gen_int_cmp ~gcx ~cmp left_val right_val
  in
  Gcx.emit ~gcx (`CSet (Size32, cond)) [| result_op |];
  cond

and gen_add_n ~gcx ~result_op ~type_ reg_op n =
  let size = register_size_of_mir_value_type type_ in
  if is_in_add_sub_range n then
    if Integers.int64_less_than n 0L then
      gen_add_sub_i ~gcx (`SubI size) result_op reg_op (Int64.neg n)
    else
      gen_add_sub_i ~gcx (`AddI size) result_op reg_op n
  else
    let n_reg = mk_vreg ~type_ in
    gen_load_immediate_to_reg ~gcx ~type_ n n_reg;
    Gcx.emit ~gcx (`AddR (size, noop_extend)) [| result_op; reg_op; n_reg |]

and gen_sub_n ~gcx ~result_op ~type_ reg_op n =
  let size = register_size_of_mir_value_type type_ in
  if is_in_add_sub_range n then
    if Integers.int64_less_than n 0L then
      gen_add_sub_i ~gcx (`AddI size) result_op reg_op (Int64.neg n)
    else
      gen_add_sub_i ~gcx (`SubI size) result_op reg_op n
  else
    let n_reg = mk_vreg ~type_ in
    gen_load_immediate_to_reg ~gcx ~type_ n n_reg;
    Gcx.emit ~gcx (`SubR size) [| result_op; reg_op; n_reg |]

and gen_add_sub_i ~gcx instr dest_op reg_op n =
  (* n < 2^12 fits in a single add or sub instruction *)
  if Integers.int64_less_than n 4096L then
    Gcx.emit ~gcx instr [| dest_op; reg_op; mk_imm64 ~n; imm_0 |]
  else
    (* n >= 2^12 is split into two add or sub instructions for low and high 12-bit chunks *)
    let low_12_bits = Int64.logand n 4095L in
    let high_12_bits = Int64.shift_right n 12 |> Int64.logand 4095L in
    Gcx.emit ~gcx instr [| dest_op; reg_op; mk_imm64 ~n:high_12_bits; imm_12 |];
    Gcx.emit ~gcx instr [| dest_op; reg_op; mk_imm64 ~n:low_12_bits; imm_0 |]

(* -2^24 < n < 2^24 can be encoded as immediate in 1 or 2 add/sub instructions *)
and is_in_add_sub_range n =
  Integers.int64_less_than (-16777216L) n && Integers.int64_less_than n 16777216L

(* Generate an operand that can be used in add or sub instructions. 24-bit immediates are allowed,
   but larger immediates must be loaded to a register. *)
and gen_add_sub_value ~gcx (use : Use.t) : Operand.t =
  match use.value.value with
  | Lit ((Bool _ | Byte _ | Int _ | Long _) as lit) ->
    let n = int64_of_literal lit in
    if is_in_add_sub_range n then
      mk_imm64 ~n
    else
      gen_mov_immediate ~gcx lit
  | _ -> gen_value ~gcx use

(* Sign extend an operand to its full register width, if needed *)
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

(* Generate a sign extended value that fits the full register *)
and gen_sign_extended_value ~gcx (use : Use.t) =
  match use.value.value with
  | Lit ((Bool _ | Byte _ | Int _ | Long _) as lit) -> gen_mov_immediate ~gcx lit
  | _ ->
    let type_ = type_of_use use in
    let op = gen_value ~gcx use in
    gen_sign_extended_op ~gcx ~type_ op

(* Generate a zero extended value that fits the full register *)
and gen_zero_extended_value ~gcx (use : Use.t) =
  match use.value.value with
  | Lit ((Bool _ | Byte _ | Int _ | Long _) as lit) -> gen_mov_immediate ~gcx lit
  | _ ->
    let op = gen_value ~gcx use in
    let subregister_size = subregister_size_of_mir_value_type (type_of_use use) in
    (match subregister_size with
    | B ->
      let zext_op = mk_vreg_of_op op in
      Gcx.emit ~gcx (`AndI Size32) [| zext_op; op; imm_byte_mask |];
      zext_op
    | H ->
      let zext_op = mk_vreg_of_op op in
      Gcx.emit ~gcx (`AndI Size32) [| zext_op; op; imm_half_word_mask |];
      zext_op
    | W
    | X ->
      op)

and gen_mul_i ~gcx ~type_ ~result_op reg_op n =
  let size = register_size_of_mir_value_type type_ in
  (* Optimize multiplication by power of two to a left shift *)
  if Integers.is_power_of_two n then
    let power_of_two = Integers.int64_ctz n in
    let power_of_two_imm_op = mk_imm ~imm:(Imm8 (Int8.of_int power_of_two)) in
    Gcx.emit ~gcx (`LslI size) [| result_op; reg_op; power_of_two_imm_op |]
  else
    let n_vreg = mk_vreg ~type_ in
    gen_load_immediate_to_reg ~gcx ~type_ n n_vreg;
    Gcx.emit ~gcx (`Mul size) [| result_op; reg_op; n_vreg |]

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

and gen_bitmask_value ~gcx (use : Use.t) =
  match use.value.value with
  | Lit ((Bool _ | Byte _ | Int _) as lit) ->
    let n = int32_of_literal lit in
    if can_encode_bitmask_imm32 n then
      mk_imm32 ~n
    else
      gen_value ~gcx use
  | Lit (Long n) ->
    if can_encode_bitmask_imm64 n then
      mk_imm64 ~n
    else
      gen_value ~gcx use
  | _ -> gen_value ~gcx use

(* Using algorithm for detecting and encoding aarch64 bitmask immediates from:
   https://dougallj.wordpress.com/2021/10/30/bit-twiddling-optimising-aarch64-logical-immediate-encoding-and-decoding/ *)
and can_encode_bitmask_imm64 n =
  if Int64.equal n 0L || Int64.equal n (-1L) then
    false
  else
    (* Normalize number by rotating so that the first one is in the least significant bit,
       meaning the number starts with the sequence of ones and ends with the sequence of zeros. *)
    let rotate = Integers.int64_ctz (Int64.logand n (Int64.succ n)) in
    let normalized = Integers.int64_rotate_left n (-rotate) in

    (* Find the length of the leading zeros and trailing ones sequences to find total size of the
       repeated sequence. *)
    let leading_zeros = Integers.int64_clz normalized in
    let trailing_ones = Integers.int64_ctz (Int64.lognot normalized) in
    let size = leading_zeros + trailing_ones in

    (* Encodable if the entire number consists of the same subsequence repeated throughout the entire
       number. Check this by seeing if rotating by the repeated sequence size gives the same number. *)
    let rotated = Integers.int64_rotate_left n size in
    Int64.equal rotated n

and can_encode_bitmask_imm32 n =
  if Int32.equal n 0l || Int32.equal n (-1l) then
    false
  else
    let rotate = Integers.int32_ctz (Int32.logand n (Int32.succ n)) in
    let normalized = Integers.int32_rotate_left n (-rotate) in

    let leading_zeros = Integers.int32_clz normalized in
    let trailing_ones = Integers.int32_ctz (Int32.lognot normalized) in
    let size = leading_zeros + trailing_ones in

    let rotated = Integers.int32_rotate_left n size in
    Int32.equal rotated n

and gen_shift_value ~gcx ~size shift_val =
  match shift_val.Use.value.value with
  | Lit ((Bool _ | Byte _ | Int _ | Long _) as lit) ->
    (* Only low bits are used for shift, so truncate immediate *)
    let n = int64_of_literal lit in
    if size == Size64 then
      mk_imm64 ~n:(Int64.logand n 63L)
    else
      mk_imm64 ~n:(Int64.logand n 31L)
  | _ -> gen_value ~gcx shift_val

and gen_size_from_count_and_type ~gcx count_use count_param_type count_param_mir_type mir_ty =
  let element_size = size_of_mir_type ~agg_cache:gcx.agg_cache mir_ty in
  let result_op =
    match count_param_type with
    | ParamInRegister reg -> mk_precolored ~type_:count_param_mir_type reg
    | ParamOnStack _ -> failwith "Cannot pass builtin size argument on stack"
  in
  match count_use.value.value with
  (* If count is a literal precalculate total requested size *)
  | Lit ((Byte _ | Int _ | Long _) as count_lit) ->
    let count = int64_of_literal count_lit in
    let total_size = Int64.mul count (Int64.of_int element_size) in
    gen_load_immediate_to_reg ~gcx ~type_:count_param_mir_type total_size result_op
  (* If count is a variable multiply by size before putting in argument register *)
  | _ ->
    let count_op = gen_value ~gcx count_use in
    (* Check for special case where element size is a single byte - no multiplication required *)
    if element_size == 1 then
      gen_mov ~gcx ~type_:count_param_mir_type ~dest:result_op ~src:count_op
    else
      gen_mul_i ~gcx ~type_:count_param_mir_type ~result_op count_op (Int64.of_int element_size)

and gen_get_pointer ~gcx (get_pointer_instr : Mir.Instruction.GetPointer.t) =
  let open Mir.Instruction.GetPointer in
  let { pointer; pointer_offset; offsets } = get_pointer_instr in

  (* Add address of root pointer *)
  let (base, immediate_offset) =
    match pointer.value.value with
    | Lit (Global { name; _ }) ->
      let (high_bits_reg, low_bits_offset) =
        gen_adrp_and_low_bits ~gcx ~type_:(type_of_use pointer) name
      in
      (high_bits_reg, Some (LabelOffset low_bits_offset))
    | Lit (NullPointer _) ->
      let null_pointer_reg = mk_vreg ~type_:Long in
      gen_load_immediate_to_reg ~gcx ~type_:Long 0L null_pointer_reg;
      (null_pointer_reg, None)
    | _ ->
      let pointer_reg = gen_value ~gcx pointer in
      (pointer_reg, None)
  in

  let base = ref base in
  let immediate_offset = ref immediate_offset in
  let scaled_reg_offset = ref None in

  let emit_current_address_calculation () =
    let new_base =
      match (!immediate_offset, !scaled_reg_offset) with
      | (None, None) -> !base
      | (Some (LabelOffset label_offset), None) ->
        let new_base = mk_vreg ~type_:Long in
        Gcx.emit ~gcx (`AddI Size64) [| new_base; !base; label_offset; imm_0 |];
        new_base
      | (Some (ImmediateOffset imm_offset), None) ->
        let new_base = mk_vreg ~type_:Long in
        gen_add_n ~gcx ~result_op:new_base ~type_:Long !base imm_offset;
        new_base
      | (None, Some (reg, reg_mir_type, scale)) ->
        let new_base = mk_vreg ~type_:Long in
        (if scale == 1 then
          Gcx.emit ~gcx (`AddR (Size64, noop_extend)) [| new_base; !base; reg |]
        else
          let subregister_size = subregister_size_of_mir_value_type reg_mir_type in
          let extend = sign_extend_of_subregister_size subregister_size in
          let shift_imm =
            match scale with
            | 2 -> imm_1
            | 4 -> imm_2
            | 8 -> imm_3
            | _ -> failwith "Invalid scale"
          in
          Gcx.emit ~gcx (`AddR (Size64, extend)) [| new_base; !base; reg; shift_imm |]);
        new_base
      | (Some _, Some _) -> failwith "Can only have one offset type"
    in

    base := new_base;
    immediate_offset := None;
    scaled_reg_offset := None
  in

  let add_fixed_offset new_offset =
    if !scaled_reg_offset != None then emit_current_address_calculation ();
    match !immediate_offset with
    | None -> immediate_offset := Some (ImmediateOffset new_offset)
    (* Combine immediate offsets *)
    | Some (ImmediateOffset existing_offset) ->
      let combined_offset = Int64.add new_offset existing_offset in
      immediate_offset := Some (ImmediateOffset combined_offset)
    (* Immediate offset can be added to label *)
    | Some (LabelOffset label_op) ->
      let label = cast_to_label label_op in
      let new_label = Printf.sprintf "%s+%s" label (Int64.to_string new_offset) in
      immediate_offset := Some (LabelOffset (mk_label_op ~label:new_label))
  in

  let add_scaled_register (reg, reg_mir_type) scale =
    if !immediate_offset != None || !scaled_reg_offset != None then
      emit_current_address_calculation ();
    match scale with
    | 1
    | 2
    | 4
    | 8 ->
      scaled_reg_offset := Some (reg, reg_mir_type, scale)
    | _ ->
      emit_current_address_calculation ();
      let scaled_reg = mk_vreg ~type_:reg_mir_type in
      gen_mul_i ~gcx ~type_:reg_mir_type ~result_op:scaled_reg reg (Int64.of_int scale);
      scaled_reg_offset := Some (scaled_reg, reg_mir_type, 1)
  in

  (* The element type that the next offset calculation should be using *)
  let current_element_type = ref (pointer_value_element_type pointer.value) in

  let gen_offset (offset : use_offset) =
    match offset with
    | PointerIndex pointer_offset ->
      let element_size =
        match !current_element_type with
        | Type.Array (mir_type, _)
        | mir_type ->
          size_of_mir_type ~agg_cache:gcx.agg_cache mir_type
      in
      (match pointer_offset.value.value with
      | Lit lit ->
        let num_elements = int64_of_literal lit in
        if not (Int64.equal num_elements Int64.zero) then
          let offset = Int64.mul num_elements (Int64.of_int element_size) in
          add_fixed_offset offset
      | _ ->
        let num_elements_reg = gen_value ~gcx pointer_offset in
        let num_elements_reg_mir_type = type_of_use pointer_offset in
        add_scaled_register (num_elements_reg, num_elements_reg_mir_type) element_size)
    | FieldIndex element_index ->
      (match !current_element_type with
      | Aggregate ({ Aggregate.elements; _ } as agg) ->
        (* Find offset of aggregate element in aggregate's layout, add add it to address *)
        let agg_layout = AggregateLayoutCache.get gcx.agg_cache agg in
        let { AggregateElement.offset; _ } = AggregateLayout.get_element agg_layout element_index in
        if offset <> 0 then add_fixed_offset (Int64.of_int offset);
        (* Update current type to element type *)
        let (_, element_type) = List.nth elements element_index in
        current_element_type := element_type
      | _ -> failwith "FieldIndex must index into aggregate type")
  in

  (* Visit all offsets *)
  (match pointer_offset with
  | Some pointer_offset -> gen_offset (PointerIndex pointer_offset)
  | None -> ());
  List.iter (fun offset -> gen_offset offset) offsets;

  (* ignore (gcx, get_pointer_instr) *)
  emit_current_address_calculation ();
  !base

(* Generate an immediate value loaded to a register. Only 16 bits can be moved in a single
   instruction, if more are required then additional movk instructions must be used. *)
and gen_mov_immediate ~gcx ?(allow_zr = true) (lit : Literal.t) : Operand.t =
  let type_ = type_of_literal lit in
  if is_zero_literal lit && allow_zr then
    mk_precolored ~type_ `ZR
  else
    let n = int64_of_literal lit in
    let vreg = mk_vreg ~type_ in
    gen_load_immediate_to_reg ~gcx ~type_ n vreg;
    vreg

and gen_load_immediate_to_reg ~gcx ~(type_ : Type.t) (n : Int64.t) (vreg : Operand.t) =
  let bytes = Bytes.create 8 in
  Bytes.set_int64_le bytes 0 n;

  (* Extract an unsigned 16 bit chunk from `n` starting at the given byte offset *)
  let get_imm16_chunk bytes byte_offset =
    let imm16 = Bytes.get_uint16_le bytes byte_offset in
    mk_imm ~imm:(Imm16 imm16)
  in

  let register_size = register_size_of_mir_value_type type_ in

  let is_greater_than x y = Int64.compare x y != -1 in

  if is_greater_than n 0L then (
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
    )

and aarch64_immediate_mantissa_mask = 0x0000FFFFFFFFFFFFL

and is_fmov_float_immediate (n : Float.t) : bool =
  let bits = Int64.bits_of_float n in
  let exponent = Floats.get_exponent_of_bits bits in
  if exponent >= -3 && exponent <= 4 then
    Int64.equal (Int64.logand bits aarch64_immediate_mantissa_mask) 0L
  else
    false

and gen_load_double_immediate_to_reg ~gcx (f : Float.t) (vreg : Operand.t) =
  if Floats.is_positive_zero f then
    Gcx.emit ~gcx `FMovR [| vreg; mk_precolored ~type_:Long `ZR |]
  else if is_fmov_float_immediate f then
    let float_imm = mk_float_imm ~f in
    Gcx.emit ~gcx `FMovI [| vreg; float_imm |]
  else
    let float_label = Gcx.get_float_label ~gcx f in
    let (high_bits_reg, low_bits_imm) = gen_adrp_and_low_bits ~gcx ~type_:Long float_label in
    Gcx.emit ~gcx (`LdrI (Size64, X, true, Offset)) [| vreg; high_bits_reg; low_bits_imm |]

(* Generate an AdrP instruction that loads the high bits of a label to a register. Also return
   the offset for the low 12 bits, which need to be added to the high bits register to form a
   complete address in an Add, Ldr, or Str instruction. *)
and gen_adrp_and_low_bits ~gcx ~type_ name =
  let label = label_of_mir_label name in
  let vreg = mk_vreg ~type_ in
  Gcx.emit ~gcx `AdrP [| vreg; mk_label_op ~label |];
  (vreg, mk_label_op ~label:(":lo12:" ^ label))

and gen_value ~gcx ?(allow_zr = true) (use : Use.t) =
  match use.value.value with
  | Lit ((Bool _ | Byte _ | Int _ | Long _) as lit) -> gen_mov_immediate ~gcx ~allow_zr lit
  | Lit (Double f) ->
    let vreg = mk_vreg ~type_:(type_of_use use) in
    gen_load_double_immediate_to_reg ~gcx f vreg;
    vreg
  | Lit (Function { name; _ })
  | Lit (Global { name; _ }) ->
    let (high_bits_reg, low_bits_offset) =
      gen_adrp_and_low_bits ~gcx ~type_:(type_of_use use) name
    in
    Gcx.emit ~gcx (`AddI Size64) [| high_bits_reg; high_bits_reg; low_bits_offset; imm_0 |];
    high_bits_reg
  | Lit (NullPointer _) ->
    if allow_zr then
      mk_precolored ~type_:(Pointer Byte) `ZR
    else
      imm_0
  | Lit (ArrayString _)
  | Lit (ArrayVtable _) ->
    failwith "TODO: Cannot codegen array literals"
  | Lit (AggregateClosure _) -> failwith "TODO: Cannot codegen aggregate literals"
  | Instr { type_; _ }
  | Argument { type_; _ } ->
    mk_virtual_register_of_value_id ~value_id:use.value.id ~type_

and block_op_of_mir_block ~gcx mir_block =
  mk_block_op ~block:(Gcx.get_block_from_mir_block ~gcx mir_block)

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
  | Aggregate _ -> failwith "TODO: Cannot codegen aggregate structure literals"
  | Array _ -> failwith "TODO: Cannot codegen array literals"

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
  | Aggregate _ -> failwith "TODO: Cannot codegen aggregate structure literals"
  | Array _ -> failwith "TODO: Cannot codegen array literals"

and int_cond_of_mir_comparison cmp =
  match cmp with
  | Mir.Instruction.Eq -> EQ
  | Neq -> NE
  | Lt -> LT
  | LtEq -> LE
  | Gt -> GT
  | GtEq -> GE

and float_cond_of_mir_comparison cmp =
  match cmp with
  | Mir.Instruction.Eq -> EQ
  | Neq -> NE
  | Lt -> MI
  | LtEq -> LS
  | Gt -> GT
  | GtEq -> GE
