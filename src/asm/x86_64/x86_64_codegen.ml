open Asm
open Asm_builders
open Asm_calling_convention
open Asm_codegen
open Asm_instruction_definition.X86_64
open Basic_collections
open Mir
open Mir_builders
open Mir_type
open X86_64_asm
open X86_64_calling_convention
open X86_64_gen_context
open X86_64_layout

type resolved_source_value =
  (* An immediate value *)
  | SImm of Operand.t
  (* Value is a register *)
  | SReg of Operand.t * operand_size
  (* Value is the contents at a memory location *)
  | SMem of Operand.t * operand_size
  (* Value is a memory address *)
  | SAddr of Operand.t * Type.t

type mir_comparison_ccs =
  (* Condition is true if condition code is set *)
  | SingleCC of condition_code
  (* Condition is true if both condition codes are set *)
  | AndCC of condition_code * condition_code
  (* Condition is true if at least one condition code is set *)
  | OrCC of condition_code * condition_code

let rec gen ~gcx (ir : Program.t) =
  let should_filter_stdlib = Asm_gen.should_filter_stdlib () in

  (* Calculate layout of all aggregate types *)
  SMap.iter (fun _ agg -> Gcx.build_agg_layout ~gcx agg) ir.types;

  (* Calculate initial function info for all functions *)
  SMap.iter (fun _ func -> preprocess_function ~gcx ~ir func) ir.funcs;

  (* Generate all globals in program *)
  SMap.iter
    (fun name global ->
      if not (should_filter_stdlib && has_std_lib_prefix name) then gen_global ~gcx ~ir global)
    ir.globals;

  (* Generate all functions in program *)
  SMap.iter
    (fun name func ->
      if not (should_filter_stdlib && has_std_lib_prefix name) then gen_function ~gcx ~ir func)
    ir.funcs;

  Gcx.finish_builders ~gcx

and gen_global ~gcx ~ir:_ global =
  let label = label_of_mir_label global.name in
  match global.init_val with
  (* Fake zero size global is not generated *)
  | _ when global.name = zero_size_name -> ()
  (* If uninitialized, place global variable in bss section *)
  | None ->
    let size = Gcx.size_of_mir_type ~gcx global.type_ in
    let align = Gcx.alignment_of_mir_type ~gcx global.type_ in
    let is_pointer = is_pointer_type global.type_ in
    Gcx.add_bss ~gcx { label; value = (); size; is_pointer } align
  (* Array literal is known at compile time, so insert into initialized data section *)
  | Some { value = { value = Lit (ArrayString data); _ }; _ } ->
    let size = String.length data in
    Gcx.add_data ~gcx { label; value = AsciiData data; size; is_pointer = false }
  | Some { value = { value = Lit (ArrayVtable (_, func_labels)); _ }; _ } ->
    let label_values =
      List.map
        (fun func_use ->
          let func = cast_to_function_literal func_use.Use.value in
          label_of_mir_label func.name)
        func_labels
    in
    let size = List.length label_values * pointer_size in
    Gcx.add_data ~gcx { label; value = LabelData label_values; size; is_pointer = false }
  (* Aggregate closure globals are special cased, with 0 set as environment *)
  | Some { value = { value = Lit (AggregateClosure (_, func_use)); _ }; _ } ->
    let func = cast_to_function_literal func_use.value in
    let func_data = LabelData [label_of_mir_label func.name] in
    let env_data = ImmediateData (Imm64 0L) in
    Gcx.add_data
      ~gcx
      { label; value = ArrayData [func_data; env_data]; size = ptr_size * 2; is_pointer = false }
  (* Pointer and function literals are labels, so insert into initialized data section *)
  | Some { value = { value = Lit (Global { name = init_label; _ }); _ }; _ }
  | Some { value = { value = Lit (Function { name = init_label; _ }); _ }; _ } ->
    let init_label = label_of_mir_label init_label in
    let is_pointer = is_pointer_type global.type_ in
    let data = { label; value = LabelData [init_label]; size = pointer_size; is_pointer } in
    Gcx.add_data ~gcx data
  (* Global is initialized to immediate, so insert into initialized data section *)
  | Some init_val ->
    (match resolve_ir_value ~gcx ~allow_imm64:true init_val with
    | SImm imm ->
      let imm = cast_to_immediate imm in
      let size = bytes_of_size (size_of_immediate imm) in
      let is_pointer = is_pointer_type global.type_ in
      let data = { label; value = ImmediateData imm; size; is_pointer } in
      Gcx.add_data ~gcx data
    | SAddr _
    | SReg _
    | SMem _ ->
      failwith "Global init value must be a constant")

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
          let size = operand_size_of_mir_value_type type_ in
          let param_op = mk_virtual_register_of_value_id ~value_id:arg_id ~type_:param_mir_type in
          Gcx.emit ~gcx (`MovMM size) [| mk_precolored_of_operand reg param_op; param_op |];
          param_op)
      mir_func.params;

  (* Jump to function start and gen function body *)
  Gcx.emit ~gcx `Jmp [| block_op_of_mir_block ~gcx mir_func.start_block |];
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

and gen_instructions ~gcx ~ir ~block instructions =
  let gen_instructions = gen_instructions ~gcx ~ir ~block in
  let operand_of_value_id value_id = mk_virtual_register_of_value_id ~value_id in
  let mk_vreg ~type_ = mk_virtual_register ~type_ in
  let mk_vreg_of_op op = mk_virtual_register ~type_:op.Operand.type_ in
  let resolve_ir_value ?(allow_imm64 = false) v = resolve_ir_value ~gcx ~allow_imm64 v in
  let emit_mem mem =
    match mem with
    | SReg (reg, _) -> reg
    | SMem (mem, _) -> mem
    | SAddr (addr, type_) ->
      let vreg = mk_vreg ~type_ in
      Gcx.emit ~gcx (`Lea Size64) [| addr; vreg |];
      vreg
    | _ -> failwith "Only called on address, memory location, or vreg"
  in
  let emit_bool_as_reg value =
    match resolve_ir_value value with
    | SReg (reg, _) -> reg
    | SMem (mem, size) ->
      let vreg = mk_vreg_of_op mem in
      Gcx.emit ~gcx (`MovMM size) [| mem; vreg |];
      vreg
    | _ -> failwith "Boolean variable must be vreg or memory location"
  in
  (* Return preferred (source, dest) args for a commutative binary operation. We try to avoid having
     the destination be a memory location, so source always contains memory location if one exists. *)
  let choose_commutative_source_dest_arg_order v1 v2 =
    match (v1, v2) with
    | (SMem _, _) -> (v1, v2)
    | (_, SMem _) -> (v2, v1)
    | _ -> (v1, v2)
  in
  let gen_call_arguments param_types arg_vals =
    (* First move function arguments that are passed in stack slots *)
    List.iteri
      (fun i arg_val ->
        match param_types.(i) with
        | ParamInRegister _ -> ()
        | ParamOnStack stack_slot_idx ->
          let arg_type = type_of_use arg_val in
          let argument_stack_slot_op =
            Gcx.mk_function_argument_stack_slot ~gcx ~i:stack_slot_idx ~type_:arg_type
          in
          (match resolve_ir_value arg_val with
          | SImm imm ->
            let dest_size = operand_size_of_mir_value_type arg_type in
            Gcx.emit ~gcx (`MovIM dest_size) [| imm; argument_stack_slot_op |]
          (* Address must be calculated in a register and then moved into stack slot *)
          | SAddr (addr, type_) ->
            let vreg = mk_vreg ~type_ in
            Gcx.emit ~gcx (`Lea Size64) [| addr; vreg |];
            Gcx.emit ~gcx (`MovMM Size64) [| vreg; argument_stack_slot_op |]
          | SMem (mem, size) -> Gcx.emit ~gcx (`MovMM size) [| mem; argument_stack_slot_op |]
          | SReg (reg, size) -> Gcx.emit ~gcx (`MovMM size) [| reg; argument_stack_slot_op |]))
      arg_vals;
    (* Then move function arguments that are passed in stack slots​ *)
    List.iteri
      (fun i arg_val ->
        match param_types.(i) with
        | ParamOnStack _ -> ()
        | ParamInRegister _ when is_zero_size_global arg_val -> ()
        | ParamInRegister reg ->
          let precolored_reg = mk_precolored ~type_:(type_of_use arg_val) reg in
          (match resolve_ir_value ~allow_imm64:true arg_val with
          | SImm imm ->
            let size = size_of_immediate (cast_to_immediate imm) in
            Gcx.emit ~gcx (`MovIM size) [| imm; precolored_reg |]
          | SAddr (addr, _) -> Gcx.emit ~gcx (`Lea Size64) [| addr; precolored_reg |]
          | SMem (mem, size) -> Gcx.emit ~gcx (`MovMM size) [| mem; precolored_reg |]
          | SReg (source_reg, size) -> Gcx.emit ~gcx (`MovMM size) [| source_reg; precolored_reg |]))
      arg_vals
  in
  let gen_mov result_value_id value =
    let result_op = operand_of_value_id ~type_:(type_of_use value) result_value_id in
    match resolve_ir_value ~allow_imm64:true value with
    | SImm imm ->
      let size = size_of_immediate (cast_to_immediate imm) in
      Gcx.emit ~gcx (`MovIM size) [| imm; result_op |]
    | SAddr (addr, _) -> Gcx.emit ~gcx (`Lea Size64) [| addr; result_op |]
    | SMem (mem, size) -> Gcx.emit ~gcx (`MovMM size) [| mem; result_op |]
    | SReg (src_reg, size) -> Gcx.emit ~gcx (`MovMM size) [| src_reg; result_op |]
  in
  let gen_idiv left_val right_val =
    let mk_short_precolored_a () = mk_precolored ~type_:Short `A in
    let mk_precolored_a () = mk_precolored ~type_:(type_of_use left_val) `A in
    match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    (* Convert 8-byte divides to 16-byte divides *)
    | (SImm { value = Immediate (Imm8 dividend_imm); _ }, divisor) ->
      let dividend_imm = mk_imm ~imm:(Imm16 (Int8.to_int dividend_imm)) in
      let divisor_mem = emit_mem divisor in
      let divisor_vreg = mk_vreg ~type_:Short in
      Gcx.emit ~gcx (`MovIM Size16) [| dividend_imm; mk_short_precolored_a () |];
      Gcx.emit ~gcx (`MovSX (Size8, Size16)) [| divisor_mem; divisor_vreg |];
      Gcx.emit ~gcx (`ConvertDouble Size16) [||];
      Gcx.emit ~gcx (`IDiv Size16) [| divisor_vreg |];
      Size32
    | (SImm dividend_imm, divisor) ->
      let size = operand_size_of_svalue divisor in
      let divisor_mem = emit_mem divisor in
      Gcx.emit ~gcx (`MovIM size) [| dividend_imm; mk_precolored_a () |];
      Gcx.emit ~gcx (`ConvertDouble size) [||];
      Gcx.emit ~gcx (`IDiv size) [| divisor_mem |];
      size
    (* Convert 8-byte divides to 16-byte divides *)
    | (dividend, SImm { value = Immediate (Imm8 divisor_imm); _ }) ->
      let dividend_mem = emit_mem dividend in
      let divisor_imm = mk_imm ~imm:(Imm16 (Int8.to_int divisor_imm)) in
      let divisor_vreg = mk_vreg ~type_:Short in
      Gcx.emit ~gcx (`MovSX (Size8, Size16)) [| dividend_mem; mk_short_precolored_a () |];
      Gcx.emit ~gcx (`MovIM Size16) [| divisor_imm; divisor_vreg |];
      Gcx.emit ~gcx (`ConvertDouble Size16) [||];
      Gcx.emit ~gcx (`IDiv Size16) [| divisor_vreg |];
      Size32
    | (dividend, SImm divisor_imm) ->
      let size = operand_size_of_svalue dividend in
      let dividend_mem = emit_mem dividend in
      let divisor_vreg = mk_vreg ~type_:(type_of_use left_val) in
      Gcx.emit ~gcx (`MovMM size) [| dividend_mem; mk_precolored_a () |];
      Gcx.emit ~gcx (`MovIM size) [| divisor_imm; divisor_vreg |];
      Gcx.emit ~gcx (`ConvertDouble size) [||];
      Gcx.emit ~gcx (`IDiv size) [| divisor_vreg |];
      size
    | (dividend, divisor) ->
      let size = operand_size_of_svalue dividend in
      (* Convert 8-byte divides to 16-byte divides *)
      if size = Size8 then (
        let dividend_mem = emit_mem dividend in
        let divisor_mem = emit_mem divisor in
        let divisor_vreg = mk_vreg ~type_:Short in
        Gcx.emit ~gcx (`MovSX (Size8, Size16)) [| dividend_mem; mk_short_precolored_a () |];
        Gcx.emit ~gcx (`MovSX (Size8, Size16)) [| divisor_mem; divisor_vreg |];
        Gcx.emit ~gcx (`ConvertDouble Size16) [||];
        Gcx.emit ~gcx (`IDiv Size16) [| divisor_vreg |];
        Size32
      ) else
        let dividend_mem = emit_mem dividend in
        let divisor_mem = emit_mem divisor in
        Gcx.emit ~gcx (`MovMM size) [| dividend_mem; mk_precolored_a () |];
        Gcx.emit ~gcx (`ConvertDouble size) [||];
        Gcx.emit ~gcx (`IDiv size) [| divisor_mem |];
        size
  in
  (* Generate a bit shift instruction from the target and shift arguments *)
  let gen_shift ~mk_reg_instr ~mk_imm_instr result_value_id target_use shift_use =
    let type_ = type_of_use target_use in
    let result_op = operand_of_value_id ~type_ result_value_id in
    (* Do not reduce size of target immediate, as we must know its original size to know what
       size to make the shift operation. *)
    (match (resolve_ir_value target_use, resolve_ir_value ~allow_imm64:true shift_use) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm target_imm, shift) ->
      let size = operand_size_of_svalue shift in
      let precolored_c = mk_precolored ~type_ `C in
      let shift_mem = emit_mem shift in
      (* Only low byte is used for shift, so avoid REX prefix for small code size optimization *)
      let shift_size =
        if size = Size64 then
          Size32
        else
          size
      in
      Gcx.emit ~gcx (`MovMM shift_size) [| shift_mem; precolored_c |];
      Gcx.emit ~gcx (`MovIM size) [| target_imm; result_op |];
      mk_reg_instr size result_op
    | (target, SImm shift_imm) ->
      let size = operand_size_of_svalue target in
      let target_mem = emit_mem target in
      let shift_value = int64_of_immediate (cast_to_immediate shift_imm) in
      let shift_imm = mk_imm ~imm:(Imm8 (Int8.of_int64 shift_value)) in
      (* Only low byte of immediate is used for shift, so truncate immediate to low byte *)
      Gcx.emit ~gcx (`MovMM size) [| target_mem; result_op |];
      mk_imm_instr size shift_imm result_op
    | (target, shift) ->
      let size = operand_size_of_svalue target in
      (* Only low byte is used for shift, so avoid REX prefix for small code size optimization *)
      let shift_size = operand_size_of_svalue shift in
      let shift_size =
        if shift_size = Size64 then
          Size32
        else
          shift_size
      in
      let precolored_c = mk_precolored ~type_ `C in
      let shift_mem = emit_mem shift in
      Gcx.emit ~gcx (`MovMM shift_size) [| shift_mem; precolored_c |];
      let target_mem = emit_mem target in
      Gcx.emit ~gcx (`MovMM size) [| target_mem; result_op |];
      mk_reg_instr size result_op);
    result_op
  in
  (* Generate a cmp instruction between two arguments. Return whether order was swapped. *)
  let gen_cmp left_val right_val =
    let cmp_op size =
      if type_of_use left_val == Double then
        `UComiSD
      else
        `CmpMM size
    in
    match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    (* Comparison to immediate - swap arguments if necessary *)
    | (SImm imm, other) ->
      let size = operand_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (`CmpMI size) [| other_mem; imm |];
      true
    | (other, SImm imm) ->
      let size = operand_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (`CmpMI size) [| other_mem; imm |];
      false
    (* Floating point comparison requires first operand to be a register, so flip if possible *)
    | (SMem (mem1, size), SReg (reg2, _)) when mem1.type_ == Double ->
      Gcx.emit ~gcx (cmp_op size) [| reg2; mem1 |];
      true
    (* Cannot compare two memory locations at the same time. First operand must be a register for
       floating point comparisons. *)
    | (SMem (mem1, size), SMem (mem2, _)) ->
      let vreg = mk_vreg_of_op mem1 in
      Gcx.emit ~gcx (`MovMM size) [| mem1; vreg |];
      Gcx.emit ~gcx (cmp_op size) [| vreg; mem2 |];
      false
    | (v1, v2) ->
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (cmp_op (operand_size_of_svalue v1)) [| mem1; mem2 |];
      false
  in
  let swap_compound_cc cc =
    match cc with
    | SingleCC cc -> SingleCC (swap_condition_code_order cc)
    | AndCC (cc1, cc2) -> AndCC (swap_condition_code_order cc1, swap_condition_code_order cc2)
    | OrCC (cc1, cc2) -> OrCC (swap_condition_code_order cc1, swap_condition_code_order cc2)
  in
  (* Generate a comparison and SetCC instruction to load the result of the comparison to a register.
     Return the condition code that was used (may be different than the input condition code, as
     order of arguments may have been swapped). *)
  let gen_cmp_set_cc cc result_value_id left_val right_val =
    let result_op = operand_of_value_id ~type_:Bool result_value_id in
    Gcx.emit ~gcx (`XorMM Size32) [| result_op; result_op |];
    let swapped = gen_cmp left_val right_val in
    let cc =
      if swapped then
        swap_compound_cc cc
      else
        cc
    in
    match cc with
    | SingleCC cc ->
      Gcx.emit ~gcx (`SetCC cc) [| result_op |];
      SingleCC cc
    (* For compound conditions generate multiple SetCCs and And the results. Only true if all
       conditions were true (aka And/Or result is 1, aka ZF=0, equivalent to NE condition code) *)
    | AndCC (cc1, cc2) ->
      let vreg = mk_vreg ~type_:Bool in
      Gcx.emit ~gcx (`SetCC cc1) [| result_op |];
      Gcx.emit ~gcx (`SetCC cc2) [| vreg |];
      Gcx.emit ~gcx (`AndMM Size8) [| vreg; result_op |];
      SingleCC NE
    | OrCC (cc1, cc2) ->
      let vreg = mk_vreg ~type_:Bool in
      Gcx.emit ~gcx (`SetCC cc1) [| result_op |];
      Gcx.emit ~gcx (`SetCC cc2) [| vreg |];
      Gcx.emit ~gcx (`OrMM Size8) [| vreg; result_op |];
      SingleCC NE
  in
  let gen_cond_jmp cc result_id left_val right_val =
    let cc =
      (* If the only use of the comparison is in this branch instruction, only need to generate
         a comparison instruction and use the current flags. *)
      if value_has_single_use left_val.Use.user then
        let swapped = gen_cmp left_val right_val in
        if swapped then
          swap_compound_cc cc
        else
          cc
      (* Otherwise the result of the comparison is used elsewhere, so we must load to a register
         with a SetCC instruction. We can still emit a JmpCC directly off the current flags though. *)
      else
        gen_cmp_set_cc cc result_id left_val right_val
    in
    let (continue, jump) =
      match get_terminator block with
      | Some { instr = Branch { continue; jump; _ }; _ } -> (continue, jump)
      | _ -> failwith "Only called on blocks with conditional branches"
    in
    match cc with
    | SingleCC cc ->
      (* Note that the condition code is inverted as we emit a JmpCC to the false branch *)
      let cc = invert_condition_code cc in
      Gcx.emit ~gcx (`JmpCC cc) [| block_op_of_mir_block ~gcx jump |];
      Gcx.emit ~gcx `Jmp [| block_op_of_mir_block ~gcx continue |]
    | AndCC (cc1, cc2) ->
      (* Jump to jump block if either condition code is false *)
      let cc1 = invert_condition_code cc1 in
      let cc2 = invert_condition_code cc2 in
      Gcx.emit ~gcx (`JmpCC cc1) [| block_op_of_mir_block ~gcx jump |];
      Gcx.emit ~gcx (`JmpCC cc2) [| block_op_of_mir_block ~gcx jump |];
      Gcx.emit ~gcx `Jmp [| block_op_of_mir_block ~gcx continue |]
    | OrCC (cc1, cc2) ->
      (* Jump to continue block if either condition code is true *)
      Gcx.emit ~gcx (`JmpCC cc1) [| block_op_of_mir_block ~gcx continue |];
      Gcx.emit ~gcx (`JmpCC cc2) [| block_op_of_mir_block ~gcx continue |];
      Gcx.emit ~gcx `Jmp [| block_op_of_mir_block ~gcx jump |]
  in

  match instructions with
  | [] -> ()
  (*
   * ===========================================
   *                   Mov
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Mov value; _ }; _ } :: rest_instructions ->
    gen_mov result_id value;
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
    let calling_convention =
      match func_val.value.value with
      | Lit (Function mir_func) -> Gcx.mir_function_calling_convention mir_func
      | _ ->
        (* TODO: Annotate calling convention on MIR call instructions and enforce single calling
           convention for all functions that flow to a single call instruction. *)
        system_v_calling_convention
    in

    (* Emit arguments for call *)
    let param_mir_types = List.map type_of_use arg_vals in
    let param_types = calling_convention#calculate_param_types param_mir_types in
    gen_call_arguments param_types arg_vals;

    (* Emit call instruction *)
    (match func_val.value.value with
    | Lit (Function mir_func) ->
      let func = Gcx.get_func_from_mir_func ~gcx mir_func in
      Gcx.emit ~gcx (`CallL (param_types, calling_convention)) [| mk_function_op ~func |]
    | _ ->
      let func_mem = emit_mem (resolve_ir_value func_val) in
      Gcx.emit ~gcx (`CallM (Size64, param_types, calling_convention)) [| func_mem |]);
    (* Move result from return register to return operand *)
    (if has_return then
      let return_size = operand_size_of_mir_value_type type_ in
      let return_reg = calling_convention#calculate_return_register type_ in
      let return_reg_op = mk_precolored ~type_ return_reg in
      let return_op = operand_of_value_id ~type_ result_id in
      Gcx.emit ~gcx (`MovMM return_size) [| return_reg_op; return_op |]);
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
      let calling_convention = (Option.get gcx.current_func).calling_convention in
      let return_reg = calling_convention#calculate_return_register (type_of_use value) in
      let return_reg_op = mk_precolored ~type_:(type_of_use value) return_reg in
      (match resolve_ir_value ~allow_imm64:true value with
      | SImm imm ->
        let size = size_of_immediate (cast_to_immediate imm) in
        Gcx.emit ~gcx (`MovIM size) [| imm; return_reg_op |]
      | SAddr (addr, _) -> Gcx.emit ~gcx (`Lea Size64) [| addr; return_reg_op |]
      | SMem (mem, size) -> Gcx.emit ~gcx (`MovMM size) [| mem; return_reg_op |]
      | SReg (reg, size) -> Gcx.emit ~gcx (`MovMM size) [| reg; return_reg_op |]));
    Gcx.emit ~gcx `Ret [||]
  (*
   * ===========================================
   *                Load
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Load pointer; _ }; _ } :: rest_instructions ->
    let type_ = pointer_value_element_type pointer.value in
    let result_op = operand_of_value_id ~type_ result_id in
    let size =
      match type_ with
      | Bool
      | Byte
      | Short
      | Int
      | Long
      | Double
      | Function
      | Pointer _ ->
        operand_size_of_mir_value_type type_
      | Aggregate _ -> failwith "TODO: Cannot compile aggregate literals"
      | Array _ -> failwith "TODO: Cannot compile array literals"
    in
    let src =
      match pointer.value.value with
      | Lit (Global { name; _ }) -> mk_memory_address ~address:(mk_label_memory_address name) ~type_
      | _ ->
        (match resolve_ir_value pointer with
        | SReg (reg, _) ->
          mk_memory_address
            ~address:{ offset = None; base = RegBase reg; index_and_scale = None }
            ~type_
        | SMem (mem, _) -> mem
        | SImm _
        | SAddr _ ->
          failwith "Expected memory or address")
    in
    Gcx.emit ~gcx (`MovMM size) [| src; result_op |];
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Store
   * ===========================================
   *)
  | { value = Instr { instr = Store (pointer, value); _ }; _ } :: rest_instructions ->
    let type_ = pointer_value_element_type pointer.value in
    let size =
      match type_ with
      | Bool
      | Byte
      | Short
      | Int
      | Long
      | Double
      | Function
      | Pointer _ ->
        operand_size_of_mir_value_type type_
      | Aggregate _ -> failwith "TODO: Cannot compile aggregate literals"
      | Array _ -> failwith "TODO: Cannot compile array literals"
    in
    let value = resolve_ir_value ~allow_imm64:true value in
    let dest =
      match pointer.value.value with
      | Lit (Global { Global.name; _ }) ->
        mk_memory_address ~address:(mk_label_memory_address name) ~type_
      | _ ->
        (match resolve_ir_value pointer with
        | SReg (reg, _) ->
          mk_memory_address
            ~address:{ offset = None; base = RegBase reg; index_and_scale = None }
            ~type_
        | SMem (mem, _) -> mem
        | SImm _
        | SAddr _ ->
          failwith "Expected memory or address")
    in

    (match value with
    | SImm imm -> Gcx.emit ~gcx (`MovIM size) [| imm; dest |]
    | SAddr (addr, type_) ->
      let vreg = mk_vreg ~type_ in
      Gcx.emit ~gcx (`Lea Size64) [| addr; vreg |];
      Gcx.emit ~gcx (`MovMM Size64) [| vreg; dest |]
    | SMem (mem, _) ->
      let vreg = mk_vreg_of_op mem in
      Gcx.emit ~gcx (`MovMM size) [| mem; vreg |];
      Gcx.emit ~gcx (`MovMM size) [| vreg; dest |]
    | SReg (reg, _) -> Gcx.emit ~gcx (`MovMM size) [| reg; dest |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Add
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Add, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = operand_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (`MovMM size) [| other_mem; result_op |];
      Gcx.emit ~gcx (`AddIM size) [| imm; result_op |]
    | (v1, v2) ->
      let size = operand_size_of_svalue v1 in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      let add_op =
        if type_ == Double then
          `AddSD
        else
          `AddMM size
      in
      Gcx.emit ~gcx (`MovMM size) [| mem2; result_op |];
      Gcx.emit ~gcx add_op [| mem1; result_op |]);
    maybe_truncate_bool_operand ~gcx ~if_bool:left_val result_op;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Sub
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Sub, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm left_imm, right) ->
      let right_mem = emit_mem right in
      let size = operand_size_of_svalue right in
      Gcx.emit ~gcx (`MovIM size) [| left_imm; result_op |];
      Gcx.emit ~gcx (`SubMM size) [| right_mem; result_op |]
    | (left, SImm right_imm) ->
      let left_mem = emit_mem left in
      let size = operand_size_of_svalue left in
      Gcx.emit ~gcx (`MovMM size) [| left_mem; result_op |];
      Gcx.emit ~gcx (`SubIM size) [| right_imm; result_op |]
    | (left, right) ->
      let left_mem = emit_mem left in
      let right_mem = emit_mem right in
      let size = operand_size_of_svalue left in
      let sub_op =
        if type_ == Double then
          `SubSD
        else
          `SubMM size
      in
      Gcx.emit ~gcx (`MovMM size) [| left_mem; result_op |];
      Gcx.emit ~gcx sub_op [| right_mem; result_op |]);
    maybe_truncate_bool_operand ~gcx ~if_bool:left_val result_op;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Mul
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Mul, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = min_size16 (operand_size_of_svalue other) in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (`IMulIMR size) [| imm; other_mem; result_op |]
    | (v1, v2) ->
      let size = min_size16 (operand_size_of_svalue v1) in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      let mul_op =
        if type_ == Double then
          `MulSD
        else
          `IMulMR size
      in
      Gcx.emit ~gcx (`MovMM size) [| mem2; result_op |];
      Gcx.emit ~gcx mul_op [| mem1; result_op |]);
    maybe_truncate_bool_operand ~gcx ~if_bool:left_val result_op;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Div
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Div, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (* Floating point divide uses SSE registers and separate instruction *)
    if type_ = Double then (
      let dividend = resolve_ir_value left_val in
      let divisor = resolve_ir_value right_val in
      let size = operand_size_of_svalue dividend in
      let dividend_mem = emit_mem dividend in
      let divisor_mem = emit_mem divisor in
      Gcx.emit ~gcx (`MovMM size) [| dividend_mem; result_op |];
      Gcx.emit ~gcx `DivSD [| divisor_mem; result_op |]
    ) else (
      (match resolve_ir_value ~allow_imm64:true right_val with
      (* Division by a power of two can be optimized to a right shift *)
      | SImm imm
        when Opts.optimize ()
             && Integers.is_power_of_two (int64_of_immediate (cast_to_immediate imm)) ->
        let power_of_two = Integers.power_of_two (int64_of_immediate (cast_to_immediate imm)) in
        let power_of_two_imm = mk_imm ~imm:(Imm8 (Int8.of_int power_of_two)) in
        let left = resolve_ir_value left_val in
        let left_mem = emit_mem left in
        let size = operand_size_of_svalue left in
        Gcx.emit ~gcx (`MovMM size) [| left_mem; result_op |];
        Gcx.emit ~gcx (`SarI size) [| power_of_two_imm; result_op |]
      (* Otherwise emit a divide instruction *)
      | _ ->
        let precolored_a = mk_precolored ~type_ `A in
        let size = gen_idiv left_val right_val in
        Gcx.emit ~gcx (`MovMM size) [| precolored_a; result_op |]);
      maybe_truncate_bool_operand ~gcx ~if_bool:left_val result_op
    );
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Rem
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Rem, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    let precolored_d = mk_precolored ~type_ `D in
    let size = gen_idiv left_val right_val in
    Gcx.emit ~gcx (`MovMM size) [| precolored_d; result_op |];
    maybe_truncate_bool_operand ~gcx ~if_bool:left_val result_op;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Neg
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Unary (Neg, arg); type_; _ }; _ } :: rest_instructions
    ->
    let resolved_value = resolve_ir_value arg in
    let size = operand_size_of_svalue resolved_value in
    let arg_mem = emit_mem resolved_value in
    let result_op = operand_of_value_id ~type_ result_id in

    (* No single instruction for floating point negate. Instead xor sign bit with negate mask *)
    if type_ = Double then (
      Gcx.add_double_negate_mask ~gcx;
      let double_negate_mask =
        mk_memory_address ~address:(mk_label_memory_address double_negate_mask_label) ~type_
      in
      Gcx.emit ~gcx (`MovMM size) [| arg_mem; result_op |];
      Gcx.emit ~gcx `XorPD [| double_negate_mask; result_op |];
      gen_instructions rest_instructions
    ) else (
      Gcx.emit ~gcx (`MovMM size) [| arg_mem; result_op |];
      Gcx.emit ~gcx (`NegM size) [| result_op |];
      maybe_truncate_bool_operand ~gcx ~if_bool:arg result_op;
      gen_instructions rest_instructions
    )
  (*
   * ===========================================
   *                    Not
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Unary (Not, arg); type_; _ }; _ } :: rest_instructions
    ->
    (if is_bool_value arg.value then (
      let arg_reg = emit_bool_as_reg arg in
      let result_op = operand_of_value_id ~type_ result_id in
      Gcx.emit ~gcx (`XorMM Size32) [| result_op; result_op |];
      Gcx.emit ~gcx (`TestMR Size8) [| arg_reg; arg_reg |];
      Gcx.emit ~gcx (`SetCC E) [| result_op |]
    ) else
      let resolved_value = resolve_ir_value arg in
      let size = operand_size_of_svalue resolved_value in
      let arg_mem = emit_mem resolved_value in
      let result_op = operand_of_value_id ~type_ result_id in
      Gcx.emit ~gcx (`MovMM size) [| arg_mem; result_op |];
      Gcx.emit ~gcx (`NotM size) [| result_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    And
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (And, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = operand_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (`MovMM size) [| other_mem; result_op |];
      Gcx.emit ~gcx (`AndIM size) [| imm; result_op |]
    | (v1, v2) ->
      let size = operand_size_of_svalue v1 in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (`MovMM size) [| mem2; result_op |];
      Gcx.emit ~gcx (`AndMM size) [| mem1; result_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Or
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Or, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = operand_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (`MovMM size) [| other_mem; result_op |];
      Gcx.emit ~gcx (`OrIM size) [| imm; result_op |]
    | (v1, v2) ->
      let size = operand_size_of_svalue v1 in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (`MovMM size) [| mem2; result_op |];
      Gcx.emit ~gcx (`OrMM size) [| mem1; result_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Xor
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Xor, left_val, right_val); type_; _ }; _ }
    :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = operand_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (`MovMM size) [| other_mem; result_op |];
      Gcx.emit ~gcx (`XorIM size) [| imm; result_op |]
    | (v1, v2) ->
      let size = operand_size_of_svalue v1 in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (`MovMM size) [| mem2; result_op |];
      Gcx.emit ~gcx (`XorMM size) [| mem1; result_op |]);
    maybe_truncate_bool_operand ~gcx ~if_bool:left_val result_op;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  Shl
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Shl, target_use, shift_use); _ }; _ }
    :: rest_instructions ->
    let result_op =
      gen_shift
        result_id
        target_use
        shift_use
        ~mk_reg_instr:(fun size mem -> Gcx.emit ~gcx (`ShlM size) [| mem |])
        ~mk_imm_instr:(fun size imm mem -> Gcx.emit ~gcx (`ShlI size) [| imm; mem |])
    in
    maybe_truncate_bool_operand ~gcx ~if_bool:target_use result_op;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  Shr
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Shr, target_use, shift_use); _ }; _ }
    :: rest_instructions ->
    (* Arithmetic right shift is a no-op on bools and cannot be represented by sar instruction *)
    if is_bool_value target_use.value then
      gen_mov result_id target_use
    else
      ignore
        (gen_shift
           result_id
           target_use
           shift_use
           ~mk_reg_instr:(fun size mem -> Gcx.emit ~gcx (`SarM size) [| mem |])
           ~mk_imm_instr:(fun size imm mem -> Gcx.emit ~gcx (`SarI size) [| imm; mem |]));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  Shrl
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Binary (Shrl, target_use, shift_use); _ }; _ }
    :: rest_instructions ->
    ignore
      (gen_shift
         result_id
         target_use
         shift_use
         ~mk_reg_instr:(fun size mem -> Gcx.emit ~gcx (`ShrM size) [| mem |])
         ~mk_imm_instr:(fun size imm mem -> Gcx.emit ~gcx (`ShrI size) [| imm; mem |]));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Cmp
   * ===========================================
   *)
  | [
   { id = result_id; value = Instr { instr = Cmp (cmp, left_val, right_val); _ }; _ };
   {
     value =
       Instr
         { instr = Branch { test = { value = { id; value = Instr _ | Argument _; _ }; _ }; _ }; _ };
     _;
   };
  ]
    when result_id == id ->
    let cc = cc_of_mir_comparison cmp (type_of_use left_val) in
    gen_cond_jmp cc result_id left_val right_val
  | { id = result_id; value = Instr { instr = Cmp (cmp, left_val, right_val); _ }; _ }
    :: rest_instructions ->
    let cc = cc_of_mir_comparison cmp (type_of_use left_val) in
    ignore (gen_cmp_set_cc cc result_id left_val right_val);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Terminators
   * ===========================================
   *)
  | [{ value = Instr { instr = Unreachable; _ }; _ }] -> ()
  | [{ value = Instr { instr = Continue continue; _ }; _ }] ->
    (* TODO: Create better structure for tracking relative block locations *)
    Gcx.emit ~gcx `Jmp [| block_op_of_mir_block ~gcx continue |]
  | [{ value = Instr { instr = Branch { test; continue; jump }; _ }; _ }] ->
    let test =
      match test.value.value with
      | Lit _ -> failwith "Dead branch pruning must have already occurred"
      | _ -> test
    in
    let reg = emit_bool_as_reg test in
    Gcx.emit ~gcx (`TestMR Size8) [| reg; reg |];
    Gcx.emit ~gcx (`JmpCC E) [| block_op_of_mir_block ~gcx jump |];
    Gcx.emit ~gcx `Jmp [| block_op_of_mir_block ~gcx continue |]
  | { value = Instr { instr = Ret _ | Continue _ | Branch _ | Unreachable; _ }; _ } :: _ ->
    failwith "Terminator instructions must be last instruction"
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
    let builtin_func = X86_64_builtin.get_asm_builtin mir_builtin in
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
        (* Generic case for all otehr builtins *)
        let param_mir_types = List.map type_of_use args in
        let param_types = calling_convention#calculate_param_types param_mir_types in
        gen_call_arguments param_types args;
        param_types
    in

    (* Call builtin function *)
    Gcx.emit ~gcx (`CallL (param_types, calling_convention)) [| mk_function_op ~func:builtin_func |];

    (* Move return value to result register *)
    (match builtin_func.return_type with
    | None -> ()
    | Some return_mir_type ->
      let return_reg = calling_convention#calculate_return_register return_mir_type in
      let return_reg_op = mk_precolored ~type_:return_type return_reg in
      let return_op = operand_of_value_id ~type_:return_type result_id in
      Gcx.emit ~gcx (`MovMM Size64) [| return_reg_op; return_op |]);

    gen_instructions rest_instructions
  (*
   * ===========================================
   *                 GetPointer
   * ===========================================
   *)
  | ({ value = Instr { instr = GetPointer get_pointer_instr; _ }; _ } as instr) :: rest_instructions
    ->
    gen_get_pointer ~gcx instr get_pointer_instr;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Cast
   * ===========================================
   *)
  | { id = result_id; value = Instr { instr = Cast arg_val; type_; _ }; _ } :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (* Cast simply copies argument to result operand, which will likely be optimized away *)
    (match resolve_ir_value ~allow_imm64:true arg_val with
    | SImm imm ->
      let size = size_of_immediate (cast_to_immediate imm) in
      Gcx.emit ~gcx (`MovIM size) [| imm; result_op |]
    | SAddr (addr, _) -> Gcx.emit ~gcx (`Lea Size64) [| addr; result_op |]
    | SMem (mem, size) -> Gcx.emit ~gcx (`MovMM size) [| mem; result_op |]
    | SReg (reg, size) -> Gcx.emit ~gcx (`MovMM size) [| reg; result_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  Trunc
   * ===========================================
   *)
  | { id = result_id; value = Instr { type_; instr = Trunc arg_val; _ }; _ } :: rest_instructions ->
    (* Truncation occurs when later accessing only a portion of the arg. Emit mov to link arg and
       result, which will likely be optimized away (but may not be optimized away if we end up
       moving the truncated portion to memory). *)
    let result_op = operand_of_value_id ~type_ result_id in
    (match resolve_ir_value arg_val with
    | SImm _ -> failwith "Constants must be folded before gen"
    | arg ->
      let size = operand_size_of_mir_value_type (type_ :> Type.t) in
      let arg_mem = emit_mem arg in
      Gcx.emit ~gcx (`MovMM size) [| arg_mem; result_op |]);
    (* Bools must be further truncated to only lowest bit *)
    (if type_ = Bool then
      let mask_imm = mk_imm ~imm:(Imm8 (Int8.of_int 1)) in
      Gcx.emit ~gcx (`AndIM Size8) [| mask_imm; result_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  SExt
   * ===========================================
   *)
  | { id = result_id; value = Instr { type_; instr = SExt arg_val; _ }; _ } :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (match resolve_ir_value arg_val with
    | SImm _ -> failwith "Constants must be folded before gen"
    | arg ->
      let arg_size = operand_size_of_svalue arg in
      let result_size = operand_size_of_mir_value_type (type_ :> Type.t) in
      let arg_mem = emit_mem arg in
      if arg_size <> result_size then
        Gcx.emit ~gcx (`MovSX (arg_size, result_size)) [| arg_mem; result_op |]
      else
        Gcx.emit ~gcx (`MovMM arg_size) [| arg_mem; result_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  ZExt
   * ===========================================
   *)
  | { id = result_id; value = Instr { type_; instr = ZExt arg_val; _ }; _ } :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (match resolve_ir_value arg_val with
    | SImm _ -> failwith "Constants must be folded before gen"
    | arg ->
      let arg_size = operand_size_of_svalue arg in
      let result_size = operand_size_of_mir_value_type (type_ :> Type.t) in
      let arg_mem = emit_mem arg in
      if arg_size <> result_size then
        Gcx.emit ~gcx (`MovZX (arg_size, result_size)) [| arg_mem; result_op |]
      else
        Gcx.emit ~gcx (`MovMM arg_size) [| arg_mem; result_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                IntToFloat
   * ===========================================
   *)
  | { id = result_id; value = Instr { type_; instr = IntToFloat arg_val; _ }; _ }
    :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (match resolve_ir_value arg_val with
    | SImm _ -> failwith "Constants must be folded before gen"
    | arg ->
      let int_size = operand_size_of_svalue arg in
      let arg_mem = emit_mem arg in
      (* Sign extend argument to 32-bits if smaller *)
      if int_size == Size8 || int_size == Size16 then (
        let vreg = mk_vreg_of_op arg_mem in
        Gcx.emit ~gcx (`MovSX (int_size, Size32)) [| arg_mem; vreg |];
        Gcx.emit ~gcx (`ConvertIntToFloat Size32) [| vreg; result_op |]
      ) else
        Gcx.emit ~gcx (`ConvertIntToFloat int_size) [| arg_mem; result_op |]);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                FloatToInt
   * ===========================================
   *)
  | { id = result_id; value = Instr { type_; instr = FloatToInt arg_val; _ }; _ }
    :: rest_instructions ->
    let result_op = operand_of_value_id ~type_ result_id in
    (match resolve_ir_value arg_val with
    | SImm _ -> failwith "Constants must be folded before gen"
    | arg ->
      let int_size = min_size32 (operand_size_of_mir_value_type type_) in
      let arg_mem = emit_mem arg in
      Gcx.emit ~gcx (`ConvertFloatToInt int_size) [| arg_mem; result_op |]);
    gen_instructions rest_instructions
  | { value = Instr { instr = Mir.Instruction.Phi _; _ }; _ } :: _ ->
    failwith "Phi nodes must be removed before asm gen"
  | { value = Instr { instr = Mir.Instruction.StackAlloc _; _ }; _ } :: _ ->
    failwith "StackAlloc instructions removed before asm gen"
  | { value = Lit _ | Argument _; _ } :: _ -> failwith "Expected instruction value"

and gen_get_pointer
    ~gcx (get_pointer_instr_val : Mir.Value.t) (get_pointer_instr : Mir.Instruction.GetPointer.t) =
  let open Mir.Instruction.GetPointer in
  let result_id = get_pointer_instr_val.id in
  let { Mir.Instruction.type_ = return_type; _ } = cast_to_instruction get_pointer_instr_val in
  let { pointer; pointer_offset; offsets } = get_pointer_instr in
  let element_ty = pointer_value_element_type pointer.value in

  (* Utilities for creating operands *)
  let operand_of_value_id result_id = mk_virtual_register_of_value_id ~value_id:result_id in
  let mk_vreg ~type_ = mk_virtual_register ~type_ in
  let mk_vreg_of_op op = mk_virtual_register ~type_:op.Operand.type_ in

  (* Current address calculation - updated as offsets are visited. Note that base and index_and_scale
     can only contain 64-bit registers. *)
  let offset = ref None in
  let base = ref X86_64_MemoryAddress.NoBase in
  let index_and_scale = ref None in

  (* Zero extend index register to 64 bits for address calculation *)
  let zero_extend_reg_to_size64 reg size =
    match size with
    | Size64 -> reg
    | _ ->
      let zero_extended_vreg = mk_vreg ~type_:Long in
      Gcx.emit ~gcx (`MovZX (size, Size64)) [| reg; zero_extended_vreg |];
      zero_extended_vreg
  in
  let set_base reg size = base := RegBase (zero_extend_reg_to_size64 reg size) in
  let set_index_and_scale reg size scale =
    let reg = zero_extend_reg_to_size64 reg size in
    index_and_scale := Some (reg, scale)
  in

  (* Emit the current address calculation as a Lea instruction, returning the resulting var and
     storing it as the base of the next instruction. If only a register base is present, do not emit
     an instruction and simply return that register. *)
  let emit_current_address_calculation () =
    let result_vreg =
      match (!offset, !base, !index_and_scale) with
      | (None, RegBase reg, None) -> reg
      | (None, NoBase, None) ->
        let result_vreg = mk_vreg ~type_:Int in
        Gcx.emit ~gcx (`XorMM Size32) [| result_vreg; result_vreg |];
        result_vreg
      | (offset, base, index_and_scale) ->
        let result_vreg = mk_vreg ~type_:Long in
        let addr =
          mk_memory_address
            ~address:{ X86_64_MemoryAddress.offset; base; index_and_scale }
            ~type_:Long
        in
        Gcx.emit ~gcx (`Lea Size64) [| addr; result_vreg |];
        result_vreg
    in

    (* Set up next address calculation *)
    base := RegBase result_vreg;
    offset := None;
    index_and_scale := None;
    result_vreg
  in

  (* Add a 32-bit constant to the current address calculation *)
  let add_fixed_offset new_offset =
    match !offset with
    (* If there is not already an offset, add one *)
    | None -> offset := Some (ImmediateOffset new_offset)
    (* Cannot combine label offset with immediate offset, so emit current address calculation
       instruction and start new address calculation with immediate offset. *)
    | Some (LabelOffset _) ->
      ignore (emit_current_address_calculation ());
      offset := Some (ImmediateOffset new_offset)
      (* Combine immediate offsets if they fit in 32-bit immediate *)
    | Some (ImmediateOffset current_offset) ->
      let full_offset = Int64.add (Int64.of_int32 current_offset) (Int64.of_int32 new_offset) in
      if not (Integers.is_out_of_unsigned_int_range full_offset) then
        offset := Some (ImmediateOffset (Int64.to_int32 full_offset))
      else (
        (* Otherwise emit current address calculation instruction for old offset and start new
           address calculation with new offset. *)
        ignore (emit_current_address_calculation ());
        offset := Some (ImmediateOffset new_offset)
      )
  in

  (* Add a register with a known scale to the current address calculation *)
  let add_scaled_register (reg, size) scale =
    (* Add an unscaled register to current address calculation. First try to add it to base if there
       is no base yet, then try adding to index with scale = 1 if there is no index, otherwise
       emit current address calculation and try again. *)
    let rec add_unscaled_register reg size =
      if !base = NoBase then
        set_base reg size
      else if !base != IPBase && !index_and_scale = None then
        set_index_and_scale reg size Scale1
      else (
        ignore (emit_current_address_calculation ());
        add_unscaled_register reg size
      )
    in
    (* Add a scaled register to current address calculation. If there is already a scaled register
       then emit the current address calculation and try again. *)
    let rec add_scaled_register reg size scale =
      if !base != IPBase && !index_and_scale = None then
        set_index_and_scale reg size scale
      else (
        ignore (emit_current_address_calculation ());
        add_scaled_register reg size scale
      )
    in
    match scale with
    | 1 -> add_unscaled_register reg size
    | 2 -> add_scaled_register reg size Scale2
    | 4 -> add_scaled_register reg size Scale4
    | 8 -> add_scaled_register reg size Scale8
    (* Otherwise emit multiply to calculate index *)
    | scale ->
      ignore (emit_current_address_calculation ());
      let scaled_vreg = mk_vreg ~type_:Long in
      let scale_imm = mk_imm ~imm:(Imm32 (Int32.of_int scale)) in
      (* TODO: Handle sign extending byte arguments to 32/64 bits (movzbl/q) *)
      Gcx.emit ~gcx (`IMulIMR size) [| scale_imm; reg; scaled_vreg |];
      add_unscaled_register scaled_vreg size
  in

  (* Add address of root pointer *)
  (match pointer.value.value with
  | Lit (Global global) ->
    offset := Some (LabelOffset (label_of_mir_label global.name));
    base := IPBase
  | Lit (NullPointer _) -> ()
  | _ ->
    (match resolve_ir_value ~gcx pointer with
    | SReg (reg, size) -> set_base reg size
    | SMem (mem, size) ->
      let vreg = mk_vreg_of_op mem in
      (* Must zero extend to 64 bits *)
      (match size with
      | Size32
      | Size64 ->
        Gcx.emit ~gcx (`MovMM size) [| mem; vreg |]
      | _ -> Gcx.emit ~gcx (`MovZX (size, Size64)) [| mem; vreg |]);
      set_base vreg Size64
    | _ -> failwith "PointerV must resolve to SReg or SMem"));

  (* The type that is currently being indexed into *)
  let current_ty = ref element_ty in

  let gen_offset offset ty =
    match offset with
    | PointerIndex pointer_offset ->
      (* TODO: Handle sign extending byte arguments to 32/64 bits (movzbl/q) *)
      let element_size =
        match ty with
        | Type.Array (ty, _) -> Gcx.size_of_mir_type ~gcx ty
        | _ -> Gcx.size_of_mir_type ~gcx ty
      in
      (match resolve_ir_value ~gcx ~allow_imm64:true pointer_offset with
      | SImm imm ->
        let num_elements = int64_of_immediate (cast_to_immediate imm) in
        if num_elements <> Int64.zero then (
          (* Check if calculated offset fits in 64-bit immediate *)
          let offset = Int64.mul num_elements (Int64.of_int element_size) in
          if not (Integers.is_out_of_unsigned_int_range offset) then
            add_fixed_offset (Int64.to_int32 offset)
          else
            (* If not, 64-bit immediate offset must first be loaded into register *)
            let offset_imm = mk_imm ~imm:(Imm64 offset) in
            let vreg = mk_vreg ~type_:Long in
            Gcx.emit ~gcx (`MovIM Size64) [| offset_imm; vreg |];
            add_scaled_register (vreg, Size64) 1
        )
      | SReg (reg, size) -> add_scaled_register (reg, size) element_size
      | SMem (mem, size) ->
        let vreg = mk_vreg_of_op mem in
        Gcx.emit ~gcx (`MovMM size) [| mem; vreg |];
        add_scaled_register (vreg, size) element_size
      | SAddr _ -> failwith "PointerIndex cannot be resolved to SAddr")
    | FieldIndex element_index ->
      (match ty with
      | Aggregate ({ Aggregate.elements; _ } as agg) ->
        (* Find offset of aggregate element in aggregate's layout, add add it to address *)
        let agg_layout = Gcx.get_agg_layout ~gcx agg in
        let { AggregateElement.offset; _ } = AggregateLayout.get_element agg_layout element_index in
        if offset <> 0 then add_fixed_offset (Int32.of_int offset);
        (* Update current type to element type *)
        let (_, element_ty) = List.nth elements element_index in
        current_ty := element_ty
      | _ -> failwith "FieldIndex must index into aggregate type")
  in

  (* Visit all offsets *)
  (match pointer_offset with
  | Some pointer_offset -> gen_offset (PointerIndex pointer_offset) element_ty
  | None -> ());
  List.iter (fun offset -> gen_offset offset !current_ty) offsets;

  let address_op = emit_current_address_calculation () in
  let result_op = operand_of_value_id ~type_:return_type result_id in
  Gcx.emit ~gcx (`MovMM Size64) [| address_op; result_op |]

and gen_size_from_count_and_type ~gcx count_use count_param_type count_param_mir_type mir_ty =
  let element_size = Gcx.size_of_mir_type ~gcx mir_ty in
  let mk_result_op () =
    match count_param_type with
    | ParamInRegister reg -> mk_precolored ~type_:count_param_mir_type reg
    | ParamOnStack _ -> failwith "Cannot pass builtin size argument on stck"
  in
  match count_use.value.value with
  (* If count is a literal precalculate total requested size and fit into smallest immediate *)
  | Lit ((Byte _ | Int _ | Long _) as count_lit) ->
    let count = int64_of_literal count_lit in
    let total_size = Int64.mul count (Int64.of_int element_size) in
    let (size, total_size_imm) =
      if Integers.is_out_of_unsigned_int_range total_size then
        (Size64, Imm64 total_size)
      else
        (Size32, Imm32 (Int64.to_int32 total_size))
    in
    Gcx.emit ~gcx (`MovIM size) [| mk_imm ~imm:total_size_imm; mk_result_op () |]
  (* If count is a variable multiply by size before putting in argument register *)
  | _ ->
    let count_reg =
      match resolve_ir_value ~gcx count_use with
      | SReg (count_reg, _) -> count_reg
      | _ -> failwith "Must be register"
    in
    (* Check for special case where element size is a single byte - no multiplication required *)
    if element_size = 1 then
      Gcx.emit ~gcx (`MovMM Size32) [| count_reg; mk_result_op () |]
    else
      let size_imm = mk_imm ~imm:(Imm32 (Int32.of_int element_size)) in
      (* TODO: Could overflow Size32, but count has Size32 so we do not want to use Size64 directly *)
      Gcx.emit ~gcx (`IMulIMR Size32) [| size_imm; count_reg; mk_result_op () |]

(* Truncate a single byte operand to just its lowest bit if the test val has type bool *)
and maybe_truncate_bool_operand ~gcx ~if_bool op =
  if is_bool_value if_bool.value then
    Gcx.emit ~gcx (`AndIM Size8) [| mk_imm ~imm:(Imm8 (Int8.of_int 1)); op |]

and resolve_ir_value ~gcx ?(allow_imm64 = false) (use : Use.t) =
  match use.value.value with
  | Lit (Bool b) ->
    let int_8 =
      Int8.of_int
        (if b then
          1
        else
          0)
    in
    SImm (mk_imm ~imm:(Imm8 int_8))
  | Lit (Byte b) -> SImm (mk_imm ~imm:(Imm8 b))
  (* Int literals can be downgraded to an 8 byte literal if they fit *)
  | Lit (Int i) -> SImm (mk_imm ~imm:(Imm32 i))
  (* Long literals can be downgraded to a 32 byte int literal if it fits. Otherwise 64 bit literal
     must first be loaded to a register with a mov instruction. *)
  | Lit (Long l) ->
    if allow_imm64 then
      SImm (mk_imm ~imm:(Imm64 l))
    else if not (Integers.is_out_of_signed_int_range l) then
      SImm (mk_imm ~imm:(Imm32 (Int64.to_int32 l)))
    else
      let vreg = mk_virtual_register ~type_:Long in
      Gcx.emit ~gcx (`MovIM Size64) [| mk_imm ~imm:(Imm64 l); vreg |];
      SReg (vreg, Size64)
  (* Double literals cannot be immediates and must be loaded from memory. Write as constant in
     data section while deduplicating double literals. *)
  | Lit (Double d) ->
    let literal_label = Gcx.get_float_literal ~gcx d in
    let op = mk_memory_address ~address:(mk_label_memory_address literal_label) ~type_:Double in
    SMem (op, Size64)
  | Lit (Function { name; _ })
  | Lit (Global { name; _ }) ->
    let address = mk_label_memory_address name in
    let type_ = type_of_use use in
    SAddr (mk_memory_address ~address ~type_, type_)
  | Lit (NullPointer _) -> SImm (mk_imm ~imm:(Imm64 0L))
  | Lit (ArrayString _)
  | Lit (ArrayVtable _) ->
    failwith "TODO: Cannot compile array literals"
  | Lit (AggregateClosure _) -> failwith "TODO: Cannot compile aggregate literals"
  | Instr { type_; _ }
  | Argument { type_; _ } ->
    let op = mk_virtual_register_of_value_id ~value_id:use.value.id ~type_ in
    let size = operand_size_of_mir_value_type type_ in
    if Operand.is_memory_value op then
      SMem (op, size)
    else
      SReg (op, size)

and operand_size_of_mir_value_type value_type =
  match value_type with
  | Bool
  | Byte ->
    Size8
  | Short -> Size16
  | Int -> Size32
  | Long
  | Double
  | Function
  | Pointer _ ->
    Size64
  | Aggregate _ -> failwith "TODO: Cannot compile aggregate structure literals"
  | Array _ -> failwith "TODO: Cannot compile array literals"

and operand_size_of_svalue value =
  match value with
  | SImm op -> size_of_immediate (cast_to_immediate op)
  | SReg (_, size) -> size
  | SMem (_, size) -> size
  | SAddr _ -> Size64

and block_op_of_mir_block ~gcx mir_block =
  mk_block_op ~block:(Gcx.get_block_from_mir_block ~gcx mir_block)

and min_size16 size =
  match size with
  | Size8 -> Size16
  | _ -> size

and min_size32 size =
  match size with
  | Size8
  | Size16 ->
    Size32
  | _ -> size

and cc_of_mir_comparison cmp mir_type =
  match (cmp, mir_type) with
  | (Mir.Instruction.Eq, Type.Double) -> AndCC (E, NP)
  | (Neq, Double) -> OrCC (NE, P)
  | (Lt, Double) -> SingleCC B
  | (LtEq, Double) -> SingleCC BE
  | (Gt, Double) -> SingleCC A
  | (GtEq, Double) -> SingleCC AE
  | (Eq, _) -> SingleCC E
  | (Neq, _) -> SingleCC NE
  | (Lt, _) -> SingleCC L
  | (LtEq, _) -> SingleCC LE
  | (Gt, _) -> SingleCC G
  | (GtEq, _) -> SingleCC GE

and mk_label_memory_address label =
  {
    X86_64_MemoryAddress.offset = Some (LabelOffset (label_of_mir_label label));
    base = IPBase;
    index_and_scale = None;
  }

and is_zero_size_global use =
  match use.value.value with
  | Lit (Global { name; _ }) when name = zero_size_name -> true
  | _ -> false