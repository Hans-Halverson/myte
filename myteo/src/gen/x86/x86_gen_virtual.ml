open Basic_collections
open Mir
open Mir_type
open X86_gen_context
open X86_instructions
open X86_layout

type resolved_source_value =
  (* An immediate value *)
  | SImm of immediate
  (* Value is a virtual register *)
  | SVReg of VReg.t * register_size
  (* Value is the contents at a memory location *)
  | SMem of VReg.t memory_address * register_size
  (* Value is a memory address *)
  | SAddr of VReg.t memory_address

let invalid_label_chars = Str.regexp "[<>,*]"

let rec gen ~gcx (ir : ssa_program) =
  (* Calculate layout of all aggregate types *)
  SMap.iter (fun _ agg -> Gcx.build_agg_layout ~gcx agg) ir.types;

  (* Generate all globals in program *)
  SMap.iter (fun _ global -> gen_global_instruction_builder ~gcx ~ir global) ir.globals;

  (* Generate all functions in program *)
  SMap.iter (fun _ func -> gen_function_instruction_builder ~gcx ~ir func) ir.funcs;

  (* Generate entrypoint *)
  gen_entrypoint ~gcx ir;
  Gcx.finish_builders ~gcx

(* Generate the _start entrypoint function for the executable. The entrypoint function initializes
   the myte runtime, initializes all global variables if necessary, and finally calls the main
   function. *)
and gen_entrypoint ~gcx ir =
  let func = Gcx.start_function ~gcx [] 0 in
  Gcx.start_block ~gcx ~label:(Some start_label) ~func:func.id ~mir_block_id:None;
  let prologue_block_id = (Option.get gcx.current_block_builder).id in
  func.prologue <- prologue_block_id;
  Gcx.emit ~gcx (CallL X86_runtime.myte_init_label);
  (* Call the init function if it exists *)
  if SMap.mem init_func_name ir.funcs then Gcx.emit ~gcx (CallL init_func_name);
  Gcx.emit ~gcx (CallL main_label);
  Gcx.emit ~gcx Ret;
  Gcx.finish_block ~gcx;
  Gcx.finish_function ~gcx

and gen_global_instruction_builder ~gcx ~ir:_ global =
  let label = label_of_mir_label global.name in
  match global.init_val with
  | None ->
    (* If uninitialized, place global variable in bss section *)
    let size = Gcx.size_of_mir_type ~gcx global.ty in
    Gcx.add_bss ~gcx { label; size }
  | Some (`ArrayL (_, _, data)) ->
    (* Array literal is known at compile time, so insert into initialized data section *)
    Gcx.add_data ~gcx { label; value = AsciiData data }
  | Some init_val ->
    (match resolve_ir_value ~gcx ~func:0 ~allow_imm64:true init_val with
    | SImm imm ->
      (* Global is initialized to immediate, so insert into initialized data section *)
      let data = { label; value = ImmediateData imm } in
      Gcx.add_data ~gcx data
    | SAddr _
    | SVReg _
    | SMem _ ->
      failwith "Global init value must be a constant")

and gen_function_instruction_builder ~gcx ~ir func =
  let func_ = Gcx.start_function ~gcx [] 0 in
  let label =
    if func.body_start_block = ir.main_id then
      main_label
    else
      label_of_mir_label func.name
  in
  (* Create function prologue which copies all params from physical registers to temporaries *)
  Gcx.start_block ~gcx ~label:(Some label) ~func:func_.id ~mir_block_id:None;
  let prologue_block_id = (Option.get gcx.current_block_builder).id in
  func_.prologue <- prologue_block_id;
  func_.params <-
    List.mapi
      (fun i (_, var_id, value_type) ->
        (* First 6 parameters are passed in known registers *)
        let size = register_size_of_mir_value_type value_type in
        let move_from_precolored color =
          let param_vreg = VReg.of_var_id ~resolution:Unresolved ~func:(Some func_.id) var_id in
          Gcx.emit ~gcx (MovMM (size, Reg (Gcx.mk_precolored ~gcx color), Reg param_vreg));
          param_vreg
        in
        match i with
        | 0 -> move_from_precolored DI
        | 1 -> move_from_precolored SI
        | 2 -> move_from_precolored D
        | 3 -> move_from_precolored C
        | 4 -> move_from_precolored R8
        | 5 -> move_from_precolored R9
        (* All other parameters pushed onto stack before call. Address will be calculated once
           we know stack frame size after stack coloring. *)
        | _ ->
          let vreg = VReg.of_var_id ~resolution:Unresolved ~func:(Some func_.id) var_id in
          vreg.resolution <- StackSlot (FunctionStackArgument vreg);
          vreg)
      func.params;
  Gcx.emit ~gcx (Jmp (Gcx.get_block_id_from_mir_block_id ~gcx func.body_start_block));
  Gcx.finish_block ~gcx;
  gen_blocks ~gcx ~ir func.body_start_block None func_.id;
  Gcx.finish_function ~gcx

and gen_blocks ~gcx ~ir start_block_id label func =
  let ordered_blocks = Block_ordering.order_blocks ~program:ir start_block_id in
  List.iteri
    (fun i mir_block_id ->
      let mir_block = IMap.find mir_block_id ir.blocks in
      let label =
        if i = 0 then
          label
        else
          None
      in
      Gcx.start_block ~gcx ~label ~func ~mir_block_id:(Some mir_block_id);
      gen_instructions ~gcx ~ir ~func ~block:mir_block (List.map snd mir_block.instructions);
      Gcx.finish_block ~gcx)
    ordered_blocks

and gen_instructions ~gcx ~ir ~func ~block instructions =
  let open Instruction in
  let gen_instructions = gen_instructions ~gcx ~ir ~func ~block in
  let vreg_of_result_var_id var_id =
    VReg.of_var_id ~resolution:Unresolved ~func:(Some func) var_id
  in
  let mk_vreg () = VReg.mk ~resolution:Unresolved ~func:(Some func) in
  let resolve_ir_value ?(allow_imm64 = false) v =
    resolve_ir_value ~gcx ~func ~allow_imm64 (v :> var_id Value.t)
  in
  let is_cond_jump var_id =
    match block.next with
    | Branch { test = `BoolV test_var_id; _ } when test_var_id = var_id -> true
    | _ -> false
  in
  let emit_mem mem =
    match mem with
    | SVReg (vreg, _) -> Reg vreg
    | SMem (mem, _) -> Mem mem
    | SAddr addr ->
      let vreg = mk_vreg () in
      Gcx.emit ~gcx (Lea (Size64, addr, vreg));
      Reg vreg
    | _ -> failwith "Only called on address, memory location, or vreg"
  in
  (* Return preferred (source, dest) args for a commutative binary operation. We try to avoid having
     the destination be a memory location, so source always contains memory location if one exists. *)
  let choose_commutative_source_dest_arg_order v1 v2 =
    match (v1, v2) with
    | (SMem _, _) -> (v1, v2)
    | (_, SMem _) -> (v2, v1)
    | _ -> (v1, v2)
  in
  let gen_call_arguments arg_vals =
    (* Arguments 7+ are pushed on stack in reverse order *)
    let stack_arg_vals = List.rev (List_utils.drop 6 arg_vals) in
    List.iter
      (fun arg_val ->
        match resolve_ir_value arg_val with
        | SImm imm -> Gcx.emit ~gcx (PushI imm)
        (* Address must be calculated in a register and then pushed onto stack *)
        | SAddr addr ->
          let vreg = mk_vreg () in
          Gcx.emit ~gcx (Lea (Size64, addr, vreg));
          Gcx.emit ~gcx (PushM (Reg vreg))
        | SMem (mem, _) -> Gcx.emit ~gcx (PushM (Mem mem))
        | SVReg (var_id, _) -> Gcx.emit ~gcx (PushM (Reg var_id)))
      stack_arg_vals;
    (* First six arguments are placed in registers %rdi​, ​%rsi​, ​%rdx​, ​%rcx​, ​%r8​, and ​%r9​ *)
    List.iteri
      (fun i arg_val ->
        if i >= 6 then
          ()
        else
          match register_of_param i with
          | None -> ()
          | Some color ->
            let vreg = Gcx.mk_precolored ~gcx color in
            (match resolve_ir_value ~allow_imm64:true arg_val with
            | SImm imm -> Gcx.emit ~gcx (MovIM (size_of_immediate imm, imm, Reg vreg))
            | SAddr addr -> Gcx.emit ~gcx (Lea (Size64, addr, vreg))
            | SMem (mem, size) -> Gcx.emit ~gcx (MovMM (size, Mem mem, Reg vreg))
            | SVReg (source_vreg, size) -> Gcx.emit ~gcx (MovMM (size, Reg source_vreg, Reg vreg))))
      arg_vals
  in
  let gen_idiv left_val right_val =
    let precolored_a = Gcx.mk_precolored ~gcx A in
    match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm dividend_imm, divisor) ->
      let size = register_size_of_svalue divisor in
      let divisor_mem = emit_mem divisor in
      Gcx.emit ~gcx (MovIM (size, dividend_imm, Reg precolored_a));
      Gcx.emit ~gcx (IDiv (size, divisor_mem));
      size
    | (dividend, SImm divisor_imm) ->
      let size = register_size_of_svalue dividend in
      let dividend_mem = emit_mem dividend in
      let divisor_vreg = mk_vreg () in
      Gcx.emit ~gcx (MovMM (size, dividend_mem, Reg precolored_a));
      Gcx.emit ~gcx (MovIM (size, divisor_imm, Reg divisor_vreg));
      Gcx.emit ~gcx (IDiv (size, Reg divisor_vreg));
      size
    | (dividend, divisor) ->
      let size = register_size_of_svalue dividend in
      let dividend_mem = emit_mem dividend in
      let divisor_mem = emit_mem divisor in
      Gcx.emit ~gcx (MovMM (size, dividend_mem, Reg precolored_a));
      Gcx.emit ~gcx (IDiv (size, divisor_mem));
      size
  in
  (* Generate a not instruction applied to a partiuclar argument *)
  let gen_not result_var_id arg =
    let resolved_value = resolve_ir_value arg in
    let size = register_size_of_svalue resolved_value in
    let arg_mem = emit_mem resolved_value in
    let result_vreg = vreg_of_result_var_id result_var_id in
    Gcx.emit ~gcx (MovMM (size, arg_mem, Reg result_vreg));
    Gcx.emit ~gcx (NotM (size, Reg result_vreg))
  in
  (* Generate an and instruction between two arguments *)
  let gen_and result_var_id left_val right_val =
    let result_vreg = vreg_of_result_var_id result_var_id in
    match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = register_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (MovMM (size, other_mem, Reg result_vreg));
      Gcx.emit ~gcx (AndIM (size, imm, Reg result_vreg))
    | (v1, v2) ->
      let size = register_size_of_svalue v1 in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (MovMM (size, mem2, Reg result_vreg));
      Gcx.emit ~gcx (AndMM (size, mem1, Reg result_vreg))
  in
  (* Generate an or instruction between two arguments *)
  let gen_or result_var_id left_val right_val =
    let result_vreg = vreg_of_result_var_id result_var_id in
    match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = register_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (MovMM (size, other_mem, Reg result_vreg));
      Gcx.emit ~gcx (OrIM (size, imm, Reg result_vreg))
    | (v1, v2) ->
      let size = register_size_of_svalue v1 in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (MovMM (size, mem2, Reg result_vreg));
      Gcx.emit ~gcx (OrMM (size, mem1, Reg result_vreg))
  in
  (* Generate a cmp instruction between two arguments. Return whether order was swapped. *)
  let gen_cmp left_val right_val =
    match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    (* Comparison to immediate - swap arguments if necessary *)
    | (SImm imm, other) ->
      let size = register_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (CmpMI (size, other_mem, imm));
      true
    | (other, SImm imm) ->
      let size = register_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (CmpMI (size, other_mem, imm));
      false
    (* Cannot compare two memory locations at the same time *)
    | (SMem (mem1, size), SMem (mem2, _)) ->
      let vreg = mk_vreg () in
      Gcx.emit ~gcx (MovMM (size, Mem mem1, Reg vreg));
      Gcx.emit ~gcx (CmpMM (size, Reg vreg, Mem mem2));
      false
    | (v1, v2) ->
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (CmpMM (register_size_of_svalue v1, mem1, mem2));
      false
  in
  (* Generate a bit shift instruction from the target and shift arguments *)
  let gen_shift ~mk_reg_instr ~mk_imm_instr result_var_id target_val shift_val =
    let result_vreg = vreg_of_result_var_id result_var_id in
    (* Do not reduce size of target immediate, as we must know its original size to know what
       size to make the shift operation. *)
    match (resolve_ir_value target_val, resolve_ir_value ~allow_imm64:true shift_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm target_imm, shift) ->
      let shift_size = register_size_of_svalue shift in
      let precolored_c = Gcx.mk_precolored ~gcx C in
      let shift_mem = emit_mem shift in
      Gcx.emit ~gcx (MovMM (shift_size, shift_mem, Reg precolored_c));
      Gcx.emit ~gcx (MovIM (shift_size, target_imm, Reg result_vreg));
      Gcx.emit ~gcx (mk_reg_instr shift_size (Reg result_vreg))
    | (target, SImm shift_imm) ->
      let size = register_size_of_svalue target in
      let target_mem = emit_mem target in
      let shift_value = int64_of_immediate shift_imm in
      if not (Integers.is_out_of_unsigned_byte_range shift_value) then (
        (* 8-byte shift immediate is used directly in operation *)
        Gcx.emit ~gcx (MovMM (size, target_mem, Reg result_vreg));
        Gcx.emit ~gcx (mk_imm_instr size shift_imm (Reg result_vreg))
      ) else
        (* Other shift immediates must first be moved to register *)
        let precolored_c = Gcx.mk_precolored ~gcx C in
        Gcx.emit ~gcx (MovIM (size, shift_imm, Reg precolored_c));
        Gcx.emit ~gcx (MovMM (size, target_mem, Reg result_vreg));
        Gcx.emit ~gcx (mk_reg_instr size (Reg result_vreg))
    | (target, shift) ->
      let target_size = register_size_of_svalue target in
      let shift_size = register_size_of_svalue shift in
      let precolored_c = Gcx.mk_precolored ~gcx C in
      let shift_mem = emit_mem shift in
      Gcx.emit ~gcx (MovMM (shift_size, shift_mem, Reg precolored_c));
      let target_mem = emit_mem target in
      Gcx.emit ~gcx (MovMM (target_size, target_mem, Reg result_vreg));
      Gcx.emit ~gcx (mk_reg_instr target_size (Reg result_vreg))
  in
  let gen_cond_jmp cc left_val right_val =
    let swapped = gen_cmp left_val right_val in
    let cc =
      invert_condition_code
        ( if swapped then
          swap_condition_code_order cc
        else
          cc )
    in
    let (continue, jump) =
      match block.next with
      | Branch { continue; jump; _ } -> (continue, jump)
      | _ -> failwith "Only called on blocks with conditional branches"
    in
    Gcx.emit ~gcx (JmpCC (cc, Gcx.get_block_id_from_mir_block_id ~gcx jump));
    Gcx.emit ~gcx (Jmp (Gcx.get_block_id_from_mir_block_id ~gcx continue))
  in
  let gen_set_cc cc result_var_id left_val right_val =
    let result_vreg = vreg_of_result_var_id result_var_id in
    Gcx.emit ~gcx (XorMM (Size32, Reg result_vreg, Reg result_vreg));
    let swapped = gen_cmp left_val right_val in
    let cc =
      if swapped then
        swap_condition_code_order cc
      else
        cc
    in
    Gcx.emit ~gcx (SetCC (cc, Reg result_vreg))
  in
  match instructions with
  | [] ->
    (* Conditional jump when the condition is in a variable *)
    (match block.next with
    | Branch { test = `BoolL _; _ } -> failwith "Dead branch pruning must have already occurred"
    | Continue continue ->
      (* TODO: Create better structure for tracking relative block locations *)
      Gcx.emit ~gcx (Jmp (Gcx.get_block_id_from_mir_block_id ~gcx continue))
    | Branch { test = `BoolV _ as test; continue; jump } ->
      let vreg =
        match resolve_ir_value test with
        | SVReg (vreg, _) -> vreg
        | SMem (mem, size) ->
          let vreg = mk_vreg () in
          Gcx.emit ~gcx (MovMM (size, Mem mem, Reg vreg));
          vreg
        | _ -> failwith "Boolean variable must be vreg or memory location"
      in
      Gcx.emit ~gcx (TestMR (Size8, Reg vreg, vreg));
      Gcx.emit ~gcx (JmpCC (E, Gcx.get_block_id_from_mir_block_id ~gcx jump));
      Gcx.emit ~gcx (Jmp (Gcx.get_block_id_from_mir_block_id ~gcx continue))
    | _ -> ())
  (*
   * ===========================================
   *                   Mov
   * ===========================================
   *)
  | Mir.Instruction.Mov (dest_var_id, value) :: rest_instructions ->
    let dest_vreg = vreg_of_result_var_id dest_var_id in
    let instr =
      match resolve_ir_value ~allow_imm64:true value with
      | SImm imm -> MovIM (size_of_immediate imm, imm, Reg dest_vreg)
      | SAddr addr -> Lea (Size64, addr, dest_vreg)
      | SMem (mem, size) -> MovMM (size, Mem mem, Reg dest_vreg)
      | SVReg (src_var_id, size) -> MovMM (size, Reg src_var_id, Reg dest_vreg)
    in
    Gcx.emit ~gcx instr;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Call
   * ===========================================
   *)
  | Mir.Instruction.Call (return_var_id, ret_ty, func_val, arg_vals) :: rest_instructions ->
    (* Emit arguments for call *)
    gen_call_arguments arg_vals;
    (* Emit call instruction *)
    let inst =
      match func_val with
      | `FunctionL label -> CallL (label_of_mir_label label)
      | `FunctionV _ ->
        let func_mem = emit_mem (resolve_ir_value func_val) in
        CallM (Size64, func_mem)
    in
    Gcx.emit ~gcx inst;
    (* Return stack pointer to address before arguments were pushed onto stack *)
    let num_stack_arg_vals = max 0 (List.length arg_vals - 6) in
    if num_stack_arg_vals <> 0 then
      Gcx.emit
        ~gcx
        (AddIM
           (Size64, Imm32 (Int32.of_int (num_stack_arg_vals * 8)), Reg (Gcx.mk_precolored ~gcx SP)));
    (* Move result from register A to return vreg *)
    let return_size = register_size_of_mir_value_type ret_ty in
    Gcx.emit
      ~gcx
      (MovMM (return_size, Reg (Gcx.mk_precolored ~gcx A), Reg (vreg_of_result_var_id return_var_id)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Ret
   * ===========================================
   *)
  | Mir.Instruction.Ret value :: rest_instructions ->
    (match value with
    | None -> ()
    | Some value ->
      let precolored_vreg = Gcx.mk_precolored ~gcx A in
      (match resolve_ir_value ~allow_imm64:true value with
      | SImm imm -> Gcx.emit ~gcx (MovIM (size_of_immediate imm, imm, Reg precolored_vreg))
      | SAddr addr -> Gcx.emit ~gcx (Lea (Size64, addr, precolored_vreg))
      | SMem (mem, size) -> Gcx.emit ~gcx (MovMM (size, Mem mem, Reg precolored_vreg))
      | SVReg (vreg, size) -> Gcx.emit ~gcx (MovMM (size, Reg vreg, Reg precolored_vreg))));
    Gcx.emit ~gcx Ret;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Load
   * ===========================================
   *)
  | Mir.Instruction.Load (result_var_id, pointer) :: rest_instructions ->
    let result_vreg = vreg_of_result_var_id result_var_id in
    let pointer_element_type = pointer_value_element_type pointer in
    let size =
      match pointer_element_type with
      | `UnitT
      | `BoolT
      | `ByteT
      | `IntT
      | `LongT
      | `FunctionT
      | `PointerT _ ->
        register_size_of_mir_value_type pointer_element_type
      | `AggregateT _ -> failwith "TODO: Cannot compile aggregate literals"
      | `ArrayT _ -> failwith "TODO: Cannot compile array literals"
    in
    let src =
      match pointer with
      | `PointerL (_, label) -> mk_label_memory_address label
      | `PointerV _ ->
        (match resolve_ir_value pointer with
        | SVReg (vreg, _) ->
          PhysicalAddress { offset = None; base = RegBase vreg; index_and_scale = None }
        | SMem (mem, _) -> mem
        | SImm _
        | SAddr _ ->
          failwith "Expected memory or address")
    in
    Gcx.emit ~gcx (MovMM (size, Mem src, Reg result_vreg));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Store
   * ===========================================
   *)
  | Mir.Instruction.Store (pointer, value) :: rest_instructions ->
    let pointer_element_type = pointer_value_element_type pointer in
    let size =
      match pointer_element_type with
      | `UnitT
      | `BoolT
      | `ByteT
      | `IntT
      | `LongT
      | `FunctionT
      | `PointerT _ ->
        register_size_of_mir_value_type pointer_element_type
      | `AggregateT _ -> failwith "TODO: Cannot compile aggregate literals"
      | `ArrayT _ -> failwith "TODO: Cannot compile array literals"
    in
    let value = resolve_ir_value ~allow_imm64:true value in
    let dest =
      match pointer with
      | `PointerL (_, label) -> mk_label_memory_address label
      | `PointerV _ ->
        (match resolve_ir_value pointer with
        | SVReg (vreg, _) ->
          PhysicalAddress { offset = None; base = RegBase vreg; index_and_scale = None }
        | SMem (mem, _) -> mem
        | SImm _
        | SAddr _ ->
          failwith "Expected memory or address")
    in

    (match value with
    | SImm imm -> Gcx.emit ~gcx (MovIM (size, imm, Mem dest))
    | SAddr addr ->
      let vreg = mk_vreg () in
      Gcx.emit ~gcx (Lea (Size64, addr, vreg));
      Gcx.emit ~gcx (MovMM (Size64, Reg vreg, Mem dest))
    | SMem (mem, _) ->
      let vreg = mk_vreg () in
      Gcx.emit ~gcx (MovMM (size, Mem mem, Reg vreg));
      Gcx.emit ~gcx (MovMM (size, Reg vreg, Mem dest))
    | SVReg (reg, _) -> Gcx.emit ~gcx (MovMM (size, Reg reg, Mem dest)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Add
   * ===========================================
   *)
  | Mir.Instruction.Add (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_result_var_id result_var_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = register_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (MovMM (size, other_mem, Reg result_vreg));
      Gcx.emit ~gcx (AddIM (size, imm, Reg result_vreg))
    | (v1, v2) ->
      let size = register_size_of_svalue v1 in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (MovMM (size, mem2, Reg result_vreg));
      Gcx.emit ~gcx (AddMM (size, mem1, Reg result_vreg)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Sub
   * ===========================================
   *)
  | Mir.Instruction.Sub (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_result_var_id result_var_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm left_imm, right) ->
      let right_mem = emit_mem right in
      let size = register_size_of_svalue right in
      Gcx.emit ~gcx (MovIM (size, left_imm, Reg result_vreg));
      Gcx.emit ~gcx (SubMM (size, right_mem, Reg result_vreg))
    | (left, SImm right_imm) ->
      let left_mem = emit_mem left in
      let size = register_size_of_svalue left in
      Gcx.emit ~gcx (MovMM (size, left_mem, Reg result_vreg));
      Gcx.emit ~gcx (SubIM (size, right_imm, Reg result_vreg))
    | (left, right) ->
      let left_mem = emit_mem left in
      let right_mem = emit_mem right in
      let size = register_size_of_svalue left in
      Gcx.emit ~gcx (MovMM (size, left_mem, Reg result_vreg));
      Gcx.emit ~gcx (SubMM (size, right_mem, Reg result_vreg)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Mul
   * ===========================================
   *)
  | Mir.Instruction.Mul (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_result_var_id result_var_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = min_size16 (register_size_of_svalue other) in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (IMulMIR (size, other_mem, imm, result_vreg))
    | (v1, v2) ->
      let size = min_size16 (register_size_of_svalue v1) in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (MovMM (size, mem2, Reg result_vreg));
      Gcx.emit ~gcx (IMulMR (size, mem1, result_vreg)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Div
   * ===========================================
   *)
  | Mir.Instruction.Div (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_result_var_id result_var_id in
    let precolored_a = Gcx.mk_precolored ~gcx A in
    let size = gen_idiv left_val right_val in
    Gcx.emit ~gcx (MovMM (size, Reg precolored_a, Reg result_vreg));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Rem
   * ===========================================
   *)
  | Mir.Instruction.Rem (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_result_var_id result_var_id in
    let precolored_d = Gcx.mk_precolored ~gcx D in
    let size = gen_idiv left_val right_val in
    Gcx.emit ~gcx (MovMM (size, Reg precolored_d, Reg result_vreg));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Neg
   * ===========================================
   *)
  | Mir.Instruction.Neg (result_var_id, arg) :: rest_instructions ->
    let resolved_value = resolve_ir_value arg in
    let size = register_size_of_svalue resolved_value in
    let arg_mem = emit_mem resolved_value in
    let result_vreg = vreg_of_result_var_id result_var_id in
    Gcx.emit ~gcx (MovMM (size, arg_mem, Reg result_vreg));
    Gcx.emit ~gcx (NegM (size, Reg result_vreg));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogNot
   * ===========================================
   *)
  | Mir.Instruction.LogNot (result_var_id, arg) :: rest_instructions ->
    gen_not result_var_id arg;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogAnd
   * ===========================================
   *)
  | Mir.Instruction.LogAnd (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_and result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogOr
   * ===========================================
   *)
  | Mir.Instruction.LogOr (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_or result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  BitNot
   * ===========================================
   *)
  | Mir.Instruction.BitNot (result_var_id, arg) :: rest_instructions ->
    gen_not result_var_id arg;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  BitAnd
   * ===========================================
   *)
  | Mir.Instruction.BitAnd (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_and result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  BitOr
   * ===========================================
   *)
  | Mir.Instruction.BitOr (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_or result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  BitXor
   * ===========================================
   *)
  | Mir.Instruction.BitXor (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_result_var_id result_var_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = register_size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (MovMM (size, other_mem, Reg result_vreg));
      Gcx.emit ~gcx (XorIM (size, imm, Reg result_vreg))
    | (v1, v2) ->
      let size = register_size_of_svalue v1 in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (MovMM (size, mem2, Reg result_vreg));
      Gcx.emit ~gcx (XorMM (size, mem1, Reg result_vreg)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  Shl
   * ===========================================
   *)
  | Mir.Instruction.Shl (result_var_id, target_val, shift_val) :: rest_instructions ->
    gen_shift
      result_var_id
      target_val
      shift_val
      ~mk_reg_instr:(fun size mem -> ShlR (size, mem))
      ~mk_imm_instr:(fun size imm mem -> ShlI (size, imm, mem));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  Shr
   * ===========================================
   *)
  | Mir.Instruction.Shr (result_var_id, target_val, shift_val) :: rest_instructions ->
    gen_shift
      result_var_id
      target_val
      shift_val
      ~mk_reg_instr:(fun size mem -> SarR (size, mem))
      ~mk_imm_instr:(fun size imm mem -> SarI (size, imm, mem));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  Shrl
   * ===========================================
   *)
  | Mir.Instruction.Shrl (result_var_id, target_val, shift_val) :: rest_instructions ->
    gen_shift
      result_var_id
      target_val
      shift_val
      ~mk_reg_instr:(fun size mem -> ShrR (size, mem))
      ~mk_imm_instr:(fun size imm mem -> ShrI (size, imm, mem));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Eq
   * ===========================================
   *)
  | [Mir.Instruction.Eq (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp E left_val right_val
  | Mir.Instruction.Eq (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc E result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Neq
   * ===========================================
   *)
  | [Mir.Instruction.Neq (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp NE left_val right_val
  | Mir.Instruction.Neq (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc NE result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Lt
   * ===========================================
   *)
  | [Mir.Instruction.Lt (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp L left_val right_val
  | Mir.Instruction.Lt (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc L result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   LtEq
   * ===========================================
   *)
  | [Mir.Instruction.LtEq (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp LE left_val right_val
  | Mir.Instruction.LtEq (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc LE result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Gt
   * ===========================================
   *)
  | [Mir.Instruction.Gt (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp G left_val right_val
  | Mir.Instruction.Gt (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc G result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   GtEq
   * ===========================================
   *)
  | [Mir.Instruction.GtEq (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp GE left_val right_val
  | Mir.Instruction.GtEq (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc GE result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                CallBuiltin
   * ===========================================
   *)
  | Mir.Instruction.CallBuiltin (ret_var, ret_mir_ty, { Builtin.name; _ }, args)
    :: rest_instructions ->
    let open Mir_builtin in
    let ret_vreg = vreg_of_result_var_id ret_var in
    (*
     * ===========================================
     *                myte_alloc
     * ===========================================
     *)
    if name = myte_alloc.name then (
      let (`PointerT element_mir_ty) = cast_to_pointer_type ret_mir_ty in
      let element_size = Gcx.size_of_mir_type ~gcx element_mir_ty in
      let precolored_a = Gcx.mk_precolored ~gcx A in
      let precolored_di = Gcx.mk_precolored ~gcx DI in
      (match args with
      (* If count is a literal precalculate total requested size and fit into smallest immediate *)
      | [((`ByteL _ | `IntL _ | `LongL _) as count_lit)] ->
        let count = int64_of_literal count_lit in
        let total_size = Int64.mul count (Int64.of_int element_size) in
        let (size, total_size_imm) =
          if Integers.is_out_of_unsigned_int_range total_size then
            (Size64, Imm64 total_size)
          else
            (Size32, Imm32 (Int64.to_int32 total_size))
        in
        Gcx.emit ~gcx (MovIM (size, total_size_imm, Reg precolored_di))
      (* If count is a variable multiply by size before putting in argument register *)
      | [((`ByteV _ | `IntV _ | `LongV _) as count_var)] ->
        let count_vreg =
          match resolve_ir_value count_var with
          | SVReg (count_vreg, _) -> count_vreg
          | _ -> failwith "Must be virtual register"
        in
        (* Check for special case where element size is a single byte - no multiplication required *)
        if element_size = 1 then
          Gcx.emit ~gcx (MovMM (Size64, Reg count_vreg, Reg precolored_di))
        else
          Gcx.emit
            ~gcx
            (IMulMIR (Size64, Reg count_vreg, Imm32 (Int32.of_int element_size), precolored_di))
      | _ -> failwith "Incorrect arguments");
      Gcx.emit ~gcx (CallL X86_runtime.myte_alloc_label);
      Gcx.emit ~gcx (MovMM (Size64, Reg precolored_a, Reg ret_vreg))
      (*
       * ===========================================
       *                myte_write
       * ===========================================
       *)
    ) else if name = myte_write.name then (
      gen_call_arguments args;
      let precolored_a = Gcx.mk_precolored ~gcx A in
      Gcx.emit ~gcx (CallL X86_runtime.myte_write_label);
      Gcx.emit ~gcx (MovMM (Size64, Reg precolored_a, Reg ret_vreg))
    ) else
      failwith (Printf.sprintf "Cannot compile unknown builtin %s to assembly" name);
    gen_instructions rest_instructions
  | Mir.Instruction.GetPointer get_pointer_instr :: rest_instructions ->
    gen_get_pointer ~gcx ~func get_pointer_instr;
    gen_instructions rest_instructions

and gen_get_pointer ~gcx ~func (get_pointer_instr : var_id Mir.Instruction.GetPointer.t) =
  let open Mir.Instruction.GetPointer in
  let { var_id; return_ty = _; pointer; pointer_offset; offsets } = get_pointer_instr in
  let element_ty = pointer_value_element_type pointer in

  (* Current address calculation - updated as offsets are visited *)
  let offset = ref None in
  let base = ref NoBase in
  let index_and_scale = ref None in

  (* Utilities for creating vregs *)
  let vreg_of_result_var_id var_id =
    VReg.of_var_id ~resolution:Unresolved ~func:(Some func) var_id
  in
  let mk_vreg () = VReg.mk ~resolution:Unresolved ~func:(Some func) in

  (* Emit the current address calculation as a Lea instruction, returning the resulting var and
     storing it as the base of the next instruction. If only a register base is present, do not emit
     an instruction and simply return that register. *)
  let emit_current_address_calculation () =
    (* TODO: All registers must have same size - must sign extend those that do not match *)
    let result_vreg =
      match (!offset, !base, !index_and_scale) with
      | (None, RegBase reg, None) -> reg
      | (offset, base, index_and_scale) ->
        let result_vreg = mk_vreg () in
        Gcx.emit ~gcx (Lea (Size64, PhysicalAddress { offset; base; index_and_scale }, result_vreg));
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
  let add_scaled_register (vreg, size) scale =
    (* Add an unscaled register to current address calculation. First try to add it to base if there
       is no base yet, then try adding to index with scale = 1 if there is no index, otherwise
       emit current address calculation and try again. *)
    let rec add_unscaled_register vreg =
      if !base = NoBase then
        base := RegBase vreg
      else if !index_and_scale = None then
        index_and_scale := Some (vreg, Scale1)
      else (
        ignore (emit_current_address_calculation ());
        add_unscaled_register vreg
      )
    in
    (* Add a scaled register to current address calculation. If there is already a scaled register
       then emit the current address calculation and try again. *)
    let rec add_scaled_register vreg scale =
      if !index_and_scale = None then
        index_and_scale := Some (vreg, scale)
      else (
        ignore (emit_current_address_calculation ());
        add_scaled_register vreg scale
      )
    in
    match scale with
    | 1 -> add_unscaled_register vreg
    | 2 -> add_scaled_register vreg Scale2
    | 4 -> add_scaled_register vreg Scale4
    | 8 -> add_scaled_register vreg Scale8
    (* Otherwise emit multiply to calculate index *)
    | scale ->
      ignore (emit_current_address_calculation ());
      let scaled_vreg = mk_vreg () in
      let scale_imm = Imm32 (Int32.of_int scale) in
      (* TODO: Handle sign extending byte arguments to 32/64 bits (movzbl/q) *)
      Gcx.emit ~gcx (IMulMIR (size, Reg vreg, scale_imm, scaled_vreg));
      add_unscaled_register scaled_vreg
  in

  (* Add address of root pointer *)
  (match pointer with
  | `PointerL (_, label) ->
    offset := Some (LabelOffset (label_of_mir_label label));
    base := IPBase
  | `PointerV _ ->
    (match resolve_ir_value ~gcx ~func (pointer :> ssa_value) with
    | SVReg (vreg, _) -> base := RegBase vreg
    | SMem (mem, _) ->
      let vreg = mk_vreg () in
      Gcx.emit ~gcx (MovMM (Size64, Mem mem, Reg vreg));
      base := RegBase vreg
    | _ -> failwith "PointerV must resolve to VReg or Mem"));

  (* The type that is currently being indexed into *)
  let current_ty = ref element_ty in

  let gen_offset offset ty =
    match offset with
    | PointerIndex pointer_offset ->
      (* TODO: Handle sign extending byte arguments to 32/64 bits (movzbl/q) *)
      let element_size = Gcx.size_of_mir_type ~gcx ty in
      (match resolve_ir_value ~gcx ~func ~allow_imm64:true (pointer_offset :> ssa_value) with
      | SImm imm ->
        let num_elements = int64_of_immediate imm in
        if num_elements <> Int64.zero then (
          (* Check if calculated offset fits in 64-bit immediate *)
          let offset = Int64.mul num_elements (Int64.of_int element_size) in
          if not (Integers.is_out_of_unsigned_int_range offset) then
            add_fixed_offset (Int64.to_int32 offset)
          else
            (* If not, 64-bit immediate offset must first be loaded into register  *)
            let vreg = mk_vreg () in
            Gcx.emit ~gcx Instruction.(MovIM (Size64, Imm64 offset, Reg vreg));
            add_scaled_register (vreg, Size64) 1
        )
      | SVReg (vreg, size) -> add_scaled_register (vreg, size) element_size
      | SMem (mem, size) ->
        let vreg = mk_vreg () in
        Gcx.emit ~gcx (MovMM (size, Mem mem, Reg vreg));
        add_scaled_register (vreg, size) element_size
      | SAddr _ -> failwith "PointerIndex cannot be resolved to SAddr")
    | FieldIndex element_index ->
      (match ty with
      | `AggregateT ({ Aggregate.elements; _ } as agg) ->
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

  let address_vreg = emit_current_address_calculation () in
  let result_vreg = vreg_of_result_var_id var_id in
  Gcx.emit ~gcx (MovMM (Size64, Reg address_vreg, Reg result_vreg))

and resolve_ir_value ~gcx ~func ?(allow_imm64 = false) value =
  let vreg_of_var var_id size =
    let vreg = VReg.of_var_id ~resolution:Unresolved ~func:(Some func) var_id in
    match vreg.resolution with
    | StackSlot mem -> SMem (mem, size)
    | _ -> SVReg (vreg, size)
  in
  match value with
  | `UnitL -> SImm (Imm8 0)
  | `UnitV var_id -> vreg_of_var var_id Size8
  | `BoolL b ->
    SImm
      (Imm8
         ( if b then
           1
         else
           0 ))
  | `BoolV var_id -> vreg_of_var var_id Size8
  | `ByteL b -> SImm (Imm8 b)
  | `ByteV var_id -> vreg_of_var var_id Size8
  (* Int literals can be downgraded to an 8 byte literal if they fit *)
  | `IntL i -> SImm (Imm32 i)
  | `IntV var_id -> vreg_of_var var_id Size32
  (* Long literals can be downgraded to a 32 byte int literal if it fits. Otherwise 64 bit literal
     must first be loaded to a register with a mov instruction. *)
  | `LongL l ->
    if allow_imm64 then
      SImm (Imm64 l)
    else if not (Integers.is_out_of_signed_int_range l) then
      SImm (Imm32 (Int64.to_int32 l))
    else
      let vreg = VReg.mk ~resolution:Unresolved ~func:(Some func) in
      Gcx.emit ~gcx Instruction.(MovIM (Size64, Imm64 l, Reg vreg));
      SVReg (vreg, Size64)
  | `LongV var_id -> vreg_of_var var_id Size64
  | `FunctionL name -> SAddr (mk_label_memory_address name)
  | `FunctionV var_id -> vreg_of_var var_id Size64
  | `PointerL (_, label) -> SAddr (mk_label_memory_address label)
  | `PointerV (_, var_id) -> vreg_of_var var_id Size64
  | `AggregateV _ -> failwith "TODO: Cannot compile aggregate structures yet"
  | `ArrayL _ -> failwith "TODO: Cannot compile array literals yet"
  | `ArrayV _ -> failwith "TODO: Cannot compile array variables yet"

and register_size_of_mir_value_type value_type =
  match value_type with
  | `UnitT
  | `BoolT
  | `ByteT ->
    Size8
  | `IntT -> Size32
  | `LongT
  | `FunctionT
  | `PointerT _ ->
    Size64
  | `AggregateT _ -> failwith "TODO: Cannot compile aggregate structure literals"
  | `ArrayT _ -> failwith "TODO: Cannot compile array literals"

and register_size_of_svalue value =
  match value with
  | SImm imm -> size_of_immediate imm
  | SVReg (_, size) -> size
  | SMem (_, size) -> size
  | SAddr _ -> Size64

and min_size16 size =
  match size with
  | Size8 -> Size16
  | _ -> size

and mk_label_memory_address label =
  PhysicalAddress
    {
      offset = Some (LabelOffset (label_of_mir_label label));
      base = IPBase;
      index_and_scale = None;
    }

and label_of_mir_label label = Str.global_replace invalid_label_chars "$" label
