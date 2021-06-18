open Basic_collections
open Mir
open X86_gen_context
open X86_instructions

type resolved_source_value =
  (* An immediate value *)
  | SImm of immediate
  (* Value is a virtual register *)
  | SVReg of VReg.t * size
  (* Value is the contents at a memory location *)
  | SMem of VReg.t memory_address * size
  (* Value is a memory address *)
  | SAddr of VReg.t memory_address

let rec gen ~gcx (ir : ssa_program) =
  (* Add init block with initialization of globals *)
  let func = Gcx.start_function ~gcx [] 0 in
  Gcx.start_block ~gcx ~label:(Some "_init") ~func:func.id ~mir_block_id:None;
  let prologue_block_id = (Option.get gcx.current_block_builder).id in
  func.prologue <- prologue_block_id;
  Gcx.finish_block ~gcx;
  SMap.iter (fun _ global -> gen_global_instruction_builder ~gcx ~ir global func) ir.globals;
  Gcx.finish_function ~gcx;
  (* Remove init block if there are no init sections *)
  if List.length func.blocks = 1 then begin
    gcx.blocks_by_id <- IMap.empty;
    gcx.funcs_by_id <- IMap.remove func.id gcx.funcs_by_id
  end;
  SMap.iter (fun _ func -> gen_function_instruction_builder ~gcx ~ir func) ir.funcs;
  Gcx.finish_builders ~gcx

and gen_global_instruction_builder ~gcx ~ir global init_func =
  let open Instruction in
  let init_val_info = resolve_ir_value ~gcx ~func:init_func.id global.init_val in
  match init_val_info with
  | SImm imm ->
    (* Global is initialized to immediate, so insert into initialized data section *)
    let data = { label = global.name; value = ImmediateData imm } in
    Gcx.add_data ~gcx data
  | SAddr addr ->
    (* Global is initialized to address. Since this is position independent code we must calculate
       the address at runtime as it cannot be known statically, so place in uninitialized
       (bss) section. *)
    let bss_data = { label = global.name; size = bytes_of_size Size64 } in
    Gcx.add_bss ~gcx bss_data;
    (* Emit init block to move address to global *)
    Gcx.start_block
      ~gcx
      ~label:(Some ("_init_" ^ global.name))
      ~func:init_func.id
      ~mir_block_id:None;
    let reg = VReg.mk ~resolution:Unresolved ~func:(Some init_func.id) in
    Gcx.emit ~gcx (Lea (Size64, addr, reg));
    Gcx.emit ~gcx (MovMM (Size64, Reg reg, Mem (mk_label_memory_address global.name)));
    Gcx.finish_block ~gcx
  | SMem (mem, size) ->
    (* Global is initialized to value at a memory location. This must be read at runtime so place in
       uninitialized (bss) section. *)
    let bss_data = { label = global.name; size = bytes_of_size size } in
    Gcx.add_bss ~gcx bss_data;
    (* Emit init block to move initial value to global *)
    Gcx.start_block
      ~gcx
      ~label:(Some ("_init_" ^ global.name))
      ~func:init_func.id
      ~mir_block_id:None;
    let reg = VReg.mk ~resolution:Unresolved ~func:(Some init_func.id) in
    Gcx.emit ~gcx (MovMM (size, Mem mem, Reg reg));
    Gcx.emit ~gcx (MovMM (size, Reg reg, Mem (mk_label_memory_address global.name)));
    Gcx.finish_block ~gcx
  | SVReg (_, size) ->
    (* Global is not initialized to a constant, so it must have its own initialization block.
       Place global in uninitialized (bss) section. *)
    let bss_data = { label = global.name; size = bytes_of_size size } in
    Gcx.add_bss ~gcx bss_data;
    gen_blocks ~gcx ~ir global.init_start_block (Some ("_init_" ^ global.name)) init_func.id

and gen_function_instruction_builder ~gcx ~ir func =
  let func_ = Gcx.start_function ~gcx [] 0 in
  let label =
    if func.body_start_block = ir.main_id then
      "_main"
    else
      func.name
  in
  (* Create function prologue which copies all params from physical registers to temporaries *)
  Gcx.start_block ~gcx ~label:(Some label) ~func:func_.id ~mir_block_id:None;
  let prologue_block_id = (Option.get gcx.current_block_builder).id in
  func_.prologue <- prologue_block_id;
  func_.params <-
    List.mapi
      (fun i (_, var_id, value_type) ->
        (* First 6 parameters are passed in known registers *)
        let size = size_of_mir_value_type value_type in
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
  let vreg_of_var var_id = VReg.of_var_id ~resolution:Unresolved ~func:(Some func) var_id in
  let mk_vreg () = VReg.mk ~resolution:Unresolved ~func:(Some func) in
  let resolve_ir_value ?(allowImm64 = false) v =
    resolve_ir_value ~gcx ~func ~allowImm64 (v :> var_id Value.t)
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
  (* Generate a cmp instruction between two arguments. Return whether order was swapped. *)
  let gen_cmp left_val right_val =
    match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    (* Comparison to immediate - swap arguments if necessary *)
    | (SImm imm, other) ->
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (CmpMI (other_mem, imm));
      true
    | (other, SImm imm) ->
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (CmpMI (other_mem, imm));
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
      Gcx.emit ~gcx (CmpMM (size_of_svalue v1, mem1, mem2));
      false
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
    let result_vreg = vreg_of_var result_var_id in
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
    let dest_vreg = vreg_of_var dest_var_id in
    let instr =
      match resolve_ir_value ~allowImm64:true value with
      | SImm imm -> MovIM (imm, Reg dest_vreg)
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
    (* Arguments 7+ are pushed on stack in reverse order *)
    let stack_arg_vals = List.rev (List_utils.drop 6 arg_vals) in
    let num_stack_arg_vals = List.length stack_arg_vals in
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
            (match resolve_ir_value ~allowImm64:true arg_val with
            | SImm imm -> Gcx.emit ~gcx (MovIM (imm, Reg vreg))
            | SAddr addr -> Gcx.emit ~gcx (Lea (Size64, addr, vreg))
            | SMem (mem, size) -> Gcx.emit ~gcx (MovMM (size, Mem mem, Reg vreg))
            | SVReg (source_vreg, size) -> Gcx.emit ~gcx (MovMM (size, Reg source_vreg, Reg vreg))))
      arg_vals;
    (* Emit call instruction *)
    let inst =
      match func_val with
      | `FunctionL label -> CallL label
      | `FunctionV _ ->
        let func_mem = emit_mem (resolve_ir_value func_val) in
        CallM (Size64, func_mem)
    in
    Gcx.emit ~gcx inst;
    (* Return stack pointer to address before arguments were pushed onto stack *)
    if num_stack_arg_vals <> 0 then
      Gcx.emit
        ~gcx
        (AddIM
           (Size64, Imm32 (Int32.of_int (num_stack_arg_vals * 8)), Reg (Gcx.mk_precolored ~gcx SP)));
    (* Move result from register A to return vreg *)
    let return_size = size_of_mir_value_type ret_ty in
    Gcx.emit
      ~gcx
      (MovMM (return_size, Reg (Gcx.mk_precolored ~gcx A), Reg (vreg_of_var return_var_id)));
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
      (match resolve_ir_value value with
      | SImm imm -> Gcx.emit ~gcx (MovIM (imm, Reg precolored_vreg))
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
  | Mir.Instruction.Load (_var_id, _label) :: rest_instructions ->
    (* let global = SMap.find label ir.globals in
       let global_size = size_of_mir_value_type global.ty in
       Gcx.emit
         ~gcx
         (MovMM (global_size, Mem (mk_label_memory_address label), Reg (vreg_of_var var_id))); *)
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Store
   * ===========================================
   *)
  | Mir.Instruction.Store (_label, _value) :: rest_instructions ->
    (* let global_address = mk_label_memory_address label in
       (match resolve_ir_value value with
       | SImm imm -> Gcx.emit ~gcx (MovIM (imm, Mem global_address))
       | SAddr addr ->
         let vreg = mk_vreg () in
         Gcx.emit ~gcx (Lea (Size64, addr, vreg));
         Gcx.emit ~gcx (MovMM (Size64, Reg vreg, Mem global_address))
       | SMem (mem, size) ->
         let vreg = mk_vreg () in
         Gcx.emit ~gcx (MovMM (size, Mem mem, Reg vreg));
         Gcx.emit ~gcx (MovMM (size, Reg vreg, Mem global_address))
       | SVReg (reg, size) -> Gcx.emit ~gcx (MovMM (size, Reg reg, Mem global_address))); *)
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Add
   * ===========================================
   *)
  | Mir.Instruction.Add (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_var result_var_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = size_of_svalue other in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (MovMM (size, other_mem, Reg result_vreg));
      Gcx.emit ~gcx (AddIM (size, imm, Reg result_vreg))
    | (v1, v2) ->
      let size = size_of_svalue v1 in
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
    let result_vreg = vreg_of_var result_var_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm left_imm, right) ->
      let right_mem = emit_mem right in
      Gcx.emit ~gcx (MovIM (left_imm, Reg result_vreg));
      Gcx.emit ~gcx (SubMM (size_of_svalue right, right_mem, Reg result_vreg))
    | (left, SImm right_imm) ->
      let left_mem = emit_mem left in
      let size = size_of_svalue left in
      Gcx.emit ~gcx (MovMM (size, left_mem, Reg result_vreg));
      Gcx.emit ~gcx (SubIM (size, right_imm, Reg result_vreg))
    | (left, right) ->
      let left_mem = emit_mem left in
      let right_mem = emit_mem right in
      let size = size_of_svalue left in
      Gcx.emit ~gcx (MovMM (size, left_mem, Reg result_vreg));
      Gcx.emit ~gcx (SubMM (size, right_mem, Reg result_vreg)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Mul
   * ===========================================
   *)
  | Mir.Instruction.Mul (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_var result_var_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let size = min_size16 (size_of_svalue other) in
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (IMulMIR (size, other_mem, imm, result_vreg))
    | (v1, v2) ->
      let size = min_size16 (size_of_svalue v1) in
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
    let result_vreg = vreg_of_var result_var_id in
    let precolored_a = Gcx.mk_precolored ~gcx A in
    let size =
      match (resolve_ir_value left_val, resolve_ir_value right_val) with
      | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
      | (SImm dividend_imm, divisor) ->
        let size = size_of_svalue divisor in
        let divisor_mem = emit_mem divisor in
        Gcx.emit ~gcx (MovIM (dividend_imm, Reg precolored_a));
        Gcx.emit ~gcx (IDiv (size, divisor_mem));
        size
      | (dividend, SImm divisor_imm) ->
        let size = size_of_svalue dividend in
        let dividend_mem = emit_mem dividend in
        let divisor_vreg = mk_vreg () in
        Gcx.emit ~gcx (MovMM (size, dividend_mem, Reg precolored_a));
        Gcx.emit ~gcx (MovIM (divisor_imm, Reg divisor_vreg));
        Gcx.emit ~gcx (IDiv (size, Reg divisor_vreg));
        size
      | (dividend, divisor) ->
        let size = size_of_svalue dividend in
        let dividend_mem = emit_mem dividend in
        let divisor_mem = emit_mem divisor in
        Gcx.emit ~gcx (MovMM (size, dividend_mem, Reg precolored_a));
        Gcx.emit ~gcx (IDiv (size, divisor_mem));
        size
    in
    Gcx.emit ~gcx (MovMM (size, Reg precolored_a, Reg result_vreg));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Neg
   * ===========================================
   *)
  | Mir.Instruction.Neg (result_var_id, arg) :: rest_instructions ->
    let resolved_value = resolve_ir_value arg in
    let size = size_of_svalue resolved_value in
    let arg_mem = emit_mem resolved_value in
    let result_vreg = vreg_of_var result_var_id in
    Gcx.emit ~gcx (MovMM (size, arg_mem, Reg result_vreg));
    Gcx.emit ~gcx (NegM (size, Reg result_vreg));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogNot
   * ===========================================
   *)
  | Mir.Instruction.LogNot (result_var_id, arg) :: rest_instructions ->
    let resolved_value = resolve_ir_value arg in
    let size = size_of_svalue resolved_value in
    let arg_mem = emit_mem resolved_value in
    let result_vreg = vreg_of_var result_var_id in
    Gcx.emit ~gcx (MovMM (size, arg_mem, Reg result_vreg));
    Gcx.emit ~gcx (NotM (size, Reg result_vreg));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogAnd
   * ===========================================
   *)
  | Mir.Instruction.LogAnd (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_var result_var_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (MovMM (size_of_immediate imm, other_mem, Reg result_vreg));
      Gcx.emit ~gcx (AndIM (imm, Reg result_vreg))
    | (v1, v2) ->
      let size = size_of_svalue v1 in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (MovMM (size, mem2, Reg result_vreg));
      Gcx.emit ~gcx (AndMM (size, mem1, Reg result_vreg)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogOr
   * ===========================================
   *)
  | Mir.Instruction.LogOr (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_var result_var_id in
    (match (resolve_ir_value left_val, resolve_ir_value right_val) with
    | (SImm _, SImm _) -> failwith "Constants must be folded before gen"
    | (SImm imm, other)
    | (other, SImm imm) ->
      let other_mem = emit_mem other in
      Gcx.emit ~gcx (MovMM (size_of_immediate imm, other_mem, Reg result_vreg));
      Gcx.emit ~gcx (OrIM (imm, Reg result_vreg))
    | (v1, v2) ->
      let size = size_of_svalue v1 in
      let (v1, v2) = choose_commutative_source_dest_arg_order v1 v2 in
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (MovMM (size, mem2, Reg result_vreg));
      Gcx.emit ~gcx (OrMM (size, mem1, Reg result_vreg)));
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
  | _ -> failwith "TODO: Implement generation of virtual assembly from this instruction"

and resolve_ir_value ~gcx ~func ?(allowImm64 = false) value =
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
  | `IntL i ->
    if Int32.compare i (Int32.of_int 128) = -1 && Int32.compare (Int32.of_int (-129)) i = 1 then
      SImm (Imm8 (Int32.to_int i))
    else
      SImm (Imm32 i)
  | `IntV var_id -> vreg_of_var var_id Size32
  (* Long literals can be downgraded to an 8 or byte int literal if they fit. Otherwise 64 bit
     literal must first be loaded to a register with a mov instruction. *)
  | `LongL l ->
    if not (Integers.is_out_of_signed_byte_range l) then
      SImm (Imm8 (Int64.to_int l))
    else if not (Integers.is_out_of_signed_int_range l) then
      SImm (Imm32 (Int64.to_int32 l))
    else if allowImm64 then
      SImm (Imm64 l)
    else
      let vreg = VReg.mk ~resolution:Unresolved ~func:(Some func) in
      Gcx.emit ~gcx Instruction.(MovIM (Imm64 l, Reg vreg));
      SVReg (vreg, Size64)
  | `LongV var_id -> vreg_of_var var_id Size64
  | `FunctionL name -> SAddr (mk_label_memory_address name)
  | `FunctionV var_id -> vreg_of_var var_id Size64
  | `StringL _
  | `StringV _ ->
    failwith "TODO: Cannot compile string literals"
  | `PointerL _
  | `PointerV _ ->
    failwith "TODO: Cannot compile pointers yet"
  | `AggregateV _ -> failwith "TODO: Cannot compile aggregate structures yet"

and size_of_mir_value_type value_type =
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
  | `StringT -> failwith "TODO: Cannot compile string literals"
  | `AggregateT _ -> failwith "TODO: Cannot compile aggregate structure literals"

and size_of_svalue value =
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
  PhysicalAddress { offset = Some (LabelOffset label); base = None; index_and_scale = None }
