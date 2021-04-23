open Basic_collections
open Mir
open X86_gen_context
open X86_instructions

let imm_byte_of_bool bool =
  ByteImmediate
    ( if bool then
      1
    else
      0 )

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
  let func = Gcx.mk_function ~gcx [] 0 in
  Gcx.start_block ~gcx ~label:(Some "_init") ~func:func.id ~mir_block_id:None;
  let prologue_block_id = (Option.get gcx.current_block_builder).id in
  func.prologue <- prologue_block_id;
  Gcx.finish_block ~gcx;
  SMap.iter (fun _ global -> gen_global_instruction_builder ~gcx ~ir global func) ir.globals;
  (* Remove init block if there are no init sections *)
  if List.length gcx.text = 1 then begin
    gcx.text <- [];
    gcx.blocks_by_id <- IMap.empty;
    gcx.funcs_by_id <- IMap.remove func.id gcx.funcs_by_id
  end;
  SMap.iter (fun _ func -> gen_function_instruction_builder ~gcx ~ir func) ir.funcs;
  Gcx.finish_builders ~gcx

and gen_global_instruction_builder ~gcx ~ir global init_func =
  let open Instruction in
  let init_val_info = get_resolved_source_value ~func:init_func.id global.init_val in
  match init_val_info with
  | SImm imm ->
    (* Global is initialized to immediate, so insert into initialized data section *)
    let data = { label = global.name; value = ImmediateData imm } in
    Gcx.add_data ~gcx data
  | SAddr addr ->
    (* Global is initialized to address. Since this is position independent code we must calculate
       the address at runtime as it cannot be known statically, so place in uninitialized
       (bss) section. *)
    let bss_data = { label = global.name; size = bytes_of_size Quad } in
    Gcx.add_bss ~gcx bss_data;
    (* Emit init block to move address to global *)
    Gcx.start_block
      ~gcx
      ~label:(Some ("_init_" ^ global.name))
      ~func:init_func.id
      ~mir_block_id:None;
    let reg = VReg.mk ~resolution:Unresolved ~func:(Some init_func.id) in
    Gcx.emit ~gcx (Lea (addr, reg));
    Gcx.emit ~gcx (MovMM (Reg reg, Mem (mk_label_memory_address global.name)));
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
    Gcx.emit ~gcx (MovMM (Mem mem, Reg reg));
    Gcx.emit ~gcx (MovMM (Reg reg, Mem (mk_label_memory_address global.name)));
    Gcx.finish_block ~gcx
  | SVReg (_, size) ->
    (* Global is not initialized to a constant, so it must have its own initialization block.
       Place global in uninitialized (bss) section. *)
    let bss_data = { label = global.name; size = bytes_of_size size } in
    Gcx.add_bss ~gcx bss_data;
    gen_blocks ~gcx ~ir global.init_start_block (Some ("_init_" ^ global.name)) init_func.id

and gen_function_instruction_builder ~gcx ~ir func =
  let func_ = Gcx.mk_function ~gcx [] 0 in
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
      (fun i (_, var_id, _) ->
        (* First 6 parameters are passed in known registers *)
        let move_from_precolored color =
          let param_vreg = VReg.of_var_id ~resolution:Unresolved ~func:(Some func_.id) var_id in
          Gcx.emit ~gcx (MovMM (Reg (Gcx.mk_precolored ~gcx color), Reg param_vreg));
          param_vreg
        in
        match i with
        | 0 -> move_from_precolored DI
        | 1 -> move_from_precolored SI
        | 2 -> move_from_precolored D
        | 3 -> move_from_precolored C
        | 4 -> move_from_precolored R8
        | 5 -> move_from_precolored R9
        (* All other parameters pushed onto stack before call *)
        | n ->
          let resolution =
            VReg.StackSlot
              (PhysicalAddress
                 {
                   base = Some (Gcx.mk_precolored ~gcx BP);
                   offset = Some (ImmediateOffset (Int64.of_int (4 * (n - 5))));
                   index_and_scale = None;
                 })
          in
          VReg.of_var_id ~resolution ~func:(Some func_.id) var_id)
      func.params;
  Gcx.emit ~gcx (Jmp (Gcx.get_block_id_from_mir_block_id ~gcx func.body_start_block));
  Gcx.finish_block ~gcx;
  gen_blocks ~gcx ~ir func.body_start_block None func_.id

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
  let is_cond_jump var_id =
    match block.next with
    | Branch { test = Var test_var_id; _ } when test_var_id = var_id -> true
    | _ -> false
  in
  let emit_mem mem =
    match mem with
    | SVReg (vreg, _) -> Reg vreg
    | SMem (mem, _) -> Mem mem
    | SAddr addr ->
      let vreg = mk_vreg () in
      Gcx.emit ~gcx (Lea (addr, vreg));
      Reg vreg
    | _ -> failwith "Only called on address, memory location, or vreg"
  in
  (* Generate a cmp instruction between two arguments. Return whether order was swapped. *)
  let gen_cmp left_val right_val =
    match (get_resolved_source_value ~func left_val, get_resolved_source_value ~func right_val) with
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
    | (SMem (mem1, _), SMem (mem2, _)) ->
      let vreg = mk_vreg () in
      Gcx.emit ~gcx (MovMM (Mem mem1, Reg vreg));
      Gcx.emit ~gcx (CmpMM (Reg vreg, Mem mem2));
      false
    | (v1, v2) ->
      let mem1 = emit_mem v1 in
      let mem2 = emit_mem v2 in
      Gcx.emit ~gcx (CmpMM (mem1, mem2));
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
    Gcx.emit ~gcx (XorMM (Reg result_vreg, Reg result_vreg));
    let swapped = gen_cmp (Numeric left_val) (Numeric right_val) in
    let cc =
      if swapped then
        swap_condition_code_order cc
      else
        cc
    in
    Gcx.emit ~gcx (SetCC (cc, result_vreg))
  in
  match instructions with
  | [] ->
    (* Conditional jump when the condition is in a variable *)
    (match block.next with
    | Branch { test = Lit _; _ } -> failwith "Dead branch pruning must have already occurred"
    | Continue continue ->
      (* TODO: Create better structure for tracking relative block locations *)
      Gcx.emit ~gcx (Jmp (Gcx.get_block_id_from_mir_block_id ~gcx continue))
    | Branch { test = Var _ as test; continue; jump } ->
      let vreg =
        match get_resolved_source_value ~func (Bool test) with
        | SVReg (vreg, _) -> vreg
        | SMem (mem, _) ->
          let vreg = mk_vreg () in
          Gcx.emit ~gcx (MovMM (Mem mem, Reg vreg));
          vreg
        | _ -> failwith "Boolean variable can only be vreg or memory location"
      in
      Gcx.emit ~gcx (TestMR (Reg vreg, vreg));
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
      match get_resolved_source_value ~func value with
      | SImm imm -> MovIM (imm, Reg dest_vreg)
      | SAddr addr -> Lea (addr, dest_vreg)
      | SMem (mem, _) -> MovMM (Mem mem, Reg dest_vreg)
      | SVReg (src_var_id, _) -> MovMM (Reg src_var_id, Reg dest_vreg)
    in
    Gcx.emit ~gcx instr;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Call
   * ===========================================
   *)
  | Mir.Instruction.Call (return_var_id, func_val, arg_vals) :: rest_instructions ->
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
            (match get_resolved_source_value ~func arg_val with
            | SImm imm -> Gcx.emit ~gcx (MovIM (imm, Reg vreg))
            | SAddr addr -> Gcx.emit ~gcx (Lea (addr, vreg))
            | SMem (mem, _) -> Gcx.emit ~gcx (MovMM (Mem mem, Reg vreg))
            | SVReg (source_vreg, _) -> Gcx.emit ~gcx (MovMM (Reg source_vreg, Reg vreg))))
      arg_vals;
    (* Later arguments are pushed on stack in reverse order *)
    let rest_arg_vals = List.rev (List_utils.drop 6 arg_vals) in
    List.iter
      (fun arg_val ->
        match get_resolved_source_value ~func arg_val with
        (* Push does not support 64-bit immediates. Must instead move onto stack. *)
        | SImm (QuadImmediate _) -> failwith "Unimplemented"
        | SImm imm -> Gcx.emit ~gcx (PushI imm)
        (* Address must be calculated in a register and then pushed onto stack *)
        | SAddr addr ->
          let vreg = mk_vreg () in
          Gcx.emit ~gcx (Lea (addr, vreg));
          Gcx.emit ~gcx (PushM (Reg vreg))
        | SMem (mem, _) -> Gcx.emit ~gcx (PushM (Mem mem))
        | SVReg (var_id, _) -> Gcx.emit ~gcx (PushM (Reg var_id)))
      rest_arg_vals;
    (* Emit call instruction and move result from register A to return vreg *)
    let inst =
      match func_val with
      | Mir.Instruction.FunctionValue.Lit label -> CallL label
      | Mir.Instruction.FunctionValue.Var var_id -> CallR (vreg_of_var var_id)
    in
    Gcx.emit ~gcx inst;
    Gcx.emit ~gcx (MovMM (Reg (Gcx.mk_precolored ~gcx A), Reg (vreg_of_var return_var_id)));
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
      (match get_resolved_source_value ~func value with
      | SImm imm -> Gcx.emit ~gcx (MovIM (imm, Reg precolored_vreg))
      | SAddr addr -> Gcx.emit ~gcx (Lea (addr, precolored_vreg))
      | SMem (mem, _) -> Gcx.emit ~gcx (MovMM (Mem mem, Reg precolored_vreg))
      | SVReg (vreg, _) -> Gcx.emit ~gcx (MovMM (Reg vreg, Reg precolored_vreg))));
    Gcx.emit ~gcx Ret;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Load Global
   * ===========================================
   *)
  | Mir.Instruction.LoadGlobal (var_id, label) :: rest_instructions ->
    Gcx.emit ~gcx (MovMM (Mem (mk_label_memory_address label), Reg (vreg_of_var var_id)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Store Global
   * ===========================================
   *)
  | Mir.Instruction.StoreGlobal (label, value) :: rest_instructions ->
    let global_address = mk_label_memory_address label in
    (match get_resolved_source_value ~func value with
    | SImm imm -> Gcx.emit ~gcx (MovIM (imm, Mem global_address))
    | SAddr addr ->
      let vreg = mk_vreg () in
      Gcx.emit ~gcx (Lea (addr, vreg));
      Gcx.emit ~gcx (MovMM (Reg vreg, Mem global_address))
    | SMem (mem, _) ->
      let vreg = mk_vreg () in
      Gcx.emit ~gcx (MovMM (Mem mem, Reg vreg));
      Gcx.emit ~gcx (MovMM (Reg vreg, Mem global_address))
    | SVReg (reg, _) -> Gcx.emit ~gcx (MovMM (Reg reg, Mem global_address)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Add
   * ===========================================
   *)
  | Mir.Instruction.Add (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_var result_var_id in
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIM (QuadImmediate (Int64.add left_lit right_lit), Reg result_vreg))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (MovMM (Reg (vreg_of_var arg_var_id), Reg result_vreg));
      Gcx.emit ~gcx (AddIR (QuadImmediate lit, result_vreg))
    | (IntVar left_var_id, IntVar right_var_id) ->
      Gcx.emit ~gcx (MovMM (Reg (vreg_of_var right_var_id), Reg result_vreg));
      Gcx.emit ~gcx (AddRR (vreg_of_var left_var_id, result_vreg)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Sub
   * ===========================================
   *)
  | Mir.Instruction.Sub (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_var result_var_id in
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIM (QuadImmediate (Int64.sub left_lit right_lit), Reg result_vreg))
    | (IntLit left_lit, IntVar right_var_id) ->
      Gcx.emit ~gcx (MovIM (QuadImmediate left_lit, Reg result_vreg));
      Gcx.emit ~gcx (SubRR (vreg_of_var right_var_id, result_vreg))
    | (IntVar left_var_id, IntLit right_lit) ->
      Gcx.emit ~gcx (MovMM (Reg (vreg_of_var left_var_id), Reg result_vreg));
      Gcx.emit ~gcx (SubIR (QuadImmediate right_lit, result_vreg))
    | (IntVar left_var_id, IntVar right_var_id) ->
      Gcx.emit ~gcx (MovMM (Reg (vreg_of_var left_var_id), Reg result_vreg));
      Gcx.emit ~gcx (SubRR (vreg_of_var right_var_id, result_vreg)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Mul
   * ===========================================
   *)
  | Mir.Instruction.Mul (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_var result_var_id in
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIM (QuadImmediate (Int64.mul left_lit right_lit), Reg result_vreg))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (IMulRIR (vreg_of_var arg_var_id, QuadImmediate lit, result_vreg))
    | (IntVar var1, IntVar var2) -> Gcx.emit ~gcx (IMulRR (vreg_of_var var1, vreg_of_var var2)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Div
   * ===========================================
   *)
  | Mir.Instruction.Div (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_var result_var_id in
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIM (QuadImmediate (Int64.div left_lit right_lit), Reg result_vreg))
    | (IntLit lit, IntVar arg_var_id) ->
      Gcx.emit ~gcx (MovIM (QuadImmediate lit, Reg result_vreg));
      Gcx.emit ~gcx (IDivR (vreg_of_var arg_var_id))
    | (IntVar _arg_var_id, IntLit lit) ->
      let reg = mk_vreg () in
      Gcx.emit ~gcx (MovIM (QuadImmediate lit, Reg reg));
      Gcx.emit ~gcx (IDivR reg)
    | (IntVar _var1, IntVar var2) -> Gcx.emit ~gcx (IDivR (vreg_of_var var2)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Neg
   * ===========================================
   *)
  | Mir.Instruction.Neg (result_var_id, arg) :: rest_instructions ->
    let arg_vreg =
      match arg with
      | IntLit _ -> failwith "Constant folding must have already occurred"
      | IntVar var_id -> vreg_of_var var_id
    in
    let result_vreg = vreg_of_var result_var_id in
    Gcx.emit ~gcx (MovMM (Reg arg_vreg, Reg result_vreg));
    Gcx.emit ~gcx (NegR result_vreg);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogNot
   * ===========================================
   *)
  | Mir.Instruction.LogNot (_var_id, arg) :: rest_instructions ->
    let arg_vreg =
      match arg with
      | Lit _ -> failwith "Constant folding must have already occurred"
      | Var var_id -> vreg_of_var var_id
    in
    Gcx.emit ~gcx (NotR arg_vreg);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogAnd
   * ===========================================
   *)
  | Mir.Instruction.LogAnd (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_var result_var_id in
    (match (left_val, right_val) with
    | (Lit left_lit, Lit right_lit) ->
      Gcx.emit ~gcx (MovIM (imm_byte_of_bool (left_lit && right_lit), Reg result_vreg))
    | (Lit lit, Var arg_var_id)
    | (Var arg_var_id, Lit lit) ->
      Gcx.emit ~gcx (AndIR (imm_byte_of_bool lit, vreg_of_var arg_var_id))
    | (Var var1, Var var2) -> Gcx.emit ~gcx (AndRR (vreg_of_var var1, vreg_of_var var2)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogOr
   * ===========================================
   *)
  | Mir.Instruction.LogOr (result_var_id, left_val, right_val) :: rest_instructions ->
    let result_vreg = vreg_of_var result_var_id in
    (match (left_val, right_val) with
    | (Lit left_lit, Lit right_lit) ->
      Gcx.emit ~gcx (MovIM (imm_byte_of_bool (left_lit || right_lit), Reg result_vreg))
    | (Lit lit, Var arg_var_id)
    | (Var arg_var_id, Lit lit) ->
      Gcx.emit ~gcx (OrIR (imm_byte_of_bool lit, vreg_of_var arg_var_id))
    | (Var var1, Var var2) -> Gcx.emit ~gcx (OrRR (vreg_of_var var1, vreg_of_var var2)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Eq
   * ===========================================
   *)
  | [Mir.Instruction.Eq (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp E (Numeric left_val) (Numeric right_val)
  | Mir.Instruction.Eq (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc E result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Neq
   * ===========================================
   *)
  | [Mir.Instruction.Neq (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp NE (Numeric left_val) (Numeric right_val)
  | Mir.Instruction.Neq (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc NE result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Lt
   * ===========================================
   *)
  | [Mir.Instruction.Lt (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp L (Numeric left_val) (Numeric right_val)
  | Mir.Instruction.Lt (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc L result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   LtEq
   * ===========================================
   *)
  | [Mir.Instruction.LtEq (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp LE (Numeric left_val) (Numeric right_val)
  | Mir.Instruction.LtEq (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc LE result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Gt
   * ===========================================
   *)
  | [Mir.Instruction.Gt (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp G (Numeric left_val) (Numeric right_val)
  | Mir.Instruction.Gt (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc G result_var_id left_val right_val;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   GtEq
   * ===========================================
   *)
  | [Mir.Instruction.GtEq (result_var_id, left_val, right_val)] when is_cond_jump result_var_id ->
    gen_cond_jmp GE (Numeric left_val) (Numeric right_val)
  | Mir.Instruction.GtEq (result_var_id, left_val, right_val) :: rest_instructions ->
    gen_set_cc GE result_var_id left_val right_val;
    gen_instructions rest_instructions

and get_resolved_source_value ~func value =
  let open Mir.Instruction.Value in
  let vreg_of_var var_id size =
    let vreg = VReg.of_var_id ~resolution:Unresolved ~func:(Some func) var_id in
    match vreg.resolution with
    | StackSlot mem -> SMem (mem, size)
    | _ -> SVReg (vreg, size)
  in
  match value with
  | Unit Lit -> SImm (ByteImmediate 0)
  | Unit (Var var_id) -> vreg_of_var var_id Byte
  | Bool (Lit b) ->
    SImm
      (ByteImmediate
         ( if b then
           1
         else
           0 ))
  | Bool (Var var_id) -> vreg_of_var var_id Byte
  | Numeric (IntLit i) -> SImm (QuadImmediate i)
  | Numeric (IntVar var_id) -> vreg_of_var var_id Quad
  | Function (Lit name) -> SAddr (mk_label_memory_address name)
  | Function (Var var_id) -> vreg_of_var var_id Quad
  | String _ -> failwith "TODO: Cannot compile string literals"

and mk_label_memory_address label =
  PhysicalAddress { offset = Some (LabelOffset label); base = None; index_and_scale = None }
