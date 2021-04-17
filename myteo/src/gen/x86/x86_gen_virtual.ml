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

type source_value_info =
  | SVImmediate of immediate
  | SVLabel of label * size
  | SVVariable of VirtualRegister.t * size
  | SVStringImmediate of string

let rec gen ~gcx (ir : ssa_program) =
  (* Add init block with initialization of globals *)
  Gcx.start_block ~gcx ~label:"_init" ~mir_block_id:None;
  Gcx.finish_block ~gcx;
  SMap.iter (fun _ global -> gen_global_instruction_builder ~gcx ~ir global) ir.globals;
  (* Remove init block if there are no init sections *)
  if List.length gcx.text = 1 then begin
    gcx.text <- [];
    gcx.blocks_by_id <- IMap.empty
  end;
  SMap.iter (fun _ func -> gen_function_instruction_builder ~gcx ~ir func) ir.funcs;
  Gcx.finish_builders ~gcx

and gen_global_instruction_builder ~gcx ~ir global =
  let open Instruction in
  let init_val_info = get_source_value_info global.init_val in
  match init_val_info with
  | SVImmediate imm ->
    (* Global is initialized to immediate, so insert into initialized data section *)
    let data = { label = global.name; value = ImmediateData imm } in
    Gcx.add_data ~gcx data
  | SVStringImmediate str ->
    (* Global is initialized to string literal, so insert into rodata section *)
    ignore (Gcx.add_string_literal ~gcx ~label:global.name str)
  | SVLabel (label, size) ->
    (* Global is initialized to label. Since this is position independent code we must read the
       label's address at runtime and it cannot be known statically, so place in uninitialized
       (bss) section. *)
    let bss_data = { label = global.name; size = bytes_of_size size } in
    Gcx.add_bss ~gcx bss_data;
    (* Emit init block to move label's address to global *)
    Gcx.start_block ~gcx ~label:("_init_" ^ global.name) ~mir_block_id:None;
    let reg = VirtualRegister.mk () in
    Gcx.emit ~gcx (MovMR (mk_label_memory_address label, reg));
    Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address global.name));
    Gcx.finish_block ~gcx
  | SVVariable (_, size) ->
    (* Global is not initialized to a constant, so it must have its own initialization block.
       Place global in uninitialized (bss) section. *)
    let bss_data = { label = global.name; size = bytes_of_size size } in
    Gcx.add_bss ~gcx bss_data;
    gen_blocks ~gcx ~ir global.init_start_block ("_init_" ^ global.name)

and gen_function_instruction_builder ~gcx ~ir func =
  gen_blocks ~gcx ~ir func.body_start_block func.name

and gen_blocks ~gcx ~ir start_block_id label =
  let ordered_blocks = Block_ordering.order_blocks ~program:ir start_block_id in
  List.iteri
    (fun i mir_block_id ->
      let mir_block = IMap.find mir_block_id ir.blocks in
      let label =
        if i = 0 then
          label
        else
          Gcx.mk_new_label ~gcx
      in
      Gcx.start_block ~gcx ~label ~mir_block_id:(Some mir_block_id);
      gen_instructions ~gcx ~ir ~block:mir_block (List.map snd mir_block.instructions);
      Gcx.finish_block ~gcx)
    ordered_blocks

and gen_instructions ~gcx ~ir ~block instructions =
  let open Instruction in
  let gen_instructions = gen_instructions ~gcx ~ir ~block in
  let is_cond_jump var_id =
    match block.next with
    | Branch { test = Var test_var_id; _ } when test_var_id = var_id -> true
    | _ -> false
  in
  let get_branches () =
    match block.next with
    | Branch { continue; jump; _ } -> (continue, jump)
    | _ -> failwith "Only called on blocks with conditional branches"
  in
  let gen_cond_jmp kind left_val right_val =
    let open Mir.Instruction.NumericValue in
    (match (left_val, right_val) with
    | (IntLit _, IntLit _) -> failwith "Constants must be folded before gen"
    | (IntLit lit, IntVar var_id)
    | (IntVar var_id, IntLit lit) ->
      Gcx.emit ~gcx (CmpRI (var_id, QuadImmediate lit))
    | (IntVar arg1, IntVar arg2) -> Gcx.emit ~gcx (CmpRR (arg1, arg2)));
    let (continue, jump) = get_branches () in
    Gcx.emit ~gcx (CondJmp (kind, Gcx.get_block_id_from_mir_block_id ~gcx jump));
    Gcx.emit ~gcx (Jmp (Gcx.get_block_id_from_mir_block_id ~gcx continue))
  in
  match instructions with
  | [] ->
    (* Conditional jump when the condition is in a variable *)
    (match block.next with
    | Branch { test = Lit _; _ } -> failwith "Dead branch pruning must have already occurred"
    | Continue continue ->
      (* TODO: Create better structure for tracking relative block locations *)
      Gcx.emit ~gcx (Jmp continue)
    | Branch { test = Var var_id; jump; continue } ->
      Gcx.emit ~gcx (TestRR (var_id, var_id));
      Gcx.emit ~gcx (CondJmp (NotEqual, Gcx.get_block_id_from_mir_block_id ~gcx jump));
      Gcx.emit ~gcx (Jmp (Gcx.get_block_id_from_mir_block_id ~gcx continue))
    | _ -> ())
  (*
   * ===========================================
   *                   Mov
   * ===========================================
   *)
  | Mir.Instruction.Mov (var_id, value) :: rest_instructions ->
    let instr =
      match get_source_value_info value with
      | SVImmediate imm -> MovIR (imm, var_id)
      | SVLabel (label, _) -> MovMR (mk_label_memory_address label, var_id)
      | SVVariable (src_var_id, _) -> MovRR (src_var_id, var_id)
      | SVStringImmediate str ->
        let label = Gcx.add_string_literal ~gcx str in
        MovMR (mk_label_memory_address label, var_id)
    in
    Gcx.emit ~gcx instr;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Call
   * ===========================================
   *)
  | Mir.Instruction.Call (_var_id, func_val, arg_vals) :: rest_instructions ->
    (* First six arguments are placed in registers %rdi​, ​%rsi​, ​%rdx​, ​%rcx​, ​%r8​, and ​%r9​ *)
    List.iteri
      (fun i arg_val ->
        if i >= 6 then
          ()
        else
          let reg =
            match get_source_value_info arg_val with
            | SVImmediate imm ->
              let reg = VirtualRegister.mk () in
              Gcx.emit ~gcx (MovIR (imm, reg));
              reg
            | SVStringImmediate str ->
              let reg = VirtualRegister.mk () in
              let label = Gcx.add_string_literal ~gcx str in
              Gcx.emit ~gcx (Lea (mk_label_memory_address label, reg));
              reg
            | SVLabel (label, _) ->
              let reg = VirtualRegister.mk () in
              Gcx.emit ~gcx (Lea (mk_label_memory_address label, reg));
              reg
            | SVVariable (var_id, _) -> var_id
          in
          ignore reg)
      arg_vals;
    (* Later arguments are pushed on stack in reverse order *)
    let rest_arg_vals = List.rev (List_utils.drop 6 arg_vals) in
    List.iter
      (fun arg_val ->
        match get_source_value_info arg_val with
        (* Push does not support 64-bit immediates. Must instead move onto stack. *)
        | SVImmediate (QuadImmediate _) -> failwith "Unimplemented"
        | SVImmediate imm -> Gcx.emit ~gcx (PushI imm)
        | SVStringImmediate str ->
          let label = Gcx.add_string_literal ~gcx str in
          Gcx.emit ~gcx (PushM (mk_label_memory_address label))
        | SVLabel (label, _) -> Gcx.emit ~gcx (PushM (mk_label_memory_address label))
        | SVVariable (var_id, _) -> Gcx.emit ~gcx (PushR var_id))
      rest_arg_vals;
    let inst =
      match func_val with
      | Mir.Instruction.FunctionValue.Lit label -> CallM (mk_label_memory_address label)
      | Mir.Instruction.FunctionValue.Var var_id -> CallR var_id
    in
    Gcx.emit ~gcx inst;
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
      (match get_source_value_info value with
      | SVImmediate imm ->
        let reg = VirtualRegister.mk () in
        Gcx.emit ~gcx (MovIR (imm, reg))
      | SVLabel (label, _) ->
        let reg = VirtualRegister.mk () in
        Gcx.emit ~gcx (MovMR (mk_label_memory_address label, reg))
      | SVStringImmediate str ->
        let label = Gcx.add_string_literal ~gcx str in
        let reg = VirtualRegister.mk () in
        Gcx.emit ~gcx (MovMR (mk_label_memory_address label, reg))
      | SVVariable _ -> ()));
    Gcx.emit ~gcx Ret;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Load Global
   * ===========================================
   *)
  | Mir.Instruction.LoadGlobal (var_id, label) :: rest_instructions ->
    Gcx.emit ~gcx (MovMR (mk_label_memory_address label, var_id));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Store Global
   * ===========================================
   *)
  | Mir.Instruction.StoreGlobal (label, value) :: rest_instructions ->
    (match get_source_value_info value with
    | SVImmediate imm -> Gcx.emit ~gcx (MovIM (imm, mk_label_memory_address label))
    | SVStringImmediate str ->
      let label = Gcx.add_string_literal ~gcx str in
      let reg = VirtualRegister.mk () in
      Gcx.emit ~gcx (MovMR (mk_label_memory_address label, reg));
      Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address label))
    | SVLabel (label, _) ->
      let reg = VirtualRegister.mk () in
      Gcx.emit ~gcx (MovMR (mk_label_memory_address label, reg));
      Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address label))
    | SVVariable (reg, _) -> Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address label)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Add
   * ===========================================
   *)
  | Mir.Instruction.Add (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIR (QuadImmediate (Int64.add left_lit right_lit), var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (AddIR (QuadImmediate lit, arg_var_id))
    | (IntVar var1, IntVar var2) -> Gcx.emit ~gcx (AddRR (var1, var2)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Sub
   * ===========================================
   *)
  | Mir.Instruction.Sub (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIR (QuadImmediate (Int64.sub left_lit right_lit), var_id))
    | (IntLit left_lit, IntVar right_var_id) ->
      Gcx.emit ~gcx (MovIR (QuadImmediate left_lit, var_id));
      Gcx.emit ~gcx (SubRR (var_id, right_var_id))
    | (IntVar left_var_id, IntLit right_lit) ->
      Gcx.emit ~gcx (SubIR (QuadImmediate right_lit, left_var_id))
    | (IntVar left_var, IntVar right_var) -> Gcx.emit ~gcx (SubRR (left_var, right_var)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Mul
   * ===========================================
   *)
  | Mir.Instruction.Mul (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIR (QuadImmediate (Int64.mul left_lit right_lit), var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (IMulRIR (arg_var_id, QuadImmediate lit, var_id))
    | (IntVar var1, IntVar var2) -> Gcx.emit ~gcx (IMulRR (var1, var2)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Div
   * ===========================================
   *)
  | Mir.Instruction.Div (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIR (QuadImmediate (Int64.div left_lit right_lit), var_id))
    | (IntLit lit, IntVar arg_var_id) ->
      Gcx.emit ~gcx (MovIR (QuadImmediate lit, var_id));
      Gcx.emit ~gcx (IDivR arg_var_id)
    | (IntVar _arg_var_id, IntLit lit) ->
      let reg = VirtualRegister.mk () in
      Gcx.emit ~gcx (MovIR (QuadImmediate lit, reg));
      Gcx.emit ~gcx (IDivR reg)
    | (IntVar _var1, IntVar var2) -> Gcx.emit ~gcx (IDivR var2));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Neg
   * ===========================================
   *)
  | Mir.Instruction.Neg (_var_id, arg) :: rest_instructions ->
    let var_id =
      match arg with
      | IntLit _ -> failwith "Constant folding must have already occurred"
      | IntVar var_id -> var_id
    in
    Gcx.emit ~gcx (NegR var_id);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogNot
   * ===========================================
   *)
  | Mir.Instruction.LogNot (_var_id, arg) :: rest_instructions ->
    let var_id =
      match arg with
      | Lit _ -> failwith "Constant folding must have already occurred"
      | Var var_id -> var_id
    in
    Gcx.emit ~gcx (NotR var_id);
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogAnd
   * ===========================================
   *)
  | Mir.Instruction.LogAnd (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (Lit left_lit, Lit right_lit) ->
      Gcx.emit ~gcx (MovIR (imm_byte_of_bool (left_lit && right_lit), var_id))
    | (Lit lit, Var arg_var_id)
    | (Var arg_var_id, Lit lit) ->
      Gcx.emit ~gcx (AndIR (imm_byte_of_bool lit, arg_var_id))
    | (Var var1, Var var2) -> Gcx.emit ~gcx (AndRR (var1, var2)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogOr
   * ===========================================
   *)
  | Mir.Instruction.LogOr (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (Lit left_lit, Lit right_lit) ->
      Gcx.emit ~gcx (MovIR (imm_byte_of_bool (left_lit || right_lit), var_id))
    | (Lit lit, Var arg_var_id)
    | (Var arg_var_id, Lit lit) ->
      Gcx.emit ~gcx (OrIR (imm_byte_of_bool lit, arg_var_id))
    | (Var var1, Var var2) -> Gcx.emit ~gcx (OrRR (var1, var2)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Eq
   * ===========================================
   *)
  | [Mir.Instruction.Eq (var_id, left_val, right_val)] when is_cond_jump var_id ->
    gen_cond_jmp Equal left_val right_val
  | Mir.Instruction.Eq (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIR (imm_byte_of_bool (left_lit = right_lit), var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (CmpRI (arg_var_id, QuadImmediate lit));
      Gcx.emit ~gcx (SetCmp (SetE, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.emit ~gcx (CmpRR (var1, var2));
      Gcx.emit ~gcx (SetCmp (SetE, var_id)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Neq
   * ===========================================
   *)
  | [Mir.Instruction.Neq (var_id, left_val, right_val)] when is_cond_jump var_id ->
    gen_cond_jmp NotEqual left_val right_val
  | Mir.Instruction.Neq (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIR (imm_byte_of_bool (left_lit <> right_lit), var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (CmpRI (arg_var_id, QuadImmediate lit));
      Gcx.emit ~gcx (SetCmp (SetNE, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.emit ~gcx (CmpRR (var1, var2));
      Gcx.emit ~gcx (SetCmp (SetNE, var_id)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Lt
   * ===========================================
   *)
  | [Mir.Instruction.Lt (var_id, left_val, right_val)] when is_cond_jump var_id ->
    gen_cond_jmp LessThan left_val right_val
  | Mir.Instruction.Lt (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIR (imm_byte_of_bool (left_lit < right_lit), var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (CmpRI (arg_var_id, QuadImmediate lit));
      Gcx.emit ~gcx (SetCmp (SetL, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.emit ~gcx (CmpRR (var1, var2));
      Gcx.emit ~gcx (SetCmp (SetL, var_id)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   LtEq
   * ===========================================
   *)
  | [Mir.Instruction.LtEq (var_id, left_val, right_val)] when is_cond_jump var_id ->
    gen_cond_jmp LessThanEqual left_val right_val
  | Mir.Instruction.LtEq (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIR (imm_byte_of_bool (left_lit <= right_lit), var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (CmpRI (arg_var_id, QuadImmediate lit));
      Gcx.emit ~gcx (SetCmp (SetLE, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.emit ~gcx (CmpRR (var1, var2));
      Gcx.emit ~gcx (SetCmp (SetLE, var_id)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Gt
   * ===========================================
   *)
  | [Mir.Instruction.Gt (var_id, left_val, right_val)] when is_cond_jump var_id ->
    gen_cond_jmp GreaterThan left_val right_val
  | Mir.Instruction.Gt (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIR (imm_byte_of_bool (left_lit > right_lit), var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (CmpRI (arg_var_id, QuadImmediate lit));
      Gcx.emit ~gcx (SetCmp (SetG, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.emit ~gcx (CmpRR (var1, var2));
      Gcx.emit ~gcx (SetCmp (SetG, var_id)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   GtEq
   * ===========================================
   *)
  | [Mir.Instruction.GtEq (var_id, left_val, right_val)] when is_cond_jump var_id ->
    gen_cond_jmp GreaterThanEqual left_val right_val
  | Mir.Instruction.GtEq (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIR (imm_byte_of_bool (left_lit >= right_lit), var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (CmpRI (arg_var_id, QuadImmediate lit));
      Gcx.emit ~gcx (SetCmp (SetGE, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.emit ~gcx (CmpRR (var1, var2));
      Gcx.emit ~gcx (SetCmp (SetGE, var_id)));
    gen_instructions rest_instructions

and get_source_value_info value =
  let open Mir.Instruction.Value in
  match value with
  | Unit Lit -> SVImmediate (ByteImmediate 0)
  | Unit (Var var_id) -> SVVariable (var_id, Byte)
  | Bool (Lit b) ->
    SVImmediate
      (ByteImmediate
         ( if b then
           1
         else
           0 ))
  | Bool (Var var_id) -> SVVariable (var_id, Byte)
  | Numeric (IntLit i) -> SVImmediate (QuadImmediate i)
  | Numeric (IntVar var_id) -> SVVariable (var_id, Quad)
  | Function (Lit name) -> SVLabel (name, Quad)
  | Function (Var var_id) -> SVVariable (var_id, Quad)
  | String (Lit str) -> SVStringImmediate str
  | String (Var var_id) -> SVVariable (var_id, Quad)

and mk_label_memory_address label =
  { offset = Some (LabelOffset label); base = IP; index_and_scale = None }
