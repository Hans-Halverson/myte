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
  | SVVariable of VReg.t * size
  | SVStringImmediate of string

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
  let init_val_info = get_source_value_info ~func:init_func.id global.init_val in
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
    Gcx.start_block
      ~gcx
      ~label:(Some ("_init_" ^ global.name))
      ~func:init_func.id
      ~mir_block_id:None;
    let reg = VReg.mk ~resolution:Unresolved ~func:(Some init_func.id) in
    Gcx.emit ~gcx (MovMR (mk_label_memory_address label, reg));
    Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address global.name));
    Gcx.finish_block ~gcx
  | SVVariable (_, size) ->
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
          Gcx.emit ~gcx (MovRR (Gcx.mk_precolored ~gcx color, param_vreg));
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
              (Some
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
      Gcx.emit ~gcx (CmpRI (vreg_of_var var_id, QuadImmediate lit))
    | (IntVar arg1, IntVar arg2) -> Gcx.emit ~gcx (CmpRR (vreg_of_var arg1, vreg_of_var arg2)));
    let (continue, jump) = get_branches () in
    Gcx.emit ~gcx (JmpCC (invert_condition_code kind, Gcx.get_block_id_from_mir_block_id ~gcx jump));
    Gcx.emit ~gcx (Jmp (Gcx.get_block_id_from_mir_block_id ~gcx continue))
  in
  let gen_set_cc cc result_var_id left_val right_val =
    let open Mir.Instruction.NumericValue in
    let result_vreg = vreg_of_var result_var_id in
    match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.emit ~gcx (MovIR (imm_byte_of_bool (left_lit = right_lit), result_vreg))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (XorRR (result_vreg, result_vreg));
      Gcx.emit ~gcx (CmpRI (vreg_of_var arg_var_id, QuadImmediate lit));
      Gcx.emit ~gcx (SetCC (cc, result_vreg))
    | (IntVar var1, IntVar var2) ->
      Gcx.emit ~gcx (XorRR (result_vreg, result_vreg));
      Gcx.emit ~gcx (CmpRR (vreg_of_var var1, vreg_of_var var2));
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
    | Branch { test = Var var_id; continue; jump } ->
      Gcx.emit ~gcx (TestRR (vreg_of_var var_id, vreg_of_var var_id));
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
      match get_source_value_info ~func value with
      | SVImmediate imm -> MovIR (imm, dest_vreg)
      | SVLabel (label, _) -> MovMR (mk_label_memory_address label, dest_vreg)
      | SVVariable (src_var_id, _) -> MovRR (src_var_id, dest_vreg)
      | SVStringImmediate str ->
        let label = Gcx.add_string_literal ~gcx str in
        MovMR (mk_label_memory_address label, dest_vreg)
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
            (match get_source_value_info ~func arg_val with
            | SVImmediate imm -> Gcx.emit ~gcx (MovIR (imm, vreg))
            | SVStringImmediate str ->
              let label = Gcx.add_string_literal ~gcx str in
              Gcx.emit ~gcx (Lea (mk_label_memory_address label, vreg))
            | SVLabel (label, _) -> Gcx.emit ~gcx (Lea (mk_label_memory_address label, vreg))
            | SVVariable (source_vreg, _) -> Gcx.emit ~gcx (MovRR (source_vreg, vreg))))
      arg_vals;
    (* Later arguments are pushed on stack in reverse order *)
    let rest_arg_vals = List.rev (List_utils.drop 6 arg_vals) in
    List.iter
      (fun arg_val ->
        match get_source_value_info ~func arg_val with
        (* Push does not support 64-bit immediates. Must instead move onto stack. *)
        | SVImmediate (QuadImmediate _) -> failwith "Unimplemented"
        | SVImmediate imm -> Gcx.emit ~gcx (PushI imm)
        | SVStringImmediate str ->
          let label = Gcx.add_string_literal ~gcx str in
          Gcx.emit ~gcx (PushM (mk_label_memory_address label))
        | SVLabel (label, _) -> Gcx.emit ~gcx (PushM (mk_label_memory_address label))
        | SVVariable (var_id, _) -> Gcx.emit ~gcx (PushR var_id))
      rest_arg_vals;
    (* Emit call instruction and move result from register A to return vreg *)
    let inst =
      match func_val with
      | Mir.Instruction.FunctionValue.Lit label -> CallL label
      | Mir.Instruction.FunctionValue.Var var_id -> CallR (vreg_of_var var_id)
    in
    Gcx.emit ~gcx inst;
    Gcx.emit ~gcx (MovRR (Gcx.mk_precolored ~gcx A, vreg_of_var return_var_id));
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
      (match get_source_value_info ~func value with
      | SVImmediate imm ->
        let vreg = Gcx.mk_precolored ~gcx A in
        Gcx.emit ~gcx (MovIR (imm, vreg))
      | SVLabel (label, _) ->
        let vreg = Gcx.mk_precolored ~gcx A in
        Gcx.emit ~gcx (MovMR (mk_label_memory_address label, vreg))
      | SVStringImmediate str ->
        let label = Gcx.add_string_literal ~gcx str in
        let vreg = Gcx.mk_precolored ~gcx A in
        Gcx.emit ~gcx (MovMR (mk_label_memory_address label, vreg))
      | SVVariable (vreg, _) ->
        let vreg_copy = Gcx.mk_precolored ~gcx A in
        Gcx.emit ~gcx (MovRR (vreg, vreg_copy))));
    Gcx.emit ~gcx Ret;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Load Global
   * ===========================================
   *)
  | Mir.Instruction.LoadGlobal (var_id, label) :: rest_instructions ->
    Gcx.emit ~gcx (MovMR (mk_label_memory_address label, vreg_of_var var_id));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Store Global
   * ===========================================
   *)
  | Mir.Instruction.StoreGlobal (label, value) :: rest_instructions ->
    (match get_source_value_info ~func value with
    | SVImmediate imm -> Gcx.emit ~gcx (MovIM (imm, mk_label_memory_address label))
    | SVStringImmediate str ->
      let label = Gcx.add_string_literal ~gcx str in
      let reg = mk_vreg () in
      Gcx.emit ~gcx (MovMR (mk_label_memory_address label, reg));
      Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address label))
    | SVLabel (label, _) ->
      let reg = mk_vreg () in
      Gcx.emit ~gcx (MovMR (mk_label_memory_address label, reg));
      Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address label))
    | SVVariable (reg, _) -> Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address label)));
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
      Gcx.emit ~gcx (MovIR (QuadImmediate (Int64.add left_lit right_lit), result_vreg))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.emit ~gcx (MovRR (vreg_of_var arg_var_id, result_vreg));
      Gcx.emit ~gcx (AddIR (QuadImmediate lit, result_vreg))
    | (IntVar left_var_id, IntVar right_var_id) ->
      Gcx.emit ~gcx (MovRR (vreg_of_var right_var_id, result_vreg));
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
      Gcx.emit ~gcx (MovIR (QuadImmediate (Int64.sub left_lit right_lit), result_vreg))
    | (IntLit left_lit, IntVar right_var_id) ->
      Gcx.emit ~gcx (MovIR (QuadImmediate left_lit, result_vreg));
      Gcx.emit ~gcx (SubRR (vreg_of_var right_var_id, result_vreg))
    | (IntVar left_var_id, IntLit right_lit) ->
      Gcx.emit ~gcx (MovRR (vreg_of_var left_var_id, result_vreg));
      Gcx.emit ~gcx (SubIR (QuadImmediate right_lit, result_vreg))
    | (IntVar left_var_id, IntVar right_var_id) ->
      Gcx.emit ~gcx (MovRR (vreg_of_var left_var_id, result_vreg));
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
      Gcx.emit ~gcx (MovIR (QuadImmediate (Int64.mul left_lit right_lit), result_vreg))
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
      Gcx.emit ~gcx (MovIR (QuadImmediate (Int64.div left_lit right_lit), result_vreg))
    | (IntLit lit, IntVar arg_var_id) ->
      Gcx.emit ~gcx (MovIR (QuadImmediate lit, result_vreg));
      Gcx.emit ~gcx (IDivR (vreg_of_var arg_var_id))
    | (IntVar _arg_var_id, IntLit lit) ->
      let reg = mk_vreg () in
      Gcx.emit ~gcx (MovIR (QuadImmediate lit, reg));
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
    Gcx.emit ~gcx (MovRR (arg_vreg, result_vreg));
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
      Gcx.emit ~gcx (MovIR (imm_byte_of_bool (left_lit && right_lit), result_vreg))
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
      Gcx.emit ~gcx (MovIR (imm_byte_of_bool (left_lit || right_lit), result_vreg))
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

and get_source_value_info ~func value =
  let open Mir.Instruction.Value in
  let vreg_of_var var_id = VReg.of_var_id ~resolution:Unresolved ~func:(Some func) var_id in
  match value with
  | Unit Lit -> SVImmediate (ByteImmediate 0)
  | Unit (Var var_id) -> SVVariable (vreg_of_var var_id, Byte)
  | Bool (Lit b) ->
    SVImmediate
      (ByteImmediate
         ( if b then
           1
         else
           0 ))
  | Bool (Var var_id) -> SVVariable (vreg_of_var var_id, Byte)
  | Numeric (IntLit i) -> SVImmediate (QuadImmediate i)
  | Numeric (IntVar var_id) -> SVVariable (vreg_of_var var_id, Quad)
  | Function (Lit name) -> SVLabel (name, Quad)
  | Function (Var var_id) -> SVVariable (vreg_of_var var_id, Quad)
  | String (Lit str) -> SVStringImmediate str
  | String (Var var_id) -> SVVariable (vreg_of_var var_id, Quad)

and mk_label_memory_address label =
  { offset = Some (LabelOffset label); base = None; index_and_scale = None }
