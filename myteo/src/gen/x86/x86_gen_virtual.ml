open Basic_collections
open Mir
open X86_instructions

let imm_byte_of_bool bool =
  ByteImmediate
    ( if bool then
      1
    else
      0 )

module VirtualRegister = struct
  type t = var_id

  let mk () = mk_var_id ()

  let of_var_id var_id = var_id
end

module VirtualInstruction = struct
  type virtual_block = var_id Block.t

  module Gcx = struct
    type t = {
      mutable visited_mir_blocks: ISet.t;
      mutable init_builders: virtual_block list;
      mutable in_init: bool;
      mutable text: virtual_block list;
      mutable data: data list;
      mutable bss: bss_data list;
      mutable rodata: data list;
      mutable current_block_builder: virtual_block option;
      mutable blocks_by_id: virtual_block IMap.t;
      mutable mir_block_id_to_x86_block_id: Block.id IMap.t;
      mutable x86_block_id_to_mir_block_id: Mir.Block.id IMap.t;
      mutable max_string_literal_id: int;
      mutable max_label_id: int;
      mutable mir_block_id_to_label: label IMap.t;
      ip_var_id: VirtualRegister.t;
    }

    let mk () =
      {
        visited_mir_blocks = ISet.empty;
        init_builders = [];
        in_init = true;
        text = [];
        data = [];
        bss = [];
        rodata = [];
        current_block_builder = None;
        blocks_by_id = IMap.empty;
        mir_block_id_to_x86_block_id = IMap.empty;
        x86_block_id_to_mir_block_id = IMap.empty;
        max_string_literal_id = 0;
        max_label_id = 0;
        mir_block_id_to_label = IMap.empty;
        ip_var_id = VirtualRegister.mk ();
      }

    let check_visited_block ~gcx (mir_block : var_id Mir.Block.t) =
      let is_visited = ISet.mem mir_block.id gcx.visited_mir_blocks in
      if not is_visited then gcx.visited_mir_blocks <- ISet.add mir_block.id gcx.visited_mir_blocks;
      is_visited

    let reset_visited_blocks ~gcx = gcx.visited_mir_blocks <- ISet.empty

    let finish_builders ~gcx =
      {
        text = List.rev gcx.text;
        data = List.rev gcx.data;
        bss = List.rev gcx.bss;
        rodata = List.rev gcx.rodata;
      }

    let add_data ~gcx d = gcx.data <- d :: gcx.data

    let add_string_literal ~gcx ?label str =
      let label =
        match label with
        | None ->
          let id = gcx.max_string_literal_id in
          gcx.max_string_literal_id <- id + 1;
          ".S" ^ string_of_int id
        | Some label -> label
      in
      let data = { label; value = AsciiData str } in
      gcx.rodata <- data :: gcx.rodata;
      label

    let add_bss ~gcx bd = gcx.bss <- bd :: gcx.bss

    let start_init ~gcx = gcx.in_init <- true

    let end_init ~gcx = gcx.in_init <- false

    let start_block ~gcx ~label ~mir_block =
      let id = Block.mk_id () in
      Option.iter
        (fun { Mir.Block.id = mir_block_id; _ } ->
          gcx.mir_block_id_to_x86_block_id <-
            IMap.add mir_block_id id gcx.mir_block_id_to_x86_block_id;
          gcx.x86_block_id_to_mir_block_id <-
            IMap.add id mir_block_id gcx.x86_block_id_to_mir_block_id)
        mir_block;
      gcx.current_block_builder <- Some { id; label; instructions = [] }

    let finish_block ~gcx =
      let block = Option.get gcx.current_block_builder in
      block.instructions <- List.rev block.instructions;
      gcx.blocks_by_id <- IMap.add block.id block gcx.blocks_by_id;
      gcx.text <- block :: gcx.text;
      if gcx.in_init then gcx.init_builders <- block :: gcx.init_builders;
      gcx.current_block_builder <- None

    let emit ~gcx instr =
      let current_block = Option.get gcx.current_block_builder in
      current_block.instructions <- instr :: current_block.instructions

    let get_label_from_mir_block_id ~gcx mir_block_id =
      match IMap.find_opt mir_block_id gcx.mir_block_id_to_label with
      | Some label -> label
      | None ->
        let id = gcx.max_label_id in
        gcx.max_label_id <- gcx.max_label_id + 1;
        let label = ".L" ^ string_of_int id in
        gcx.mir_block_id_to_label <- IMap.add mir_block_id label gcx.mir_block_id_to_label;
        label
  end

  type source_value_info =
    | SVImmediate of immediate
    | SVLabel of label * size
    | SVVariable of VirtualRegister.t * size
    | SVStringImmediate of string

  let rec gen (ir : ssa_program) =
    let gcx = Gcx.mk () in
    (* Add init block with initialization of globals *)
    Gcx.start_block ~gcx ~label:"_init" ~mir_block:None;
    Gcx.finish_block ~gcx;
    SMap.iter (fun _ global -> gen_global_instruction_builder ~gcx ~ir global) ir.globals;
    (* Remove init block if there are no init sections *)
    if (List.hd gcx.init_builders).label = "_init" then begin
      gcx.init_builders <- [];
      gcx.text <- [];
      gcx.blocks_by_id <- IMap.empty
    end;
    Gcx.end_init ~gcx;
    SMap.iter (fun _ func -> gen_function_instruction_builder ~gcx ~ir func) ir.funcs;
    Gcx.finish_builders ~gcx

  and gen_global_instruction_builder ~gcx ~ir global =
    let last_init_block = get_block ~ir (List.hd (List.rev global.init)) in
    let init_val =
      match List.rev Mir.Block.(last_init_block.instructions) with
      | (_, StoreGlobal (_, init_val)) :: _ -> init_val
      | _ -> failwith "Global init must end with StoreGlobal instruction"
    in
    let init_val_info = get_source_value_info init_val in
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
      Gcx.start_block ~gcx ~label:("_init_" ^ global.name) ~mir_block:None;
      let reg = VirtualRegister.mk () in
      Gcx.emit ~gcx (MovMR (mk_label_memory_address ~gcx label, reg));
      Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address ~gcx global.name));
      Gcx.finish_block ~gcx
    | SVVariable (var_id, size) ->
      (* Global is not initialized to a constant, so it must have its own initialization block.
         Place global in uninitialized (bss) section. *)
      let bss_data = { label = global.name; size = bytes_of_size size } in
      Gcx.add_bss ~gcx bss_data;
      let init_start_block = IMap.find (List.hd global.init) ir.blocks in
      gen_block ~gcx ~ir ~label:("_init_" ^ global.name) init_start_block;
      Gcx.emit ~gcx (MovRM (var_id, mk_label_memory_address ~gcx global.name));
      Gcx.finish_block ~gcx

  and gen_function_instruction_builder ~gcx ~ir func =
    Gcx.reset_visited_blocks ~gcx;
    let func_start_block = IMap.find (List.hd func.body) ir.blocks in
    gen_block ~gcx ~ir ~label:func.name func_start_block

  and gen_block ~gcx ~ir ?label mir_block =
    let label =
      match label with
      | None -> Gcx.get_label_from_mir_block_id ~gcx mir_block.id
      | Some label -> label
    in
    if Gcx.check_visited_block ~gcx mir_block then
      ()
    else (
      Gcx.start_block ~gcx ~label ~mir_block:(Some mir_block);
      gen_instructions ~gcx ~ir ~block:mir_block (List.map snd mir_block.instructions);
      match mir_block.next with
      | Halt -> Gcx.finish_block ~gcx
      | Continue block_id ->
        let block = IMap.find block_id ir.blocks in
        Gcx.finish_block ~gcx;
        gen_block ~gcx ~ir block
      | Branch { continue; jump; _ } ->
        let continue_block = IMap.find continue ir.blocks in
        let jump_block = IMap.find jump ir.blocks in
        Gcx.finish_block ~gcx;
        gen_block ~gcx ~ir continue_block;
        gen_block ~gcx ~ir jump_block
    )

  and gen_instructions ~gcx ~ir ~block instructions =
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
      let open Instruction.NumericValue in
      (match (left_val, right_val) with
      | (IntLit _, IntLit _) -> failwith "Constants must be folded before gen"
      | (IntLit lit, IntVar var_id)
      | (IntVar var_id, IntLit lit) ->
        Gcx.emit ~gcx (CmpRI (var_id, QuadImmediate lit))
      | (IntVar arg1, IntVar arg2) -> Gcx.emit ~gcx (CmpRR (arg1, arg2)));
      let (_, jump) = get_branches () in
      Gcx.emit ~gcx (CondJmp (kind, Gcx.get_label_from_mir_block_id ~gcx jump))
    in
    match instructions with
    | [] ->
      (* Conditional jump when the condition is in a variable *)
      (match block.next with
      | Branch { test = Lit _; _ } -> failwith "Dead branch pruning must have already occurred"
      | Continue continue ->
        (* TODO: Create better structure for tracking relative block locations *)
        Gcx.emit ~gcx (Jmp (Gcx.get_label_from_mir_block_id ~gcx continue))
      | Branch { test = Var var_id; jump; continue = _ } ->
        Gcx.emit ~gcx (TestRR (var_id, var_id));
        Gcx.emit ~gcx (CondJmp (NotEqual, Gcx.get_label_from_mir_block_id ~gcx jump))
      | _ -> ())
    (*
     * ===========================================
     *                   Mov
     * ===========================================
     *)
    | Instruction.Mov (var_id, value) :: rest_instructions ->
      let instr =
        match get_source_value_info value with
        | SVImmediate imm -> MovIR (imm, var_id)
        | SVLabel (label, _) -> MovMR (mk_label_memory_address ~gcx label, var_id)
        | SVVariable (src_var_id, _) -> MovRR (src_var_id, var_id)
        | SVStringImmediate str ->
          let label = Gcx.add_string_literal ~gcx str in
          MovMR (mk_label_memory_address ~gcx label, var_id)
      in
      Gcx.emit ~gcx instr;
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                   Call
     * ===========================================
     *)
    | Instruction.Call (_var_id, func_val, arg_vals) :: rest_instructions ->
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
                Gcx.emit ~gcx (Lea (mk_label_memory_address ~gcx label, reg));
                reg
              | SVLabel (label, _) ->
                let reg = VirtualRegister.mk () in
                Gcx.emit ~gcx (Lea (mk_label_memory_address ~gcx label, reg));
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
            Gcx.emit ~gcx (PushM (mk_label_memory_address ~gcx label))
          | SVLabel (label, _) -> Gcx.emit ~gcx (PushM (mk_label_memory_address ~gcx label))
          | SVVariable (var_id, _) -> Gcx.emit ~gcx (PushR var_id))
        rest_arg_vals;
      let inst =
        match func_val with
        | Instruction.FunctionValue.Lit label -> CallM (mk_label_memory_address ~gcx label)
        | Instruction.FunctionValue.Var var_id -> CallR var_id
      in
      Gcx.emit ~gcx inst;
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                   Ret
     * ===========================================
     *)
    | Instruction.Ret value :: rest_instructions ->
      (match value with
      | None -> ()
      | Some value ->
        (match get_source_value_info value with
        | SVImmediate imm ->
          let reg = VirtualRegister.mk () in
          Gcx.emit ~gcx (MovIR (imm, reg))
        | SVLabel (label, _) ->
          let reg = VirtualRegister.mk () in
          Gcx.emit ~gcx (MovMR (mk_label_memory_address ~gcx label, reg))
        | SVStringImmediate str ->
          let label = Gcx.add_string_literal ~gcx str in
          let reg = VirtualRegister.mk () in
          Gcx.emit ~gcx (MovMR (mk_label_memory_address ~gcx label, reg))
        | SVVariable _ -> ()));
      Gcx.emit ~gcx Ret;
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                Load Global
     * ===========================================
     *)
    | Instruction.LoadGlobal (var_id, label) :: rest_instructions ->
      Gcx.emit ~gcx (MovMR (mk_label_memory_address ~gcx label, var_id));
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                Store Global
     * ===========================================
     *)
    | Instruction.StoreGlobal (label, value) :: rest_instructions ->
      (match get_source_value_info value with
      | SVImmediate imm -> Gcx.emit ~gcx (MovIM (imm, mk_label_memory_address ~gcx label))
      | SVStringImmediate str ->
        let label = Gcx.add_string_literal ~gcx str in
        let reg = VirtualRegister.mk () in
        Gcx.emit ~gcx (MovMR (mk_label_memory_address ~gcx label, reg));
        Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address ~gcx label))
      | SVLabel (label, _) ->
        let reg = VirtualRegister.mk () in
        Gcx.emit ~gcx (MovMR (mk_label_memory_address ~gcx label, reg));
        Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address ~gcx label))
      | SVVariable (reg, _) -> Gcx.emit ~gcx (MovRM (reg, mk_label_memory_address ~gcx label)));
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                   Add
     * ===========================================
     *)
    | Instruction.Add (var_id, left_val, right_val) :: rest_instructions ->
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
    | Instruction.Sub (var_id, left_val, right_val) :: rest_instructions ->
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
    | Instruction.Mul (var_id, left_val, right_val) :: rest_instructions ->
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
    | Instruction.Div (var_id, left_val, right_val) :: rest_instructions ->
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
    | Instruction.Neg (_var_id, arg) :: rest_instructions ->
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
    | Instruction.LogNot (_var_id, arg) :: rest_instructions ->
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
    | Instruction.LogAnd (var_id, left_val, right_val) :: rest_instructions ->
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
    | Instruction.LogOr (var_id, left_val, right_val) :: rest_instructions ->
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
    | [Instruction.Eq (var_id, left_val, right_val)] when is_cond_jump var_id ->
      gen_cond_jmp Equal left_val right_val
    | Instruction.Eq (var_id, left_val, right_val) :: rest_instructions ->
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
    | [Instruction.Neq (var_id, left_val, right_val)] when is_cond_jump var_id ->
      gen_cond_jmp NotEqual left_val right_val
    | Instruction.Neq (var_id, left_val, right_val) :: rest_instructions ->
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
    | [Instruction.Lt (var_id, left_val, right_val)] when is_cond_jump var_id ->
      gen_cond_jmp LessThan left_val right_val
    | Instruction.Lt (var_id, left_val, right_val) :: rest_instructions ->
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
    | [Instruction.LtEq (var_id, left_val, right_val)] when is_cond_jump var_id ->
      gen_cond_jmp LessThanEqual left_val right_val
    | Instruction.LtEq (var_id, left_val, right_val) :: rest_instructions ->
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
    | [Instruction.Gt (var_id, left_val, right_val)] when is_cond_jump var_id ->
      gen_cond_jmp GreaterThan left_val right_val
    | Instruction.Gt (var_id, left_val, right_val) :: rest_instructions ->
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
    | [Instruction.GtEq (var_id, left_val, right_val)] when is_cond_jump var_id ->
      gen_cond_jmp GreaterThanEqual left_val right_val
    | Instruction.GtEq (var_id, left_val, right_val) :: rest_instructions ->
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
    let open Instruction.Value in
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

  and mk_label_memory_address ~gcx label =
    { offset = Some (LabelOffset label); base_register = gcx.ip_var_id; index_and_scale = None }
end
