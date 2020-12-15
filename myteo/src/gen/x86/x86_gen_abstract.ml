open Basic_collections
open Mir
open X86_instructions

let imm_quad imm = ImmediateSource (QuadImmediate imm)

let imm_byte_of_bool bool =
  ImmediateSource
    (ByteImmediate
       ( if bool then
         1
       else
         0 ))

module type ABSTRACT_REGISTER = sig
  type t

  val mk : unit -> t

  val of_var_id : var_id -> t
end

module SSARegister = struct
  type t = var_id

  let mk () = mk_var_id ()

  let of_var_id var_id = var_id
end

module CFRegister = struct
  type t = cf_var

  let mk () = Id (mk_var_id ())

  let of_var_id var_id = Id var_id
end

module AbstractInstruction (Reg : ABSTRACT_REGISTER) = struct
  type t =
    (* Stack instructions *)
    | Push of Reg.t source
    | Pop of Reg.t destination
    (* Data instructions *)
    | Mov of Reg.t source * Reg.t destination
    | Lea of Reg.t memory_address * Reg.t
    (* Numeric operations *)
    | Neg of Reg.t destination
    | Add of Reg.t * Reg.t source (* Operands do not have order *)
    | Sub of Reg.t * Reg.t source (* Left hand side is also dest *)
    | IMul of Reg.t * Reg.t source (* Operands do not have order *)
    | IDiv of Reg.t source
    (* Bitwise operations *)
    | Not of Reg.t destination
    | And of Reg.t * Reg.t source (* Operands do not have order *)
    | Or of Reg.t * Reg.t source (* Operands do not have order *)
    (* Comparisons *)
    | Cmp of Reg.t source * Reg.t source
    | Test of Reg.t source * Reg.t source
    | SetCmp of set_cmp_kind * Reg.t
    (* Control flow *)
    | Jmp of label
    | CondJmp of cond_jmp_kind * label * label (* kind, continue, jump *)
    | Call of Reg.t source
    | Leave
    | Ret
    | Syscall

  type block = {
    label: label;
    mutable instructions: t list;
  }

  type abstract_result = {
    blocks: block SMap.t;
    init_blocks: block list;
    data: data list;
    bss: bss_data list;
    rodata: data list;
  }

  module Gcx = struct
    type t = {
      mutable visited_blocks: SSet.t;
      mutable init_builders: block list;
      mutable in_init: bool;
      mutable data: data list;
      mutable bss: bss_data list;
      mutable rodata: data list;
      mutable current_block_builder: block option;
      mutable block_builders: block SMap.t;
      mutable max_string_literal_id: int;
      mutable max_label_id: int;
      mutable block_labels: label IMap.t;
      ip_var_id: Reg.t;
    }

    let mk () =
      {
        visited_blocks = SSet.empty;
        init_builders = [];
        in_init = true;
        data = [];
        bss = [];
        rodata = [];
        current_block_builder = None;
        block_builders = SMap.empty;
        max_string_literal_id = 0;
        max_label_id = 0;
        block_labels = IMap.empty;
        ip_var_id = Reg.mk ();
      }

    let check_visited_block ~gcx label =
      let is_visited = SSet.mem label gcx.visited_blocks in
      if not is_visited then gcx.visited_blocks <- SSet.add label gcx.visited_blocks;
      is_visited

    let reset_visited_blocks ~gcx = gcx.visited_blocks <- SSet.empty

    let finish_builders ~gcx =
      {
        blocks = gcx.block_builders;
        init_blocks = List.rev gcx.init_builders;
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

    let start_block_builder ~gcx label =
      gcx.current_block_builder <- Some { label; instructions = [] }

    let finish_block_builder ~gcx =
      let block = Option.get gcx.current_block_builder in
      block.instructions <- List.rev block.instructions;
      gcx.block_builders <- SMap.add block.label block gcx.block_builders;
      if gcx.in_init then gcx.init_builders <- block :: gcx.init_builders;
      gcx.current_block_builder <- None

    let emit ~gcx instr =
      let current_block = Option.get gcx.current_block_builder in
      current_block.instructions <- instr :: current_block.instructions

    let get_block_builder ~gcx label = SMap.find label gcx.block_builders

    let get_label ~gcx block_id =
      match IMap.find_opt block_id gcx.block_labels with
      | Some label -> label
      | None ->
        let id = gcx.max_label_id in
        gcx.max_label_id <- gcx.max_label_id + 1;
        let label = ".L" ^ string_of_int id in
        gcx.block_labels <- IMap.add block_id label gcx.block_labels;
        label
  end

  type source_value_info =
    | SVImmediate of immediate
    | SVLabel of label * size
    | SVVariable of Reg.t * size
    | SVStringImmediate of string

  let rec gen (ir : Reg.t Mir.Program.t) =
    let gcx = Gcx.mk () in
    (* Add init block *)
    Gcx.start_block_builder ~gcx "_init";
    Gcx.finish_block_builder ~gcx;
    SMap.iter (fun _ global -> gen_global_instruction_builder ~gcx ~ir global) ir.globals;
    (* Remove init block if there are no init sections *)
    if (List.hd gcx.init_builders).label = "_init" then
      gcx.init_builders <- List.tl gcx.init_builders;
    Gcx.end_init ~gcx;
    SMap.iter (fun _ func -> gen_function_instruction_builder ~gcx ~ir func) ir.funcs;
    Gcx.finish_builders ~gcx

  and gen_global_instruction_builder ~gcx ~ir global =
    let last_init_block = get_block ~ir (List.hd (List.rev global.init)) in
    let init_val =
      match List.rev last_init_block.Block.instructions with
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
      Gcx.start_block_builder ~gcx ("_init_" ^ global.name);
      Gcx.emit ~gcx (Mov (mk_label_memory_read ~gcx label, mk_label_memory_write ~gcx global.name));
      Gcx.finish_block_builder ~gcx
    | SVVariable (var_id, size) ->
      (* Global is not initialized to a constant, so it must have its own initialization block.
         Place global in uninitialized (bss) section. *)
      let bss_data = { label = global.name; size = bytes_of_size size } in
      Gcx.add_bss ~gcx bss_data;
      let init_start_block = IMap.find (List.hd global.init) ir.blocks in
      gen_block ~gcx ~ir ~label:("_init_" ^ global.name) init_start_block;
      Gcx.emit ~gcx (Mov (RegisterSource var_id, mk_label_memory_write ~gcx global.name));
      Gcx.finish_block_builder ~gcx

  and gen_function_instruction_builder ~gcx ~ir func =
    Gcx.reset_visited_blocks ~gcx;
    let func_start_block = IMap.find (List.hd func.body) ir.blocks in
    gen_block ~gcx ~ir ~label:func.name func_start_block

  and gen_block ~gcx ~ir ?label block =
    let label =
      match label with
      | None -> Gcx.get_label ~gcx block.id
      | Some label -> label
    in
    if Gcx.check_visited_block ~gcx label then
      ()
    else (
      Gcx.start_block_builder ~gcx label;
      gen_instructions ~gcx ~ir ~block (List.map snd block.instructions);
      match block.next with
      | Halt -> Gcx.finish_block_builder ~gcx
      | Continue block_id ->
        let block = IMap.find block_id ir.blocks in
        Gcx.finish_block_builder ~gcx;
        gen_block ~gcx ~ir block
      | Branch { continue; jump; _ } ->
        let continue_block = IMap.find continue ir.blocks in
        let jump_block = IMap.find jump ir.blocks in
        Gcx.finish_block_builder ~gcx;
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
        Gcx.emit ~gcx (Cmp (RegisterSource var_id, imm_quad lit))
      | (IntVar arg1, IntVar arg2) -> Gcx.emit ~gcx (Cmp (RegisterSource arg1, RegisterSource arg2)));
      let (continue, jump) = get_branches () in
      Gcx.emit ~gcx (CondJmp (kind, Gcx.get_label ~gcx continue, Gcx.get_label ~gcx jump))
    in
    match instructions with
    | [] ->
      (* Conditional jump when the condition is in a variable *)
      (match block.next with
      | Branch { test = Lit _; _ } -> failwith "Dead branch pruning must have already occurred"
      | Continue continue ->
        (* TODO: Create better structure for tracking relative block locations *)
        Gcx.emit ~gcx (Jmp (Gcx.get_label ~gcx continue))
      | Branch { test = Var var_id; continue; jump } ->
        Gcx.emit ~gcx (Test (RegisterSource var_id, RegisterSource var_id));
        Gcx.emit ~gcx (CondJmp (NotEqual, Gcx.get_label ~gcx continue, Gcx.get_label ~gcx jump))
      | _ -> ())
    (*
     * ===========================================
     *                   Mov
     * ===========================================
     *)
    | Instruction.Mov (var_id, value) :: rest_instructions ->
      let source = get_source_value ~gcx value in
      Gcx.emit ~gcx (Mov (source, RegisterDest var_id));
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
                let reg = Reg.mk () in
                Gcx.emit ~gcx (Mov (ImmediateSource imm, RegisterDest reg));
                reg
              | SVStringImmediate str ->
                let reg = Reg.mk () in
                let label = Gcx.add_string_literal ~gcx str in
                Gcx.emit ~gcx (Lea (mk_label_memory_address ~gcx label, reg));
                reg
              | SVLabel (label, _) ->
                let reg = Reg.mk () in
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
          | SVImmediate imm -> Gcx.emit ~gcx (Push (ImmediateSource imm))
          | SVStringImmediate str ->
            let label = Gcx.add_string_literal ~gcx str in
            Gcx.emit ~gcx (Push (mk_label_memory_read ~gcx label))
          | SVLabel (label, _) -> Gcx.emit ~gcx (Push (mk_label_memory_read ~gcx label))
          | SVVariable (var_id, _) -> Gcx.emit ~gcx (Push (RegisterSource var_id)))
        rest_arg_vals;
      let func =
        match func_val with
        | Instruction.FunctionValue.Lit label -> mk_label_memory_read ~gcx label
        | Instruction.FunctionValue.Var var_id -> RegisterSource var_id
      in
      Gcx.emit ~gcx (Call func);
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
        let emit_mov source =
          let reg = Reg.mk () in
          Gcx.emit ~gcx (Mov (source, RegisterDest reg))
        in
        (match get_source_value_info value with
        | SVImmediate imm -> emit_mov (ImmediateSource imm)
        | SVLabel (label, _) -> emit_mov (mk_label_memory_read ~gcx label)
        | SVStringImmediate str ->
          let label = Gcx.add_string_literal ~gcx str in
          emit_mov (mk_label_memory_read ~gcx label)
        | SVVariable _ -> ()));
      Gcx.emit ~gcx Leave;
      Gcx.emit ~gcx Ret;
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                Load Global
     * ===========================================
     *)
    | Instruction.LoadGlobal (var_id, label) :: rest_instructions ->
      Gcx.emit ~gcx (Mov (mk_label_memory_read ~gcx label, RegisterDest var_id));
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                Store Global
     * ===========================================
     *)
    | Instruction.StoreGlobal (label, value) :: rest_instructions ->
      let source =
        match get_source_value_info value with
        | SVImmediate imm -> ImmediateSource imm
        | SVStringImmediate str ->
          let label = Gcx.add_string_literal ~gcx str in
          let reg = Reg.mk () in
          Gcx.emit ~gcx (Mov (mk_label_memory_read ~gcx label, RegisterDest reg));
          RegisterSource reg
        | SVLabel (label, _) ->
          let reg = Reg.mk () in
          Gcx.emit ~gcx (Mov (mk_label_memory_read ~gcx label, RegisterDest reg));
          RegisterSource reg
        | SVVariable (reg, _) -> RegisterSource reg
      in
      Gcx.emit ~gcx (Mov (source, mk_label_memory_write ~gcx label));
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                   Add
     * ===========================================
     *)
    | Instruction.Add (var_id, left_val, right_val) :: rest_instructions ->
      (match (left_val, right_val) with
      | (IntLit left_lit, IntLit right_lit) ->
        Gcx.emit ~gcx (Mov (imm_quad (Int64.add left_lit right_lit), RegisterDest var_id))
      | (IntLit lit, IntVar arg_var_id)
      | (IntVar arg_var_id, IntLit lit) ->
        Gcx.emit ~gcx (Add (arg_var_id, imm_quad lit))
      | (IntVar var1, IntVar var2) -> Gcx.emit ~gcx (Add (var1, RegisterSource var2)));
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                    Sub
     * ===========================================
     *)
    | Instruction.Sub (var_id, left_val, right_val) :: rest_instructions ->
      (match (left_val, right_val) with
      | (IntLit left_lit, IntLit right_lit) ->
        Gcx.emit ~gcx (Mov (imm_quad (Int64.sub left_lit right_lit), RegisterDest var_id))
      | (IntLit left_lit, IntVar right_var_id) ->
        Gcx.emit ~gcx (Mov (imm_quad left_lit, RegisterDest var_id));
        Gcx.emit ~gcx (Sub (var_id, RegisterSource right_var_id))
      | (IntVar left_var_id, IntLit right_lit) ->
        Gcx.emit ~gcx (Sub (left_var_id, imm_quad right_lit))
      | (IntVar left_var, IntVar right_var) ->
        Gcx.emit ~gcx (Sub (left_var, RegisterSource right_var)));
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                   Mul
     * ===========================================
     *)
    | Instruction.Mul (var_id, left_val, right_val) :: rest_instructions ->
      (match (left_val, right_val) with
      | (IntLit left_lit, IntLit right_lit) ->
        Gcx.emit ~gcx (Mov (imm_quad (Int64.mul left_lit right_lit), RegisterDest var_id))
      | (IntLit lit, IntVar arg_var_id)
      | (IntVar arg_var_id, IntLit lit) ->
        Gcx.emit ~gcx (IMul (arg_var_id, imm_quad lit))
      | (IntVar var1, IntVar var2) -> Gcx.emit ~gcx (IMul (var1, RegisterSource var2)));
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                   Div
     * ===========================================
     *)
    | Instruction.Div (var_id, left_val, right_val) :: rest_instructions ->
      (match (left_val, right_val) with
      | (IntLit left_lit, IntLit right_lit) ->
        Gcx.emit ~gcx (Mov (imm_quad (Int64.div left_lit right_lit), RegisterDest var_id))
      | (IntLit lit, IntVar arg_var_id) ->
        Gcx.emit ~gcx (Mov (imm_quad lit, RegisterDest var_id));
        Gcx.emit ~gcx (IDiv (RegisterSource arg_var_id))
      | (IntVar _arg_var_id, IntLit lit) ->
        let reg = Reg.mk () in
        Gcx.emit ~gcx (Mov (imm_quad lit, RegisterDest reg));
        Gcx.emit ~gcx (IDiv (RegisterSource reg))
      | (IntVar _var1, IntVar var2) -> Gcx.emit ~gcx (IDiv (RegisterSource var2)));
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
      Gcx.emit ~gcx (Neg (RegisterDest var_id));
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
      Gcx.emit ~gcx (Not (RegisterDest var_id));
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                  LogAnd
     * ===========================================
     *)
    | Instruction.LogAnd (var_id, left_val, right_val) :: rest_instructions ->
      (match (left_val, right_val) with
      | (Lit left_lit, Lit right_lit) ->
        Gcx.emit ~gcx (Mov (imm_byte_of_bool (left_lit && right_lit), RegisterDest var_id))
      | (Lit lit, Var arg_var_id)
      | (Var arg_var_id, Lit lit) ->
        Gcx.emit ~gcx (And (arg_var_id, imm_byte_of_bool lit))
      | (Var var1, Var var2) -> Gcx.emit ~gcx (And (var1, RegisterSource var2)));
      gen_instructions rest_instructions
    (*
     * ===========================================
     *                  LogOr
     * ===========================================
     *)
    | Instruction.LogOr (var_id, left_val, right_val) :: rest_instructions ->
      (match (left_val, right_val) with
      | (Lit left_lit, Lit right_lit) ->
        Gcx.emit ~gcx (Mov (imm_byte_of_bool (left_lit || right_lit), RegisterDest var_id))
      | (Lit lit, Var arg_var_id)
      | (Var arg_var_id, Lit lit) ->
        Gcx.emit ~gcx (Or (arg_var_id, imm_byte_of_bool lit))
      | (Var var1, Var var2) -> Gcx.emit ~gcx (Or (var1, RegisterSource var2)));
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
        Gcx.emit ~gcx (Mov (imm_byte_of_bool (left_lit = right_lit), RegisterDest var_id))
      | (IntLit lit, IntVar arg_var_id)
      | (IntVar arg_var_id, IntLit lit) ->
        Gcx.emit ~gcx (Cmp (imm_quad lit, RegisterSource arg_var_id));
        Gcx.emit ~gcx (SetCmp (SetE, var_id))
      | (IntVar var1, IntVar var2) ->
        Gcx.emit ~gcx (Cmp (RegisterSource var1, RegisterSource var2));
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
        Gcx.emit ~gcx (Mov (imm_byte_of_bool (left_lit <> right_lit), RegisterDest var_id))
      | (IntLit lit, IntVar arg_var_id)
      | (IntVar arg_var_id, IntLit lit) ->
        Gcx.emit ~gcx (Cmp (imm_quad lit, RegisterSource arg_var_id));
        Gcx.emit ~gcx (SetCmp (SetNE, var_id))
      | (IntVar var1, IntVar var2) ->
        Gcx.emit ~gcx (Cmp (RegisterSource var1, RegisterSource var2));
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
        Gcx.emit ~gcx (Mov (imm_byte_of_bool (left_lit < right_lit), RegisterDest var_id))
      | (IntLit lit, IntVar arg_var_id)
      | (IntVar arg_var_id, IntLit lit) ->
        Gcx.emit ~gcx (Cmp (imm_quad lit, RegisterSource arg_var_id));
        Gcx.emit ~gcx (SetCmp (SetL, var_id))
      | (IntVar var1, IntVar var2) ->
        Gcx.emit ~gcx (Cmp (RegisterSource var1, RegisterSource var2));
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
        Gcx.emit ~gcx (Mov (imm_byte_of_bool (left_lit <= right_lit), RegisterDest var_id))
      | (IntLit lit, IntVar arg_var_id)
      | (IntVar arg_var_id, IntLit lit) ->
        Gcx.emit ~gcx (Cmp (imm_quad lit, RegisterSource arg_var_id));
        Gcx.emit ~gcx (SetCmp (SetLE, var_id))
      | (IntVar var1, IntVar var2) ->
        Gcx.emit ~gcx (Cmp (RegisterSource var1, RegisterSource var2));
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
        Gcx.emit ~gcx (Mov (imm_byte_of_bool (left_lit > right_lit), RegisterDest var_id))
      | (IntLit lit, IntVar arg_var_id)
      | (IntVar arg_var_id, IntLit lit) ->
        Gcx.emit ~gcx (Cmp (imm_quad lit, RegisterSource arg_var_id));
        Gcx.emit ~gcx (SetCmp (SetG, var_id))
      | (IntVar var1, IntVar var2) ->
        Gcx.emit ~gcx (Cmp (RegisterSource var1, RegisterSource var2));
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
        Gcx.emit ~gcx (Mov (imm_byte_of_bool (left_lit >= right_lit), RegisterDest var_id))
      | (IntLit lit, IntVar arg_var_id)
      | (IntVar arg_var_id, IntLit lit) ->
        Gcx.emit ~gcx (Cmp (imm_quad lit, RegisterSource arg_var_id));
        Gcx.emit ~gcx (SetCmp (SetGE, var_id))
      | (IntVar var1, IntVar var2) ->
        Gcx.emit ~gcx (Cmp (RegisterSource var1, RegisterSource var2));
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

  and get_source_value ~gcx value =
    match get_source_value_info value with
    | SVImmediate imm -> ImmediateSource imm
    | SVLabel (label, _) -> mk_label_memory_read ~gcx label
    | SVVariable (var_id, _) -> RegisterSource var_id
    | SVStringImmediate str ->
      let label = Gcx.add_string_literal ~gcx str in
      mk_label_memory_read ~gcx label

  and mk_label_memory_address ~gcx label =
    { offset = Some (LabelOffset label); base_register = gcx.ip_var_id; index_and_scale = None }

  and mk_label_memory_read ~gcx label = MemorySource (mk_label_memory_address ~gcx label)

  and mk_label_memory_write ~gcx label = MemoryDest (mk_label_memory_address ~gcx label)
end

module CFAbstractInstruction = AbstractInstruction (CFRegister)
module SSAAbstractInstruction = AbstractInstruction (SSARegister)
