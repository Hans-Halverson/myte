open Basic_collections
open Mir
open X86_instructions

let rsquad r = RegisterSource (r, Quad)

let isquad i = ImmediateSource (QuadImmediate i)

let rwquad r = RegisterDest (r, Quad)

let msquad offset r index_and_scale = { base_register = (r, Quad); offset; index_and_scale }

let hello_world_executable =
  {
    rodata = [{ label = "msg"; value = AsciiData "Hello World!\n" }];
    bss = [];
    data = [];
    text =
      [
        {
          label = "_main";
          instructions =
            [
              Push (rsquad BP);
              Mov (rsquad SP, rwquad BP);
              Mov (isquad (Int64.of_int 14), rwquad D);
              Lea (msquad (Some (LabelOffset "msg")) IP None, (SI, Quad));
              Mov (isquad Int64.one, rwquad DI);
              Mov (isquad (Int64.of_int 33554436), rwquad A);
              Syscall;
              Mov (isquad Int64.zero, rwquad A);
              Leave;
              Ret;
            ];
        };
      ];
  }

let imm_byte imm = ImmediateSource (ByteImmediate imm)

let imm_byte_of_bool b =
  imm_byte
    ( if b then
      1
    else
      0 )

let imm_quad imm = ImmediateSource (QuadImmediate (Int64.of_int imm))

module InstructionBuilder = struct
  type 'reg t =
    (* Stack instructions *)
    | Push of 'reg source
    | Pop of 'reg destination
    (* Data instructions *)
    | Mov of 'reg source * 'reg destination
    | Lea of 'reg memory_address * 'reg
    (* Numeric operations *)
    | Neg of 'reg destination
    | Add of 'reg * 'reg source (* Operands do not have order *)
    | Sub of 'reg * 'reg source (* Left hand side is also dest *)
    | IMul of 'reg * 'reg source (* Operands do not have order *)
    | IDiv of 'reg source
    (* Bitwise operations *)
    | Not of 'reg destination
    | And of 'reg * 'reg source (* Operands do not have order *)
    | Or of 'reg * 'reg source (* Operands do not have order *)
    (* Comparisons *)
    | Cmp of 'reg source * 'reg source
    | Test of 'reg source * 'reg source
    | SetCmp of set_cmp_kind * 'reg
    (* Control flow *)
    | Jmp of label
    | CondJmp of cond_jmp_kind * label * label (* kind, continue, jump *)
    | Call of 'reg source
    | Leave
    | Ret
    | Syscall

  type 'reg block = {
    label: label;
    mutable instructions: 'reg t list;
  }
end

module IB = InstructionBuilder

module RegisterGraph = struct
  type node = |

  type t = {
    ip_var_id: int;
    sp_var_id: int;
  }

  let mk () = { ip_var_id = mk_var_id (); sp_var_id = mk_var_id () }

  let get_ip_var_id rg = rg.ip_var_id

  let get_sp_var_id rg = rg.sp_var_id
end

module GenContext = struct
  type t = {
    mutable visited_blocks: SSet.t;
    mutable init_builders: var_id IB.block list;
    mutable init_blocks: var_id block list;
    mutable in_init: bool;
    mutable text: var_id block list;
    mutable data: data list;
    mutable bss: bss_data list;
    mutable rodata: data list;
    mutable current_block_builder: var_id IB.block option;
    mutable current_block: var_id block option;
    mutable block_builders: var_id IB.block SMap.t;
    mutable blocks: var_id block SMap.t;
    mutable string_literal_id: int;
    mutable label_id: int;
    mutable block_labels: label IMap.t;
    rg: RegisterGraph.t;
  }

  let mk () =
    {
      visited_blocks = SSet.empty;
      init_builders = [];
      init_blocks = [];
      in_init = true;
      text = [];
      data = [];
      bss = [];
      rodata = [];
      current_block_builder = None;
      current_block = None;
      block_builders = SMap.empty;
      blocks = SMap.empty;
      string_literal_id = 0;
      label_id = 0;
      block_labels = IMap.empty;
      rg = RegisterGraph.mk ();
    }

  let check_visited_block ~gcx label =
    let is_visited = SSet.mem label gcx.visited_blocks in
    if not is_visited then gcx.visited_blocks <- SSet.add label gcx.visited_blocks;
    is_visited

  let reset_visited_blocks ~gcx = gcx.visited_blocks <- SSet.empty

  let finish_builders ~gcx =
    gcx.init_builders <- List.rev gcx.init_builders;
    gcx.data <- List.rev gcx.data;
    gcx.bss <- List.rev gcx.bss;
    gcx.rodata <- List.rev gcx.rodata;
    gcx.current_block_builder <- None

  let finish_blocks ~gcx =
    gcx.init_blocks <- List.rev gcx.init_blocks;
    gcx.text <- List.rev gcx.text;
    gcx.current_block <- None

  let add_data ~gcx d = gcx.data <- d :: gcx.data

  let add_string_literal ~gcx ?label str =
    let label =
      match label with
      | None ->
        let id = gcx.string_literal_id in
        gcx.string_literal_id <- id + 1;
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

  let start_block ~gcx label = gcx.current_block <- Some { label; instructions = [] }

  let finish_block_builder ~gcx =
    let block = Option.get gcx.current_block_builder in
    block.instructions <- List.rev block.instructions;
    gcx.block_builders <- SMap.add block.label block gcx.block_builders;
    if gcx.in_init then gcx.init_builders <- block :: gcx.init_builders;
    gcx.current_block_builder <- None

  let finish_block ~gcx =
    let block = Option.get gcx.current_block in
    block.instructions <- List.rev block.instructions;
    gcx.blocks <- SMap.add block.label block gcx.blocks;
    gcx.text <- block :: gcx.text;
    if gcx.in_init then gcx.init_blocks <- block :: gcx.init_blocks;
    gcx.current_block <- None

  let ib_emit ~gcx instr =
    let current_block = Option.get gcx.current_block_builder in
    current_block.instructions <- instr :: current_block.instructions

  let emit ~gcx instr =
    let current_block = Option.get gcx.current_block in
    current_block.instructions <- instr :: current_block.instructions

  let get_block_builder ~gcx label = SMap.find label gcx.block_builders

  let get_block ~gcx label = SMap.find label gcx.blocks

  let get_label ~gcx block_id =
    match IMap.find_opt block_id gcx.block_labels with
    | Some label -> label
    | None ->
      let id = gcx.label_id in
      gcx.label_id <- gcx.label_id + 1;
      let label = ".L" ^ string_of_int id in
      gcx.block_labels <- IMap.add block_id label gcx.block_labels;
      label
end

module Gcx = GenContext

type source_value_info =
  | SVImmediate of immediate
  | SVLabel of label * size
  | SVVariable of var_id * size
  | SVStringImmediate of string

let rec gen_x86_executable ir =
  let gcx = Gcx.mk () in
  gen_instruction_builder ~gcx ~ir;
  get_constraints ~gcx ~ir

and gen_instruction_builder ~gcx ~(ir : var_id Program.t) =
  (* Add init block *)
  Gcx.start_block_builder ~gcx "_init";
  Gcx.finish_block_builder ~gcx;
  SMap.iter (fun _ global -> gen_global_instruction_builder ~gcx ~ir global) ir.globals;
  (* Remove init block if there are no init sections *)
  if (List.hd gcx.init_builders).label = "_init" then gcx.init_builders <- List.tl gcx.init_builders;
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
    Gcx.ib_emit ~gcx (Mov (mk_label_memory_read ~gcx label, mk_label_memory_write ~gcx global.name));
    Gcx.finish_block_builder ~gcx
  | SVVariable (var_id, size) ->
    (* Global is not initialized to a constant, so it must have its own initialization block.
       Place global in uninitialized (bss) section. *)
    let bss_data = { label = global.name; size = bytes_of_size size } in
    Gcx.add_bss ~gcx bss_data;
    let init_start_block = IMap.find (List.hd global.init) ir.blocks in
    gen_block ~gcx ~ir ~label:("_init_" ^ global.name) init_start_block;
    Gcx.ib_emit ~gcx (Mov (RegisterSource var_id, mk_label_memory_write ~gcx global.name));
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
      Gcx.ib_emit ~gcx (IB.Cmp (RegisterSource var_id, imm_quad lit))
    | (IntVar arg1, IntVar arg2) ->
      Gcx.ib_emit ~gcx (IB.Cmp (RegisterSource arg1, RegisterSource arg2)));
    let (continue, jump) = get_branches () in
    Gcx.ib_emit ~gcx (IB.CondJmp (kind, Gcx.get_label ~gcx continue, Gcx.get_label ~gcx jump))
  in
  match instructions with
  | [] ->
    (* Conditional jump when the condition is in a variable *)
    (match block.next with
    | Branch { test = Lit _; _ } -> failwith "Dead branch pruning must have already occurred"
    | Continue continue ->
      (* TODO: Create better structure for tracking relative block locations *)
      Gcx.ib_emit ~gcx (IB.Jmp (Gcx.get_label ~gcx continue))
    | Branch { test = Var var_id; continue; jump } ->
      Gcx.ib_emit ~gcx (IB.Test (RegisterSource var_id, RegisterSource var_id));
      Gcx.ib_emit ~gcx (IB.CondJmp (NotEqual, Gcx.get_label ~gcx continue, Gcx.get_label ~gcx jump))
    | _ -> ())
  (*
   * ===========================================
   *                   Mov
   * ===========================================
   *)
  | Instruction.Mov (var_id, value) :: rest_instructions ->
    let source = get_source_value ~gcx value in
    Gcx.ib_emit ~gcx (IB.Mov (source, RegisterDest var_id));
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
          let var_id =
            match get_source_value_info arg_val with
            | SVImmediate imm ->
              let var_id = mk_var_id () in
              Gcx.ib_emit ~gcx (Mov (ImmediateSource imm, RegisterDest var_id));
              var_id
            | SVStringImmediate str ->
              let var_id = mk_var_id () in
              let label = Gcx.add_string_literal ~gcx str in
              Gcx.ib_emit ~gcx (Lea (mk_label_memory_address ~gcx label, var_id));
              var_id
            | SVLabel (label, _) ->
              let var_id = mk_var_id () in
              Gcx.ib_emit ~gcx (Lea (mk_label_memory_address ~gcx label, var_id));
              var_id
            | SVVariable (var_id, _) -> var_id
          in
          ignore var_id)
      arg_vals;
    (* Later arguments are pushed on stack in reverse order *)
    let rest_arg_vals = List.rev (List_utils.drop 6 arg_vals) in
    List.iter
      (fun arg_val ->
        match get_source_value_info arg_val with
        | SVImmediate imm -> Gcx.ib_emit ~gcx (Push (ImmediateSource imm))
        | SVStringImmediate str ->
          let label = Gcx.add_string_literal ~gcx str in
          Gcx.ib_emit ~gcx (Push (mk_label_memory_read ~gcx label))
        | SVLabel (label, _) -> Gcx.ib_emit ~gcx (Push (mk_label_memory_read ~gcx label))
        | SVVariable (var_id, _) -> Gcx.ib_emit ~gcx (Push (RegisterSource var_id)))
      rest_arg_vals;
    let func =
      match func_val with
      | Instruction.FunctionValue.Lit label -> mk_label_memory_read ~gcx label
      | Instruction.FunctionValue.Var var_id -> RegisterSource var_id
    in
    Gcx.ib_emit ~gcx (IB.Call func);
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
        let var_id = mk_var_id () in
        Gcx.ib_emit ~gcx (IB.Mov (source, RegisterDest var_id))
      in
      (match get_source_value_info value with
      | SVImmediate imm -> emit_mov (ImmediateSource imm)
      | SVLabel (label, _) -> emit_mov (mk_label_memory_read ~gcx label)
      | SVStringImmediate str ->
        let label = Gcx.add_string_literal ~gcx str in
        emit_mov (mk_label_memory_read ~gcx label)
      | SVVariable _ -> ()));
    Gcx.ib_emit ~gcx IB.Leave;
    Gcx.ib_emit ~gcx IB.Ret;
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                Load Global
   * ===========================================
   *)
  | Instruction.LoadGlobal (var_id, label) :: rest_instructions ->
    Gcx.ib_emit ~gcx (IB.Mov (mk_label_memory_read ~gcx label, RegisterDest var_id));
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
        let var_id = mk_var_id () in
        Gcx.ib_emit ~gcx (IB.Mov (mk_label_memory_read ~gcx label, RegisterDest var_id));
        RegisterSource var_id
      | SVLabel (label, _) ->
        let var_id = mk_var_id () in
        Gcx.ib_emit ~gcx (IB.Mov (mk_label_memory_read ~gcx label, RegisterDest var_id));
        RegisterSource var_id
      | SVVariable (var_id, _) -> RegisterSource var_id
    in
    Gcx.ib_emit ~gcx (IB.Mov (source, mk_label_memory_write ~gcx label));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Add
   * ===========================================
   *)
  | Instruction.Add (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.ib_emit ~gcx (IB.Mov (imm_quad (left_lit + right_lit), RegisterDest var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.ib_emit ~gcx (IB.Add (arg_var_id, imm_quad lit))
    | (IntVar var1, IntVar var2) -> Gcx.ib_emit ~gcx (IB.Add (var1, RegisterSource var2)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                    Sub
   * ===========================================
   *)
  | Instruction.Sub (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.ib_emit ~gcx (IB.Mov (imm_quad (left_lit - right_lit), RegisterDest var_id))
    | (IntLit left_lit, IntVar right_var_id) ->
      Gcx.ib_emit ~gcx (IB.Mov (imm_quad left_lit, RegisterDest var_id));
      Gcx.ib_emit ~gcx (IB.Sub (var_id, RegisterSource right_var_id))
    | (IntVar left_var_id, IntLit right_lit) ->
      Gcx.ib_emit ~gcx (IB.Sub (left_var_id, imm_quad right_lit))
    | (IntVar left_var, IntVar right_var) ->
      Gcx.ib_emit ~gcx (IB.Sub (left_var, RegisterSource right_var)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Mul
   * ===========================================
   *)
  | Instruction.Mul (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.ib_emit ~gcx (IB.Mov (imm_quad (left_lit * right_lit), RegisterDest var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.ib_emit ~gcx (IB.IMul (arg_var_id, imm_quad lit))
    | (IntVar var1, IntVar var2) -> Gcx.ib_emit ~gcx (IB.IMul (var1, RegisterSource var2)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                   Div
   * ===========================================
   *)
  | Instruction.Div (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (IntLit left_lit, IntLit right_lit) ->
      Gcx.ib_emit ~gcx (IB.Mov (imm_quad (left_lit / right_lit), RegisterDest var_id))
    | (IntLit lit, IntVar arg_var_id) ->
      Gcx.ib_emit ~gcx (IB.Mov (imm_quad lit, RegisterDest var_id));
      Gcx.ib_emit ~gcx (IB.IDiv (RegisterSource arg_var_id))
    | (IntVar _arg_var_id, IntLit lit) ->
      let var_id = mk_var_id () in
      Gcx.ib_emit ~gcx (IB.Mov (imm_quad lit, RegisterDest var_id));
      Gcx.ib_emit ~gcx (IB.IDiv (RegisterSource var_id))
    | (IntVar _var1, IntVar var2) -> Gcx.ib_emit ~gcx (IB.IDiv (RegisterSource var2)));
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
    Gcx.ib_emit ~gcx (IB.Neg (RegisterDest var_id));
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
    Gcx.ib_emit ~gcx (IB.Not (RegisterDest var_id));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogAnd
   * ===========================================
   *)
  | Instruction.LogAnd (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (Lit left_lit, Lit right_lit) ->
      Gcx.ib_emit ~gcx (IB.Mov (imm_byte_of_bool (left_lit && right_lit), RegisterDest var_id))
    | (Lit lit, Var arg_var_id)
    | (Var arg_var_id, Lit lit) ->
      Gcx.ib_emit ~gcx (IB.And (arg_var_id, imm_byte_of_bool lit))
    | (Var var1, Var var2) -> Gcx.ib_emit ~gcx (IB.And (var1, RegisterSource var2)));
    gen_instructions rest_instructions
  (*
   * ===========================================
   *                  LogOr
   * ===========================================
   *)
  | Instruction.LogOr (var_id, left_val, right_val) :: rest_instructions ->
    (match (left_val, right_val) with
    | (Lit left_lit, Lit right_lit) ->
      Gcx.ib_emit ~gcx (IB.Mov (imm_byte_of_bool (left_lit || right_lit), RegisterDest var_id))
    | (Lit lit, Var arg_var_id)
    | (Var arg_var_id, Lit lit) ->
      Gcx.ib_emit ~gcx (IB.Or (arg_var_id, imm_byte_of_bool lit))
    | (Var var1, Var var2) -> Gcx.ib_emit ~gcx (IB.Or (var1, RegisterSource var2)));
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
      Gcx.ib_emit ~gcx (IB.Mov (imm_byte_of_bool (left_lit = right_lit), RegisterDest var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.ib_emit ~gcx (IB.Cmp (imm_quad lit, RegisterSource arg_var_id));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetE, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.ib_emit ~gcx (IB.Cmp (RegisterSource var1, RegisterSource var2));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetE, var_id)));
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
      Gcx.ib_emit ~gcx (IB.Mov (imm_byte_of_bool (left_lit <> right_lit), RegisterDest var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.ib_emit ~gcx (IB.Cmp (imm_quad lit, RegisterSource arg_var_id));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetNE, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.ib_emit ~gcx (IB.Cmp (RegisterSource var1, RegisterSource var2));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetNE, var_id)));
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
      Gcx.ib_emit ~gcx (IB.Mov (imm_byte_of_bool (left_lit < right_lit), RegisterDest var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.ib_emit ~gcx (IB.Cmp (imm_quad lit, RegisterSource arg_var_id));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetL, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.ib_emit ~gcx (IB.Cmp (RegisterSource var1, RegisterSource var2));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetL, var_id)));
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
      Gcx.ib_emit ~gcx (IB.Mov (imm_byte_of_bool (left_lit <= right_lit), RegisterDest var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.ib_emit ~gcx (IB.Cmp (imm_quad lit, RegisterSource arg_var_id));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetLE, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.ib_emit ~gcx (IB.Cmp (RegisterSource var1, RegisterSource var2));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetLE, var_id)));
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
      Gcx.ib_emit ~gcx (IB.Mov (imm_byte_of_bool (left_lit > right_lit), RegisterDest var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.ib_emit ~gcx (IB.Cmp (imm_quad lit, RegisterSource arg_var_id));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetG, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.ib_emit ~gcx (IB.Cmp (RegisterSource var1, RegisterSource var2));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetG, var_id)));
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
      Gcx.ib_emit ~gcx (IB.Mov (imm_byte_of_bool (left_lit >= right_lit), RegisterDest var_id))
    | (IntLit lit, IntVar arg_var_id)
    | (IntVar arg_var_id, IntLit lit) ->
      Gcx.ib_emit ~gcx (IB.Cmp (imm_quad lit, RegisterSource arg_var_id));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetGE, var_id))
    | (IntVar var1, IntVar var2) ->
      Gcx.ib_emit ~gcx (IB.Cmp (RegisterSource var1, RegisterSource var2));
      Gcx.ib_emit ~gcx (IB.SetCmp (SetGE, var_id)));
    gen_instructions rest_instructions

and get_constraints ~gcx ~ir : var_id X86_instructions.executable =
  let rec visit_block (block : var_id IB.block) =
    let label = block.label in
    if Gcx.check_visited_block ~gcx label then
      ()
    else (
      Gcx.start_block ~gcx label;
      List.iter
        (fun instr_builder ->
          let instr =
            match instr_builder with
            | IB.Push arg -> Push arg
            | IB.Pop arg -> Pop arg
            | IB.Mov (source, dest) -> Mov (source, dest)
            | IB.Lea (addr, reg) -> Lea (addr, reg)
            | IB.Neg arg -> Neg arg
            | IB.Add (reg, source) -> Add (source, RegisterDest reg)
            | IB.Sub (dest_reg, source) -> Sub (source, RegisterDest dest_reg)
            | IB.IMul (arg_reg, arg_source) -> IMul (arg_source, arg_reg)
            | IB.IDiv source -> IDiv source
            | IB.Not arg -> Not arg
            | IB.And (reg, source) -> Add (source, RegisterDest reg)
            | IB.Or (reg, source) -> Add (source, RegisterDest reg)
            | IB.Cmp (arg1, arg2) -> Cmp (arg1, arg2)
            | IB.Test (arg1, arg2) -> Test (arg1, arg2)
            | IB.SetCmp (set_cmp_kind, reg) -> SetCmp (set_cmp_kind, reg)
            | IB.Jmp label -> Jmp label
            | IB.CondJmp (cond_jmp_kind, _continue, jump) -> CondJmp (cond_jmp_kind, jump)
            | IB.Call source -> Call source
            | IB.Leave -> Leave
            | IB.Ret -> Ret
            | IB.Syscall -> Syscall
          in
          Gcx.emit ~gcx instr)
        block.instructions;
      Gcx.finish_block ~gcx;
      match List_utils.last block.instructions with
      | IB.Jmp label ->
        let block = Gcx.get_block_builder ~gcx label in
        visit_block block
      | IB.CondJmp (_, continue, jump) ->
        let continue_block = Gcx.get_block_builder ~gcx continue in
        let jump_block = Gcx.get_block_builder ~gcx jump in
        visit_block continue_block;
        visit_block jump_block
      | _ -> ()
    )
  in
  Gcx.start_init ~gcx;
  List.iter (fun block -> visit_block block) gcx.init_builders;
  Gcx.end_init ~gcx;
  SMap.iter
    (fun name _ ->
      Gcx.reset_visited_blocks ~gcx;
      visit_block (SMap.find name gcx.block_builders))
    ir.funcs;
  Gcx.finish_blocks ~gcx;
  { text = gcx.text; data = gcx.data; rodata = gcx.rodata; bss = gcx.bss }

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
  | Numeric (IntLit i) -> SVImmediate (QuadImmediate (Int64.of_int i))
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
  {
    offset = Some (LabelOffset label);
    base_register = RegisterGraph.get_ip_var_id gcx.rg;
    index_and_scale = None;
  }

and mk_label_memory_read ~gcx label = MemorySource (mk_label_memory_address ~gcx label)

and mk_label_memory_write ~gcx label = MemoryDest (mk_label_memory_address ~gcx label)
