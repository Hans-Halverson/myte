open Basic_collections
open Mir

type t = {
  (* Data structures for MIR *)
  main_id: Block.id;
  mutable blocks: Block.t IMap.t;
  mutable globals: Global.t LocMap.t;
  mutable funcs: Function.t LocMap.t;
  mutable global_ids: ISet.t;
  (* Instructions in the block currently being built, in reverse *)
  mutable current_instructions: (Loc.t * Instruction.t) list;
  (* Block ids in the current sequence, in reverse *)
  mutable current_block_sequence_ids: Block.id list;
  (* Current IR variables for all program variables in scope *)
  mutable current_var_ids: var_id LocMap.t list;
}

let mk () =
  {
    main_id = mk_block_id ();
    blocks = IMap.empty;
    globals = LocMap.empty;
    funcs = LocMap.empty;
    global_ids = ISet.empty;
    current_instructions = [];
    current_block_sequence_ids = [];
    current_var_ids = [LocMap.empty];
  }

let add_block ~ecx block = ecx.blocks <- IMap.add block.Block.id block ecx.blocks

let add_global ~ecx global =
  ecx.globals <- LocMap.add global.Global.loc global ecx.globals;
  ecx.global_ids <- ISet.add global.Global.var_id ecx.global_ids

let add_function ~ecx func = ecx.funcs <- LocMap.add func.Function.loc func ecx.funcs

let is_global_id ~ecx var_id = ISet.mem var_id ecx.global_ids

let emit ~ecx loc inst = ecx.current_instructions <- (loc, inst) :: ecx.current_instructions

let finish_block_halt ~ecx =
  let block_id = mk_block_id () in
  let block =
    { Block.id = block_id; instructions = List.rev ecx.current_instructions; next = Halt }
  in
  add_block ~ecx block;
  ecx.current_instructions <- [];
  ecx.current_block_sequence_ids <- block_id :: ecx.current_block_sequence_ids

let start_block_sequence ~ecx =
  ecx.current_instructions <- [];
  ecx.current_block_sequence_ids <- []

let get_block_sequence ~ecx =
  let block_ids = List.rev ecx.current_block_sequence_ids in
  block_ids

let add_variable ~ecx decl_loc var_id =
  match ecx.current_var_ids with
  | [] -> ()
  | hd :: tl -> ecx.current_var_ids <- LocMap.add decl_loc var_id hd :: tl

let update_variable ~ecx decl_loc var_id =
  let rec update_variable_list var_ids_list =
    match var_ids_list with
    | [] -> []
    | hd :: tl ->
      if LocMap.mem decl_loc hd then
        LocMap.add decl_loc var_id hd :: tl
      else
        hd :: update_variable_list tl
  in
  ecx.current_var_ids <- update_variable_list ecx.current_var_ids

let lookup_variable ~ecx decl_loc =
  let rec lookup lst =
    match lst with
    | [] -> failwith "Declaration must have been added"
    | hd :: tl ->
      (match LocMap.find_opt decl_loc hd with
      | None -> lookup tl
      | Some var_id -> var_id)
  in
  lookup ecx.current_var_ids

let enter_variable_scope ~ecx = ecx.current_var_ids <- LocMap.empty :: ecx.current_var_ids

let exit_variable_scope ~ecx = ecx.current_var_ids <- List.tl ecx.current_var_ids

let update_last_instruction_variable ~ecx var_id =
  let open Instruction in
  match ecx.current_instructions with
  | [] -> ()
  | (loc, instr) :: instrs ->
    let instr' =
      match instr with
      | Mov (_, arg) -> Mov (var_id, arg)
      | Ret _ -> instr
      | LogNot (_, arg) -> LogNot (var_id, arg)
      | LogAnd (_, left, right) -> LogAnd (var_id, left, right)
      | LogOr (_, left, right) -> LogOr (var_id, left, right)
      | Neg (_, arg) -> Neg (var_id, arg)
      | Add (_, left, right) -> Add (var_id, left, right)
      | Sub (_, left, right) -> Sub (var_id, left, right)
      | Mul (_, left, right) -> Mul (var_id, left, right)
      | Div (_, left, right) -> Div (var_id, left, right)
      | Eq (_, left, right) -> Eq (var_id, left, right)
      | Neq (_, left, right) -> Neq (var_id, left, right)
      | Lt (_, left, right) -> Lt (var_id, left, right)
      | LtEq (_, left, right) -> LtEq (var_id, left, right)
      | Gt (_, left, right) -> Gt (var_id, left, right)
      | GtEq (_, left, right) -> GtEq (var_id, left, right)
    in
    ecx.current_instructions <- (loc, instr') :: instrs
