open Basic_collections
open Mir
open Mir_visitor

type t = {
  (* The program we are optimizing, is internally mutable *)
  program: ssa_program;
  (* Block id to the blocks it jumps to *)
  mutable next_blocks: IIMMap.t;
  (* Block id to the previous blocks that jump to it *)
  mutable prev_blocks: IIMMap.t;
  (* Variable id to the block that defines it.
     Not updated on block or variable deletions! *)
  mutable var_def_blocks: Block.id IMap.t;
  (* Variable id to the set of blocks that use it.
     Not updated on block or variable deletions! *)
  mutable var_use_blocks: IIMMap.t;
}

let get_block ~ocx block_id = IMap.find block_id ocx.program.blocks

let add_block_link ~ocx prev_block next_block =
  ocx.next_blocks <- IIMMap.add prev_block next_block ocx.next_blocks;
  ocx.prev_blocks <- IIMMap.add next_block prev_block ocx.prev_blocks

let remove_block_link ~ocx prev_block next_block =
  ocx.next_blocks <- IIMMap.remove prev_block next_block ocx.next_blocks;
  ocx.prev_blocks <- IIMMap.remove next_block prev_block ocx.prev_blocks

(* Remove all references to a block from phi nodes of on of its next blocks.
   This may be needed when removing a block or block link. *)
let remove_phi_backreferences_for_block ~ocx block_id_to_remove next_block_id =
  let next_block = get_block ~ocx next_block_id in
  next_block.phis <-
    List.map
      (fun (value_type, dest_var_id, args) ->
        ( value_type,
          dest_var_id,
          IMap.filter (fun prev_block_id _ -> prev_block_id != block_id_to_remove) args ))
      next_block.phis

(* Replace all references to old_block_id in the phis of a block with a new_block_id.
   This may be needed when editing the program. *)
let map_phi_backreferences_for_block ~ocx old_block_id new_block_id block_to_edit =
  let block = get_block ~ocx block_to_edit in
  block.phis <-
    List.map
      (fun (value_type, dest_var_id, args) ->
        ( value_type,
          dest_var_id,
          match IMap.find_opt old_block_id args with
          | None -> args
          | Some var_id -> IMap.add new_block_id var_id args |> IMap.remove old_block_id ))
      block.phis

let can_remove_block ~ocx (block : var_id Block.t) =
  let is_start_block () =
    let func = SMap.find block.func ocx.program.funcs in
    func.body_start_block = block.id
  in
  block.instructions = []
  && block.phis = []
  &&
  match block.next with
  | Halt -> not (is_start_block ())
  | Continue _ -> true
  | Branch _ -> false

let remove_block ~ocx block_id =
  let block = get_block ~ocx block_id in
  (* This may be the first block in a function. If so, update the function to point to the
     next block as the start. *)
  (match block.next with
  | Continue continue_block ->
    let func = SMap.find block.func ocx.program.funcs in
    if func.body_start_block = block_id then func.body_start_block <- continue_block
  | _ -> ());
  let prev_blocks = IMap.find block_id ocx.prev_blocks in
  ISet.iter
    (fun prev_block_id ->
      let prev_block = get_block ~ocx prev_block_id in
      match block.next with
      | Halt -> failwith "TODO: Handle this case"
      | Continue next_id ->
        let map_id id =
          if id = block_id then (
            add_block_link ~ocx prev_block_id next_id;
            map_phi_backreferences_for_block ~ocx block_id prev_block_id next_id;
            next_id
          ) else
            id
        in
        prev_block.next <-
          (match prev_block.next with
          | Halt -> Halt
          | Continue id -> Continue (map_id id)
          | Branch { test; continue; jump } ->
            let new_continue = map_id continue in
            let new_jump = map_id jump in
            (* If both branches points to same label convert to continue *)
            if new_continue = new_jump then
              Continue new_continue
            else
              (* Otherwise create branch to new block *)
              Branch { test; continue = new_continue; jump = new_jump })
      | Branch _ -> ())
    prev_blocks;
  (* Remove references to this removed block from phi nodes of next blocks *)
  let next_blocks = IMap.find block_id ocx.next_blocks in
  ISet.iter
    (fun next_block_id -> remove_phi_backreferences_for_block ~ocx block_id next_block_id)
    next_blocks;
  (* Remove prev pointers from next blocks to this removed block *)
  let next_blocks = IMap.find block_id ocx.next_blocks in
  ISet.iter
    (fun next_block_id -> ocx.prev_blocks <- IIMMap.remove next_block_id block_id ocx.prev_blocks)
    next_blocks;
  (* Remove next pointers from prev blocks to this removed block *)
  ISet.iter
    (fun prev_block_id -> ocx.next_blocks <- IIMMap.remove prev_block_id block_id ocx.next_blocks)
    prev_blocks;
  (* Remove block from remaining maps in context *)
  ocx.prev_blocks <- IMap.remove block_id ocx.prev_blocks;
  ocx.next_blocks <- IMap.remove block_id ocx.next_blocks;
  ocx.program.blocks <- IMap.remove block_id ocx.program.blocks

(* Merge adjacent blocks b1 and b2. Must only be called if b1 and b2 can be merged, meaning
   b1 only continues to b2 and b2 has no other previous blocks. *)
let merge_adjacent_blocks ~ocx block_id1 block_id2 =
  let open Block in
  let b1 = get_block ~ocx block_id1 in
  let b2 = get_block ~ocx block_id2 in
  let map_id id =
    if id = block_id2 then
      block_id1
    else
      id
  in
  b1.instructions <- b1.instructions @ b2.instructions;
  (* Use b2's next, but take care to reference b1 instead of b2 in the case of self references *)
  b1.next <-
    (match b2.next with
    | Halt -> Halt
    | Continue continue -> Continue (map_id continue)
    | Branch ({ continue; jump; _ } as branch) ->
      Branch { branch with continue = map_id continue; jump = map_id jump });
  (* References to the b2 block in phi nodes of blocks that succeed b2 should be rewritten
     to now reference b1 instead. *)
  let next_blocks = IMap.find b2.id ocx.next_blocks in
  ISet.iter
    (fun next_block_id -> map_phi_backreferences_for_block ~ocx b2.id b1.id next_block_id)
    next_blocks;
  (* Set prev pointers for blocks that succeed b2 to point to b1 instead *)
  ISet.iter
    (fun next_block_id ->
      remove_block_link ~ocx b2.id next_block_id;
      add_block_link ~ocx b1.id next_block_id)
    next_blocks;
  remove_block_link ~ocx block_id1 block_id2;
  (* Remove b2 from remaining maps in context *)
  ocx.next_blocks <- IMap.remove b2.id ocx.next_blocks;
  ocx.prev_blocks <- IMap.remove b2.id ocx.prev_blocks;
  ocx.program.blocks <- IMap.remove b2.id ocx.program.blocks

class init_visitor ~ocx =
  object
    inherit [var_id] IRVisitor.t ~program:ocx.program

    method! visit_edge b1 b2 = add_block_link ~ocx b1.id b2.id

    method! visit_result_variable ~block var_id =
      ocx.var_def_blocks <- IMap.add var_id block.id ocx.var_def_blocks

    method! visit_use_variable ~block var_id =
      let new_blocks =
        match IMap.find_opt var_id ocx.var_use_blocks with
        | None -> ISet.singleton block.id
        | Some blocks -> ISet.add block.id blocks
      in
      ocx.var_use_blocks <- IMap.add var_id new_blocks ocx.var_use_blocks
  end

let mk program =
  (* Initialize prev and next block maps *)
  let init_block_mmaps =
    IMap.fold
      (fun block_id _ mmap -> IMap.add block_id ISet.empty mmap)
      program.Program.blocks
      IMap.empty
  in
  let ocx =
    {
      program;
      next_blocks = init_block_mmaps;
      prev_blocks = init_block_mmaps;
      var_def_blocks = IMap.empty;
      var_use_blocks = IMap.empty;
    }
  in
  let init_visitor = new init_visitor ~ocx in
  init_visitor#run ();
  ocx
