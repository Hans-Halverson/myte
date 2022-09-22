open Basic_collections
open Mir
open Mir_builders
open Mir_type

(* Promotion of variables on stack into registers joined with Phi nodes.

   Algorithms are based off the paper "Efficiently Computing Single Static Assignment Form and
   the Control Dependence Graph":

   Ron Cytron, Jeanne Ferrante, Barry K. Rosen, Mark N. Wegman, and F. Kenneth Zadeck. 1991.
   Efficiently computing static single assignment form and the control dependence graph.
   ACM Transactions on Programming Languages and Systems, 13(4), 451–490. *)

let rec run ~program =
  (* First clean up unreachable blocks, as they will cause issues with dominator tree construction *)
  program_iter_blocks program (block_remove_if_unreachable ~on_removed_block:ignore);

  SMap.iter
    (fun _ func ->
      (* Collect and remove allocations for all memory values that can be promoted to registers *)
      let pointer_instrs = ref VSet.empty in
      let phi_to_pointer_value = ref VMap.empty in
      func_iter_blocks func (fun block ->
          iter_instructions block (fun instr_value instr ->
              match instr.instr with
              | StackAlloc _ ->
                pointer_instrs := VSet.add instr_value !pointer_instrs;
                remove_instruction instr_value
              | _ -> ()));

      (* Convert to SSA following algorithms from paper *)
      let dt = Dominator_tree.build_dominator_tree ~func in
      insert_phis ~dt ~phi_to_pointer_value !pointer_instrs;
      rename_variables ~dt func !pointer_instrs !phi_to_pointer_value;

      (* Inserted phi instructions may be dead, so run dead instruction elimination pass on them *)
      let phi_instrs = ref VSet.empty in
      VMap.iter
        (fun phi_instr _ -> phi_instrs := VSet.add phi_instr !phi_instrs)
        !phi_to_pointer_value;
      Dead_instruction_elimination.run_worklist !phi_instrs)
    program.Program.funcs

(* Phi insertion algorithm from paper. Inserts empty phis at all join points where they may be needed. *)
and insert_phis ~dt ~phi_to_pointer_value pointer_values =
  let w = ref BlockSet.empty in
  let work = ref BlockMap.empty in
  let has_already = ref BlockMap.empty in
  let iter_count = ref 0 in

  VSet.iter
    (fun pointer_value ->
      iter_count := !iter_count + 1;

      iter_def_blocks pointer_value (fun def_block ->
          work := BlockMap.add def_block !iter_count !work;
          w := BlockSet.add def_block !w);

      while not (BlockSet.is_empty !w) do
        let x = BlockSet.choose !w in
        w := BlockSet.remove x !w;

        let df_x = Dominator_tree.get_dominance_frontier ~dt x in
        BlockMMap.VSet.iter
          (fun y ->
            if find_or_zero y !has_already < !iter_count then (
              create_phi_node ~phi_to_pointer_value pointer_value y;

              has_already := BlockMap.add y !iter_count !has_already;

              if find_or_zero y !work < !iter_count then (
                work := BlockMap.add y !iter_count !work;
                w := BlockSet.add y !w
              )
            ))
          df_x
      done)
    pointer_values

and find_or_zero block block_map = BlockMap.find_opt block block_map |> Option.value ~default:0

(* Iterate over the blocks that contain definitions (aka stores) for a given promoted memory value *)
and iter_def_blocks pointer_value f =
  value_iter_uses ~value:pointer_value (fun use ->
      match use.user.value with
      | Instr { instr = Store (ptr_use, _); block = def_block; _ }
        when ptr_use.value == pointer_value ->
        f def_block
      | _ -> ())

(* Create a phi node for a promoted memory value in the specified block. Phi node will be inserted
   at the end of the list of phi nodes. *)
and create_phi_node ~phi_to_pointer_value pointer_value block =
  let type_ = cast_to_pointer_type (type_of_value pointer_value) in
  let phi = mk_blockless_phi ~type_ ~args:BlockMap.empty in
  phi_to_pointer_value := VMap.add phi pointer_value !phi_to_pointer_value;

  (* Phi is inserted before the first non-phi node (aka at the end of the phi list) *)
  let first_non_phi_instr =
    find_instruction block (fun _ instr ->
        match instr.instr with
        | Instruction.Phi _ -> false
        | _ -> true)
  in
  match first_non_phi_instr with
  | None -> append_instruction block phi
  | Some first_non_phi_instr -> insert_instruction_before ~before:first_non_phi_instr phi

(* Rename loads of promoted memory value to their last definition, and fills out phi args.
   Also rewrites program to remove unnecessary loads, stores, and allocations. *)
and rename_variables
    ~dt (func : Function.t) (pointer_values : VSet.t) (phi_to_pointer_value : Value.t VMap.t) =
  let stacks =
    VSet.fold
      (fun pointer_values acc -> VMap.add pointer_values (Stack.create ()) acc)
      pointer_values
      VMap.empty
  in

  (* Recursive traversal of dominator tree in preorder. Maintains a stack of the last definition
     for each memory value. *)
  let rec search block =
    iter_instructions block (fun instr_value instr ->
        match instr.instr with
        (* Definitions (aka phis or stores) of this promoted memory value are pushed onto the
           memory value's last definition stack. *)
        | Phi _ ->
          let pointer_value = VMap.find instr_value phi_to_pointer_value in
          let stack = VMap.find pointer_value stacks in
          Stack.push instr_value stack
        | Store (ptr_use, arg_use) when VSet.mem ptr_use.value pointer_values ->
          let stack = VMap.find ptr_use.value stacks in
          Stack.push arg_use.value stack
        (* Uses (aka loads) of this promoted memory value are popped off the memory value's last
           definition stack. The load is replaced with the last definition. *)
        | Load ptr_use when VSet.mem ptr_use.value pointer_values ->
          let stack = VMap.find ptr_use.value stacks in
          let top_value = Stack.top stack in
          value_replace_uses ~from:instr_value ~to_:top_value;
          remove_instruction instr_value
        | _ -> ());

    (* Add last definition to the phi nodes of the next blocks *)
    iter_next_blocks block (fun next_block ->
        block_iter_phis next_block (fun phi_val phi ->
            let pointer_value = VMap.find phi_val phi_to_pointer_value in
            let stack = VMap.find pointer_value stacks in
            if not (Stack.is_empty stack) then
              let top_value = Stack.top stack in
              phi_add_arg ~phi_val ~phi ~block ~value:top_value));

    (* Visit all dominated nodes in dominator tree *)
    let children = Dominator_tree.get_children ~dt block in
    BlockMMap.VSet.iter (fun child -> search child) children;

    (* Pop definitions in each stack once all children in dominator tree have been visited *)
    let pop pointer_value = ignore (Stack.pop (VMap.find pointer_value stacks)) in
    iter_instructions block (fun instr_value instr ->
        match instr.instr with
        | Phi _ ->
          let pointer_value = VMap.find instr_value phi_to_pointer_value in
          pop pointer_value
        | Store (ptr_use, _) when VSet.mem ptr_use.value pointer_values ->
          pop ptr_use.value;
          remove_instruction instr_value
        | _ -> ())
  in
  search func.start_block
