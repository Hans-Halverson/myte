open Mir
open Mir_builders
open Mir_type

(* Promotion of variables on stack into registers joined with Phi nodes.

   Algorithms are based off the paper "Efficiently Computing Single Static Assignment Form and
   the Control Dependence Graph":

   Ron Cytron, Jeanne Ferrante, Barry K. Rosen, Mark N. Wegman, and F. Kenneth Zadeck. 1991.
   Efficiently computing static single assignment form and the control dependence graph.
   ACM Transactions on Programming Languages and Systems, 13(4), 451–490. *)

(* Implement to allow promoting particular patterns to a "promotable item", which will be promoted
   into a variable that is used directly throughout the program. *)
module type SSA_TRANFORMER_CONTEXT = sig
  type t

  type promotable_item

  (* Return whether the current instruction is promotable and should be deleted *)
  val check_promotable_instr : cx:t -> Value.t -> Instruction.t -> bool

  val iter_promotable_items : cx:t -> (promotable_item -> unit) -> unit

  val iter_promotable_item_def_blocks : cx:t -> promotable_item -> (Block.t -> unit) -> unit

  val create_phi_node : cx:t -> promotable_item -> Value.t

  val get_promoted_item_def_stack : cx:t -> promotable_item -> Value.t Stack.t

  val get_phi_promoted_item : cx:t -> Value.t -> Instruction.t -> promotable_item option

  val get_store_promoted_item : cx:t -> Value.t -> Instruction.t -> (promotable_item * Use.t) option

  val get_load_promoted_item : cx:t -> Value.t -> Instruction.t -> promotable_item option

  val debug_string_of_promotable_item : promotable_item -> string
end

module SSATransformer (Context : SSA_TRANFORMER_CONTEXT) = struct
  let rec run ~cx ~func =
    (* First clean up unreachable blocks, as they will cause issues with dominator tree construction *)
    remove_unreachable_blocks_in_func func;

    (* Collect and remove allocations for all memory values that can be promoted to registers *)
    func_iter_blocks func (fun block ->
        iter_instructions block (fun instr_value instr ->
            if Context.check_promotable_instr ~cx instr_value instr then
              remove_instruction instr_value));

    (* Convert to SSA following algorithms from paper *)
    let dt = Dominator_tree.build_dominator_tree ~func in
    let phis = ref VSet.empty in
    insert_phis ~cx ~dt ~phis;
    rename_variables ~cx ~dt func;

    (* Inserted phi instructions may be dead, so run dead instruction elimination pass on them *)
    Dead_instruction_elimination.run_worklist !phis

  (* Phi insertion algorithm from paper. Inserts empty phis at all join points where they may be needed. *)
  and insert_phis ~cx ~dt ~phis =
    let w = ref BlockSet.empty in
    let work = ref BlockMap.empty in
    let has_already = ref BlockMap.empty in
    let iter_count = ref 0 in

    Context.iter_promotable_items ~cx (fun promotable_item ->
        iter_count := !iter_count + 1;

        Context.iter_promotable_item_def_blocks ~cx promotable_item (fun def_block ->
            work := BlockMap.add def_block !iter_count !work;
            w := BlockSet.add def_block !w);

        while not (BlockSet.is_empty !w) do
          let x = BlockSet.choose !w in
          w := BlockSet.remove x !w;

          let df_x = Dominator_tree.get_dominance_frontier ~dt x in
          BlockSet.iter
            (fun y ->
              if find_or_zero y !has_already < !iter_count then (
                create_phi_node ~cx ~phis promotable_item y;

                has_already := BlockMap.add y !iter_count !has_already;

                if find_or_zero y !work < !iter_count then (
                  work := BlockMap.add y !iter_count !work;
                  w := BlockSet.add y !w
                )
              ))
            df_x
        done)

  and find_or_zero block block_map = BlockMap.find_opt block block_map |> Option.value ~default:0

  (* Create a phi node for a promoted memory value in the specified block. Phi node will be inserted
     at the end of the list of phi nodes. *)
  and create_phi_node ~cx ~phis promoted_item block =
    let phi = Context.create_phi_node ~cx promoted_item in
    phis := VSet.add phi !phis;

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
  and rename_variables ~cx ~dt (func : Function.t) =
    (* Recursive traversal of dominator tree in preorder. Maintains a stack of the last definition
       for each memory value. *)
    let rec search block =
      iter_instructions block (fun instr_value instr ->
          (* Definitions (aka phis or stores) of this promoted memory value are pushed onto the
             memory value's last definition stack. *)
          (match Context.get_phi_promoted_item ~cx instr_value instr with
          | Some promoted_item ->
            let stack = Context.get_promoted_item_def_stack ~cx promoted_item in
            Stack.push instr_value stack
          | _ -> ());

          (match Context.get_store_promoted_item ~cx instr_value instr with
          | Some (promoted_item, arg_use) ->
            let stack = Context.get_promoted_item_def_stack ~cx promoted_item in
            Stack.push arg_use.value stack
          | _ -> ());

          match Context.get_load_promoted_item ~cx instr_value instr with
          | Some promoted_item ->
            let stack = Context.get_promoted_item_def_stack ~cx promoted_item in
            let top_value = Stack.top stack in
            value_replace_uses ~from:instr_value ~to_:top_value;
            remove_instruction instr_value
          | _ -> ());

      (* Add last definition to the phi nodes of the next blocks *)
      iter_next_blocks block (fun next_block ->
          block_iter_phis next_block (fun phi_val phi ->
              match Context.get_phi_promoted_item ~cx phi_val (cast_to_instruction phi_val) with
              | Some promoted_item ->
                let stack = Context.get_promoted_item_def_stack ~cx promoted_item in
                if not (Stack.is_empty stack) then
                  let top_value = Stack.top stack in
                  phi_add_arg ~phi_val ~phi ~block ~value:top_value
              | _ -> ()));

      (* Visit all dominated nodes in dominator tree *)
      let children = Dominator_tree.get_children ~dt block in
      BlockSet.iter (fun child -> search child) children;

      (* Pop definitions in each stack once all children in dominator tree have been visited *)
      iter_instructions block (fun instr_value instr ->
          (match Context.get_phi_promoted_item ~cx instr_value instr with
          | Some promoted_item ->
            let stack = Context.get_promoted_item_def_stack ~cx promoted_item in
            ignore (Stack.pop stack)
          | _ -> ());

          match Context.get_store_promoted_item ~cx instr_value instr with
          | Some (promoted_item, _) ->
            let stack = Context.get_promoted_item_def_stack ~cx promoted_item in
            ignore (Stack.pop stack);
            remove_instruction instr_value
          | _ -> ())
    in

    search func.start_block
end

module SSAContext = struct
  type t = {
    mutable stack_alloc_instrs: VSet.t;
    mutable phi_to_stack_alloc_instr: Value.t VMap.t;
    mutable stacks: Value.t Stack.t VMap.t;
  }

  type promotable_item = Value.t

  let mk () =
    { stack_alloc_instrs = VSet.empty; phi_to_stack_alloc_instr = VMap.empty; stacks = VMap.empty }

  let check_promotable_instr ~cx instr_value instr =
    match instr.Instruction.instr with
    | StackAlloc _ ->
      cx.stack_alloc_instrs <- VSet.add instr_value cx.stack_alloc_instrs;
      cx.stacks <- VMap.add instr_value (Stack.create ()) cx.stacks;
      true
    | _ -> false

  let iter_promotable_items ~cx f = VSet.iter f cx.stack_alloc_instrs

  (* Iterate over the blocks that contain definitions (aka stores) for a given promoted memory value *)
  let iter_promotable_item_def_blocks ~cx:_ stack_alloc_instr f =
    value_iter_uses ~value:stack_alloc_instr (fun use ->
        match use.user.value with
        | Instr { instr = Store (ptr_use, _); block = def_block; _ }
          when ptr_use.value == stack_alloc_instr ->
          f def_block
        | _ -> ())

  let create_phi_node ~cx stack_alloc_instr =
    let type_ = cast_to_pointer_type (type_of_value stack_alloc_instr) in
    let phi = mk_blockless_phi ~type_ ~args:BlockMap.empty in
    cx.phi_to_stack_alloc_instr <- VMap.add phi stack_alloc_instr cx.phi_to_stack_alloc_instr;
    phi

  let get_phi_promoted_item ~cx instr_value instr =
    match instr.Instruction.instr with
    | Phi _ -> VMap.find_opt instr_value cx.phi_to_stack_alloc_instr
    | _ -> None

  let get_store_promoted_item ~cx _ instr =
    match instr.Instruction.instr with
    | Store (ptr_use, arg_use) when VSet.mem ptr_use.value cx.stack_alloc_instrs ->
      Some (ptr_use.value, arg_use)
    | _ -> None

  let get_load_promoted_item ~cx _ instr =
    match instr.Instruction.instr with
    | Load ptr_use when VSet.mem ptr_use.value cx.stack_alloc_instrs -> Some ptr_use.value
    | _ -> None

  let get_promoted_item_def_stack ~cx stack_alloc_instr = VMap.find stack_alloc_instr cx.stacks

  let debug_string_of_promotable_item stack_alloc_instr = string_of_int stack_alloc_instr.Value.id
end

module _ : SSA_TRANFORMER_CONTEXT = SSAContext

module SSA = SSATransformer (SSAContext)

let run ~program =
  program_iter_funcs program (fun func ->
      let cx = SSAContext.mk () in
      SSA.run ~cx ~func)
