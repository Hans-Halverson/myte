open Basic_collections
open X86_64_gen_context
open X86_64_instructions

(* Assign physical registers to all virtual registers and handle spills.
   Strategy is iterated register coalescing:

   Build interference graph from liveness information in program.

   Remove non-move-related low degree vregs from the graph, assigning them a register (color).
   Then attempt to coalesce moves where it is possible. When there are no longer any nodes to
   remove or coalesce, choose a vreg to freeze which marks it uncoalescable but non-move-related.
   Repeat this process until there only spills. Spill vregs and try to assign colors again,
   repeating this process until every vreg is colored.

   Based off Iterated Register Coalescing paper:
   George, L., & Appel, A. W. (1996). Iterated register coalescing.
   ACM Transactions on Programming Languages and Systems, 18(3), 300–324. *)

let general_purpose_registers =
  RegSet.of_list [A; B; C; D; SI; DI; R8; R9; R10; R11; R12; R13; R14; R15]

let num_allocatable_registers = RegSet.cardinal general_purpose_registers - 1

module RegisterAllocator = struct
  type t = {
    func: Function.t;
    gcx: Gcx.t;
    (* Map of virtual registers live at the beginning of each block *)
    mutable live_out: VReg.t list IMap.t;
    (* Every virtual register is in exactly one of these sets *)
    mutable precolored_vregs: VRegSet.t;
    mutable initial_vregs: VRegSet.t;
    (* Low degree, non move related vregs *)
    mutable simplify_worklist: VRegSet.t;
    mutable freeze_worklist: VRegSet.t;
    mutable spill_worklist: VRegSet.t;
    mutable spilled_vregs: VRegSet.t;
    mutable coalesced_vregs: VRegSet.t;
    mutable colored_vregs: VRegSet.t;
    (* Stack of vregs that are ready for an attempt to have colors assigned to them *)
    mutable select_stack: VReg.t list;
    (* Every move is in exactly one of these sets *)
    mutable coalesced_moves: ISet.t;
    mutable constrained_moves: ISet.t;
    mutable frozen_moves: ISet.t;
    (* Candidates for coalescing *)
    mutable worklist_moves: ISet.t;
    mutable active_moves: ISet.t;
    (* Adjacency list representation of interference graph. Maps from virtual register to a set of
       all virtual registers that interfere with it. *)
    mutable interference_graph: VVMMap.t;
    (* Degree of each virtual register in interference graph *)
    mutable interference_degree: int VRegMap.t;
    (* Map from virtual register to instruction ids of all moves it is a part of *)
    mutable move_list: VIMMap.t;
  }

  let mk ~(gcx : Gcx.t) ~func =
    (* Initialize representative precolored vregs *)
    let (precolored_vregs, interference_degree) =
      RegMap.fold
        (fun _ vreg (precolored_vregs, interference_degree) ->
          (VRegSet.add vreg precolored_vregs, VRegMap.add vreg Int.max_int interference_degree))
        gcx.color_to_vreg
        (VRegSet.empty, VRegMap.empty)
    in
    {
      func;
      gcx;
      live_out = IMap.empty;
      precolored_vregs;
      initial_vregs = VRegSet.empty;
      simplify_worklist = VRegSet.empty;
      freeze_worklist = VRegSet.empty;
      spill_worklist = VRegSet.empty;
      spilled_vregs = VRegSet.empty;
      coalesced_vregs = VRegSet.empty;
      colored_vregs = VRegSet.empty;
      select_stack = [];
      coalesced_moves = ISet.empty;
      constrained_moves = ISet.empty;
      frozen_moves = ISet.empty;
      worklist_moves = ISet.empty;
      active_moves = ISet.empty;
      interference_graph = VVMMap.empty;
      interference_degree;
      move_list = VIMMap.empty;
    }

  let liveness_analysis ~(ra : t) =
    let (_, live_out) =
      X86_64_liveness_analysis.analyze_vregs ra.func.blocks ra.gcx.color_to_vreg
    in
    ra.live_out <- live_out

  class use_def_finder color_to_vreg =
    object
      inherit X86_64_liveness_analysis.use_def_finder color_to_vreg

      val mutable vreg_uses = VRegSet.empty

      val mutable vreg_defs = VRegSet.empty

      method vreg_uses = vreg_uses

      method vreg_defs = vreg_defs

      method reset =
        vreg_uses <- VRegSet.empty;
        vreg_defs <- VRegSet.empty

      method! add_vreg_use ~block:_ vreg = vreg_uses <- VRegSet.add vreg vreg_uses

      method! add_vreg_def ~block:_ vreg = vreg_defs <- VRegSet.add vreg vreg_defs
    end

  (* Add an interference edge between two virtual registers, also updating degree *)
  let add_interference_edge ~(ra : t) vreg1 vreg2 =
    let inc_degree vreg =
      ra.interference_degree <-
        (match VRegMap.find_opt vreg ra.interference_degree with
        | None -> VRegMap.add vreg 1 ra.interference_degree
        | Some degree -> VRegMap.add vreg (degree + 1) ra.interference_degree)
    in
    if (not (VVMMap.contains vreg1 vreg2 ra.interference_graph)) && vreg1 != vreg2 then (
      if not (VRegSet.mem vreg1 ra.precolored_vregs) then (
        ra.interference_graph <- VVMMap.add vreg1 vreg2 ra.interference_graph;
        inc_degree vreg1
      );
      if not (VRegSet.mem vreg2 ra.precolored_vregs) then (
        ra.interference_graph <- VVMMap.add vreg2 vreg1 ra.interference_graph;
        inc_degree vreg2
      )
    )

  let build_interference_graph ~(ra : t) =
    let use_def_finder = new use_def_finder ra.gcx.color_to_vreg in
    let find_use_defs block instr =
      use_def_finder#reset;
      use_def_finder#visit_instruction ~block instr;
      (use_def_finder#vreg_uses, use_def_finder#vreg_defs)
    in
    List.iter
      (fun block ->
        let open Block in
        let live = ref (IMap.find block.id ra.live_out |> VRegSet.of_list) in
        List.iter
          (fun ((instr_id, instr) as instr_with_id) ->
            begin
              match instr with
              | Instruction.MovMM (_, src_vreg, dest_vreg)
                when VReg.is_reg_value src_vreg && VReg.is_reg_value dest_vreg ->
                live := VRegSet.remove src_vreg !live;
                ra.move_list <- VIMMap.add src_vreg instr_id ra.move_list;
                ra.move_list <- VIMMap.add dest_vreg instr_id ra.move_list;
                ra.worklist_moves <- ISet.add instr_id ra.worklist_moves
              | _ -> ()
            end;
            let (vreg_uses, vreg_defs) = find_use_defs block instr_with_id in
            live := VRegSet.union vreg_defs !live;
            VRegSet.iter
              (fun vreg_def ->
                VRegSet.iter (fun live_vreg -> add_interference_edge ~ra live_vreg vreg_def) !live)
              vreg_defs;
            live := VRegSet.union vreg_uses (VRegSet.diff !live vreg_defs))
          (List.rev block.Block.instructions))
      ra.func.blocks

  let adjacent ~(ra : t) vreg =
    let adjacent_vregs = VVMMap.find_all vreg ra.interference_graph in
    let to_ignore = VRegSet.union (VRegSet.of_list ra.select_stack) ra.coalesced_vregs in
    VRegSet.diff adjacent_vregs to_ignore

  let node_moves ~(ra : t) vreg =
    ISet.inter (VIMMap.find_all vreg ra.move_list) (ISet.union ra.active_moves ra.worklist_moves)

  let move_related ~ra vreg = not (ISet.is_empty (node_moves ~ra vreg))

  let degree ~(ra : t) vreg =
    VRegMap.find_opt vreg ra.interference_degree |> Option.value ~default:0

  let make_worklist ~(ra : t) =
    VRegSet.iter
      (fun vreg ->
        ra.initial_vregs <- VRegSet.remove vreg ra.initial_vregs;
        if degree ~ra vreg >= num_allocatable_registers then
          ra.spill_worklist <- VRegSet.add vreg ra.spill_worklist
        else if move_related ~ra vreg then
          ra.freeze_worklist <- VRegSet.add vreg ra.freeze_worklist
        else
          ra.simplify_worklist <- VRegSet.add vreg ra.simplify_worklist)
      ra.initial_vregs

  let enable_moves ~(ra : t) vregs =
    VRegSet.iter
      (fun vreg ->
        let node_moves = node_moves ~ra vreg in
        ISet.iter
          (fun move_instr_id ->
            if ISet.mem move_instr_id ra.active_moves then (
              ra.active_moves <- ISet.remove move_instr_id ra.active_moves;
              ra.worklist_moves <- ISet.add move_instr_id ra.worklist_moves
            ))
          node_moves)
      vregs

  let decrement_degree ~(ra : t) vreg =
    let degree = degree ~ra vreg in
    ra.interference_degree <- VRegMap.add vreg (max 0 (degree - 1)) ra.interference_degree;
    if degree = num_allocatable_registers then (
      enable_moves ~ra (VRegSet.add vreg (adjacent ~ra vreg));
      ra.spill_worklist <- VRegSet.remove vreg ra.spill_worklist;
      if move_related ~ra vreg then
        ra.freeze_worklist <- VRegSet.add vreg ra.freeze_worklist
      else
        ra.simplify_worklist <- VRegSet.add vreg ra.simplify_worklist
    )

  (* Choose a vreg from the worklist of low degree, non move related vregs. Remove this vreg from
     the graph, adding it to the select stack, and decrement the degree of all its neighbors. *)
  let simplify ~(ra : t) =
    let vreg = VRegSet.choose ra.simplify_worklist in
    ra.simplify_worklist <- VRegSet.remove vreg ra.simplify_worklist;
    ra.select_stack <- vreg :: ra.select_stack;
    let adjacent_vregs = adjacent ~ra vreg in
    VRegSet.iter (fun adjacent_vreg -> decrement_degree ~ra adjacent_vreg) adjacent_vregs

  let add_to_simplify_work_list ~(ra : t) vreg =
    if
      (not (VRegSet.mem vreg ra.precolored_vregs))
      && (not (move_related ~ra vreg))
      && degree ~ra vreg < num_allocatable_registers
    then (
      ra.freeze_worklist <- VRegSet.remove vreg ra.freeze_worklist;
      ra.simplify_worklist <- VRegSet.add vreg ra.simplify_worklist
    )

  (* A virtual register can only be coalesced with a precolored register if the coalescing would
     not increase the degree of any adjacent vregs to K or greater. *)
  let can_coalesce_with_precolored ~(ra : t) precolored vreg =
    VRegSet.for_all
      (fun adjacent_vreg ->
        (* Degree stays the same since vreg gains and loses a neighbor *)
        degree ~ra adjacent_vreg < num_allocatable_registers
        (* All precolored registers interfere so degree does not change *)
        || VRegSet.mem adjacent_vreg ra.precolored_vregs
        (* Already interferes so degree does not change *)
        || VVMMap.contains adjacent_vreg precolored ra.interference_graph)
      (adjacent ~ra vreg)

  (* Briggs conservative coalescing heuristic:
     If there are fewer than K neighbor vregs of significant degree, then coalescing cannot make
     the graph non-K colorable. *)
  let can_conservative_coalesce ~(ra : t) vreg1 vreg2 =
    let neighbor_vregs = VRegSet.union (adjacent ~ra vreg1) (adjacent ~ra vreg2) in
    let k = ref 0 in
    VRegSet.iter
      (fun vreg -> if degree ~ra vreg >= num_allocatable_registers then k := !k + 1)
      neighbor_vregs;
    !k < num_allocatable_registers

  let rec get_vreg_alias ~ra vreg =
    match vreg.VReg.resolution with
    | Alias alias when VRegSet.mem vreg ra.coalesced_vregs -> get_vreg_alias ~ra alias
    | _ -> vreg

  let get_vreg_resolution ~ra vreg = (get_vreg_alias ~ra vreg).resolution

  (* Combine two vregs, making them alias to each other, combining their moves and interference edges.
   *)
  let combine_vregs ~(ra : t) alias_vreg vreg =
    if VRegSet.mem vreg ra.freeze_worklist then
      ra.freeze_worklist <- VRegSet.remove vreg ra.freeze_worklist
    else
      ra.spill_worklist <- VRegSet.remove vreg ra.spill_worklist;
    ra.coalesced_vregs <- VRegSet.add vreg ra.coalesced_vregs;
    (get_vreg_alias ~ra vreg).resolution <- Alias alias_vreg;
    ra.move_list <-
      VRegMap.add
        alias_vreg
        (ISet.union (VIMMap.find_all alias_vreg ra.move_list) (VIMMap.find_all vreg ra.move_list))
        ra.move_list;
    VRegSet.iter
      (fun adjacent_vreg ->
        add_interference_edge ~ra adjacent_vreg alias_vreg;
        decrement_degree ~ra adjacent_vreg)
      (adjacent ~ra vreg);
    if
      degree ~ra alias_vreg >= num_allocatable_registers
      && VRegSet.mem alias_vreg ra.freeze_worklist
    then (
      ra.freeze_worklist <- VRegSet.remove alias_vreg ra.freeze_worklist;
      ra.spill_worklist <- VRegSet.add alias_vreg ra.spill_worklist
    )

  (* Return the source and destination vregs for a particular instruction (MovRR (source, dest)) *)
  let source_dest_vregs_of_move ~(ra : t) move_instr_id =
    let move_instruction = Gcx.get_instruction ~gcx:ra.gcx move_instr_id in
    match move_instruction with
    | Instruction.MovMM (_, source_vreg, dest_vreg)
      when VReg.is_reg_value source_vreg && VReg.is_reg_value dest_vreg ->
      (source_vreg, dest_vreg)
    | _ -> failwith "Expected id of virtual register to virtual register move instruction"

  (* Choose a move from the worklist and try to combine its virtual registers if doing so would
     not make the graph uncolorable *)
  let coalesce ~(ra : t) =
    (* Choose an abitrary move instruction *)
    let move_instr_id = ISet.choose ra.worklist_moves in
    ra.worklist_moves <- ISet.remove move_instr_id ra.worklist_moves;
    let (source_vreg, dest_vreg) = source_dest_vregs_of_move ~ra move_instr_id in
    let source_vreg = get_vreg_alias ~ra source_vreg in
    let dest_vreg = get_vreg_alias ~ra dest_vreg in
    let (vreg1, vreg2) =
      if VRegSet.mem dest_vreg ra.precolored_vregs then
        (dest_vreg, source_vreg)
      else
        (source_vreg, dest_vreg)
    in
    if vreg1 == vreg2 then (
      ra.coalesced_moves <- ISet.add move_instr_id ra.coalesced_moves;
      add_to_simplify_work_list ~ra vreg1
    ) else if
        VRegSet.mem vreg2 ra.precolored_vregs || VVMMap.contains vreg2 vreg1 ra.interference_graph
      then (
      ra.constrained_moves <- ISet.add move_instr_id ra.constrained_moves;
      add_to_simplify_work_list ~ra vreg1;
      add_to_simplify_work_list ~ra vreg2
    ) else if
        (VRegSet.mem vreg1 ra.precolored_vregs && can_coalesce_with_precolored ~ra vreg1 vreg2)
        || (not (VRegSet.mem vreg1 ra.precolored_vregs))
           && can_conservative_coalesce ~ra vreg1 vreg2
      then (
      ra.coalesced_moves <- ISet.add move_instr_id ra.coalesced_moves;
      combine_vregs ~ra vreg1 vreg2;
      add_to_simplify_work_list ~ra vreg1
    ) else
      ra.active_moves <- ISet.add move_instr_id ra.active_moves

  (* Freeze all the moves associated with a vreg. This makes them no longer eligible for coalescing. *)
  let freeze_moves ~(ra : t) vreg =
    let vreg_moves = node_moves ~ra vreg in
    ISet.iter
      (fun move_instr_id ->
        if ISet.mem move_instr_id ra.active_moves then
          ra.active_moves <- ISet.remove move_instr_id ra.active_moves
        else
          ra.worklist_moves <- ISet.remove move_instr_id ra.worklist_moves;
        ra.frozen_moves <- ISet.add move_instr_id ra.frozen_moves;
        let (source_vreg, dest_vreg) = source_dest_vregs_of_move ~ra move_instr_id in
        let (source_vreg, dest_vreg) =
          (get_vreg_alias ~ra source_vreg, get_vreg_alias ~ra dest_vreg)
        in
        let maybe_unfreeze vreg =
          if ISet.is_empty (node_moves ~ra vreg) && degree ~ra vreg < num_allocatable_registers then (
            ra.freeze_worklist <- VRegSet.remove vreg ra.freeze_worklist;
            ra.simplify_worklist <- VRegSet.add vreg ra.simplify_worklist
          )
        in
        if source_vreg = vreg then
          maybe_unfreeze dest_vreg
        else if dest_vreg = vreg then
          maybe_unfreeze source_vreg
        else
          failwith "Moves in move list must be indexed by register in that move")
      vreg_moves

  (* Choose a vreg from the freeze worklist and freeze all the moves associated with it. Vreg can now
     be simplified. *)
  let freeze ~(ra : t) =
    let vreg = VRegSet.choose ra.freeze_worklist in
    ra.freeze_worklist <- VRegSet.remove vreg ra.freeze_worklist;
    ra.simplify_worklist <- VRegSet.add vreg ra.simplify_worklist;
    freeze_moves ~ra vreg

  (* Choose a vreg from the spill worklist and freeze all the moves associated with it. Vreg can now
     be simplified.*)
  let select_spill ~(ra : t) vreg_num_use_defs =
    (* Simple spill heuristic - minimize cost C where C = (#uses + #defs) / degree *)
    let heuristic_chosen_vreg =
      VRegSet.fold
        (fun vreg chosen_vreg_opt ->
          let num_use_defs = Float.of_int (VRegMap.find vreg vreg_num_use_defs) in
          let degree = Float.of_int (degree ~ra vreg) in
          let cost = num_use_defs /. degree in
          match chosen_vreg_opt with
          | None -> Some (vreg, cost)
          | Some (_, chosen_cost) ->
            if cost < chosen_cost then
              Some (vreg, cost)
            else
              chosen_vreg_opt)
        ra.spill_worklist
        None
    in
    let potential_spill_vreg =
      match heuristic_chosen_vreg with
      | Some (vreg, _) -> vreg
      | None -> VRegSet.choose ra.spill_worklist
    in
    ra.spill_worklist <- VRegSet.remove potential_spill_vreg ra.spill_worklist;
    ra.simplify_worklist <- VRegSet.add potential_spill_vreg ra.simplify_worklist;
    freeze_moves ~ra potential_spill_vreg

  let find_highest_priority_reg possible_regs reg_priorities =
    let opt_reg =
      RegSet.fold
        (fun reg acc ->
          match RegMap.find_opt reg reg_priorities with
          | None -> acc
          | Some priority ->
            (match acc with
            | None -> Some (reg, priority)
            | Some (_, max_pri) when max_pri < priority -> Some (reg, priority)
            | Some _ -> acc))
        possible_regs
        None
    in
    match opt_reg with
    | None -> RegSet.min_elt_opt possible_regs
    | Some (reg, _) -> Some reg

  (* Select a color for a vreg from a set of possible colors to choose from. Only spill a new callee
     saved register if we need to. *)
  let select_color_for_vreg ~(ra : t) possible_regs reg_priorities =
    (* Unresolved vregs must always be part of a function *)
    let unused_callee_saved_regs =
      RegSet.diff callee_saved_registers ra.func.spilled_callee_saved_regs
    in
    let possible_not_unused_callee_saved_regs =
      RegSet.diff possible_regs unused_callee_saved_regs
    in
    match find_highest_priority_reg possible_not_unused_callee_saved_regs reg_priorities with
    | Some reg -> Some reg
    | None ->
      let possible_unused_callee_saved_regs = RegSet.inter possible_regs unused_callee_saved_regs in
      (match find_highest_priority_reg possible_unused_callee_saved_regs reg_priorities with
      | None -> None
      | Some reg ->
        ra.func.spilled_callee_saved_regs <- RegSet.add reg ra.func.spilled_callee_saved_regs;
        Some reg)

  (* Pop nodes off the select stack and greedily assign colors to them. If no color can be assigned,
     add vreg to spill worklist. *)
  let assign_colors ~(ra : t) =
    while ra.select_stack <> [] do
      let vreg = List.hd ra.select_stack in
      ra.select_stack <- List.tl ra.select_stack;
      let interfering_vregs = VVMMap.find_all vreg ra.interference_graph in

      (* Create a set of all registers and remove colors of all neighbors in interference graph *)
      let ok_registers = ref general_purpose_registers in
      VRegSet.iter
        (fun interfering_vreg ->
          let alias = get_vreg_alias ~ra interfering_vreg in
          if VRegSet.mem alias ra.colored_vregs || VRegSet.mem alias ra.precolored_vregs then
            match alias.resolution with
            | PhysicalRegister reg -> ok_registers := RegSet.remove reg !ok_registers
            | _ -> ())
        interfering_vregs;

      (* Calculate priorities for each color. Simple heuristic is choose most common color among
         colored vregs that are move related to this vreg.*)
      let register_priorities = ref RegMap.empty in
      let vreg_moves = VIMMap.find_all vreg ra.move_list in
      ISet.iter
        (fun move_id ->
          let (vreg1, vreg2) = source_dest_vregs_of_move ~ra move_id in
          let vreg1 = get_vreg_alias ~ra vreg1 in
          let vreg2 = get_vreg_alias ~ra vreg2 in
          let move_related_vreg =
            if vreg == vreg1 then
              vreg2
            else
              vreg1
          in
          if
            VRegSet.mem move_related_vreg ra.colored_vregs
            || VRegSet.mem move_related_vreg ra.precolored_vregs
          then
            match move_related_vreg.resolution with
            | PhysicalRegister reg ->
              (match RegMap.find_opt reg !register_priorities with
              | None -> register_priorities := RegMap.add reg 1 !register_priorities
              | Some prev_priority ->
                register_priorities := RegMap.add reg (prev_priority + 1) !register_priorities)
            | _ -> ())
        vreg_moves;

      (* Choose an arbitrary color from the remaining set, otherwise spill *)
      match select_color_for_vreg ~ra !ok_registers !register_priorities with
      | None -> ra.spilled_vregs <- VRegSet.add vreg ra.spilled_vregs
      | Some physical_reg ->
        ra.colored_vregs <- VRegSet.add vreg ra.colored_vregs;
        vreg.resolution <- PhysicalRegister physical_reg
    done;
    VRegSet.iter
      (fun vreg ->
        match get_vreg_resolution ~ra vreg with
        | PhysicalRegister _ as alias_resolution -> vreg.resolution <- alias_resolution
        | _ -> failwith "Alias must be colored")
      ra.coalesced_vregs

  let rewrite_program ~(ra : t) =
    (* Resolve spilled vregs to virtual stack slots *)
    VRegSet.iter
      (fun vreg ->
        ra.func.spilled_vregs <- VRegSet.add vreg ra.func.spilled_vregs;
        vreg.resolution <- VirtualStackSlot)
      ra.spilled_vregs;
    (* Then rewrite program to include newly resolved memory locations *)
    let spill_writer = new X86_64_spill_writer.spill_writer ~gcx:ra.gcx in
    List.iter (fun block -> spill_writer#write_block_spills block) ra.func.blocks;
    (* Reset state of register allocator *)
    let new_vregs = spill_writer#new_vregs in
    ra.spilled_vregs <- VRegSet.empty;
    ra.initial_vregs <- VRegSet.union new_vregs (VRegSet.union ra.colored_vregs ra.coalesced_vregs);
    ra.colored_vregs <- VRegSet.empty;
    ra.coalesced_vregs <- VRegSet.empty;
    ra.interference_graph <- VVMMap.empty;
    ra.interference_degree <-
      VRegSet.fold
        (fun vreg acc -> VRegMap.add vreg Int.max_int acc)
        ra.precolored_vregs
        VRegMap.empty;
    ra.move_list <- VRegMap.empty

  (* Remove all moves where both the source and destination alias to the same register, as they are
     unnecessary. *)
  let remove_coalesced_moves ~(ra : t) =
    let open Block in
    List.iter
      (fun block ->
        block.instructions <-
          List.filter
            (fun (_, instr) ->
              match instr with
              | Instruction.MovMM (_, source_vreg, dest_vreg)
                when VReg.is_reg_value source_vreg && VReg.is_reg_value dest_vreg ->
                get_vreg_alias ~ra source_vreg != get_vreg_alias ~ra dest_vreg
              | _ -> true)
            block.instructions)
      ra.func.blocks

  class allocate_init_visitor =
    object (this)
      inherit X86_64_visitor.instruction_visitor as super

      val mutable vreg_num_use_defs = VRegMap.empty

      method vreg_num_use_defs = vreg_num_use_defs

      method all_vregs =
        VRegMap.fold (fun vreg _ vregs -> VRegSet.add vreg vregs) vreg_num_use_defs VRegSet.empty

      method visit_vreg vreg =
        vreg_num_use_defs <-
          VRegMap.add
            vreg
            (match VRegMap.find_opt vreg vreg_num_use_defs with
            | None -> 0
            | Some prev_count -> prev_count + 1)
            vreg_num_use_defs

      method! visit_read_vreg ~block vreg =
        if VReg.is_reg_value vreg then
          this#visit_vreg vreg
        else
          super#visit_read_vreg ~block vreg

      method! visit_write_vreg ~block vreg =
        if VReg.is_reg_value vreg then
          this#visit_vreg vreg
        else
          super#visit_write_vreg ~block vreg
    end

  (* Allocate physical registers (colors) to each virtual register using iterated register coalescing.
     Simply the graph afterwards to remove unnecessary instructions. *)
  let allocate_registers ~(ra : t) =
    (* Perform initial rewrite to force registers in some locations *)
    let spill_writer = new X86_64_spill_writer.spill_writer ~gcx:ra.gcx in
    List.iter (fun block -> spill_writer#write_block_spills block) ra.func.blocks;

    (* Collect all registers in program, then remove precolored to create initial vreg list *)
    let init_visitor = new allocate_init_visitor in
    List.iter
      (fun block ->
        List.iter (fun instr -> init_visitor#visit_instruction ~block instr) block.instructions)
      ra.func.blocks;
    let vreg_num_use_defs = init_visitor#vreg_num_use_defs in
    ra.initial_vregs <- VRegSet.diff init_visitor#all_vregs ra.precolored_vregs;
    let rec iter () =
      liveness_analysis ~ra;
      build_interference_graph ~ra;
      make_worklist ~ra;
      while
        not
          ( VRegSet.is_empty ra.simplify_worklist
          && ISet.is_empty ra.worklist_moves
          && VRegSet.is_empty ra.freeze_worklist
          && VRegSet.is_empty ra.spill_worklist )
      do
        if not (VRegSet.is_empty ra.simplify_worklist) then
          simplify ~ra
        else if not (ISet.is_empty ra.worklist_moves) then
          coalesce ~ra
        else if not (VRegSet.is_empty ra.freeze_worklist) then
          freeze ~ra
        else if not (VRegSet.is_empty ra.spill_worklist) then
          select_spill ~ra vreg_num_use_defs
      done;
      assign_colors ~ra;

      if not (VRegSet.is_empty ra.spilled_vregs) then (
        rewrite_program ~ra;
        iter ()
      )
    in
    iter ();
    remove_coalesced_moves ~ra
end
