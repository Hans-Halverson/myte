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
    mutable live_out: Operand.t list IMap.t;
    (* Every register is in exactly one of these sets *)
    mutable precolored_regs: OperandSet.t;
    mutable initial_vregs: OperandSet.t;
    (* Low degree, non move related vregs *)
    mutable simplify_worklist: OperandSet.t;
    mutable freeze_worklist: OperandSet.t;
    mutable spill_worklist: OperandSet.t;
    mutable spilled_vregs: OperandSet.t;
    mutable coalesced_vregs: OperandSet.t;
    mutable colored_vregs: OperandSet.t;
    (* Stack of vregs that are ready for an attempt to have colors assigned to them *)
    mutable select_stack: Operand.t list;
    (* Every move is in exactly one of these sets *)
    mutable coalesced_moves: ISet.t;
    mutable constrained_moves: ISet.t;
    mutable frozen_moves: ISet.t;
    (* Candidates for coalescing *)
    mutable worklist_moves: ISet.t;
    mutable active_moves: ISet.t;
    (* Adjacency list representation of interference graph. Maps from register to a set of all
       registers that interfere with it. *)
    mutable interference_graph: OOMMap.t;
    (* Degree of each register in interference graph *)
    mutable interference_degree: int OperandMap.t;
    (* Map from virtual register to instruction ids of all moves it is a part of *)
    mutable move_list: OIMMap.t;
    (* Map from virtual register to register it is aliased to *)
    mutable aliases: Operand.t OperandMap.t;
    (* Total number of uses and defs for each register *)
    mutable reg_num_use_defs: int OperandMap.t;
  }

  let mk ~(gcx : Gcx.t) ~func =
    {
      func;
      gcx;
      live_out = IMap.empty;
      precolored_regs = OperandSet.empty;
      initial_vregs = OperandSet.empty;
      simplify_worklist = OperandSet.empty;
      freeze_worklist = OperandSet.empty;
      spill_worklist = OperandSet.empty;
      spilled_vregs = OperandSet.empty;
      coalesced_vregs = OperandSet.empty;
      colored_vregs = OperandSet.empty;
      select_stack = [];
      coalesced_moves = ISet.empty;
      constrained_moves = ISet.empty;
      frozen_moves = ISet.empty;
      worklist_moves = ISet.empty;
      active_moves = ISet.empty;
      interference_graph = OOMMap.empty;
      interference_degree = OperandMap.empty;
      move_list = OIMMap.empty;
      aliases = OperandMap.empty;
      reg_num_use_defs = OperandMap.empty;
    }

  let liveness_analysis ~(ra : t) =
    let (_, live_out) = X86_64_liveness_analysis.analyze_regs ra.func.blocks ra.gcx.color_to_op in
    ra.live_out <- live_out

  class use_def_finder color_to_op =
    object
      inherit X86_64_liveness_analysis.use_def_finder color_to_op

      val mutable reg_uses = OperandSet.empty

      val mutable reg_defs = OperandSet.empty

      method reg_uses = reg_uses

      method reg_defs = reg_defs

      method reset =
        reg_uses <- OperandSet.empty;
        reg_defs <- OperandSet.empty

      method! add_register_use ~block:_ reg = reg_uses <- OperandSet.add reg reg_uses

      method! add_register_def ~block:_ reg = reg_defs <- OperandSet.add reg reg_defs
    end

  (* Add an interference edge between two virtual registers, also updating degree *)
  let add_interference_edge ~(ra : t) reg1 reg2 =
    let inc_degree reg =
      ra.interference_degree <-
        (match OperandMap.find_opt reg ra.interference_degree with
        | None -> OperandMap.add reg 1 ra.interference_degree
        | Some degree -> OperandMap.add reg (degree + 1) ra.interference_degree)
    in
    if (not (OOMMap.contains reg1 reg2 ra.interference_graph)) && reg1 != reg2 then (
      if not (OperandSet.mem reg1 ra.precolored_regs) then (
        ra.interference_graph <- OOMMap.add reg1 reg2 ra.interference_graph;
        inc_degree reg1
      );
      if not (OperandSet.mem reg2 ra.precolored_regs) then (
        ra.interference_graph <- OOMMap.add reg2 reg1 ra.interference_graph;
        inc_degree reg2
      )
    )

  let build_interference_graph ~(ra : t) =
    let use_def_finder = new use_def_finder ra.gcx.color_to_op in
    let find_use_defs block instr =
      use_def_finder#reset;
      use_def_finder#visit_instruction ~block instr;
      (use_def_finder#reg_uses, use_def_finder#reg_defs)
    in
    List.iter
      (fun block ->
        let open Block in
        let live = ref (IMap.find block.id ra.live_out |> OperandSet.of_list) in
        List.iter
          (fun ((instr_id, instr) as instr_with_id) ->
            begin
              match instr with
              | Instruction.MovMM (_, src_op, dest_op)
                when Operand.is_reg_value src_op && Operand.is_reg_value dest_op ->
                live := OperandSet.remove src_op !live;
                ra.move_list <- OIMMap.add src_op instr_id ra.move_list;
                ra.move_list <- OIMMap.add dest_op instr_id ra.move_list;
                ra.worklist_moves <- ISet.add instr_id ra.worklist_moves
              | _ -> ()
            end;
            let (reg_uses, reg_defs) = find_use_defs block instr_with_id in
            live := OperandSet.union reg_defs !live;
            OperandSet.iter
              (fun reg_def ->
                OperandSet.iter (fun live_reg -> add_interference_edge ~ra live_reg reg_def) !live)
              reg_defs;
            live := OperandSet.union reg_uses (OperandSet.diff !live reg_defs))
          (List.rev block.Block.instructions))
      ra.func.blocks

  let adjacent ~(ra : t) reg =
    let adjacent_regs = OOMMap.find_all reg ra.interference_graph in
    let to_ignore = OperandSet.union (OperandSet.of_list ra.select_stack) ra.coalesced_vregs in
    OperandSet.diff adjacent_regs to_ignore

  let node_moves ~(ra : t) reg =
    ISet.inter (OIMMap.find_all reg ra.move_list) (ISet.union ra.active_moves ra.worklist_moves)

  let move_related ~ra reg = not (ISet.is_empty (node_moves ~ra reg))

  let degree ~(ra : t) reg =
    OperandMap.find_opt reg ra.interference_degree |> Option.value ~default:0

  let make_worklist ~(ra : t) =
    OperandSet.iter
      (fun vreg ->
        ra.initial_vregs <- OperandSet.remove vreg ra.initial_vregs;
        if degree ~ra vreg >= num_allocatable_registers then
          ra.spill_worklist <- OperandSet.add vreg ra.spill_worklist
        else if move_related ~ra vreg then
          ra.freeze_worklist <- OperandSet.add vreg ra.freeze_worklist
        else
          ra.simplify_worklist <- OperandSet.add vreg ra.simplify_worklist)
      ra.initial_vregs

  let enable_moves ~(ra : t) regs =
    OperandSet.iter
      (fun reg ->
        let node_moves = node_moves ~ra reg in
        ISet.iter
          (fun move_instr_id ->
            if ISet.mem move_instr_id ra.active_moves then (
              ra.active_moves <- ISet.remove move_instr_id ra.active_moves;
              ra.worklist_moves <- ISet.add move_instr_id ra.worklist_moves
            ))
          node_moves)
      regs

  let decrement_degree ~(ra : t) reg =
    let degree = degree ~ra reg in
    ra.interference_degree <- OperandMap.add reg (max 0 (degree - 1)) ra.interference_degree;
    if degree = num_allocatable_registers then (
      enable_moves ~ra (OperandSet.add reg (adjacent ~ra reg));
      ra.spill_worklist <- OperandSet.remove reg ra.spill_worklist;
      if move_related ~ra reg then
        ra.freeze_worklist <- OperandSet.add reg ra.freeze_worklist
      else
        ra.simplify_worklist <- OperandSet.add reg ra.simplify_worklist
    )

  (* Choose a vreg from the worklist of low degree, non move related vregs. Remove this vreg from
     the graph, adding it to the select stack, and decrement the degree of all its neighbors. *)
  let simplify ~(ra : t) =
    let vreg = OperandSet.choose ra.simplify_worklist in
    ra.simplify_worklist <- OperandSet.remove vreg ra.simplify_worklist;
    ra.select_stack <- vreg :: ra.select_stack;
    let adjacent_regs = adjacent ~ra vreg in
    OperandSet.iter (fun adjacent_reg -> decrement_degree ~ra adjacent_reg) adjacent_regs

  let add_to_simplify_work_list ~(ra : t) vreg =
    if
      (not (OperandSet.mem vreg ra.precolored_regs))
      && (not (move_related ~ra vreg))
      && degree ~ra vreg < num_allocatable_registers
    then (
      ra.freeze_worklist <- OperandSet.remove vreg ra.freeze_worklist;
      ra.simplify_worklist <- OperandSet.add vreg ra.simplify_worklist
    )

  (* A virtual register can only be coalesced with a precolored register if the coalescing would
     not increase the degree of any adjacent regs to K or greater. *)
  let can_coalesce_with_precolored ~(ra : t) precolored reg =
    OperandSet.for_all
      (fun adjacent_reg ->
        (* Degree stays the same since reg gains and loses a neighbor *)
        degree ~ra adjacent_reg < num_allocatable_registers
        (* All precolored registers interfere so degree does not change *)
        || OperandSet.mem adjacent_reg ra.precolored_regs
        (* Already interferes so degree does not change *)
        || OOMMap.contains adjacent_reg precolored ra.interference_graph)
      (adjacent ~ra reg)

  (* Briggs conservative coalescing heuristic:
     If there are fewer than K neighbor regs of significant degree, then coalescing cannot make
     the graph non-K colorable. *)
  let can_conservative_coalesce ~(ra : t) reg1 reg2 =
    let neighbor_regs = OperandSet.union (adjacent ~ra reg1) (adjacent ~ra reg2) in
    let k = ref 0 in
    OperandSet.iter
      (fun reg -> if degree ~ra reg >= num_allocatable_registers then k := !k + 1)
      neighbor_regs;
    !k < num_allocatable_registers

  let rec get_operand_alias ~ra op =
    match OperandMap.find_opt op ra.aliases with
    | Some alias -> get_operand_alias ~ra alias
    | None -> op

  let is_reg_value ~ra op = Operand.is_reg_value (get_operand_alias ~ra op)

  (* Combine two registers, making them alias to each other, combining their moves and
     interference edges. *)
  let combine_regs ~(ra : t) rep_reg aliased_vreg =
    if OperandSet.mem aliased_vreg ra.freeze_worklist then
      ra.freeze_worklist <- OperandSet.remove aliased_vreg ra.freeze_worklist
    else
      ra.spill_worklist <- OperandSet.remove aliased_vreg ra.spill_worklist;
    ra.coalesced_vregs <- OperandSet.add aliased_vreg ra.coalesced_vregs;
    ra.aliases <- OperandMap.add aliased_vreg rep_reg ra.aliases;
    ra.move_list <-
      OperandMap.add
        rep_reg
        (ISet.union
           (OIMMap.find_all rep_reg ra.move_list)
           (OIMMap.find_all aliased_vreg ra.move_list))
        ra.move_list;
    OperandSet.iter
      (fun adjacent_reg ->
        add_interference_edge ~ra adjacent_reg rep_reg;
        decrement_degree ~ra adjacent_reg)
      (adjacent ~ra aliased_vreg);
    if degree ~ra rep_reg >= num_allocatable_registers && OperandSet.mem rep_reg ra.freeze_worklist
    then (
      ra.freeze_worklist <- OperandSet.remove rep_reg ra.freeze_worklist;
      ra.spill_worklist <- OperandSet.add rep_reg ra.spill_worklist
    )

  (* Return the source and destination registers for a particular instruction (MovRR (source, dest)) *)
  let source_dest_regs_of_move ~(ra : t) move_instr_id =
    let move_instruction = Gcx.get_instruction ~gcx:ra.gcx move_instr_id in
    match move_instruction with
    | Instruction.MovMM (_, source_op, dest_op)
      when is_reg_value ~ra source_op && is_reg_value ~ra dest_op ->
      (source_op, dest_op)
    | _ -> failwith "Expected id of virtual register to virtual register move instruction"

  (* Choose a move from the worklist and try to combine its virtual registers if doing so would
     not make the graph uncolorable *)
  let coalesce ~(ra : t) =
    (* Choose an abitrary move instruction *)
    let move_instr_id = ISet.choose ra.worklist_moves in
    ra.worklist_moves <- ISet.remove move_instr_id ra.worklist_moves;
    let (source_reg, dest_reg) = source_dest_regs_of_move ~ra move_instr_id in
    let source_reg = get_operand_alias ~ra source_reg in
    let dest_reg = get_operand_alias ~ra dest_reg in
    let (reg1, reg2) =
      if OperandSet.mem dest_reg ra.precolored_regs then
        (dest_reg, source_reg)
      else
        (source_reg, dest_reg)
    in
    if reg1 == reg2 then (
      ra.coalesced_moves <- ISet.add move_instr_id ra.coalesced_moves;
      add_to_simplify_work_list ~ra reg1
    ) else if
        OperandSet.mem reg2 ra.precolored_regs || OOMMap.contains reg2 reg1 ra.interference_graph
      then (
      ra.constrained_moves <- ISet.add move_instr_id ra.constrained_moves;
      add_to_simplify_work_list ~ra reg1;
      add_to_simplify_work_list ~ra reg2
    ) else if
        (OperandSet.mem reg1 ra.precolored_regs && can_coalesce_with_precolored ~ra reg1 reg2)
        || (not (OperandSet.mem reg1 ra.precolored_regs))
           && can_conservative_coalesce ~ra reg1 reg2
      then (
      ra.coalesced_moves <- ISet.add move_instr_id ra.coalesced_moves;
      combine_regs ~ra reg1 reg2;
      add_to_simplify_work_list ~ra reg1
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
        let (source_reg, dest_reg) = source_dest_regs_of_move ~ra move_instr_id in
        let (source_reg, dest_reg) =
          (get_operand_alias ~ra source_reg, get_operand_alias ~ra dest_reg)
        in
        let maybe_unfreeze vreg =
          if ISet.is_empty (node_moves ~ra vreg) && degree ~ra vreg < num_allocatable_registers then (
            ra.freeze_worklist <- OperandSet.remove vreg ra.freeze_worklist;
            ra.simplify_worklist <- OperandSet.add vreg ra.simplify_worklist
          )
        in
        if source_reg = vreg then
          maybe_unfreeze dest_reg
        else if dest_reg = vreg then
          maybe_unfreeze source_reg
        else
          failwith "Moves in move list must be indexed by register in that move")
      vreg_moves

  (* Choose a vreg from the freeze worklist and freeze all the moves associated with it. Vreg can now
     be simplified. *)
  let freeze ~(ra : t) =
    let vreg = OperandSet.choose ra.freeze_worklist in
    ra.freeze_worklist <- OperandSet.remove vreg ra.freeze_worklist;
    ra.simplify_worklist <- OperandSet.add vreg ra.simplify_worklist;
    freeze_moves ~ra vreg

  (* Choose a vreg from the spill worklist and freeze all the moves associated with it. Vreg can now
     be simplified.*)
  let select_spill ~(ra : t) =
    (* Simple spill heuristic - minimize cost C where C = (#uses + #defs) / degree *)
    let heuristic_chosen_vreg =
      OperandSet.fold
        (fun vreg chosen_vreg_opt ->
          let num_use_defs = Float.of_int (OperandMap.find vreg ra.reg_num_use_defs) in
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
      | None -> OperandSet.choose ra.spill_worklist
    in
    ra.spill_worklist <- OperandSet.remove potential_spill_vreg ra.spill_worklist;
    ra.simplify_worklist <- OperandSet.add potential_spill_vreg ra.simplify_worklist;
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
      let interfering_vregs = OOMMap.find_all vreg ra.interference_graph in

      (* Create a set of all registers and remove colors of all neighbors in interference graph *)
      let ok_registers = ref general_purpose_registers in
      OperandSet.iter
        (fun interfering_vreg ->
          let alias = get_operand_alias ~ra interfering_vreg in
          if OperandSet.mem alias ra.colored_vregs || OperandSet.mem alias ra.precolored_regs then
            match alias.value with
            | PhysicalRegister reg -> ok_registers := RegSet.remove reg !ok_registers
            | _ -> ())
        interfering_vregs;

      (* Calculate priorities for each color. Simple heuristic is choose most common color among
         colored registers that are move related to this vreg.*)
      let register_priorities = ref RegMap.empty in
      let vreg_moves = OIMMap.find_all vreg ra.move_list in
      ISet.iter
        (fun move_id ->
          let (reg1, reg2) = source_dest_regs_of_move ~ra move_id in
          let reg1 = get_operand_alias ~ra reg1 in
          let reg2 = get_operand_alias ~ra reg2 in
          let move_related_reg =
            if vreg == reg1 then
              reg2
            else
              reg1
          in
          if
            OperandSet.mem move_related_reg ra.colored_vregs
            || OperandSet.mem move_related_reg ra.precolored_regs
          then
            match move_related_reg.value with
            | PhysicalRegister reg ->
              (match RegMap.find_opt reg !register_priorities with
              | None -> register_priorities := RegMap.add reg 1 !register_priorities
              | Some prev_priority ->
                register_priorities := RegMap.add reg (prev_priority + 1) !register_priorities)
            | _ -> ())
        vreg_moves;

      (* Choose an arbitrary color from the remaining set, otherwise spill *)
      match select_color_for_vreg ~ra !ok_registers !register_priorities with
      | None -> ra.spilled_vregs <- OperandSet.add vreg ra.spilled_vregs
      | Some physical_reg ->
        ra.colored_vregs <- OperandSet.add vreg ra.colored_vregs;
        vreg.value <- PhysicalRegister physical_reg
    done;
    OperandSet.iter
      (fun vreg ->
        match (get_operand_alias ~ra vreg).value with
        | PhysicalRegister _ as alias_resolution -> vreg.value <- alias_resolution
        | _ -> failwith "Alias must be colored")
      ra.coalesced_vregs

  let rewrite_program ~(ra : t) =
    (* Resolve spilled vregs to virtual stack slots *)
    OperandSet.iter
      (fun vreg ->
        ra.func.spilled_vslots <- OperandSet.add vreg ra.func.spilled_vslots;
        vreg.value <- VirtualStackSlot)
      ra.spilled_vregs;
    (* Then rewrite program to include newly resolved memory locations *)
    let spill_writer =
      new X86_64_spill_writer.spill_writer ~gcx:ra.gcx ~get_alias:(get_operand_alias ~ra)
    in
    List.iter (fun block -> spill_writer#write_block_spills block) ra.func.blocks;
    (* Reset state of register allocator *)
    ra.spilled_vregs <- OperandSet.empty;
    ra.colored_vregs <- OperandSet.empty;
    ra.coalesced_vregs <- OperandSet.empty;
    ra.interference_graph <- OOMMap.empty;
    ra.move_list <- OperandMap.empty;
    ra.aliases <- OperandMap.empty

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
              | Instruction.MovMM (_, source_op, dest_op)
                when Operand.is_reg_value source_op && Operand.is_reg_value dest_op ->
                get_operand_alias ~ra source_op != get_operand_alias ~ra dest_op
              | _ -> true)
            block.instructions)
      ra.func.blocks

  class allocate_init_visitor =
    object (this)
      inherit X86_64_visitor.instruction_visitor as super

      val mutable reg_num_use_defs = OperandMap.empty

      val mutable precolored_regs = OperandSet.empty

      val mutable initial_vregs = OperandSet.empty

      method reg_num_use_defs = reg_num_use_defs

      method precolored_regs = precolored_regs

      method initial_vregs = initial_vregs

      method visit_reg reg =
        (match reg.Operand.value with
        | PhysicalRegister _ -> precolored_regs <- OperandSet.add reg precolored_regs
        | VirtualRegister -> initial_vregs <- OperandSet.add reg initial_vregs
        | _ -> failwith "Expected register");
        reg_num_use_defs <-
          OperandMap.add
            reg
            (match OperandMap.find_opt reg reg_num_use_defs with
            | None -> 0
            | Some prev_count -> prev_count + 1)
            reg_num_use_defs

      method! visit_read_operand ~block op =
        if Operand.is_reg_value op then
          this#visit_reg op
        else
          super#visit_read_operand ~block op

      method! visit_write_operand ~block op =
        if Operand.is_reg_value op then
          this#visit_reg op
        else
          super#visit_write_operand ~block op
    end

  let initialize ~ra =
    (* Collect all registers in program, splitting into precolored and other initial vregs *)
    let init_visitor = new allocate_init_visitor in
    List.iter
      (fun block ->
        List.iter (fun instr -> init_visitor#visit_instruction ~block instr) block.instructions)
      ra.func.blocks;
    ra.reg_num_use_defs <- init_visitor#reg_num_use_defs;
    ra.initial_vregs <- init_visitor#initial_vregs;
    ra.precolored_regs <- init_visitor#precolored_regs;
    (* Also make sure all representative precolored ops are included *)
    RegMap.iter
      (fun _ reg -> ra.precolored_regs <- OperandSet.add reg ra.precolored_regs)
      ra.gcx.color_to_op;
    ra.interference_degree <-
      OperandSet.fold
        (fun reg interference_degree -> OperandMap.add reg Int.max_int interference_degree)
        ra.precolored_regs
        OperandMap.empty

  (* Allocate physical registers (colors) to each virtual register using iterated register coalescing.
     Simply the graph afterwards to remove unnecessary instructions. *)
  let allocate_registers ~(ra : t) =
    (* Perform initial rewrite to force registers in some locations *)
    let spill_writer =
      new X86_64_spill_writer.spill_writer ~gcx:ra.gcx ~get_alias:(get_operand_alias ~ra)
    in
    List.iter (fun block -> spill_writer#write_block_spills block) ra.func.blocks;

    let rec iter () =
      initialize ~ra;
      liveness_analysis ~ra;
      build_interference_graph ~ra;
      make_worklist ~ra;
      while
        not
          ( OperandSet.is_empty ra.simplify_worklist
          && ISet.is_empty ra.worklist_moves
          && OperandSet.is_empty ra.freeze_worklist
          && OperandSet.is_empty ra.spill_worklist )
      do
        if not (OperandSet.is_empty ra.simplify_worklist) then
          simplify ~ra
        else if not (ISet.is_empty ra.worklist_moves) then
          coalesce ~ra
        else if not (OperandSet.is_empty ra.freeze_worklist) then
          freeze ~ra
        else if not (OperandSet.is_empty ra.spill_worklist) then
          select_spill ~ra
      done;
      assign_colors ~ra;

      let has_spilled_vregs = not (OperandSet.is_empty ra.spilled_vregs) in
      rewrite_program ~ra;
      if has_spilled_vregs then iter ()
    in
    iter ();
    remove_coalesced_moves ~ra
end
