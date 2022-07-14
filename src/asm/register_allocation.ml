open Basic_collections

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

module type RA_REGISTER = sig
  include SORTED

  type class_
end

module type RA_OPERAND = sig
  include SORTED

  val is_reg_value : t -> bool
end

module type RA_BLOCK = sig
  type t

  val get_id : t -> int
end

module type REGISTER_ALLOCATOR_CONTEXT = sig
  module Register : RA_REGISTER
  module Operand : RA_OPERAND
  module Instruction : SORTED
  module Block : RA_BLOCK

  module RegSet : Set.S with type elt = Register.t
  module RegMap : Map.S with type key = Register.t
  module OperandSet : Set.S with type elt = Operand.t
  module OperandMap : Map.S with type key = Operand.t
  module InstrSet : Set.S with type elt = Instruction.t

  module OOMMap :
    MultiMap.S
      with type key = Operand.t
       and type value = Operand.t
       and module KMap = OperandMap
       and module VSet = OperandSet
  module OInstrMMap :
    MultiMap.S
      with type key = Operand.t
       and type value = Instruction.t
       and module KMap = OperandMap
       and module VSet = InstrSet

  type t

  (* Calling conventions *)

  val allocatable_registers : Register.class_ -> RegSet.t

  val callee_saved_registers : RegSet.t

  val num_allocatable_registers : Register.class_ -> int

  val get_rep_physical_registers : t -> Operand.t RegMap.t

  val get_spilled_callee_saved_registers : t -> RegSet.t

  val add_spilled_callee_saved_register : t -> Register.t -> unit

  (* Operand functions *)

  val is_precolored : Operand.t -> bool

  val get_rep_register : t -> Operand.t -> Operand.t

  val get_physical_register_opt : Operand.t -> Register.t option

  val assign_physical_register : Operand.t -> Register.t -> unit

  val get_class : Operand.t -> Register.class_

  (* Instruction functions *)

  val iter_blocks : (Block.t -> unit) -> t -> unit

  val iter_instrs_rev : (Instruction.t -> unit) -> Block.t -> unit

  val filter_instrs : (Instruction.t -> bool) -> Block.t -> unit

  val get_move_opt : Instruction.t -> (Operand.t * Operand.t) option

  (* Register allocation lifecycle *)

  val init_context : t -> int OperandMap.t * OperandSet.t

  val get_live_out_regs : t -> Operand.t list IMap.t

  val get_use_defs_for_instruction : t -> Instruction.t -> Block.t -> OperandSet.t * OperandSet.t

  val spill_virtual_register : t -> Operand.t -> unit

  val rewrite_spilled_program : t -> get_alias:(Operand.t -> Operand.t) -> unit
end

module RegisterAllocator (Cx : REGISTER_ALLOCATOR_CONTEXT) = struct
  module Register = Cx.Register
  module Operand = Cx.Operand
  module Instruction = Cx.Instruction
  module Block = Cx.Block

  module RegSet = Cx.RegSet
  module RegMap = Cx.RegMap
  module OperandSet = Cx.OperandSet
  module OperandMap = Cx.OperandMap
  module InstrSet = Cx.InstrSet

  module OOMMap = Cx.OOMMap
  module OInstrMMap = Cx.OInstrMMap

  type t = {
    cx: Cx.t;
    (* Map of virtual registers live at the beginning of each block *)
    mutable live_out: Operand.t list IMap.t;
    (* Every register is in exactly one of these sets *)
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
    mutable coalesced_moves: InstrSet.t;
    mutable constrained_moves: InstrSet.t;
    mutable frozen_moves: InstrSet.t;
    (* Candidates for coalescing *)
    mutable worklist_moves: InstrSet.t;
    mutable active_moves: InstrSet.t;
    (* Adjacency list representation of interference graph. Maps from register to a set of all
       registers that interfere with it. *)
    mutable interference_graph: OOMMap.t;
    (* Degree of each register in interference graph *)
    mutable interference_degree: int OperandMap.t;
    (* Map from virtual register to instruction ids of all moves it is a part of *)
    mutable move_list: OInstrMMap.t;
    (* Map from virtual register to register it is aliased to *)
    mutable aliases: Operand.t OperandMap.t;
    (* Total number of uses and defs for each register *)
    mutable reg_num_use_defs: int OperandMap.t;
  }

  let mk ~(cx : Cx.t) =
    {
      cx;
      live_out = IMap.empty;
      initial_vregs = OperandSet.empty;
      simplify_worklist = OperandSet.empty;
      freeze_worklist = OperandSet.empty;
      spill_worklist = OperandSet.empty;
      spilled_vregs = OperandSet.empty;
      coalesced_vregs = OperandSet.empty;
      colored_vregs = OperandSet.empty;
      select_stack = [];
      coalesced_moves = InstrSet.empty;
      constrained_moves = InstrSet.empty;
      frozen_moves = InstrSet.empty;
      worklist_moves = InstrSet.empty;
      active_moves = InstrSet.empty;
      interference_graph = OOMMap.empty;
      interference_degree = OperandMap.empty;
      move_list = OInstrMMap.empty;
      aliases = OperandMap.empty;
      reg_num_use_defs = OperandMap.empty;
    }

  let liveness_analysis ~(ra : t) =
    let live_out = Cx.get_live_out_regs ra.cx in
    ra.live_out <- live_out

  (* All operands read from the instruction objects must pass through this function before they
     can be used as registers. Resolves all physical registers to their representative operands. *)
  let get_rep_register ~(ra : t) op = Cx.get_rep_register ra.cx op

  let is_precolored op = Cx.is_precolored op

  (* Add an interference edge between two virtual registers, also updating degree *)
  let add_interference_edge ~(ra : t) reg1 reg2 =
    let inc_degree reg =
      ra.interference_degree <-
        (match OperandMap.find_opt reg ra.interference_degree with
        | None -> OperandMap.add reg 1 ra.interference_degree
        | Some degree -> OperandMap.add reg (degree + 1) ra.interference_degree)
    in
    if (not (OOMMap.contains reg1 reg2 ra.interference_graph)) && reg1 != reg2 then (
      if not (is_precolored reg1) then (
        ra.interference_graph <- OOMMap.add reg1 reg2 ra.interference_graph;
        inc_degree reg1
      );
      if not (is_precolored reg2) then (
        ra.interference_graph <- OOMMap.add reg2 reg1 ra.interference_graph;
        inc_degree reg2
      )
    )

  let build_interference_graph ~(ra : t) =
    Cx.iter_blocks
      (fun block ->
        let live = ref (IMap.find (Block.get_id block) ra.live_out |> OperandSet.of_list) in
        Cx.iter_instrs_rev
          (fun instr ->
            begin
              match Cx.get_move_opt instr with
              | Some (src_op, dest_op) ->
                let src_op = get_rep_register ~ra src_op in
                let dest_op = get_rep_register ~ra dest_op in
                live := OperandSet.remove src_op !live;
                ra.move_list <- OInstrMMap.add src_op instr ra.move_list;
                ra.move_list <- OInstrMMap.add dest_op instr ra.move_list;
                ra.worklist_moves <- InstrSet.add instr ra.worklist_moves
              | None -> ()
            end;
            let (reg_uses, reg_defs) = Cx.get_use_defs_for_instruction ra.cx instr block in
            live := OperandSet.union reg_defs !live;
            OperandSet.iter
              (fun reg_def ->
                OperandSet.iter
                  (fun live_reg ->
                    (* Registers only interfere if they have the same class *)
                    if Cx.get_class reg_def == Cx.get_class live_reg then
                      add_interference_edge ~ra live_reg reg_def)
                  !live)
              reg_defs;
            live := OperandSet.union reg_uses (OperandSet.diff !live reg_defs))
          block)
      ra.cx

  let adjacent ~(ra : t) reg =
    let adjacent_regs = OOMMap.find_all reg ra.interference_graph in
    let to_ignore = OperandSet.union (OperandSet.of_list ra.select_stack) ra.coalesced_vregs in
    OperandSet.diff adjacent_regs to_ignore

  let node_moves ~(ra : t) reg =
    InstrSet.inter
      (OInstrMMap.find_all reg ra.move_list)
      (InstrSet.union ra.active_moves ra.worklist_moves)

  let move_related ~ra reg = not (InstrSet.is_empty (node_moves ~ra reg))

  let degree ~(ra : t) reg =
    OperandMap.find_opt reg ra.interference_degree |> Option.value ~default:0

  let make_worklist ~(ra : t) =
    OperandSet.iter
      (fun vreg ->
        ra.initial_vregs <- OperandSet.remove vreg ra.initial_vregs;
        if degree ~ra vreg >= Cx.num_allocatable_registers (Cx.get_class vreg) then
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
        InstrSet.iter
          (fun move_instr ->
            if InstrSet.mem move_instr ra.active_moves then (
              ra.active_moves <- InstrSet.remove move_instr ra.active_moves;
              ra.worklist_moves <- InstrSet.add move_instr ra.worklist_moves
            ))
          node_moves)
      regs

  let decrement_degree ~(ra : t) reg =
    let degree = degree ~ra reg in
    ra.interference_degree <- OperandMap.add reg (max 0 (degree - 1)) ra.interference_degree;
    if degree = Cx.num_allocatable_registers (Cx.get_class reg) then (
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
      (not (is_precolored vreg))
      && (not (move_related ~ra vreg))
      && degree ~ra vreg < Cx.num_allocatable_registers (Cx.get_class vreg)
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
        degree ~ra adjacent_reg < Cx.num_allocatable_registers (Cx.get_class adjacent_reg)
        (* All precolored registers interfere so degree does not change *)
        || is_precolored adjacent_reg
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
      (fun reg ->
        if degree ~ra reg >= Cx.num_allocatable_registers (Cx.get_class reg) then k := !k + 1)
      neighbor_regs;
    !k < Cx.num_allocatable_registers (Cx.get_class reg1)

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
        (InstrSet.union
           (OInstrMMap.find_all rep_reg ra.move_list)
           (OInstrMMap.find_all aliased_vreg ra.move_list))
        ra.move_list;
    OperandSet.iter
      (fun adjacent_reg ->
        add_interference_edge ~ra adjacent_reg rep_reg;
        decrement_degree ~ra adjacent_reg)
      (adjacent ~ra aliased_vreg);
    if
      degree ~ra rep_reg >= Cx.num_allocatable_registers (Cx.get_class rep_reg)
      && OperandSet.mem rep_reg ra.freeze_worklist
    then (
      ra.freeze_worklist <- OperandSet.remove rep_reg ra.freeze_worklist;
      ra.spill_worklist <- OperandSet.add rep_reg ra.spill_worklist
    )

  (* Return the source and destination registers for a particular instruction (MovRR (source, dest)) *)
  let source_dest_regs_of_move ~(ra : t) move_instr =
    match Cx.get_move_opt move_instr with
    | Some (source_op, dest_op) when is_reg_value ~ra source_op && is_reg_value ~ra dest_op ->
      let source_op = get_rep_register ~ra source_op in
      let dest_op = get_rep_register ~ra dest_op in
      (source_op, dest_op)
    | _ -> failwith "Expected id of virtual register to virtual register move instruction"

  (* Choose a move from the worklist and try to combine its virtual registers if doing so would
     not make the graph uncolorable *)
  let coalesce ~(ra : t) =
    (* Choose an abitrary move instruction *)
    let move_instr = InstrSet.choose ra.worklist_moves in
    ra.worklist_moves <- InstrSet.remove move_instr ra.worklist_moves;
    let (source_reg, dest_reg) = source_dest_regs_of_move ~ra move_instr in
    let source_reg = get_operand_alias ~ra source_reg in
    let dest_reg = get_operand_alias ~ra dest_reg in
    let (reg1, reg2) =
      if is_precolored dest_reg then
        (dest_reg, source_reg)
      else
        (source_reg, dest_reg)
    in
    if reg1 == reg2 then (
      ra.coalesced_moves <- InstrSet.add move_instr ra.coalesced_moves;
      add_to_simplify_work_list ~ra reg1
    ) else if is_precolored reg2 || OOMMap.contains reg2 reg1 ra.interference_graph then (
      ra.constrained_moves <- InstrSet.add move_instr ra.constrained_moves;
      add_to_simplify_work_list ~ra reg1;
      add_to_simplify_work_list ~ra reg2
    ) else if
        (is_precolored reg1 && can_coalesce_with_precolored ~ra reg1 reg2)
        || ((not (is_precolored reg1)) && can_conservative_coalesce ~ra reg1 reg2)
      then (
      ra.coalesced_moves <- InstrSet.add move_instr ra.coalesced_moves;
      combine_regs ~ra reg1 reg2;
      add_to_simplify_work_list ~ra reg1
    ) else
      ra.active_moves <- InstrSet.add move_instr ra.active_moves

  (* Freeze all the moves associated with a vreg. This makes them no longer eligible for coalescing. *)
  let freeze_moves ~(ra : t) vreg =
    let vreg_moves = node_moves ~ra vreg in
    InstrSet.iter
      (fun move_instr ->
        if InstrSet.mem move_instr ra.active_moves then
          ra.active_moves <- InstrSet.remove move_instr ra.active_moves
        else
          ra.worklist_moves <- InstrSet.remove move_instr ra.worklist_moves;
        ra.frozen_moves <- InstrSet.add move_instr ra.frozen_moves;
        let (source_reg, dest_reg) = source_dest_regs_of_move ~ra move_instr in
        let (source_reg, dest_reg) =
          (get_operand_alias ~ra source_reg, get_operand_alias ~ra dest_reg)
        in
        let maybe_unfreeze vreg =
          if
            InstrSet.is_empty (node_moves ~ra vreg)
            && degree ~ra vreg < Cx.num_allocatable_registers (Cx.get_class vreg)
          then (
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
      RegSet.diff Cx.callee_saved_registers (Cx.get_spilled_callee_saved_registers ra.cx)
    in
    let possible_already_used_callee_saved_regs =
      RegSet.diff possible_regs unused_callee_saved_regs
    in
    match find_highest_priority_reg possible_already_used_callee_saved_regs reg_priorities with
    | Some reg -> Some reg
    | None ->
      let possible_unused_callee_saved_regs = RegSet.inter possible_regs unused_callee_saved_regs in
      (match find_highest_priority_reg possible_unused_callee_saved_regs reg_priorities with
      | None -> None
      | Some reg ->
        Cx.add_spilled_callee_saved_register ra.cx reg;
        Some reg)

  (* Pop nodes off the select stack and greedily assign colors to them. If no color can be assigned,
     add vreg to spill worklist. *)
  let assign_colors ~(ra : t) =
    while ra.select_stack <> [] do
      let vreg = List.hd ra.select_stack in
      ra.select_stack <- List.tl ra.select_stack;
      let interfering_vregs = OOMMap.find_all vreg ra.interference_graph in

      (* Create a set of all registers and remove colors of all neighbors in interference graph *)
      let ok_registers = ref (Cx.allocatable_registers (Cx.get_class vreg)) in
      OperandSet.iter
        (fun interfering_vreg ->
          let alias = get_operand_alias ~ra interfering_vreg in
          if OperandSet.mem alias ra.colored_vregs || is_precolored alias then
            match Cx.get_physical_register_opt alias with
            | Some reg -> ok_registers := RegSet.remove reg !ok_registers
            | _ -> ())
        interfering_vregs;

      (* Calculate priorities for each color. Simple heuristic is choose most common color among
         colored registers that are move related to this vreg.*)
      let register_priorities = ref RegMap.empty in
      let vreg_moves = OInstrMMap.find_all vreg ra.move_list in
      InstrSet.iter
        (fun move_instr ->
          let (reg1, reg2) = source_dest_regs_of_move ~ra move_instr in
          let reg1 = get_operand_alias ~ra reg1 in
          let reg2 = get_operand_alias ~ra reg2 in
          let move_related_reg =
            if vreg == reg1 then
              reg2
            else
              reg1
          in
          if OperandSet.mem move_related_reg ra.colored_vregs || is_precolored move_related_reg then
            match Cx.get_physical_register_opt move_related_reg with
            | Some reg ->
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
        Cx.assign_physical_register vreg physical_reg
    done;
    OperandSet.iter
      (fun vreg ->
        match Cx.get_physical_register_opt (get_operand_alias ~ra vreg) with
        | Some alias_physical_reg -> Cx.assign_physical_register vreg alias_physical_reg
        | _ -> failwith "Alias must be colored")
      ra.coalesced_vregs

  let rewrite_program ~(ra : t) =
    (* Resolve spilled vregs to virtual stack slots *)
    OperandSet.iter (Cx.spill_virtual_register ra.cx) ra.spilled_vregs;
    (* Then rewrite program to include newly resolved memory locations *)
    Cx.rewrite_spilled_program ra.cx ~get_alias:(get_operand_alias ~ra);
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
    Cx.iter_blocks
      (fun block ->
        Cx.filter_instrs
          (fun instr ->
            match Cx.get_move_opt instr with
            | Some (source_op, dest_op) ->
              get_operand_alias ~ra source_op != get_operand_alias ~ra dest_op
            | None -> true)
          block)
      ra.cx

  let initialize ~ra =
    (* Collect all registers in program, splitting into precolored and other initial vregs *)
    let (num_use_defs, initial_vregs) = Cx.init_context ra.cx in
    ra.reg_num_use_defs <- num_use_defs;
    ra.initial_vregs <- initial_vregs;
    ra.interference_degree <-
      RegMap.fold
        (fun _ reg interference_degree -> OperandMap.add reg Int.max_int interference_degree)
        (Cx.get_rep_physical_registers ra.cx)
        OperandMap.empty

  (* Allocate physical registers (colors) to each virtual register using iterated register coalescing.
     Simply the graph afterwards to remove unnecessary instructions. *)
  let allocate_registers ~(ra : t) =
    (* Perform initial rewrite to force registers in some locations *)
    Cx.rewrite_spilled_program ra.cx ~get_alias:(get_operand_alias ~ra);

    let rec iter () =
      initialize ~ra;
      liveness_analysis ~ra;
      build_interference_graph ~ra;
      make_worklist ~ra;
      while
        not
          (OperandSet.is_empty ra.simplify_worklist
          && InstrSet.is_empty ra.worklist_moves
          && OperandSet.is_empty ra.freeze_worklist
          && OperandSet.is_empty ra.spill_worklist)
      do
        if not (OperandSet.is_empty ra.simplify_worklist) then
          simplify ~ra
        else if not (InstrSet.is_empty ra.worklist_moves) then
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
