open Basic_collections
open X86_gen_context
open X86_instructions

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

let add_to_vi_multimap key value mmap =
  let new_values =
    match VRegMap.find_opt key mmap with
    | None -> ISet.singleton value
    | Some values -> ISet.add value values
  in
  VRegMap.add key new_values mmap

let in_vi_multimap key value mmap =
  match VRegMap.find_opt key mmap with
  | None -> false
  | Some values -> ISet.mem value values

let get_from_vi_multimap key mmap =
  match VRegMap.find_opt key mmap with
  | None -> ISet.empty
  | Some value -> value

let add_to_vv_multimap key value mmap =
  let new_values =
    match VRegMap.find_opt key mmap with
    | None -> VRegSet.singleton value
    | Some values -> VRegSet.add value values
  in
  VRegMap.add key new_values mmap

let in_vv_multimap key value mmap =
  match VRegMap.find_opt key mmap with
  | None -> false
  | Some values -> VRegSet.mem value values

let get_from_vv_multimap key mmap =
  match VRegMap.find_opt key mmap with
  | None -> VRegSet.empty
  | Some value -> value

let add_to_multimap key value mmap =
  let new_values =
    match VRegMap.find_opt key mmap with
    | None -> ISet.singleton value
    | Some values -> ISet.add value values
  in
  VRegMap.add key new_values mmap

let get_from_multimap key mmap =
  match IMap.find_opt key mmap with
  | None -> ISet.empty
  | Some value -> value

let in_multimap key value mmap =
  match IMap.find_opt key mmap with
  | None -> false
  | Some values -> ISet.mem value values

let liveness_analysis ~(gcx : Gcx.t) =
  let (_, live_out) = X86_liveness_analysis.analyze_vregs gcx.blocks_by_id in
  gcx.live_out <- live_out

class use_def_finder =
  object
    inherit X86_visitor.instruction_visitor

    val mutable vreg_uses = VRegSet.empty

    val mutable vreg_defs = VRegSet.empty

    method vreg_uses = vreg_uses

    method vreg_defs = vreg_defs

    method reset =
      vreg_uses <- VRegSet.empty;
      vreg_defs <- VRegSet.empty

    method! visit_read_vreg ~block:_ vreg = vreg_uses <- VRegSet.add vreg vreg_uses

    method! visit_write_vreg ~block:_ vreg = vreg_defs <- VRegSet.add vreg vreg_defs
  end

(* Add an interference edge between two virtual registers, also updating degree *)
let add_interference_edge ~(gcx : Gcx.t) vreg1 vreg2 =
  let inc_degree vreg =
    gcx.interference_degree <-
      (match VRegMap.find_opt vreg gcx.interference_degree with
      | None -> VRegMap.add vreg 1 gcx.interference_degree
      | Some degree -> VRegMap.add vreg (degree + 1) gcx.interference_degree)
  in
  if (not (in_vv_multimap vreg1 vreg2 gcx.interference_graph)) && vreg1 != vreg2 then (
    if not (VRegSet.mem vreg1 gcx.precolored_vregs) then (
      gcx.interference_graph <- add_to_vv_multimap vreg1 vreg2 gcx.interference_graph;
      inc_degree vreg1
    );
    if not (VRegSet.mem vreg2 gcx.precolored_vregs) then (
      gcx.interference_graph <- add_to_vv_multimap vreg2 vreg1 gcx.interference_graph;
      inc_degree vreg2
    )
  )

let build_interference_graph ~(gcx : Gcx.t) =
  let use_def_finder = new use_def_finder in
  let find_use_defs block instr =
    use_def_finder#reset;
    use_def_finder#visit_instruction ~block instr;
    (use_def_finder#vreg_uses, use_def_finder#vreg_defs)
  in
  IMap.iter
    (fun block_id block ->
      let live = ref (IMap.find block_id gcx.live_out |> VRegSet.of_list) in
      List.iter
        (fun ((instr_id, instr) as instr_with_id) ->
          begin
            match instr with
            | Instruction.MovMM (Reg src_vreg, Reg dest_vreg) ->
              live := VRegSet.remove src_vreg !live;
              gcx.move_list <- add_to_multimap src_vreg instr_id gcx.move_list;
              gcx.move_list <- add_to_multimap dest_vreg instr_id gcx.move_list;
              gcx.worklist_moves <- ISet.add instr_id gcx.worklist_moves
            (* Caller saved registers are modeled by creating interferences with all live registers
               at call instructions. *)
            | Instruction.CallL _
            | Instruction.CallM _ ->
              RegSet.iter
                (fun reg ->
                  let color_vreg = RegMap.find reg gcx.color_to_vreg in
                  VRegSet.iter
                    (fun live_vreg -> add_interference_edge ~gcx live_vreg color_vreg)
                    !live)
                caller_saved_registers
            | _ -> ()
          end;
          let (vreg_uses, vreg_defs) = find_use_defs block instr_with_id in
          live := VRegSet.union vreg_defs !live;
          VRegSet.iter
            (fun vreg_def ->
              VRegSet.iter (fun live_vreg -> add_interference_edge ~gcx live_vreg vreg_def) !live)
            vreg_defs;
          live := VRegSet.union vreg_uses (VRegSet.diff !live vreg_defs))
        (List.rev block.Block.instructions))
    gcx.blocks_by_id

let adjacent ~(gcx : Gcx.t) vreg =
  let adjacent_vregs = get_from_vv_multimap vreg gcx.interference_graph in
  let to_ignore = VRegSet.union (VRegSet.of_list gcx.select_stack) gcx.coalesced_vregs in
  VRegSet.diff adjacent_vregs to_ignore

let node_moves ~(gcx : Gcx.t) vreg =
  ISet.inter
    (get_from_vi_multimap vreg gcx.move_list)
    (ISet.union gcx.active_moves gcx.worklist_moves)

let move_related ~gcx vreg = not (ISet.is_empty (node_moves ~gcx vreg))

let degree ~(gcx : Gcx.t) vreg =
  VRegMap.find_opt vreg gcx.interference_degree |> Option.value ~default:0

let make_worklist ~(gcx : Gcx.t) =
  VRegSet.iter
    (fun vreg ->
      gcx.initial_vregs <- VRegSet.remove vreg gcx.initial_vregs;
      if degree ~gcx vreg >= num_allocatable_registers then
        gcx.spill_worklist <- VRegSet.add vreg gcx.spill_worklist
      else if move_related ~gcx vreg then
        gcx.freeze_worklist <- VRegSet.add vreg gcx.freeze_worklist
      else
        gcx.simplify_worklist <- VRegSet.add vreg gcx.simplify_worklist)
    gcx.initial_vregs

let enable_moves ~(gcx : Gcx.t) vregs =
  VRegSet.iter
    (fun vreg ->
      let node_moves = node_moves ~gcx vreg in
      ISet.iter
        (fun move_instr_id ->
          if ISet.mem move_instr_id gcx.active_moves then (
            gcx.active_moves <- ISet.remove move_instr_id gcx.active_moves;
            gcx.worklist_moves <- ISet.add move_instr_id gcx.worklist_moves
          ))
        node_moves)
    vregs

let decrement_degree ~(gcx : Gcx.t) vreg =
  let degree = degree ~gcx vreg in
  gcx.interference_degree <- VRegMap.add vreg (max 0 (degree - 1)) gcx.interference_degree;
  if degree = num_allocatable_registers then (
    enable_moves ~gcx (VRegSet.add vreg (adjacent ~gcx vreg));
    gcx.spill_worklist <- VRegSet.remove vreg gcx.spill_worklist;
    if move_related ~gcx vreg then
      gcx.freeze_worklist <- VRegSet.add vreg gcx.freeze_worklist
    else
      gcx.simplify_worklist <- VRegSet.add vreg gcx.simplify_worklist
  )

(* Choose a vreg from the worklist of low degree, non move related vregs. Remove this vreg from
   the graph, adding it to the select stack, and decrement the degree of all its neighbors. *)
let simplify ~(gcx : Gcx.t) =
  let vreg = VRegSet.choose gcx.simplify_worklist in
  gcx.simplify_worklist <- VRegSet.remove vreg gcx.simplify_worklist;
  gcx.select_stack <- vreg :: gcx.select_stack;
  let adjacent_vregs = adjacent ~gcx vreg in
  VRegSet.iter (fun adjacent_vreg -> decrement_degree ~gcx adjacent_vreg) adjacent_vregs

let add_to_simplify_work_list ~(gcx : Gcx.t) vreg =
  if
    (not (VRegSet.mem vreg gcx.precolored_vregs))
    && (not (move_related ~gcx vreg))
    && degree ~gcx vreg < num_allocatable_registers
  then (
    gcx.freeze_worklist <- VRegSet.remove vreg gcx.freeze_worklist;
    gcx.simplify_worklist <- VRegSet.add vreg gcx.simplify_worklist
  )

(* A virtual register can only be coalesced with a precolored register if the coalescing would
   not increase the degree of any adjacent vregs to K or greater. *)
let can_coalesce_with_precolored ~(gcx : Gcx.t) precolored vreg =
  VRegSet.for_all
    (fun adjacent_vreg ->
      (* Degree stays the same since vreg gains and loses a neighbor *)
      degree ~gcx adjacent_vreg < num_allocatable_registers
      (* All precolored registers interfere so degree does not change *)
      || VRegSet.mem adjacent_vreg gcx.precolored_vregs
      (* Already interferes so degree does not change *)
      || in_vv_multimap adjacent_vreg precolored gcx.interference_graph)
    (adjacent ~gcx vreg)

(* Briggs conservative coalescing heuristic:
   If there are fewer than K neighbor vregs of significant degree, then coalescing cannot make
   the graph non-K colorable. *)
let can_conservative_coalesce ~(gcx : Gcx.t) vreg1 vreg2 =
  let neighbor_vregs = VRegSet.union (adjacent ~gcx vreg1) (adjacent ~gcx vreg2) in
  let k = ref 0 in
  VRegSet.iter
    (fun vreg -> if degree ~gcx vreg >= num_allocatable_registers then k := !k + 1)
    neighbor_vregs;
  !k < num_allocatable_registers

(* Combine two vregs, making them alias to each other, combining their moves and interference edges.
   *)
let combine_vregs ~(gcx : Gcx.t) alias_vreg vreg =
  if VRegSet.mem vreg gcx.freeze_worklist then
    gcx.freeze_worklist <- VRegSet.remove vreg gcx.freeze_worklist
  else
    gcx.spill_worklist <- VRegSet.remove vreg gcx.spill_worklist;
  gcx.coalesced_vregs <- VRegSet.add vreg gcx.coalesced_vregs;
  vreg.resolution <- Alias alias_vreg;
  gcx.move_list <-
    VRegMap.add
      alias_vreg
      (ISet.union
         (get_from_vi_multimap alias_vreg gcx.move_list)
         (get_from_vi_multimap vreg gcx.move_list))
      gcx.move_list;
  VRegSet.iter
    (fun adjacent_vreg ->
      add_interference_edge ~gcx adjacent_vreg alias_vreg;
      decrement_degree ~gcx adjacent_vreg)
    (adjacent ~gcx vreg);
  if
    degree ~gcx alias_vreg >= num_allocatable_registers
    && VRegSet.mem alias_vreg gcx.freeze_worklist
  then (
    gcx.freeze_worklist <- VRegSet.remove alias_vreg gcx.freeze_worklist;
    gcx.spill_worklist <- VRegSet.add alias_vreg gcx.spill_worklist
  )

(* Return the source and destination vregs for a particular instruction (MovRR (source, dest)) *)
let source_dest_vregs_of_move ~(gcx : Gcx.t) move_instr_id =
  let move_instruction = Gcx.get_instruction ~gcx move_instr_id in
  match move_instruction with
  | Instruction.MovMM (Reg source_vreg, Reg dest_vreg) -> (source_vreg, dest_vreg)
  | _ -> failwith "Expected id of virtual register to virtual register move instruction"

(* Choose a move from the worklist and try to  combine its virtual registers if doing so would
   not make the graph uncolorable *)
let coalesce ~(gcx : Gcx.t) =
  (* Choose an abitrary move instruction *)
  let move_instr_id = ISet.choose gcx.worklist_moves in
  gcx.worklist_moves <- ISet.remove move_instr_id gcx.worklist_moves;
  let (source_vreg, dest_vreg) = source_dest_vregs_of_move ~gcx move_instr_id in
  let source_vreg = Gcx.get_vreg_alias ~gcx source_vreg in
  let dest_vreg = Gcx.get_vreg_alias ~gcx dest_vreg in
  let (vreg1, vreg2) =
    if VRegSet.mem dest_vreg gcx.precolored_vregs then
      (dest_vreg, source_vreg)
    else
      (source_vreg, dest_vreg)
  in
  if vreg1 = vreg2 then (
    gcx.coalesced_moves <- ISet.add move_instr_id gcx.coalesced_moves;
    add_to_simplify_work_list ~gcx vreg1
  ) else if
      VRegSet.mem vreg2 gcx.precolored_vregs || in_vv_multimap vreg2 vreg1 gcx.interference_graph
    then (
    gcx.constrained_moves <- ISet.add move_instr_id gcx.constrained_moves;
    add_to_simplify_work_list ~gcx vreg1;
    add_to_simplify_work_list ~gcx vreg2
  ) else if
      (VRegSet.mem vreg1 gcx.precolored_vregs && can_coalesce_with_precolored ~gcx vreg1 vreg2)
      || (not (VRegSet.mem vreg1 gcx.precolored_vregs))
         && can_conservative_coalesce ~gcx vreg1 vreg2
    then (
    gcx.coalesced_moves <- ISet.add move_instr_id gcx.coalesced_moves;
    combine_vregs ~gcx vreg1 vreg2;
    add_to_simplify_work_list ~gcx vreg1
  ) else
    gcx.active_moves <- ISet.add move_instr_id gcx.active_moves

(* Freeze all the moves associated with a vreg. This makes them no longer eligible for coalescing. *)
let freeze_moves ~(gcx : Gcx.t) vreg =
  let vreg_moves = node_moves ~gcx vreg in
  ISet.iter
    (fun move_instr_id ->
      if ISet.mem move_instr_id gcx.active_moves then
        gcx.active_moves <- ISet.remove move_instr_id gcx.active_moves
      else
        gcx.worklist_moves <- ISet.remove move_instr_id gcx.worklist_moves;
      gcx.frozen_moves <- ISet.add move_instr_id gcx.frozen_moves;
      let (source_vreg, dest_vreg) = source_dest_vregs_of_move ~gcx move_instr_id in
      let other_vreg =
        if source_vreg = vreg then
          dest_vreg
        else if dest_vreg = vreg then
          source_vreg
        else
          failwith "Moves in move list must be indexed by register in that move"
      in
      if
        ISet.is_empty (node_moves ~gcx other_vreg)
        && degree ~gcx other_vreg < num_allocatable_registers
      then (
        gcx.freeze_worklist <- VRegSet.remove other_vreg gcx.freeze_worklist;
        gcx.simplify_worklist <- VRegSet.add other_vreg gcx.simplify_worklist
      ))
    vreg_moves

(* Choose a vreg from the freeze worklist and freeze all the moves associated with it. Vreg can now
   be simplified. *)
let freeze ~(gcx : Gcx.t) =
  let vreg = VRegSet.choose gcx.freeze_worklist in
  gcx.freeze_worklist <- VRegSet.remove vreg gcx.freeze_worklist;
  gcx.simplify_worklist <- VRegSet.add vreg gcx.simplify_worklist;
  freeze_moves ~gcx vreg

(* Choose a vreg from the spill worklist and freeze all the moves associated with it. Vreg can now
   be simplified.*)
let select_spill ~(gcx : Gcx.t) vreg_num_use_defs =
  (* Simple spill heuristic - minimize cost C where C = (#uses + #defs) / degree *)
  let heuristic_chosen_vreg =
    VRegSet.fold
      (fun vreg chosen_vreg_opt ->
        let num_use_defs = Float.of_int (VRegMap.find vreg vreg_num_use_defs) in
        let degree = Float.of_int (degree ~gcx vreg) in
        let cost = num_use_defs /. degree in
        match chosen_vreg_opt with
        | None -> Some (vreg, cost)
        | Some (_, chosen_cost) ->
          if cost < chosen_cost then
            Some (vreg, cost)
          else
            chosen_vreg_opt)
      gcx.spill_worklist
      None
  in
  let potential_spill_vreg =
    match heuristic_chosen_vreg with
    | Some (vreg, _) -> vreg
    | None -> VRegSet.choose gcx.spill_worklist
  in
  gcx.spill_worklist <- VRegSet.remove potential_spill_vreg gcx.spill_worklist;
  gcx.simplify_worklist <- VRegSet.add potential_spill_vreg gcx.simplify_worklist;
  freeze_moves ~gcx potential_spill_vreg

(* Select a color for a vreg from a set of possible colors to choose from. Only spill a new callee
   saved register if we need to. *)
let select_color_for_vreg ~(gcx : Gcx.t) vreg possible_regs =
  (* Unresolved vregs must always be part of a function *)
  let func = Option.get vreg.VReg.func in
  let func = IMap.find func gcx.funcs_by_id in
  let unused_callee_saved_regs =
    RegSet.diff callee_saved_registers func.spilled_callee_saved_regs
  in
  let possible_not_unused_callee_saved_regs = RegSet.diff possible_regs unused_callee_saved_regs in
  match RegSet.min_elt_opt possible_not_unused_callee_saved_regs with
  | Some reg -> Some reg
  | None ->
    let possible_unused_callee_saved_regs = RegSet.inter possible_regs unused_callee_saved_regs in
    (match RegSet.min_elt_opt possible_unused_callee_saved_regs with
    | None -> None
    | Some reg ->
      func.spilled_callee_saved_regs <- RegSet.add reg func.spilled_callee_saved_regs;
      Some reg)

(* Pop nodes off the select stack and greedily assign colors to them. If no color can be assigned,
   add vreg to spill worklist. *)
let assign_colors ~(gcx : Gcx.t) =
  while gcx.select_stack <> [] do
    let vreg = List.hd gcx.select_stack in
    gcx.select_stack <- List.tl gcx.select_stack;
    let interfering_vregs = get_from_vv_multimap vreg gcx.interference_graph in
    (* Create a set of all registers and remove colors of all neighbors in interference graph *)
    let ok_registers = ref general_purpose_registers in
    VRegSet.iter
      (fun interfering_vreg ->
        let alias = Gcx.get_vreg_alias ~gcx interfering_vreg in
        if VRegSet.mem alias gcx.colored_vregs || VRegSet.mem alias gcx.precolored_vregs then
          match alias.resolution with
          | Physical reg -> ok_registers := RegSet.remove reg !ok_registers
          | _ -> ())
      interfering_vregs;
    (* Choose an arbitrary color from the remaining set, otherwise spill *)
    match select_color_for_vreg ~gcx vreg !ok_registers with
    | None -> gcx.spilled_vregs <- VRegSet.add vreg gcx.spilled_vregs
    | Some physical_reg ->
      gcx.colored_vregs <- VRegSet.add vreg gcx.colored_vregs;
      vreg.resolution <- Physical physical_reg
  done;
  VRegSet.iter
    (fun vreg ->
      match Gcx.get_vreg_resolution ~gcx vreg with
      | Physical _ as alias_resolution -> vreg.resolution <- alias_resolution
      | _ -> failwith "Alias must be colored")
    gcx.coalesced_vregs

let rewrite_program ~(gcx : Gcx.t) =
  (* Resolve spilled vregs to virtual stack slots *)
  VRegSet.iter
    (fun vreg ->
      let func = IMap.find (Option.get vreg.func) gcx.funcs_by_id in
      func.spilled_vregs <- VRegSet.add vreg func.spilled_vregs;
      vreg.resolution <- StackSlot (VirtualStackSlot vreg))
    gcx.spilled_vregs;
  (* Then rewrite program to include newly resolved memory locations *)
  let spill_writer = new X86_spill_writer.spill_writer ~gcx in
  IMap.iter (fun _ block -> spill_writer#write_block_spills block) gcx.blocks_by_id;
  (* Reset state of register allocator *)
  let new_vregs = spill_writer#new_vregs in
  gcx.spilled_vregs <- VRegSet.empty;
  gcx.initial_vregs <- VRegSet.union new_vregs (VRegSet.union gcx.colored_vregs gcx.coalesced_vregs);
  gcx.colored_vregs <- VRegSet.empty;
  gcx.coalesced_vregs <- VRegSet.empty;
  gcx.move_list <- VRegMap.empty

(* Remove all moves where both the source and destination alias to the same register, as they are
   unnecessary. *)
let remove_coalesced_moves ~(gcx : Gcx.t) =
  let open Block in
  IMap.iter
    (fun _ block ->
      block.instructions <-
        List.filter
          (fun (_, instr) ->
            match instr with
            | Instruction.MovMM (Reg source_vreg, Reg dest_vreg) ->
              Gcx.get_vreg_alias ~gcx source_vreg != Gcx.get_vreg_alias ~gcx dest_vreg
            | _ -> true)
          block.instructions)
    gcx.blocks_by_id

class allocate_init_visitor =
  object (this)
    inherit X86_visitor.instruction_visitor

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

    method! visit_read_vreg ~block:_ vreg = this#visit_vreg vreg

    method! visit_write_vreg ~block:_ vreg = this#visit_vreg vreg
  end

(* Allocate physical registers (colors) to each virtual register using iterated register coalescing.
   Simply the graph afterwards to remove unnecessary instructions. *)
let allocate_registers ~(gcx : Gcx.t) =
  (* Collect all registers in program, then remove precolored to create initial vreg list *)
  let init_visitor = new allocate_init_visitor in
  IMap.iter
    (fun _ block ->
      List.iter (fun instr -> init_visitor#visit_instruction ~block instr) block.instructions)
    gcx.blocks_by_id;
  let vreg_num_use_defs = init_visitor#vreg_num_use_defs in
  gcx.initial_vregs <- VRegSet.diff init_visitor#all_vregs gcx.precolored_vregs;
  let rec iter () =
    liveness_analysis ~gcx;
    build_interference_graph ~gcx;
    make_worklist ~gcx;
    while
      not
        ( VRegSet.is_empty gcx.simplify_worklist
        && ISet.is_empty gcx.worklist_moves
        && VRegSet.is_empty gcx.freeze_worklist
        && VRegSet.is_empty gcx.spill_worklist )
    do
      if not (VRegSet.is_empty gcx.simplify_worklist) then
        simplify ~gcx
      else if not (ISet.is_empty gcx.worklist_moves) then
        coalesce ~gcx
      else if not (VRegSet.is_empty gcx.freeze_worklist) then
        freeze ~gcx
      else if not (VRegSet.is_empty gcx.spill_worklist) then
        select_spill ~gcx vreg_num_use_defs
    done;
    assign_colors ~gcx;
    if not (VRegSet.is_empty gcx.spilled_vregs) then (
      rewrite_program ~gcx;
      iter ()
    )
  in
  iter ();
  remove_coalesced_moves ~gcx
