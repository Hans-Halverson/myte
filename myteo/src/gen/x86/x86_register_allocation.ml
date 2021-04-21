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

let add_to_multimap key value mmap =
  let new_values =
    match IMap.find_opt key mmap with
    | None -> ISet.singleton value
    | Some values -> ISet.add value values
  in
  IMap.add key new_values mmap

let get_from_multimap key mmap =
  match IMap.find_opt key mmap with
  | None -> ISet.empty
  | Some value -> value

let in_multimap key value mmap =
  match IMap.find_opt key mmap with
  | None -> false
  | Some values -> ISet.mem value values

class liveness_init_visitor (blocks_by_id : virtual_block IMap.t) =
  object (this)
    inherit X86_visitor.instruction_visitor

    val mutable prev_blocks =
      IMap.fold (fun block_id _ acc -> IMap.add block_id ISet.empty acc) blocks_by_id IMap.empty

    val mutable vreg_use_blocks = IMap.empty

    val mutable vreg_def_blocks = IMap.empty

    val mutable vreg_use_before_def_blocks = IMap.empty

    method prev_blocks = prev_blocks

    method vreg_use_blocks = vreg_use_blocks

    method vreg_def_blocks = vreg_def_blocks

    method vreg_use_before_def_blocks = vreg_use_before_def_blocks

    method run () = IMap.iter (fun _ block -> this#visit_block block) blocks_by_id

    method visit_block block = List.iter (this#visit_instruction ~block) block.instructions

    method! visit_block_edge ~block next_block_id =
      prev_blocks <-
        IMap.add next_block_id (ISet.add block.id (IMap.find next_block_id prev_blocks)) prev_blocks

    method! visit_read_vreg ~block vreg_id =
      vreg_use_blocks <- add_to_multimap vreg_id block.id vreg_use_blocks

    method! visit_write_vreg ~block vreg_id =
      if
        in_multimap vreg_id block.id vreg_use_blocks
        && not (in_multimap vreg_id block.id vreg_def_blocks)
      then
        vreg_use_before_def_blocks <- add_to_multimap vreg_id block.id vreg_use_before_def_blocks;
      vreg_def_blocks <- add_to_multimap vreg_id block.id vreg_def_blocks
  end

let rec liveness_analysis ~(gcx : Gcx.t) =
  let (_, live_out) = liveness_analysis_from_blocks gcx.blocks_by_id in
  gcx.live_out <- live_out

and liveness_analysis_from_blocks blocks_by_id =
  (* Calculate use and def blocks for each variable *)
  let init_visitor = new liveness_init_visitor blocks_by_id in
  init_visitor#run ();

  let prev_blocks = init_visitor#prev_blocks in
  let vreg_use_blocks = init_visitor#vreg_use_blocks in
  let vreg_def_blocks = init_visitor#vreg_def_blocks in
  let vreg_use_before_def_blocks = init_visitor#vreg_use_before_def_blocks in

  (* Initialize liveness sets *)
  let live_in = ref IMap.empty in
  let live_out = ref IMap.empty in
  IMap.iter
    (fun block_id _ ->
      live_in := IMap.add block_id [] !live_in;
      live_out := IMap.add block_id [] !live_out)
    blocks_by_id;

  (* Propagate a single variable backwards through the program, building liveness sets as we go *)
  let set_contains set block_id var_id =
    match IMap.find block_id !set with
    | hd :: _ when hd = var_id -> true
    | _ -> false
  in
  let set_add set block_id var_id =
    set := IMap.add block_id (var_id :: IMap.find block_id !set) !set
  in
  let rec propagate_backwards ~block_id ~vreg_id =
    (* Stop backwards propagation if we reach a block that has already been visited or where the
       vreg is defined (unless the vreg is used in the block before it is defined in the block) *)
    if
      (not (set_contains live_in block_id vreg_id))
      && ( (not (in_multimap vreg_id block_id vreg_def_blocks))
         || in_multimap vreg_id block_id vreg_use_before_def_blocks )
    then (
      set_add live_in block_id vreg_id;
      let prev_blocks = IMap.find block_id prev_blocks in
      ISet.iter
        (fun prev_block ->
          if not (set_contains live_out prev_block vreg_id) then set_add live_out prev_block vreg_id;
          propagate_backwards ~block_id:prev_block ~vreg_id)
        prev_blocks
    )
  in

  (* Liveness is calculated for all variables in program *)
  IMap.iter
    (fun vreg_id use_blocks ->
      ISet.iter (fun block_id -> propagate_backwards ~block_id ~vreg_id) use_blocks)
    vreg_use_blocks;

  (!live_in, !live_out)

class use_def_finder =
  object
    inherit X86_visitor.instruction_visitor

    val mutable vreg_uses = ISet.empty

    val mutable vreg_defs = ISet.empty

    method vreg_uses = vreg_uses

    method vreg_defs = vreg_defs

    method! visit_read_vreg ~block:_ vreg_id = vreg_uses <- ISet.add vreg_id vreg_uses

    method! visit_write_vreg ~block:_ vreg_id = vreg_defs <- ISet.add vreg_id vreg_defs
  end

let find_use_defs block instruction =
  let finder = new use_def_finder in
  finder#visit_instruction ~block instruction;
  (finder#vreg_uses, finder#vreg_defs)

(* Add an interference edge between two virtual registers, also updating degree *)
let add_interference_edge ~(gcx : Gcx.t) vreg1 vreg2 =
  let inc_degree vreg =
    gcx.interference_degree <-
      (match IMap.find_opt vreg gcx.interference_degree with
      | None -> IMap.add vreg 1 gcx.interference_degree
      | Some degree -> IMap.add vreg (degree + 1) gcx.interference_degree)
  in
  if (not (in_multimap vreg1 vreg2 gcx.interference_graph)) && vreg1 <> vreg2 then (
    if not (ISet.mem vreg1 gcx.precolored_vregs) then (
      gcx.interference_graph <- add_to_multimap vreg1 vreg2 gcx.interference_graph;
      inc_degree vreg1
    );
    if not (ISet.mem vreg2 gcx.precolored_vregs) then (
      gcx.interference_graph <- add_to_multimap vreg2 vreg1 gcx.interference_graph;
      inc_degree vreg2
    )
  )

let build_interference_graph ~(gcx : Gcx.t) =
  IMap.iter
    (fun block_id block ->
      let live = ref (IMap.find block_id gcx.live_out |> ISet.of_list) in
      List.iter
        (fun ((instr_id, instr) as instr_with_id) ->
          begin
            match instr with
            | Instruction.MovRR (src_vreg, dest_vreg) ->
              live := ISet.remove src_vreg !live;
              gcx.move_list <- add_to_multimap src_vreg instr_id gcx.move_list;
              gcx.move_list <- add_to_multimap dest_vreg instr_id gcx.move_list;
              gcx.worklist_moves <- ISet.add instr_id gcx.worklist_moves
            (* Caller saved registers are modeled by creating interferences with all live registers
               at call instructions. *)
            | Instruction.CallL _
            | Instruction.CallR _ ->
              RegSet.iter
                (fun reg ->
                  let color_vreg = RegMap.find reg gcx.color_to_vreg in
                  ISet.iter (fun live_vreg -> add_interference_edge ~gcx live_vreg color_vreg) !live)
                caller_saved_registers
            | _ -> ()
          end;
          let (vreg_uses, vreg_defs) = find_use_defs block instr_with_id in
          live := ISet.union vreg_defs !live;
          ISet.iter
            (fun vreg_def ->
              ISet.iter (fun live_vreg -> add_interference_edge ~gcx live_vreg vreg_def) !live)
            vreg_defs;
          live := ISet.union vreg_uses (ISet.diff !live vreg_defs))
        (List.rev block.Block.instructions))
    gcx.blocks_by_id

let adjacent ~(gcx : Gcx.t) vreg =
  let adjacent_vregs = get_from_multimap vreg gcx.interference_graph in
  let to_ignore = ISet.union (ISet.of_list gcx.select_stack) gcx.coalesced_vregs in
  ISet.diff adjacent_vregs to_ignore

let node_moves ~(gcx : Gcx.t) vreg =
  ISet.inter (get_from_multimap vreg gcx.move_list) (ISet.union gcx.active_moves gcx.worklist_moves)

let move_related ~gcx vreg = not (ISet.is_empty (node_moves ~gcx vreg))

let degree ~(gcx : Gcx.t) vreg =
  IMap.find_opt vreg gcx.interference_degree |> Option.value ~default:0

let make_worklist ~(gcx : Gcx.t) =
  ISet.iter
    (fun vreg ->
      gcx.initial_vregs <- ISet.remove vreg gcx.initial_vregs;
      if degree ~gcx vreg >= num_allocatable_registers then
        gcx.spill_worklist <- ISet.add vreg gcx.spill_worklist
      else if move_related ~gcx vreg then
        gcx.freeze_worklist <- ISet.add vreg gcx.freeze_worklist
      else
        gcx.simplify_worklist <- ISet.add vreg gcx.simplify_worklist)
    gcx.initial_vregs

let enable_moves ~(gcx : Gcx.t) vregs =
  ISet.iter
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
  gcx.interference_degree <- IMap.add vreg (max 0 (degree - 1)) gcx.interference_degree;
  if degree = num_allocatable_registers then (
    enable_moves ~gcx (ISet.add vreg (adjacent ~gcx vreg));
    gcx.spill_worklist <- ISet.remove vreg gcx.spill_worklist;
    if move_related ~gcx vreg then
      gcx.freeze_worklist <- ISet.add vreg gcx.freeze_worklist
    else
      gcx.simplify_worklist <- ISet.add vreg gcx.simplify_worklist
  )

(* Choose a vreg from the worklist of low degree, non move related vregs. Remove this vreg from
   the graph, adding it to the select stack, and decrement the degree of all its neighbors. *)
let simplify ~(gcx : Gcx.t) =
  let vreg = ISet.choose gcx.simplify_worklist in
  gcx.simplify_worklist <- ISet.remove vreg gcx.simplify_worklist;
  gcx.select_stack <- vreg :: gcx.select_stack;
  let adjacent_vregs = adjacent ~gcx vreg in
  ISet.iter (fun adjacent_vreg -> decrement_degree ~gcx adjacent_vreg) adjacent_vregs

(* Fully resolve an alias (may be a chain in the alias map) *)
let get_alias ~gcx = Gcx.get_vreg_alias ~gcx

let add_to_simplify_work_list ~(gcx : Gcx.t) vreg =
  if
    (not (ISet.mem vreg gcx.precolored_vregs))
    && (not (move_related ~gcx vreg))
    && degree ~gcx vreg < num_allocatable_registers
  then (
    gcx.freeze_worklist <- ISet.remove vreg gcx.freeze_worklist;
    gcx.simplify_worklist <- ISet.add vreg gcx.simplify_worklist
  )

(* A virtual register can only be coalesced with a precolored register if the coalescing would
   not increase the degree of any adjacent vregs to K or greater. *)
let can_coalesce_with_precolored ~(gcx : Gcx.t) precolored vreg =
  ISet.for_all
    (fun adjacent_vreg ->
      (* Degree stays the same since vreg gains and loses a neighbor *)
      degree ~gcx adjacent_vreg < num_allocatable_registers
      (* All precolored registers interfere so degree does not change *)
      || ISet.mem adjacent_vreg gcx.precolored_vregs
      (* Already interferes so degree does not change *)
      || in_multimap adjacent_vreg precolored gcx.interference_graph)
    (adjacent ~gcx vreg)

(* Briggs conservative coalescing heuristic:
   If there are fewer than K neighbor vregs of significant degree, then coalescing cannot make
   the graph non-K colorable. *)
let can_conservative_coalesce ~(gcx : Gcx.t) vreg1 vreg2 =
  let neighbor_vregs = ISet.union (adjacent ~gcx vreg1) (adjacent ~gcx vreg2) in
  let k = ref 0 in
  ISet.iter
    (fun vreg -> if degree ~gcx vreg >= num_allocatable_registers then k := !k + 1)
    neighbor_vregs;
  !k < num_allocatable_registers

(* Combine two vregs, making them alias to each other, combining their moves and interference edges.
   *)
let combine_vregs ~(gcx : Gcx.t) alias_vreg vreg =
  if ISet.mem vreg gcx.freeze_worklist then
    gcx.freeze_worklist <- ISet.remove vreg gcx.freeze_worklist
  else
    gcx.spill_worklist <- ISet.remove vreg gcx.spill_worklist;
  gcx.coalesced_vregs <- ISet.add vreg gcx.coalesced_vregs;
  gcx.vreg_to_alias <- IMap.add vreg alias_vreg gcx.vreg_to_alias;
  gcx.move_list <-
    IMap.add
      alias_vreg
      (ISet.union
         (get_from_multimap alias_vreg gcx.move_list)
         (get_from_multimap vreg gcx.move_list))
      gcx.move_list;
  ISet.iter
    (fun adjacent_vreg ->
      add_interference_edge ~gcx adjacent_vreg alias_vreg;
      decrement_degree ~gcx adjacent_vreg)
    (adjacent ~gcx vreg);
  if degree ~gcx alias_vreg >= num_allocatable_registers && ISet.mem alias_vreg gcx.freeze_worklist
  then (
    gcx.freeze_worklist <- ISet.remove alias_vreg gcx.freeze_worklist;
    gcx.spill_worklist <- ISet.add alias_vreg gcx.spill_worklist
  )

(* Return the source and destination vregs for a particular instruction (MovRR (source, dest)) *)
let source_dest_vregs_of_move ~(gcx : Gcx.t) move_instr_id =
  let move_instruction = Gcx.get_instruction ~gcx move_instr_id in
  match move_instruction with
  | Instruction.MovRR (source_vreg, dest_vreg) -> (source_vreg, dest_vreg)
  | _ -> failwith "Expected id of virtual register to virtual register move instruction"

(* Choose a move from the worklist and try to  combine its virtual registers if doing so would
   not make the graph uncolorable *)
let coalesce ~(gcx : Gcx.t) =
  (* Choose an abitrary move instruction *)
  let move_instr_id = ISet.choose gcx.worklist_moves in
  gcx.worklist_moves <- ISet.remove move_instr_id gcx.worklist_moves;
  let (source_vreg, dest_vreg) = source_dest_vregs_of_move ~gcx move_instr_id in
  let source_vreg = get_alias ~gcx source_vreg in
  let dest_vreg = get_alias ~gcx dest_vreg in
  let (vreg1, vreg2) =
    if ISet.mem dest_vreg gcx.precolored_vregs then
      (dest_vreg, source_vreg)
    else
      (source_vreg, dest_vreg)
  in
  if vreg1 = vreg2 then (
    gcx.coalesced_moves <- ISet.add move_instr_id gcx.coalesced_moves;
    add_to_simplify_work_list ~gcx vreg1
  ) else if ISet.mem vreg2 gcx.precolored_vregs || in_multimap vreg2 vreg1 gcx.interference_graph
    then (
    gcx.constrained_moves <- ISet.add move_instr_id gcx.constrained_moves;
    add_to_simplify_work_list ~gcx vreg1;
    add_to_simplify_work_list ~gcx vreg2
  ) else if
      (ISet.mem vreg1 gcx.precolored_vregs && can_coalesce_with_precolored ~gcx vreg1 vreg2)
      || ((not (ISet.mem vreg1 gcx.precolored_vregs)) && can_conservative_coalesce ~gcx vreg1 vreg2)
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
        gcx.freeze_worklist <- ISet.remove other_vreg gcx.freeze_worklist;
        gcx.simplify_worklist <- ISet.add other_vreg gcx.simplify_worklist
      ))
    vreg_moves

(* Choose a vreg from the freeze worklist and freeze all the moves associated with it. Vreg can now
   be simplified. *)
let freeze ~(gcx : Gcx.t) =
  let vreg = ISet.choose gcx.freeze_worklist in
  gcx.freeze_worklist <- ISet.remove vreg gcx.freeze_worklist;
  gcx.simplify_worklist <- ISet.add vreg gcx.simplify_worklist;
  freeze_moves ~gcx vreg

(* Choose a vreg from the spill worklist and freeze all the moves associated with it. Vreg can now
   be simplified.*)
let select_spill ~(gcx : Gcx.t) =
  (* TODO: Choose a good heuristic for selection of vreg to spill *)
  let vreg = ISet.choose gcx.spill_worklist in
  gcx.spill_worklist <- ISet.remove vreg gcx.spill_worklist;
  gcx.simplify_worklist <- ISet.add vreg gcx.simplify_worklist;
  freeze_moves ~gcx vreg

(* Select a color for a vreg from a set of possible colors to choose from. Only spill a new callee
   saved register if we need to. *)
let select_color_for_vreg ~(gcx : Gcx.t) vreg possible_regs =
  let func = IMap.find vreg gcx.vreg_to_func in
  let used_callee_saved_regs =
    IMap.find_opt func gcx.func_to_spilled_callee_saved_reg |> Option.value ~default:RegSet.empty
  in
  let unused_callee_saved_regs = RegSet.diff callee_saved_registers used_callee_saved_regs in
  let possible_not_unused_callee_saved_regs = RegSet.diff possible_regs unused_callee_saved_regs in
  match RegSet.min_elt_opt possible_not_unused_callee_saved_regs with
  | Some reg -> Some reg
  | None ->
    let possible_unused_callee_saved_regs = RegSet.inter possible_regs unused_callee_saved_regs in
    (match RegSet.min_elt_opt possible_unused_callee_saved_regs with
    | None -> None
    | Some reg ->
      gcx.func_to_spilled_callee_saved_reg <-
        IMap.add
          func
          (match IMap.find_opt func gcx.func_to_spilled_callee_saved_reg with
          | None -> RegSet.singleton reg
          | Some regs -> RegSet.add reg regs)
          gcx.func_to_spilled_callee_saved_reg;
      Some reg)

(* Pop nodes off the select stack and greedily assign colors to them. If no color can be assigned,
   add vreg to spill worklist. *)
let assign_colors ~(gcx : Gcx.t) =
  while gcx.select_stack <> [] do
    let vreg = List.hd gcx.select_stack in
    gcx.select_stack <- List.tl gcx.select_stack;
    let interfering_vregs = get_from_multimap vreg gcx.interference_graph in
    let all_colored_vregs = ISet.union gcx.colored_vregs gcx.precolored_vregs in
    (* Create a set of all registers and remove colors of all neighbors in interference graph *)
    let ok_registers = ref general_purpose_registers in
    ISet.iter
      (fun interfering_vreg ->
        let interfering_vreg = get_alias ~gcx interfering_vreg in
        if ISet.mem interfering_vreg all_colored_vregs then
          ok_registers := RegSet.remove (IMap.find interfering_vreg gcx.vreg_to_color) !ok_registers)
      interfering_vregs;
    (* Choose an arbitrary color from the remaining set, otherwise spill *)
    match select_color_for_vreg ~gcx vreg !ok_registers with
    | None -> gcx.spilled_vregs <- ISet.add vreg gcx.spilled_vregs
    | Some physical_reg ->
      gcx.colored_vregs <- ISet.add vreg gcx.colored_vregs;
      gcx.vreg_to_color <- IMap.add vreg physical_reg gcx.vreg_to_color
  done;
  ISet.iter
    (fun vreg ->
      let vreg_alias = get_alias ~gcx vreg in
      gcx.vreg_to_color <- IMap.add vreg (IMap.find vreg_alias gcx.vreg_to_color) gcx.vreg_to_color)
    gcx.coalesced_vregs

let rewrite_program ~(gcx : Gcx.t) =
  (* TODO: Rewrite program for each vreg in gcx.spilled_vregs, and collect new temporaries *)
  gcx.spilled_vregs <- ISet.empty;
  gcx.initial_vregs <- ISet.union gcx.colored_vregs gcx.coalesced_moves;
  gcx.colored_vregs <- ISet.empty;
  gcx.coalesced_vregs <- ISet.empty

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
            | Instruction.MovRR (source_vreg, dest_vreg) ->
              get_alias ~gcx source_vreg <> get_alias ~gcx dest_vreg
            | _ -> true)
          block.instructions)
    gcx.blocks_by_id

(* Write all function prologues by pushing the used callee saved registers on the stack *)
let write_function_prologues ~(gcx : Gcx.t) =
  IMap.iter
    (fun func_id func ->
      let prologue = IMap.find func.Function.prologue gcx.blocks_by_id in
      let spilled_callee_saved_regs =
        IMap.find_opt func_id gcx.func_to_spilled_callee_saved_reg
        |> Option.value ~default:RegSet.empty
      in
      let push_instrs =
        RegSet.fold
          (fun reg acc ->
            if RegSet.mem reg spilled_callee_saved_regs then
              Instruction.(mk_id (), PushR (Gcx.mk_precolored ~gcx reg)) :: acc
            else
              acc)
          callee_saved_registers
          []
      in
      prologue.instructions <- List.rev push_instrs @ prologue.instructions)
    gcx.funcs_by_id

(* Write all function epilogues by pushing the used callee saved registers on the stack *)
let write_function_epilogues ~(gcx : Gcx.t) =
  IMap.iter
    (fun _ block ->
      let open Block in
      let spilled_callee_saved_regs =
        IMap.find_opt block.func gcx.func_to_spilled_callee_saved_reg
        |> Option.value ~default:RegSet.empty
      in
      let offset = ref 0 in
      block.instructions <-
        List.map
          (fun ((_, instr) as instr_with_id) ->
            let open Instruction in
            match instr with
            | Ret ->
              RegSet.fold
                (fun reg acc ->
                  if RegSet.mem reg spilled_callee_saved_regs then (
                    offset := !offset + 1;
                    Instruction.(mk_id (), PopR (Gcx.mk_precolored ~gcx reg)) :: acc
                  ) else
                    acc)
                callee_saved_registers
                [instr_with_id]
            | _ -> [instr_with_id])
          block.instructions
        |> List.flatten)
    gcx.blocks_by_id

(* Allocate physical registers (colors) to each virtual register using iterated register coalescing.
   Simply the graph afterwards to remove unnecessary instructions. *)
let allocate_registers ~(gcx : Gcx.t) =
  (* Collect all registers in program, then remove precolored to create initial vreg list *)
  let finder = new use_def_finder in
  IMap.iter
    (fun _ block ->
      List.iter (fun instr -> finder#visit_instruction ~block instr) block.instructions)
    gcx.blocks_by_id;
  let all_vregs = ISet.union finder#vreg_uses finder#vreg_defs in
  gcx.initial_vregs <- ISet.diff all_vregs gcx.precolored_vregs;
  let rec iter () =
    liveness_analysis ~gcx;
    build_interference_graph ~gcx;
    make_worklist ~gcx;
    while
      not
        ( ISet.is_empty gcx.simplify_worklist
        && ISet.is_empty gcx.worklist_moves
        && ISet.is_empty gcx.freeze_worklist
        && ISet.is_empty gcx.spill_worklist )
    do
      if not (ISet.is_empty gcx.simplify_worklist) then
        simplify ~gcx
      else if not (ISet.is_empty gcx.worklist_moves) then
        coalesce ~gcx
      else if not (ISet.is_empty gcx.freeze_worklist) then
        freeze ~gcx
      else if not (ISet.is_empty gcx.spill_worklist) then
        select_spill ~gcx
    done;
    assign_colors ~gcx;
    if not (ISet.is_empty gcx.spilled_vregs) then (
      rewrite_program ~gcx;
      iter ()
    )
  in
  iter ();
  remove_coalesced_moves ~gcx;
  Gcx.compress_jump_aliases ~gcx;
  Gcx.remove_redundant_instructions ~gcx;
  write_function_prologues ~gcx;
  write_function_epilogues ~gcx
