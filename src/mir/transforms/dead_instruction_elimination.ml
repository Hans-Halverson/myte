open Mir
open Mir_builders

type cx = {
  (* Set of all instructions to check for liveness *)
  mutable worklist: VSet.t;
  (* Set of all instructions that have been removed so far *)
  mutable removed: VSet.t;
}

let mk_cx ~worklist = { worklist; removed = VSet.empty }

(* Phis are dead if they have no uses or if they are part of a useless phi cycle. A useless phi
   cycle is when all phis in the entire phi web are only used by other phi nodes. *)
let is_phi_dead (phi_value : Value.t) =
  let has_non_phi_use = ref false in
  let visited_phis = ref VSet.empty in

  let rec search_for_non_phi_uses phi_value =
    if (not !has_non_phi_use) && not (VSet.mem phi_value !visited_phis) then (
      visited_phis := VSet.add phi_value !visited_phis;
      value_iter_uses ~value:phi_value (fun use ->
          match use.user.value with
          | Instr { instr = Phi _; _ } -> search_for_non_phi_uses use.user
          | _ -> has_non_phi_use := true)
    )
  in
  search_for_non_phi_uses phi_value;
  not !has_non_phi_use

let is_dead_instruction (instr_value : Value.t) =
  if value_has_uses instr_value then
    match instr_value.value with
    | Instr { instr = Phi _; _ } -> is_phi_dead instr_value
    | _ -> false
  else
    let instr = cast_to_instruction instr_value in
    match instr.instr with
    (* Set of instructions that either do not have direct uses or may have side effects *)
    | Call _
    | Store _
    | Ret _
    | Continue _
    | Branch _
    | Unreachable ->
      false
    | _ -> true

let visit_instruction ~(cx : cx) instr_value =
  if is_dead_instruction instr_value then (
    let instr = cast_to_instruction instr_value in
    (* Remove instruction and enqueue instruction operands as they may now be unused. Be sure not
       to reenqueue already deleted instructions, such as in the case of dead phi cycles. *)
    cx.removed <- VSet.add instr_value cx.removed;
    instruction_iter_operands ~instr (fun operand ->
        match operand.value.value with
        | Instr _ when not (VSet.mem operand.value cx.removed) ->
          cx.worklist <- VSet.add operand.value cx.worklist
        | _ -> ());
    remove_instruction instr_value
  )

let run_until_empty_worklist ~(cx : cx) =
  (* Keep checking for simplification and enqueuing dependent uses until no possible changes are left *)
  while not (VSet.is_empty cx.worklist) do
    let instr_value = VSet.choose cx.worklist in
    visit_instruction ~cx instr_value;
    cx.worklist <- VSet.remove instr_value cx.worklist
  done

let run_worklist worklist =
  let cx = mk_cx ~worklist in
  run_until_empty_worklist ~cx

let run ~program =
  let cx = mk_cx ~worklist:VSet.empty in
  (* Initial pass visits all instructions, enqueuing dependent uses to recheck *)
  program_iter_blocks program (fun block ->
      iter_instructions block (fun instr_value _ -> visit_instruction ~cx instr_value));
  run_until_empty_worklist ~cx
