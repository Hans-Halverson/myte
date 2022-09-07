open Mir
open Mir_builders

let is_phi_dead (phi_value : Value.t) =
  let has_only_self_uses = ref true in
  value_iter_uses ~value:phi_value (fun use ->
      has_only_self_uses := !has_only_self_uses && use.user == phi_value);
  !has_only_self_uses

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

let visit_instruction ~worklist instr_value =
  if is_dead_instruction instr_value then (
    let instr = cast_to_instruction instr_value in
    (* Remove instruction and enqueue instruction operands as they may now be unused. Be sure not
       to reenqueue the instruction if it is a phi node with itself in its args. *)
    instruction_iter_operands ~instr (fun operand ->
        match operand.value.value with
        | Instr _ when operand.value != instr_value -> worklist := VSet.add operand.value !worklist
        | _ -> ());
    remove_instruction instr_value
  )

let run_worklist worklist =
  (* Keep checking for simplification and enqueuing depdent uses until no possible changes are left *)
  while not (VSet.is_empty !worklist) do
    let instr_value = VSet.choose !worklist in
    visit_instruction ~worklist instr_value;
    worklist := VSet.remove instr_value !worklist
  done

let run ~program =
  let worklist = ref VSet.empty in
  (* Initial pass visits all instructions, enqueuing dependent uses to recheck *)
  program_iter_blocks program (fun block ->
      iter_instructions block (fun instr_value _ -> visit_instruction ~worklist instr_value));
  run_worklist worklist
