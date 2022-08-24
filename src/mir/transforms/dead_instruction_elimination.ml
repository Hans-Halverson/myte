open Mir
open Mir_builders

let is_dead_instruction (instr_value : Value.t) =
  if value_has_uses instr_value then
    false
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
    (* Remove instruction and enqueue instruction operands as they may now be unused *)
    instruction_iter_operands ~instr (fun operand ->
        match operand.value.value with
        | Instr _ -> worklist := VSet.add operand.value !worklist
        | _ -> ());
    remove_instruction instr_value
  )

let run ~program =
  let worklist = ref VSet.empty in
  (* Initial pass visits all instructions, enqueuing dependent uses to recheck *)
  program_iter_blocks program (fun block ->
      iter_instructions block (fun instr_value _ -> visit_instruction ~worklist instr_value));

  (* Keep checking for simplification and enqueuing depdent uses until no possible changes are left *)
  while not (VSet.is_empty !worklist) do
    let instr_value = VSet.choose !worklist in
    visit_instruction ~worklist instr_value;
    worklist := VSet.remove instr_value !worklist
  done
