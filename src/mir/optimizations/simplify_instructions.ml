open Mir
open Mir_builders

type cx = { (* Instructions that could potentially be simplified *)
            mutable worklist: VSet.t }

let simplify_instruction ~cx instr_value =
  let instr = cast_to_instruction instr_value in

  let replace_instr_enqueue_uses new_instr_value =
    value_iter_uses ~value:instr_value (fun use -> cx.worklist <- VSet.add use.user cx.worklist);
    insert_instruction_before ~before:instr_value new_instr_value;
    replace_instruction ~from:instr_value ~to_:new_instr_value
  in

  match instr.instr with
  (* Comparison followed by not can be changed to inverted comparison *)
  | Unary (Not, operand) when is_single_use operand && type_of_use operand == Bool ->
    (match operand.value.value with
    | Instr { instr = Cmp (comparison, op1, op2); _ } ->
      let inverted_comparison = invert_comparison comparison in
      let new_instr = mk_blockless_cmp ~cmp:inverted_comparison ~left:op1.value ~right:op2.value in
      remove_instruction operand.value;
      replace_instr_enqueue_uses new_instr
    | _ -> ())
  | _ -> ()

let run ~program =
  let cx = { worklist = VSet.empty } in
  (* Initial pass visits all instructions, enqueuing dependent uses to recheck *)
  program_iter_blocks program (fun block ->
      iter_instructions block (fun instr_value _ -> simplify_instruction ~cx instr_value));
  (* Keep checking for simplification and enqueuing depdent uses until no possible changes are left *)
  while not (VSet.is_empty cx.worklist) do
    let instr_value = VSet.choose cx.worklist in
    cx.worklist <- VSet.remove instr_value cx.worklist;
    simplify_instruction ~cx instr_value
  done
