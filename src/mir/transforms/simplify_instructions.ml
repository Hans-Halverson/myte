open Mir
open Mir_builders
open Mir_type

type cx = { (* Instructions that could potentially be simplified *)
            mutable worklist: VSet.t }

let enqueue_instr ~cx instr_value = cx.worklist <- VSet.add instr_value cx.worklist

let dequeue_instr ~cx =
  let instr_value = VSet.choose cx.worklist in
  cx.worklist <- VSet.remove instr_value cx.worklist;
  instr_value

let simplify_instruction ~cx instr_value =
  let instr = cast_to_instruction instr_value in

  let enqueue_operands_and_uses instr_value =
    value_iter_uses ~value:instr_value (fun use -> enqueue_instr ~cx use.user);
    instruction_iter_operands ~instr (fun operand ->
        match operand.value.value with
        | Instr _ -> enqueue_instr ~cx operand.value
        | _ -> ())
  in

  let replace_instr_enqueue_uses new_instr_value =
    enqueue_operands_and_uses instr_value;
    insert_instruction_before ~before:instr_value new_instr_value;
    replace_instruction ~from:instr_value ~to_:new_instr_value
  in

  let replace_instr_with_value_enqueue_uses new_value =
    enqueue_operands_and_uses instr_value;
    replace_instruction ~from:instr_value ~to_:new_value
  in

  match instr.instr with
  (* Simplify phis where all arguments have the same value *)
  | Phi phi ->
    (match phi_get_single_arg_value phi with
    | None -> ()
    | Some arg_value -> replace_instr_with_value_enqueue_uses arg_value)
  (* Comparison followed by not can be changed to inverted comparison *)
  | Unary (Not, operand) ->
    (match operand.value.value with
    | Instr { instr = Cmp (comparison, op1, op2); _ } ->
      let inverted_comparison = invert_comparison comparison in
      let new_instr = mk_blockless_cmp ~cmp:inverted_comparison ~left:op1.value ~right:op2.value in
      replace_instr_enqueue_uses new_instr
    | _ -> ())
  | Trunc trunc_arg ->
    (match trunc_arg.value.value with
    (* Trunc of a Trunc is combined to a single Trunc to final size *)
    | Instr { instr = Trunc original_arg; _ } ->
      let new_trunc_instr = mk_blockless_trunc ~arg:original_arg.value ~type_:instr.type_ in
      replace_instr_enqueue_uses new_trunc_instr
    (* Trunc of an extension may either cancel or shorten extension *)
    | Instr { instr = (ZExt ext_arg | SExt ext_arg) as ext_instr; _ } ->
      let original_type = type_of_use ext_arg in
      let original_size = size_of_type original_type in
      let intermediate_type = type_of_use trunc_arg in
      let intermediate_size = size_of_type intermediate_type in
      let final_type = instr.type_ in
      let final_size = size_of_type final_type in
      (* If truncated back to original size then replace trunc with original value *)
      if original_size == final_size then
        replace_instr_with_value_enqueue_uses ext_arg.value
      (* If truncated below original size then ignore extension and trunc original value *)
      else if original_size > final_size then
        let new_trunc_instr = mk_blockless_trunc ~arg:ext_arg.value ~type_:final_type in
        replace_instr_enqueue_uses new_trunc_instr
      (* If truncated below extended size, shorten extension to the truncated size *)
      else if intermediate_size > final_size then
        let new_ext_instr =
          match ext_instr with
          | ZExt _ -> mk_blockless_zext ~arg:ext_arg.value ~type_:final_type
          | SExt _ -> mk_blockless_sext ~arg:ext_arg.value ~type_:final_type
          | _ -> failwith "Expected extension instruction"
        in
        replace_instr_enqueue_uses new_ext_instr
    | _ -> ())
  (* ZExt of a ZExt is condensed to a single ZExt to final size *)
  | ZExt zext_arg ->
    (match zext_arg.value.value with
    | Instr { instr = ZExt original_arg; _ } ->
      let new_trunc_instr = mk_blockless_zext ~arg:original_arg.value ~type_:instr.type_ in
      replace_instr_enqueue_uses new_trunc_instr
    | _ -> ())
  (* SExt of a SExt is condensed to a single SExt to final size *)
  | SExt sext_arg ->
    (match sext_arg.value.value with
    | Instr { instr = SExt original_arg; _ } ->
      let new_trunc_instr = mk_blockless_sext ~arg:original_arg.value ~type_:instr.type_ in
      replace_instr_enqueue_uses new_trunc_instr
    | _ -> ())
  | _ -> ()

let run_on_instruction ~cx instr_value =
  let instr = cast_to_instruction instr_value in
  if Dead_instruction_elimination.is_dead_instruction instr_value then (
    (* Remove instruction and enqueue instruction operands as they may now be unused *)
    instruction_iter_operands ~instr (fun operand ->
        match operand.value.value with
        | Instr _ -> enqueue_instr ~cx operand.value
        | _ -> ());
    remove_instruction instr_value
  ) else
    (* First try constant folding, otherwise try to simplify instruction *)
    match Fold_constants.try_fold_instruction instr with
    | Some constant ->
      let constant_value = mk_value (Lit constant) in
      value_iter_uses ~value:instr_value (fun use -> enqueue_instr ~cx use.user);
      replace_instruction ~from:instr_value ~to_:constant_value
    | None -> simplify_instruction ~cx instr_value

let run ~program =
  let cx = { worklist = VSet.empty } in
  (* Initial pass enqueues all instructions *)
  program_iter_blocks program (fun block ->
      iter_instructions block (fun instr_value _ -> enqueue_instr ~cx instr_value));
  (* Keep checking for simplification and enqueuing dependent uses until no possible changes are left *)
  while not (VSet.is_empty cx.worklist) do
    let instr_value = dequeue_instr ~cx in
    run_on_instruction ~cx instr_value
  done
