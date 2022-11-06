open Aarch64_codegen
open Aarch64_gen_context
open Aarch64_register
open Asm
open Asm_builders
open Asm_instruction_definition
open Asm_register

let split_into_pairs list =
  let rec inner list acc =
    match list with
    | hd1 :: hd2 :: tl -> inner tl ([hd1; hd2] :: acc)
    | [hd] -> [hd] :: acc
    | [] -> acc
  in
  let rev_pairs = inner list [] in
  List.rev rev_pairs

let get_pair list =
  match list with
  | [hd1; hd2] -> (hd1, hd2)
  | _ -> failwith "Expected pair"

let mk_reg reg = mk_precolored ~type_:Long reg

let mk_sp () = mk_reg `SP

let mk_prepend_sequence_emitter before_instr : AArch64.instr -> Operand.t array -> unit =
  let prev_instr = ref before_instr in
  fun instr operands ->
    let prev = !prev_instr in
    let new_instr = mk_blockless_instr (instr :> instr) operands in
    if prev == before_instr then
      insert_instruction_before ~before:prev new_instr
    else
      insert_instruction_after ~after:prev new_instr;
    prev_instr := new_instr

let mk_append_sequence_emitter after_instr : AArch64.instr -> Operand.t array -> unit =
  let prev_instr = ref after_instr in
  fun instr operands ->
    let new_instr = mk_blockless_instr (instr :> instr) operands in
    insert_instruction_after ~after:!prev_instr new_instr;
    prev_instr := new_instr

let write_function_prologue_and_epilogue (func : Function.t) =
  (* Instructions appended to prologue block, one at a time *)
  let prologue = func.prologue in
  let prologue_original_first_instr = get_first_instr_opt prologue in
  let add_prologue_instr instr operands =
    let instr = mk_blockless_instr instr operands in
    match prologue_original_first_instr with
    | None -> append_instruction prologue instr
    | Some original_first_instr -> insert_instruction_before ~before:original_first_instr instr
  in

  (* Find all ret instructions in function *)
  let epilogue_start_instrs = ref [] in
  func_iter_blocks func (fun block ->
      iter_instructions block (fun instr ->
          if instr.instr == `Ret then epilogue_start_instrs := ref instr :: !epilogue_start_instrs));

  (* Instructions prepended in epilogue sections, one at a time *)
  let add_epilogue_instr (f : unit -> Instruction.t) =
    List.iter
      (fun start_instr ->
        let new_instr = f () in
        insert_instruction_before ~before:!start_instr new_instr;
        start_instr := new_instr)
      !epilogue_start_instrs
  in

  (* Split callee saved registers into pairs so that load/store pair instructions can be used *)
  let (general_callee_saved_reg_spills, vector_callee_saved_reg_spills) =
    RegSet.fold
      (fun reg (general_regs, vector_regs) ->
        match register_class reg with
        | GeneralClass -> (reg :: general_regs, vector_regs)
        | VectorClass -> (general_regs, reg :: vector_regs))
      func.spilled_callee_saved_regs
      ([], [])
  in
  let general_callee_saved_reg_spills =
    if func.is_leaf then
      general_callee_saved_reg_spills
    else
      `R30 :: general_callee_saved_reg_spills
  in
  let general_callee_saved_reg_spill_pairs = split_into_pairs general_callee_saved_reg_spills in
  let vector_callee_saved_reg_spill_pairs = split_into_pairs vector_callee_saved_reg_spills in
  let num_general_callee_saved_reg_spills = List.length general_callee_saved_reg_spills in
  let total_number_callee_saved_reg_spills =
    num_general_callee_saved_reg_spills + List.length vector_callee_saved_reg_spills
  in
  (* Round up to nearest power of 16 *)
  let reg_spill_size = (total_number_callee_saved_reg_spills + 1) / 2 * 16 in

  let add_spilled_reg_store_load reg_pair is_first offset =
    (* The very first load/store pair should update the stack pointer with pre/post index mode.
       All later load/store pairs use a postive offset from the already moved stack pointer. *)
    let (load_mode, store_mode, store_offset) =
      if is_first then
        (AArch64.PostIndex, AArch64.PreIndex, -offset)
      else
        (Offset, Offset, offset)
    in
    if List_utils.is_pair reg_pair then (
      let (reg2, reg1) = get_pair reg_pair in
      add_prologue_instr
        (`Stp (Size64, store_mode))
        [| mk_reg reg1; mk_reg reg2; mk_sp (); mk_imm16 ~n:store_offset |];
      add_epilogue_instr (fun _ ->
          mk_blockless_instr
            (`Ldp (Size64, load_mode))
            [| mk_reg reg1; mk_reg reg2; mk_sp (); mk_imm16 ~n:offset |])
    ) else
      let reg = List.hd reg_pair in
      add_prologue_instr
        (`StrI (X, store_mode))
        [| mk_reg reg; mk_sp (); mk_imm16 ~n:store_offset |];
      add_epilogue_instr (fun _ ->
          mk_blockless_instr
            (`LdrI (Size64, X, false, load_mode))
            [| mk_reg reg; mk_sp (); mk_imm16 ~n:offset |])
  in

  (* Write first general register load/store pair which updates stack pointer *)
  (match List_utils.hd_opt general_callee_saved_reg_spill_pairs with
  | None -> ()
  | Some reg_pair -> add_spilled_reg_store_load reg_pair true reg_spill_size);

  (* Write all remaining general register load store pairs *)
  List.iteri
    (fun i reg_pair ->
      let offset = (i + 1) * 16 in
      add_spilled_reg_store_load reg_pair false offset)
    (List_utils.tl_or_empty general_callee_saved_reg_spill_pairs);

  (* Write first vector register to update stack pointer if there are no spilled general registers *)
  let remaining_vector_callee_saved_reg_spill_pairs =
    match List_utils.hd_opt vector_callee_saved_reg_spill_pairs with
    | Some reg_pair when general_callee_saved_reg_spill_pairs == [] ->
      add_spilled_reg_store_load reg_pair true reg_spill_size;
      List_utils.tl_or_empty vector_callee_saved_reg_spill_pairs
    | _ -> vector_callee_saved_reg_spill_pairs
  in

  (* Write all vector register load store pairs *)
  let general_spill_offset = num_general_callee_saved_reg_spills * 8 in
  List.iteri
    (fun i reg_pair ->
      let offset = (i * 16) + general_spill_offset in
      add_spilled_reg_store_load reg_pair false offset)
    remaining_vector_callee_saved_reg_spill_pairs;

  (* Create stack frame for vslots and arguments by adjusting stack pointer *)
  let stack_frame_size = func.num_stack_frame_slots * 8 in
  if stack_frame_size != 0 then (
    let n = Int64.of_int stack_frame_size in
    let sp = mk_sp () in

    (* Create stack frame at end of the prologue *)
    let emit =
      match prologue_original_first_instr with
      | Some original_first_instr -> mk_prepend_sequence_emitter original_first_instr
      | None ->
        fun instr operands ->
          let instr = mk_blockless_instr (instr :> instr) operands in
          append_instruction prologue instr
    in
    gen_sub_n_ ~emit ~result_op:sp ~type_:Long sp n;

    (* Undo stack frame at start of each epilogue *)
    List.iter
      (fun start_instr ->
        let emit = mk_prepend_sequence_emitter !start_instr in
        gen_add_n_ ~emit ~result_op:sp ~type_:Long sp n)
      !epilogue_start_instrs
  );

  (* Full stack frame contains all spilled callee saved registers, vslots, and argument slots *)
  reg_spill_size + stack_frame_size

let write_stack_slots (func : Function.t) (stack_frame_size : int) =
  (* Get the offset in bytes from the stack pointer for this stack slot. Negative stack slot indices
     represent stack slots in the parent's stack frame. *)
  let get_stack_offset (stack_slot_op : Operand.t) (stack_frame_size : int) =
    let i = cast_to_stack_slot stack_slot_op in
    let offset =
      if i >= 0 then
        i * 8
      else
        let offset_in_parent_stack_frame = (-i - 1) * 8 in
        stack_frame_size + offset_in_parent_stack_frame
    in
    Int64.of_int offset
  in

  func_iter_blocks func (fun block ->
      iter_instructions block (fun instr ->
          match instr.instr with
          | `SpillUse (size, subregister_size, is_signed) ->
            let emit = mk_prepend_sequence_emitter instr in
            let result_op = instr.operands.(0) in
            let base_reg = mk_sp () in
            let offset = get_stack_offset instr.operands.(1) stack_frame_size in
            gen_ldr_offset_n_ ~emit ~result_op ~base_reg size subregister_size is_signed offset;
            remove_instruction instr
          | `SpillDef subregister_size ->
            let emit = mk_append_sequence_emitter instr in
            let arg_op = instr.operands.(0) in
            let base_reg = mk_sp () in
            let offset = get_stack_offset instr.operands.(1) stack_frame_size in
            gen_str_offset_n_ ~emit ~arg_op ~base_reg subregister_size offset;
            remove_instruction instr
          | _ -> ()))

let write_stack_frame ~(gcx : Gcx.t) =
  FunctionSet.iter
    (fun func ->
      let stack_frame_size = write_function_prologue_and_epilogue func in
      write_stack_slots func stack_frame_size)
    gcx.funcs
