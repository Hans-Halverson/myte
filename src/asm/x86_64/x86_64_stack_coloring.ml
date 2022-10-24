open Asm
open Asm_builders
open Asm_instruction_definition
open Asm_register
open Basic_collections
open X86_64_builders
open X86_64_gen_context

let find_vslot_use_defs (instr : Instruction.t) =
  let uses = ref OperandSet.empty in
  let defs = ref OperandSet.empty in

  instr_iter_reg_mem_operands instr (fun operand operand_def ->
      match operand.value with
      | VirtualStackSlot ->
        if operand_is_use operand_def then uses := OperandSet.add operand !uses;
        if operand_is_def operand_def then defs := OperandSet.add operand !defs
      | _ -> ());

  (!uses, !defs)

let liveness_analysis func =
  let liveness_analyzer = new X86_64_liveness_analysis.vslots_liveness_analyzer func in
  let (_, live_out) = liveness_analyzer#analyze () in
  let graph = ref OOMMap.empty in
  func_iter_blocks func (fun block ->
      let live = ref (BlockMap.find block live_out |> OperandSet.of_list) in
      iter_instructions_rev block (fun instr ->
          let (vslot_uses, vslot_defs) = find_vslot_use_defs instr in
          live := OperandSet.union vslot_defs !live;
          OperandSet.iter
            (fun vslot_def ->
              OperandSet.iter
                (fun live_vslot ->
                  if (not (OOMMap.contains live_vslot vslot_def !graph)) && live_vslot != vslot_def
                  then (
                    graph := OOMMap.add live_vslot vslot_def !graph;
                    graph := OOMMap.add vslot_def live_vslot !graph
                  ))
                !live)
            vslot_defs;
          live := OperandSet.union vslot_uses (OperandSet.diff !live vslot_defs)));
  !graph

let resolve_to_physical_stack_slot operand offset =
  operand.Operand.value <-
    X86_64_MemoryAddress
      {
        base = RegBase (mk_precolored ~type_:Long `SP);
        offset = Some (ImmediateOffset (Int32.of_int offset));
        index_and_scale = None;
      }

let allocate_stack_slots ~(gcx : Gcx.t) =
  FunctionSet.iter
    (fun func ->
      let open Function in
      let interference_graph = liveness_analysis func in

      (* Virtual stack slot colors correspond to shared slots on the stack *)
      let max_stack_slot = ref 0 in
      let color_to_stack_slots = ref IMap.empty in

      (* Color each virtual stack slot in function *)
      OperandSet.iter
        (fun vslot ->
          (* Find the first non-interfering color *)
          let color_opt =
            IMap.fold
              (fun color stack_slots found_color ->
                match found_color with
                | Some _ -> found_color
                | None ->
                  if
                    OperandSet.for_all
                      (fun stack_slot -> not (OOMMap.contains vslot stack_slot interference_graph))
                      stack_slots
                  then
                    Some color
                  else
                    None)
              !color_to_stack_slots
              None
          in
          (* Color vslot. If no such non-interfering color exists, create new stack slot. *)
          match color_opt with
          | Some color ->
            color_to_stack_slots :=
              IMap.add
                color
                (OperandSet.add vslot (IMap.find color !color_to_stack_slots))
                !color_to_stack_slots
          | None ->
            color_to_stack_slots :=
              IMap.add !max_stack_slot (OperandSet.singleton vslot) !color_to_stack_slots;
            max_stack_slot := !max_stack_slot + 1)
        func.spilled_vslots;

      (* Calculate total size of stack frame and its sections *)
      let num_local_stack_slots = !max_stack_slot in
      let arguments_stack_frame_section_size = func.num_argument_stack_slots * 8 in

      func.num_stack_frame_slots <- func.num_argument_stack_slots + num_local_stack_slots;
      let stack_frame_size = func.num_stack_frame_slots * 8 in

      (* Write physical addresses in stack for each argument stack slot in function *)
      List.iter
        (fun stack_slot_op ->
          let i =
            match stack_slot_op.Operand.value with
            | FunctionArgumentStackSlot i -> i
            | _ -> failwith "Expected FunctionArgumentStackSlot"
          in
          resolve_to_physical_stack_slot stack_slot_op (i * 8))
        func.argument_stack_slots;

      (* Write physical addresses in stack for each vslot in function *)
      IMap.iter
        (fun color vslots ->
          OperandSet.iter
            (fun vslot ->
              let offset = (color * 8) + arguments_stack_frame_section_size in
              resolve_to_physical_stack_slot vslot offset)
            vslots)
        !color_to_stack_slots;

      (* Write physical addresses for each param passed on stack now that stack frame size is known *)
      let num_used_callee_saved_regs = RegSet.cardinal func.spilled_callee_saved_regs in
      List.iteri
        (fun i param_op ->
          let param_type = func.param_types.(i) in
          match param_type with
          | ParamInRegister _ -> ()
          | ParamOnStack i ->
            (* Offset must reach past entire stack frame, then all used callee saved registers that were
               pushed on stack, then return address pushed onto stack from call instruction, and then
               finally can start accessing function arguments that were pushed onto stack. *)
            let offset = stack_frame_size + ((num_used_callee_saved_regs + i + 1) * 8) in
            resolve_to_physical_stack_slot param_op offset)
        func.params)
    gcx.funcs
