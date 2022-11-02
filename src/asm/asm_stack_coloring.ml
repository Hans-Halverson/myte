open Asm
open Asm_builders
open Asm_instruction_definition
open Basic_collections

class virtual stack_coloring =
  object (this)
    (* Apply a function to all operands of an instruction *)
    method virtual iter_operands : Instruction.t -> (Operand.t -> OperandDef.t -> unit) -> unit

    (* Return the live out set for the blocks of this function *)
    method virtual calculate_live_out : Function.t -> Operand.t list BlockMap.t

    method liveness_analysis (func : Function.t) =
      let find_vslot_use_defs (instr : Instruction.t) =
        let uses = ref OperandSet.empty in
        let defs = ref OperandSet.empty in

        this#iter_operands instr (fun operand operand_def ->
            match operand.value with
            | VirtualStackSlot ->
              if operand_is_use operand_def then uses := OperandSet.add operand !uses;
              if operand_is_def operand_def then defs := OperandSet.add operand !defs
            | _ -> ());

        (!uses, !defs)
      in

      let live_out = this#calculate_live_out func in
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
                      if
                        (not (OOMMap.contains live_vslot vslot_def !graph))
                        && live_vslot != vslot_def
                      then (
                        graph := OOMMap.add live_vslot vslot_def !graph;
                        graph := OOMMap.add vslot_def live_vslot !graph
                      ))
                    !live)
                vslot_defs;
              live := OperandSet.union vslot_uses (OperandSet.diff !live vslot_defs)));
      !graph

    method run (func : Function.t) =
      let interference_graph = this#liveness_analysis func in

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
          let stack_slot_index =
            match color_opt with
            | Some color ->
              color_to_stack_slots :=
                IMap.add
                  color
                  (OperandSet.add vslot (IMap.find color !color_to_stack_slots))
                  !color_to_stack_slots;
              color
            | None ->
              let stack_slot_index = !max_stack_slot in
              color_to_stack_slots :=
                IMap.add stack_slot_index (OperandSet.singleton vslot) !color_to_stack_slots;
              max_stack_slot := stack_slot_index + 1;
              stack_slot_index
          in
          vslot.value <- StackSlot (func.num_argument_stack_slots + stack_slot_index))
        func.spilled_vslots;

      (* Calculate total size of stack frame and its sections *)
      let num_local_stack_slots = !max_stack_slot in
      func.num_stack_frame_slots <- func.num_argument_stack_slots + num_local_stack_slots
  end
