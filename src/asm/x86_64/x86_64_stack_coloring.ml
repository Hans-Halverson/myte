open Basic_collections
open X86_64_gen_context
open X86_64_instructions

class vslot_use_def_finder =
  object
    inherit X86_64_visitor.instruction_visitor as super

    val mutable vslot_uses = OperandSet.empty

    val mutable vslot_defs = OperandSet.empty

    method vslot_uses = vslot_uses

    method vslot_defs = vslot_defs

    method! visit_read_operand ~block op =
      match op.value with
      | VirtualStackSlot -> vslot_uses <- OperandSet.add op vslot_uses
      | _ -> super#visit_read_operand ~block op

    method! visit_write_operand ~block op =
      match op.value with
      | VirtualStackSlot -> vslot_defs <- OperandSet.add op vslot_defs
      | _ -> super#visit_write_operand ~block op
  end

let find_vslot_use_defs block instruction =
  let finder = new vslot_use_def_finder in
  finder#visit_instruction ~block instruction;
  (finder#vslot_uses, finder#vslot_defs)

let liveness_analysis ~(gcx : Gcx.t) =
  let (_, live_out) = X86_64_liveness_analysis.analyze_virtual_stack_slots ~gcx in
  let graph = ref OperandMap.empty in
  IMap.iter
    (fun block_id block ->
      let live = ref (IMap.find block_id live_out |> OperandSet.of_list) in
      List.iter
        (fun instr ->
          let (vslot_uses, vslot_defs) = find_vslot_use_defs block instr in
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
          live := OperandSet.union vslot_uses (OperandSet.diff !live vslot_defs))
        (List.rev block.Block.instructions))
    gcx.blocks_by_id;
  !graph

let resolve_to_physical_stack_slot ~gcx operand offset =
  operand.Operand.value <-
    MemoryAddress
      {
        base = RegBase (Gcx.mk_precolored ~gcx SP);
        offset = Some (ImmediateOffset (Int32.of_int offset));
        index_and_scale = None;
      }

let allocate_stack_slots ~(gcx : Gcx.t) =
  let interference_graph = liveness_analysis ~gcx in

  IMap.iter
    (fun _ func ->
      let open Function in
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
          resolve_to_physical_stack_slot ~gcx stack_slot_op (i * 8))
        func.argument_stack_slots;

      (* Write physical addresses in stack for each vslot in function *)
      IMap.iter
        (fun color vslots ->
          OperandSet.iter
            (fun vslot ->
              let offset = (color * 8) + arguments_stack_frame_section_size in
              resolve_to_physical_stack_slot ~gcx vslot offset)
            vslots)
        !color_to_stack_slots;

      (* Write physical addresses for each param passed on stack now that stack frame size is known *)
      let args_on_stack = List_utils.drop 6 func.params in
      let num_used_callee_saved_regs = RegSet.cardinal func.spilled_callee_saved_regs in
      List.iteri
        (fun i param_op ->
          (* Offset must reach past entire stack frame, then all used callee saved registers that were
             pushed on stack, then return address pushed onto stack from call instruction, and then
             finally can start accessing function arguments that were pushed onto stack. *)
          let offset = stack_frame_size + ((num_used_callee_saved_regs + i + 1) * 8) in
          resolve_to_physical_stack_slot ~gcx param_op offset)
        args_on_stack)
    gcx.funcs_by_id
