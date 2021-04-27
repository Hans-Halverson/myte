open Basic_collections
open X86_gen_context
open X86_instructions

class vslot_use_def_finder =
  object
    inherit X86_visitor.instruction_visitor

    val mutable vslot_uses = VRegSet.empty

    val mutable vslot_defs = VRegSet.empty

    method vslot_uses = vslot_uses

    method vslot_defs = vslot_defs

    method! visit_read_mem ~block:_ mem =
      let open Instruction in
      match mem with
      | Mem (VirtualStackSlot vreg) -> vslot_uses <- VRegSet.add vreg vslot_uses
      | _ -> ()

    method! visit_write_mem ~block:_ mem =
      let open Instruction in
      match mem with
      | Mem (VirtualStackSlot vreg) -> vslot_defs <- VRegSet.add vreg vslot_defs
      | _ -> ()
  end

let find_vslot_use_defs block instruction =
  let finder = new vslot_use_def_finder in
  finder#visit_instruction ~block instruction;
  (finder#vslot_uses, finder#vslot_defs)

let liveness_analysis ~(gcx : Gcx.t) =
  let (_, live_out) = X86_liveness_analysis.analyze_virtual_stack_slots ~gcx in
  let graph = ref VRegMap.empty in
  IMap.iter
    (fun block_id block ->
      let live = ref (IMap.find block_id live_out |> VRegSet.of_list) in
      List.iter
        (fun instr ->
          let (vslot_uses, vslot_defs) = find_vslot_use_defs block instr in
          live := VRegSet.union vslot_defs !live;
          VRegSet.iter
            (fun vslot_def ->
              VRegSet.iter
                (fun live_vslot ->
                  if (not (VVMMap.contains live_vslot vslot_def !graph)) && live_vslot != vslot_def
                  then (
                    graph := VVMMap.add live_vslot vslot_def !graph;
                    graph := VVMMap.add vslot_def live_vslot !graph
                  ))
                !live)
            vslot_defs;
          live := VRegSet.union vslot_uses (VRegSet.diff !live vslot_defs))
        (List.rev block.Block.instructions))
    gcx.blocks_by_id;
  !graph

let allocate_stack_slots ~(gcx : Gcx.t) =
  let interference_graph = liveness_analysis ~gcx in

  (* VRegMap.iter (fun vreg vregs -> Printf.printf "%d -> %s\n" vreg.id (string_of_vset vregs)) interference_graph; *)
  IMap.iter
    (fun _ func ->
      let open Function in
      (* Virtual stack slot colors correspond to shared slots on the stack *)
      let max_stack_slot = ref 0 in
      let color_to_stack_slots = ref IMap.empty in

      (* Color each virtual stack slot in function *)
      VRegSet.iter
        (fun vslot ->
          (* Find the first non-interfering color *)
          let color_opt =
            IMap.fold
              (fun color stack_slots found_color ->
                match found_color with
                | Some _ -> found_color
                | None ->
                  if
                    VRegSet.for_all
                      (fun stack_slot -> not (VVMMap.contains vslot stack_slot interference_graph))
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
                (VRegSet.add vslot (IMap.find color !color_to_stack_slots))
                !color_to_stack_slots
          | None ->
            color_to_stack_slots :=
              IMap.add !max_stack_slot (VRegSet.singleton vslot) !color_to_stack_slots;
            max_stack_slot := !max_stack_slot + 1)
        func.spilled_vregs;

      func.num_stack_frame_slots <- !max_stack_slot;
      let stack_frame_size = func.num_stack_frame_slots * 8 in

      (* Write physical addresses in stack for each vslot in function *)
      IMap.iter
        (fun color vslots ->
          VRegSet.iter
            (fun vslot ->
              let offset = stack_frame_size - (color * 8) in
              vslot.resolution <-
                StackSlot
                  (PhysicalAddress
                     {
                       base = Some (Gcx.mk_precolored ~gcx SP);
                       offset = Some (ImmediateOffset (Int64.of_int offset));
                       index_and_scale = None;
                     }))
            vslots)
        !color_to_stack_slots;

      (* Write physical addresses for each param passed on stack now that stack frame size is known *)
      let args_on_stack = List_utils.drop 6 func.params in
      let num_used_callee_saved_regs = RegSet.cardinal func.spilled_callee_saved_regs in
      List.iteri
        (fun i param_vreg ->
          let open VReg in
          (* Offset must reach past entire stack frame, then all used callee saved registers that were
             pushed on stack, then return address pushed onto stack from call instruction, and then
             finally can start accessing function arguments that were pushed onto stack. *)
          let offset = stack_frame_size + ((num_used_callee_saved_regs + i + 1) * 8) in
          param_vreg.resolution <-
            StackSlot
              (PhysicalAddress
                 {
                   base = Some (Gcx.mk_precolored ~gcx SP);
                   offset = Some (ImmediateOffset (Int64.of_int offset));
                   index_and_scale = None;
                 }))
        args_on_stack)
    gcx.funcs_by_id
