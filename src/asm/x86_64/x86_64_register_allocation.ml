open Asm
open Asm_builders
open Asm_register
open Asm_register_allocation
open X86_64_builders
open X86_64_gen_context
open X86_64_register

let allocatable_general_purpose_registers =
  RegSet.of_list [`A; `B; `C; `D; `SI; `DI; `R8; `R9; `R10; `R11; `R12; `R13; `R14; `R15]

let num_allocatable_general_purpose_registers =
  RegSet.cardinal allocatable_general_purpose_registers - 1

let num_allocatable_sse_registers = RegSet.cardinal all_sse_registers - 1

module X86_64_RegisterAllocatorContext = struct
  type register_class = Register.class_

  type t = {
    func: Function.t;
    gcx: Gcx.t;
    find_use_defs: Instruction.t -> OperandSet.t * OperandSet.t;
  }

  class use_def_finder color_to_op =
    object (this)
      inherit X86_64_liveness_analysis.use_def_finder color_to_op

      val mutable reg_uses : OperandSet.t = OperandSet.empty

      val mutable reg_defs : OperandSet.t = OperandSet.empty

      method find_use_defs instr =
        reg_uses <- OperandSet.empty;
        reg_defs <- OperandSet.empty;
        this#visit_instruction instr;
        (reg_uses, reg_defs)

      method! add_register_use ~instr:_ (reg : Operand.t) = reg_uses <- OperandSet.add reg reg_uses

      method! add_register_def ~instr:_ (reg : Operand.t) = reg_defs <- OperandSet.add reg reg_defs
    end

  let mk ~gcx ~func =
    let use_def_finder = new use_def_finder gcx.Gcx.color_to_op in
    { func; gcx; find_use_defs = use_def_finder#find_use_defs }

  (* Calling conventions *)

  let allocatable_registers reg_class =
    match reg_class with
    | Register.GeneralClass -> allocatable_general_purpose_registers
    | SSEClass -> all_sse_registers

  let callee_saved_registers = callee_saved_registers

  let num_allocatable_registers reg_class =
    match reg_class with
    | Register.GeneralClass -> num_allocatable_general_purpose_registers
    | SSEClass -> num_allocatable_sse_registers

  let get_rep_physical_registers cx = cx.gcx.color_to_op

  let get_spilled_callee_saved_registers cx = cx.func.spilled_callee_saved_regs

  let add_spilled_callee_saved_register cx reg =
    cx.func.spilled_callee_saved_regs <- RegSet.add reg cx.func.spilled_callee_saved_regs

  (* Operand functions *)

  let is_precolored op =
    match op.Operand.value with
    | PhysicalRegister _ -> true
    | _ -> false

  let get_rep_register cx op =
    match op.Operand.value with
    | PhysicalRegister reg -> RegMap.find reg cx.gcx.color_to_op
    | VirtualRegister -> op
    | _ -> failwith "Expected register"

  let get_physical_register_opt op =
    match op.Operand.value with
    | PhysicalRegister reg -> Some reg
    | _ -> None

  let assign_physical_register op reg = op.Operand.value <- PhysicalRegister reg

  let get_class op =
    match op.Operand.type_ with
    | Double -> Register.SSEClass
    | _ -> GeneralClass

  (* Instruction functions *)

  let iter_blocks f cx = List.iter f cx.func.blocks

  let iter_instrs_rev f block = iter_instructions_rev block f

  let filter_instrs f block = filter_instructions block f

  let get_move_opt (instr : Instruction.t) =
    match instr with
    | { instr = `MovMM _; operands = [| src_op; dest_op |]; _ }
      when Operand.is_reg_value src_op
           && Operand.is_reg_value dest_op
           && get_class src_op == get_class dest_op ->
      Some (src_op, dest_op)
    | _ -> None

  (* Register allocation lifecycle *)

  class init_visitor ~cx =
    object
      val mutable reg_num_use_defs = OperandMap.empty

      val mutable initial_vregs = OperandSet.empty

      method reg_num_use_defs = reg_num_use_defs

      method initial_vregs = initial_vregs

      method visit_instruction (instr : Instruction.t) =
        instr_iter_reg_mem_operands instr (fun operand _ ->
            if Operand.is_reg_value operand then (
              (match operand.value with
              | VirtualRegister -> initial_vregs <- OperandSet.add operand initial_vregs
              | _ -> ());
              let reg = get_rep_register cx operand in
              reg_num_use_defs <-
                OperandMap.add
                  reg
                  (match OperandMap.find_opt reg reg_num_use_defs with
                  | None -> 1
                  | Some prev_count -> prev_count + 1)
                  reg_num_use_defs
            ))
    end

  let init_context cx =
    (* Collect all registers in program, splitting into precolored and other initial vregs *)
    let init_visitor = new init_visitor ~cx in
    List.iter (fun block -> iter_instructions block init_visitor#visit_instruction) cx.func.blocks;
    (init_visitor#reg_num_use_defs, init_visitor#initial_vregs)

  let get_live_out_regs cx =
    let liveness_analyzer =
      new X86_64_liveness_analysis.regs_liveness_analyzer cx.func cx.gcx.color_to_op
    in
    let (_, live_out) = liveness_analyzer#analyze () in
    live_out

  let get_use_defs_for_instruction cx instr _ = cx.find_use_defs instr

  let choose_register_from_possible possible_regs =
    let min_reg =
      RegSet.fold
        (fun reg min_reg ->
          let reg_order = get_reg_order reg in
          match min_reg with
          | None -> Some (reg, reg_order)
          | Some (_, min_reg_order) ->
            if reg_order < min_reg_order then
              Some (reg, reg_order)
            else
              min_reg)
        possible_regs
        None
    in
    match min_reg with
    | None -> None
    | Some (reg, _) -> Some reg

  let spill_virtual_register cx vreg =
    cx.func.spilled_vslots <- Asm.OperandSet.add vreg cx.func.spilled_vslots;
    vreg.value <- VirtualStackSlot

  let rewrite_spilled_program cx ~get_alias =
    let spill_writer = new X86_64_spill_writer.spill_writer ~get_alias in
    List.iter (fun block -> spill_writer#write_block_spills block) cx.func.blocks
end

module _ : REGISTER_ALLOCATOR_CONTEXT = X86_64_RegisterAllocatorContext

module X86_64_RegisterAllocator = RegisterAllocator (X86_64_RegisterAllocatorContext)

let run ~gcx ~func =
  let cx = X86_64_RegisterAllocatorContext.mk ~gcx ~func in
  let register_allocator = X86_64_RegisterAllocator.mk ~cx in
  X86_64_RegisterAllocator.allocate_registers ~ra:register_allocator
