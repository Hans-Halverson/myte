open Asm
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

class use_def_collector color_to_op =
  object
    inherit X86_64_liveness_analysis.use_def_visitor color_to_op
    inherit Asm_register_allocation.use_def_collector
  end

module X86_64_RegisterAllocatorContext = struct
  type register_class = Register.class_

  type t = {
    func: Function.t;
    gcx: Gcx.t;
    find_use_defs: Instruction.t -> OperandSet.t * OperandSet.t;
  }

  let mk ~(gcx : Gcx.t) ~(func : Function.t) =
    let use_def_collector = new use_def_collector gcx.color_to_op in
    { func; gcx; find_use_defs = use_def_collector#find_use_defs }

  (* Register functions *)

  let allocatable_registers reg_class =
    match reg_class with
    | Register.GeneralClass -> allocatable_general_purpose_registers
    | SSEClass -> all_sse_registers

  let num_allocatable_registers reg_class =
    match reg_class with
    | Register.GeneralClass -> num_allocatable_general_purpose_registers
    | SSEClass -> num_allocatable_sse_registers

  let get_reg_order reg = get_reg_order reg

  (* Operand functions *)

  let get_class op =
    match op.Operand.type_ with
    | Double -> Register.SSEClass
    | _ -> GeneralClass

  (* Instruction functions *)

  let instr_iter_operands instr f = instr_iter_reg_mem_operands instr f

  let get_move_opt (instr : Instruction.t) =
    match instr with
    | { instr = `MovMM _; operands = [| src_op; dest_op |]; _ }
      when Operand.is_reg_value src_op
           && Operand.is_reg_value dest_op
           && get_class src_op == get_class dest_op ->
      Some (src_op, dest_op)
    | _ -> None

  (* Register allocation lifecycle *)

  let get_live_out_regs cx =
    let liveness_analyzer =
      new X86_64_liveness_analysis.regs_liveness_analyzer cx.func cx.gcx.color_to_op
    in
    let (_, live_out) = liveness_analyzer#analyze () in
    live_out

  let get_use_defs_for_instruction cx instr _ = cx.find_use_defs instr

  let rewrite_spilled_program cx ~get_alias =
    let spill_writer = new X86_64_spill_writer.spill_writer ~get_alias in
    List.iter (fun block -> spill_writer#write_block_spills block) cx.func.blocks
end

module _ : REGISTER_ALLOCATOR_CONTEXT = X86_64_RegisterAllocatorContext

module X86_64_RegisterAllocator = RegisterAllocator (X86_64_RegisterAllocatorContext)

let run ~gcx ~func =
  let cx = X86_64_RegisterAllocatorContext.mk ~gcx ~func in
  let register_allocator =
    X86_64_RegisterAllocator.mk ~cx ~func ~representative_precolored:gcx.color_to_op
  in
  X86_64_RegisterAllocator.allocate_registers ~ra:register_allocator
