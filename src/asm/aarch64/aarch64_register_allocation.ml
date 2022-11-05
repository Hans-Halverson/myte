open Aarch64_builders
open Aarch64_gen_context
open Aarch64_register
open Asm
open Asm_register
open Asm_register_allocation
module Register = Aarch64_register.Register

let allocatable_general_purpose_registers =
  RegSet.diff general_purpose_registers (RegSet.of_list [`R18; `R30])

let num_allocatable_general_purpose_registers =
  RegSet.cardinal allocatable_general_purpose_registers - 1

let num_allocatable_vector_registers = RegSet.cardinal vector_registers - 1

class use_def_collector color_to_op =
  object
    inherit Aarch64_liveness_analysis.use_def_visitor color_to_op
    inherit Asm_register_allocation.use_def_collector
  end

module AArch64RegisterAllocatorContext = struct
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
    | VectorClass -> vector_registers

  let num_allocatable_registers reg_class =
    match reg_class with
    | Register.GeneralClass -> num_allocatable_general_purpose_registers
    | VectorClass -> num_allocatable_vector_registers

  let get_reg_order reg = get_reg_order reg

  (* Operand functions *)

  let get_class op =
    match op.Operand.type_ with
    | Double -> Register.VectorClass
    | _ -> GeneralClass

  (* Instruction functions *)

  let instr_iter_operands instr f = instr_iter_all_operands instr f

  let get_move_opt (instr : Instruction.t) =
    match instr with
    (* SP and ZR are do not count as moves to prevent them from being coalesced and placed in an
       instruction that does not allow them. *)
    | { instr = `MovR _; operands = [| { value = PhysicalRegister (`ZR | `SP); _ }; _ |]; _ }
    | { instr = `MovR _; operands = [| _; { value = PhysicalRegister (`ZR | `SP); _ } |]; _ } ->
      None
    | { instr = `MovR _; operands = [| dest_op; src_op |]; _ }
      when Operand.is_reg_value src_op
           && Operand.is_reg_value dest_op
           && get_class src_op == get_class dest_op ->
      Some (src_op, dest_op)
    | _ -> None

  (* Register allocation lifecycle *)

  let get_live_out_regs cx =
    let liveness_analyzer =
      new Aarch64_liveness_analysis.regs_liveness_analyzer cx.func cx.gcx.color_to_op
    in
    let (_, live_out) = liveness_analyzer#analyze () in
    live_out

  let get_use_defs_for_instruction cx instr _ = cx.find_use_defs instr

  let rewrite_spilled_program cx ~get_alias =
    (* TODO: Rewrite program to account for spills *)
    ignore (cx, get_alias)
end

module _ : REGISTER_ALLOCATOR_CONTEXT = AArch64RegisterAllocatorContext

module AArch64RegisterAllocator = RegisterAllocator (AArch64RegisterAllocatorContext)

let run ~gcx ~func =
  let cx = AArch64RegisterAllocatorContext.mk ~gcx ~func in
  let register_allocator =
    AArch64RegisterAllocator.mk ~cx ~func ~representative_precolored:gcx.color_to_op
  in
  AArch64RegisterAllocator.allocate_registers ~ra:register_allocator
