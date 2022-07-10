open Mir_type
open X86_64_instructions

(*
 * ============================
 *          Operands
 * ============================
 *)

let mk_operand ~(value : Operand.value) ~(type_ : Type.t) : Operand.t =
  Operand.mk ~id:(Mir.mk_value_id ()) ~value ~type_

let mk_precolored ~(type_ : Type.t) (color : register_slot) : Operand.t =
  mk_operand ~value:(PhysicalRegister color) ~type_

let mk_precolored_of_operand (color : register_slot) (op : Operand.t) : Operand.t =
  mk_precolored ~type_:op.type_ color

let mk_virtual_register ~(type_ : Type.t) : Operand.t = mk_operand ~value:VirtualRegister ~type_

let mk_virtual_register_of_value_id ~(value_id : Mir.Value.id) ~(type_ : Type.t) : Operand.t =
  Operand.of_value_id ~value:VirtualRegister ~type_ value_id

let mk_memory_address ~(address : MemoryAddress.t) ~(type_ : Type.t) : Operand.t =
  mk_operand ~value:(MemoryAddress address) ~type_

let mk_function_argument_stack_slot ~(i : int) ~(type_ : Type.t) : Operand.t =
  mk_operand ~value:(FunctionArgumentStackSlot i) ~type_

let mk_function_stack_argument ~(arg_id : int) ~(type_ : Type.t) : Operand.t =
  Operand.of_value_id ~value:FunctionStackArgument arg_id ~type_

(*
 * ============================
 *         Instructions
 * ============================
 *)

let mk_instr instr =
  let id = Instruction.mk_id () in
  let instruction = { Instruction.id; instr } in
  instruction
