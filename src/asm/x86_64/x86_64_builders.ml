open X86_64_instructions

(*
 * ============================
 *          Operands
 * ============================
 *)

let mk_operand ~(value : Operand.value) : Operand.t = Operand.mk ~id:(Mir.mk_value_id ()) ~value

let mk_precolored (color : register_slot) : Operand.t = mk_operand ~value:(PhysicalRegister color)

let mk_virtual_register () : Operand.t = mk_operand ~value:VirtualRegister

let mk_virtual_register_of_value_id ~(value_id : Mir.Value.id) : Operand.t =
  Operand.of_value_id ~value:VirtualRegister value_id

let mk_memory_address ~(address : MemoryAddress.t) : Operand.t =
  mk_operand ~value:(MemoryAddress address)

let mk_function_argument_stack_slot ~(i : int) : Operand.t =
  mk_operand ~value:(FunctionArgumentStackSlot i)

let mk_function_stack_argument ~(arg_id : int) : Operand.t =
  Operand.of_value_id ~value:FunctionStackArgument arg_id
