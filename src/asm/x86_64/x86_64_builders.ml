open Mir_type
open X86_64_instructions
open X86_64_register

(*
 * ============================
 *          Operands
 * ============================
 *)

let mk_operand ~(value : Operand.value) ~(type_ : Type.t) : Operand.t =
  Operand.mk ~id:(Mir.mk_value_id ()) ~value ~type_

let mk_precolored ~(type_ : Type.t) (color : Register.t) : Operand.t =
  mk_operand ~value:(PhysicalRegister color) ~type_

let mk_precolored_of_operand (color : Register.t) (op : Operand.t) : Operand.t =
  mk_precolored ~type_:op.type_ color

let mk_virtual_register ~(type_ : Type.t) : Operand.t = mk_operand ~value:VirtualRegister ~type_

let mk_virtual_register_of_value_id ~(value_id : Mir.Value.id) ~(type_ : Type.t) : Operand.t =
  Operand.of_value_id ~value:VirtualRegister ~type_ value_id

let mk_memory_address ~(address : MemoryAddress.t) ~(type_ : Type.t) : Operand.t =
  mk_operand ~value:(MemoryAddress address) ~type_

let mk_imm ~(imm : immediate) : Operand.t =
  let type_ =
    match imm with
    | Imm8 _ -> Type.Byte
    | Imm16 _ -> Int
    | Imm32 _ -> Int
    | Imm64 _ -> Long
  in
  mk_operand ~value:(Immediate imm) ~type_

let mk_function_label ~(label : label) : Operand.t = mk_operand ~value:(Label label) ~type_:Function

let mk_block_op ~(block : Block.t) = mk_operand ~value:(Block block) ~type_:Long

let mk_function_argument_stack_slot ~(i : int) ~(type_ : Type.t) : Operand.t =
  mk_operand ~value:(FunctionArgumentStackSlot i) ~type_

let mk_function_stack_argument ~(arg_id : int) ~(type_ : Type.t) : Operand.t =
  Operand.of_value_id ~value:FunctionStackArgument arg_id ~type_

(*
 * ============================
 *         Instructions
 * ============================
 *)

let max_instr_id = ref 0

let mk_instr_id () =
  let id = !max_instr_id in
  max_instr_id := id + 1;
  id

let rec mk_blockless_instr (instr : Instruction.instr) : Instruction.t =
  let rec instruction =
    {
      Instruction.id = mk_instr_id ();
      instr;
      prev = instruction;
      next = instruction;
      block = null_block;
    }
  in
  instruction

and mk_instr ~(block : Block.t) (instr : Instruction.instr) : Instruction.t =
  let instr = mk_blockless_instr instr in
  append_instruction block instr;
  instr

and mk_instr_ ~(block : Block.t) (instr : Instruction.instr) : unit = ignore (mk_instr ~block instr)

(*
 * ============================
 *           Blocks
 * ============================
 *)

and max_block_id = ref 0

and mk_block_id () =
  let id = !max_block_id in
  max_block_id := id + 1;
  id

and mk_block ~(func : Function.t) : Block.t =
  let block = { Block.id = mk_block_id (); label = None; func; instructions = None } in
  block

(*
 * ============================
 *      Block Instructions
 * ============================
 *)
and has_single_instruction (block : Block.t) : bool =
  match block.instructions with
  | Some { first; last } when first == last -> true
  | _ -> false

and add_instr_link (instr1 : Instruction.t) (instr2 : Instruction.t) =
  instr1.next <- instr2;
  instr2.prev <- instr1

(* Append an instruction to the end of a block's instruction list *)
and append_instruction (block : Block.t) (instr : Instruction.t) =
  instr.block <- block;
  match block.instructions with
  | None -> block.instructions <- Some { first = instr; last = instr }
  | Some ({ first; last } as list) ->
    add_instr_link last instr;
    add_instr_link instr first;
    list.last <- instr

(* Insert an instruction immediately before another instruction in a block's instruction list *)
and insert_instruction_before ~(before : Instruction.t) (instr : Instruction.t) =
  let block = before.block in
  instr.block <- block;
  match block.instructions with
  | None -> failwith "Block must have before instruction"
  | Some list ->
    let prev_instr = before.prev in
    add_instr_link prev_instr instr;
    add_instr_link instr before;
    if list.first == before then list.first <- instr

(* Insert an instruction immediately after another instruction in a block's instruction list *)
and insert_instruction_after ~(after : Instruction.t) (instr : Instruction.t) =
  let block = after.block in
  instr.block <- block;
  match block.instructions with
  | None -> failwith "Block must have before instruction"
  | Some list ->
    let next_instr = after.next in
    add_instr_link instr next_instr;
    add_instr_link after instr;
    if list.last == after then list.last <- instr

(* Remove an instruction from a block's instruction list *)
and remove_instruction (instr : Instruction.t) =
  let block = instr.block in

  (* Instruction list is circular, so check if single element list *)
  if instr.next == instr then
    block.instructions <- None
  else
    let prev = instr.prev in
    let next = instr.next in
    add_instr_link prev next;
    let list = Option.get block.instructions in
    if list.first == instr then list.first <- next;
    if list.last == instr then list.last <- prev

and iter_instructions (block : Block.t) (f : Instruction.t -> unit) =
  match block.instructions with
  | None -> ()
  | Some { first; last } ->
    let rec iter current last f =
      (* Save next in case instruction is modified *)
      let next = current.Instruction.next in
      f current;
      if current != last then iter next last f
    in
    iter first last f

and iter_instructions_rev (block : Block.t) (f : Instruction.t -> unit) =
  match block.instructions with
  | None -> ()
  | Some { first; last } ->
    let rec iter current first f =
      (* Save prev in case instruction is modified *)
      let prev = current.Instruction.prev in
      f current;
      if current != first then iter prev first f
    in
    iter last first f

and filter_instructions (block : Block.t) (f : Instruction.t -> bool) =
  iter_instructions block (fun instr -> if not (f instr) then remove_instruction instr)

and get_first_instr_opt (block : Block.t) : Instruction.t option =
  match block.instructions with
  | Some { first; _ } -> Some first
  | None -> None

and get_last_instr_opt (block : Block.t) : Instruction.t option =
  match block.instructions with
  | Some { last; _ } -> Some last
  | None -> None

(*
 * ============================
 *         Functions
 * ============================
 *)

let max_func_id = ref 0

let mk_func_id () =
  let id = !max_func_id in
  max_func_id := id + 1;
  id

let func_iter_blocks (func : Function.t) (f : Block.t -> unit) = List.iter f func.blocks

let funcs_iter_blocks (funcs : FunctionSet.t) (f : Block.t -> unit) =
  FunctionSet.iter (fun func -> func_iter_blocks func f) funcs
