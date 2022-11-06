open Asm_calling_convention
open Asm_instruction_definition
open Asm_register
open Basic_collections

type label = string

type immediate =
  | Imm8 of Int8.t
  | Imm16 of int
  | Imm32 of Int32.t
  | Imm64 of Int64.t

module rec X86_64_MemoryAddress : sig
  type t = {
    mutable offset: offset option;
    mutable base: base;
    mutable index_and_scale: (Operand.t * scale) option;
  }

  and offset =
    | ImmediateOffset of Int32.t
    | LabelOffset of label

  and base =
    | NoBase
    | RegBase of Operand.t
    | IPBase

  and scale =
    | Scale1
    | Scale2
    | Scale4
    | Scale8
end =
  X86_64_MemoryAddress

and Operand : sig
  type id = int

  type t = {
    id: id;
    mutable value: value;
    (* MIR type for this operand, only guaranteed to be accurate as necessary for tracking pointers
       into the heap. *)
    type_: Mir_type.Type.t;
    (* Whether this operand resulted from a spill. If so this operand will not be spilled later. *)
    mutable from_spill: bool;
  }

  and value =
    (* A physical register in a particular register slot *)
    | PhysicalRegister of Register.t
    (* This is a virtual register that has not yet been resolved *)
    | VirtualRegister
    (* Immediate *)
    | Immediate of immediate
    (* Float immediate *)
    | FloatImmediate of Float.t
    (* Memory address (which may contain vregs as the base, offset, or index) *)
    | X86_64_MemoryAddress of X86_64_MemoryAddress.t
    (* Direct reference to a function *)
    | Function of Function.t
    (* Direct reference to a basic block *)
    | Block of Block.t
    (* Textual label, often address of a function or global *)
    | Label of label
    (* Slot in the current function's stack frame. Contains arguments passed to callee functions
       at the bottom of the frame starting at index 0, followed by virtual stack slots after coloring.
       Stack slot -1 represents the bottom of the caller function's stack frame. This is used to
       reference arguments passed to by the caller to the current function on the stack. *)
    | StackSlot of int
    | VirtualStackSlot

  val of_value_id : value:value -> type_:Mir_type.Type.t -> id -> t

  val mk : id:id -> value:value -> type_:Mir_type.Type.t -> t

  val compare : t -> t -> int

  val get_physical_register_value : t -> Register.t

  val is_memory_value : t -> bool

  val is_reg_value : t -> bool
end = struct
  type id = int

  type t = {
    id: id;
    mutable value: value;
    type_: Mir_type.Type.t;
    mutable from_spill: bool;
  }

  and value =
    | PhysicalRegister of Register.t
    | VirtualRegister
    | Immediate of immediate
    | FloatImmediate of Float.t
    | X86_64_MemoryAddress of X86_64_MemoryAddress.t
    | Function of Function.t
    | Block of Block.t
    | Label of label
    | StackSlot of int
    | VirtualStackSlot

  let mk ~id ~value ~type_ = { id; value; type_; from_spill = false }

  let ops_by_id = ref IMap.empty

  let of_value_id ~value ~type_ value_id =
    match IMap.find_opt value_id !ops_by_id with
    | Some existing_op -> existing_op
    | None ->
      let new_op = mk ~id:value_id ~value ~type_ in
      ops_by_id := IMap.add value_id new_op !ops_by_id;
      new_op

  let compare v1 v2 = Int.compare v1.id v2.id

  let get_physical_register_value op =
    match op.value with
    | PhysicalRegister reg -> reg
    | _ -> failwith "Expected operand to be physical register"

  let is_memory_value value =
    match value.value with
    | X86_64_MemoryAddress _
    | StackSlot _
    | VirtualStackSlot ->
      true
    | _ -> false

  let is_reg_value value =
    match value.value with
    | PhysicalRegister _
    | VirtualRegister ->
      true
    | _ -> false
end

and Instruction : sig
  type id = int

  type t = {
    id: id;
    mutable instr: instr;
    mutable operands: Operand.t array;
    mutable block: Block.t;
    (* Circular, doubly linked list of instructions in block *)
    mutable prev: Instruction.t;
    mutable next: Instruction.t;
  }
end =
  Instruction

and Block : sig
  type id = int

  and t = {
    id: id;
    mutable label: label option;
    mutable func: Function.t;
    mutable instructions: instructions option;
  }

  (* Circular doubly linked list of all instructions in block *)
  and instructions = {
    mutable first: Instruction.t;
    mutable last: Instruction.t;
  }

  val compare : t -> t -> int
end = struct
  type id = int

  and t = {
    id: id;
    mutable label: label option;
    mutable func: Function.t;
    mutable instructions: instructions option;
  }

  and instructions = {
    mutable first: Instruction.t;
    mutable last: Instruction.t;
  }

  let compare b1 b2 = Int.compare b1.id b2.id
end

and Function : sig
  type id = int

  type t = {
    id: id;
    label: label;
    mutable params: Operand.t list;
    param_types: param_type array;
    return_type: Mir_type.Type.t option;
    calling_convention: calling_convention;
    mutable prologue: Block.t;
    mutable blocks: Block.t list;
    mutable spilled_callee_saved_regs: RegSet.t;
    mutable spilled_vslots: OperandSet.t;
    mutable num_stack_frame_slots: int;
    (* Stack slots in stack frame which hold arguments to pass on stack to callee functions. First
       element in list is at bottom of stack frame (closest to callee function's stack frame). *)
    mutable argument_stack_slots: Operand.t list;
    mutable num_argument_stack_slots: int;
    (* Whether this is a leaf function (includes no calls, including recursive calls). Is true until
       a call is added. *)
    mutable is_leaf: bool;
  }
end =
  Function

and OperandCollection : (MultiMap.KEY_AND_VALUE_TYPE with type t = Operand.t) =
  MakeCollection (Operand)

and InstructionCollection : (MultiMap.KEY_AND_VALUE_TYPE with type t = Instruction.t) =
MakeCollection (struct
  type t = Instruction.t
  let compare (i1 : Instruction.t) (i2 : Instruction.t) = Int.compare i1.id i2.id
end)

and BlockCollection : (MultiMap.KEY_AND_VALUE_TYPE with type t = Block.t) = MakeCollection (Block)

and FunctionCollection : (MultiMap.KEY_AND_VALUE_TYPE with type t = Function.t) =
MakeCollection (struct
  type t = Function.t
  let compare (f1 : Function.t) (f2 : Function.t) = Int.compare f1.id f2.id
end)

and OperandSet : (Set.S with type elt = Operand.t) = OperandCollection.Set
and OperandMap : (Map.S with type key = Operand.t) = OperandCollection.Map
and InstrSet : (Set.S with type elt = Instruction.t) = InstructionCollection.Set
and InstrMap : (Map.S with type key = Instruction.t) = InstructionCollection.Map
and BlockSet : (Set.S with type elt = Block.t) = BlockCollection.Set
and BlockMap : (Map.S with type key = Block.t) = BlockCollection.Map
and FunctionSet : (Set.S with type elt = Function.t) = FunctionCollection.Set

module BlockMMap = MultiMap.Make (BlockMap) (BlockSet)

module OInstrMMap = MultiMap.Make (OperandMap) (InstrSet)

module OOMMap = MultiMap.Make (OperandMap) (OperandSet)

module OBMMap = MultiMap.Make (OperandMap) (BlockSet)

let string_of_oset oset =
  let elements =
    OperandSet.to_seq oset
    |> List.of_seq
    |> List.map (fun op -> string_of_int op.Operand.id)
    |> String.concat ", "
  in
  "(" ^ elements ^ ")"

type data_value =
  | ImmediateData of immediate
  | AsciiData of string
  | LabelData of string list
  | ArrayData of data_value list
  | SSELiteral of immediate list

type 'a data_item = {
  label: label;
  value: 'a;
  size: int;
  is_pointer: bool;
}

type initialized_data_item = data_value data_item

type uninitialized_data_item = unit data_item

(* Array of data lists, where every element of the data list at index i has alignment of 2^i *)
type 'a data_section = 'a list array

type data = initialized_data_item data_section

type bss = uninitialized_data_item data_section

type program = {
  text: Block.t list;
  data: data;
  bss: bss;
}

let rec null_function : Function.t =
  {
    Function.id = 0;
    label = "<null>";
    params = [];
    param_types = null_param_types;
    return_type = None;
    calling_convention = null_calling_convention;
    prologue = null_block;
    blocks = [];
    spilled_callee_saved_regs = RegSet.empty;
    spilled_vslots = OperandSet.empty;
    num_stack_frame_slots = 0;
    argument_stack_slots = [];
    num_argument_stack_slots = 0;
    is_leaf = true;
  }

and null_block : Block.t = { Block.id = 0; label = None; func = null_function; instructions = None }

let empty_x86_64_memory_address =
  { X86_64_MemoryAddress.offset = None; base = NoBase; index_and_scale = None }

let bytes_of_size size =
  match size with
  | X86_64.Size8 -> 1
  | Size16 -> 2
  | Size32 -> 4
  | Size64 -> 8
  | Size128 -> 16

let size_of_immediate imm =
  match imm with
  | Imm8 _ -> X86_64.Size8
  | Imm16 _ -> Size16
  | Imm32 _ -> Size32
  | Imm64 _ -> Size64

let cast_to_immediate op =
  match op.Operand.value with
  | Immediate imm -> imm
  | _ -> failwith "Expected immediate operand"

let cast_to_memory_address op =
  match op.Operand.value with
  | X86_64_MemoryAddress addr -> addr
  | _ -> failwith "Expected memory address operand"

let cast_to_block op =
  match op.Operand.value with
  | Block block -> block
  | _ -> failwith "Expected block operand"

let cast_to_function op =
  match op.Operand.value with
  | Function func -> func
  | _ -> failwith "Expected function operand"

let cast_to_stack_slot op =
  match op.Operand.value with
  | StackSlot index -> index
  | _ -> failwith "Expected stack slot operand"

let cast_to_label op =
  match op.Operand.value with
  | Label label -> label
  | _ -> failwith "Expected label operand"

let int64_of_immediate imm =
  match imm with
  | Imm8 i -> Int8.to_int64 i
  | Imm16 i -> Int64.of_int i
  | Imm32 i -> Int64.of_int32 i
  | Imm64 i -> i

let is_zero_immediate imm =
  match imm with
  | Imm8 i -> Int8.to_int i == 0
  | Imm16 i -> i == 0
  | Imm32 i -> Int32.equal i 0l
  | Imm64 i -> Int64.equal i 0L

let main_label = "_main"

let init_label = "_myte_init"

let pointer_size = 8
