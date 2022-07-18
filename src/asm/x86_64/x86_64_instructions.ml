open Basic_collections
open X86_64_calling_conventions
open X86_64_register

type label = string

type register = Register.t * register_size

type immediate =
  | Imm8 of int
  | Imm16 of int
  | Imm32 of Int32.t
  | Imm64 of Int64.t

module rec MemoryAddress : sig
  type t = {
    offset: offset option;
    base: base;
    index_and_scale: (Operand.t * scale) option;
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
  MemoryAddress

and Operand : sig
  type id = int

  type t = {
    id: id;
    mutable value: value;
    (* MIR type for this operand, only guaranteed to be accurate as necessary for tracking pointers
       into the heap. *)
    type_: Mir_type.Type.t;
  }

  and value =
    (* A physical register in a particular register slot *)
    | PhysicalRegister of Register.t
    (* This is a virtual register that has not yet been resolved *)
    | VirtualRegister
    (* Memory address (which may contain vregs as the base, offset, or index) *)
    | MemoryAddress of MemoryAddress.t
    | VirtualStackSlot
    (* An argument to the current function that is passed on the stack. These arguments appear at
       the bottom of the previous function's stack frame. *)
    | FunctionStackArgument
    (* An argument to pass to a callee function on the stack. These arguments appear at this bottom
       of the current function's stack frame. Int is the index of the argument. *)
    | FunctionArgumentStackSlot of int

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
  }

  and value =
    | PhysicalRegister of Register.t
    | VirtualRegister
    | MemoryAddress of MemoryAddress.t
    | VirtualStackSlot
    | FunctionStackArgument
    | FunctionArgumentStackSlot of int

  let mk ~id ~value ~type_ = { id; value; type_ }

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
    | MemoryAddress _
    | VirtualStackSlot
    | FunctionStackArgument
    | FunctionArgumentStackSlot _ ->
      true
    | _ -> false

  let is_reg_value value =
    match value.value with
    | PhysicalRegister _
    | VirtualRegister ->
      true
    | _ -> false
end

let empty_memory_address = { MemoryAddress.offset = None; base = NoBase; index_and_scale = None }

type condition_code =
  | E
  | NE
  | L
  | LE
  | G
  | GE
  | B
  | BE
  | A
  | AE

type block_id = int

type func_id = int

module OperandCollection = MakeCollection (Operand)

module OperandSet = OperandCollection.Set
module OperandMap = OperandCollection.Map
module OOMMap = MultiMap.Make (OperandCollection) (OperandCollection)
module OIMMap = MultiMap.Make (OperandCollection) (IntCollection)

let string_of_oset oset =
  let elements =
    OperandSet.to_seq oset
    |> List.of_seq
    |> List.map (fun op -> string_of_int op.Operand.id)
    |> String.concat ", "
  in
  "(" ^ elements ^ ")"

module Instruction = struct
  type id = int

  type instr =
    (* Instruction Suffixes:
          R - virtual register
          I - immediate
          M - memory location or virtual register

        When multiple suffixes are used, first is source and second is dest when applicable.
        All MM instructions must contain at least one register as an argument.

        Unless otherwise noted, immediates can only be 8, 16, or 32 bits. *)
    (* Stack instructions, all implicitly have size of 64 bits *)
    | PushI of immediate
    | PushM of Operand.t
    | PopM of Operand.t
    (* Data instructions *)
    (* Allows 64-bit immediate. register_size is destination size which may not match immediate size *)
    | MovIM of register_size * immediate * Operand.t
    (* Allows 64-bit immediate. Allows SSE registers. register_size is destination size, or transferred size if SSE *)
    | MovMM of register_size * Operand.t * Operand.t
    (* Src size then dest size where src size < dest size *)
    | MovSX of register_size * register_size * Operand.t * (* Register *) Operand.t
    (* Src size then dest size where src size < dest size *)
    | MovZX of register_size * register_size * Operand.t * (* Register *) Operand.t
    | Lea of
        register_size
        * MemoryAddress.t
        * Operand.t (* Only supports 32 or 64 bit register argument *)
    (* Numeric operations *)
    | NegM of register_size * Operand.t
    | AddIM of register_size * immediate * Operand.t
    (* Allows SSE registers (with 64 bit size), if SSE then destination must be a register *)
    | AddMM of register_size * Operand.t * Operand.t
    (* For sub instructions, right/dest := right/dest - left/src *)
    | SubIM of register_size * immediate * Operand.t
    (* Allows SSE registers (with 64 bit size), if SSE then destination must be a register *)
    | SubMM of register_size * Operand.t * Operand.t
    (* Allows SSE registers (with 64 bit size) *)
    | MulMR of
        register_size
        * Operand.t
        * (* Register *) Operand.t (* Only supports 16, 32, and 64-bit arguments *)
    | IMulMIR of
        register_size
        * Operand.t
        * immediate
        * (* Register *) Operand.t (* Only supports 16 and 32-bit immediates *)
    | IDiv of register_size * Operand.t
    (* Requires SSE registers (with 64 bit size). right/dest := (right/dest) / (left/src) *)
    | FDivMR of register_size * Operand.t * Operand.t
    (* Bitwise operations *)
    | NotM of register_size * Operand.t
    | AndIM of register_size * immediate * Operand.t
    | AndMM of register_size * Operand.t * Operand.t
    | OrIM of register_size * immediate * Operand.t
    | OrMM of register_size * Operand.t * Operand.t
    | XorIM of register_size * immediate * Operand.t
    (* Allows SSE registers (with 128 bit size), if SSE then destination must be a register *)
    | XorMM of register_size * Operand.t * Operand.t
    (* Bit shifts *)
    | ShlI of register_size * immediate * Operand.t (* Requires 8-bit immediate *)
    | ShlR of register_size * Operand.t
    | ShrI of register_size * immediate * Operand.t (* Requires 8-bit immediate *)
    | ShrR of register_size * Operand.t
    | SarI of register_size * immediate * Operand.t (* Requires 8-bit immediate *)
    | SarR of register_size * Operand.t
    (* Comparisons *)
    | CmpMI of register_size * Operand.t * immediate
    (* Allows SSE registers (with 64 bit size). Must be CmpRM if SSE. *)
    | CmpMM of register_size * Operand.t * Operand.t
    | TestMR of register_size * Operand.t * (* Register *) Operand.t
    | SetCC of condition_code * Operand.t (* Only supports 8-bit destination *)
    (* Conversions *)
    | ConvertDouble of register_size (* Only supports 16, 32, and 64 byte sizes (cwd/cdq/cqo) *)
    (* Control flow *)
    | Jmp of block_id
    | JmpCC of condition_code * block_id
    | CallL of label
    | CallM of register_size * Operand.t
    | Leave
    | Ret
    | Syscall

  type t = {
    id: id;
    mutable instr: instr;
  }

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id

  let compare i1 i2 = Int.compare i1.id i2.id
end

module Block = struct
  type id = block_id

  and t = {
    id: id;
    label: label option;
    func: func_id;
    mutable instructions: Instruction.t list;
  }

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id

  let get_id block = block.id

  let iter_instrs_rev f block = List.iter f (List.rev block.instructions)

  let filter_instrs f block = block.instructions <- List.filter f block.instructions
end

module Function = struct
  type id = int

  type t = {
    id: id;
    mutable params: Operand.t list;
    param_types: param_type array;
    mutable prologue: block_id;
    mutable blocks: Block.t list;
    mutable spilled_callee_saved_regs: RegSet.t;
    mutable spilled_vslots: OperandSet.t;
    mutable num_stack_frame_slots: int;
    (* Stack slots in stack frame which hold arguments to pass on stack to callee functions. First
       element in list is at bottom of stack frame (closest to callee function's stack frame). *)
    mutable argument_stack_slots: Operand.t list;
    mutable num_argument_stack_slots: int;
  }

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id
end

module InstructionCollection = MakeCollection (Instruction)

module InstrSet = InstructionCollection.Set
module OInstrMMap = MultiMap.Make (OperandCollection) (InstructionCollection)

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

let pointer_size = 8

let mk_data_section () = Array.make 5 []

let bytes_of_size size =
  match size with
  | Size8 -> 1
  | Size16 -> 2
  | Size32 -> 4
  | Size64 -> 8
  | Size128 -> 16

let size_of_immediate imm =
  match imm with
  | Imm8 _ -> Size8
  | Imm16 _ -> Size16
  | Imm32 _ -> Size32
  | Imm64 _ -> Size64

let int64_of_immediate imm =
  match imm with
  | Imm8 i -> Int64.of_int i
  | Imm16 i -> Int64.of_int i
  | Imm32 i -> Int64.of_int32 i
  | Imm64 i -> i

let rec align_of_data_value d =
  match d with
  | ImmediateData imm -> bytes_of_size (size_of_immediate imm)
  | AsciiData _ -> 1
  | LabelData _ -> 8
  | SSELiteral _ -> 16
  | ArrayData data ->
    List.fold_left
      (fun max_align value ->
        let align = align_of_data_value value in
        if align > max_align then
          align
        else
          max_align)
      1
      data

let align_to_data_section_align_index align =
  match align with
  | 1 -> 0
  | 2 -> 1
  | 4 -> 2
  | 8 -> 3
  | 16 -> 4
  | _ -> failwith "Invalid alignment"

(* Return the opposite of a condition code (NOT CC) *)
let invert_condition_code cc =
  match cc with
  | E -> NE
  | NE -> E
  | L -> GE
  | G -> LE
  | LE -> G
  | GE -> L
  | B -> AE
  | BE -> A
  | A -> BE
  | AE -> B

(* Return the condition code that results from swapping the arguments *)
let swap_condition_code_order cc =
  match cc with
  | E
  | NE ->
    cc
  | L -> G
  | G -> L
  | LE -> GE
  | GE -> LE
  | B -> A
  | A -> B
  | BE -> AE
  | AE -> BE

let main_label = "_main"

let init_label = "_myte_init"

let double_negate_mask_label = "_double_negate_mask"
