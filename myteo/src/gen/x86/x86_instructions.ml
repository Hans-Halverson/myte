type label = string

type size =
  (* 8 bit *)
  | Byte
  (* 16 bit *)
  | Word
  (* 32 bit *)
  | Long
  (* 64 bit *)
  | Quad

type register_slot =
  | A
  | B
  | C
  | D
  | SI
  | DI
  | SP
  | BP
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | IP

type register = register_slot * size

type immediate =
  | ByteImmediate of int
  | WordImmediate of int
  | LongImmediate of Int32.t
  | QuadImmediate of Int64.t

type memory_address_offset =
  | ImmediateOffset of Int64.t
  | LabelOffset of label

type memory_address_scale =
  | Scale1
  | Scale2
  | Scale4
  | Scale8

type memory_address = {
  offset: memory_address_offset option;
  base_register: register;
  index_and_scale: (register * memory_address_scale) option;
}

type source =
  | Immediate of immediate
  | RegisterRead of register
  | MemoryRead of memory_address

type destination =
  | RegisterWrite of register
  | MemoryWrite of memory_address

type conditional_type =
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessThanEqual
  | GreaterThanEqual

type instruction =
  (* Stack instructions *)
  | Push of source
  | Pop of destination
  (* Data instructions *)
  | Mov of source * destination
  | Lea of memory_address * register
  (* Numeric operations *)
  | Neg of destination
  | Add of source * destination
  | Sub of source * destination
  | IMul of source * destination
  | IDiv of source * destination
  (* Bitwise operations *)
  | Not of destination
  | And of source * destination
  | Or of source * destination
  (* Control flow *)
  | Jmp of label
  | CondJmp of conditional_type * label
  | Call of label
  | Cmp of source * source
  | Leave
  | Ret
  | Syscall

type block = {
  label: label;
  instructions: instruction list;
}

type data_value =
  | ImmediateData of immediate
  | AsciiData of string

type data = {
  label: label;
  value: data_value;
}

type executable = {
  text: block list;
  data: data list;
  rodata: data list;
}
