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

type 'reg memory_address = {
  offset: memory_address_offset option;
  base_register: 'reg;
  index_and_scale: ('reg * memory_address_scale) option;
}

type 'reg source =
  | ImmediateSource of immediate
  | RegisterSource of 'reg
  | MemorySource of 'reg memory_address

type 'reg destination =
  | RegisterDest of 'reg
  | MemoryDest of 'reg memory_address

type cond_jmp_kind =
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessThanEqual
  | GreaterThanEqual

type set_cmp_kind =
  | SetE
  | SetNE
  | SetL
  | SetLE
  | SetG
  | SetGE

type 'reg instruction =
  (* Stack instructions *)
  | Push of 'reg source
  | Pop of 'reg destination
  (* Data instructions *)
  | Mov of 'reg source * 'reg destination
  | Lea of 'reg memory_address * 'reg
  (* Numeric operations *)
  | Neg of 'reg destination
  | Add of 'reg source * 'reg destination
  | Sub of 'reg source * 'reg destination
  | IMul of 'reg source * 'reg
  | IDiv of 'reg source
  (* Bitwise operations *)
  | Not of 'reg destination
  | And of 'reg source * 'reg destination
  | Or of 'reg source * 'reg destination
  (* Comparisons *)
  | Cmp of 'reg source * 'reg source
  | Test of 'reg source * 'reg source
  | SetCmp of set_cmp_kind * 'reg
  (* Control flow *)
  | Jmp of label
  | CondJmp of cond_jmp_kind * label
  | Call of 'reg source
  | Leave
  | Ret
  | Syscall

type 'reg block = {
  label: label;
  mutable instructions: 'reg instruction list;
}

type data_value =
  | ImmediateData of immediate
  | AsciiData of string

type data = {
  label: label;
  value: data_value;
}

type bss_data = {
  label: label;
  size: int;
}

type 'reg executable = {
  text: 'reg block list;
  data: data list;
  bss: bss_data list;
  rodata: data list;
}

let bytes_of_size size =
  match size with
  | Byte -> 1
  | Word -> 2
  | Long -> 4
  | Quad -> 8
