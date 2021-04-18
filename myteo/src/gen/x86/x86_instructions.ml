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

type 'reg memory_address_base =
  | BaseRegister of 'reg
  | IP

type 'reg memory_address = {
  offset: memory_address_offset option;
  base: 'reg memory_address_base;
  index_and_scale: ('reg * memory_address_scale) option;
}

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

type block_id = int

module Instruction = struct
  type id = int

  type 'reg t' =
    (* Instruction Suffixes:
          R - virtual register
          I - immediate
          M - memory location

        When multiple suffixes are used, first is source and second is dest when applicable. *)
    (* Stack instructions *)
    | PushR of 'reg
    | PushI of immediate (* Only supports 8, 16, and 32-bit immediates *)
    | PushM of 'reg memory_address
    | PopR of 'reg
    (* Data instructions *)
    | MovRR of 'reg * 'reg
    | MovRM of 'reg * 'reg memory_address
    | MovMR of 'reg memory_address * 'reg
    | MovIR of immediate * 'reg
    | MovIM of immediate * 'reg memory_address
    | Lea of 'reg memory_address * 'reg
    (* Numeric operations *)
    | NegR of 'reg
    | AddRR of 'reg * 'reg
    | AddIR of immediate * 'reg (* Only supports 8, 16, and 32-bit immediates *)
    | SubRR of 'reg * 'reg
    | SubIR of immediate * 'reg (* Only supports 8, 16, and 32-bit immediates *)
    | IMulRR of 'reg * 'reg
    | IMulRIR of 'reg * immediate * 'reg (* Only supports 8, 16, and 32-bit immediates *)
    | IDivR of 'reg
    (* Bitwise operations *)
    | NotR of 'reg
    | AndRR of 'reg * 'reg
    | AndIR of immediate * 'reg (* Only supports 8, 16, and 32-bit immediates *)
    | OrRR of 'reg * 'reg
    | OrIR of immediate * 'reg (* Only supports 8, 16, and 32-bit immediates *)
    (* Comparisons *)
    | CmpRR of 'reg * 'reg
    | CmpRI of 'reg * immediate (* Only supports 8, 16, and 32-bit immediates *)
    | TestRR of 'reg * 'reg
    | SetCmp of set_cmp_kind * 'reg
    (* Control flow *)
    | Jmp of block_id
    | CondJmp of cond_jmp_kind * block_id
    | CallR of 'reg
    | CallM of 'reg memory_address
    | Leave
    | Ret
    | Syscall

  type 'reg t = id * 'reg t'

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id
end

module Block = struct
  type id = block_id

  and 'reg t = {
    id: id;
    label: label;
    mutable instructions: 'reg Instruction.t list;
  }

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id
end

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

type 'reg program = {
  text: 'reg Block.t list;
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

let invert_cond_jump_kind kind =
  match kind with
  | Equal -> NotEqual
  | NotEqual -> Equal
  | LessThan -> GreaterThanEqual
  | GreaterThan -> LessThanEqual
  | LessThanEqual -> GreaterThan
  | GreaterThanEqual -> LessThan
