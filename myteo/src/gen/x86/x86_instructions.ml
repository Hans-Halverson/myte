open Basic_collections

type label = string

type register_size =
  | Size8
  | Size16
  | Size32
  | Size64

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

module RegCollection = struct
  type t = register_slot

  let compare = Stdlib.compare
end

module RegSet = Set.Make (RegCollection)
module RegMap = Map.Make (RegCollection)

let all_registers =
  RegSet.of_list [A; B; C; D; SI; DI; SP; BP; R8; R9; R10; R11; R12; R13; R14; R15]

let callee_saved_registers = RegSet.of_list [B; SP; BP; R12; R13; R14; R15]

let caller_saved_registers = RegSet.of_list [A; C; D; SI; DI; R8; R9; R10; R11]

type register = register_slot * register_size

type immediate =
  | Imm8 of int
  | Imm16 of int
  | Imm32 of Int32.t
  | Imm64 of Int64.t

type memory_address_offset =
  | ImmediateOffset of Int32.t
  | LabelOffset of label

type 'a memory_address_base =
  | NoBase
  | RegBase of 'a
  | IPBase

type memory_address_scale =
  | Scale1
  | Scale2
  | Scale4
  | Scale8

type 'reg memory_address =
  | VirtualStackSlot of 'reg
  | FunctionStackArgument of 'reg
  | PhysicalAddress of {
      offset: memory_address_offset option;
      base: 'reg memory_address_base;
      index_and_scale: ('reg * memory_address_scale) option;
    }

let empty_memory_address = PhysicalAddress { offset = None; base = NoBase; index_and_scale = None }

type condition_code =
  | E
  | NE
  | L
  | LE
  | G
  | GE

type block_id = int

type func_id = int

type vreg_id = int

module VirtualRegister = struct
  type id = int

  type t = {
    id: id;
    mutable resolution: resolution;
    mutable func: func_id option;
  }

  and resolution =
    (* This vreg has been aliased to another. The resolution is the resolution of the alias vreg. *)
    | Alias of t
    (* This vreg has been mapped to a physical register in a particular register slot *)
    | Physical of register_slot
    (* This vreg has been mapped to a slot on the stack. May be explicit or result from spills. *)
    | StackSlot of t memory_address
    (* This vreg has not yet been resolved to a physical location *)
    | Unresolved

  let vregs_by_id = ref IMap.empty

  let of_var_id ~resolution ~func var_id =
    match IMap.find_opt var_id !vregs_by_id with
    | None ->
      let new_vreg = { id = var_id; resolution; func } in
      vregs_by_id := IMap.add var_id new_vreg !vregs_by_id;
      new_vreg
    | Some existing_vreg -> existing_vreg

  let mk ~resolution ~func = { id = Mir.mk_var_id (); resolution; func }

  let compare v1 v2 = Int.compare v1.id v2.id

  let rec get_vreg_alias vreg =
    match vreg.resolution with
    | Alias alias -> get_vreg_alias alias
    | _ -> vreg

  let get_vreg_resolution vreg = (get_vreg_alias vreg).resolution

  let get_physical_resolution vreg =
    match get_vreg_resolution vreg with
    | Physical reg -> reg
    | _ -> failwith "Expected virtual register to be resolved to physical register"
end

module VReg = VirtualRegister
module VRegSet = Set.Make (VirtualRegister)
module VRegMap = Map.Make (VirtualRegister)
module VVMMap = MultiMap.Make (VReg) (VReg)
module VIMMap = MultiMap.Make (VReg) (Int)

let string_of_vset vset =
  let elements =
    VRegSet.to_seq vset
    |> List.of_seq
    |> List.map (fun vreg -> string_of_int vreg.VReg.id)
    |> String.concat ", "
  in
  "(" ^ elements ^ ")"

module Instruction = struct
  type id = int

  type 'reg memory =
    | Reg of 'reg
    | Mem of 'reg memory_address

  type 'reg t' =
    (* Instruction Suffixes:
          R - virtual register
          I - immediate
          M - memory location or virtual register

        When multiple suffixes are used, first is source and second is dest when applicable.
        All MM instructions must contain at least one register as an argument.

        Unless otherwise noted, immediates can only be 8, 16, or 32 bits. *)
    (* Stack instructions *)
    | PushI of immediate
    | PushM of 'reg memory
    | PopM of 'reg memory
    (* Data instructions *)
    | MovIM of register_size * immediate * 'reg memory (* Allows 64-bit immediate *)
    | MovMM of register_size * 'reg memory * 'reg memory
    | Lea of register_size * 'reg memory_address * 'reg (* Only supports 32 or 64 bit register argument *)
    (* Numeric operations *)
    | NegM of register_size * 'reg memory
    | AddIM of register_size * immediate * 'reg memory
    | AddMM of register_size * 'reg memory * 'reg memory
    (* For sub instructions, right/dest := right/dest - left/src *)
    | SubIM of register_size * immediate * 'reg memory
    | SubMM of register_size * 'reg memory * 'reg memory
    | IMulMR of register_size * 'reg memory * 'reg (* Only supports 16, 32, and 64-bit arguments *)
    | IMulMIR of register_size * 'reg memory * immediate * 'reg (* Only supports 16 and 32-bit immediates *)
    | IDiv of register_size * 'reg memory
    (* Bitwise operations *)
    | NotM of register_size * 'reg memory
    | AndIM of register_size * immediate * 'reg memory
    | AndMM of register_size * 'reg memory * 'reg memory
    | OrIM of register_size * immediate * 'reg memory
    | OrMM of register_size * 'reg memory * 'reg memory
    | XorIM of register_size * immediate * 'reg memory
    | XorMM of register_size * 'reg memory * 'reg memory
    (* Bit shifts *)
    | ShlI of register_size * immediate * 'reg memory (* Requires 8-bit immediate *)
    | ShlR of register_size * 'reg memory
    | ShrI of register_size * immediate * 'reg memory (* Requires 8-bit immediate *)
    | ShrR of register_size * 'reg memory
    | SarI of register_size * immediate * 'reg memory (* Requires 8-bit immediate *)
    | SarR of register_size * 'reg memory
    (* Comparisons *)
    | CmpMI of register_size * 'reg memory * immediate
    | CmpMM of register_size * 'reg memory * 'reg memory
    | TestMR of register_size * 'reg memory * 'reg
    | SetCC of condition_code * 'reg memory (* Only supports 8-bit destination *)
    (* Control flow *)
    | Jmp of block_id
    | JmpCC of condition_code * block_id
    | CallL of label
    | CallM of register_size * 'reg memory
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
    label: label option;
    func: func_id;
    mutable instructions: 'reg Instruction.t list;
  }

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id
end

module Function = struct
  type id = int

  type 'reg t = {
    id: id;
    mutable params: 'reg list;
    mutable prologue: block_id;
    mutable blocks: 'reg Block.t list;
    mutable spilled_callee_saved_regs: RegSet.t;
    mutable spilled_vregs: VRegSet.t;
    mutable num_stack_frame_slots: int;
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

type virtual_instruction = VReg.t Instruction.t

type virtual_block = VReg.t Block.t

type virtual_function = VReg.t Function.t

type virtual_program = VReg.t program

let bytes_of_size size =
  match size with
  | Size8 -> 1
  | Size16 -> 2
  | Size32 -> 4
  | Size64 -> 8

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

(* Return the opposite of a condition code (NOT CC) *)
let invert_condition_code cc =
  match cc with
  | E -> NE
  | NE -> E
  | L -> GE
  | G -> LE
  | LE -> G
  | GE -> L

(* Return the condition code that results from swapping the arguments *)
let swap_condition_code_order cc =
  match cc with
  | E
  | NE ->
    cc
  | L -> GE
  | G -> LE
  | LE -> G
  | GE -> L

let register_of_param i =
  if i < 6 then
    Some
      (match i with
      | 0 -> DI
      | 1 -> SI
      | 2 -> D
      | 3 -> C
      | 4 -> R8
      | 5 -> R9
      | _ -> R9)
  else
    None

let debug_string_of_reg reg =
  match reg with
  | A -> "rax"
  | B -> "rbx"
  | C -> "rcx"
  | D -> "rdx"
  | SI -> "rsi"
  | DI -> "rdi"
  | SP -> "rsp"
  | BP -> "rbp"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"

let string_of_sized_reg reg size =
  match (reg, size) with
  | (A, Size64) -> "rax"
  | (B, Size64) -> "rbx"
  | (C, Size64) -> "rcx"
  | (D, Size64) -> "rdx"
  | (SI, Size64) -> "rsi"
  | (DI, Size64) -> "rdi"
  | (SP, Size64) -> "rsp"
  | (BP, Size64) -> "rbp"
  | (R8, Size64) -> "r8"
  | (R9, Size64) -> "r9"
  | (R10, Size64) -> "r10"
  | (R11, Size64) -> "r11"
  | (R12, Size64) -> "r12"
  | (R13, Size64) -> "r13"
  | (R14, Size64) -> "r14"
  | (R15, Size64) -> "r15"
  | (A, Size32) -> "eax"
  | (B, Size32) -> "ebx"
  | (C, Size32) -> "ecx"
  | (D, Size32) -> "edx"
  | (SI, Size32) -> "esi"
  | (DI, Size32) -> "edi"
  | (SP, Size32) -> "esp"
  | (BP, Size32) -> "ebp"
  | (R8, Size32) -> "r8d"
  | (R9, Size32) -> "r9d"
  | (R10, Size32) -> "r10d"
  | (R11, Size32) -> "r11d"
  | (R12, Size32) -> "r12d"
  | (R13, Size32) -> "r13d"
  | (R14, Size32) -> "r14d"
  | (R15, Size32) -> "r15d"
  | (A, Size16) -> "ax"
  | (B, Size16) -> "bx"
  | (C, Size16) -> "cx"
  | (D, Size16) -> "dx"
  | (SI, Size16) -> "si"
  | (DI, Size16) -> "di"
  | (SP, Size16) -> "sp"
  | (BP, Size16) -> "bp"
  | (R8, Size16) -> "r8w"
  | (R9, Size16) -> "r9w"
  | (R10, Size16) -> "r10w"
  | (R11, Size16) -> "r11w"
  | (R12, Size16) -> "r12w"
  | (R13, Size16) -> "r13w"
  | (R14, Size16) -> "r14w"
  | (R15, Size16) -> "r15w"
  | (A, Size8) -> "al"
  | (B, Size8) -> "bl"
  | (C, Size8) -> "cl"
  | (D, Size8) -> "dl"
  | (SI, Size8) -> "sil"
  | (DI, Size8) -> "dil"
  | (SP, Size8) -> "spl"
  | (BP, Size8) -> "bpl"
  | (R8, Size8) -> "r8b"
  | (R9, Size8) -> "r9b"
  | (R10, Size8) -> "r10b"
  | (R11, Size8) -> "r11b"
  | (R12, Size8) -> "r12b"
  | (R13, Size8) -> "r13b"
  | (R14, Size8) -> "r14b"
  | (R15, Size8) -> "r15b"

let main_label = "_main"

let start_label = "_start"
