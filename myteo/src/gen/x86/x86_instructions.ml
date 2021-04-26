open Basic_collections

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

type 'reg memory_address =
  | VirtualStackSlot of 'reg
  | FunctionStackArgument of 'reg
  | PhysicalAddress of {
      offset: memory_address_offset option;
      base: 'reg option;
      index_and_scale: ('reg * memory_address_scale) option;
    }

let empty_memory_address = PhysicalAddress { offset = None; base = None; index_and_scale = None }

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
        All MM instructions must contain at least one register as an argument. *)
    (* Stack instructions *)
    | PushI of immediate (* Only supports 16 and 32-bit immediates *)
    | PushM of 'reg memory (* Does not support 8-bit memory due to stack alignment *)
    | PopM of 'reg memory (* Does not support 8-bit memory due to stack alignment *)
    (* Data instructions *)
    | MovIM of immediate * 'reg memory
    | MovMM of 'reg memory * 'reg memory
    | Lea of 'reg memory_address * 'reg
    (* Numeric operations *)
    | NegM of 'reg memory
    | AddIM of immediate * 'reg memory (* Only supports 8, 16, and 32-bit immediates *)
    | AddMM of 'reg memory * 'reg memory
    (* For sub instructions, right/dest := right/dest - left/src *)
    | SubIM of immediate * 'reg memory (* Only supports 8, 16, and 32-bit immediates *)
    | SubMM of 'reg memory * 'reg memory
    | IMulMR of 'reg memory * 'reg
    | IMulMIR of 'reg memory * immediate * 'reg (* Only supports 8, 16, and 32-bit immediates *)
    | IDiv of 'reg memory
    (* Bitwise operations *)
    | NotM of 'reg memory
    | AndIM of immediate * 'reg memory (* Only supports 8, 16, and 32-bit immediates *)
    | AndMM of 'reg memory * 'reg memory
    | OrIM of immediate * 'reg memory (* Only supports 8, 16, and 32-bit immediates *)
    | OrMM of 'reg memory * 'reg memory
    | XorMM of 'reg memory * 'reg memory
    (* Comparisons *)
    | CmpMI of 'reg memory * immediate (* Only supports 8, 16, and 32-bit immediates *)
    | CmpMM of 'reg memory * 'reg memory
    | TestMR of 'reg memory * 'reg
    | SetCC of condition_code * 'reg memory (* Only supports 8-bit location *)
    (* Control flow *)
    | Jmp of block_id
    | JmpCC of condition_code * block_id
    | CallL of label
    | CallM of 'reg memory
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
  | Byte -> 1
  | Word -> 2
  | Long -> 4
  | Quad -> 8

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
