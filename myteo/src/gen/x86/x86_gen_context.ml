open Basic_collections
open Mir
open X86_instructions

type vreg_id = int

module VirtualRegister = struct
  type t = int

  let mk () = mk_var_id ()
end

module VReg = VirtualRegister

type virtual_block = VReg.t Block.t

module Gcx = struct
  type t = {
    (* Virtual blocks and builders *)
    mutable text: virtual_block list;
    mutable data: data list;
    mutable bss: bss_data list;
    mutable rodata: data list;
    mutable current_block_builder: virtual_block option;
    (* All blocks, indexed by id *)
    mutable blocks_by_id: virtual_block IMap.t;
    (* Map from instruction id to the block that contains it *)
    mutable instruction_to_block: Block.id IMap.t;
    mutable max_string_literal_id: int;
    mutable max_label_id: int;
    mutable mir_block_id_to_label: label IMap.t;
  }

  let mk () =
    {
      text = [];
      data = [];
      bss = [];
      rodata = [];
      current_block_builder = None;
      blocks_by_id = IMap.empty;
      instruction_to_block = IMap.empty;
      max_string_literal_id = 0;
      max_label_id = 0;
      mir_block_id_to_label = IMap.empty;
    }

  let finish_builders ~gcx =
    {
      text = List.rev gcx.text;
      data = List.rev gcx.data;
      bss = List.rev gcx.bss;
      rodata = List.rev gcx.rodata;
    }

  let add_data ~gcx d = gcx.data <- d :: gcx.data

  let add_string_literal ~gcx ?label str =
    let label =
      match label with
      | None ->
        let id = gcx.max_string_literal_id in
        gcx.max_string_literal_id <- id + 1;
        ".S" ^ string_of_int id
      | Some label -> label
    in
    let data = { label; value = AsciiData str } in
    gcx.rodata <- data :: gcx.rodata;
    label

  let add_bss ~gcx bd = gcx.bss <- bd :: gcx.bss

  let start_block ~gcx ~label =
    let id = Block.mk_id () in
    gcx.current_block_builder <- Some { id; label; instructions = [] }

  let finish_block ~gcx =
    let block = Option.get gcx.current_block_builder in
    block.instructions <- List.rev block.instructions;
    gcx.blocks_by_id <- IMap.add block.id block gcx.blocks_by_id;
    gcx.text <- block :: gcx.text;
    gcx.current_block_builder <- None

  let emit ~gcx instr =
    let current_block = Option.get gcx.current_block_builder in
    let instr_id = Instruction.mk_id () in
    current_block.instructions <- (instr_id, instr) :: current_block.instructions;
    gcx.instruction_to_block <- IMap.add instr_id current_block.id gcx.instruction_to_block

  let get_label_from_mir_block_id ~gcx mir_block_id =
    match IMap.find_opt mir_block_id gcx.mir_block_id_to_label with
    | Some label -> label
    | None ->
      let id = gcx.max_label_id in
      gcx.max_label_id <- gcx.max_label_id + 1;
      let label = ".L" ^ string_of_int id in
      gcx.mir_block_id_to_label <- IMap.add mir_block_id label gcx.mir_block_id_to_label;
      label

  let get_instruction ~gcx instr_id =
    let block_id = IMap.find instr_id gcx.instruction_to_block in
    let block = IMap.find block_id gcx.blocks_by_id in
    let (_, instr) = List.find (fun (id, _) -> id = instr_id) block.instructions in
    instr
end
