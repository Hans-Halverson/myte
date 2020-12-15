open Basic_collections
open Mir
open X86_gen_abstract
open X86_instructions

let rsquad r = RegisterSource (r, Quad)

let isquad i = ImmediateSource (QuadImmediate i)

let rwquad r = RegisterDest (r, Quad)

let msquad offset r index_and_scale = { base_register = (r, Quad); offset; index_and_scale }

let hello_world_executable =
  {
    rodata = [{ label = "msg"; value = AsciiData "Hello World!\n" }];
    bss = [];
    data = [];
    text =
      [
        {
          label = "_main";
          instructions =
            [
              Push (rsquad BP);
              Mov (rsquad SP, rwquad BP);
              Mov (isquad (Int64.of_int 14), rwquad D);
              Lea (msquad (Some (LabelOffset "msg")) IP None, (SI, Quad));
              Mov (isquad Int64.one, rwquad DI);
              Mov (isquad (Int64.of_int 33554436), rwquad A);
              Syscall;
              Mov (isquad Int64.zero, rwquad A);
              Leave;
              Ret;
            ];
        };
      ];
  }

module GenContext = struct
  type t = {
    mutable visited_blocks: SSet.t;
    mutable init_blocks: var_id block list;
    mutable in_init: bool;
    mutable text: var_id block list;
    mutable current_block: var_id block option;
    mutable blocks: var_id block SMap.t;
    mutable block_labels: label IMap.t;
  }

  let mk () =
    {
      visited_blocks = SSet.empty;
      init_blocks = [];
      in_init = true;
      text = [];
      current_block = None;
      blocks = SMap.empty;
      block_labels = IMap.empty;
    }

  let check_visited_block ~gcx label =
    let is_visited = SSet.mem label gcx.visited_blocks in
    if not is_visited then gcx.visited_blocks <- SSet.add label gcx.visited_blocks;
    is_visited

  let reset_visited_blocks ~gcx = gcx.visited_blocks <- SSet.empty

  let finish_blocks ~gcx =
    gcx.init_blocks <- List.rev gcx.init_blocks;
    gcx.text <- List.rev gcx.text;
    gcx.current_block <- None

  let start_init ~gcx = gcx.in_init <- true

  let end_init ~gcx = gcx.in_init <- false

  let start_block ~gcx label = gcx.current_block <- Some { label; instructions = [] }

  let finish_block ~gcx =
    let block = Option.get gcx.current_block in
    block.instructions <- List.rev block.instructions;
    gcx.blocks <- SMap.add block.label block gcx.blocks;
    gcx.text <- block :: gcx.text;
    if gcx.in_init then gcx.init_blocks <- block :: gcx.init_blocks;
    gcx.current_block <- None

  let emit ~gcx instr =
    let current_block = Option.get gcx.current_block in
    current_block.instructions <- instr :: current_block.instructions

  let get_block ~gcx label = SMap.find label gcx.blocks
end

module Gcx = GenContext

type source_value_info =
  | SVImmediate of immediate
  | SVLabel of label * size
  | SVVariable of var_id * size
  | SVStringImmediate of string

let rec gen_x86_executable ir =
  let gcx = Gcx.mk () in
  let abstract_result = SSAAbstractInstruction.gen ir in
  allocate_registers ~gcx ~ir ~abstract_result

and allocate_registers ~gcx ~ir ~abstract_result : var_id X86_instructions.executable =
  let module AI = SSAAbstractInstruction in
  let rec visit_block (block : AI.block) =
    let label = block.label in
    if Gcx.check_visited_block ~gcx label then
      ()
    else (
      Gcx.start_block ~gcx label;
      List.iter
        (fun instr_builder ->
          let instr =
            match instr_builder with
            | AI.Push arg -> Push arg
            | AI.Pop arg -> Pop arg
            | AI.Mov (source, dest) -> Mov (source, dest)
            | AI.Lea (addr, reg) -> Lea (addr, reg)
            | AI.Neg arg -> Neg arg
            | AI.Add (reg, source) -> Add (source, RegisterDest reg)
            | AI.Sub (dest_reg, source) -> Sub (source, RegisterDest dest_reg)
            | AI.IMul (arg_reg, arg_source) -> IMul (arg_source, arg_reg)
            | AI.IDiv source -> IDiv source
            | AI.Not arg -> Not arg
            | AI.And (reg, source) -> Add (source, RegisterDest reg)
            | AI.Or (reg, source) -> Add (source, RegisterDest reg)
            | AI.Cmp (arg1, arg2) -> Cmp (arg1, arg2)
            | AI.Test (arg1, arg2) -> Test (arg1, arg2)
            | AI.SetCmp (set_cmp_kind, reg) -> SetCmp (set_cmp_kind, reg)
            | AI.Jmp label -> Jmp label
            | AI.CondJmp (cond_jmp_kind, _continue, jump) -> CondJmp (cond_jmp_kind, jump)
            | AI.Call source -> Call source
            | AI.Leave -> Leave
            | AI.Ret -> Ret
            | AI.Syscall -> Syscall
          in
          Gcx.emit ~gcx instr)
        block.instructions;
      Gcx.finish_block ~gcx;
      match List_utils.last block.instructions with
      | AI.Jmp label ->
        let block = SMap.find label abstract_result.blocks in
        visit_block block
      | AI.CondJmp (_, continue, jump) ->
        let continue_block = SMap.find continue abstract_result.blocks in
        let jump_block = SMap.find jump abstract_result.blocks in
        visit_block continue_block;
        visit_block jump_block
      | _ -> ()
    )
  in
  Gcx.start_init ~gcx;
  List.iter (fun block -> visit_block block) abstract_result.init_blocks;
  Gcx.end_init ~gcx;
  SMap.iter
    (fun name _ ->
      Gcx.reset_visited_blocks ~gcx;
      visit_block (SMap.find name abstract_result.blocks))
    ir.funcs;
  Gcx.finish_blocks ~gcx;
  {
    text = gcx.text;
    data = abstract_result.data;
    rodata = abstract_result.rodata;
    bss = abstract_result.bss;
  }
