open Basic_collections
open X86_gen_context
open X86_instructions

type print_context = { mutable block_print_labels: string IMap.t }

class incoming_jump_blocks_visitor ~(gcx : Gcx.t) =
  object (this)
    inherit X86_visitor.instruction_visitor

    val mutable incoming_jump_blocks = ISet.empty

    method incoming_jump_blocks = incoming_jump_blocks

    method run () = IMap.iter (fun _ block -> this#visit_block block) gcx.blocks_by_id

    method visit_block block = List.iter (this#visit_instruction ~block) block.instructions

    method! visit_block_edge ~block:_ next_block_id =
      incoming_jump_blocks <- ISet.add next_block_id incoming_jump_blocks
  end

let find_incoming_jump_blocks ~gcx =
  let visitor = new incoming_jump_blocks_visitor ~gcx in
  visitor#run ();
  visitor#incoming_jump_blocks

let mk_pcx ~gcx =
  let pcx = { block_print_labels = IMap.empty } in
  (* Determine labels for every unlabed block in program that has an incoming jump *)
  let max_label_id = ref 0 in
  let incoming_jump_blocks = find_incoming_jump_blocks ~gcx in
  IMap.iter
    (fun _ func ->
      List.iter
        (fun block ->
          let open Block in
          match block.label with
          | Some _ -> ()
          | None ->
            if ISet.mem block.id incoming_jump_blocks then (
              let id = !max_label_id in
              max_label_id := !max_label_id + 1;
              pcx.block_print_labels <-
                IMap.add block.id (".L" ^ string_of_int id) pcx.block_print_labels
            ))
        func.Function.blocks)
    gcx.funcs_by_id;
  pcx

let mk_buf ?(size = 1024) () = Buffer.create size

let add_char ~buf c = Buffer.add_char buf c

let add_string ~buf str = Buffer.add_string buf str

let add_indent ~buf = Buffer.add_string buf "  "

let add_line ~buf ?(indent = true) f =
  if indent then add_indent ~buf;
  f buf;
  add_char ~buf '\n'

let add_blank_line ~buf = add_char ~buf '\n'

let add_label_line ~buf label =
  add_line ~buf ~indent:false (fun buf ->
      add_string ~buf label;
      add_char ~buf ':')

let quote_string str =
  let buf = mk_buf ~size:16 () in
  String.iter
    (fun c ->
      match c with
      | '"' -> add_string ~buf "\\\""
      | '\\' -> add_string ~buf "\\\\"
      | '\n' -> add_string ~buf "\\n"
      | '\t' -> add_string ~buf "\\t"
      | '\r' -> add_string ~buf "\\r"
      | c -> add_char ~buf c)
    str;
  "\"" ^ Buffer.contents buf ^ "\""

let pp_size_suffix ~buf size =
  add_char
    ~buf
    (match size with
    | Size8 -> 'b'
    | Size16 -> 'w'
    | Size32 -> 'l'
    | Size64 -> 'q')

let pp_label_debug_prefix ~buf block_id =
  if Opts.dump_debug () then (
    add_char ~buf '(';
    add_string ~buf (string_of_int block_id);
    add_string ~buf ") "
  )

let pp_label ~pcx block =
  let open Block in
  match block.label with
  | Some label -> Some label
  | None -> IMap.find_opt block.id pcx.block_print_labels

let pp_condition_code cc =
  match cc with
  | E -> "e"
  | NE -> "ne"
  | L -> "l"
  | G -> "g"
  | LE -> "le"
  | GE -> "ge"

let pp_immediate ~buf imm =
  add_char ~buf '$';
  add_string
    ~buf
    (match imm with
    | Imm8 imm
    | Imm16 imm ->
      string_of_int imm
    | Imm32 imm -> Int32.to_string imm
    | Imm64 imm -> Int64.to_string imm)

let pp_virtual_stack_slot ~buf vreg =
  add_string ~buf "VSLOT:";
  add_string ~buf (string_of_int vreg.VReg.id)

let pp_function_stack_argument ~buf vreg =
  add_string ~buf "STACK_ARG:";
  add_string ~buf (string_of_int vreg.VReg.id)

let rec pp_register ~gcx ~buf ~size reg =
  let reg_alias = VReg.get_vreg_alias reg in
  match reg_alias.resolution with
  | Physical reg ->
    add_char ~buf '%';
    add_string ~buf (string_of_sized_reg reg size);
    if Opts.dump_debug () then (
      add_char ~buf ':';
      add_string ~buf (string_of_int reg_alias.id)
    )
  | StackSlot (PhysicalAddress _ as addr) -> pp_memory_address ~gcx ~buf addr
  | StackSlot (VirtualStackSlot vreg) -> pp_virtual_stack_slot ~buf vreg
  | StackSlot (FunctionStackArgument vreg) -> pp_function_stack_argument ~buf vreg
  | Unresolved ->
    add_char ~buf '%';
    add_string ~buf (string_of_int reg_alias.id)
  | Alias _ -> failwith "Alias cannot be final resolution"

and pp_memory_address ~gcx ~buf mem =
  match mem with
  | VirtualStackSlot vreg ->
    (match VReg.get_vreg_resolution vreg with
    | StackSlot (PhysicalAddress _ as addr) -> pp_memory_address ~gcx ~buf addr
    | StackSlot (VirtualStackSlot vreg) -> pp_virtual_stack_slot ~buf vreg
    | _ -> failwith "Virtual stack slot must have been resolved before printing")
  | FunctionStackArgument vreg ->
    (match VReg.get_vreg_resolution vreg with
    | StackSlot (PhysicalAddress _ as addr) -> pp_memory_address ~gcx ~buf addr
    | StackSlot (FunctionStackArgument vreg) -> pp_function_stack_argument ~buf vreg
    | _ -> failwith "Function stack argument must have been resolved before printing")
  | PhysicalAddress mem ->
    begin
      match mem.offset with
      | None -> ()
      | Some (ImmediateOffset imm) -> add_string ~buf (Int64.to_string imm)
      | Some (LabelOffset label) -> add_string ~buf label
    end;
    add_char ~buf '(';
    begin
      match mem.base with
      | None -> ()
      | Some reg -> pp_register ~gcx ~buf ~size:Size64 reg
    end;
    begin
      match mem.index_and_scale with
      | None -> ()
      | Some (index_register, scale) ->
        add_string ~buf ", ";
        pp_register ~gcx ~buf ~size:Size64 index_register;
        begin
          match scale with
          | Scale1 -> ()
          | Scale2 -> add_string ~buf ", 2"
          | Scale4 -> add_string ~buf ", 4"
          | Scale8 -> add_string ~buf ", 8"
        end
    end;
    add_char ~buf ')'

let pp_mem ~gcx ~buf ~size mem =
  let open Instruction in
  match mem with
  | Reg reg -> pp_register ~gcx ~buf ~size reg
  | Mem addr -> pp_memory_address ~gcx ~buf addr

let pp_instruction ~gcx ~pcx ~buf instruction =
  let open Instruction in
  if Opts.dump_debug () then (
    let instr_id = string_of_int (fst instruction) in
    let padding_size = max 0 (4 - String.length instr_id) in
    add_char ~buf '#';
    add_string ~buf instr_id;
    add_string ~buf (String.make padding_size ' ')
  );
  add_line ~buf (fun buf ->
      let add_string = add_string ~buf in
      let pp_op op =
        add_string op;
        add_char ~buf ' '
      in
      let pp_sized_op op size =
        add_string op;
        pp_size_suffix ~buf size;
        add_char ~buf ' '
      in
      let pp_register = pp_register ~gcx ~buf in
      let pp_mem = pp_mem ~gcx ~buf in
      let pp_immediate = pp_immediate ~buf in
      let pp_memory_address = pp_memory_address ~gcx ~buf in
      let pp_args_separator () = add_string ", " in
      match snd instruction with
      | PushI imm ->
        pp_op "push";
        pp_immediate imm
      | PushM mem ->
        pp_op "push";
        pp_mem ~size:Size64 mem
      | PopM mem ->
        pp_op "pop";
        pp_mem ~size:Size64 mem
      | MovMM (size, src_mem, dest_mem) ->
        pp_sized_op "mov" size;
        pp_mem ~size src_mem;
        pp_args_separator ();
        pp_mem ~size dest_mem
      | MovIM (src_imm, dest_mem) ->
        let size = size_of_immediate src_imm in
        pp_sized_op "mov" size;
        pp_immediate src_imm;
        pp_args_separator ();
        pp_mem ~size dest_mem
      | Lea (size, mem, reg) ->
        pp_sized_op "lea" size;
        pp_memory_address mem;
        pp_args_separator ();
        pp_register ~size reg
      (* Numeric operations *)
      | NegM (size, mem) ->
        pp_sized_op "neg" size;
        pp_mem ~size mem
      | AddMM (size, src_mem, dest_mem) ->
        pp_sized_op "add" size;
        pp_mem ~size src_mem;
        pp_args_separator ();
        pp_mem ~size dest_mem
      | AddIM (imm, dest_mem) ->
        let size = size_of_immediate imm in
        pp_sized_op "add" size;
        pp_immediate imm;
        pp_args_separator ();
        pp_mem ~size dest_mem
      | SubMM (size, src_mem, dest_mem) ->
        pp_sized_op "sub" size;
        pp_mem ~size src_mem;
        pp_args_separator ();
        pp_mem ~size dest_mem
      | SubIM (imm, dest_mem) ->
        let size = size_of_immediate imm in
        pp_sized_op "sub" size;
        pp_immediate imm;
        pp_args_separator ();
        pp_mem ~size dest_mem
      | IMulMR (size, src_mem, dest_reg) ->
        pp_sized_op "imul" size;
        pp_mem ~size src_mem;
        pp_args_separator ();
        pp_register ~size dest_reg
      | IMulMIR (src_mem, imm, dest_reg) ->
        let size = size_of_immediate imm in
        pp_sized_op "imul" size;
        pp_mem ~size src_mem;
        pp_args_separator ();
        pp_immediate imm;
        pp_args_separator ();
        pp_register ~size dest_reg
      | IDiv (size, mem) ->
        pp_sized_op "idiv" size;
        pp_mem ~size mem
      (* Bitwise operations *)
      | NotM (size, mem) ->
        pp_sized_op "not" size;
        pp_mem ~size mem
      | AndMM (size, src_mem, dest_mem) ->
        pp_sized_op "and" size;
        pp_mem ~size src_mem;
        pp_args_separator ();
        pp_mem ~size dest_mem
      | AndIM (imm, dest_mem) ->
        let size = size_of_immediate imm in
        pp_sized_op "and" size;
        pp_immediate imm;
        pp_args_separator ();
        pp_mem ~size dest_mem
      | OrMM (size, src_mem, dest_mem) ->
        pp_sized_op "or" size;
        pp_mem ~size src_mem;
        pp_args_separator ();
        pp_mem ~size dest_mem
      | OrIM (imm, dest_mem) ->
        let size = size_of_immediate imm in
        pp_sized_op "or" size;
        pp_immediate imm;
        pp_args_separator ();
        pp_mem ~size dest_mem
      | XorMM (size, src_mem, dest_mem) ->
        pp_sized_op "xor" size;
        pp_mem ~size src_mem;
        pp_args_separator ();
        pp_mem ~size dest_mem
      (* Comparisons - arguments intentionally flipped *)
      | CmpMM (size, mem1, mem2) ->
        pp_sized_op "cmp" size;
        pp_mem ~size mem2;
        pp_args_separator ();
        pp_mem ~size mem1
      | CmpMI (mem, imm) ->
        let size = size_of_immediate imm in
        pp_sized_op "cmp" size;
        pp_immediate imm;
        pp_args_separator ();
        pp_mem ~size mem
      | TestMR (size, mem, reg) ->
        pp_sized_op "test" size;
        pp_mem ~size mem;
        pp_args_separator ();
        pp_register ~size reg
      | SetCC (cc, mem) ->
        pp_op ("set" ^ pp_condition_code cc);
        pp_mem ~size:Size8 mem
      (* Control flow *)
      | Jmp block_id ->
        let block = IMap.find block_id gcx.Gcx.blocks_by_id in
        pp_op "jmp";
        pp_label_debug_prefix ~buf block_id;
        add_string (Option.get (pp_label ~pcx block))
      | JmpCC (cc, block_id) ->
        let block = IMap.find block_id gcx.Gcx.blocks_by_id in
        pp_op ("j" ^ pp_condition_code cc);
        pp_label_debug_prefix ~buf block_id;
        add_string (Option.get (pp_label ~pcx block))
      | CallM (size, mem) ->
        pp_sized_op "call" size;
        pp_mem ~size mem
      | CallL label ->
        pp_op "call";
        add_string label
      | Leave -> add_string "leave"
      | Ret -> add_string "ret"
      | Syscall -> add_string "syscall")

let pp_data ~buf (data : data) =
  (* Data blocks have the form:
     label:
       .directive immediate *)
  add_label_line ~buf data.label;
  add_line ~buf (fun buf ->
      let (directive_string, value_string) =
        match data.value with
        | ImmediateData (Imm8 imm) -> ("byte", string_of_int imm)
        | ImmediateData (Imm16 imm) -> ("value", string_of_int imm)
        | ImmediateData (Imm32 imm) -> ("long", Int32.to_string imm)
        | ImmediateData (Imm64 imm) -> ("quad", Int64.to_string imm)
        | AsciiData str -> ("ascii", quote_string str)
      in
      add_char ~buf '.';
      add_string ~buf directive_string;
      add_char ~buf ' ';
      add_string ~buf value_string)

let pp_block ~gcx ~pcx ~buf (block : virtual_block) =
  (match pp_label ~pcx block with
  | None -> ()
  | Some label ->
    pp_label_debug_prefix ~buf block.id;
    add_label_line ~buf label);
  List.iter (pp_instruction ~gcx ~pcx ~buf) block.instructions

let pp_x86_program ~gcx =
  let open Gcx in
  let pcx = mk_pcx ~gcx in
  let buf = mk_buf () in
  (* Add global directive *)
  add_line ~buf (fun buf -> add_string ~buf ".global _main");
  if gcx.bss <> [] then
    List.iter
      (fun { label; size } ->
        add_line ~buf (fun buf -> add_string ~buf (Printf.sprintf ".lcomm %s, %d" label size)))
      gcx.bss;
  (* Add rodata section *)
  if gcx.rodata <> [] then (
    add_blank_line ~buf;
    add_line ~buf (fun buf -> add_string ~buf ".section \"r\", rodata");
    List.iter (pp_data ~buf) gcx.rodata
  );
  (* Add data section *)
  if gcx.data <> [] then (
    add_blank_line ~buf;
    add_line ~buf (fun buf -> add_string ~buf ".data");
    List.iter (pp_data ~buf) gcx.data
  );
  (* Add text section *)
  add_blank_line ~buf;
  add_line ~buf (fun buf -> add_string ~buf ".text");
  IMap.iter (fun _ func -> List.iter (pp_block ~gcx ~pcx ~buf) func.Function.blocks) gcx.funcs_by_id;
  Buffer.contents buf
