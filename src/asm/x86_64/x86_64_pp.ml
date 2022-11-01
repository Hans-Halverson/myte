open Asm
open Asm_builders
open Asm_instruction_definition
open Asm_pp
open X86_64_gen_context
open X86_64_instruction_definitions
open X86_64_register

let mk_pcx ~(funcs : FunctionSet.t) =
  (* Find set of all blocks that have an incoming jump *)
  let incoming_jump_blocks = ref BlockSet.empty in
  funcs_iter_blocks funcs (fun block ->
      iter_instructions block (fun instr ->
          match instr with
          | { instr = `Jmp | `JmpCC _; operands = [| { value = Block next_block; _ } |]; _ } ->
            incoming_jump_blocks := BlockSet.add next_block !incoming_jump_blocks
          | _ -> ()));

  mk_pcx ~funcs ~incoming_jump_blocks:!incoming_jump_blocks

let pp_integer_size_suffix ~buf size =
  add_char
    ~buf
    (match size with
    | X86_64.Size8 -> 'b'
    | Size16 -> 'w'
    | Size32 -> 'l'
    | Size64 -> 'q'
    | Size128 -> failwith "Invalid integer operand size")

let pp_condition_code cc =
  match cc with
  | X86_64.E -> "e"
  | NE -> "ne"
  | L -> "l"
  | G -> "g"
  | LE -> "le"
  | GE -> "ge"
  | B -> "b"
  | BE -> "be"
  | A -> "a"
  | AE -> "ae"
  | P -> "p"
  | NP -> "np"

let pp_immediate ~buf imm =
  add_char ~buf '$';
  add_string
    ~buf
    (match imm with
    | Imm8 imm -> Int8.to_string imm
    | Imm16 imm -> string_of_int imm
    | Imm32 imm -> Int32.to_string imm
    | Imm64 imm -> Int64.to_string imm)

let pp_virtual_stack_slot ~buf op =
  add_string ~buf "VSLOT:";
  add_string ~buf (string_of_int op.Operand.id)

let pp_stack_slot ~buf op =
  add_string ~buf "STACK_SLOT:";
  add_string ~buf (string_of_int op.Operand.id)

let pp_sized_register ~buf reg size =
  add_char ~buf '%';
  add_string ~buf (string_of_sized_reg reg size)

let rec pp_operand ~gcx ~pcx ~buf ~size op =
  match op.Operand.value with
  | PhysicalRegister reg ->
    pp_sized_register ~buf reg size;
    if Opts.dump_debug () then (
      add_char ~buf ':';
      add_string ~buf (string_of_int op.id)
    )
  | Immediate imm -> pp_immediate ~buf imm
  | X86_64_MemoryAddress addr -> pp_memory_address ~gcx ~pcx ~buf addr
  | Function func -> add_string ~buf func.label
  | Block block ->
    pp_label_debug_prefix ~buf block;
    add_string ~buf (Option.get (pp_label ~pcx block))
  (* The following operand types should be resolved before printing non-virtual asm *)
  | StackSlot _ when Opts.dump_virtual_asm () -> pp_stack_slot ~buf op
  | VirtualStackSlot when Opts.dump_virtual_asm () -> pp_virtual_stack_slot ~buf op
  | VirtualRegister when Opts.dump_virtual_asm () ->
    add_char ~buf '%';
    add_string ~buf (string_of_int op.id)
  | Label _ -> failwith "Labels not used in x86 assembly"
  | VirtualRegister
  | StackSlot _
  | VirtualStackSlot ->
    failwith "Must be resolved before printing"

and pp_memory_address ~gcx ~pcx ~buf mem =
  begin
    match mem.offset with
    | None
    | Some (ImmediateOffset 0l) ->
      ()
    | Some (ImmediateOffset imm) -> add_string ~buf (Int32.to_string imm)
    | Some (LabelOffset label) -> add_string ~buf label
  end;
  add_char ~buf '(';
  begin
    match mem.base with
    | NoBase -> ()
    | IPBase -> add_string ~buf "%rip"
    | RegBase reg -> pp_operand ~gcx ~pcx ~buf ~size:Size64 reg
  end;
  begin
    match mem.index_and_scale with
    | None -> ()
    | Some (index_register, scale) ->
      add_string ~buf ", ";
      pp_operand ~gcx ~pcx ~buf ~size:Size64 index_register;
      begin
        match scale with
        | Scale1 -> ()
        | Scale2 -> add_string ~buf ", 2"
        | Scale4 -> add_string ~buf ", 4"
        | Scale8 -> add_string ~buf ", 8"
      end
  end;
  add_char ~buf ')'

let pp_instruction ~gcx ~pcx ~buf instr =
  let open Instruction in
  pp_instr_debug_prefix ~buf ~instr;
  add_line ~buf (fun buf ->
      let add_string = add_string ~buf in
      let pp_op op =
        add_string op;
        add_char ~buf ' '
      in
      let pp_sized_op op size =
        add_string op;
        pp_integer_size_suffix ~buf size;
        add_char ~buf ' '
      in
      let pp_double_sized_op op size1 size2 =
        add_string op;
        pp_integer_size_suffix ~buf size1;
        pp_integer_size_suffix ~buf size2;
        add_char ~buf ' '
      in
      let pp_no_args_op op = add_string op in
      let pp_operand = pp_operand ~gcx ~pcx ~buf in
      let pp_args_separator () = add_string ", " in
      let pp_operands () =
        let last_operand_idx = Array.length instr.operands - 1 in
        Array.iteri
          (fun i operand ->
            let size =
              match operand.Operand.value with
              | PhysicalRegister _ -> instr_operand_size instr.instr i
              | _ -> Size64
            in
            pp_operand ~size operand;
            if i != last_operand_idx then pp_args_separator ())
          instr.operands
      in
      match instr.instr with
      | `PushI ->
        pp_op "push";
        pp_operands ()
      | `PushM ->
        pp_op "push";
        pp_operands ()
      | `PopM ->
        pp_op "pop";
        pp_operands ()
      | `MovMM size ->
        pp_sized_op "mov" size;
        pp_operands ()
      | `MovIM size ->
        pp_sized_op "mov" size;
        pp_operands ()
      | `MovSX (src_size, dest_size) ->
        pp_double_sized_op "movs" src_size dest_size;
        pp_operands ()
      | `MovZX (src_size, dest_size) ->
        pp_double_sized_op "movz" src_size dest_size;
        pp_operands ()
      | `Lea size ->
        pp_sized_op "lea" size;
        pp_operands ()
      (* Numeric operations *)
      | `NegM size ->
        pp_sized_op "neg" size;
        pp_operands ()
      | `AddMM size ->
        pp_sized_op "add" size;
        pp_operands ()
      | `AddIM size ->
        pp_sized_op "add" size;
        pp_operands ()
      | `SubMM size ->
        pp_sized_op "sub" size;
        pp_operands ()
      | `SubIM size ->
        pp_sized_op "sub" size;
        pp_operands ()
      | `IMulMR size
      | `IMulIMR size ->
        pp_sized_op "imul" size;
        pp_operands ()
      | `IDiv size ->
        pp_sized_op "idiv" size;
        pp_operands ()
      | `AddSD ->
        pp_op "addsd";
        pp_operands ()
      | `SubSD ->
        pp_op "subsd";
        pp_operands ()
      | `MulSD ->
        pp_op "mulsd";
        pp_operands ()
      | `DivSD ->
        pp_op "divsd";
        pp_operands ()
      | `XorPD ->
        pp_op "xorpd";
        pp_operands ()
      (* Bitwise operations *)
      | `NotM size ->
        pp_sized_op "not" size;
        pp_operands ()
      | `AndMM size ->
        pp_sized_op "and" size;
        pp_operands ()
      | `AndIM size ->
        pp_sized_op "and" size;
        pp_operands ()
      | `OrMM size ->
        pp_sized_op "or" size;
        pp_operands ()
      | `OrIM size ->
        pp_sized_op "or" size;
        pp_operands ()
      | `XorMM size ->
        pp_sized_op "xor" size;
        pp_operands ()
      | `XorIM size ->
        pp_sized_op "xor" size;
        pp_operands ()
      | `ShlM size ->
        pp_sized_op "shl" size;
        pp_sized_register ~buf `C Size8;
        pp_args_separator ();
        pp_operand ~size instr.operands.(0)
      | `ShlI size ->
        pp_sized_op "shl" size;
        pp_operands ()
      | `ShrM size ->
        pp_sized_op "shr" size;
        pp_sized_register ~buf `C Size8;
        pp_args_separator ();
        pp_operand ~size instr.operands.(0)
      | `ShrI size ->
        pp_sized_op "shr" size;
        pp_operands ()
      | `SarM size ->
        pp_sized_op "sar" size;
        pp_sized_register ~buf `C Size8;
        pp_args_separator ();
        pp_operand ~size instr.operands.(0)
      | `SarI size ->
        pp_sized_op "sar" size;
        pp_operands ()
      (* Comparisons - arguments intentionally flipped *)
      | `CmpMM size ->
        pp_sized_op "cmp" size;
        pp_operand ~size instr.operands.(1);
        pp_args_separator ();
        pp_operand ~size instr.operands.(0)
      | `CmpMI size ->
        pp_sized_op "cmp" size;
        pp_operand ~size instr.operands.(1);
        pp_args_separator ();
        pp_operand ~size instr.operands.(0)
      | `UComiSD ->
        pp_op "ucomisd";
        pp_operand ~size:Size64 instr.operands.(1);
        pp_args_separator ();
        pp_operand ~size:Size64 instr.operands.(0)
      | `TestMR size ->
        pp_sized_op "test" size;
        pp_operands ()
      | `SetCC cc ->
        pp_op ("set" ^ pp_condition_code cc);
        pp_operands ()
      | `ConvertDouble size ->
        let op =
          match size with
          | Size16 -> "cwd"
          | Size32 -> "cdq"
          | Size64 -> "cqo"
          | Size8
          | Size128 ->
            failwith "ConvertDouble cannot have 1-byte or 16-byte size"
        in
        pp_op op
      | `ConvertIntToFloat _ ->
        pp_op "cvtsi2sd";
        pp_operands ()
      | `ConvertFloatToInt _ ->
        pp_op "cvttsd2si";
        pp_operands ()
      (* Control flow *)
      | `Jmp ->
        pp_op "jmp";
        pp_operands ()
      | `JmpCC cc ->
        pp_op ("j" ^ pp_condition_code cc);
        pp_operands ()
      | `CallM (size, _, _) ->
        pp_sized_op "call" size;
        add_char ~buf '*';
        pp_operands ()
      | `CallL _ ->
        pp_op "call";
        pp_operands ()
      | `Ret -> pp_no_args_op "ret"
      | _ -> failwith "Unknown X86_64 instr")

let pp_data_section_bitmap ~buf (bitmap : X86_64_bitmaps.data_section_bitmap) section_name =
  (* Write bitmap size *)
  add_label_line ~buf (section_name ^ "_bitmap_size");
  add_line ~buf (fun buf ->
      add_string ~buf ".quad ";
      add_string ~buf (string_of_int bitmap.num_words));

  (* Write bitmap data *)
  add_label_line ~buf (section_name ^ "_bitmap");
  let next_byte_to_write = ref 0 in
  let bytes_remaining = ref (Bytes.length bitmap.bitmap) in

  (* Write .quads while there are enough bytes remaining *)
  while !bytes_remaining >= 8 do
    let quad = Bytes.get_int64_be bitmap.bitmap !next_byte_to_write in
    add_line ~buf (fun buf -> add_string ~buf (Printf.sprintf ".quad 0x%LX" quad));
    next_byte_to_write := !next_byte_to_write + 8;
    bytes_remaining := !bytes_remaining - 8
  done;

  (* Write all 0-7 remaining bytes in a single .byte directive *)
  if !bytes_remaining > 0 then
    add_line ~buf (fun buf ->
        add_string ~buf ".byte ";
        for i = 0 to !bytes_remaining - 1 do
          if i <> 0 then add_string ~buf ", ";
          let byte = Bytes.get_uint8 bitmap.bitmap (!next_byte_to_write + i) in
          add_string ~buf (Printf.sprintf "0x%X" byte)
        done)

let pp_block ~gcx ~pcx ~buf (block : Block.t) =
  (match pp_label ~pcx block with
  | None -> ()
  | Some label ->
    pp_label_debug_prefix ~buf block;
    add_label_line ~buf label);
  iter_instructions block (pp_instruction ~gcx ~pcx ~buf)

let pp_program ~gcx =
  let open Gcx in
  let pcx = mk_pcx ~funcs:gcx.funcs in
  let buf = mk_buf () in

  pp_global_directives ~buf;
  pp_data_sections ~buf ~bss:gcx.bss ~data:gcx.data;

  (* Add text section *)
  add_blank_line ~buf;
  add_line ~buf (fun buf -> add_string ~buf ".text");
  FunctionSet.iter (fun func -> List.iter (pp_block ~gcx ~pcx ~buf) func.Function.blocks) gcx.funcs;

  (* Add data section bitmaps *)
  if Opts.custom_gc () && write_full_asm () then (
    let bss_bitmap = X86_64_bitmaps.gen_data_section_bitmap gcx.bss in
    pp_data_section_bitmap ~buf bss_bitmap "bss";
    let data_bitmap = X86_64_bitmaps.gen_data_section_bitmap gcx.data in
    pp_data_section_bitmap ~buf data_bitmap "data"
  );

  Buffer.contents buf
