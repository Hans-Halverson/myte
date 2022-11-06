open Aarch64_gen_context
open Aarch64_instruction_definitions
open Aarch64_register
open Asm
open Asm_builders
open Asm_instruction_definition
open Asm_pp

let mk_pcx ~(funcs : FunctionSet.t) =
  (* Find set of all blocks that have an incoming jump *)
  let incoming_jump_blocks = ref BlockSet.empty in
  funcs_iter_blocks funcs (fun block ->
      iter_instructions block (fun instr ->
          match instr with
          | { instr = `B | `BCond _; operands = [| { value = Block next_block; _ } |]; _ }
          | { instr = `Cbz _; operands = [| _; { value = Block next_block; _ } |]; _ } ->
            incoming_jump_blocks := BlockSet.add next_block !incoming_jump_blocks
          | _ -> ()));

  mk_pcx ~funcs ~incoming_jump_blocks:!incoming_jump_blocks

let pp_immediate ~buf imm =
  add_char ~buf '#';
  add_string
    ~buf
    (match imm with
    | Imm8 imm -> Int8.to_string imm
    | Imm16 imm -> string_of_int imm
    | Imm32 imm -> Int32.to_string imm
    | Imm64 imm -> Int64.to_string imm)

let pp_sized_register ~buf reg size = add_string ~buf (string_of_sized_reg reg size)

let pp_operand ~pcx ~buf ~size op =
  match op.Operand.value with
  | PhysicalRegister reg ->
    pp_sized_register ~buf reg size;
    if Opts.dump_debug () then (
      add_char ~buf ':';
      add_string ~buf (string_of_int op.id)
    )
  | Immediate imm -> pp_immediate ~buf imm
  | FloatImmediate float ->
    add_char ~buf '#';
    add_string ~buf (Float.to_string float)
  | Function func -> add_string ~buf func.label
  | Block block ->
    pp_label_debug_prefix ~buf block;
    add_string ~buf (Option.get (pp_label ~pcx block))
  | Label label -> add_string ~buf label
  (* The following operand types should be resolved before printing non-virtual asm *)
  | StackSlot _ when Opts.dump_virtual_asm () ->
    add_string ~buf "STACK_SLOT:";
    add_string ~buf (string_of_int op.Operand.id)
  | VirtualStackSlot when Opts.dump_virtual_asm () ->
    add_string ~buf "VSLOT:";
    add_string ~buf (string_of_int op.Operand.id)
  | VirtualRegister when Opts.dump_virtual_asm () ->
    add_char ~buf '%';
    add_string ~buf (string_of_int op.id)
  | X86_64_MemoryAddress _ -> failwith "x86_64 memory address not allowed in aarch64 assembly"
  | VirtualRegister
  | StackSlot _
  | VirtualStackSlot ->
    failwith "Must be resolved before printing"

let pp_extend ~buf (extend : AArch64.extend) =
  let pp str =
    add_string ~buf ", ";
    add_string ~buf str
  in
  match extend with
  (* Noops which do not need to be written *)
  | UXTX
  | SXTX ->
    ()
  | UXTB -> pp "uxtb"
  | UXTH -> pp "uxth"
  | UXTW -> pp "uxtw"
  | SXTB -> pp "sxtb"
  | SXTH -> pp "sxth"
  | SXTW -> pp "sxtw"

let pp_immediate_address ~pcx ~buf (mode : AArch64.addressing_mode) reg_op imm_op =
  let add_char = add_char ~buf in
  let add_string = add_string ~buf in
  let pp_operand = pp_operand ~pcx ~buf ~size:Size64 in
  add_char '[';
  pp_operand reg_op;
  match mode with
  | Offset ->
    add_string ", ";
    pp_operand imm_op;
    add_char ']'
  | PreIndex ->
    add_string ", ";
    pp_operand imm_op;
    add_string "]!"
  | PostIndex ->
    add_string "], ";
    pp_operand imm_op

let pp_offset_address
    ~pcx ~buf (instr : instr) (operands : Operand.t array) (extend : AArch64.addressing_extend) =
  let add_char = add_char ~buf in
  let add_string = add_string ~buf in
  let pp_operand i = pp_operand ~pcx ~buf ~size:(instr_register_size instr i) operands.(i) in
  let num_operands = Array.length operands in
  add_char '[';
  pp_operand 1;
  if num_operands >= 3 then (
    add_string ", ";
    pp_operand 2;
    match extend with
    | LSL ->
      if num_operands == 4 && not (is_zero_immediate (cast_to_immediate operands.(3))) then (
        add_string ", lsl ";
        pp_operand 3
      )
    | UXTW ->
      add_string ", uxtw";
      if num_operands == 4 then (
        add_char ' ';
        pp_operand 3
      )
    | SXTW ->
      add_string ", sxtw";
      if num_operands == 4 then (
        add_char ' ';
        pp_operand 3
      )
  );
  add_char ']'

let string_of_cond (cond : AArch64.cond) =
  match cond with
  | EQ -> "eq"
  | NE -> "ne"
  | LT -> "lt"
  | LE -> "le"
  | GT -> "gt"
  | GE -> "ge"
  | MI -> "mi"
  | LS -> "ls"

let ldr_suffix
    (dest_size : AArch64.register_size) (load_size : AArch64.subregister_size) (is_signed : bool) =
  match (is_signed, load_size, dest_size) with
  | (_, X, _)
  | (_, W, Size32)
  | (false, W, Size64) ->
    ""
  | (false, B, _) -> "b"
  | (false, H, _) -> "h"
  | (true, B, _) -> "sb"
  | (true, H, _) -> "sh"
  | (true, W, Size64) -> "sw"

let str_suffix (store_size : AArch64.subregister_size) =
  match store_size with
  | X
  | W ->
    ""
  | B -> "b"
  | H -> "h"

let pp_instruction ~pcx ~buf instr =
  let pp_operand = pp_operand ~pcx ~buf in
  pp_instr_debug_prefix ~buf ~instr;
  add_line ~buf (fun buf ->
      let add_string = add_string ~buf in
      let pp_args_separator () = add_string ", " in
      let pp_operands () =
        let last_operand_idx = Array.length instr.operands - 1 in
        Array.iteri
          (fun i operand ->
            let size =
              match operand.Operand.value with
              | PhysicalRegister _ -> instr_register_size instr.instr i
              | _ -> Size64
            in
            pp_operand ~size operand;
            if i != last_operand_idx then pp_args_separator ())
          instr.operands
      in
      let pp_lsl_immediate ~size op =
        (* A zero shift is left out *)
        if not (is_zero_immediate (cast_to_immediate op)) then (
          pp_args_separator ();
          add_string "lsl ";
          pp_operand ~size op
        )
      in
      let pp_op op =
        add_string op;
        add_char ~buf ' '
      in
      let pp_op_and_operands op =
        pp_op op;
        pp_operands ()
      in
      let pp_no_args_op op = add_string op in
      let operands = instr.operands in
      match instr.instr with
      | `MovR _ -> pp_op_and_operands "mov"
      | `MovI (size, suffix) ->
        let op =
          match suffix with
          | Z -> "movz"
          | N -> "movn"
          | K -> "movk"
        in
        pp_op op;
        pp_operand ~size operands.(0);
        pp_args_separator ();
        pp_operand ~size operands.(1);
        pp_lsl_immediate ~size operands.(2)
      | `LdrI (dest_size, load_size, is_signed, mode) ->
        pp_op ("ldr" ^ ldr_suffix dest_size load_size is_signed);
        pp_operand ~size:dest_size operands.(0);
        pp_args_separator ();
        pp_immediate_address ~pcx ~buf mode operands.(1) operands.(2)
      | `StrI (store_size, mode) ->
        pp_op ("str" ^ str_suffix store_size);
        pp_operand ~size:(instr_register_size instr.instr 0) operands.(0);
        pp_args_separator ();
        pp_immediate_address ~pcx ~buf mode operands.(1) operands.(2)
      | `LdrR (dest_size, load_size, is_signed, extend) ->
        pp_op ("ldr" ^ ldr_suffix dest_size load_size is_signed);
        pp_operand ~size:dest_size operands.(0);
        pp_args_separator ();
        pp_offset_address ~pcx ~buf instr.instr operands extend
      | `StrR (store_size, extend) ->
        pp_op ("str" ^ str_suffix store_size);
        pp_operand ~size:(instr_register_size instr.instr 0) operands.(0);
        pp_args_separator ();
        pp_offset_address ~pcx ~buf instr.instr operands extend
      | `Ldp (size, mode) ->
        pp_op "ldp";
        pp_operand ~size operands.(0);
        pp_args_separator ();
        pp_operand ~size operands.(1);
        pp_args_separator ();
        pp_immediate_address ~pcx ~buf mode operands.(2) operands.(3)
      | `Stp (size, mode) ->
        pp_op "stp";
        pp_operand ~size operands.(0);
        pp_args_separator ();
        pp_operand ~size operands.(1);
        pp_args_separator ();
        pp_immediate_address ~pcx ~buf mode operands.(2) operands.(3)
      | `AdrP ->
        pp_op "adrp";
        pp_operands ()
      | `AddI size ->
        pp_op "add";
        pp_operand ~size operands.(0);
        pp_args_separator ();
        pp_operand ~size operands.(1);
        pp_args_separator ();
        pp_operand ~size operands.(2);
        pp_lsl_immediate ~size operands.(3)
      | `AddR (size, extend) ->
        pp_op "add";
        pp_operand ~size operands.(0);
        pp_args_separator ();
        pp_operand ~size operands.(1);
        pp_args_separator ();
        pp_operand ~size operands.(2);
        if Array.length operands == 4 then (
          pp_extend ~buf extend;
          add_char ~buf ' ';
          pp_operand ~size operands.(3)
        )
      | `SubI size ->
        pp_op "sub";
        pp_operand ~size operands.(0);
        pp_args_separator ();
        pp_operand ~size operands.(1);
        pp_args_separator ();
        pp_operand ~size operands.(2);
        pp_lsl_immediate ~size operands.(3)
      | `SubR _ ->
        pp_op "sub";
        pp_operands ()
      | `Mul _ ->
        pp_op "mul";
        pp_operands ()
      | `SDiv _ ->
        pp_op "sdiv";
        pp_operands ()
      | `MSub _ ->
        pp_op "msub";
        pp_operands ()
      | `Neg _ ->
        pp_op "neg";
        pp_operands ()
      | `Mvn _ ->
        pp_op "mvn";
        pp_operands ()
      | `AndI _
      | `AndR _ ->
        pp_op "and";
        pp_operands ()
      | `OrrI _
      | `OrrR _ ->
        pp_op "orr";
        pp_operands ()
      | `EorI _
      | `EorR _ ->
        pp_op "eor";
        pp_operands ()
      | `LslI _
      | `LslR _ ->
        pp_op "lsl";
        pp_operands ()
      | `LsrI _
      | `LsrR _ ->
        pp_op "lsr";
        pp_operands ()
      | `AsrI _
      | `AsrR _ ->
        pp_op "asr";
        pp_operands ()
      | `Sbfx _ ->
        pp_op "sbfx";
        pp_operands ()
      | `Ubfx _ ->
        pp_op "ubfx";
        pp_operands ()
      | `CmpI _ ->
        pp_op "cmp";
        pp_operands ()
      | `CmpR (size, extend) ->
        pp_op "cmp";
        pp_operand ~size operands.(0);
        pp_args_separator ();
        pp_operand ~size operands.(1);
        pp_extend ~buf extend
      | `CmnI _ ->
        pp_op "cmn";
        pp_operands ()
      | `CSet (_, cond) ->
        pp_op "cset";
        pp_operands ();
        pp_args_separator ();
        add_string (string_of_cond cond)
      | `Sxt (_, subregister_size) ->
        let suffix =
          match subregister_size with
          | B -> "b"
          | H -> "h"
          | W -> "w"
          | X -> failwith "Invalid subregister size for sdiv"
        in
        pp_op ("sxt" ^ suffix);
        pp_operands ()
      | `FMovI
      | `FMovR ->
        pp_op "fmov";
        pp_operands ()
      | `FAdd ->
        pp_op "fadd";
        pp_operands ()
      | `FSub ->
        pp_op "fsub";
        pp_operands ()
      | `FMul ->
        pp_op "fmul";
        pp_operands ()
      | `FDiv ->
        pp_op "fdiv";
        pp_operands ()
      | `FNeg ->
        pp_op "fneg";
        pp_operands ()
      | `FCmpZ ->
        pp_op "fcmp";
        pp_operands ();
        pp_args_separator ();
        add_string "#0."
      | `FCmpR ->
        pp_op "fcmp";
        pp_operands ()
      | `Ret -> pp_no_args_op "ret"
      | `B ->
        pp_op "b";
        pp_operands ()
      | `BCond cond ->
        let op = "b." ^ string_of_cond cond in
        pp_op op;
        pp_operands ()
      | `Cbz _ ->
        pp_op "cbz";
        pp_operands ()
      | `BL _ ->
        pp_op "bl";
        pp_operands ()
      | `BLR _ ->
        pp_op "blr";
        pp_operands ()
      | _ -> failwith "Unknown AArch64 instr")

let pp_block ~pcx ~buf (block : Block.t) =
  (match pp_label ~pcx block with
  | None -> ()
  | Some label ->
    pp_label_debug_prefix ~buf block;
    add_label_line ~buf label);
  iter_instructions block (pp_instruction ~pcx ~buf)

let pp_program ~(gcx : Gcx.t) : string =
  let pcx = mk_pcx ~funcs:gcx.funcs in
  let buf = mk_buf () in

  pp_global_directives ~buf;
  pp_data_sections ~buf ~bss:gcx.bss ~data:gcx.data;

  (* Add text section *)
  add_blank_line ~buf;
  add_line ~buf (fun buf -> add_string ~buf ".text");
  FunctionSet.iter (fun func -> List.iter (pp_block ~pcx ~buf) func.Function.blocks) gcx.funcs;

  Buffer.contents buf
