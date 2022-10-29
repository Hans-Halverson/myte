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
  | Function func -> add_string ~buf func.label
  | Block block ->
    pp_label_debug_prefix ~buf block;
    add_string ~buf (Option.get (pp_label ~pcx block))
  | _ -> failwith "Printing operand not yet implemented"

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

let string_of_cond (cond : AArch64.cond) =
  match cond with
  | EQ -> "eq"
  | NE -> "ne"
  | LT -> "lt"
  | LE -> "le"
  | GT -> "gt"
  | GE -> "ge"

let pp_instruction ~gcx ~pcx ~buf instr =
  ignore gcx;
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
      | `AddI size ->
        pp_op "add";
        pp_operand ~size operands.(0);
        pp_args_separator ();
        pp_operand ~size operands.(1);
        pp_args_separator ();
        pp_operand ~size operands.(2);
        pp_lsl_immediate ~size operands.(3)
      | `AddR _ ->
        pp_op "add";
        pp_operands ()
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

let pp_block ~gcx ~pcx ~buf (block : Block.t) =
  (match pp_label ~pcx block with
  | None -> ()
  | Some label ->
    pp_label_debug_prefix ~buf block;
    add_label_line ~buf label);
  iter_instructions block (pp_instruction ~gcx ~pcx ~buf)

let pp_program ~(gcx : Gcx.t) : string =
  let pcx = mk_pcx ~funcs:gcx.funcs in
  let buf = mk_buf () in

  pp_global_directives ~buf;
  pp_data_sections ~buf ~bss:gcx.bss ~data:gcx.data;

  (* Add text section *)
  add_blank_line ~buf;
  add_line ~buf (fun buf -> add_string ~buf ".text");
  FunctionSet.iter (fun func -> List.iter (pp_block ~gcx ~pcx ~buf) func.Function.blocks) gcx.funcs;

  Buffer.contents buf
