open Aarch64_gen_context
open Aarch64_instruction_definitions
open Aarch64_register
open Asm
open Asm_builders
open Asm_pp

let mk_pcx ~(funcs : FunctionSet.t) =
  (* Find set of all blocks that have an incoming jump *)
  let incoming_jump_blocks = ref BlockSet.empty in
  funcs_iter_blocks funcs (fun block ->
      iter_instructions block (fun instr ->
          match instr with
          | { instr = `B; operands = [| { value = Block next_block; _ } |]; _ } ->
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
  | Block block ->
    pp_label_debug_prefix ~buf block;
    add_string ~buf (Option.get (pp_label ~pcx block))
  | _ -> failwith "Printing operand not yet implemented"

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
              | PhysicalRegister _ -> instr_register_size instr.instr
              | _ -> Size64
            in
            pp_operand ~size operand;
            if i != last_operand_idx then pp_args_separator ())
          instr.operands
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
      | `MovI _
      | `MovR _ ->
        pp_op_and_operands "mov"
      | `MovK size ->
        pp_op "movk";
        pp_operand ~size operands.(0);
        pp_args_separator ();
        pp_operand ~size operands.(1);
        pp_args_separator ();
        add_string "lsl ";
        pp_operand ~size operands.(2)
      | `Ret -> pp_no_args_op "ret"
      | `B ->
        pp_op "b";
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
