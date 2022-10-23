open Aarch64_gen_context
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

let pp_operand ~pcx ~buf op =
  match op.Operand.value with
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
      let pp_op op =
        add_string op;
        add_char ~buf ' '
      in
      let pp_no_args_op op = add_string op in
      match instr.instr with
      | `Ret -> pp_no_args_op "ret"
      | `B ->
        pp_op "b";
        pp_operand instr.operands.(0)
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
