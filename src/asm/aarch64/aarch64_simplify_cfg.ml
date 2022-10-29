open Aarch64_gen_context
open Asm
open Asm_builders

class compress_branch_aliases ~(gcx : Gcx.t) ~func =
  object
    inherit Asm_simplify_cfg.compress_jump_aliases ~func

    method get_unconditional_branch_block instr =
      match instr with
      | { instr = `B; operands = [| { value = Block next_jump_block; _ } |]; _ } ->
        Some next_jump_block
      | _ -> None

    method iter_all_block_operands f =
      func_iter_blocks func (fun block ->
          iter_instructions block (fun instr ->
              match instr with
              | { instr = `B | `BCond _; operands = [| block_op |]; _ } -> f block_op
              | _ -> ()))

    method on_removed_block block = gcx.prev_blocks <- BlockMMap.remove_key block gcx.prev_blocks
  end

(* Simplify branch instructions, removing unnecessary branch instructions when possible *)
let simplify_branches ~(gcx : Gcx.t) =
  let rec merge blocks =
    match blocks with
    | block1 :: block2 :: tl ->
      (* An unconditional branch followed by the branched to block can be removed *)
      (match get_last_instr_opt block1 with
      | Some ({ instr = `B; operands = [| { value = Block next_block; _ } |]; _ } as branch_instr)
        when next_block.id = block2.id ->
        remove_instruction branch_instr
      | _ -> ());
      merge (block2 :: tl)
    | _ -> ()
  in
  FunctionSet.iter (fun func -> merge func.blocks) gcx.funcs

let compress_branch_aliases ~(gcx : Gcx.t) =
  FunctionSet.iter
    (fun func ->
      let compress_branch_aliases = new compress_branch_aliases ~gcx ~func in
      compress_branch_aliases#run ())
    gcx.funcs
