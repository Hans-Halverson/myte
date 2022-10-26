open Asm
open Asm_builders
open X86_64_asm
open X86_64_gen_context

class compress_jump_aliases ~(gcx : Gcx.t) ~func =
  object
    inherit Asm_simplify_cfg.compress_jump_aliases ~func

    method get_unconditional_branch_block instr =
      match instr with
      | { instr = `Jmp; operands = [| { value = Block next_jump_block; _ } |]; _ } ->
        Some next_jump_block
      | _ -> None

    method iter_all_block_operands f =
      func_iter_blocks func (fun block ->
          iter_instructions block (fun instr ->
              match instr with
              | { instr = `Jmp | `JmpCC _; operands = [| block_op |]; _ } -> f block_op
              | _ -> ()))

    method on_removed_block block = gcx.prev_blocks <- BlockMMap.remove_key block gcx.prev_blocks
  end

(* Simplify Jmp instructions, removing unnecessary Jmp instructions when possible *)
let simplify_jumps ~(gcx : Gcx.t) =
  let rec merge blocks =
    match blocks with
    | block1 :: block2 :: tl ->
      (match get_last_instr_opt block1 with
      (* An unconditional Jmp followed by the jumped to block can be removed *)
      | Some ({ instr = `Jmp; operands = [| { value = Block next_block; _ } |]; _ } as jmp_instr)
        when next_block.id = block2.id ->
        remove_instruction jmp_instr
      (* Eliminate unnecessary Jmp instructions by reordering sequences of JmpCCs:

           JmpCC block1
           Jmp block2           JmpCC block2 (with inverse CC)
         block1:              block1:
           ...                  ...
         block2:              block2:
           ...                  ... *)
      | Some
          ({
             instr = `Jmp;
             operands = [| { value = Block jmp_block; _ } |];
             prev =
               { instr = `JmpCC cc; operands = [| { value = Block jmp_cc_block; _ } |]; _ } as
               jmp_cc_instr;
             _;
           } as jmp_instr)
        when jmp_cc_block.id == block2.id ->
        remove_instruction jmp_instr;
        jmp_cc_instr.instr <- `JmpCC (invert_condition_code cc);
        jmp_cc_instr.operands.(0) <- mk_block_op ~block:jmp_block
      | _ -> ());
      merge (block2 :: tl)
    | _ -> ()
  in
  FunctionSet.iter (fun func -> merge func.blocks) gcx.funcs

let compress_jump_aliases ~(gcx : Gcx.t) =
  FunctionSet.iter
    (fun func ->
      let compress_jump_aliases = new compress_jump_aliases ~gcx ~func in
      compress_jump_aliases#run ())
    gcx.funcs
