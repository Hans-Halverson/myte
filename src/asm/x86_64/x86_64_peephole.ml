open Asm
open Asm_builders
open X86_64_gen_context

(* A peephole optimization is a function that takes an instruction and applies an optimization if
   possible, returning whether an edit was made *)
type peephole_optimization = gcx:Gcx.t -> Instruction.t -> bool

class peephole_optimization_runner ~(gcx : Gcx.t) (opts : peephole_optimization list) =
  object (this)
    val mutable has_changed = false

    method run () =
      (* Apply peephole optimizations in each function to fixpoint *)
      let rec run_to_fixpoint func =
        has_changed <- false;
        List.iter this#visit_block func.Function.blocks;
        if has_changed then run_to_fixpoint func
      in
      FunctionSet.iter (fun func -> run_to_fixpoint func) gcx.funcs

    (* Try running all peephole optimizations at a particular instruction, returning an edit if
       an optimization is applied. *)
    method run_opts_on_instruction instr =
      let rec iter opts =
        match opts with
        | [] -> ()
        | opt :: rest_opts ->
          if opt ~gcx instr then
            has_changed <- true
          else
            iter rest_opts
      in
      iter opts

    method visit_block block =
      iter_instructions block (fun instr -> this#run_opts_on_instruction instr)
  end

(* Avoid partial register stalls/dependencies by rewriting byte to byte register moves to instead
   write full register (note: writing 32 bits does not cause partial stall, so has same effect as
   writing full 64-bits with smaller code size by avoiding REX prefix). *)
let remove_byte_reg_reg_moves_optimization ~gcx:_ instr =
  let open Instruction in
  match instr with
  | { instr = `MovMM Size8; operands = [| src_reg; dest_reg |]; _ }
    when Operand.is_reg_value src_reg && Operand.is_reg_value dest_reg ->
    instr.instr <- `MovMM Size32;
    instr.operands.(0) <- src_reg;
    instr.operands.(1) <- dest_reg;
    true
  | _ -> false

(* Zero extending 32 bit register is actually just a regular mov instruction since the upper bytes
   are automatically zeroed. *)
let simplify_32_to_64_zext_optimization ~gcx:_ instr =
  let open Instruction in
  match instr with
  | { instr = `MovZX (Size32, Size64); _ } ->
    instr.instr <- `MovMM Size32;
    true
  | _ -> false

(* Loading zero to a register can be replaced by a reflexive xor for smaller instruction size *)
let load_zero_to_register_optimization ~gcx:_ instr =
  let open Instruction in
  match instr with
  | { instr = `MovIM _; operands = [| { value = Immediate imm; _ }; dest_reg |]; _ }
    when Int64.equal (int64_of_immediate imm) Int64.zero && Operand.is_reg_value dest_reg ->
    instr.instr <- `XorMM Size32;
    instr.operands.(0) <- dest_reg;
    instr.operands.(1) <- dest_reg;
    true
  | _ -> false

(* Some arithmetic operations that involve immediate powers of two can be reduced to bit shifts *)
let power_of_two_strength_reduction_optimization ~gcx:_ instr =
  let open Instruction in
  match instr with
  (* Multiplication by power of two can be reduced to a left shift *)
  | { instr = `IMulIMR size; operands = [| { value = Immediate imm; _ }; src; dest_reg |]; _ }
    when Integers.is_power_of_two (int64_of_immediate imm) ->
    let power_of_two = Int8.of_int (Integers.int64_ctz (int64_of_immediate imm)) in
    instr.instr <- `ShlI size;
    instr.operands <- [| mk_imm ~imm:(Imm8 power_of_two); dest_reg |];
    let shift_instr = instr in
    (* If same register is source and dest, can shift it in place *)
    if
      Operand.is_reg_value src
      && Operand.get_physical_register_value src = Operand.get_physical_register_value dest_reg
    then
      true
    else
      (* Otherwise must move to dest register before shift *)
      let mov_instr = mk_blockless_instr (`MovMM size) [| src; dest_reg |] in
      insert_instruction_before ~before:shift_instr mov_instr;
      true
  | _ -> false

let basic_peephole_optimizations =
  [remove_byte_reg_reg_moves_optimization; simplify_32_to_64_zext_optimization]

let optimizer_peephole_optimizations =
  [load_zero_to_register_optimization; power_of_two_strength_reduction_optimization]

let run_peephole_optimizations ~gcx =
  let peephole_optimizations =
    if Opts.optimize () then
      basic_peephole_optimizations @ optimizer_peephole_optimizations
    else
      basic_peephole_optimizations
  in
  let runner = new peephole_optimization_runner ~gcx peephole_optimizations in
  runner#run ()
