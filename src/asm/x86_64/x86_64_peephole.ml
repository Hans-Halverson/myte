open X86_64_builders
open X86_64_instructions
open X86_64_gen_context
open X86_64_register

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

(* An instruction mapper which coalesces Lea instructions into the next instruction if applicable *)
let coalesce_lea_mapper =
  object (this)
    inherit X86_64_visitor.instruction_visitor

    val mutable has_coalesced = false

    val mutable reg_to_replace = Register.A

    val mutable address_to_coalesce = empty_memory_address

    method has_coalesced = has_coalesced

    method set_reg_and_address reg addr =
      has_coalesced <- false;
      reg_to_replace <- Operand.get_physical_register_value reg;
      address_to_coalesce <- addr

    method visit_operand op =
      match op.Operand.value with
      | MemoryAddress { offset = None; base = RegBase reg; index_and_scale = None }
        when Operand.get_physical_register_value reg = reg_to_replace ->
        has_coalesced <- true;
        op.value <- MemoryAddress address_to_coalesce
      | _ -> ()

    method! visit_read_operand ~block:_ op = this#visit_operand op

    method! visit_write_operand ~block:_ op = this#visit_operand op
  end

(* Coalesce a Lea instruction's address into the next instruction if the next instruction is the
   only use of the Lea instruction's calculated address.

   Example Before:
   leaq 4(%rax, %rdi), %rcx
   mov (%rcx), %rdx

   Example After:
   mov 4(%rax, %rdi), %rdx

   TODO: Track uses of reg defs to make sure next instruction is only use of Lea result reg *)
let coalesce_lea_optimization ~gcx:_ instr =
  let open Instruction in
  match instr.instr with
  | Lea (_, { value = MemoryAddress addr; _ }, result_reg) ->
    let block = instr.block in
    (* Check if there is an instruction following the Lea *)
    if instr == (Option.get block.instructions).last then
      false
    else (
      coalesce_lea_mapper#set_reg_and_address result_reg addr;
      coalesce_lea_mapper#visit_instruction ~block instr.next;
      if coalesce_lea_mapper#has_coalesced then (
        remove_instruction instr;
        true
      ) else
        false
    )
  | _ -> false

(* Avoid partial register stalls/dependencies by rewriting byte to byte register moves to instead
   write full register (note: writing 32 bits does not cause partial stall, so has same effect as
   writing full 64-bits with smaller code size by avoiding REX prefix). *)
let remove_byte_reg_reg_moves_optimization ~gcx:_ instr =
  let open Instruction in
  match instr.instr with
  | MovMM (Size8, src_reg, dest_reg)
    when Operand.is_reg_value src_reg && Operand.is_reg_value dest_reg ->
    instr.instr <- MovMM (Size32, src_reg, dest_reg);
    true
  | _ -> false

(* Loading zero to a register can be replaced by a reflexive xor for smaller instruction size *)
let load_zero_to_register_optimization ~gcx:_ instr =
  let open Instruction in
  match instr.instr with
  | MovIM (_, { value = Immediate imm; _ }, dest_reg)
    when Int64.equal (int64_of_immediate imm) Int64.zero && Operand.is_reg_value dest_reg ->
    instr.instr <- XorMM (Size32, dest_reg, dest_reg);
    true
  | _ -> false

(* Some arithmetic operations that involve immediate powers of two can be reduced to bit shifts *)
let power_of_two_strength_reduction_optimization ~gcx:_ instr =
  let open Instruction in
  match instr.instr with
  (* Multiplication by power of two can be reduced to a left shift *)
  | IMulMIR (size, src, { value = Immediate imm; _ }, dest_reg)
    when Integers.is_power_of_two (int64_of_immediate imm) ->
    let power_of_two = Int8.of_int (Integers.power_of_two (int64_of_immediate imm)) in
    instr.instr <- ShlI (size, mk_imm ~imm:(Imm8 power_of_two), dest_reg);
    let shift_instr = instr in
    (* If same register is source and dest, can shift it in place *)
    if
      Operand.is_reg_value src
      && Operand.get_physical_register_value src = Operand.get_physical_register_value dest_reg
    then
      true
    else
      (* Otherwise must move to dest register before shift *)
      let mov_instr = mk_blockless_instr (MovMM (size, src, dest_reg)) in
      insert_instruction_before ~before:shift_instr mov_instr;
      true
  | _ -> false

let basic_peephole_optimizations =
  [coalesce_lea_optimization; remove_byte_reg_reg_moves_optimization]

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
