open Asm
open Asm_builders
open Asm_calling_convention
open Asm_liveness_analysis
open Asm_instruction_definition
open Asm_register
open X86_64_builders
open X86_64_register

class use_def_finder color_to_representative_operand =
  object (this)
    method add_register_use ~instr:_ (_reg : Operand.t) = ()

    method add_register_def ~instr:_ (_reg : Operand.t) = ()

    method get_representative_register reg = RegMap.find reg color_to_representative_operand

    method visit_instruction (instr : Instruction.t) =
      match instr with
      (* Calls implicitly use all parameter register and define all caller save registers *)
      | { instr = `CallM (_, param_types); _ }
      | { instr = `CallL param_types; _ } ->
        Array.iter
          (fun param_type ->
            match param_type with
            | ParamInRegister reg ->
              this#add_register_use ~instr (this#get_representative_register reg)
            | _ -> ())
          param_types;
        RegSet.iter
          (fun reg -> this#add_register_def ~instr (this#get_representative_register reg))
          caller_saved_registers;
        this#visit_explicit_uses_and_defs instr
      (* IDiv uses the value in register A and writes to registers A and D *)
      | { instr = `IDiv _; _ } ->
        let reg_a = this#get_representative_register `A in
        let reg_d = this#get_representative_register `D in
        this#add_register_use ~instr reg_a;
        this#add_register_def ~instr reg_a;
        this#add_register_def ~instr reg_d;
        this#visit_explicit_uses_and_defs instr
      (* ConvertDouble (e.g. cdq) uses the value in register A and writes to register D *)
      | { instr = `ConvertDouble _; _ } ->
        this#add_register_use ~instr (this#get_representative_register `A);
        this#add_register_def ~instr (this#get_representative_register `D);
        this#visit_explicit_uses_and_defs instr
      (* Shifts with register shift argument implicitly use value in register C *)
      | { instr = `ShlM _ | `ShrM _ | `SarM _; _ } ->
        this#add_register_use ~instr (this#get_representative_register `C);
        this#visit_explicit_uses_and_defs instr
      (* Xor of a register with itself zeros the register, and only counts as a def, not a use, as
         the result is completely independent of the original value in the register. *)
      | { instr = `XorMM _ | `XorPD; operands = [| reg1; reg2 |]; _ }
        when Operand.is_reg_value reg1 && reg1.id = reg2.id ->
        this#visit_write_operand ~instr reg1
      | _ -> this#visit_explicit_uses_and_defs instr

    method visit_explicit_uses_and_defs instr =
      (* First visit uses then defs *)
      instr_iter_reg_mem_operands instr (fun operand operand_def ->
          if operand_is_use operand_def then this#visit_read_operand ~instr operand);
      instr_iter_reg_mem_operands instr (fun operand operand_def ->
          if operand_is_def operand_def then this#visit_write_operand ~instr operand)

    method resolve_register op =
      match op.Operand.value with
      | PhysicalRegister reg -> Some (this#get_representative_register reg)
      | VirtualRegister -> Some op
      | _ -> None

    method visit_read_operand ~instr op =
      match this#resolve_register op with
      | Some reg -> this#add_register_use ~instr reg
      | None -> ()

    method visit_write_operand ~instr op =
      match this#resolve_register op with
      | Some reg -> this#add_register_def ~instr reg
      | None -> ()
  end

class regs_liveness_analyzer (func : Function.t) color_to_operand =
  object (this)
    inherit use_def_finder color_to_operand
    inherit liveness_analyzer func

    val mutable prev_blocks = BlockMMap.empty
    val mutable use_blocks = OBMMap.empty
    val mutable def_blocks = OBMMap.empty
    val mutable use_before_def_blocks = OBMMap.empty

    method prev_blocks = prev_blocks
    method use_blocks = use_blocks
    method def_blocks = def_blocks
    method use_before_def_blocks = use_before_def_blocks

    method init () = func_iter_blocks func (fun block -> this#visit_block block)

    method visit_block block =
      iter_instructions block (fun instr ->
          (* Mark block edges *)
          (match instr with
          | { instr = `Jmp | `JmpCC _; operands = [| { value = Block next_block; _ } |]; block; _ }
            ->
            prev_blocks <- BlockMMap.add next_block block prev_blocks
          | _ -> ());

          this#visit_instruction instr)

    method! add_register_use ~(instr : Instruction.t) (reg : Operand.t) =
      use_blocks <- OBMMap.add reg instr.block use_blocks

    method! add_register_def ~(instr : Instruction.t) (reg : Operand.t) =
      let block = instr.block in
      if OBMMap.contains reg block use_blocks && not (OBMMap.contains reg block def_blocks) then
        use_before_def_blocks <- OBMMap.add reg block use_before_def_blocks;
      def_blocks <- OBMMap.add reg block def_blocks
  end

class vslots_liveness_analyzer (func : Function.t) =
  object (this)
    inherit liveness_analyzer func

    val mutable prev_blocks = BlockMMap.empty
    val mutable use_blocks = OBMMap.empty
    val mutable def_blocks = OBMMap.empty
    val mutable use_before_def_blocks = OBMMap.empty

    method prev_blocks = prev_blocks
    method use_blocks = use_blocks
    method def_blocks = def_blocks
    method use_before_def_blocks = use_before_def_blocks

    method init () =
      func_iter_blocks func (fun block -> iter_instructions block this#visit_instruction)

    method visit_block block = iter_instructions block this#visit_instruction

    method visit_instruction instr =
      (* Mark block edges *)
      (match instr with
      | { instr = `Jmp | `JmpCC _; operands = [| { value = Block next_block; _ } |]; block; _ } ->
        prev_blocks <- BlockMMap.add next_block block prev_blocks
      | _ -> ());

      (* First visit uses *)
      instr_iter_reg_mem_operands instr (fun operand operand_def ->
          if operand_is_use operand_def then use_blocks <- OBMMap.add operand instr.block use_blocks);

      (* Then visit defs *)
      instr_iter_reg_mem_operands instr (fun operand operand_def ->
          match operand.value with
          | VirtualStackSlot when operand_is_def operand_def ->
            let block = instr.block in
            if
              OBMMap.contains operand block use_blocks
              && not (OBMMap.contains operand block def_blocks)
            then
              use_before_def_blocks <- OBMMap.add operand block use_before_def_blocks;
            def_blocks <- OBMMap.add operand block def_blocks
          | _ -> ())
  end
