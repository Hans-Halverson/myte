open Asm
open Asm_register
open X86_64_builders
open X86_64_calling_conventions
open X86_64_gen_context
open X86_64_instruction_definitions

class use_def_finder color_to_op =
  object (this)
    inherit X86_64_liveness_analysis.use_def_finder color_to_op

    val mutable memory_reg_uses : RegSet.t = RegSet.empty

    val mutable other_reg_uses : RegSet.t = RegSet.empty

    val mutable reg_defs : RegSet.t = RegSet.empty

    method find_use_defs (instr : Instruction.t) =
      memory_reg_uses <- RegSet.empty;
      other_reg_uses <- RegSet.empty;
      reg_defs <- RegSet.empty;
      this#visit_instruction instr;
      (memory_reg_uses, other_reg_uses, reg_defs)

    method! visit_explicit_uses_and_defs instr =
      instr_iter_all_operands instr (fun operand operand_def ->
          match operand.value with
          | X86_64_MemoryAddress { offset = None; base = RegBase reg; index_and_scale = None } ->
            memory_reg_uses <- RegSet.add (Operand.get_physical_register_value reg) memory_reg_uses
          | _ ->
            operand_iter_reg_mem_operands operand operand_def (fun operand operand_def ->
                if operand_is_use operand_def then this#visit_read_operand ~instr operand;
                if operand_is_def operand_def then this#visit_write_operand ~instr operand))

    method! add_register_use ~instr:_ (reg : Operand.t) =
      other_reg_uses <- RegSet.add (Operand.get_physical_register_value reg) other_reg_uses

    method! add_register_def ~instr:_ (reg : Operand.t) =
      reg_defs <- RegSet.add (Operand.get_physical_register_value reg) reg_defs
  end

let inline_at_uses (lea_instr : Instruction.t) (use_instrs : InstrSet.t) =
  match lea_instr with
  | { instr = `Lea _; operands = [| { value = X86_64_MemoryAddress addr; _ }; dest_reg |]; _ } ->
    let reg_to_replace = Operand.get_physical_register_value dest_reg in

    InstrSet.iter
      (fun use_instr ->
        Array.iter
          (fun operand ->
            match operand.Operand.value with
            | X86_64_MemoryAddress { offset = None; base = RegBase reg; index_and_scale = None }
              when Operand.get_physical_register_value reg = reg_to_replace ->
              operand.value <- X86_64_MemoryAddress addr
            | _ -> ())
          use_instr.operands)
      use_instrs
  | _ -> ()

(* Get all register arguments used in a memory operand *)
let lea_get_reg_args (lea_instr : Instruction.t) : RegSet.t =
  match lea_instr with
  | { instr = `Lea _; operands = [| { value = X86_64_MemoryAddress addr; _ }; _ |]; _ } ->
    (match (addr.base, addr.index_and_scale) with
    | (RegBase first_reg, Some (second_reg, _)) ->
      RegSet.add
        (Operand.get_physical_register_value first_reg)
        (RegSet.singleton (Operand.get_physical_register_value second_reg))
    | (RegBase reg, _)
    | (_, Some (reg, _)) ->
      RegSet.singleton (Operand.get_physical_register_value reg)
    | _ -> RegSet.empty)
  | _ -> failwith "Expected lea instruction"

let rec run ~(gcx : Gcx.t) =
  let use_def_finder = new use_def_finder gcx.color_to_op in
  FunctionSet.iter (run_on_func ~gcx ~use_def_finder) gcx.funcs

and run_on_func ~gcx ~use_def_finder func =
  let (_, live_out) = X86_64_liveness_analysis.analyze_regs func.blocks gcx.color_to_op in
  let return_reg = Option.map SystemVCallingConvention.calculate_return_register func.return_type in

  func_iter_blocks func (fun block ->
      (* All leas that should be inlined, mapped to the instructions that use that lea's address *)
      let all_inlinable_leas = ref InstrMap.empty in

      (* All lea instruction results that may still be alive *)
      let current_live_leas = ref RegMap.empty in

      (* The args to all lea instructions that may currently be alive. These are needed so that
         on writes to them we can invalidate the lea. *)
      let current_live_lea_args = ref RegMap.empty in

      (* Leas which have args that have been overwritten by this point, and cannot be inlined *)
      let invalidated_leas = ref InstrSet.empty in

      let remove_if_currently_live reg =
        match RegMap.find_opt reg !current_live_leas with
        | None -> ()
        | Some lea_instr -> all_inlinable_leas := InstrMap.remove lea_instr !all_inlinable_leas
      in

      iter_instructions block (fun instr ->
          let (memory_reg_uses, other_reg_uses, def_regs) = use_def_finder#find_use_defs instr in

          (* Mark memory uses of currently live lea instruction results *)
          RegSet.iter
            (fun memory_use_reg ->
              match RegMap.find_opt memory_use_reg !current_live_leas with
              | None -> ()
              | Some lea_instr ->
                (* If an lea has been invalidated before it is used, then it cannot be inlined *)
                if InstrSet.mem lea_instr !invalidated_leas then
                  all_inlinable_leas := InstrMap.remove lea_instr !all_inlinable_leas
                else
                  let new_use_instrs =
                    InstrSet.add instr (InstrMap.find lea_instr !all_inlinable_leas)
                  in
                  all_inlinable_leas := InstrMap.add lea_instr new_use_instrs !all_inlinable_leas)
            memory_reg_uses;

          (* Any non-memory use of a lea makes it not inlinable *)
          RegSet.iter remove_if_currently_live other_reg_uses;

          (* If a lea result register is redefined then lea instr and its args are now dead *)
          RegSet.iter
            (fun def_reg ->
              (match RegMap.find_opt def_reg !current_live_leas with
              | None -> ()
              | Some lea_instr ->
                current_live_leas := RegMap.remove def_reg !current_live_leas;

                let addr_reg_args = lea_get_reg_args lea_instr in
                RegSet.iter
                  (fun addr_reg_arg ->
                    current_live_lea_args := RegMap.remove addr_reg_arg !current_live_lea_args)
                  addr_reg_args);

              (* If a lea arg is redefined then the lea is invalidated as it cannot be inlined
                 from this point on. *)
              match RegMap.find_opt def_reg !current_live_lea_args with
              | None -> ()
              | Some lea_instr ->
                current_live_lea_args := RegMap.remove def_reg !current_live_lea_args;
                invalidated_leas := InstrSet.add lea_instr !invalidated_leas)
            def_regs;

          (* Add the lea address result and its args as currently live *)
          match instr with
          | { instr = `Lea _; operands = [| _; dest_reg |]; _ } ->
            let lea_address_reg = Operand.get_physical_register_value dest_reg in
            current_live_leas := RegMap.add lea_address_reg instr !current_live_leas;
            all_inlinable_leas := InstrMap.add instr InstrSet.empty !all_inlinable_leas;

            let addr_reg_args = lea_get_reg_args instr in
            RegSet.iter
              (fun addr_reg_arg ->
                current_live_lea_args := RegMap.add addr_reg_arg instr !current_live_lea_args)
              addr_reg_args
          | _ -> ());

      (* Lea def registers still live at the end of the block cannot have their address inlined,
         as inlining addresses across block boundaries would require additional analysis. *)
      let live_out_regs = BlockMap.find block live_out in
      List.iter
        (fun live_out_reg ->
          let reg = Operand.get_physical_register_value live_out_reg in
          remove_if_currently_live reg)
        live_out_regs;

      (* If last instruction is a ret then return register is still live *)
      (match (get_last_instr_opt block, return_reg) with
      | (Some { instr = `Ret; _ }, Some return_reg) -> remove_if_currently_live return_reg
      | _ -> ());

      (* Inline all remaining lea instructions at their uses *)
      InstrMap.iter
        (fun lea_instr use_instrs ->
          inline_at_uses lea_instr use_instrs;
          remove_instruction lea_instr)
        !all_inlinable_leas)
