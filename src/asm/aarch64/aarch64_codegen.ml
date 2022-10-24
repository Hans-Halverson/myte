open Aarch64_gen_context
open Asm_builders
open Asm_codegen
open Mir
open Mir_builders

let rec gen ~(gcx : Gcx.t) (ir : Program.t) =
  Mir_builders.program_iter_funcs ir (fun func -> preprocess_function ~gcx func);
  Mir_builders.program_iter_funcs ir (fun func -> gen_function ~gcx ~ir func)

and preprocess_function ~gcx func =
  let calling_convention = Gcx.mir_function_calling_convention func in
  let param_mir_types = List.map (fun param -> type_of_value param) func.params in
  let param_types = calling_convention#calculate_param_types param_mir_types in
  gcx.mir_func_to_param_types <- FunctionMap.add func param_types gcx.mir_func_to_param_types

and gen_function ~gcx ~ir func =
  let param_types = FunctionMap.find func gcx.mir_func_to_param_types in
  let func_ = Gcx.start_function ~gcx func param_types in
  let label = get_asm_function_label ~ir func in
  (* Create function prologue which copies all params from physical registers or stack slots to
     temporaries *)
  Gcx.start_block ~gcx ~label:(Some label) ~func:func_ ~mir_block:None;
  func_.prologue <- Option.get gcx.current_block;

  func_.params <-
    List.mapi
      (fun i param ->
        let param_mir_type = type_of_value param in
        let arg_id = param.id in
        let { Argument.type_; _ } = cast_to_argument param in
        match func_.param_types.(i) with
        | ParamOnStack _ -> failwith "TODO: Handle stack arguments"
        | ParamInRegister reg ->
          let size = register_size_of_mir_value_type type_ in
          let param_op = mk_virtual_register_of_value_id ~value_id:arg_id ~type_:param_mir_type in
          Gcx.emit ~gcx (`MovR size) [| mk_precolored_of_operand reg param_op; param_op |];
          param_op)
      func.params;

  (* Jump to function start and gen function body *)
  Gcx.emit ~gcx `B [| block_op_of_mir_block ~gcx func.start_block |];
  Gcx.finish_block ~gcx;
  gen_blocks ~gcx ~ir func.start_block None func_;
  Gcx.finish_function ~gcx

and gen_blocks ~gcx ~ir start_block label func =
  let ordered_blocks = Mir_graph_ordering.get_ordered_cfg start_block in
  List.iteri
    (fun i mir_block ->
      let label =
        if i = 0 then
          label
        else
          None
      in
      Gcx.start_block ~gcx ~label ~func ~mir_block:(Some mir_block);
      let instructions =
        fold_instructions mir_block [] (fun instr_val _ acc -> instr_val :: acc) |> List.rev
      in
      gen_instructions ~gcx ~ir ~block:mir_block instructions;
      Gcx.finish_block ~gcx)
    ordered_blocks

and gen_instructions ~gcx ~ir:_ ~block:_ instructions =
  match instructions with
  | [] -> ()
  (*
   * ===========================================
   *                   Ret
   * ===========================================
   *)
  | [{ value = Instr { instr = Ret _; _ }; _ }] -> Gcx.emit ~gcx `Ret [||]
  (*
   * ===========================================
   *                Terminators
   * ===========================================
   *)
  | [{ value = Instr { instr = Unreachable; _ }; _ }] -> ()
  | [{ value = Instr { instr = Continue continue; _ }; _ }] ->
    Gcx.emit ~gcx `B [| block_op_of_mir_block ~gcx continue |]
  | { value = Instr { instr = Ret _ | Continue _ | Branch _ | Unreachable; _ }; _ } :: _ ->
    failwith "Terminator instructions must be last instruction"
  | { value = Instr { instr = Mir.Instruction.Phi _; _ }; _ } :: _ ->
    failwith "Phi nodes must be removed before asm gen"
  | { value = Instr { instr = Mir.Instruction.StackAlloc _; _ }; _ } :: _ ->
    failwith "StackAlloc instructions removed before asm gen"
  | { value = Instr _; _ } :: _ -> failwith "Unimplemented MIR instruction"
  | { value = Lit _ | Argument _; _ } :: _ -> failwith "Expected instruction value"

and block_op_of_mir_block ~gcx mir_block =
  mk_block_op ~block:(Gcx.get_block_from_mir_block ~gcx mir_block)

and register_size_of_mir_value_type value_type =
  match value_type with
  | Bool
  | Byte
  | Short
  | Int ->
    Size32
  | Long
  | Double
  | Function
  | Pointer _ ->
    Size64
  | Aggregate _ -> failwith "TODO: Cannot compile aggregate structure literals"
  | Array _ -> failwith "TODO: Cannot compile array literals"
