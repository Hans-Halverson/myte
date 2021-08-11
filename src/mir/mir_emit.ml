open Ast
open Basic_collections
open Mir
open Mir_type
module Ecx = Mir_emit_context
module Pcx = Program_context

type 'a access_chain_part =
  | AccessChainOffset of 'a Instruction.GetPointer.offset * Type.t
  | AccessChainDereference

type 'a access_chain_result =
  | GetPointerEmittedResult of 'a Value.pointer_value
  | InlinedValueResult of 'a Value.t

let rec emit_control_flow_ir (pcx : Pcx.t) : Ecx.t * cf_program =
  let ecx = Ecx.mk ~pcx in
  start_init_function ~ecx;
  emit_type_declarations ~ecx;
  emit_toplevel_variable_declarations ~ecx;
  emit_function_declarations ~ecx;
  emit_pending_instantiations ~ecx;
  finish_init_function ~ecx;
  ( ecx,
    {
      Program.main_id = ecx.main_id;
      blocks = Ecx.builders_to_blocks ecx.blocks;
      globals = ecx.globals;
      funcs = ecx.funcs;
      types = ecx.types;
    } )

and emit_type_declarations ~ecx =
  let open Ast.Module in
  List.iter
    (fun (_, module_) ->
      List.iter
        (fun toplevel ->
          match toplevel with
          | TypeDeclaration decl -> emit_type_declaration ~ecx decl
          | _ -> ())
        module_.toplevels)
    ecx.pcx.modules

and emit_toplevel_variable_declarations ~ecx =
  let open Ast.Module in
  List.iter
    (fun (_, module_) ->
      List.iter
        (fun toplevel ->
          match toplevel with
          | VariableDeclaration decl -> emit_toplevel_variable_declaration ~ecx decl
          | _ -> ())
        module_.toplevels)
    ecx.pcx.modules

and emit_function_declarations ~ecx =
  let open Ast.Module in
  List.iter
    (fun (_, module_) ->
      List.iter
        (fun toplevel ->
          match toplevel with
          | FunctionDeclaration decl -> emit_function_declaration ~ecx decl false
          | TraitDeclaration { methods; _ } ->
            List.iter
              (fun ({ Ast.Function.static; _ } as func_decl) ->
                emit_function_declaration ~ecx func_decl (not static))
              methods
          | _ -> ())
        module_.toplevels)
    ecx.pcx.modules

and emit_pending_instantiations ~ecx =
  (* Emit all pending instantiations of generic functions *)
  let rec iter () =
    match Ecx.pop_pending_func_instantiation ~ecx with
    | None -> ()
    | Some func_instantiation ->
      emit_function_instantiation ~ecx func_instantiation;
      iter ()
  in
  iter ()

(* Create and store MIR ADT and variant objects to be used during MIR emission *)
and emit_type_declaration ~ecx decl =
  let open TypeDeclaration in
  match decl.decl with
  | Tuple _ ->
    let mir_adt_layout = Mir_adt_layout_builder.mk_mir_tuple_layout ~ecx decl in
    Ecx.add_mir_adt_layout ~ecx mir_adt_layout
  | Record record_decl ->
    let mir_adt_layout = Mir_adt_layout_builder.mk_mir_record_layout ~ecx decl record_decl in
    Ecx.add_mir_adt_layout ~ecx mir_adt_layout
  | Variant variants ->
    let mir_adt_layout = Mir_adt_layout_builder.mk_mir_variants_layout ~ecx decl variants in
    Ecx.add_mir_adt_layout ~ecx mir_adt_layout
  | Alias _
  | Builtin ->
    ()

and emit_toplevel_variable_declaration ~ecx decl =
  let { Statement.VariableDeclaration.pattern; init; _ } = decl in
  (* TODO: Emit MIR for arbitrary patterns *)
  let { Identifier.loc; _ } = List.hd (Ast_utils.ids_of_pattern pattern) in
  let binding = Bindings.get_value_binding ecx.pcx.bindings loc in
  let name = mk_value_binding_name binding in
  (* Find value type of variable *)
  let binding = Type_context.get_value_binding ~cx:ecx.pcx.type_ctx loc in
  let var_decl = Bindings.get_var_decl binding in
  let ty = type_to_mir_type ~ecx (Types.Type.TVar var_decl.tvar) in
  (* Build IR for variable init *)
  let init_val =
    Ecx.emit_init_section ~ecx (fun _ ->
        ecx.current_in_std_lib <- Bindings.is_std_lib_value binding;
        (* If initial value is statically known at compile time, add it as constant initialization *)
        let init_val = emit_expression ~ecx init in
        if is_literal init_val then
          Some init_val
        else (
          (* Otherwise value must be calculated and stored at runtime *)
          Ecx.emit ~ecx (Store (`PointerL (ty, name), init_val));
          None
        ))
  in
  Ecx.add_global ~ecx { Global.loc; name; ty; init_val }

and emit_function_declaration ~ecx decl is_method =
  let open Ast.Function in
  let { name = { Identifier.loc; _ }; body; _ } = decl in
  let binding = Bindings.get_value_binding ecx.pcx.bindings loc in
  let name = mk_value_binding_name binding in
  Ecx.add_function_declaration_node ~ecx name decl;
  (* Non-generic functions are emitted now. An instance of generic functions will be emitted when
     they are instantiated. *)
  if body <> Signature && not is_method then
    let binding = Type_context.get_value_binding ~cx:ecx.pcx.type_ctx loc in
    let func_decl = Bindings.get_func_decl binding in
    if func_decl.type_params = [] then emit_function_body ~ecx name decl

and emit_function_instantiation ~ecx (name, name_with_args, type_param_bindings) =
  Ecx.in_type_binding_context ~ecx type_param_bindings (fun _ ->
      let func_decl_node = SMap.find name ecx.func_decl_nodes in
      if not func_decl_node.builtin then emit_function_body ~ecx name_with_args func_decl_node)

and emit_function_body ~ecx name decl =
  let open Ast.Function in
  let { loc = full_loc; name = { Identifier.loc; _ }; params; body; _ } = decl in
  let binding = Type_context.get_value_binding ~cx:ecx.pcx.type_ctx loc in
  ecx.current_in_std_lib <- Bindings.is_std_lib_value binding;
  ecx.current_is_main <- loc = Option.get ecx.pcx.main_loc;

  (* Build IR for function body *)
  Ecx.set_current_func ~ecx name;
  let body_start_block = Ecx.start_new_block ~ecx in
  if ecx.current_is_main then ecx.main_id <- body_start_block;
  (match body with
  | Block { Statement.Block.statements; _ } ->
    List.iter (emit_statement ~ecx) statements;
    (* Add an implicit return if the last instruction is not a return *)
    (match ecx.current_block_builder with
    | Some { Ecx.BlockBuilder.instructions = (_, Ret _) :: _; _ } -> ()
    | _ ->
      (* Handle implicit return from main *)
      let return_val =
        if ecx.current_is_main then
          Some (`IntL Int32.zero)
        else
          None
      in
      Ecx.emit ~ecx (Ret return_val))
  | Expression expr ->
    let ret_val = emit_expression ~ecx expr in
    Ecx.emit ~ecx (Ret (Some ret_val))
  | Signature -> ());
  Ecx.finish_block_halt ~ecx;

  (* Find value type of function *)
  let func_decl = Bindings.get_func_decl binding in
  let params =
    List.map2
      (fun { Param.name = { Identifier.loc; _ }; _ } ty ->
        (loc, mk_var_id (), type_to_mir_type ~ecx ty))
      params
      func_decl.params
  in

  (* Add implicit this param *)
  let params =
    match LocMap.find_opt full_loc ecx.pcx.bindings.value_bindings with
    | Some { declaration = FunParamDecl { tvar }; _ } ->
      let this_type = type_to_mir_type ~ecx (Types.Type.TVar tvar) in
      (full_loc, mk_var_id (), this_type) :: params
    | _ -> params
  in

  (* The main function must always return an Int *)
  let return_ty = type_to_mir_type ~ecx func_decl.return in
  let return_ty =
    if ecx.current_is_main && return_ty = `UnitT then
      `IntT
    else
      return_ty
  in

  Ecx.add_function ~ecx { Function.loc; name; params; return_ty; body_start_block }

and start_init_function ~ecx =
  Ecx.emit_init_section ~ecx (fun _ -> ());
  let body_start_block = (Option.get ecx.Ecx.last_init_block_builder).id in
  Ecx.add_function
    ~ecx
    {
      Function.loc = Loc.none;
      name = init_func_name;
      params = [];
      return_ty = `UnitT;
      body_start_block;
    }

and finish_init_function ~ecx =
  let last_init_block = Option.get ecx.Ecx.last_init_block_builder in
  last_init_block.instructions <-
    (mk_instr_id (), Instruction.Ret None) :: last_init_block.instructions

and emit_expression ~ecx expr =
  let open Expression in
  let open Instruction in
  match expr with
  (*
   * ============================
   *         Literals
   * ============================
   *)
  | Unit _ -> `UnitL
  | BoolLiteral { value; _ } -> `BoolL value
  | IntLiteral { loc; raw; base } ->
    let value = Integers.int64_of_string_opt raw base |> Option.get in
    let ty = mir_type_of_loc ~ecx loc in
    (match ty with
    | `ByteT -> `ByteL (Int64.to_int value)
    | `IntT -> `IntL (Int64.to_int32 value)
    | `LongT -> `LongL value
    | _ -> failwith "Int literal must have integer type")
  | StringLiteral { loc; value; _ } ->
    let string_global_ptr = Ecx.add_string_literal ~ecx loc value in
    let string_pointer_type = mir_type_of_loc ~ecx loc in
    let (`PointerT string_type) = cast_to_pointer_type string_pointer_type in
    let (`AggregateT string_agg) = cast_to_aggregate_type string_type in
    (* Call myte_alloc builtin to allocate space for string *)
    let agg_ptr_var_id = mk_cf_var_id () in
    let agg_ptr_var = `PointerV (string_type, agg_ptr_var_id) in
    let (agg_ptr_val, myte_alloc_instr) =
      Mir_builtin.(mk_call_builtin myte_alloc agg_ptr_var_id [`IntL Int32.one] [string_type])
    in
    Ecx.emit ~ecx myte_alloc_instr;
    (* Write all string literal fields *)
    let emit_field_store name value =
      let (element_ty, element_idx) = lookup_element string_agg name in
      let (element_offset_var, get_ptr_instr) =
        mk_get_pointer_instr element_ty agg_ptr_var [GetPointer.FieldIndex element_idx]
      in
      Ecx.emit ~ecx (GetPointer get_ptr_instr);
      Ecx.emit ~ecx (Store (element_offset_var, value))
    in
    let length = `IntL (Int32.of_int (String.length value)) in
    emit_field_store "data" string_global_ptr;
    emit_field_store "size" length;
    emit_field_store "capacity" length;
    agg_ptr_val
  (*
   * ============================
   *       Unary Operations
   * ============================
   *)
  | UnaryOperation { op = Plus; operand; _ } -> emit_expression ~ecx operand
  | UnaryOperation { loc; op = Minus; operand } ->
    let var_id = mk_cf_var_id () in
    let operand_val = emit_numeric_expression ~ecx operand in
    let ty = mir_type_of_loc ~ecx loc in
    Ecx.emit ~ecx (Neg (var_id, operand_val));
    var_value_of_type var_id ty
  | UnaryOperation { op = Not; loc; operand } ->
    let var_id = mk_cf_var_id () in
    let value_ty = mir_type_of_loc ~ecx loc in
    (match value_ty with
    | `BoolT ->
      let operand_val = emit_bool_expression ~ecx operand in
      Ecx.emit ~ecx (LogNot (var_id, operand_val));
      `BoolV var_id
    | `ByteT
    | `IntT
    | `LongT ->
      let operand_val = emit_numeric_expression ~ecx operand in
      Ecx.emit ~ecx (BitNot (var_id, operand_val));
      var_value_of_type var_id value_ty
    | _ -> failwith "Not argument must be a bool or int")
  (*
   * ============================
   *        Logical And
   * ============================
   *)
  | LogicalAnd { loc = _; left; right } ->
    (* Short circuit when lhs is false by jumping to false case *)
    let rhs_builder = Ecx.mk_block_builder ~ecx in
    let false_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    let left_val = emit_bool_expression ~ecx left in
    Ecx.finish_block_branch ~ecx left_val rhs_builder.id false_builder.id;
    (* Emit right hand side when lhs is true and continue to join block *)
    Ecx.set_block_builder ~ecx rhs_builder;
    let right_val = emit_expression ~ecx right in
    let right_var_id = emit_var_of_value ~ecx right_val in
    let rhs_end_block_id = Ecx.get_block_builder_id_throws ~ecx in
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Emit false literal when lhs is false and continue to join block *)
    Ecx.set_block_builder ~ecx false_builder;
    let false_var_id = mk_cf_var_id () in
    Ecx.emit ~ecx (Mov (false_var_id, `BoolL false));
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Join cases together and emit explicit phi *)
    Ecx.set_block_builder ~ecx join_builder;
    let var_id = mk_cf_var_id () in
    Ecx.emit_phi
      ~ecx
      `BoolT
      var_id
      (IMap.add rhs_end_block_id right_var_id (IMap.singleton false_builder.id false_var_id));
    `BoolV var_id
  (*
   * ============================
   *         Logical Or
   * ============================
   *)
  | LogicalOr { loc = _; left; right } ->
    (* Short circuit when lhs is true by jumping to true case *)
    let rhs_builder = Ecx.mk_block_builder ~ecx in
    let true_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    let left_val = emit_bool_expression ~ecx left in
    Ecx.finish_block_branch ~ecx left_val true_builder.id rhs_builder.id;
    (* Emit right hand side when lhs is false and continue to join block *)
    Ecx.set_block_builder ~ecx rhs_builder;
    let right_val = emit_expression ~ecx right in
    let right_var_id = emit_var_of_value ~ecx right_val in
    let rhs_end_block_id = Ecx.get_block_builder_id_throws ~ecx in
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Emit true literal when lhs is true and continue to join block *)
    Ecx.set_block_builder ~ecx true_builder;
    let true_var_id = mk_cf_var_id () in
    Ecx.emit ~ecx (Mov (true_var_id, `BoolL true));
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Join cases together and emit explicit phi *)
    Ecx.set_block_builder ~ecx join_builder;
    let var_id = mk_cf_var_id () in
    Ecx.emit_phi
      ~ecx
      `BoolT
      var_id
      (IMap.add rhs_end_block_id right_var_id (IMap.singleton true_builder.id true_var_id));
    `BoolV var_id
  (*
   * ============================
   *          Equality
   * ============================
   *)
  | BinaryOperation { op = (Equal | NotEqual) as op; left; right; _ } ->
    let left_ty = type_of_loc ~ecx (Ast_utils.expression_loc left) in
    let receiver_val = emit_expression ~ecx left in
    let arg_vals = [emit_expression ~ecx right] in
    let equals_result_val =
      emit_method_call
        ~ecx
        ~method_name:"equals"
        ~receiver_val
        ~receiver_ty:left_ty
        ~arg_vals
        ~method_instance_type_args:[]
        ~ret_type:`BoolT
    in
    if op = Equal then
      equals_result_val
    else
      let var_id = mk_cf_var_id () in
      Ecx.emit ~ecx (LogNot (var_id, cast_to_bool_value equals_result_val));
      var_value_of_type var_id `BoolT
  (*
   * ============================
   *       Binary Operations
   * ============================
   *)
  | BinaryOperation { loc; op; left; right } ->
    let open BinaryOperation in
    let var_id = mk_cf_var_id () in
    let left_val = emit_numeric_expression ~ecx left in
    let right_val = emit_numeric_expression ~ecx right in
    let ty = mir_type_of_loc ~ecx loc in
    let (instr, ty) =
      match op with
      | Add -> (Instruction.Add (var_id, left_val, right_val), ty)
      | Subtract -> (Sub (var_id, left_val, right_val), ty)
      | Multiply -> (Mul (var_id, left_val, right_val), ty)
      | Divide -> (Div (var_id, left_val, right_val), ty)
      | Remainder -> (Rem (var_id, left_val, right_val), ty)
      | BitwiseAnd -> (BitAnd (var_id, left_val, right_val), ty)
      | BitwiseOr -> (BitOr (var_id, left_val, right_val), ty)
      | BitwiseXor -> (BitXor (var_id, left_val, right_val), ty)
      | LeftShift -> (Shl (var_id, left_val, right_val), ty)
      | ArithmeticRightShift -> (Shr (var_id, left_val, right_val), ty)
      | LogicalRightShift -> (Shrl (var_id, left_val, right_val), ty)
      | LessThan -> (Lt (var_id, left_val, right_val), `BoolT)
      | GreaterThan -> (Gt (var_id, left_val, right_val), `BoolT)
      | LessThanOrEqual -> (LtEq (var_id, left_val, right_val), `BoolT)
      | GreaterThanOrEqual -> (GtEq (var_id, left_val, right_val), `BoolT)
      | Equal
      | NotEqual ->
        failwith "Handled separately"
      | Is -> failwith "TODO: Emit Is expression"
    in
    Ecx.emit ~ecx instr;
    var_value_of_type var_id ty
  (*
   * ============================
   *         Identifiers
   * ============================
   *)
  | Identifier { loc = id_loc as loc; name }
  | ScopedIdentifier { loc; name = { Identifier.loc = id_loc; name }; _ } ->
    let binding = Bindings.get_value_binding ecx.pcx.bindings id_loc in
    let decl_loc = binding.loc in
    (match binding.declaration with
    (* May be an enum constructor, which is represented as an integer tag (with padding up to the
       size of the entire variant if not a pure enum). *)
    | CtorDecl { adt_sig; _ } ->
      let mir_adt_layout = Ecx.get_mir_adt_layout ~ecx adt_sig in
      (match mir_adt_layout.layout with
      | PureEnum { tags; _ } -> SMap.find name tags
      | Variants _ -> failwith "TODO: Emit enum variants"
      | Aggregate _
      | InlineValue _ ->
        failwith "Invalid layout for enum")
    (* Create function literal for functions *)
    | FunDecl { Bindings.FunctionDeclaration.type_params; is_builtin; _ } ->
      let func_name = mk_value_binding_name binding in
      let ty = type_of_loc ~ecx loc in
      let func_name =
        if type_params = [] || is_builtin then
          func_name
        else
          let (type_args, _, _) = Type_util.cast_to_function_type ty in
          Ecx.add_necessary_func_instantiation ~ecx func_name type_params type_args
      in
      `FunctionL func_name
    (* Variables may be either globals or locals *)
    | VarDecl { tvar; _ } ->
      let var =
        if Bindings.is_module_decl ecx.pcx.bindings decl_loc then (
          let binding_name = mk_value_binding_name binding in
          let global = SMap.find binding_name ecx.globals in
          let var_id = mk_cf_var_id () in
          Ecx.emit ~ecx (Load (var_id, `PointerL (global.ty, global.name)));
          var_id
        ) else
          mk_cf_local id_loc
      in
      let mir_ty = type_to_mir_type ~ecx (Types.Type.TVar tvar) in
      var_value_of_type var mir_ty
    | FunParamDecl { tvar }
    | MatchCaseVarDecl { tvar } ->
      let var = mk_cf_local id_loc in
      let mir_ty = type_to_mir_type ~ecx (Types.Type.TVar tvar) in
      var_value_of_type var mir_ty)
  (*
   * ============================
   *     Type Casts (ignored)
   * ============================
   *)
  | TypeCast { expr; _ } -> emit_expression ~ecx expr
  (*
   * ============================
   *        Tuple Literals
   * ============================
   *)
  | Tuple { loc; elements } ->
    let ty = type_of_loc ~ecx loc in
    let element_tys = Type_util.cast_to_tuple_type ty in
    let agg = Ecx.instantiate_tuple ~ecx element_tys in
    emit_construct_tuple ~ecx agg elements
  (*
   * ============================
   *        Method Calls
   * ============================
   *)
  | Call
      { loc; func = NamedAccess { loc = method_loc; name = method_name; target = receiver }; args }
    when Type_context.is_method_use ~cx:ecx.pcx.type_ctx method_name.loc ->
    let receiver_ty = type_of_loc ~ecx (Ast_utils.expression_loc receiver) in
    let receiver_val = emit_expression ~ecx receiver in
    let arg_vals = List.map (emit_expression ~ecx) args in
    let method_ty = type_of_loc ~ecx method_loc in
    let (method_instance_type_args, _, _) = Type_util.cast_to_function_type method_ty in
    let ret_type = mir_type_of_loc ~ecx loc in
    emit_method_call
      ~ecx
      ~method_name:method_name.name
      ~receiver_val
      ~receiver_ty
      ~arg_vals
      ~method_instance_type_args
      ~ret_type
  (*
   * =======================================
   *  Function Calls and Tuple Constructors
   * =======================================
   *)
  | Call { loc; func; args } ->
    (* Emit tuple constructor *)
    let ctor_result_opt =
      match func with
      | Identifier { Identifier.loc = ctor_id_loc; _ }
      | ScopedIdentifier { ScopedIdentifier.name = { Identifier.loc = ctor_id_loc; _ }; _ } ->
        let binding = Type_context.get_value_binding ~cx:ecx.pcx.type_ctx ctor_id_loc in
        (match binding.declaration with
        | CtorDecl _ ->
          (* Find MIR ADT layout this constructor *)
          let adt = type_of_loc ~ecx loc in
          let (type_args, adt_sig) = Type_util.cast_to_adt_type adt in
          let mir_adt_layout = Ecx.get_mir_adt_layout ~ecx adt_sig in
          (match mir_adt_layout.layout with
          (* If layout is an aggregate, construct tuple *)
          | Aggregate _ ->
            let agg = Ecx.instantiate_mir_adt_aggregate_layout ~ecx mir_adt_layout type_args in
            let agg_ptr_val = emit_construct_tuple ~ecx agg args in
            Some agg_ptr_val
          (* If layout is an inlined value there must have been a single arg, which we use directly
             without actually constructing a tuple. *)
          | InlineValue _ -> Some (emit_expression ~ecx (List.hd args))
          | Variants _ -> failwith "TODO: Emit tuple variants"
          | PureEnum _ -> failwith "Invalid layout for tuple")
        | _ -> None)
      | _ -> None
    in
    (* If not a tuple constructor, must be a call *)
    (match ctor_result_opt with
    | Some result -> result
    | None ->
      let func_val = emit_function_expression ~ecx func in
      let arg_vals = List.map (emit_expression ~ecx) args in
      let ret_type = mir_type_of_loc ~ecx loc in
      emit_call ~ecx ~func_val ~arg_vals ~receiver_val:None ~ret_type)
  (*
   * ============================
   *     Record Constructors
   * ============================
   *)
  | Record { Record.loc; fields; _ } ->
    let emit_field_value_expression { Record.Field.name; value; _ } =
      match value with
      | Some expr -> emit_expression ~ecx expr
      | None -> emit_expression ~ecx (Identifier name)
    in
    (* Find MIR ADT layout for this constructor *)
    let adt = type_of_loc ~ecx loc in
    let (type_args, adt_sig) = Type_util.cast_to_adt_type adt in
    let mir_adt_layout = Ecx.get_mir_adt_layout ~ecx adt_sig in
    (match mir_adt_layout.layout with
    (* If layout is an aggregate, construct record *)
    | Aggregate _ ->
      let agg = Ecx.instantiate_mir_adt_aggregate_layout ~ecx mir_adt_layout type_args in
      let agg_ty = `AggregateT agg in
      (* Call myte_alloc builtin to allocate space for record *)
      let agg_ptr_var_id = mk_cf_var_id () in
      let agg_ptr_var = `PointerV (agg_ty, agg_ptr_var_id) in
      let (agg_ptr_val, myte_alloc_instr) =
        Mir_builtin.(mk_call_builtin myte_alloc agg_ptr_var_id [`IntL Int32.one] [agg_ty])
      in
      Ecx.emit ~ecx myte_alloc_instr;
      (* Store each argument to the record constructor in space allocated for record *)
      List.iter
        (fun ({ Record.Field.name = { name; _ }; _ } as field) ->
          (* Calculate offset for this element and store *)
          let arg_var = emit_field_value_expression field in
          let (element_ty, element_idx) = lookup_element agg name in
          let (element_offset_var, get_ptr_instr) =
            mk_get_pointer_instr element_ty agg_ptr_var [GetPointer.FieldIndex element_idx]
          in
          Ecx.emit ~ecx (GetPointer get_ptr_instr);
          Ecx.emit ~ecx (Store (element_offset_var, arg_var)))
        fields;
      agg_ptr_val
    | Variants _ -> failwith "TODO: Emit record variants"
    | InlineValue _
    | PureEnum _ ->
      failwith "Invalid layout for record")
  (*
   * ============================
   *        Access Chains
   * ============================
   *)
  | IndexedAccess { loc; target; index; _ } ->
    let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
    (match target_ty with
    (* If indexing a vector, call Vec's `get` method instead of emitting access chain *)
    | ADT { adt_sig; _ } when adt_sig == !Std_lib.vec_adt_sig ->
      let target_var = emit_expression ~ecx target in
      let index_var = emit_expression ~ecx index in
      emit_call_vec_get ~ecx loc target_var index_var
    | _ -> emit_expression_access_chain_load ~ecx expr)
  | NamedAccess _ -> emit_expression_access_chain_load ~ecx expr
  (*
   * ============================
   *     String Interpolation
   * ============================
   *)
  | InterpolatedString { loc; parts } ->
    let (first_part, rest_parts) = List_utils.split_first parts in
    let string_ty = type_of_loc ~ecx loc in
    let string_type = type_to_mir_type ~ecx string_ty in
    (* String parts are emitted directly, expression parts have their `toString` method called *)
    let emit_part part =
      match part with
      | InterpolatedString.String string_literal ->
        emit_expression ~ecx (StringLiteral string_literal)
      | Expression expr ->
        let receiver_val = emit_expression ~ecx expr in
        let receiver_ty = type_of_loc ~ecx (Ast_utils.expression_loc expr) in
        emit_method_call
          ~ecx
          ~method_name:"toString"
          ~receiver_val
          ~receiver_ty
          ~arg_vals:[]
          ~method_instance_type_args:[]
          ~ret_type:string_type
    in
    (* Emit first part string, then if there are other parts emit each and call `append` on the
       first string part. *)
    let first_part_val = emit_part first_part in
    List.iter
      (fun part ->
        let part_val = emit_part part in
        ignore
          (emit_method_call
             ~ecx
             ~method_name:"append"
             ~receiver_val:first_part_val
             ~receiver_ty:string_ty
             ~arg_vals:[part_val]
             ~method_instance_type_args:[]
             ~ret_type:`UnitT))
      rest_parts;
    first_part_val
  (*
   * ============================
   *     Ternary Expression
   * ============================
   *)
  | Ternary { loc; test; conseq; altern } ->
    let var_id = mk_cf_var_id () in
    let mir_type = mir_type_of_loc ~ecx loc in
    (* Branch to conseq or altern blocks *)
    let test_val = emit_bool_expression ~ecx test in
    let conseq_builder = Ecx.mk_block_builder ~ecx in
    let altern_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_branch ~ecx test_val conseq_builder.id altern_builder.id;
    (* Emit conseq and continue to join block *)
    Ecx.set_block_builder ~ecx conseq_builder;
    let conseq_val = emit_expression ~ecx conseq in
    let conseq_var_id = emit_var_of_value ~ecx conseq_val in
    let conseq_end_block_id = Ecx.get_block_builder_id_throws ~ecx in
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Emit altern and continue to join block *)
    Ecx.set_block_builder ~ecx altern_builder;
    let altern_val = emit_expression ~ecx altern in
    let altern_var_id = emit_var_of_value ~ecx altern_val in
    let altern_end_block_id = Ecx.get_block_builder_id_throws ~ecx in
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Join branches together and emit explicit phi *)
    Ecx.set_block_builder ~ecx join_builder;
    Ecx.emit_phi
      ~ecx
      mir_type
      var_id
      (IMap.add
         conseq_end_block_id
         conseq_var_id
         (IMap.singleton altern_end_block_id altern_var_id));
    var_value_of_type var_id mir_type
  | Match _ -> failwith "TODO: Emir MIR for match expressions"
  | Super _ -> failwith "TODO: Emit MIR for super expressions"

and emit_expression_access_chain_load ~ecx expr =
  match emit_expression_access_chain ~ecx expr with
  | GetPointerEmittedResult element_pointer_val ->
    let var_id = mk_cf_var_id () in
    Ecx.emit ~ecx (Load (var_id, element_pointer_val));
    var_value_of_type var_id (pointer_value_element_type element_pointer_val)
  | InlinedValueResult value -> value

and emit_expression_access_chain_store ~ecx expr_lvalue expr =
  match emit_expression_access_chain ~ecx expr_lvalue with
  | GetPointerEmittedResult element_pointer_val ->
    let expr_val = emit_expression ~ecx expr in
    Ecx.emit ~ecx (Store (element_pointer_val, expr_val))
  | InlinedValueResult _ -> failwith "Cannot store to inlined value"

and emit_expression_access_chain ~ecx expr =
  let open Expression in
  let open Instruction in
  let emit_get_pointer_instr target_var element_mir_ty offsets =
    let (pointer_offset, offsets) =
      match List.rev offsets with
      | GetPointer.PointerIndex pointer_idx :: rest_offsets -> (Some pointer_idx, rest_offsets)
      | offsets -> (None, offsets)
    in
    let target_ptr_var = cast_to_pointer_value target_var in
    let (element_pointer_var, get_ptr_instr) =
      mk_get_pointer_instr ~pointer_offset element_mir_ty target_ptr_var offsets
    in
    Ecx.emit ~ecx (GetPointer get_ptr_instr);
    element_pointer_var
  in
  let emit_load_instr element_pointer_var =
    let var_id = mk_cf_var_id () in
    Ecx.emit ~ecx (Load (var_id, element_pointer_var));
    var_value_of_type var_id (pointer_value_element_type element_pointer_var)
  in
  let maybe_emit_get_pointer_and_load_instrs root_var ty offsets =
    if offsets = [] then
      root_var
    else
      let element_pointer_var = emit_get_pointer_instr root_var (Ecx.to_mir_type ~ecx ty) offsets in
      emit_load_instr element_pointer_var
  in
  let rec emit_access_expression expr =
    match expr with
    | IndexedAccess ({ target; _ } as access) ->
      let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
      (match target_ty with
      | Types.Type.Array _ -> emit_array_indexed_access access
      | Tuple _ -> emit_tuple_indexed_access access
      | ADT { adt_sig; _ } when adt_sig == !Std_lib.vec_adt_sig -> emit_vec_indexed_access access
      | ADT { adt_sig = { variants; _ }; _ } when SMap.cardinal variants = 1 ->
        emit_tuple_indexed_access access
      | _ -> failwith "Target must be a tuple to pass type checking")
    | NamedAccess ({ target; _ } as access) ->
      let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
      (match target_ty with
      | Types.Type.ADT { adt_sig = { variants; _ }; _ } when SMap.cardinal variants = 1 ->
        emit_record_named_access access
      | _ -> failwith "Target must be a record to pass type checking")
    | _ -> (emit_expression ~ecx expr, [])
  and emit_tuple_indexed_access { IndexedAccess.target; index; _ } =
    let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
    let (target_root_var, target_offsets) = emit_access_expression target in
    let root_var =
      maybe_emit_get_pointer_and_load_instrs target_root_var target_ty target_offsets
    in
    (* Extract tuple element index from integer literal and add to GetPointer offsets *)
    let emit_get_pointer_index () =
      let element_idx =
        match index with
        | IntLiteral { IntLiteral.raw; base; _ } ->
          Integers.int64_of_string_opt raw base |> Option.get |> Int64.to_int
        | _ -> failwith "Index of a tuple must be an int literal to pass type checking"
      in
      (root_var, [GetPointer.FieldIndex element_idx])
    in
    match target_ty with
    | Tuple _ -> emit_get_pointer_index ()
    (* If layout is aggregate, index like normal tuple, otherwise is single element tuple so do
       not need to index at all. *)
    | ADT { adt_sig; _ } ->
      let mir_adt_layout = Ecx.get_mir_adt_layout ~ecx adt_sig in
      (match mir_adt_layout.layout with
      | Aggregate _ -> emit_get_pointer_index ()
      | InlineValue _ -> (root_var, [])
      | Variants _
      | PureEnum _ ->
        failwith "Invalid layout for indexed access")
    | _ -> failwith "Indexed access must be on tuple or ADT type"
  and emit_array_indexed_access { IndexedAccess.target; index; _ } =
    let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
    let (target_root_var, target_offsets) = emit_access_expression target in
    let root_var =
      maybe_emit_get_pointer_and_load_instrs target_root_var target_ty target_offsets
    in
    let index_var = emit_numeric_expression ~ecx index in
    (root_var, [GetPointer.PointerIndex index_var])
  (* An indexed access on a Vec calls the Vec's `get` method *)
  and emit_vec_indexed_access { IndexedAccess.loc; target; index; _ } =
    let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
    let (target_root_var, target_offsets) = emit_access_expression target in
    let target_var =
      maybe_emit_get_pointer_and_load_instrs target_root_var target_ty target_offsets
    in
    let index_var = emit_expression ~ecx index in
    let vec_get_result = emit_call_vec_get ~ecx loc target_var index_var in
    (vec_get_result, [])
  and emit_record_named_access { NamedAccess.target; name = { name; _ }; _ } =
    let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
    let (target_root_var, target_offsets) = emit_access_expression target in
    let root_var =
      maybe_emit_get_pointer_and_load_instrs target_root_var target_ty target_offsets
    in
    (* Find MIR ADT layout for this record *)
    let (type_args, adt_sig) = Type_util.cast_to_adt_type target_ty in
    let mir_adt_layout = Ecx.get_mir_adt_layout ~ecx adt_sig in
    match mir_adt_layout.layout with
    (* If layout is aggregate, index its field with a GetPointer offset *)
    | Aggregate _ ->
      let agg = Ecx.instantiate_mir_adt_aggregate_layout ~ecx mir_adt_layout type_args in
      (* Find element index in the corresponding aggregate type *)
      let (_, element_idx) = lookup_element agg name in
      (root_var, [GetPointer.FieldIndex element_idx])
    | Variants _
    | PureEnum _
    | InlineValue _ ->
      failwith "Invalid layout for record"
  in
  let (target_var, offsets) =
    match expr with
    | IndexedAccess _
    | NamedAccess _ ->
      emit_access_expression expr
    | _ -> failwith "Must be called on access expression"
  in
  if offsets = [] then
    InlinedValueResult target_var
  else
    let element_mir_ty = mir_type_of_loc ~ecx (Ast_utils.expression_loc expr) in
    GetPointerEmittedResult (emit_get_pointer_instr target_var element_mir_ty offsets)

and emit_method_call
    ~ecx
    ~(method_name : string)
    ~(receiver_val : cf_value)
    ~(receiver_ty : Types.Type.t)
    ~(arg_vals : cf_value list)
    ~(method_instance_type_args : Types.Type.t list)
    ~(ret_type : Type.t) =
  (* Emit receiver and gather its ADT signature and type args *)
  let (adt_sig, receiver_type_args) =
    match receiver_ty with
    | ADT { adt_sig; type_args; _ } -> (adt_sig, type_args)
    | _ -> (Std_lib.get_primitive_adt_sig receiver_ty, [])
  in

  (* Find method, along with the trait it was defined in *)
  let method_sig = Types.AdtSig.lookup_method adt_sig method_name in
  let method_sig = Option.get method_sig in
  let source_trait_instance = method_sig.source_trait_instance in
  let source_trait_sig = source_trait_instance.trait_sig in

  (* Calculate generic keys from trait/type *)
  let (extra_key_type_params, extra_key_type_args) =
    if method_sig.trait_sig == source_trait_sig then
      (* Method was directly declared on the ADT's trait. Substitute type args for trait's type args *)
      (source_trait_sig.type_params, receiver_type_args)
    else
      (* Method was declared on a trait that the ADT implements. The source trait instance's type
         args are in terms of the ADT trait's type params, so first map from the ADT trait's type
         params to the actual ADT instance's type args in the source trait instance's type args.
         The key args are these mapped source trait instance's type args along with the a `this`
         type of the ADT instance. *)
      let bindings =
        Types.bind_type_params_to_args method_sig.trait_sig.type_params receiver_type_args
      in
      let type_args =
        List.map (Types.substitute_type_params bindings) source_trait_instance.type_args
      in
      let all_key_type_params = source_trait_sig.this_type_param :: source_trait_sig.type_params in
      let all_key_type_args = receiver_ty :: type_args in
      (all_key_type_params, all_key_type_args)
  in

  (* Full keys are trait/type args plus method's own generic args *)
  let key_type_params = extra_key_type_params @ method_sig.type_params in
  let key_type_args = extra_key_type_args @ method_instance_type_args in

  (* Find name of method fully qualified by module and trait *)
  let method_binding = Bindings.get_value_binding ecx.pcx.bindings method_sig.loc in
  let fully_qualified_method_name = mk_value_binding_name method_binding in

  let full_method_name_with_type_args =
    Ecx.add_necessary_func_instantiation
      ~ecx
      fully_qualified_method_name
      key_type_params
      key_type_args
  in
  let func_val = `FunctionL full_method_name_with_type_args in
  emit_call ~ecx ~func_val ~arg_vals ~receiver_val:(Some receiver_val) ~ret_type

and emit_call
    ~ecx
    ~(func_val : cf_var Value.function_value)
    ~(arg_vals : cf_value list)
    ~(receiver_val : cf_value option)
    ~(ret_type : Type.t) =
  let var_id = mk_cf_var_id () in
  (* Pass `this` value as first argument if this is a method call *)
  let arg_vals =
    match receiver_val with
    | None -> arg_vals
    | Some receiver_val -> receiver_val :: arg_vals
  in
  (* Generate builtin function *)
  let builtin_functions = Lazy.force_val builtin_functions in
  let builtin_ret_opt =
    match func_val with
    | `FunctionL name ->
      (match SMap.find_opt name builtin_functions with
      | None -> None
      | Some mk_func -> Some (mk_func ~ecx arg_vals ret_type))
    | _ -> None
  in
  match builtin_ret_opt with
  | Some ret_val -> ret_val
  | None ->
    Ecx.emit ~ecx (Call (var_id, ret_type, func_val, arg_vals));
    var_value_of_type var_id ret_type

and builtin_functions =
  lazy
    ( [
        (Std_lib.std_bool_bool_equals, emit_eq);
        (Std_lib.std_byte_byte_equals, emit_eq);
        (Std_lib.std_byte_byte_toInt, emit_std_byte_byte_toInt);
        (Std_lib.std_byte_byte_toLong, emit_std_byte_byte_toLong);
        (Std_lib.std_int_int_equals, emit_eq);
        (Std_lib.std_int_int_toByte, emit_std_int_int_toByte);
        (Std_lib.std_int_int_toLong, emit_std_int_int_toLong);
        (Std_lib.std_long_long_equals, emit_eq);
        (Std_lib.std_long_long_toByte, emit_std_long_long_toByte);
        (Std_lib.std_long_long_toInt, emit_std_long_long_toInt);
        (Std_lib.std_memory_array_copy, emit_std_memory_array_copy);
        (Std_lib.std_memory_array_new, emit_std_memory_array_new);
        (Std_lib.std_io_write, emit_std_io_write);
        (Std_lib.std_sys_exit, emit_std_sys_exit);
        (Std_lib.std_unit_unit_equals, emit_eq);
      ]
    |> List.to_seq
    |> SMap.of_seq )

and emit_eq ~ecx arg_vals _ =
  match arg_vals with
  | [left_arg; right_arg] ->
    let var_id = mk_cf_var_id () in
    Ecx.emit
      ~ecx
      (Eq (var_id, cast_to_comparable_value left_arg, cast_to_comparable_value right_arg));
    var_value_of_type var_id `BoolT
  | _ -> failwith "Expected two arguments"

and emit_std_byte_byte_toInt ~ecx arg_vals _ =
  let var_id = mk_cf_var_id () in
  Ecx.emit ~ecx (SExt (var_id, cast_to_numeric_value (List.hd arg_vals), `IntT));
  var_value_of_type var_id `IntT

and emit_std_byte_byte_toLong ~ecx arg_vals _ =
  let var_id = mk_cf_var_id () in
  Ecx.emit ~ecx (SExt (var_id, cast_to_numeric_value (List.hd arg_vals), `LongT));
  var_value_of_type var_id `LongT

and emit_std_int_int_toByte ~ecx arg_vals _ =
  let var_id = mk_cf_var_id () in
  Ecx.emit ~ecx (Trunc (var_id, cast_to_numeric_value (List.hd arg_vals), `ByteT));
  var_value_of_type var_id `ByteT

and emit_std_int_int_toLong ~ecx arg_vals _ =
  let var_id = mk_cf_var_id () in
  Ecx.emit ~ecx (SExt (var_id, cast_to_numeric_value (List.hd arg_vals), `LongT));
  var_value_of_type var_id `LongT

and emit_std_long_long_toByte ~ecx arg_vals _ =
  let var_id = mk_cf_var_id () in
  Ecx.emit ~ecx (Trunc (var_id, cast_to_numeric_value (List.hd arg_vals), `ByteT));
  var_value_of_type var_id `ByteT

and emit_std_long_long_toInt ~ecx arg_vals _ =
  let var_id = mk_cf_var_id () in
  Ecx.emit ~ecx (Trunc (var_id, cast_to_numeric_value (List.hd arg_vals), `IntT));
  var_value_of_type var_id `IntT

and emit_std_memory_array_copy ~ecx arg_vals _ =
  (* If the index is nonzero, emit a GetPointer instruction to calculate the pointer's start *)
  let maybe_emit_index_past_ptr ptr_val index_val =
    match cast_to_numeric_value index_val with
    | `IntL lit when lit = Int32.zero -> ptr_val
    | index_val ->
      let ptr_val = cast_to_pointer_value ptr_val in
      let ptr_ty = pointer_value_element_type ptr_val in
      let (get_ptr_val, get_ptr_instr) =
        mk_get_pointer_instr ~pointer_offset:(Some index_val) ptr_ty ptr_val []
      in
      Ecx.emit ~ecx (GetPointer get_ptr_instr);
      (get_ptr_val :> cf_value)
  in
  match arg_vals with
  | [dest_array; dest_index; src_array; src_index; count] ->
    let dest_ptr = maybe_emit_index_past_ptr dest_array dest_index in
    let src_ptr = maybe_emit_index_past_ptr src_array src_index in
    let var_id = mk_cf_var_id () in
    let (return_val, myte_copy_instr) =
      Mir_builtin.(mk_call_builtin myte_copy var_id [dest_ptr; src_ptr; count] [])
    in
    Ecx.emit ~ecx myte_copy_instr;
    return_val
  | _ -> failwith "Array.copy expects five arguments"

and emit_std_memory_array_new ~ecx arg_vals ret_type =
  let var_id = mk_cf_var_id () in
  let (`PointerT element_ty) = cast_to_pointer_type ret_type in
  let (array_ptr_val, myte_alloc_instr) =
    Mir_builtin.(mk_call_builtin myte_alloc var_id arg_vals [element_ty])
  in
  Ecx.emit ~ecx myte_alloc_instr;
  array_ptr_val

and emit_std_io_write ~ecx arg_vals _ =
  let var_id = mk_cf_var_id () in
  let (return_val, instr) = Mir_builtin.(mk_call_builtin myte_write var_id arg_vals []) in
  Ecx.emit ~ecx instr;
  return_val

and emit_std_sys_exit ~ecx arg_vals _ =
  let var_id = mk_cf_var_id () in
  let (return_val, instr) = Mir_builtin.(mk_call_builtin myte_exit var_id arg_vals []) in
  Ecx.emit ~ecx instr;
  return_val

and emit_call_vec_get ~ecx return_loc vec_var index_var =
  (* Get full name for Vec's `get` method *)
  let vec_get_method_sig = Types.AdtSig.lookup_method !Std_lib.vec_adt_sig "get" |> Option.get in
  let vec_get_binding = Bindings.get_value_binding ecx.pcx.bindings vec_get_method_sig.loc in
  let vec_get_name = mk_value_binding_name vec_get_binding in
  (* Find element type and instantiate Vec's `get` method *)
  let element_ty = type_of_loc ~ecx return_loc in
  let func_name =
    Ecx.add_necessary_func_instantiation
      ~ecx
      vec_get_name
      vec_get_method_sig.trait_sig.type_params
      [element_ty]
  in
  (* Emit call to Vec's `get` method *)
  let var_id = mk_cf_var_id () in
  let ret_ty = Ecx.to_mir_type ~ecx element_ty in
  Ecx.emit ~ecx (Call (var_id, ret_ty, `FunctionL func_name, [vec_var; index_var]));
  var_value_of_type var_id ret_ty

and emit_call_vec_set ~ecx element_ty_loc vec_var index_var expr_var =
  (* Get full name for Vec's `set` method *)
  let vec_get_method_sig = Types.AdtSig.lookup_method !Std_lib.vec_adt_sig "set" |> Option.get in
  let vec_get_binding = Bindings.get_value_binding ecx.Ecx.pcx.bindings vec_get_method_sig.loc in
  let vec_get_name = mk_value_binding_name vec_get_binding in
  (* Find element type and instantiate Vec's `get` method *)
  let element_ty = type_of_loc ~ecx element_ty_loc in
  let func_name =
    Ecx.add_necessary_func_instantiation
      ~ecx
      vec_get_name
      vec_get_method_sig.trait_sig.type_params
      [element_ty]
  in
  (* Emit call of Vec's `set` method *)
  let var_id = mk_cf_var_id () in
  Ecx.emit ~ecx (Call (var_id, `UnitT, `FunctionL func_name, [vec_var; index_var; expr_var]))

and emit_construct_tuple ~ecx agg elements =
  let agg_ty = `AggregateT agg in
  (* Call myte_alloc builtin to allocate space for tuple *)
  let agg_ptr_var_id = mk_cf_var_id () in
  let agg_ptr_var = `PointerV (agg_ty, agg_ptr_var_id) in
  let (agg_ptr_val, myte_alloc_instr) =
    Mir_builtin.(mk_call_builtin myte_alloc agg_ptr_var_id [`IntL Int32.one] [agg_ty])
  in
  Ecx.emit ~ecx myte_alloc_instr;
  (* Store each argument to the tuple constructor in space allocated for tuple *)
  List_utils.iteri2
    (fun i arg (_, element_ty) ->
      (* Calculate offset for this element and store *)
      let arg_var = emit_expression ~ecx arg in
      let (element_offset_var, get_ptr_instr) =
        mk_get_pointer_instr element_ty agg_ptr_var [Instruction.GetPointer.FieldIndex i]
      in
      Ecx.emit ~ecx (GetPointer get_ptr_instr);
      Ecx.emit ~ecx (Store (element_offset_var, arg_var)))
    elements
    agg.Aggregate.elements;
  agg_ptr_val

and emit_bool_expression ~ecx expr =
  match emit_expression ~ecx expr with
  | (`BoolL _ | `BoolV _) as v -> v
  | _ -> failwith "Expected bool value"

and emit_numeric_expression ~ecx expr =
  match emit_expression ~ecx expr with
  | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _ | `LongV _) as v -> v
  | _ -> failwith "Expected numeric value"

and emit_function_expression ~ecx expr =
  match emit_expression ~ecx expr with
  | (`FunctionL _ | `FunctionV _) as v -> v
  | _ -> failwith "Expected function value"

and emit_statement ~ecx stmt =
  let open Statement in
  match stmt with
  | Expression (_, expr) -> ignore (emit_expression ~ecx expr)
  | Block { statements; _ } -> List.iter (emit_statement ~ecx) statements
  | If { loc = _; test; conseq; altern = None } ->
    (* Branch to conseq or join blocks *)
    let test_val = emit_bool_expression ~ecx test in
    let conseq_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_branch ~ecx test_val conseq_builder.id join_builder.id;
    (* Emit conseq and continue to join block *)
    Ecx.set_block_builder ~ecx conseq_builder;
    emit_statement ~ecx conseq;
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Start join block *)
    Ecx.set_block_builder ~ecx join_builder
  | If { loc = _; test; conseq; altern = Some altern } ->
    (* Branch to conseq or altern blocks *)
    let test_val = emit_bool_expression ~ecx test in
    let conseq_builder = Ecx.mk_block_builder ~ecx in
    let altern_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_branch ~ecx test_val conseq_builder.id altern_builder.id;
    (* Emit conseq and continue to join block *)
    Ecx.set_block_builder ~ecx conseq_builder;
    emit_statement ~ecx conseq;
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Emit altern and continue to join block *)
    Ecx.set_block_builder ~ecx altern_builder;
    emit_statement ~ecx altern;
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Start join block *)
    Ecx.set_block_builder ~ecx join_builder
  | While { loc = _; test; body } ->
    let test_builder = Ecx.mk_block_builder ~ecx in
    let body_builder = Ecx.mk_block_builder ~ecx in
    let finish_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_continue ~ecx test_builder.id;
    (* Emit test block which branches to finish or body blocks *)
    Ecx.set_block_builder ~ecx test_builder;
    let test_val = emit_bool_expression ~ecx test in
    Ecx.finish_block_branch ~ecx test_val body_builder.id finish_builder.id;
    (* Emit body block which continues to test block *)
    Ecx.push_loop_context ~ecx finish_builder.id test_builder.id;
    Ecx.set_block_builder ~ecx body_builder;
    emit_statement ~ecx body;
    Ecx.finish_block_continue ~ecx test_builder.id;
    Ecx.pop_loop_context ~ecx;
    (* Start join block *)
    Ecx.set_block_builder ~ecx finish_builder
  | Return { loc = _; arg } ->
    let arg_val = Option.map (emit_expression ~ecx) arg in
    (* Handle implicit return from main function *)
    let arg_val =
      match arg_val with
      | None
      | Some (`UnitL | `UnitV _)
        when ecx.current_is_main ->
        Some (`IntL Int32.zero)
      | _ -> arg_val
    in
    Ecx.emit ~ecx (Ret arg_val)
  | Continue _ ->
    let (_, continue_id) = Ecx.get_loop_context ~ecx in
    Ecx.finish_block_continue ~ecx continue_id
  | Break _ ->
    let (break_id, _) = Ecx.get_loop_context ~ecx in
    Ecx.finish_block_continue ~ecx break_id
  | Assignment { loc = _; lvalue; expr } ->
    (match lvalue with
    | Assignment.Pattern pattern ->
      let { Identifier.loc = use_loc; _ } = List.hd (Ast_utils.ids_of_pattern pattern) in
      let binding = Bindings.get_value_binding ecx.pcx.bindings use_loc in
      let expr_val = emit_expression ~ecx expr in
      if Bindings.is_module_decl ecx.pcx.bindings binding.loc then
        let binding_name = mk_value_binding_name binding in
        let global = SMap.find binding_name ecx.globals in
        Ecx.emit ~ecx (Store (`PointerL (global.ty, global.name), expr_val))
      else
        Ecx.emit ~ecx (Mov (mk_cf_local use_loc, expr_val))
    | Assignment.Expression (IndexedAccess { loc; target; index; _ } as expr_lvalue) ->
      let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
      (match target_ty with
      (* If indexing a vector, call Vec's `set` method instead of emitting access chain *)
      | ADT { adt_sig; _ } when adt_sig == !Std_lib.vec_adt_sig ->
        let target_var = emit_expression ~ecx target in
        let index_var = emit_expression ~ecx index in
        let expr_var = emit_expression ~ecx expr in
        emit_call_vec_set ~ecx loc target_var index_var expr_var
      | _ -> emit_expression_access_chain_store ~ecx expr_lvalue expr)
    | Assignment.Expression (NamedAccess _ as expr_lvalue) ->
      emit_expression_access_chain_store ~ecx expr_lvalue expr
    | _ -> failwith "Lvalue expression must be an access")
  | VariableDeclaration { pattern; init; _ } ->
    (* TODO: Emit MIR for arbitrary patterns *)
    let { Identifier.loc; _ } = List.hd (Ast_utils.ids_of_pattern pattern) in
    let init_val = emit_expression ~ecx init in
    Ecx.emit ~ecx (Mov (mk_cf_local loc, init_val))
  | Match _ -> failwith "TODO: Emit MIR for match statements"
  | FunctionDeclaration _ -> failwith "TODO: Emit MIR for non-toplevel function declarations"

(* Return a variable id for the given value. Variables return their id directly, while literal
   values must first emit a Mov to a variablem then return the new variable id. *)
and emit_var_of_value ~ecx (value : cf_value) : cf_var =
  match value with
  | `UnitV var_id
  | `BoolV var_id
  | `ByteV var_id
  | `IntV var_id
  | `LongV var_id
  | `FunctionV var_id
  | `PointerV (_, var_id)
  | `AggregateV (_, var_id)
  | `ArrayV (_, _, var_id) ->
    var_id
  | `UnitL
  | `BoolL _
  | `ByteL _
  | `IntL _
  | `LongL _
  | `FunctionL _
  | `PointerL _
  | `ArrayL _ ->
    let var_id = mk_cf_var_id () in
    Ecx.emit ~ecx (Mov (var_id, value));
    var_id

and mk_cf_var_id () = Id (mk_var_id ())

and mk_cf_local loc = Local loc

and mk_value_binding_name binding =
  match binding.context with
  | Module
  | Function ->
    String.concat "." (binding.module_ @ [binding.name])
  | Trait trait_name -> String.concat "." (binding.module_ @ [trait_name; binding.name])

and mk_get_pointer_instr ?(pointer_offset = None) return_ty pointer offsets =
  let var_id = mk_cf_var_id () in
  let var = `PointerV (return_ty, var_id) in
  (var, { Instruction.GetPointer.var_id; return_ty; pointer; pointer_offset; offsets })

and type_of_loc ~ecx loc =
  let tvar_id = Type_context.get_tvar_from_loc ~cx:ecx.pcx.type_ctx loc in
  Ecx.find_rep_non_generic_type ~ecx (TVar tvar_id)

and mir_type_of_loc ~ecx loc =
  let tvar_id = Type_context.get_tvar_from_loc ~cx:ecx.pcx.type_ctx loc in
  type_to_mir_type ~ecx (Types.Type.TVar tvar_id)

and type_to_mir_type ~ecx ty =
  let ty = Ecx.find_rep_non_generic_type ~ecx ty in
  Ecx.to_mir_type ~ecx ty