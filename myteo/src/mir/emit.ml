open Ast
open Basic_collections
open Mir
module Ecx = Emit_context
module Pcx = Program_context

let rec emit_control_flow_ir (pcx : Pcx.t) : Ecx.t * cf_program =
  let ecx = Emit_context.mk () in
  List.iter
    (fun mod_ ->
      let open Ast.Module in
      let name = Ast_utils.string_of_scoped_ident (snd mod_).module_.name in
      Ecx.start_module ~ecx name;
      emit_module ~pcx ~ecx mod_;
      Ecx.end_module ~ecx)
    pcx.modules;
  ( ecx,
    {
      Program.main_id = ecx.main_id;
      blocks = Ecx.builders_to_blocks ecx.blocks;
      globals = ecx.globals;
      funcs = ecx.funcs;
      types = ecx.types;
      modules = ecx.modules;
    } )

and emit_module ~pcx ~ecx (_, mod_) =
  let open Ast.Module in
  List.iter
    (fun toplevel ->
      match toplevel with
      | TypeDeclaration decl -> emit_type_declaration_prepass ~pcx ~ecx decl
      | _ -> ())
    mod_.toplevels;
  List.iter
    (fun toplevel ->
      match toplevel with
      | TypeDeclaration decl -> emit_type_declaration ~pcx ~ecx decl
      | _ -> ())
    mod_.toplevels;
  List.iter
    (fun toplevel ->
      match toplevel with
      | VariableDeclaration decl -> emit_toplevel_variable_declaration ~pcx ~ecx decl
      | _ -> ())
    mod_.toplevels;
  List.iter
    (fun toplevel ->
      match toplevel with
      | FunctionDeclaration decl -> emit_toplevel_function_declaration ~pcx ~ecx decl
      | _ -> ())
    mod_.toplevels

(* Prepass to create and store initial empty aggregate types for every type.
   This prepass allows for out-of-order type definitions. *)
and emit_type_declaration_prepass ~pcx ~ecx decl =
  let open TypeDeclaration in
  let { name = { Identifier.loc; name }; decl; _ } = decl in
  let name = Printf.sprintf "%s.%s" (Ecx.get_module_builder ~ecx).name name in
  match decl with
  | Tuple _
  | Record _ ->
    let tvar_id = Bindings.get_tvar_id_from_type_decl pcx.bindings loc in
    let ty = Type_context.find_rep_type ~cx:pcx.type_ctx (TVar tvar_id) in
    let agg = { Aggregate.name; loc; elements = [] } in
    Ecx.add_type ~ecx agg;
    Types.TypeHashtbl.add ecx.adt_to_agg_type ty agg
  | _ -> ()

(* Fill aggregate types with correct element types *)
and emit_type_declaration ~pcx ~ecx decl =
  let open TypeDeclaration in
  let { name = { Identifier.loc; name }; decl; builtin; _ } = decl in
  if builtin then
    ()
  else
    let tvar_id = Bindings.get_tvar_id_from_type_decl pcx.bindings loc in
    let ty = Type_context.find_rep_type ~cx:pcx.type_ctx (TVar tvar_id) in
    match decl with
    | Tuple _ ->
      let adt_sig = Types.get_adt_sig ty in
      let element_sigs = Types.get_tuple_variant adt_sig name in
      let element_types =
        List.map
          (fun element_sig ->
            let element_ty = type_to_mir_type ~pcx ~ecx element_sig in
            (None, element_ty))
          element_sigs
      in
      let agg = Types.TypeHashtbl.find ecx.adt_to_agg_type ty in
      agg.elements <- element_types
    | Record { fields; _ } ->
      let adt_sig = Types.get_adt_sig ty in
      let field_sigs = Types.get_record_variant adt_sig name in
      (* Collect fields for aggregate in order they are declared in *)
      let element_types =
        List.fold_left
          (fun element_types { Record.Field.name = { Identifier.name; _ }; _ } ->
            let sig_ty = SMap.find name field_sigs in
            let element_ty = type_to_mir_type ~pcx ~ecx sig_ty in
            (Some name, element_ty) :: element_types)
          []
          fields
      in
      let agg = Types.TypeHashtbl.find ecx.adt_to_agg_type ty in
      agg.elements <- List.rev element_types
    | Variant _
    | Alias _ ->
      ()

and emit_toplevel_variable_declaration ~pcx ~ecx decl =
  let { Statement.VariableDeclaration.pattern; init; _ } = decl in
  (* TODO: Emit MIR for arbitrary patterns *)
  let { Identifier.loc; name } = List.hd (Ast_utils.ids_of_pattern pattern) in
  let name = Printf.sprintf "%s.%s" (Ecx.get_module_builder ~ecx).name name in
  (* Find value type of variable *)
  let ty = mir_type_of_value_decl_loc ~pcx ~ecx loc in
  (* Build IR for variable init *)
  Ecx.start_block_sequence ~ecx (GlobalInit name);
  let init_start_block = Ecx.start_new_block ~ecx in
  let init_val = emit_expression ~pcx ~ecx init in
  let var = mk_cf_var_id () in
  Ecx.emit ~ecx (Store (`PointerV (ty, var), init_val));
  Ecx.finish_block_halt ~ecx;
  Ecx.add_global ~ecx { Global.loc; name; ty; var; init_start_block; init_val }

and emit_toplevel_function_declaration ~pcx ~ecx decl =
  let open Ast.Function in
  let { name = { Identifier.loc; name }; params; body; builtin; _ } = decl in
  let name = Printf.sprintf "%s.%s" (Ecx.get_module_builder ~ecx).name name in
  if builtin then
    ()
  else
    (* Build IR for function body *)
    let param_locs_and_ids =
      List.map (fun { Param.name = { Identifier.loc; _ }; _ } -> (loc, mk_var_id ())) params
    in
    let body_start_block =
      Ecx.start_block_sequence ~ecx (FunctionBody name);
      let body_start_block = Ecx.start_new_block ~ecx in
      if loc = Option.get pcx.main_loc then ecx.main_id <- body_start_block;
      (match body with
      | Block { Statement.Block.statements; _ } ->
        List.iter (emit_statement ~pcx ~ecx) statements;
        (* Add an implicit return if the last instruction is not a return *)
        (match ecx.current_block_builder with
        | Some { Ecx.BlockBuilder.instructions = (_, Ret _) :: _; _ } -> ()
        | _ -> Ecx.emit ~ecx (Ret None))
      | Expression expr ->
        let ret_val = emit_expression ~pcx ~ecx expr in
        Ecx.emit ~ecx (Ret (Some ret_val)));
      Ecx.finish_block_halt ~ecx;
      body_start_block
    in
    (* Find value type of function *)
    let func_tvar_id = Bindings.get_tvar_id_from_value_decl pcx.bindings loc in
    let (param_tys, return_ty) =
      match Type_context.find_rep_type ~cx:pcx.type_ctx (Types.TVar func_tvar_id) with
      | Types.Function { tparams = _; params; return } ->
        (List.map (type_to_mir_type ~pcx ~ecx) params, type_to_mir_type ~pcx ~ecx return)
      | _ -> failwith "Function must resolve to function type"
    in
    let params =
      List.map2 (fun (loc, var_id) ty -> (loc, var_id, ty)) param_locs_and_ids param_tys
    in
    Ecx.add_function ~ecx { Function.loc; name; params; return_ty; body_start_block }

and emit_expression ~pcx ~ecx expr =
  let open Expression in
  let open Instruction in
  match expr with
  | Unit _ -> `UnitL
  | IntLiteral { loc; raw; base } ->
    let value = Integers.int64_of_string_opt raw base |> Option.get in
    let ty = mir_type_of_loc ~pcx ~ecx loc in
    (match ty with
    | `ByteT -> `ByteL (Int64.to_int value)
    | `IntT -> `IntL (Int64.to_int32 value)
    | `LongT -> `LongL value
    | _ -> failwith "Int literal must have integer type")
  | StringLiteral { value; _ } -> `StringL value
  | BoolLiteral { value; _ } -> `BoolL value
  | UnaryOperation { op = Plus; operand; _ } -> emit_expression ~pcx ~ecx operand
  | UnaryOperation { loc; op = Minus; operand } ->
    let var_id = mk_cf_var_id () in
    let operand_val = emit_numeric_expression ~pcx ~ecx operand in
    let ty = mir_type_of_loc ~pcx ~ecx loc in
    Ecx.emit ~ecx (Neg (var_id, operand_val));
    var_value_of_type var_id ty
  | UnaryOperation { op = Not; loc; operand } ->
    let var_id = mk_cf_var_id () in
    let value_ty = mir_type_of_loc ~pcx ~ecx loc in
    (match value_ty with
    | `BoolT ->
      let operand_val = emit_bool_expression ~pcx ~ecx operand in
      Ecx.emit ~ecx (LogNot (var_id, operand_val));
      `BoolV var_id
    | `ByteT
    | `IntT
    | `LongT ->
      let operand_val = emit_numeric_expression ~pcx ~ecx operand in
      Ecx.emit ~ecx (BitNot (var_id, operand_val));
      var_value_of_type var_id value_ty
    | _ -> failwith "Not argument must be a bool or int")
  | LogicalAnd { loc = _; left; right } ->
    (* Short circuit when lhs is false by jumping to false case *)
    let rhs_builder = Ecx.mk_block_builder ~ecx in
    let false_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    let left_val = emit_bool_expression ~pcx ~ecx left in
    Ecx.finish_block_branch ~ecx left_val rhs_builder.id false_builder.id;
    (* Emit right hand side when lhs is true and continue to join block *)
    Ecx.set_block_builder ~ecx rhs_builder;
    let right_val = emit_expression ~pcx ~ecx right in
    let right_var_id = mk_cf_var_id () in
    Ecx.emit ~ecx (Mov (right_var_id, right_val));
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
  | LogicalOr { loc = _; left; right } ->
    (* Short circuit when lhs is true by jumping to true case *)
    let rhs_builder = Ecx.mk_block_builder ~ecx in
    let true_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    let left_val = emit_bool_expression ~pcx ~ecx left in
    Ecx.finish_block_branch ~ecx left_val true_builder.id rhs_builder.id;
    (* Emit right hand side when lhs is false and continue to join block *)
    Ecx.set_block_builder ~ecx rhs_builder;
    let right_val = emit_expression ~pcx ~ecx right in
    let right_var_id = mk_cf_var_id () in
    Ecx.emit ~ecx (Mov (right_var_id, right_val));
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
  | BinaryOperation { loc; op; left; right } ->
    let open BinaryOperation in
    let var_id = mk_cf_var_id () in
    let left_val = emit_numeric_expression ~pcx ~ecx left in
    let right_val = emit_numeric_expression ~pcx ~ecx right in
    let ty = mir_type_of_loc ~pcx ~ecx loc in
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
      | Equal -> (Eq (var_id, left_val, right_val), `BoolT)
      | NotEqual -> (Neq (var_id, left_val, right_val), `BoolT)
      | LessThan -> (Lt (var_id, left_val, right_val), `BoolT)
      | GreaterThan -> (Gt (var_id, left_val, right_val), `BoolT)
      | LessThanOrEqual -> (LtEq (var_id, left_val, right_val), `BoolT)
      | GreaterThanOrEqual -> (GtEq (var_id, left_val, right_val), `BoolT)
    in
    Ecx.emit ~ecx instr;
    var_value_of_type var_id ty
  | Identifier { loc; _ }
  | ScopedIdentifier { name = { Identifier.loc; _ }; _ } ->
    let binding = Bindings.get_source_value_binding pcx.bindings loc in
    let decl_loc = fst binding.declaration in
    (match snd binding.declaration with
    | ImportedModule _ -> failwith "Modules cannot appear in a value position"
    | CtorDecl
    | ImportedCtorDecl _ ->
      failwith "Constructors cannot appear in a value position"
    (* Create function literal for functions *)
    | FunDecl
    | ImportedFunDecl _ ->
      `FunctionL (mk_binding_name binding)
    (* Variables may be either globals or locals *)
    | VarDecl _
    | ImportedVarDecl _
    | FunParam ->
      let var =
        if Bindings.is_global_decl pcx.bindings decl_loc then (
          let binding_name = mk_binding_name binding in
          let global = SMap.find binding_name ecx.globals in
          let var_id = mk_cf_var_id () in
          Ecx.emit ~ecx (Load (var_id, `PointerV (global.ty, global.var)));
          var_id
        ) else
          mk_cf_local loc
      in
      var_value_of_type var (mir_type_of_value_decl_loc ~pcx ~ecx decl_loc))
  | TypeCast { expr; _ } -> emit_expression ~pcx ~ecx expr
  | Call { loc; func; args } ->
    (* Emit tuple constructor *)
    let ctor_result_opt =
      match func with
      | Identifier { Identifier.loc; _ }
      | ScopedIdentifier { ScopedIdentifier.name = { Identifier.loc; _ }; _ } ->
        let binding = Type_context.get_source_value_binding ~cx:pcx.type_ctx loc in
        (match snd binding.declaration with
        | CtorDecl ->
          let adt = Type_context.find_rep_type ~cx:pcx.type_ctx (TVar binding.tvar_id) in
          (* Find MIR aggregate type for this ADT *)
          let agg = Types.TypeHashtbl.find ecx.adt_to_agg_type adt in
          let agg_ty = `AggregateT agg in
          (* Call myte_alloc builtin to allocate space for tuple *)
          let agg_ptr_var_id = mk_cf_var_id () in
          let agg_ptr_var = `PointerV (agg_ty, agg_ptr_var_id) in
          let (agg_ptr_val, myte_alloc_instr) =
            Mir_builtin.(mk_call_builtin myte_alloc agg_ptr_var_id [`LongL Int64.one] agg_ty)
          in
          Ecx.emit ~ecx myte_alloc_instr;
          (* Store each argument to the tuple constructor in space allocated for tuple *)
          let args_and_element_types = List.combine args agg.Aggregate.elements in
          List.iteri
            (fun i (arg, (_, element_ty)) ->
              (* Calculate offset for this element and store *)
              let arg_var = emit_expression ~pcx ~ecx arg in
              let (element_offset_var, get_ptr_instr) =
                mk_get_pointer_instr element_ty agg_ptr_var [GetPointer.FieldIndex i]
              in
              Ecx.emit ~ecx (GetPointer get_ptr_instr);
              Ecx.emit ~ecx (Store (element_offset_var, arg_var)))
            args_and_element_types;
          Some agg_ptr_val
        | _ -> None)
      | _ -> None
    in
    (match ctor_result_opt with
    | Some result -> result
    | None ->
      let var_id = mk_cf_var_id () in
      let func_val = emit_function_expression ~pcx ~ecx func in
      let arg_vals = List.map (emit_expression ~pcx ~ecx) args in
      let ret_ty = mir_type_of_loc ~pcx ~ecx loc in
      (match func_val with
      (* Emit inlined builtins *)
      | `FunctionL name when name = Std_lib.std_array_new ->
        let (`PointerT element_ty) = cast_to_pointer_type ret_ty in
        let (array_ptr_val, myte_alloc_instr) =
          Mir_builtin.(mk_call_builtin myte_alloc var_id arg_vals element_ty)
        in
        Ecx.emit ~ecx myte_alloc_instr;
        array_ptr_val
      | _ ->
        (* Emit function call *)
        Ecx.emit ~ecx (Call (var_id, ret_ty, func_val, arg_vals));
        var_value_of_type var_id ret_ty))
  | Record { Record.loc; name = _; fields } ->
    (* Find MIR aggregate type for this ADT *)
    let adt = type_of_loc ~pcx loc in
    let agg = Types.TypeHashtbl.find ecx.adt_to_agg_type adt in
    let agg_ty = `AggregateT agg in
    (* Call myte_alloc builtin to allocate space for record *)
    let agg_ptr_var_id = mk_cf_var_id () in
    let agg_ptr_var = `PointerV (agg_ty, agg_ptr_var_id) in
    let (agg_ptr_val, myte_alloc_instr) =
      Mir_builtin.(mk_call_builtin myte_alloc agg_ptr_var_id [`LongL Int64.one] agg_ty)
    in
    Ecx.emit ~ecx myte_alloc_instr;
    (* Store each argument to the record constructor in space allocated for record *)
    List.iter
      (fun { Record.Field.name = { name; _ } as name_id; value; _ } ->
        (* Calculate offset for this element and store *)
        let arg_var =
          match value with
          | Some expr -> emit_expression ~pcx ~ecx expr
          | None -> emit_expression ~pcx ~ecx (Identifier name_id)
        in
        let (element_ty, element_idx) = lookup_element agg name in
        let (element_offset_var, get_ptr_instr) =
          mk_get_pointer_instr element_ty agg_ptr_var [GetPointer.FieldIndex element_idx]
        in
        Ecx.emit ~ecx (GetPointer get_ptr_instr);
        Ecx.emit ~ecx (Store (element_offset_var, arg_var)))
      fields;
    agg_ptr_val
  | IndexedAccess _
  | NamedAccess _ ->
    let element_pointer_var = emit_expression_access_chain ~pcx ~ecx expr in
    let var_id = mk_cf_var_id () in
    Ecx.emit ~ecx (Load (var_id, element_pointer_var));
    var_value_of_type var_id (pointer_value_element_type element_pointer_var)
  | Tuple _ -> failwith "TODO: Emit MIR for tuple expressions"
  | Ternary _ -> failwith "TODO: Emit MIR for ternary expressions"
  | Match _ -> failwith "TODO: Emir MIR for match expressions"

and emit_expression_access_chain ~pcx ~ecx expr =
  let open Expression in
  let open Instruction in
  let rec emit_indexed_access_chain ~pcx ~ecx { IndexedAccess.target; index; _ } =
    let target_ty = type_of_loc ~pcx (Ast_utils.expression_loc target) in
    let emit_tuple_indexed_access () =
      (* Emit target expression *)
      let target_var = emit_expression_access_chain_inner ~pcx ~ecx target in
      let target_ptr_var = cast_to_pointer_value target_var in
      (* Extract tuple element index from integer literal *)
      let element_idx =
        match index with
        | IntLiteral { IntLiteral.raw; base; _ } ->
          Integers.int64_of_string_opt raw base |> Option.get |> Int64.to_int
        | _ -> failwith "Index of a tuple must be an int literal to pass type checking"
      in
      (* Find element type in the corresponding aggregate type *)
      let agg = Types.TypeHashtbl.find ecx.Ecx.adt_to_agg_type target_ty in
      let (_, element_ty) = List.nth agg.elements element_idx in
      (* Calculate element offset *)
      let (element_pointer_var, get_ptr_instr) =
        mk_get_pointer_instr element_ty target_ptr_var [GetPointer.FieldIndex element_idx]
      in
      Ecx.emit ~ecx (GetPointer get_ptr_instr);
      element_pointer_var
    in
    let emit_array_indexed_access element_ty =
      (* Emit target and index expressions *)
      let target_var = emit_expression_access_chain_inner ~pcx ~ecx target in
      let target_ptr_var = cast_to_pointer_value target_var in
      let index_var = emit_numeric_expression ~pcx ~ecx index in
      (* Calculate index offset *)
      let (element_pointer_var, get_ptr_instr) =
        let element_mir_ty = type_to_mir_type ~pcx ~ecx element_ty in
        mk_get_pointer_instr ~pointer_offset:(Some index_var) element_mir_ty target_ptr_var []
      in
      Ecx.emit ~ecx (GetPointer get_ptr_instr);
      element_pointer_var
    in
    match target_ty with
    | Array element_ty -> emit_array_indexed_access element_ty
    | Tuple _ -> emit_tuple_indexed_access ()
    | ADT { adt_sig = { variant_sigs; _ }; _ } when SMap.cardinal variant_sigs = 1 ->
      emit_tuple_indexed_access ()
    | _ -> failwith "Target must be a tuple to pass type checking"
  and emit_named_access_chain ~pcx ~ecx { NamedAccess.target; name = { name; _ }; _ } =
    (* Emit target expression *)
    let target_var = emit_expression_access_chain_inner ~pcx ~ecx target in
    let target_ptr_var = cast_to_pointer_value target_var in
    (* Find element type in the corresponding aggregate type *)
    let target_ty = type_of_loc ~pcx (Ast_utils.expression_loc target) in
    let agg = Types.TypeHashtbl.find ecx.Ecx.adt_to_agg_type target_ty in
    let (element_ty, element_idx) = lookup_element agg name in
    (* Calculate element offset *)
    let (element_pointer_var, get_ptr_instr) =
      mk_get_pointer_instr element_ty target_ptr_var [GetPointer.FieldIndex element_idx]
    in
    Ecx.emit ~ecx (GetPointer get_ptr_instr);
    element_pointer_var
  and emit_expression_access_chain_inner ~pcx ~ecx expr =
    match expr with
    (* Access expressions calculate element offset and load value *)
    | IndexedAccess access ->
      let element_pointer_var = emit_indexed_access_chain ~pcx ~ecx access in
      let var_id = mk_cf_var_id () in
      Ecx.emit ~ecx (Load (var_id, element_pointer_var));
      var_value_of_type var_id (pointer_value_element_type element_pointer_var)
    | NamedAccess access ->
      let element_pointer_var = emit_named_access_chain ~pcx ~ecx access in
      let var_id = mk_cf_var_id () in
      Ecx.emit ~ecx (Load (var_id, element_pointer_var));
      var_value_of_type var_id (pointer_value_element_type element_pointer_var)
    | _ -> emit_expression ~pcx ~ecx expr
  in
  match expr with
  | IndexedAccess access -> emit_indexed_access_chain ~pcx ~ecx access
  | NamedAccess access -> emit_named_access_chain ~pcx ~ecx access
  | _ -> failwith "Must be called on access expression"

and emit_bool_expression ~pcx ~ecx expr =
  match emit_expression ~pcx ~ecx expr with
  | (`BoolL _ | `BoolV _) as v -> v
  | _ -> failwith "Expected bool value"

and emit_numeric_expression ~pcx ~ecx expr =
  match emit_expression ~pcx ~ecx expr with
  | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _ | `LongV _) as v -> v
  | _ -> failwith "Expected numeric value"

and emit_function_expression ~pcx ~ecx expr =
  match emit_expression ~pcx ~ecx expr with
  | (`FunctionL _ | `FunctionV _) as v -> v
  | _ -> failwith "Expected function value"

and cast_to_pointer_value v =
  match v with
  | (`PointerL _ | `PointerV _) as v -> v
  | _ -> failwith "Expected pointer value"

and cast_to_pointer_type v =
  match v with
  | `PointerT _ as v -> v
  | _ -> failwith "Expected pointer type"

and emit_statement ~pcx ~ecx stmt =
  let open Statement in
  match stmt with
  | Expression (_, expr) -> ignore (emit_expression ~pcx ~ecx expr)
  | Block { statements; _ } -> List.iter (emit_statement ~pcx ~ecx) statements
  | If { loc = _; test; conseq; altern = None } ->
    (* Branch to conseq or join blocks *)
    let test_val = emit_bool_expression ~pcx ~ecx test in
    let conseq_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_branch ~ecx test_val conseq_builder.id join_builder.id;
    (* Emit conseq and continue to join block *)
    Ecx.set_block_builder ~ecx conseq_builder;
    emit_statement ~pcx ~ecx conseq;
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Start join block *)
    Ecx.set_block_builder ~ecx join_builder
  | If { loc = _; test; conseq; altern = Some altern } ->
    (* Branch to conseq or altern blocks *)
    let test_val = emit_bool_expression ~pcx ~ecx test in
    let conseq_builder = Ecx.mk_block_builder ~ecx in
    let altern_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_branch ~ecx test_val conseq_builder.id altern_builder.id;
    (* Emit conseq and continue to join block *)
    Ecx.set_block_builder ~ecx conseq_builder;
    emit_statement ~pcx ~ecx conseq;
    Ecx.finish_block_continue ~ecx join_builder.id;
    (* Emit altern and continue to join block *)
    Ecx.set_block_builder ~ecx altern_builder;
    emit_statement ~pcx ~ecx altern;
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
    let test_val = emit_bool_expression ~pcx ~ecx test in
    Ecx.finish_block_branch ~ecx test_val body_builder.id finish_builder.id;
    (* Emit body block which continues to test block *)
    Ecx.push_loop_context ~ecx finish_builder.id test_builder.id;
    Ecx.set_block_builder ~ecx body_builder;
    emit_statement ~pcx ~ecx body;
    Ecx.finish_block_continue ~ecx test_builder.id;
    Ecx.pop_loop_context ~ecx;
    (* Start join block *)
    Ecx.set_block_builder ~ecx finish_builder
  | Return { loc = _; arg } ->
    let arg_val = Option.map (emit_expression ~pcx ~ecx) arg in
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
      let binding = Bindings.get_source_value_binding pcx.bindings use_loc in
      let decl_loc = fst binding.declaration in
      let expr_val = emit_expression ~pcx ~ecx expr in
      if Bindings.is_global_decl pcx.bindings decl_loc then
        let binding_name = mk_binding_name binding in
        let global = SMap.find binding_name ecx.globals in
        Ecx.emit ~ecx (Store (`PointerV (global.ty, global.var), expr_val))
      else
        Ecx.emit ~ecx (Mov (mk_cf_local use_loc, expr_val))
    | Assignment.Expression expr_lvalue ->
      let element_pointer_var = emit_expression_access_chain ~pcx ~ecx expr_lvalue in
      let expr_val = emit_expression ~pcx ~ecx expr in
      Ecx.emit ~ecx (Store (element_pointer_var, expr_val)))
  | VariableDeclaration { pattern; init; _ } ->
    (* TODO: Emit MIR for arbitrary patterns *)
    let { Identifier.loc; _ } = List.hd (Ast_utils.ids_of_pattern pattern) in
    let init_val = emit_expression ~pcx ~ecx init in
    Ecx.emit ~ecx (Mov (mk_cf_local loc, init_val))
  | Match _ -> failwith "TODO: Emit MIR for match statements"
  | FunctionDeclaration _ -> failwith "TODO: Emit MIR for non-toplevel function declarations"

and mk_cf_var_id () = Id (mk_var_id ())

and mk_cf_local loc = Local loc

and mk_binding_name binding = String.concat "." (binding.module_ @ [binding.name])

and mk_get_pointer_instr ?(pointer_offset = None) element_ty pointer offsets =
  let var_id = mk_cf_var_id () in
  let var = `PointerV (element_ty, var_id) in
  (var, { Instruction.GetPointer.var_id; return_ty = element_ty; pointer; pointer_offset; offsets })

and type_to_mir_type ~pcx ~ecx ty =
  let ty = Type_context.find_rep_type ~cx:pcx.type_ctx ty in
  match ty with
  | Types.Unit -> `UnitT
  | Types.Bool -> `BoolT
  | Types.Byte -> `ByteT
  | Types.Int -> `IntT
  | Types.Long -> `LongT
  | Types.IntLiteral { resolved; _ } -> type_to_mir_type ~pcx ~ecx (Option.get resolved)
  | Types.String -> `StringT
  | Types.Array element_ty -> `PointerT (type_to_mir_type ~pcx ~ecx element_ty)
  | Types.Tuple _ -> failwith "TODO: Implement MIR emission for tuple types"
  | Types.Function _ -> `FunctionT
  | Types.TParam _ -> failwith "TParams must be resolved for all values in IR"
  | Types.TVar _ -> failwith "TVars must be resolved for all values in IR"
  | Types.Any -> failwith "Any not allowed as value in IR"
  | Types.Alias _ -> failwith "Aliase types should be resolved during type checking"
  | Types.ADT _ ->
    (match Types.TypeHashtbl.find_opt ecx.adt_to_agg_type ty with
    (* All aggregates are currently allocated behind a pointer *)
    | Some mir_type -> `PointerT (`AggregateT mir_type)
    | None -> failwith "TODO: Generate aggregate types for all ADTs")

and type_of_loc ~pcx loc =
  let tvar_id = Type_context.get_tvar_from_loc ~cx:pcx.type_ctx loc in
  Type_context.find_rep_type ~cx:pcx.type_ctx (TVar tvar_id)

and mir_type_of_loc ~pcx ~ecx loc =
  let tvar_id = Type_context.get_tvar_from_loc ~cx:pcx.type_ctx loc in
  type_to_mir_type ~pcx ~ecx (Types.TVar tvar_id)

and mir_type_of_value_decl_loc ~pcx ~ecx loc =
  let tvar_id = Bindings.get_tvar_id_from_value_decl pcx.bindings loc in
  type_to_mir_type ~pcx ~ecx (Types.TVar tvar_id)
