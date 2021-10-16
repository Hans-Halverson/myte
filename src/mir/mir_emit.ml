open Ast
open Basic_collections
open Mir
open Mir_emit_utils
open Mir_type
module Ecx = Mir_emit_context
module Pcx = Program_context

type access_chain_part =
  | AccessChainOffset of Instruction.GetPointer.offset * Type.t
  | AccessChainDereference

type access_chain_result =
  | GetPointerEmittedResult of Value.pointer_value
  | InlinedValueResult of Value.t

let rec emit (pcx : Pcx.t) : Program.t =
  let ecx = Ecx.mk ~pcx in
  start_init_function ~ecx;
  register_type_declarations ~ecx;
  register_toplevel_variable_declarations ~ecx;
  register_function_declarations ~ecx;
  emit_pending ~ecx;
  finish_init_function ~ecx;
  {
    Program.main_label = ecx.main_label;
    blocks = Ecx.builders_to_blocks ecx.blocks;
    globals = ecx.globals;
    funcs = ecx.funcs;
    types = ecx.types;
  }

and iter_toplevels ~ecx f =
  List.iter (fun (_, m) -> List.iter f m.Ast.Module.toplevels) ecx.Ecx.pcx.modules

and register_type_declarations ~ecx =
  let open Ast.Module in
  (fun toplevel ->
    match toplevel with
    | TypeDeclaration decl ->
      (* Create and store MIR ADT and variant objects to be used during MIR emission *)
      let open TypeDeclaration in
      (match decl.decl with
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
        ())
    | _ -> ())
  |> iter_toplevels ~ecx

and register_toplevel_variable_declarations ~ecx =
  let open Ast.Module in
  (fun toplevel ->
    match toplevel with
    | VariableDeclaration ({ pattern = Identifier { loc; _ }; _ } as decl) ->
      Ecx.add_global_variable_declaration_node ~ecx loc decl
    | _ -> ())
  |> iter_toplevels ~ecx

and register_function_declarations ~ecx =
  let open Ast.Module in
  let register_function_declaration decl =
    let open Ast.Function in
    let { name = { Identifier.loc; _ }; _ } = decl in
    let binding = Bindings.get_value_binding ecx.pcx.bindings loc in
    let name = mk_value_binding_name binding in
    Ecx.add_function_declaration_node ~ecx name decl;
    if loc = Option.get ecx.pcx.main_loc then ecx.main_label <- name
  in
  (fun toplevel ->
    match toplevel with
    | FunctionDeclaration decl -> register_function_declaration decl
    | TraitDeclaration { methods; _ } -> List.iter register_function_declaration methods
    | _ -> ())
  |> iter_toplevels ~ecx

and emit_pending ~ecx =
  let complete = ref true in

  (* Add root set of functions to emit *)
  ignore (Ecx.get_nongeneric_function_value ~ecx ecx.main_label);
  ignore (Ecx.get_nongeneric_function_value ~ecx Std_lib.std_sys_init);

  (* Emit all pending global variables *)
  let rec emit_pending_globals () =
    match Ecx.pop_pending_global ~ecx with
    | None -> ()
    | Some (name, global_decl) ->
      emit_global_variable_declaration ~ecx name global_decl;
      complete := false;
      emit_pending_globals ()
  in

  (* Emit all pending nongeneric functions *)
  let rec emit_pending_nongeneric_functions () =
    match Ecx.pop_pending_nongeneric_function ~ecx with
    | None -> ()
    | Some (name, func_decl) ->
      emit_function_body ~ecx name func_decl;
      Ecx.mark_pending_nongeneric_function_completed ~ecx name;
      complete := false;
      emit_pending_nongeneric_functions ()
  in

  (* Emit all pending instantiations of generic functions *)
  let rec emit_pending_func_instantations () =
    match Ecx.pop_pending_func_instantiation ~ecx with
    | None -> ()
    | Some func_instantiation ->
      emit_function_instantiation ~ecx func_instantiation;
      complete := false;
      emit_pending_func_instantations ()
  in

  let rec emit_all_pending () =
    complete := true;
    emit_pending_globals ();
    emit_pending_nongeneric_functions ();
    emit_pending_func_instantations ();
    if not !complete then emit_all_pending ()
  in
  emit_all_pending ()

and emit_global_variable_declaration ~ecx name decl =
  let { Statement.VariableDeclaration.init; _ } = decl in
  (* Build IR for variable init *)
  let global = SMap.find name ecx.globals in
  let binding = Bindings.get_value_binding ecx.pcx.bindings global.loc in
  let init_val =
    Ecx.emit_init_section ~ecx (fun _ ->
        ecx.current_in_std_lib <- Bindings.is_std_lib_value binding;
        (* Do not generate init blocks for stdlib globals when filtering stdlib *)
        if ecx.current_in_std_lib && ecx.filter_std_lib then
          None
        else
          (* If initial value is statically known at compile time, add it as constant initialization *)
          let init_val = emit_expression ~ecx init in
          if is_literal init_val then
            Some init_val
          else (
            (* Otherwise value must be calculated and stored at runtime *)
            Ecx.emit ~ecx (Store (`PointerL (global.ty, name), init_val));
            None
          ))
  in
  global.init_val <- init_val

and emit_function_instantiation ~ecx (name, name_with_args, type_param_bindings) =
  Ecx.in_type_binding_context ~ecx type_param_bindings (fun _ ->
      let func_decl_node = SMap.find name ecx.func_decl_nodes in
      if not func_decl_node.builtin then emit_function_body ~ecx name_with_args func_decl_node)

and emit_function_body ~ecx name decl =
  let open Ast.Function in
  let { loc = full_loc; name = { Identifier.loc; _ }; params; body; _ } = decl in
  let binding = Type_context.get_value_binding ~cx:ecx.pcx.type_ctx loc in
  ecx.current_in_std_lib <- Bindings.is_std_lib_value binding;
  let current_is_main = ecx.main_label = name in

  (* Create MIR vars for function params *)
  let func_decl = Bindings.get_func_decl binding in
  let params =
    List.map2
      (fun { Param.name = { Identifier.loc; _ }; _ } ty ->
        (loc, Ecx.add_function_param ~ecx loc, type_to_mir_type ~ecx ty))
      params
      func_decl.params
  in

  (* Add implicit this param *)
  let params =
    match LocMap.find_opt full_loc ecx.pcx.bindings.value_bindings with
    | Some { declaration = FunParamDecl { tvar }; _ } ->
      let this_type = type_to_mir_type ~ecx (Types.Type.TVar tvar) in
      (full_loc, Ecx.add_function_param ~ecx full_loc, this_type) :: params
    | _ -> params
  in

  (* Build IR for function body *)
  Ecx.set_current_func ~ecx name;
  let body_start_block = Ecx.start_new_block ~ecx in
  (match body with
  | Block { Statement.Block.statements; _ } ->
    List.iter (emit_statement ~ecx) statements;
    (* Add an implicit return if the last instruction is not a return *)
    (match ecx.current_block_builder with
    | Some { Ecx.BlockBuilder.instructions = (_, Ret _) :: _; _ } -> ()
    | _ ->
      (* Handle implicit return from main *)
      let return_val =
        if current_is_main then
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

  (* The main function must always return an Int *)
  let return_ty = type_to_mir_type ~ecx func_decl.return in
  let return_ty =
    if current_is_main && return_ty = `UnitT then
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
  (* Check for trait object promotion on every expression *)
  let expr_val = emit_expression_without_promotion ~ecx expr in
  let expr_loc = Ast_utils.expression_loc expr in
  match Type_context.get_trait_object_promotion ~cx:ecx.Ecx.pcx.type_ctx expr_loc with
  | None -> expr_val
  | Some trait_instance ->
    let ty = type_of_loc ~ecx expr_loc in
    let trait_object_instance = Ecx.instantiate_trait_object_vtable ~ecx trait_instance ty in
    emit_trait_object_promotion ~ecx expr_val trait_object_instance

and emit_expression_without_promotion ~ecx expr =
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
  | CharLiteral { loc; value } ->
    let value = int_of_char value in
    let ty = mir_type_of_loc ~ecx loc in
    (match ty with
    | `ByteT -> `ByteL value
    | `IntT -> `IntL (Int32.of_int value)
    | `LongT -> `LongL (Int64.of_int value)
    | _ -> failwith "Char literal must have integer type")
  | StringLiteral { loc; value; _ } -> emit_string_literal ~ecx loc value
  (*
   * ============================
   *       Unary Operations
   * ============================
   *)
  | UnaryOperation { op = Plus; operand; _ } -> emit_expression ~ecx operand
  | UnaryOperation { loc; op = Minus; operand } ->
    let var_id = mk_var_id () in
    let operand_val = emit_numeric_expression ~ecx operand in
    let ty = mir_type_of_loc ~ecx loc in
    Ecx.emit ~ecx (Neg (var_id, operand_val));
    var_value_of_type var_id ty
  | UnaryOperation { op = Not; loc; operand } ->
    let var_id = mk_var_id () in
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
    (* Model logical and join by creating stack location to place results of branches *)
    let result_var_id = mk_var_id () in
    let result_ptr_val = `PointerV (`BoolT, result_var_id) in
    Ecx.emit ~ecx (StackAlloc (result_var_id, `BoolT));

    (* Short circuit when lhs is false by jumping to false case *)
    let rhs_builder = Ecx.mk_block_builder ~ecx in
    let false_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    let left_val = emit_bool_expression ~ecx left in
    Ecx.finish_block_branch ~ecx left_val rhs_builder.id false_builder.id;

    (* Store right hand side when lhs is true and continue to join block *)
    Ecx.set_block_builder ~ecx rhs_builder;
    let right_val = emit_expression ~ecx right in
    Ecx.emit ~ecx (Store (result_ptr_val, right_val));
    Ecx.finish_block_continue ~ecx join_builder.id;

    (* Store false literal when lhs is false and continue to join block *)
    Ecx.set_block_builder ~ecx false_builder;
    Ecx.emit ~ecx (Store (result_ptr_val, `BoolL false));
    Ecx.finish_block_continue ~ecx join_builder.id;

    (* Join cases together and load result from stack location *)
    Ecx.set_block_builder ~ecx join_builder;
    let var_id = mk_var_id () in
    Ecx.emit ~ecx (Load (var_id, result_ptr_val));
    `BoolV var_id
  (*
   * ============================
   *         Logical Or
   * ============================
   *)
  | LogicalOr { loc = _; left; right } ->
    (* Model logical or join by creating stack location to place results of branches *)
    let result_var_id = mk_var_id () in
    let result_ptr_val = `PointerV (`BoolT, result_var_id) in
    Ecx.emit ~ecx (StackAlloc (result_var_id, `BoolT));

    (* Short circuit when lhs is true by jumping to true case *)
    let rhs_builder = Ecx.mk_block_builder ~ecx in
    let true_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    let left_val = emit_bool_expression ~ecx left in
    Ecx.finish_block_branch ~ecx left_val true_builder.id rhs_builder.id;

    (* Store right hand side when lhs is false and continue to join block *)
    Ecx.set_block_builder ~ecx rhs_builder;
    let right_val = emit_expression ~ecx right in
    Ecx.emit ~ecx (Store (result_ptr_val, right_val));
    Ecx.finish_block_continue ~ecx join_builder.id;

    (* Store true literal when lhs is true and continue to join block *)
    Ecx.set_block_builder ~ecx true_builder;
    Ecx.emit ~ecx (Store (result_ptr_val, `BoolL true));
    Ecx.finish_block_continue ~ecx join_builder.id;

    (* Join cases together and load result from stack location *)
    Ecx.set_block_builder ~ecx join_builder;
    let var_id = mk_var_id () in
    Ecx.emit ~ecx (Load (var_id, result_ptr_val));
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
      let var_id = mk_var_id () in
      Ecx.emit ~ecx (LogNot (var_id, cast_to_bool_value equals_result_val));
      var_value_of_type var_id `BoolT
  (*
   * ============================
   *       Binary Operations
   * ============================
   *)
  | BinaryOperation { loc; op; left; right } ->
    let open BinaryOperation in
    let var_id = mk_var_id () in
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
    (match binding.declaration with
    | CtorDecl { adt_sig; _ } ->
      let mir_adt_layout = Ecx.get_mir_adt_layout ~ecx adt_sig in
      (match mir_adt_layout.layout with
      (* Enum constructor is a pure integer, and tag is its direct value *)
      | PureEnum { tags; _ } -> (SMap.find name tags :> Value.t)
      (* Enum constructor is part of a variant type, so allocate union aggregate and set tag *)
      | Variants { tags; _ } ->
        let adt = type_of_loc ~ecx loc in
        let (type_args, _) = Type_util.cast_to_adt_type adt in
        let instance = Ecx.instantiate_mir_adt_variants_layout ~ecx mir_adt_layout type_args in
        let union_ty = `AggregateT instance.union in

        (* Call myte_alloc builtin to allocate space for variant's union aggregate *)
        let ptr_var_id = mk_var_id () in
        let (union_ptr_val, myte_alloc_instr) =
          Mir_builtin.(mk_call_builtin myte_alloc ptr_var_id [`IntL Int32.one] [union_ty])
        in
        Ecx.emit ~ecx myte_alloc_instr;

        (* Store enum tag *)
        let tag = SMap.find name tags in
        emit_store_tag ~ecx tag ptr_var_id;
        union_ptr_val
      | Aggregate _
      | InlineValue _ ->
        failwith "Invalid layout for enum")
    (* Create function literal for functions *)
    | FunDecl { Bindings.FunctionDeclaration.type_params; is_builtin; _ } ->
      let func_name = mk_value_binding_name binding in
      let ty = type_of_loc ~ecx loc in
      let func_val =
        if is_builtin then
          `FunctionL func_name
        else if type_params = [] then
          Ecx.get_nongeneric_function_value ~ecx func_name
        else
          let (type_args, _, _) = Type_util.cast_to_function_type ty in
          Ecx.get_generic_function_value ~ecx func_name type_params type_args
      in
      (func_val :> Value.t)
    (* Variables may be either globals or locals *)
    | VarDecl { tvar; _ } ->
      let mir_ty = type_to_mir_type ~ecx (Types.Type.TVar tvar) in
      let var_id = mk_var_id () in
      ( if Bindings.is_module_decl binding then
        let global_ptr = Ecx.get_global_pointer ~ecx binding in
        Ecx.emit ~ecx (Load (var_id, global_ptr))
      else
        let ptr_var_id = Ecx.get_ptr_var_id ~ecx id_loc in
        Ecx.emit ~ecx (Load (var_id, `PointerV (mir_ty, ptr_var_id))) );
      var_value_of_type var_id mir_ty
    (* Function parameters can have their corresponding MIR var referenced directly *)
    | FunParamDecl { tvar } ->
      let var_id = Ecx.get_function_param_var_id ~ecx binding.loc in
      let mir_ty = type_to_mir_type ~ecx (Types.Type.TVar tvar) in
      var_value_of_type var_id mir_ty
    (* Match cases variables are locals, and must be loaded from their var ptr *)
    | MatchCaseVarDecl { tvar } ->
      let mir_ty = type_to_mir_type ~ecx (Types.Type.TVar tvar) in
      let var_id = mk_var_id () in
      let ptr_var_id = Ecx.get_ptr_var_id ~ecx id_loc in
      Ecx.emit ~ecx (Load (var_id, `PointerV (mir_ty, ptr_var_id)));
      var_value_of_type var_id mir_ty)
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
    emit_construct_tuple ~ecx ~tag:None agg elements
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
      | Identifier { Identifier.loc = ctor_id_loc; name }
      | ScopedIdentifier { ScopedIdentifier.name = { Identifier.loc = ctor_id_loc; name }; _ } ->
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
            let agg_ptr_val = emit_construct_tuple ~ecx ~tag:None agg args in
            Some agg_ptr_val
          (* If layout is a variant, construct tuple and set variant tag *)
          | Variants { tags; _ } ->
            let instance = Ecx.instantiate_mir_adt_variants_layout ~ecx mir_adt_layout type_args in
            let tuple_variant_agg = SMap.find name instance.variants in
            let tag = SMap.find name tags in
            let agg_ptr_val = emit_construct_tuple ~ecx ~tag:(Some tag) tuple_variant_agg args in
            Some agg_ptr_val
          (* If layout is an inlined value there must have been a single arg, which we use directly
             without actually constructing a tuple. *)
          | InlineValue _ -> Some (emit_expression ~ecx (List.hd args))
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
  | Record { Record.loc; name; fields; _ } ->
    let name =
      match name with
      | Identifier { name; _ }
      | ScopedIdentifier { name = { name; _ }; _ } ->
        name
      | _ -> failwith "Record name must be identifier"
    in
    (* Find MIR ADT layout for this constructor *)
    let adt = type_of_loc ~ecx loc in
    let (type_args, adt_sig) = Type_util.cast_to_adt_type adt in
    let mir_adt_layout = Ecx.get_mir_adt_layout ~ecx adt_sig in
    (match mir_adt_layout.layout with
    (* If layout is an aggregate, construct record *)
    | Aggregate _ ->
      let agg = Ecx.instantiate_mir_adt_aggregate_layout ~ecx mir_adt_layout type_args in
      emit_construct_record ~ecx ~tag:None agg fields
    (* If layout is a variant, construct record and set variant tag *)
    | Variants { tags; _ } ->
      let instance = Ecx.instantiate_mir_adt_variants_layout ~ecx mir_adt_layout type_args in
      let record_variant_agg = SMap.find name instance.variants in
      let tag = SMap.find name tags in
      emit_construct_record ~ecx ~tag:(Some tag) record_variant_agg fields
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
    (* If indexing a map, call Map's `get` method instead of emitting access chain *)
    | ADT { adt_sig; type_args } when adt_sig == !Std_lib.map_adt_sig ->
      let target_var = emit_expression ~ecx target in
      let index_var = emit_expression ~ecx index in
      emit_call_map_get ~ecx loc type_args target_var index_var
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
    let mir_type = mir_type_of_loc ~ecx loc in

    (* Model ternary join by creating stack location to place results of branches *)
    let result_var_id = mk_var_id () in
    let result_ptr_val = `PointerV (mir_type, result_var_id) in
    Ecx.emit ~ecx (StackAlloc (result_var_id, mir_type));

    (* Branch to conseq or altern blocks *)
    let test_val = emit_bool_expression ~ecx test in
    let conseq_builder = Ecx.mk_block_builder ~ecx in
    let altern_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_branch ~ecx test_val conseq_builder.id altern_builder.id;

    (* Emit conseq, store result, and continue to join block *)
    Ecx.set_block_builder ~ecx conseq_builder;
    let conseq_val = emit_expression ~ecx conseq in
    Ecx.emit ~ecx (Store (result_ptr_val, conseq_val));
    Ecx.finish_block_continue ~ecx join_builder.id;

    (* Emit altern, store result, and continue to join block *)
    Ecx.set_block_builder ~ecx altern_builder;
    let altern_val = emit_expression ~ecx altern in
    Ecx.emit ~ecx (Store (result_ptr_val, altern_val));
    Ecx.finish_block_continue ~ecx join_builder.id;

    (* Join branches together and load result from stack location *)
    Ecx.set_block_builder ~ecx join_builder;
    let var_id = mk_var_id () in
    Ecx.emit ~ecx (Load (var_id, result_ptr_val));
    var_value_of_type var_id mir_type
  (*
   * ============================
   *       Match Expression
   * ============================
   *)
  | Match { loc; args; cases } ->
    let mir_type = mir_type_of_loc ~ecx loc in

    (* Model join of match expression by creatign stack location to place results of cases *)
    let result_var_id = mk_var_id () in
    let result_ptr_val = `PointerV (mir_type, result_var_id) in
    Ecx.emit ~ecx (StackAlloc (result_var_id, mir_type));

    (* Emit args and decision tree *)
    let args = List.map (emit_expression ~ecx) args in
    let decision_tree = Mir_match_decision_tree.build_match_decision_tree ~ecx args cases in
    let join_block = Ecx.mk_block_builder ~ecx in
    emit_match_decision_tree
      ~ecx
      ~join_block
      ~result_ptr:(Some result_ptr_val)
      ~alloc:true
      decision_tree;

    (* Join branches together and load result from stack location *)
    Ecx.set_block_builder ~ecx join_block;
    let var_id = mk_var_id () in
    Ecx.emit ~ecx (Load (var_id, result_ptr_val));
    var_value_of_type var_id mir_type
  (*
   * ============================
   *       Vec Literal
   * ============================
   *)
  | VecLiteral { loc; elements } ->
    (* Find MIR element type *)
    let (vec_type_args, _) = Type_util.cast_to_adt_type (type_of_loc ~ecx loc) in
    let element_ty = List.hd vec_type_args in
    let element_mir_type = type_to_mir_type ~ecx element_ty in

    (* Do not allocate if vec is empty *)
    let size_val = `IntL (Int32.of_int (List.length elements)) in
    let data_val =
      if elements = [] then
        `LongL 0L
      else
        (* Call myte_alloc builtin to allocate space for elements *)
        let data_ptr_var_id = mk_var_id () in
        let (data_val, myte_alloc_instr) =
          Mir_builtin.(mk_call_builtin myte_alloc data_ptr_var_id [size_val] [element_mir_type])
        in
        Ecx.emit ~ecx myte_alloc_instr;

        (* Generate each element and store into array of elements *)
        let data_ptr_val = cast_to_pointer_value data_val in
        List.iteri
          (fun i element ->
            let element_var = emit_expression ~ecx element in
            let (element_offset_var, get_ptr_instr) =
              mk_get_pointer_instr
                element_mir_type
                data_ptr_val
                [Instruction.GetPointer.PointerIndex (`IntL (Int32.of_int i))]
            in
            Ecx.emit ~ecx (GetPointer get_ptr_instr);
            Ecx.emit ~ecx (Store (element_offset_var, element_var)))
          elements;
        data_val
    in

    (* Call myte_alloc builtin to allocate space for vec *)
    let vec_ptr_var_id = mk_var_id () in
    let vec_ptr_mir_type = mir_type_of_loc ~ecx loc in
    let (`PointerT vec_mir_type) = cast_to_pointer_type vec_ptr_mir_type in
    let vec_ptr_var = `PointerV (vec_mir_type, vec_ptr_var_id) in
    let (vec_ptr_val, myte_alloc_instr) =
      Mir_builtin.(mk_call_builtin myte_alloc vec_ptr_var_id [`IntL Int32.one] [vec_mir_type])
    in
    Ecx.emit ~ecx myte_alloc_instr;

    (* Write all vec literal fields *)
    let (`AggregateT vec_agg) = cast_to_aggregate_type vec_mir_type in
    let emit_field_store name value =
      let (element_ty, element_idx) = lookup_element vec_agg name in
      let (element_offset_var, get_ptr_instr) =
        mk_get_pointer_instr element_ty vec_ptr_var [Instruction.GetPointer.FieldIndex element_idx]
      in
      Ecx.emit ~ecx (GetPointer get_ptr_instr);
      Ecx.emit ~ecx (Store (element_offset_var, value))
    in
    emit_field_store "data" data_val;
    emit_field_store "size" size_val;
    emit_field_store "capacity" size_val;
    vec_ptr_val
  (*
   * ============================
   *         Map Literal
   * ============================
   *)
  | MapLiteral { loc; entries } ->
    (* Create new map by calling `Map.new` *)
    let map_ty = type_of_loc ~ecx loc in
    let map_mir_type = type_to_mir_type ~ecx map_ty in
    let (map_type_args, _) = Type_util.cast_to_adt_type map_ty in
    let map_ptr_val = emit_call_map_new ~ecx map_mir_type map_type_args in

    (* Reserve capacity for all entries in map literal by calling map's `reserve` method *)
    ( if List.length entries > 1 then
      let capacity_val = `IntL (Int32.of_int (List.length entries)) in
      emit_call_map_reserve ~ecx map_type_args map_ptr_val capacity_val );

    (* Add all entries to map by calling map's `add` method *)
    List.iter
      (fun { MapLiteral.Entry.key; value; _ } ->
        let key_val = emit_expression ~ecx key in
        let value_val = emit_expression ~ecx value in
        emit_call_map_add ~ecx map_type_args map_ptr_val key_val value_val)
      entries;
    map_ptr_val
  (*
   * ============================
   *         Set Literal
   * ============================
   *)
  | SetLiteral { loc; elements } ->
    (* Create new set by calling `Set.new` *)
    let set_ty = type_of_loc ~ecx loc in
    let set_mir_type = type_to_mir_type ~ecx set_ty in
    let (set_type_args, _) = Type_util.cast_to_adt_type set_ty in
    let set_ptr_val = emit_call_set_new ~ecx set_mir_type set_type_args in

    (* Reserve capacity for all entries in set literal by calling set's `reserve` method *)
    ( if List.length elements > 1 then
      let capacity_val = `IntL (Int32.of_int (List.length elements)) in
      emit_call_set_reserve ~ecx set_type_args set_ptr_val capacity_val );

    (* Add all entries to set by calling set's `add` method *)
    List.iter
      (fun element ->
        let element_val = emit_expression ~ecx element in
        emit_call_set_add ~ecx set_type_args set_ptr_val element_val)
      elements;
    set_ptr_val
  | Super _ -> failwith "TODO: Emit MIR for super expressions"
  | AnonymousFunction _ -> failwith "TODO: Emit anonymous functions"
  | If _ -> failwith "TODO: Emit if expression"

and emit_string_literal ~ecx loc value =
  let string_global_ptr = Ecx.add_string_literal ~ecx loc value in
  let string_pointer_type = mir_type_of_loc ~ecx loc in
  let (`PointerT string_type) = cast_to_pointer_type string_pointer_type in
  let (`AggregateT string_agg) = cast_to_aggregate_type string_type in
  (* Call myte_alloc builtin to allocate space for string *)
  let agg_ptr_var_id = mk_var_id () in
  let agg_ptr_var = `PointerV (string_type, agg_ptr_var_id) in
  let (agg_ptr_val, myte_alloc_instr) =
    Mir_builtin.(mk_call_builtin myte_alloc agg_ptr_var_id [`IntL Int32.one] [string_type])
  in
  Ecx.emit ~ecx myte_alloc_instr;
  (* Write all string literal fields *)
  let emit_field_store name value =
    let (element_ty, element_idx) = lookup_element string_agg name in
    let (element_offset_var, get_ptr_instr) =
      mk_get_pointer_instr element_ty agg_ptr_var [Instruction.GetPointer.FieldIndex element_idx]
    in
    Ecx.emit ~ecx (GetPointer get_ptr_instr);
    Ecx.emit ~ecx (Store (element_offset_var, value))
  in
  let length = `IntL (Int32.of_int (String.length value)) in
  emit_field_store "data" string_global_ptr;
  emit_field_store "size" length;
  emit_field_store "capacity" length;
  agg_ptr_val

and emit_trait_object_promotion ~ecx expr_val trait_object_instance =
  (* Call myte_alloc builtin to allocate space for trait object *)
  let trait_object_type = `AggregateT trait_object_instance.agg in
  let trait_object_ptr_var_id = mk_var_id () in
  let trait_object_ptr_var = `PointerV (trait_object_type, trait_object_ptr_var_id) in
  let (trait_object_ptr_val, myte_alloc_instr) =
    Mir_builtin.(
      mk_call_builtin myte_alloc trait_object_ptr_var_id [`IntL Int32.one] [trait_object_type])
  in
  Ecx.emit ~ecx myte_alloc_instr;
  (* Write trait object fields *)
  let emit_field_store name value =
    let (element_ty, element_idx) = lookup_element trait_object_instance.agg name in
    let (element_offset_var, get_ptr_instr) =
      mk_get_pointer_instr
        element_ty
        trait_object_ptr_var
        [Instruction.GetPointer.FieldIndex element_idx]
    in
    Ecx.emit ~ecx (GetPointer get_ptr_instr);
    Ecx.emit ~ecx (Store (element_offset_var, value))
  in
  emit_field_store "item" expr_val;
  emit_field_store "vtable" (trait_object_instance.vtable :> Value.t);
  trait_object_ptr_val

and emit_expression_access_chain_load ~ecx expr =
  match emit_expression_access_chain ~ecx expr with
  | GetPointerEmittedResult element_pointer_val ->
    let var_id = mk_var_id () in
    Ecx.emit ~ecx (Load (var_id, element_pointer_val));
    var_value_of_type var_id (pointer_value_element_type element_pointer_val)
  | InlinedValueResult value -> value

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
    let var_id = mk_var_id () in
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
    ~(receiver_val : Value.t)
    ~(receiver_ty : Types.Type.t)
    ~(arg_vals : Value.t list)
    ~(method_instance_type_args : Types.Type.t list)
    ~(ret_type : Type.t) =
  match receiver_ty with
  (* Trait objects perform dynamic dispatch *)
  | TraitObject { trait_sig; _ } ->
    let trait_object_layout = Ecx.get_trait_object_layout ~ecx trait_sig in
    let vtable_index = SMap.find method_name trait_object_layout.vtable_indices in
    let receiver_ptr_val = cast_to_pointer_value receiver_val in
    let receiver_ptr_val =
      cast_pointer_value receiver_ptr_val (`AggregateT trait_object_layout.trait_object_agg)
    in

    (* Load item and vtable from trait object *)
    let emit_field_load name =
      let element_var_id = mk_var_id () in
      let (element_ty, element_idx) = lookup_element trait_object_layout.trait_object_agg name in
      let (element_offset_val, get_ptr_instr) =
        mk_get_pointer_instr
          element_ty
          receiver_ptr_val
          [Instruction.GetPointer.FieldIndex element_idx]
      in
      Ecx.emit ~ecx (GetPointer get_ptr_instr);
      Ecx.emit ~ecx (Load (element_var_id, element_offset_val));
      var_value_of_type element_var_id element_ty
    in
    let item_val = emit_field_load "item" in
    let vtable_val = emit_field_load "vtable" |> cast_to_pointer_value in
    let vtable_val = cast_pointer_value vtable_val `FunctionT in

    (* Load function from vtable *)
    let func_var_id = mk_var_id () in
    let (func_offset_val, get_ptr_instr) =
      mk_get_pointer_instr
        `FunctionT
        vtable_val
        [Instruction.GetPointer.PointerIndex (`IntL (Int32.of_int vtable_index))]
    in
    Ecx.emit ~ecx (GetPointer get_ptr_instr);
    Ecx.emit ~ecx (Load (func_var_id, func_offset_val));
    let func_val = `FunctionV func_var_id in
    emit_call ~ecx ~func_val ~arg_vals ~receiver_val:(Some item_val) ~ret_type
  (* All other receivers perform static dispatch *)
  | _ ->
    let func_val =
      Ecx.get_method_function_value ~ecx ~method_name ~receiver_ty ~method_instance_type_args
    in
    emit_call ~ecx ~func_val ~arg_vals ~receiver_val:(Some receiver_val) ~ret_type

and emit_call
    ~ecx
    ~(func_val : Value.function_value)
    ~(arg_vals : Value.t list)
    ~(receiver_val : Value.t option)
    ~(ret_type : Type.t) =
  let var_id = mk_var_id () in
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
        (Std_lib.std_io_file_builtin_close, emit_std_io_close);
        (Std_lib.std_io_file_builtin_open, emit_std_io_open);
        (Std_lib.std_io_file_builtin_read, emit_std_io_read);
        (Std_lib.std_io_file_builtin_unlink, emit_std_io_unlink);
        (Std_lib.std_io_file_builtin_write, emit_std_io_write);
        (Std_lib.std_sys_exit, emit_std_sys_exit);
        (Std_lib.std_unit_unit_equals, emit_eq);
      ]
    |> List.to_seq
    |> SMap.of_seq )

and emit_eq ~ecx arg_vals _ =
  match arg_vals with
  | [left_arg; right_arg] ->
    let var_id = mk_var_id () in
    Ecx.emit
      ~ecx
      (Eq (var_id, cast_to_comparable_value left_arg, cast_to_comparable_value right_arg));
    var_value_of_type var_id `BoolT
  | _ -> failwith "Expected two arguments"

and emit_std_byte_byte_toInt ~ecx arg_vals _ =
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (SExt (var_id, cast_to_numeric_value (List.hd arg_vals), `IntT));
  var_value_of_type var_id `IntT

and emit_std_byte_byte_toLong ~ecx arg_vals _ =
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (SExt (var_id, cast_to_numeric_value (List.hd arg_vals), `LongT));
  var_value_of_type var_id `LongT

and emit_std_int_int_toByte ~ecx arg_vals _ =
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (Trunc (var_id, cast_to_numeric_value (List.hd arg_vals), `ByteT));
  var_value_of_type var_id `ByteT

and emit_std_int_int_toLong ~ecx arg_vals _ =
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (SExt (var_id, cast_to_numeric_value (List.hd arg_vals), `LongT));
  var_value_of_type var_id `LongT

and emit_std_long_long_toByte ~ecx arg_vals _ =
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (Trunc (var_id, cast_to_numeric_value (List.hd arg_vals), `ByteT));
  var_value_of_type var_id `ByteT

and emit_std_long_long_toInt ~ecx arg_vals _ =
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (Trunc (var_id, cast_to_numeric_value (List.hd arg_vals), `IntT));
  var_value_of_type var_id `IntT

and emit_std_memory_array_copy ~ecx arg_vals _ =
  match arg_vals with
  | [dest_array; dest_index; src_array; src_index; count] ->
    let dest_ptr = emit_offset_ptr ~ecx dest_array dest_index in
    let src_ptr = emit_offset_ptr ~ecx src_array src_index in
    let var_id = mk_var_id () in
    let (return_val, myte_copy_instr) =
      Mir_builtin.(mk_call_builtin myte_copy var_id [dest_ptr; src_ptr; count] [])
    in
    Ecx.emit ~ecx myte_copy_instr;
    return_val
  | _ -> failwith "Array.copy expects five arguments"

and emit_std_memory_array_new ~ecx arg_vals ret_type =
  match arg_vals with
  (* An allocation of 0 represents the null pointer *)
  | [`IntL 0l] -> `LongL 0L
  | _ ->
    let var_id = mk_var_id () in
    let (`PointerT element_ty) = cast_to_pointer_type ret_type in
    let (array_ptr_val, myte_alloc_instr) =
      Mir_builtin.(mk_call_builtin myte_alloc var_id arg_vals [element_ty])
    in
    Ecx.emit ~ecx myte_alloc_instr;
    array_ptr_val

and emit_std_io_write ~ecx arg_vals _ =
  let var_id = mk_var_id () in
  let arg_vals =
    match arg_vals with
    | [file_val; buffer_val; offset_val; size_val] ->
      let ptr_val = emit_offset_ptr ~ecx buffer_val offset_val in
      [file_val; ptr_val; size_val]
    | _ -> failwith "Expected four arguments"
  in
  let (return_val, instr) = Mir_builtin.(mk_call_builtin myte_write var_id arg_vals []) in
  Ecx.emit ~ecx instr;
  return_val

and emit_std_io_read ~ecx arg_vals _ =
  let var_id = mk_var_id () in
  let (return_val, instr) = Mir_builtin.(mk_call_builtin myte_read var_id arg_vals []) in
  Ecx.emit ~ecx instr;
  return_val

and emit_std_io_open ~ecx arg_vals _ =
  let var_id = mk_var_id () in
  let (return_val, instr) = Mir_builtin.(mk_call_builtin myte_open var_id arg_vals []) in
  Ecx.emit ~ecx instr;
  return_val

and emit_std_io_close ~ecx arg_vals _ =
  let var_id = mk_var_id () in
  let (return_val, instr) = Mir_builtin.(mk_call_builtin myte_close var_id arg_vals []) in
  Ecx.emit ~ecx instr;
  return_val

and emit_std_io_unlink ~ecx arg_vals _ =
  let var_id = mk_var_id () in
  let (return_val, instr) = Mir_builtin.(mk_call_builtin myte_unlink var_id arg_vals []) in
  Ecx.emit ~ecx instr;
  return_val

and emit_std_sys_exit ~ecx arg_vals _ =
  let var_id = mk_var_id () in
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
  let func_val =
    Ecx.get_generic_function_value
      ~ecx
      vec_get_name
      vec_get_method_sig.trait_sig.type_params
      [element_ty]
  in
  (* Emit call to Vec's `get` method *)
  let var_id = mk_var_id () in
  let ret_ty = Ecx.to_mir_type ~ecx element_ty in
  Ecx.emit ~ecx (Call (var_id, ret_ty, func_val, [vec_var; index_var]));
  var_value_of_type var_id ret_ty

and emit_call_vec_set ~ecx element_ty_loc vec_var index_var expr_var =
  (* Get full name for Vec's `set` method *)
  let vec_get_method_sig = Types.AdtSig.lookup_method !Std_lib.vec_adt_sig "set" |> Option.get in
  let vec_get_binding = Bindings.get_value_binding ecx.Ecx.pcx.bindings vec_get_method_sig.loc in
  let vec_get_name = mk_value_binding_name vec_get_binding in
  (* Find element type and instantiate Vec's `set` method *)
  let element_ty = type_of_loc ~ecx element_ty_loc in
  let func_val =
    Ecx.get_generic_function_value
      ~ecx
      vec_get_name
      vec_get_method_sig.trait_sig.type_params
      [element_ty]
  in
  (* Emit call of Vec's `set` method *)
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (Call (var_id, `UnitT, func_val, [vec_var; index_var; expr_var]))

and emit_call_map_get ~ecx return_loc map_type_args map_var index_var =
  (* Get full name for Map's `get` method *)
  let map_get_method_sig = Types.AdtSig.lookup_method !Std_lib.map_adt_sig "get" |> Option.get in
  let map_get_binding = Bindings.get_value_binding ecx.pcx.bindings map_get_method_sig.loc in
  let map_get_name = mk_value_binding_name map_get_binding in
  (* Find element type and instantiate Map's `get` method *)
  let return_ty = type_of_loc ~ecx return_loc in
  let func_val =
    Ecx.get_generic_function_value
      ~ecx
      map_get_name
      map_get_method_sig.trait_sig.type_params
      map_type_args
  in
  (* Emit call to Map's `get` method *)
  let var_id = mk_var_id () in
  let ret_ty = Ecx.to_mir_type ~ecx return_ty in
  Ecx.emit ~ecx (Call (var_id, ret_ty, func_val, [map_var; index_var]));
  var_value_of_type var_id ret_ty

and emit_call_map_add ~ecx map_type_args map_var index_var expr_var =
  (* Get full name for Map's `add` method *)
  let map_add_method_sig = Types.AdtSig.lookup_method !Std_lib.map_adt_sig "add" |> Option.get in
  let map_add_binding = Bindings.get_value_binding ecx.Ecx.pcx.bindings map_add_method_sig.loc in
  let map_add_name = mk_value_binding_name map_add_binding in
  (* Find element type and instantiate Map's `add` method *)
  let func_val =
    Ecx.get_generic_function_value
      ~ecx
      map_add_name
      map_add_method_sig.trait_sig.type_params
      map_type_args
  in
  (* Emit call of Map's `add` method *)
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (Call (var_id, `UnitT, func_val, [map_var; index_var; expr_var]))

and emit_call_map_reserve ~ecx map_type_args map_val capacity_val =
  (* Get full name for Map's `reserve` method *)
  let map_reserve_method_sig =
    Types.AdtSig.lookup_method !Std_lib.map_adt_sig "reserve" |> Option.get
  in
  let map_reserve_binding =
    Bindings.get_value_binding ecx.Ecx.pcx.bindings map_reserve_method_sig.loc
  in
  let map_reserve_name = mk_value_binding_name map_reserve_binding in
  (* Find element type and instantiate Map's `reserve` method *)
  let func_val =
    Ecx.get_generic_function_value
      ~ecx
      map_reserve_name
      map_reserve_method_sig.trait_sig.type_params
      map_type_args
  in
  (* Emit call of Map's `reserve` method *)
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (Call (var_id, `UnitT, func_val, [map_val; capacity_val]))

and emit_call_map_new ~ecx return_mir_type map_type_args =
  let decl_loc = Std_lib.lookup_stdlib_decl_loc Std_lib.std_map_map_new in
  let binding = Bindings.get_value_binding ecx.Ecx.pcx.bindings decl_loc in
  let func_val =
    match binding.declaration with
    | FunDecl { Bindings.FunctionDeclaration.type_params; _ } ->
      let func_name = mk_value_binding_name binding in
      Ecx.get_generic_function_value ~ecx func_name type_params map_type_args
    | _ -> failwith "Expected function declaration"
  in
  (* Emit call of Map's `new` function *)
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (Call (var_id, return_mir_type, func_val, []));
  var_value_of_type var_id return_mir_type

and emit_call_set_add ~ecx set_type_args set_val element_val =
  (* Get full name for set's `add` method *)
  let set_add_method_sig = Types.AdtSig.lookup_method !Std_lib.set_adt_sig "add" |> Option.get in
  let set_add_binding = Bindings.get_value_binding ecx.Ecx.pcx.bindings set_add_method_sig.loc in
  let set_add_name = mk_value_binding_name set_add_binding in
  (* Find element type and instantiate set's `add` method *)
  let func_val =
    Ecx.get_generic_function_value
      ~ecx
      set_add_name
      set_add_method_sig.trait_sig.type_params
      set_type_args
  in
  (* Emit call of set's `add` method *)
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (Call (var_id, `UnitT, func_val, [set_val; element_val]))

and emit_call_set_reserve ~ecx set_type_args set_val capacity_val =
  (* Get full name for set's `reserve` method *)
  let set_reserve_method_sig =
    Types.AdtSig.lookup_method !Std_lib.set_adt_sig "reserve" |> Option.get
  in
  let set_reserve_binding =
    Bindings.get_value_binding ecx.Ecx.pcx.bindings set_reserve_method_sig.loc
  in
  let set_reserve_name = mk_value_binding_name set_reserve_binding in
  (* Find element type and instantiate set's `reserve` method *)
  let func_val =
    Ecx.get_generic_function_value
      ~ecx
      set_reserve_name
      set_reserve_method_sig.trait_sig.type_params
      set_type_args
  in
  (* Emit call of set's `reserve` method *)
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (Call (var_id, `UnitT, func_val, [set_val; capacity_val]))

and emit_call_set_new ~ecx return_mir_type set_type_args =
  let decl_loc = Std_lib.lookup_stdlib_decl_loc Std_lib.std_set_set_new in
  let binding = Bindings.get_value_binding ecx.Ecx.pcx.bindings decl_loc in
  let func_val =
    match binding.declaration with
    | FunDecl { Bindings.FunctionDeclaration.type_params; _ } ->
      let func_name = mk_value_binding_name binding in
      Ecx.get_generic_function_value ~ecx func_name type_params set_type_args
    | _ -> failwith "Expected function declaration"
  in
  (* Emit call of set's `new` function *)
  let var_id = mk_var_id () in
  Ecx.emit ~ecx (Call (var_id, return_mir_type, func_val, []));
  var_value_of_type var_id return_mir_type

(* If the index is nonzero, emit a GetPointer instruction to calculate the pointer's start *)
and emit_offset_ptr ~ecx ptr_val offset_val =
  match cast_to_numeric_value offset_val with
  | `IntL lit when lit = Int32.zero -> ptr_val
  | offset_val ->
    let ptr_val = cast_to_pointer_value ptr_val in
    let ptr_ty = pointer_value_element_type ptr_val in
    let (get_ptr_val, get_ptr_instr) =
      mk_get_pointer_instr ~pointer_offset:(Some offset_val) ptr_ty ptr_val []
    in
    Ecx.emit ~ecx (GetPointer get_ptr_instr);
    (get_ptr_val :> Value.t)

and emit_store_tag ~ecx tag ptr_var_id =
  let tag = (tag :> Value.t) in
  let tag_ptr_val = `PointerV (type_of_value tag, ptr_var_id) in
  Ecx.emit ~ecx (Store (tag_ptr_val, tag))

and emit_construct_tuple ~ecx ~tag agg elements =
  let agg_ty = `AggregateT agg in

  (* Call myte_alloc builtin to allocate space for tuple *)
  let agg_ptr_var_id = mk_var_id () in
  let agg_ptr_var = `PointerV (agg_ty, agg_ptr_var_id) in
  let (agg_ptr_val, myte_alloc_instr) =
    Mir_builtin.(mk_call_builtin myte_alloc agg_ptr_var_id [`IntL Int32.one] [agg_ty])
  in
  Ecx.emit ~ecx myte_alloc_instr;

  (* If this tuple is a variant, store tag in first element *)
  (match tag with
  | None -> ()
  | Some tag -> emit_store_tag ~ecx tag agg_ptr_var_id);

  (* Store each argument to the tuple constructor in space allocated for tuple *)
  List.iteri
    (fun i (key, element_ty) ->
      (* Find element in tuple based on element key - note this may not match index in aggregate
         elements list due to tag or padding elements (which we skip). *)
      if key.[0] <> '$' then (
        let index = TupleKeyCache.get_index key in
        let arg = List.nth elements index in
        (* Calculate offset for this element and store *)
        let arg_var = emit_expression ~ecx arg in
        let (element_offset_var, get_ptr_instr) =
          mk_get_pointer_instr element_ty agg_ptr_var [Instruction.GetPointer.FieldIndex i]
        in
        Ecx.emit ~ecx (GetPointer get_ptr_instr);
        Ecx.emit ~ecx (Store (element_offset_var, arg_var))
      ))
    agg.Aggregate.elements;
  agg_ptr_val

and emit_construct_record ~ecx ~tag agg fields =
  let open Ast.Expression in
  let agg_ty = `AggregateT agg in
  let emit_field_value_expression { Record.Field.name; value; _ } =
    match value with
    | Some expr -> emit_expression ~ecx expr
    | None -> emit_expression ~ecx (Identifier name)
  in

  (* Call myte_alloc builtin to allocate space for record *)
  let agg_ptr_var_id = mk_var_id () in
  let agg_ptr_var = `PointerV (agg_ty, agg_ptr_var_id) in
  let (agg_ptr_val, myte_alloc_instr) =
    Mir_builtin.(mk_call_builtin myte_alloc agg_ptr_var_id [`IntL Int32.one] [agg_ty])
  in
  Ecx.emit ~ecx myte_alloc_instr;

  (* If this record is a variant, store tag in first element *)
  (match tag with
  | None -> ()
  | Some tag -> emit_store_tag ~ecx tag agg_ptr_var_id);

  (* Store each argument to the record constructor in space allocated for record *)
  List.iter
    (fun ({ Record.Field.name = { name; _ }; _ } as field) ->
      (* Calculate offset for this element and store *)
      let arg_var = emit_field_value_expression field in
      let (element_ty, element_idx) = lookup_element agg name in
      let (element_offset_var, get_ptr_instr) =
        mk_get_pointer_instr element_ty agg_ptr_var [Instruction.GetPointer.FieldIndex element_idx]
      in
      Ecx.emit ~ecx (GetPointer get_ptr_instr);
      Ecx.emit ~ecx (Store (element_offset_var, arg_var)))
    fields;
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

and emit_block ~ecx block =
  let { Statement.Block.statements; _ } = block in
  List.iter (emit_statement ~ecx) statements

and emit_statement ~ecx stmt =
  let open Statement in
  match stmt with
  | ExpressionStatement { loc = _; expr; is_value = _ } -> ignore (emit_expression ~ecx expr)
  | Block block -> emit_block ~ecx block
  (*
   * ============================
   *        If Statement
   * ============================
   *)
  | If { loc = _; test; conseq; altern = None } ->
    (* Branch to conseq or join blocks *)
    let test_val = emit_bool_expression ~ecx test in
    let conseq_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_branch ~ecx test_val conseq_builder.id join_builder.id;

    (* Emit conseq and continue to join block *)
    Ecx.set_block_builder ~ecx conseq_builder;
    emit_block ~ecx conseq;
    Ecx.finish_block_continue ~ecx join_builder.id;

    (* Start join block *)
    Ecx.set_block_builder ~ecx join_builder
  | If { loc = _; test; conseq; altern } ->
    (* Branch to conseq or altern blocks *)
    let test_val = emit_bool_expression ~ecx test in
    let conseq_builder = Ecx.mk_block_builder ~ecx in
    let altern_builder = Ecx.mk_block_builder ~ecx in
    let join_builder = Ecx.mk_block_builder ~ecx in
    Ecx.finish_block_branch ~ecx test_val conseq_builder.id altern_builder.id;

    (* Emit conseq and continue to join block *)
    Ecx.set_block_builder ~ecx conseq_builder;
    emit_block ~ecx conseq;
    Ecx.finish_block_continue ~ecx join_builder.id;

    (* Emit altern and continue to join block *)
    Ecx.set_block_builder ~ecx altern_builder;
    (match altern with
    | Block block -> emit_block ~ecx block
    | If if_ -> emit_statement ~ecx (If if_)
    | None -> failwith "Handled by previous case");
    Ecx.finish_block_continue ~ecx join_builder.id;

    (* Start join block *)
    Ecx.set_block_builder ~ecx join_builder
  (*
   * ============================
   *         While Loop
   * ============================
   *)
  | While { loc = _; test; body } ->
    (* Set up blocks for loop *)
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
    emit_block ~ecx body;
    Ecx.finish_block_continue ~ecx test_builder.id;
    Ecx.pop_loop_context ~ecx;

    (* Start join block *)
    Ecx.set_block_builder ~ecx finish_builder
  (*
   * ============================
   *         For Loop
   * ============================
   *)
  | For { pattern; iterator; body; _ } ->
    (* Set up blocks for loop *)
    let test_builder = Ecx.mk_block_builder ~ecx in
    let body_builder = Ecx.mk_block_builder ~ecx in
    let finish_builder = Ecx.mk_block_builder ~ecx in

    (* Find type of iterable *)
    let iterable_ty = type_of_loc ~ecx (Ast_utils.expression_loc iterator) in
    let (iterable_adt_type_args, iterable_adt_sig) = Type_util.cast_to_adt_type iterable_ty in
    let { Types.TraitSig.type_args = iterable_trait_type_args; _ } =
      Type_context.get_implemented_trait iterable_ty !Std_lib.iterable_trait_sig |> Option.get
    in

    (* Find type of iterator *)
    let to_iterator_sig = Types.AdtSig.lookup_method iterable_adt_sig "toIterator" |> Option.get in
    let (iterator_type_args, iterator_adt_sig) =
      Type_util.cast_to_adt_type to_iterator_sig.return
    in
    let type_param_bindings =
      Types.bind_type_params_to_args to_iterator_sig.trait_sig.type_params iterable_adt_type_args
    in
    let iterator_type_args =
      List.map (Types.substitute_type_params type_param_bindings) iterator_type_args
    in
    let iterator_ty =
      Types.Type.ADT { adt_sig = iterator_adt_sig; type_args = iterator_type_args }
    in
    let iterator_mir_type = Ecx.to_mir_type ~ecx iterator_ty in
    let item_ty = List.hd iterable_trait_type_args in

    (* Find aggregate data for option of iterator item *)
    let mir_adt_layout = Ecx.get_mir_adt_layout ~ecx !Std_lib.option_adt_sig in
    let variants_layout =
      match mir_adt_layout.layout with
      | Variants variants_layout -> variants_layout
      | _ -> failwith "Expected variants layout"
    in
    let instance = Ecx.instantiate_mir_adt_variants_layout ~ecx mir_adt_layout [item_ty] in
    let some_aggregate = SMap.find "Some" instance.variants in
    let some_tag = SMap.find "Some" variants_layout.tags in
    let tag_type = type_of_value (some_tag :> Value.t) in

    (* Before loop starts, emit iterable value and call `toIterator` to create iterator *)
    let iterable_val = emit_expression ~ecx iterator in
    let iterator_val =
      emit_method_call
        ~ecx
        ~method_name:"toIterator"
        ~receiver_val:iterable_val
        ~receiver_ty:iterable_ty
        ~arg_vals:[]
        ~method_instance_type_args:[]
        ~ret_type:iterator_mir_type
    in
    Ecx.finish_block_continue ~ecx test_builder.id;

    (* Test block starts by calling iterator's `next` method, returning item option *)
    Ecx.set_block_builder ~ecx test_builder;
    let item_opt_ptr_val =
      emit_method_call
        ~ecx
        ~method_name:"next"
        ~receiver_val:iterator_val
        ~receiver_ty:iterator_ty
        ~arg_vals:[]
        ~method_instance_type_args:[]
        ~ret_type:(Ecx.to_mir_type ~ecx (Std_lib.mk_option_type item_ty))
    in
    let item_opt_ptr_val = cast_to_pointer_value item_opt_ptr_val in

    (* Test block proceeds to load tag of item option *)
    let tag_ptr_val = cast_pointer_value item_opt_ptr_val tag_type in
    let tag_var_id = mk_var_id () in
    Ecx.emit ~ecx (Load (tag_var_id, tag_ptr_val));

    (* Test block finishes by jumping to body if item is a `Some` variant, otherwise test block
       finishes by jumping to loop end if item is a `None` variant. *)
    let tag_val = cast_to_comparable_value (var_value_of_type tag_var_id tag_type) in
    let test_var_id = mk_var_id () in
    Ecx.emit ~ecx (Eq (test_var_id, tag_val, (some_tag :> Value.comparable_value)));
    Ecx.finish_block_branch ~ecx (`BoolV test_var_id) body_builder.id finish_builder.id;

    (* Body block starts by loading payload from `Some` variant *)
    Ecx.set_block_builder ~ecx body_builder;
    let (item_mir_type, item_index) = lookup_element some_aggregate (TupleKeyCache.get_key 0) in
    let item_some_opt_ptr_val = cast_pointer_value item_opt_ptr_val (`AggregateT some_aggregate) in
    let (item_ptr_val, get_ptr_instr) =
      mk_get_pointer_instr
        item_mir_type
        item_some_opt_ptr_val
        [Instruction.GetPointer.FieldIndex item_index]
    in
    Ecx.emit ~ecx (GetPointer get_ptr_instr);
    let item_var_id = mk_var_id () in
    Ecx.emit ~ecx (Load (item_var_id, item_ptr_val));

    (* Body block then destructures payload to for loop bindings *)
    let item_val = var_value_of_type item_var_id (Ecx.to_mir_type ~ecx item_ty) in
    emit_alloc_destructuring ~ecx pattern item_val;

    (* Body block contains body of for loop and continues to test block *)
    Ecx.push_loop_context ~ecx finish_builder.id test_builder.id;
    emit_block ~ecx body;
    Ecx.pop_loop_context ~ecx;
    Ecx.finish_block_continue ~ecx test_builder.id;

    (* Start join block *)
    Ecx.set_block_builder ~ecx finish_builder
  (*
   * ============================
   *      Loop Control Flow
   * ============================
   *)
  | Continue _ ->
    let (_, continue_id) = Ecx.get_loop_context ~ecx in
    Ecx.finish_block_continue ~ecx continue_id
  | Break _ ->
    let (break_id, _) = Ecx.get_loop_context ~ecx in
    Ecx.finish_block_continue ~ecx break_id
  (*
   * ============================
   *           Return
   * ============================
   *)
  | Return { loc = _; arg } ->
    let arg_val = Option.map (emit_expression ~ecx) arg in
    (* Handle implicit return from main function *)
    let arg_val =
      match arg_val with
      | None
      | Some (`UnitL | `UnitV _)
        when ecx.current_func = ecx.main_label ->
        Some (`IntL Int32.zero)
      | _ -> arg_val
    in
    Ecx.emit ~ecx (Ret arg_val)
  (*
   * ============================
   *         Assignment
   * ============================
   *)
  | Assignment { loc = _; op; lvalue; expr } ->
    (* Apply an operation given the left and right values, returning the result *)
    let emit_apply_op op left_val expr_val =
      let var_id = mk_var_id () in
      let left_val = cast_to_numeric_value left_val in
      let expr_val = cast_to_numeric_value expr_val in
      let instr =
        match op with
        | Assignment.Add -> Instruction.Add (var_id, left_val, expr_val)
        | Subtract -> Sub (var_id, left_val, expr_val)
        | Multiply -> Mul (var_id, left_val, expr_val)
        | Divide -> Div (var_id, left_val, expr_val)
        | Remainder -> Rem (var_id, left_val, expr_val)
        | BitwiseAnd -> BitAnd (var_id, left_val, expr_val)
        | BitwiseOr -> BitOr (var_id, left_val, expr_val)
        | BitwiseXor -> BitXor (var_id, left_val, expr_val)
        | LeftShift -> Shl (var_id, left_val, expr_val)
        | ArithmeticRightShift -> Shr (var_id, left_val, expr_val)
        | LogicalRightShift -> Shrl (var_id, left_val, expr_val)
      in
      Ecx.emit ~ecx instr;
      var_id
    in

    (* Emit an assignment that stores a value at a pointer *)
    let emit_pointer_assign mk_pointer_val expr =
      let store_val =
        match op with
        (* Standard assignments simply store expression value *)
        | None -> emit_expression ~ecx expr
        (* Operator assignments load value, apply operation, then store result *)
        | Some op ->
          (* Load value *)
          let loaded_var_id = mk_var_id () in
          let pointer_val = mk_pointer_val () in
          Ecx.emit ~ecx (Load (loaded_var_id, pointer_val));
          (* Apply operation *)
          let mir_type = pointer_value_element_type pointer_val in
          let loaded_val = var_value_of_type loaded_var_id mir_type in
          let expr_val = emit_expression ~ecx expr in
          let store_var_id = emit_apply_op op loaded_val expr_val in
          var_value_of_type store_var_id mir_type
      in
      let pointer_val = mk_pointer_val () in
      Ecx.emit ~ecx (Store (pointer_val, store_val))
    in

    (* Emit an assignment that stores at an expression access chain *)
    let emit_access_chain_assign lvalue =
      let mk_pointer_val () =
        match emit_expression_access_chain ~ecx lvalue with
        | GetPointerEmittedResult element_pointer_val -> element_pointer_val
        | InlinedValueResult _ -> failwith "Cannot store to inlined value"
      in
      (* TODO: Only emit element pointer once, but this requires fixing lea coalescing in asm *)
      emit_pointer_assign mk_pointer_val expr
    in

    (match lvalue with
    (* Simple variable assignments are stored at the variable's pointer, whether global or local *)
    | Pattern (Identifier { loc; _ }) ->
      let mir_type = mir_type_of_loc ~ecx (Ast_utils.expression_loc expr) in
      let binding = Bindings.get_value_binding ecx.pcx.bindings loc in
      let pointer_val =
        if Bindings.is_module_decl binding then
          Ecx.get_global_pointer ~ecx binding
        else
          `PointerV (mir_type, Ecx.get_ptr_var_id ~ecx loc)
      in
      emit_pointer_assign (fun _ -> pointer_val) expr
    (* Pattern assignments are destructured with a decision tree *)
    | Pattern pattern ->
      if op <> None then failwith "Destructuring cannot be used in operator assignment";
      let expr_val = emit_expression ~ecx expr in
      let decision_tree =
        Mir_match_decision_tree.build_destructure_decision_tree ~ecx expr_val pattern
      in
      let join_block = Ecx.mk_block_builder ~ecx in
      emit_match_decision_tree ~ecx ~join_block ~result_ptr:None ~alloc:false decision_tree;
      Ecx.set_block_builder ~ecx join_block
    | Expression (IndexedAccess { loc; target; index; _ } as expr_lvalue) ->
      let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
      (match target_ty with
      (* If indexing a vector, call Vec's `get` and `set` methods instead of emitting access chain *)
      | ADT { adt_sig; _ } when adt_sig == !Std_lib.vec_adt_sig ->
        let target_val = emit_expression ~ecx target in
        let index_val = emit_expression ~ecx index in
        (match op with
        (* Standard assignment simply calls `set` method *)
        | None ->
          let expr_val = emit_expression ~ecx expr in
          emit_call_vec_set ~ecx loc target_val index_val expr_val
        (* Operator assignment gets old value with `get`, applies operator, then calls `set` *)
        | Some op ->
          let old_value = emit_call_vec_get ~ecx loc target_val index_val in
          let expr_val = emit_expression ~ecx expr in
          let new_var_id = emit_apply_op op old_value expr_val in
          let new_val = var_value_of_type new_var_id (mir_type_of_loc ~ecx loc) in
          emit_call_vec_set ~ecx loc target_val index_val new_val)
      (* If indexing a map, call Map's `add` method instead of emitting access chain *)
      | ADT { adt_sig; type_args } when adt_sig == !Std_lib.map_adt_sig ->
        if op <> None then failwith "Map indexing cannot be used in operator assignment";
        let target_val = emit_expression ~ecx target in
        let index_val = emit_expression ~ecx index in
        let expr_val = emit_expression ~ecx expr in
        emit_call_map_add ~ecx type_args target_val index_val expr_val
      | _ -> emit_access_chain_assign expr_lvalue)
    | Expression (NamedAccess _ as expr_lvalue) -> emit_access_chain_assign expr_lvalue
    | _ -> failwith "Lvalue expression must be an access")
  (*
   * ============================
   *     Variable Declaration
   * ============================
   *)
  | VariableDeclaration { pattern; init; _ } ->
    let init_val = emit_expression ~ecx init in
    emit_alloc_destructuring ~ecx pattern init_val
  (*
   * ============================
   *       Match Statement
   * ============================
   *)
  | Match { args; cases; _ } ->
    (* Emit args and decision tree *)
    let args = List.map (emit_expression ~ecx) args in
    let decision_tree = Mir_match_decision_tree.build_match_decision_tree ~ecx args cases in
    let join_block = Ecx.mk_block_builder ~ecx in
    emit_match_decision_tree ~ecx ~join_block ~result_ptr:None ~alloc:true decision_tree;
    Ecx.set_block_builder ~ecx join_block
  | FunctionDeclaration _ -> failwith "TODO: Emit MIR for non-toplevel function declarations"

and emit_alloc_destructuring ~(ecx : Ecx.t) pattern value =
  match pattern with
  | Pattern.Identifier { loc; _ } ->
    let mir_type = type_of_value value in
    let binding = Bindings.get_value_binding ecx.pcx.bindings loc in
    if Bindings.is_module_decl binding then
      let global_ptr = Ecx.get_global_pointer ~ecx binding in
      Ecx.emit ~ecx (Store (global_ptr, value))
    else
      let var_id = Ecx.get_ptr_var_id ~ecx loc in
      Ecx.emit ~ecx (StackAlloc (var_id, mir_type));
      Ecx.emit ~ecx (Store (`PointerV (mir_type, var_id), value))
  | _ ->
    let decision_tree =
      Mir_match_decision_tree.build_destructure_decision_tree ~ecx value pattern
    in
    let join_block = Ecx.mk_block_builder ~ecx in
    emit_match_decision_tree ~ecx ~join_block ~result_ptr:None ~alloc:true decision_tree;
    Ecx.set_block_builder ~ecx join_block

(* Structure of leaf and case body blocks:

   Leaves of a decision tree are unique and are guaranteed to have a single incoming edge. However
   the leaves of a decision tree do not have a one to one correspondence with match cases. In the
   presence of or patterns, e.g. `Foo(a) | Bar(a) -> case_body`, the same case body will be shared
   between multiple leaf nodes, which may contain different bindings. Note that each leaf node
   contains its own set of bindings, which correspond to the bindings introduced by matching a
   unique configuration of patterns (and importantly or-pattern branches).

   Additionally, case bodies may have a guard, which may be shared between multiple leaf nodes.
   Since leaf nodes may branch to different cases (e.g. in `(x, _) | (_, x) when x = 1 -> body`),
   the guard is duplicated across all leaf nodes and is considered a part of the leaf block.

   To avoid duplication, we generate each leaf and each case body only once. This means that a leaf
   block creates the bindings for that leaf and then executes its guard, before jumping to either
   the shared case body block or the guard fail block. *)

(* Emit the MIR for a match decision tree.

   [join_block] the block after the decision tree, which all branches should connect to at their end
   [result_ptr] if not None, store the resulting value here
   [alloc] fs true, generate StackAlloc instructions for all bindings *)
and emit_match_decision_tree ~ecx ~join_block ~result_ptr ~alloc decision_tree =
  let open Mir_match_decision_tree in
  (* Maintain cache of block builder at beginning of each case body (including guard) *)
  let constructed_case_bodies = ref LocSet.empty in
  let case_body_builder_cache = ref LocMap.empty in
  let get_case_body_builder case_body_loc =
    match LocMap.find_opt case_body_loc !case_body_builder_cache with
    | Some block_builder -> block_builder
    | None ->
      let block_builder = Ecx.mk_block_builder ~ecx in
      case_body_builder_cache := LocMap.add case_body_loc block_builder !case_body_builder_cache;
      block_builder
  in

  (* First emit stack allocations for every binding in decision tree. For each binding, save the
     pointer to its stack allocation. *)
  let rec emit_binding_allocations tree_node acc =
    match tree_node with
    | DecisionTree.Leaf { bindings; guard_fail_case; _ } ->
      let acc =
        List.fold_left
          (fun acc (_, binding_loc) ->
            let decl_loc = Bindings.get_decl_loc_from_value_use ecx.pcx.bindings binding_loc in
            (* The same binding may be defined in multiple places due to or patterns. Make sure to
               only emit a single stack slot. *)
            if LocSet.mem decl_loc acc then
              acc
            else
              let mir_type = mir_type_of_loc ~ecx binding_loc in
              let var_id = Ecx.get_ptr_var_id ~ecx decl_loc in
              Ecx.emit ~ecx (StackAlloc (var_id, mir_type));
              LocSet.add decl_loc acc)
          acc
          bindings
      in
      (match guard_fail_case with
      | None -> acc
      | Some guard_fail_case -> emit_binding_allocations guard_fail_case acc)
    | Test { cases; default_case; _ } ->
      let acc =
        List.fold_left (fun acc (_, tree_node) -> emit_binding_allocations tree_node acc) acc cases
      in
      (match default_case with
      | None -> acc
      | Some default_case -> emit_binding_allocations default_case acc)
  in
  if alloc then ignore (emit_binding_allocations decision_tree LocSet.empty);

  (* Then recursively emit the decision tree and its child nodes *)
  let rec emit_tree_node ~path_cache tree_node =
    match tree_node with
    (* When a leaf tree node is encountered, the pattern has been matched and we can proceed to
       emit the body of the case. *)
    | DecisionTree.Leaf { case_node; guard_fail_case; bindings } ->
      (* First emit bindings for this leaf node, in the order they are defined *)
      let bindings = List.sort (fun (_, loc1) (_, loc2) -> Loc.compare loc1 loc2) bindings in
      ignore
        (List.fold_left
           (fun path_cache (path, binding_loc) ->
             let (binding_val, path_cache) = emit_load_pattern_path ~path_cache path in
             let binding = Bindings.get_value_binding ecx.pcx.bindings binding_loc in
             let decl_loc = binding.loc in
             (* Determine pointer value - may be a global if this is a destructuring assignment *)
             let ptr_val =
               if Bindings.is_module_decl binding then
                 Ecx.get_global_pointer ~ecx binding
               else
                 let mir_type = type_of_value binding_val in
                 let ptr_var_id = Ecx.get_ptr_var_id ~ecx decl_loc in
                 `PointerV (mir_type, ptr_var_id)
             in
             Ecx.emit ~ecx (Store (ptr_val, binding_val));
             path_cache)
           path_cache
           bindings);

      (* Then if case is guarded, emit guard expression and only proceed to case body if guard
         succeeds, otherwise jump to and emit guard fail case, which is itself a decision tree. *)
      (match case_node with
      (* Destructurings do not have a case body, so simply continue to join block *)
      | None -> Ecx.finish_block_continue ~ecx join_block.id
      | Some case_node ->
        let case_body_builder = get_case_body_builder case_node.loc in
        (match case_node.guard with
        | None -> Ecx.finish_block_continue ~ecx case_body_builder.id
        | Some guard_expr ->
          let guard_test_val = emit_bool_expression ~ecx guard_expr in
          let guard_fail_builder = Ecx.mk_block_builder ~ecx in
          Ecx.finish_block_branch ~ecx guard_test_val case_body_builder.id guard_fail_builder.id;
          Ecx.set_block_builder ~ecx guard_fail_builder;
          emit_tree_node ~path_cache (Option.get guard_fail_case));

        (* Ensure that case body is only emitted once, and shared between leaves *)
        if not (LocSet.mem case_node.loc !constructed_case_bodies) then (
          constructed_case_bodies := LocSet.add case_node.loc !constructed_case_bodies;
          Ecx.set_block_builder ~ecx case_body_builder;

          (* Emit the right hand side of case, store result at result ptr if one is supplied. *)
          match case_node.right with
          | Expression expr ->
            let body_val = emit_expression ~ecx expr in
            (match result_ptr with
            | None -> ()
            | Some result_ptr -> Ecx.emit ~ecx (Store (result_ptr, body_val)))
          | Statement stmt ->
            emit_statement ~ecx stmt;
            (match result_ptr with
            | None -> ()
            | Some result_ptr -> Ecx.emit ~ecx (Store (result_ptr, `UnitL)))
        );

        (* Continue to match's overall join block at end of case body *)
        Ecx.finish_block_continue ~ecx join_block.id)
    (* Otherwise we need to test a scrutinee and potentially branch to new decision tree nodes to emit *)
    | Test { scrutinee; cases; default_case } ->
      (match List.hd cases with
      (* Tuple and units ctors are unique and do not need to be tested, continue to inner decision tree *)
      | (Ctor.Unit, tree_node)
      | (Tuple _, tree_node) ->
        emit_tree_node ~path_cache tree_node
      (* A single variant (aka a named tuple or record) is unique and does not need to be tested *)
      | (Variant (_, adt_sig, _), tree_node) when SMap.cardinal adt_sig.variants = 1 ->
        emit_tree_node ~path_cache tree_node
      (* Bool tests have exactly two cases, so emit single eq test (a length one if-else chain) *)
      | (Bool _, _) as first_case ->
        let second_case =
          match default_case with
          | Some tree_node -> tree_node
          | None -> snd (List_utils.last cases)
        in
        let (scrutinee_val, path_cache) = emit_load_pattern_path ~path_cache scrutinee in
        let scrutinee_val = cast_to_comparable_value scrutinee_val in
        emit_decision_tree_if_else_chain ~path_cache [first_case] second_case (fun ctor ->
            let test_var_id = mk_var_id () in
            Ecx.emit ~ecx (Eq (test_var_id, scrutinee_val, `BoolL (Ctor.cast_to_bool ctor)));
            `BoolV test_var_id)
      (* Strings are tested for equality in if-else-chain, following source code order *)
      | (String _, _) ->
        (* String matches cannot be fully enumerated, so must have a default case *)
        let default_tree_node = Option.get default_case in
        let (scrutinee_val, path_cache) = emit_load_pattern_path ~path_cache scrutinee in
        (* Order string tests in order cases are written in source *)
        let cases =
          List.sort
            (fun (ctor1, _) (ctor2, _) ->
              let (_, loc1) = Ctor.cast_to_string ctor1 in
              let (_, loc2) = Ctor.cast_to_string ctor2 in
              Loc.compare loc1 loc2)
            cases
        in
        emit_decision_tree_if_else_chain ~path_cache cases default_tree_node (fun ctor ->
            let (value, loc) = Ctor.cast_to_string ctor in
            (* TODO: Check raw bytes directly instead of allocating new string and calling equals *)
            let string_val = emit_string_literal ~ecx loc value in
            let test_val =
              emit_method_call
                ~ecx
                ~method_name:"equals"
                ~receiver_val:scrutinee_val
                ~receiver_ty:(Std_lib.mk_string_type ())
                ~arg_vals:[string_val]
                ~method_instance_type_args:[]
                ~ret_type:`BoolT
            in
            cast_to_bool_value test_val)
      (* Ints are tested for equality in if-else chain *)
      | (Int _, _) ->
        (* Split out last case if there is no default, as it does not have to be explicitly tested *)
        let (test_cases, default_case) =
          match default_case with
          | None ->
            let (cases, (_, last_tree_node)) = List_utils.split_last cases in
            (cases, last_tree_node)
          | Some default_tree_node -> (cases, default_tree_node)
        in
        let (scrutinee_val, path_cache) = emit_load_pattern_path ~path_cache scrutinee in
        let scrutinee_val = cast_to_comparable_value scrutinee_val in
        let scrutinee_type = type_of_value (scrutinee_val :> Value.t) in
        (* TODO: Emit better checks than linear if else chain - e.g. binary search, jump table *)
        emit_decision_tree_if_else_chain ~path_cache test_cases default_case (fun ctor ->
            let (value, _) = Ctor.cast_to_int ctor in
            let case_val =
              match scrutinee_type with
              | `ByteT -> `ByteL (Int64.to_int value)
              | `IntT -> `IntL (Int64.to_int32 value)
              | `LongT -> `LongL value
              | _ -> failwith "Expected numeric value"
            in
            let test_var_id = mk_var_id () in
            Ecx.emit ~ecx (Eq (test_var_id, scrutinee_val, case_val));
            `BoolV test_var_id)
      (* Variants are tested by loading their tag and checking against scrutinee in if-else chain *)
      | (Variant (_, adt_sig, _), _) ->
        let mir_adt_layout = Ecx.get_mir_adt_layout ~ecx adt_sig in
        let (scrutinee_tag_val, tags, path_cache) =
          match mir_adt_layout.layout with
          (* If layout is a variant then load tag value so it can be tested *)
          | Variants layout ->
            let tag_type = (layout.tag_mir_type :> Type.t) in
            let (scrutinee_val, path_cache) = emit_load_pattern_path ~path_cache scrutinee in
            let scrutinee_val = cast_to_pointer_value scrutinee_val in
            let scrutinee_tag_ptr_val = cast_pointer_value scrutinee_val tag_type in
            let scrutinee_tag_var_id = mk_var_id () in
            Ecx.emit ~ecx (Load (scrutinee_tag_var_id, scrutinee_tag_ptr_val));
            let scrutinee_tag_val =
              cast_to_comparable_value (var_value_of_type scrutinee_tag_var_id tag_type)
            in
            (scrutinee_tag_val, layout.tags, path_cache)
          (* If layout is a pure enum then scrutinee is already tag value *)
          | PureEnum layout ->
            let (scrutinee_val, path_cache) = emit_load_pattern_path ~path_cache scrutinee in
            (cast_to_comparable_value scrutinee_val, layout.tags, path_cache)
          | _ -> failwith "Invalid layout for variants"
        in
        (* Split out last case if there is no default, as it does not have to be explicitly tested *)
        let (test_cases, default_case) =
          match default_case with
          | None ->
            let (cases, (_, last_tree_node)) = List_utils.split_last cases in
            (cases, last_tree_node)
          | Some default_tree_node -> (cases, default_tree_node)
        in

        (* TODO: Emit better checks than linear if else chain - e.g. binary search, jump table *)
        emit_decision_tree_if_else_chain ~path_cache test_cases default_case (fun ctor ->
            let (name, _, _) = Ctor.cast_to_variant ctor in
            let tag_val = (SMap.find name tags :> Mir.Value.comparable_value) in
            let test_var_id = mk_var_id () in
            Ecx.emit ~ecx (Eq (test_var_id, scrutinee_tag_val, tag_val));
            `BoolV test_var_id))
  (* Return the value indexed by a pattern path. Will emit load instructions if the pattern path
     points into an aggregate. Return the path cache with all new calculated paths added. *)
  and emit_load_pattern_path ~path_cache pattern_path =
    let open Mir_match_decision_tree in
    (* Find the root value, and all field accesses after it in order *)
    let rec gather_path_fields path field_chain =
      match path with
      | PatternPath.Root value -> (value, field_chain)
      | (TupleField { parent; _ } | VariantField { parent; _ }) as field ->
        gather_path_fields parent (field :: field_chain)
    in
    let get_field_key field =
      match field with
      | PatternPath.TupleIndex index -> TupleKeyCache.get_key index
      | RecordField name -> name
    in
    let (root_value, fields) = gather_path_fields pattern_path [] in
    (* If there are field accesses, load the inner field values one at a time *)
    List.fold_left
      (fun (value, path_cache) field ->
        let path_field_id = PatternPath.get_field_id field in
        match IMap.find_opt path_field_id path_cache with
        | Some path_value -> (path_value, path_cache)
        | None ->
          (* If already in the path cache, use already generated value for path *)
          let pointer_val = cast_to_pointer_value value in
          let (aggregate, field_key, pointer_val) =
            match field with
            | PatternPath.Root _ -> failwith "Expected field"
            (* Tuple fields are simply looked up in the tuple type *)
            | TupleField { index; _ } ->
              let value_type = type_of_value value in
              let (`PointerT ptr_type) = cast_to_pointer_type value_type in
              let (`AggregateT aggregate) = cast_to_aggregate_type ptr_type in
              let field_key = TupleKeyCache.get_key index in
              (aggregate, field_key, pointer_val)
            (* Look up field in single variant (aggregate) types *)
            | VariantField { field; adt_sig; type_args; _ } when SMap.cardinal adt_sig.variants = 1
              ->
              let mir_adt_layout = Ecx.get_mir_adt_layout ~ecx adt_sig in
              let agg = Ecx.instantiate_mir_adt_aggregate_layout ~ecx mir_adt_layout type_args in
              let field_key = get_field_key field in
              (agg, field_key, pointer_val)
            (* For variant fields we must find the correct variant aggregate, fetch its field key,
               and cast the pointer to the variant aggregate type. *)
            | VariantField { field; variant_name; adt_sig; type_args; _ } ->
              let mir_adt_layout = Ecx.get_mir_adt_layout ~ecx adt_sig in
              let instance =
                Ecx.instantiate_mir_adt_variants_layout ~ecx mir_adt_layout type_args
              in
              let variant_aggregate = SMap.find variant_name instance.variants in
              let field_key = get_field_key field in
              let variant_agg_type = `AggregateT variant_aggregate in
              (variant_aggregate, field_key, cast_pointer_value pointer_val variant_agg_type)
          in
          let (element_type, element_index) = lookup_element aggregate field_key in
          let (element_ptr_val, get_ptr_instr) =
            mk_get_pointer_instr
              element_type
              pointer_val
              [Instruction.GetPointer.FieldIndex element_index]
          in
          Ecx.emit ~ecx (GetPointer get_ptr_instr);
          let var_id = mk_var_id () in
          Ecx.emit ~ecx (Load (var_id, element_ptr_val));
          let value = var_value_of_type var_id element_type in
          (value, IMap.add path_field_id value path_cache))
      (root_value, path_cache)
      fields
  (* Emit a sequence of tests and resulting blocks given a list of tests cases with ctors, the default
     case if all test ctors fail, and a function to generate a boolean test value from a ctor. *)
  and emit_decision_tree_if_else_chain ~path_cache test_cases default_case gen_test_val =
    let emit_test (ctor, tree_node) is_last_test =
      let test_val = gen_test_val ctor in
      let pass_case_builder = Ecx.mk_block_builder ~ecx in
      (* The last test links directly to the default case, otherwise create a new block for the next test *)
      let fail_case_builder =
        if is_last_test then
          Ecx.mk_block_builder ~ecx
        else
          Ecx.mk_block_builder ~ecx
      in
      Ecx.finish_block_branch ~ecx test_val pass_case_builder.id fail_case_builder.id;
      Ecx.set_block_builder ~ecx pass_case_builder;
      emit_tree_node ~path_cache tree_node;
      Ecx.set_block_builder ~ecx fail_case_builder
    in
    let rec iter test_cases =
      match test_cases with
      | [] -> ()
      | case :: rest ->
        emit_test case (rest = []);
        iter rest
    in
    iter test_cases;
    emit_tree_node ~path_cache default_case
  in

  emit_tree_node ~path_cache:IMap.empty decision_tree

and mk_get_pointer_instr ?(pointer_offset = None) return_ty pointer offsets =
  let var_id = mk_var_id () in
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
