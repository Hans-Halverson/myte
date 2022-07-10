open Ast
open Basic_collections
open Mir
open Mir_builders
open Mir_emit_utils
open Mir_type
module Ecx = Mir_emit_context
module Pcx = Program_context

type access_chain_part =
  | AccessChainOffset of Instruction.GetPointer.value_offset * Type.t
  | AccessChainDereference

type access_chain_result =
  | GetPointerEmittedResult of (* Pointer *) Value.t
  | InlinedValueResult of Value.t

let rec emit (pcx : Pcx.t) : Program.t =
  let ecx = Ecx.mk ~pcx in
  start_init_function ~ecx;
  register_type_declarations ~ecx;
  register_toplevel_variable_declarations ~ecx;
  register_function_declarations ~ecx;
  emit_pending ~ecx;
  ecx.program

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
  if Opts.emit_all () then emit_all_enqueue_pending ~ecx;

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
    | Some (func, func_decl) ->
      emit_nongeneric_function ~ecx func func_decl;
      Ecx.mark_pending_nongeneric_function_completed ~ecx func;
      complete := false;
      emit_pending_nongeneric_functions ()
  in

  (* Emit all pending instantiations of generic functions *)
  let rec emit_pending_generic_func_instantations () =
    match Ecx.pop_pending_generic_func_instantiation ~ecx with
    | None -> ()
    | Some func_instantiation ->
      emit_generic_function_instantiation ~ecx func_instantiation;
      complete := false;
      emit_pending_generic_func_instantations ()
  in

  (* Emit all pending anonymous functions *)
  let rec emit_pending_anonymous_functions () =
    match Ecx.pop_pending_anonymous_function ~ecx with
    | None -> ()
    | Some (func, func_instantiation) ->
      emit_anonymous_function_instantiation ~ecx func func_instantiation;
      Ecx.mark_pending_anonymous_function_completed ~ecx func;
      complete := false;
      emit_pending_anonymous_functions ()
  in

  (* Emit all pending trampoline functions *)
  let rec emit_pending_trampoline_functions () =
    match Ecx.pop_pending_trampoline_function ~ecx with
    | None -> ()
    | Some func ->
      emit_trampoline_function ~ecx func;
      Ecx.mark_pending_trampoline_function_completed ~ecx func;
      complete := false;
      emit_pending_trampoline_functions ()
  in

  let rec emit_all_pending () =
    complete := true;
    emit_pending_globals ();
    emit_pending_nongeneric_functions ();
    emit_pending_generic_func_instantations ();
    emit_pending_anonymous_functions ();
    emit_pending_trampoline_functions ();
    if not !complete then emit_all_pending ()
  in
  emit_all_pending ()

and emit_all_enqueue_pending ~ecx =
  let enqueue_nongeneric_function func_node =
    let binding = Bindings.get_value_binding ecx.pcx.bindings func_node.Ast.Function.name.loc in
    let func_decl = Bindings.get_func_decl binding in
    if (not func_decl.is_builtin) && (not func_decl.is_signature) && func_decl.type_params = [] then
      let func_name = mk_value_binding_name binding in
      ignore (Ecx.get_nongeneric_function_value ~ecx func_name)
  in

  iter_toplevels ~ecx (fun toplevel ->
      match toplevel with
      (* All global variables are enqueued *)
      | VariableDeclaration { pattern = Identifier { loc; _ }; _ } ->
        let binding = Bindings.get_value_binding ecx.pcx.bindings loc in
        ignore (Ecx.get_global_pointer ~ecx binding)
      | VariableDeclaration _ -> failwith "Pattern must be single identifier"
      (* Nongeneric toplevel functions are enqueued *)
      | FunctionDeclaration func_node -> enqueue_nongeneric_function func_node
      (* Nongeneric methods are enqueued, including static methods on generic traits *)
      | TraitDeclaration { kind; type_params; methods; _ } ->
        let is_trait_generic = kind = Trait || type_params <> [] in
        List.iter
          (fun meth ->
            if (not is_trait_generic) || meth.Ast.Function.static then
              enqueue_nongeneric_function meth)
          methods
      (* Nongeneric types are enqueued *)
      | TypeDeclaration _ -> ())

and emit_global_variable_declaration ~ecx name decl =
  let { Statement.VariableDeclaration.init; _ } = decl in
  (* Build IR for variable init *)
  let global = SMap.find name ecx.program.globals in
  let binding = Bindings.get_value_binding ecx.pcx.bindings global.loc in
  let init =
    Ecx.emit_init_section ~ecx (fun _ ->
        ecx.current_in_std_lib <- Bindings.is_std_lib_value binding;
        (* Do not generate init blocks for stdlib globals when filtering stdlib *)
        if ecx.current_in_std_lib && ecx.filter_std_lib then
          None
        else
          let init_val = emit_expression ~ecx init in
          (* Globals are only emitted if they have a MIR value *)
          let init_val = Option.get init_val in
          (* If initial value is statically known at compile time, add it as constant initialization *)
          if is_literal init_val then
            Some init_val
          else (
            (* Otherwise value must be calculated and stored at runtime *)
            mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:global.value ~value:init_val;
            None
          ))
  in
  global_set_init ~global ~init

and emit_nongeneric_function ~ecx func func_decl =
  if not func_decl.builtin then emit_function_body ~ecx func func_decl

and emit_generic_function_instantiation ~ecx (name, (func, type_param_bindings)) =
  Ecx.in_type_binding_context ~ecx type_param_bindings (fun _ ->
      let func_decl_node = SMap.find name ecx.func_decl_nodes in
      if not func_decl_node.builtin then emit_function_body ~ecx func func_decl_node)

and emit_anonymous_function_instantiation
    ~ecx func (pending_anon_func : Ecx.PendingAnonymousFunction.t) =
  Ecx.in_type_binding_context ~ecx pending_anon_func.type_param_bindings (fun _ ->
      emit_anonymous_function ~ecx func pending_anon_func)

and emit_function_body ~ecx (func : Function.t) (decl : Ast.Function.t) =
  let open Ast.Function in
  let { name = { Identifier.loc; _ }; params; body; _ } = decl in
  let binding = Type_context.get_value_binding ~cx:ecx.pcx.type_ctx loc in
  ecx.current_in_std_lib <- Bindings.is_std_lib_value binding;

  (* Clear emit context function context *)
  ecx.local_variable_to_alloc_instr <- Bindings.BVMap.empty;
  ecx.param_to_argument <- Bindings.BVMap.empty;

  (* Create MIR vars for function params *)
  let func_decl = Bindings.get_func_decl binding in
  let params =
    List_utils.filter_map2
      (fun { Param.name = { Identifier.loc; _ }; _ } ty ->
        match type_to_mir_type ~ecx ty with
        | Some mir_type ->
          let binding = Bindings.get_value_binding ecx.pcx.bindings loc in
          Some (Ecx.add_function_argument ~ecx ~func binding mir_type)
        | None -> None)
      params
      func_decl.params
  in

  (* Add implicit this param *)
  let params =
    match func_decl.this_binding_id with
    | Some this_binding_id ->
      let binding = Bindings.get_this_binding ecx.pcx.bindings this_binding_id in
      let { Bindings.ThisDeclaration.tvar } = Bindings.get_this_decl binding in
      (* Method receiver parameters cannot be removed if they are zero sized due to compatability
         with trait objects. Instead use pointer to zero type type for receiver. *)
      let this_type =
        match type_to_mir_type ~ecx (Types.Type.TVar tvar) with
        | Some this_type -> this_type
        | None -> Pointer (Ecx.get_zero_size_type ~ecx)
      in
      Ecx.add_function_argument ~ecx ~func binding this_type :: params
    | _ -> params
  in

  (* The main function must always return an Int *)
  let is_main_func = loc = Option.get ecx.pcx.main_loc in
  let return_ty = Ecx.find_rep_non_generic_type ~ecx func_decl.return in
  let return_type =
    if is_main_func then
      Some Type.Int
    else
      Ecx.to_mir_type ~ecx return_ty
  in
  Ecx.start_function ~ecx ~func ~loc ~params ~return_type;

  if is_main_func then ecx.program.main_func <- ecx.current_func;

  (* Set up return pointer and block, and create function context *)
  let (return_block, return_pointer) = emit_function_return_block ~ecx return_type in
  Ecx.start_function_context
    ~ecx
    ~return_ty
    ~return_block
    ~return_pointer
    ~env_agg:None
    ~env_ptr:None
    ~captures:Bindings.LBVMMap.VSet.empty;

  (* Build IR for function body *)
  Ecx.set_current_block ~ecx ecx.current_func.start_block;
  (match body with
  | Block block -> emit_function_block_body ~ecx ~is_main_func block
  | Expression expr -> emit_function_expression_body ~ecx expr
  | Signature -> failwith "Cannot emit function signature");
  Ecx.finish_block_unreachable ~ecx

and emit_function_return_block ~ecx return_type =
  (* Set up return value and return block *)
  let return_block = Ecx.mk_block ~ecx in
  let return_pointer =
    match return_type with
    (* If there is no return value then simply return in return block *)
    | None ->
      Ecx.set_current_block ~ecx return_block;
      Ecx.finish_block_ret ~ecx ~arg:None;
      None
    (* If there is return value allocate space for it in start block, then load and return it in
       return block. *)
    | Some type_ ->
      let return_pointer = mk_stack_alloc ~block:(Ecx.get_current_block ~ecx) ~type_ in
      Ecx.set_current_block ~ecx return_block;
      let return_val = mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:return_pointer in
      Ecx.finish_block_ret ~ecx ~arg:(Some return_val);
      Some return_pointer
  in
  (return_block, return_pointer)

and emit_function_block_body ~ecx ~is_main_func block =
  ignore (emit_block ~ecx ~is_expr:false block);
  (* Add an implicit return if the last instruction is not a terminator *)
  match ecx.current_block with
  | Some current_block ->
    (match get_terminator current_block with
    | None ->
      (* Handle implicit return from main *)
      (if is_main_func then
        let return_pointer = Option.get ecx.current_func_context.return_pointer in
        mk_store_
          ~block:(Ecx.get_current_block ~ecx)
          ~ptr:return_pointer
          ~value:(mk_int_lit_of_int32 Int32.zero));
      Ecx.finish_block_continue ~ecx ecx.current_func_context.return_block
    | Some _ -> ())
  | None -> ()

and emit_function_expression_body ~ecx expr =
  let return_val_opt = emit_expression ~ecx expr in
  (match return_val_opt with
  | None -> ()
  | Some return_val ->
    let return_pointer = Option.get ecx.current_func_context.return_pointer in
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:return_pointer ~value:return_val);
  Ecx.finish_block_continue ~ecx ecx.current_func_context.return_block

and emit_anonymous_function ~ecx func pending_anon_func =
  let open Ast.Expression.AnonymousFunction in
  let { loc; params; body; _ } = pending_anon_func.node in
  ecx.current_in_std_lib <- String.sub func.name 0 4 = "std.";

  (* Reset emit context function context to that of parent function *)
  ecx.local_variable_to_alloc_instr <- Bindings.BVMap.empty;
  ecx.param_to_argument <- pending_anon_func.arguments;

  (* Create environment param where environment is the closure *)
  let env_ptr_type =
    match pending_anon_func.env_agg with
    | None -> Type.Pointer Byte
    | Some env_agg -> Pointer (Aggregate env_agg)
  in
  let env_param = mk_argument ~func ~decl_loc:loc ~type_:env_ptr_type in

  (* Create MIR values for anonymous function params *)
  let params =
    List.filter_map
      (fun { Param.name = { Identifier.loc; _ }; _ } ->
        let binding = Type_context.get_value_binding ~cx:ecx.pcx.type_ctx loc in
        let param_decl = Bindings.get_func_param_decl binding in
        match type_to_mir_type ~ecx (Types.Type.TVar param_decl.tvar) with
        | Some mir_type ->
          let binding = Bindings.get_value_binding ecx.pcx.bindings loc in
          Some (Ecx.add_function_argument ~ecx ~func binding mir_type)
        | None -> None)
      params
  in
  let params = params @ [env_param] in

  (* Get return type of anonymous function *)
  let anon_func_ty = type_of_loc ~ecx loc in
  let func_decl = Type_util.cast_to_function_type anon_func_ty in
  let return_ty = Ecx.find_rep_non_generic_type ~ecx func_decl.return in
  let return_type = Ecx.to_mir_type ~ecx return_ty in

  Ecx.start_function ~ecx ~func ~loc ~params ~return_type;

  (* Set up return pointer and block, and create function context *)
  let (return_block, return_pointer) = emit_function_return_block ~ecx return_type in
  Ecx.start_function_context
    ~ecx
    ~return_ty
    ~return_block
    ~return_pointer
    ~env_agg:pending_anon_func.env_agg
    ~env_ptr:(Some env_param)
    ~captures:pending_anon_func.captures;

  (* Build IR for function body *)
  Ecx.set_current_block ~ecx ecx.current_func.start_block;
  (match body with
  | Block block -> emit_function_block_body ~ecx ~is_main_func:false block
  | Expression expr -> emit_function_expression_body ~ecx expr);
  Ecx.finish_block_unreachable ~ecx

and emit_trampoline_function ~(ecx : Ecx.t) (trampoline_func : Function.t) =
  (* Trampolines are created after wrapped function, so we can always look up the function IR object *)
  let func_name = Ecx.strip_trampoline_name trampoline_func.name in
  let func = SMap.find func_name ecx.program.funcs in

  let copy_args arg_vals =
    List.map
      (fun param ->
        let { Argument.type_; decl_loc; _ } = cast_to_argument param in
        mk_argument ~func:trampoline_func ~decl_loc ~type_)
      arg_vals
  in

  let call_inner_func_and_return args =
    let return_val =
      mk_call ~block:(Ecx.get_current_block ~ecx) ~func:func.value ~args ~return:func.return_type
    in

    (* Return value from call if applicable *)
    let arg =
      match func.return_type with
      | None -> None
      | Some _ -> Some return_val
    in
    Ecx.finish_block_ret ~ecx ~arg
  in

  (* Copy parameters to new parameters with same type but for trampoline function, except receiver
     has pointer type since it is boxed. *)
  let (receiver_param, rest_params) = List_utils.split_first func.params in
  let receiver_param = cast_to_argument receiver_param in
  let receiver_ptr_arg =
    mk_argument
      ~func:trampoline_func
      ~decl_loc:receiver_param.decl_loc
      ~type_:(Pointer receiver_param.type_)
  in
  let rest_args = copy_args rest_params in
  let params = receiver_ptr_arg :: rest_args in

  Ecx.start_function ~ecx ~func:trampoline_func ~loc:func.loc ~params ~return_type:func.return_type;

  (* Load receiver and call the wrapped function, passing original args with loaded receiver *)
  let receiver_arg = mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:receiver_ptr_arg in
  let args = receiver_arg :: rest_args in
  call_inner_func_and_return args

and start_init_function ~ecx =
  let func = Ecx.mk_empty_function ~ecx ~name:init_func_name in
  Ecx.start_function ~ecx ~func ~loc:Loc.none ~params:[] ~return_type:None;
  ecx.last_init_block <- Some ecx.current_func.start_block

(*
 * Emit an expression, returning the value for that expression if the expression produces a value.
 * If expression produces a zero sized value then return None.
 *)
and emit_expression ~ecx expr : Value.t option =
  (* Check for trait object promotion on every expression *)
  let expr_val = emit_expression_without_promotion ~ecx expr in
  let expr_loc = Ast_utils.expression_loc expr in
  match Type_context.get_trait_object_promotion ~cx:ecx.Ecx.pcx.type_ctx expr_loc with
  | None -> expr_val
  | Some trait_instance ->
    let ty = type_of_loc ~ecx expr_loc in
    let trait_object_layout = Ecx.get_trait_object_layout ~ecx trait_instance.trait_sig in
    let trait_object_instance = Ecx.instantiate_trait_object_vtable ~ecx trait_instance ty in
    emit_trait_object_promotion ~ecx expr_val trait_object_layout trait_object_instance

and emit_identifier_expression ~(ecx : Ecx.t) (loc : Loc.t) (id : Ast.Identifier.t) : Value.t option
    =
  let binding = Bindings.get_value_binding ecx.pcx.bindings id.loc in
  match binding.declaration with
  | CtorDecl _ ->
    let ty = type_of_loc ~ecx loc in
    Some (emit_construct_enum_variant ~ecx ~name:id.name ~ty)
  (* This is a function that is not statically called, so create its closure object *)
  | FunDecl { Bindings.FunctionDeclaration.type_params; is_builtin; _ } ->
    let func_name = mk_value_binding_name binding in
    let func_val =
      if is_builtin then
        failwith "TODO: Cannot use myte builtin as closure object. Generate MIR for myte builtins."
      else
        let func_val =
          if type_params = [] then
            Ecx.get_nongeneric_function_value ~ecx func_name
          else
            let ty = type_of_loc ~ecx loc in
            let { Types.Function.type_args; _ } = Type_util.cast_to_function_type ty in
            Ecx.get_generic_function_value ~ecx func_name type_params type_args
        in
        let func = cast_to_function_literal func_val in
        Ecx.get_closure_global_value ~ecx ~loc ~func
    in
    Some func_val
  | VarDecl _
  | FunParamDecl _
  | ThisDecl _
  | MatchCaseVarDecl _ ->
    emit_variable_binding_value ~ecx binding

and emit_expression_without_promotion ~ecx expr : Value.t option =
  let open Expression in
  let open Instruction in
  match expr with
  (*
   * ============================
   *         Identifiers
   * ============================
   *)
  | Identifier id -> emit_identifier_expression ~ecx id.loc id
  | NamedAccess { loc; name; _ } when Type_context.is_scope_named_access ~cx:ecx.pcx.type_ctx loc ->
    emit_identifier_expression ~ecx loc name
  (*
   * ============================
   *         Literals
   * ============================
   *)
  | Unit _ -> None
  | BoolLiteral { loc = _; value } -> Some (mk_bool_lit value)
  | IntLiteral { loc; raw; base } ->
    let value = Integers.int64_of_string_opt raw base |> Option.get in
    let ty = mir_type_of_loc ~ecx loc |> Option.get in
    let value =
      match ty with
      | Byte -> mk_byte_lit (Int64.to_int value)
      | Int -> mk_int_lit_of_int32 (Int64.to_int32 value)
      | Long -> mk_long_lit value
      | _ -> failwith "Int literal must have integer type"
    in
    Some value
  | FloatLiteral { loc = _; raw } ->
    let value = Float.of_string raw in
    Some (mk_double_lit value)
  | CharLiteral { loc; value } ->
    let value = int_of_char value in
    let ty = mir_type_of_loc ~ecx loc |> Option.get in
    let value =
      match ty with
      | Byte -> mk_byte_lit value
      | Int -> mk_int_lit_of_int32 (Int32.of_int value)
      | Long -> mk_long_lit (Int64.of_int value)
      | _ -> failwith "Char literal must have integer type"
    in
    Some value
  | StringLiteral { loc; value; _ } -> Some (emit_string_literal ~ecx loc value)
  (*
   * ============================
   *       Unary Operations
   * ============================
   *)
  | UnaryOperation { loc = _; op = Plus; operand } -> emit_expression ~ecx operand
  | UnaryOperation { loc = _; op = Minus; operand } ->
    let arg = emit_numeric_expression ~ecx operand in
    Some (mk_unary ~block:(Ecx.get_current_block ~ecx) ~op:Neg ~arg)
  | UnaryOperation { loc = _; op = Not; operand } ->
    let arg = emit_expression ~ecx operand |> Option.get in
    Some (mk_unary ~block:(Ecx.get_current_block ~ecx) ~op:Not ~arg)
  (*
   * ============================
   *        Logical And
   * ============================
   *)
  | LogicalAnd { loc = _; left; right } ->
    (* Model logical and join by creating stack location to place results of branches *)
    let result_ptr_val = mk_stack_alloc ~block:(Ecx.get_current_block ~ecx) ~type_:Bool in

    (* Short circuit when lhs is false by jumping to false case *)
    let rhs_block = Ecx.mk_block ~ecx in
    let false_block = Ecx.mk_block ~ecx in
    let join_block = Ecx.mk_block ~ecx in
    let left_val = emit_bool_expression ~ecx left in
    Ecx.finish_block_branch ~ecx left_val rhs_block false_block;

    (* Store right hand side when lhs is true and continue to join block *)
    Ecx.set_current_block ~ecx rhs_block;
    let right_val = emit_bool_expression ~ecx right in
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:result_ptr_val ~value:right_val;
    Ecx.finish_block_continue ~ecx join_block;

    (* Store false literal when lhs is false and continue to join block *)
    Ecx.set_current_block ~ecx false_block;
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:result_ptr_val ~value:(mk_bool_lit false);
    Ecx.finish_block_continue ~ecx join_block;

    (* Join cases together and load result from stack location *)
    Ecx.set_current_block ~ecx join_block;
    Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:result_ptr_val)
  (*
   * ============================
   *         Logical Or
   * ============================
   *)
  | LogicalOr { loc = _; left; right } ->
    (* Model logical or join by creating stack location to place results of branches *)
    let result_ptr_val = mk_stack_alloc ~block:(Ecx.get_current_block ~ecx) ~type_:Bool in

    (* Short circuit when lhs is true by jumping to true case *)
    let rhs_block = Ecx.mk_block ~ecx in
    let true_block = Ecx.mk_block ~ecx in
    let join_block = Ecx.mk_block ~ecx in
    let left_val = emit_bool_expression ~ecx left in
    Ecx.finish_block_branch ~ecx left_val true_block rhs_block;

    (* Store right hand side when lhs is false and continue to join block *)
    Ecx.set_current_block ~ecx rhs_block;
    let right_val = emit_bool_expression ~ecx right in
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:result_ptr_val ~value:right_val;
    Ecx.finish_block_continue ~ecx join_block;

    (* Store true literal when lhs is true and continue to join block *)
    Ecx.set_current_block ~ecx true_block;
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:result_ptr_val ~value:(mk_bool_lit true);
    Ecx.finish_block_continue ~ecx join_block;

    (* Join cases together and load result from stack location *)
    Ecx.set_current_block ~ecx join_block;
    Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:result_ptr_val)
  (*
   * ============================
   *          Equality
   * ============================
   *)
  | BinaryOperation { loc = _; op = (Equal | NotEqual) as op; left; right } ->
    let left_ty = type_of_loc ~ecx (Ast_utils.expression_loc left) in
    let receiver_val = emit_expression ~ecx left in
    let arg_vals = List.filter_map (fun x -> x) [emit_expression ~ecx right] in
    let equals_result_val =
      emit_method_call
        ~ecx
        ~method_name:"equals"
        ~receiver_val
        ~receiver_ty:left_ty
        ~arg_vals
        ~method_instance_type_args:[]
        ~ret_type:(Some Type.Bool)
    in

    if op = Equal then
      equals_result_val
    else
      Some
        (mk_unary ~block:(Ecx.get_current_block ~ecx) ~op:Not ~arg:(Option.get equals_result_val))
  (*
   * ============================
   *       Binary Operations
   * ============================
   *)
  | BinaryOperation { loc = _; op; left; right } ->
    let open BinaryOperation in
    let left_val = emit_numeric_expression ~ecx left in
    let right_val = emit_numeric_expression ~ecx right in
    let mk_binary op =
      mk_binary ~block:(Ecx.get_current_block ~ecx) ~op ~left:left_val ~right:right_val
    in
    let mk_cmp cmp =
      mk_cmp ~block:(Ecx.get_current_block ~ecx) ~cmp ~left:left_val ~right:right_val
    in
    let instr =
      match op with
      | Add -> mk_binary Add
      | Subtract -> mk_binary Sub
      | Multiply -> mk_binary Mul
      | Divide -> mk_binary Div
      | Remainder -> mk_binary Rem
      | BitwiseAnd -> mk_binary And
      | BitwiseOr -> mk_binary Or
      | BitwiseXor -> mk_binary Xor
      | LeftShift -> mk_binary Shl
      | ArithmeticRightShift -> mk_binary Shr
      | LogicalRightShift -> mk_binary Shrl
      | LessThan -> mk_cmp Lt
      | GreaterThan -> mk_cmp Gt
      | LessThanOrEqual -> mk_cmp LtEq
      | GreaterThanOrEqual -> mk_cmp GtEq
      | Equal
      | NotEqual ->
        failwith "Handled separately"
    in
    Some instr
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
    (match Ecx.instantiate_tuple ~ecx element_tys with
    | None -> None
    | Some agg ->
      let mk_elements = List.map (fun element _ -> emit_expression ~ecx element) elements in
      Some (emit_construct_tuple ~ecx ~agg ~mk_elements))
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
    let arg_vals = List.filter_map (emit_expression ~ecx) args in
    let method_ty = type_of_loc ~ecx method_loc in
    let { Types.Function.type_args = method_instance_type_args; _ } =
      Type_util.cast_to_function_type method_ty
    in
    let ret_type = mir_type_of_loc ~ecx loc in

    (* Determine if this is a super call *)
    let method_use = Type_context.get_method_use ~cx:ecx.pcx.type_ctx method_name.loc in
    if method_use.is_super_call then
      emit_super_method_call
        ~ecx
        ~method_name:method_name.name
        ~method_sig:method_use.method_sig
        ~receiver_val
        ~receiver_ty
        ~arg_vals
        ~method_instance_type_args
        ~ret_type
    else
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
    let func_id_opt =
      match func with
      | Identifier id -> Some (id.loc, id)
      | NamedAccess { loc; name; _ }
        when Type_context.is_scope_named_access ~cx:ecx.pcx.type_ctx loc ->
        Some (loc, name)
      | _ -> None
    in
    let ctor_result_opt =
      match func_id_opt with
      | Some (_, { Identifier.loc = ctor_id_loc; name }) ->
        let binding = Type_context.get_value_binding ~cx:ecx.pcx.type_ctx ctor_id_loc in
        (match binding.declaration with
        | CtorDecl _ ->
          let ty = type_of_loc ~ecx loc in
          let mk_elements = List.map (fun arg _ -> emit_expression ~ecx arg) args in
          Some (emit_construct_tuple_variant ~ecx ~name ~ty ~mk_elements)
        | _ -> None)
      | None -> None
    in
    (* If not a tuple constructor, must be a call *)
    (match ctor_result_opt with
    | Some result -> result
    | None ->
      (* First check if this is a statically known function call *)
      let static_func_val_opt =
        match func_id_opt with
        | Some (loc, { Identifier.loc = id_loc; _ }) ->
          let binding = Type_context.get_value_binding ~cx:ecx.pcx.type_ctx id_loc in
          (match binding.declaration with
          | FunDecl { Bindings.FunctionDeclaration.type_params; is_builtin; _ } ->
            let func_name = mk_value_binding_name binding in
            let ty = type_of_loc ~ecx loc in
            let func_val =
              if is_builtin then
                mk_myte_builtin_lit func_name
              else if type_params = [] then
                Ecx.get_nongeneric_function_value ~ecx func_name
              else
                let { Types.Function.type_args; _ } = Type_util.cast_to_function_type ty in
                Ecx.get_generic_function_value ~ecx func_name type_params type_args
            in
            Some func_val
          | _ -> None)
        | None -> None
      in
      (* Otherwise this is a dynamic call on a closure object *)
      let (func_val, env_opt) =
        match static_func_val_opt with
        | Some static_func_val -> (static_func_val, None)
        | None ->
          (* Load the function from the closure *)
          let closure_ptr = emit_expression ~ecx func |> Option.get in
          let closure_agg = cast_to_aggregate_type (pointer_value_element_type closure_ptr) in
          let func_ptr = emit_cast_ptr ~ecx ~element_type:Type.Function ~ptr:closure_ptr in
          let func_val = mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:func_ptr in

          (* Load the environment from the closure *)
          let (element_ty, element_idx) = lookup_element closure_agg "env" in
          let env_ptr =
            mk_get_pointer_instr
              ~block:(Ecx.get_current_block ~ecx)
              ~type_:element_ty
              ~ptr:closure_ptr
              ~offsets:[Instruction.GetPointer.FieldIndex element_idx]
              ()
          in
          let env_val = mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:env_ptr in

          (func_val, Some env_val)
      in
      (* Add env to end of arguments list if this is a dynamic call *)
      let arg_vals =
        let arg_vals = List.filter_map (emit_expression ~ecx) args in
        match env_opt with
        | None -> arg_vals
        | Some env_val -> arg_vals @ [env_val]
      in
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
      | Identifier { name; _ } -> name
      | NamedAccess { loc; name = { name; _ }; _ }
        when Type_context.is_scope_named_access ~cx:ecx.pcx.type_ctx loc ->
        name
      | _ -> failwith "Record name must be identifier"
    in
    (* Find MIR ADT layout for this constructor *)
    let adt = type_of_loc ~ecx loc in
    let (type_args, adt_sig) = Type_util.cast_to_adt_type adt in
    let layout = Ecx.get_mir_adt_layout ~ecx adt_sig type_args in
    (match layout with
    (* If layout is an aggregate, construct record *)
    | Aggregate agg ->
      let ptr =
        mk_call_builtin
          ~block:(Ecx.get_current_block ~ecx)
          Mir_builtin.myte_alloc
          [mk_int_lit_of_int32 Int32.one]
          [Aggregate agg]
      in
      emit_store_record_fields ~ecx ~ptr ~tag:None agg fields;
      Some ptr
    (* If layout is a variant, construct record and set variant tag *)
    | Variants { tags; union; variants; _ } ->
      (* Call myte_alloc builtin to allocate space for variant type *)
      let union_ptr =
        mk_call_builtin
          ~block:(Ecx.get_current_block ~ecx)
          Mir_builtin.myte_alloc
          [mk_int_lit_of_int32 Int32.one]
          [Aggregate union]
      in
      (* Cast to variant type and store all fields *)
      let record_variant_agg = SMap.find name variants in
      let variant_ptr =
        emit_cast_ptr ~ecx ~ptr:union_ptr ~element_type:(Type.Aggregate record_variant_agg)
      in
      let tag = SMap.find name tags in
      emit_store_record_fields ~ecx ~ptr:variant_ptr ~tag:(Some tag) record_variant_agg fields;
      Some union_ptr
    (* If layout is zero size, still emit all fields as they may have side effect s*)
    | ZeroSize ->
      List.iter (fun field -> ignore (emit_record_field_value ~ecx field)) fields;
      None
    (* If layout is an inlined value with niche then value is either the niche or it is the only
       fields that has a non-zero size (of which there must be exactly one). *)
    | InlineValueWithNiche inline_niche_layout ->
      let field_values = List.map (emit_record_field_value ~ecx) fields in
      emit_construct_inline_niche_value ~ecx inline_niche_layout name field_values
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
      let target_val = emit_expression ~ecx target |> Option.get in
      let index_val = emit_expression ~ecx index |> Option.get in
      emit_call_vec_get ~ecx loc target_val index_val
    (* If indexing a map, call Map's `get` method instead of emitting access chain *)
    | ADT { adt_sig; type_args } when adt_sig == !Std_lib.map_adt_sig ->
      let target_val = emit_expression ~ecx target |> Option.get in
      let index_val = emit_expression ~ecx index in
      Some (emit_call_map_get ~ecx loc type_args target_val index_val)
    | _ -> emit_expression_access_chain_load ~ecx expr)
  | NamedAccess _ -> emit_expression_access_chain_load ~ecx expr
  (*
   * ============================
   *     String Interpolation
   * ============================
   *)
  | InterpolatedString { loc; parts } ->
    let string_ty = type_of_loc ~ecx loc in
    let string_type = type_to_mir_type ~ecx string_ty in

    let (string_val, other_parts) =
      match List_utils.split_first parts with
      (* Use first part as string if first part is a string literal (which will be newly created) *)
      | (String { loc; value }, rest_parts) -> (emit_string_literal ~ecx loc value, rest_parts)
      (* Otherwise create empty new string to add to, as we do not want to modify first part *)
      | (first_part, rest_parts) -> (emit_string_literal ~ecx loc "", first_part :: rest_parts)
    in

    (* Emit parts and call `append` on the new string *)
    List.iter
      (fun part ->
        match part with
        (* String parts are stored as immutable strings and will be appended to new string *)
        | InterpolatedString.String { value; _ } ->
          let { Ecx.ImmutableString.value_global_val; size_global_val } =
            Ecx.add_immutable_string_literal ~ecx value
          in
          let size_val = mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:size_global_val in
          ignore
            (emit_method_call
               ~ecx
               ~method_name:"appendImmutable"
               ~receiver_val:(Some string_val)
               ~receiver_ty:string_ty
               ~arg_vals:[value_global_val; size_val]
               ~method_instance_type_args:[]
               ~ret_type:None)
        (* Expression parts have their `toString` method called then are appended to new string *)
        | Expression expr ->
          let receiver_val = emit_expression ~ecx expr in
          let receiver_ty = type_of_loc ~ecx (Ast_utils.expression_loc expr) in
          let part_val =
            emit_method_call
              ~ecx
              ~method_name:"toString"
              ~receiver_val
              ~receiver_ty
              ~arg_vals:[]
              ~method_instance_type_args:[]
              ~ret_type:string_type
            |> Option.get
          in
          ignore
            (emit_method_call
               ~ecx
               ~method_name:"append"
               ~receiver_val:(Some string_val)
               ~receiver_ty:string_ty
               ~arg_vals:[part_val]
               ~method_instance_type_args:[]
               ~ret_type:None))
      other_parts;
    Some string_val
  (*
   * ============================
   *       If Expression
   * ============================
   *)
  | If if_ -> emit_if_expression ~ecx if_
  (*
   * ============================
   *       Match Expression
   * ============================
   *)
  | Match match_ -> emit_match_expression ~ecx match_
  (*
   * ============================
   *       Vec Literal
   * ============================
   *)
  | VecLiteral { loc; elements } ->
    (* Find MIR element type. A type is needed for the alloc call, so use zero size type placeholder
       if vec is paramterized by zero size type. *)
    let (vec_type_args, _) = Type_util.cast_to_adt_type (type_of_loc ~ecx loc) in
    let element_ty = List.hd vec_type_args in
    let element_mir_type =
      match type_to_mir_type ~ecx element_ty with
      | Some mir_type -> mir_type
      | None -> Ecx.get_zero_size_type ~ecx
    in

    (* Do not allocate if vec is empty *)
    let size_val = mk_int_lit (List.length elements) in
    let data_val =
      if elements = [] then
        mk_null_ptr_lit element_mir_type
      else
        (* Call myte_alloc builtin to allocate space for elements *)
        let data_ptr_val =
          mk_call_builtin
            ~block:(Ecx.get_current_block ~ecx)
            Mir_builtin.myte_alloc
            [size_val]
            [element_mir_type]
        in

        (* Generate each element and store into array of elements *)
        List.iteri
          (fun i element ->
            match emit_expression ~ecx element with
            | Some value ->
              let ptr =
                mk_get_pointer_instr
                  ~block:(Ecx.get_current_block ~ecx)
                  ~type_:element_mir_type
                  ~ptr:data_ptr_val
                  ~offsets:[Instruction.GetPointer.PointerIndex (mk_int_lit i)]
                  ()
              in
              mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr ~value
            | None -> ())
          elements;
        data_ptr_val
    in

    (* Call myte_alloc builtin to allocate space for vec *)
    let vec_ptr_mir_type = mir_type_of_loc ~ecx loc |> Option.get in
    let vec_mir_type = cast_to_pointer_type vec_ptr_mir_type in
    let vec_ptr =
      mk_call_builtin
        ~block:(Ecx.get_current_block ~ecx)
        Mir_builtin.myte_alloc
        [mk_int_lit_of_int32 Int32.one]
        [vec_mir_type]
    in

    (* Write all vec literal fields *)
    let vec_agg = cast_to_aggregate_type vec_mir_type in
    let emit_field_store name value =
      let (element_ty, element_idx) = lookup_element vec_agg name in
      let ptr =
        mk_get_pointer_instr
          ~block:(Ecx.get_current_block ~ecx)
          ~type_:element_ty
          ~ptr:vec_ptr
          ~offsets:[Instruction.GetPointer.FieldIndex element_idx]
          ()
      in
      mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr ~value
    in
    emit_field_store "data" data_val;
    emit_field_store "size" size_val;
    emit_field_store "capacity" size_val;
    Some vec_ptr
  (*
   * ============================
   *         Map Literal
   * ============================
   *)
  | MapLiteral { loc; entries } ->
    (* Create new map by calling `Map.new` *)
    let map_ty = type_of_loc ~ecx loc in
    let map_mir_type = type_to_mir_type ~ecx map_ty |> Option.get in
    let (map_type_args, _) = Type_util.cast_to_adt_type map_ty in
    let map_ptr_val = emit_call_map_new ~ecx map_mir_type map_type_args in

    (* Reserve capacity for all entries in map literal by calling map's `reserve` method *)
    (if List.length entries > 1 then
      let capacity_val = mk_int_lit (List.length entries) in
      emit_call_map_reserve ~ecx map_type_args map_ptr_val capacity_val);

    (* Add all entries to map by calling map's `add` method *)
    List.iter
      (fun { MapLiteral.Entry.key; value; _ } ->
        let key_val = emit_expression ~ecx key in
        let value_val = emit_expression ~ecx value in
        emit_call_map_add ~ecx map_type_args map_ptr_val key_val value_val)
      entries;
    Some map_ptr_val
  (*
   * ============================
   *         Set Literal
   * ============================
   *)
  | SetLiteral { loc; elements } ->
    (* Create new set by calling `Set.new` *)
    let set_ty = type_of_loc ~ecx loc in
    let set_mir_type = type_to_mir_type ~ecx set_ty |> Option.get in
    let (set_type_args, _) = Type_util.cast_to_adt_type set_ty in
    let set_ptr_val = emit_call_set_new ~ecx set_mir_type set_type_args in

    (* Reserve capacity for all entries in set literal by calling set's `reserve` method *)
    (if List.length elements > 1 then
      let capacity_val = mk_int_lit (List.length elements) in
      emit_call_set_reserve ~ecx set_type_args set_ptr_val capacity_val);

    (* Add all entries to set by calling set's `add` method *)
    List.iter
      (fun element ->
        let element_val = emit_expression ~ecx element in
        emit_call_set_add ~ecx set_type_args set_ptr_val element_val)
      elements;
    Some set_ptr_val
  (*
   * ============================
   *           Unwrap
   * ============================
   *)
  | Unwrap { loc = _; operand } ->
    (* Emit operand, find its type, and the return type of function (which may differ) *)
    let operand_val = emit_expression ~ecx operand |> Option.get in
    let operand_ty = type_of_loc ~ecx (Ast_utils.expression_loc operand) in
    let return_ty = ecx.current_func_context.return_ty in

    (match operand_ty with
    | ADT { adt_sig; type_args = [item_ty] } when adt_sig == !Std_lib.option_adt_sig ->
      (* Destructure option, jumping to None branch if option is None *)
      let some_branch_block = Ecx.mk_block ~ecx in
      let none_branch_block = Ecx.mk_block ~ecx in
      let item_val =
        emit_option_destructuring
          ~ecx
          ~option_val:operand_val
          ~item_ty
          ~some_branch_block
          ~none_branch_block
      in

      (* None branch creates and returns new None value (as it may have different size) *)
      Ecx.set_current_block ~ecx none_branch_block;
      let none_val = emit_construct_enum_variant ~ecx ~name:"None" ~ty:return_ty in
      let return_pointer = Option.get ecx.current_func_context.return_pointer in
      mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:return_pointer ~value:none_val;
      Ecx.finish_block_continue ~ecx ecx.current_func_context.return_block;

      (* Return unwrapped value in Some case and proceed *)
      Ecx.set_current_block ~ecx some_branch_block;
      item_val
    | ADT { adt_sig; type_args = [ok_ty; error_ty] } when adt_sig == !Std_lib.result_adt_sig ->
      (* Destructure result, jumping to Error branch if result is Error *)
      let ok_branch_block = Ecx.mk_block ~ecx in
      let error_branch_block = Ecx.mk_block ~ecx in
      let (ok_item_val, error_item_val) =
        emit_result_destructuring
          ~ecx
          ~result_val:operand_val
          ~ok_ty
          ~error_ty
          ~ok_branch_block
          ~error_branch_block
      in

      (* Error branch creates and returns new Error value (as it may have different size) *)
      Ecx.set_current_block ~ecx error_branch_block;
      let error_val =
        emit_construct_tuple_variant
          ~ecx
          ~name:"Error"
          ~ty:return_ty
          ~mk_elements:[(fun _ -> error_item_val)]
      in
      let return_pointer = Option.get ecx.current_func_context.return_pointer in
      mk_store_
        ~block:(Ecx.get_current_block ~ecx)
        ~ptr:return_pointer
        ~value:(Option.get error_val);
      Ecx.finish_block_continue ~ecx ecx.current_func_context.return_block;

      (* Return unwrapped value in Ok case and proceed *)
      Ecx.set_current_block ~ecx ok_branch_block;
      ok_item_val
    | _ -> failwith "Only option and result types can be unwrapped")
  (*
   * ============================
   *      Anonymous Function
   * ============================
   *)
  | AnonymousFunction anon_func_node ->
    let anon_func_num = ecx.current_func_context.num_anonymous_functions in
    ecx.current_func_context.num_anonymous_functions <- anon_func_num + 1;
    let anon_func_name = Printf.sprintf "%s:%d" ecx.current_func.name anon_func_num in

    (* Find captured bindings and create type of environment, if one exists *)
    let captures =
      Type_context.get_anonymous_function_captures ~cx:ecx.pcx.type_ctx anon_func_node.loc
    in
    let (env_agg_elements, name_to_binding) =
      Bindings.LBVMMap.VSet.fold
        (fun binding (env_agg_elements, name_to_binding) ->
          (* Find binding MIR type, adding element if not zero sized *)
          let binding_mir_type =
            match binding.declaration with
            | VarDecl { tvar; _ }
            | FunParamDecl { tvar }
            | MatchCaseVarDecl { tvar }
            | ThisDecl { tvar } ->
              type_to_mir_type ~ecx (TVar tvar)
            | FunDecl _
            | CtorDecl _ ->
              failwith "Cannot be captured"
          in
          let env_agg_elements =
            match binding_mir_type with
            | None -> env_agg_elements
            | Some mir_type ->
              (* Captured mutable variables are behind a pointer *)
              let mir_type =
                if Bindings.is_mutable_variable binding then
                  Type.Pointer mir_type
                else
                  mir_type
              in
              (binding.name, mir_type) :: env_agg_elements
          in
          let name_to_binding = SMap.add binding.name binding name_to_binding in
          (env_agg_elements, name_to_binding))
        captures
        ([], SMap.empty)
    in
    let env_agg_opt =
      if env_agg_elements = [] then
        None
      else
        (* Pack and align captured bindings *)
        let env_agg_elements =
          env_agg_elements
          |> Mir_adt_layout.order_elements_by_alignment
          |> Mir_adt_layout.align_and_pad_aggregate_elements
          |> fst
        in
        let env_agg_label = anon_func_name ^ ":env" in
        let env_agg = Ecx.mk_aggregate ~ecx env_agg_label anon_func_node.loc env_agg_elements in
        Some env_agg
    in

    let func_val =
      Ecx.get_anonymous_function_value ~ecx anon_func_name anon_func_node env_agg_opt captures
    in

    (* Allocate closure *)
    let closure_type = Ecx.get_closure_type ~ecx in
    let closure_agg = cast_to_aggregate_type closure_type in
    let closure_ptr =
      mk_call_builtin
        ~block:(Ecx.get_current_block ~ecx)
        Mir_builtin.myte_alloc
        [mk_int_lit_of_int32 Int32.one]
        [closure_type]
    in

    (* Store function pointer *)
    let func_ptr = emit_cast_ptr ~ecx ~element_type:Function ~ptr:closure_ptr in
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:func_ptr ~value:func_val;

    let env_ptr_value =
      match env_agg_opt with
      (* If there are no captures environment is null pointer *)
      | None -> mk_null_ptr_lit Byte
      (* If there are captures allocate environment and fill captures *)
      | Some env_agg ->
        let env_ptr =
          mk_call_builtin
            ~block:(Ecx.get_current_block ~ecx)
            Mir_builtin.myte_alloc
            [mk_int_lit_of_int32 Int32.one]
            [Aggregate env_agg]
        in

        List.iteri
          (fun element_idx (element_name, element_ty) ->
            match SMap.find_opt element_name name_to_binding with
            | None -> ()
            | Some binding ->
              let element_value =
                (* Mutable variables are pointers to a value. If already captured then copy pointer
                   from current environment. Otherwise use local pointer without loading value from it. *)
                if Bindings.is_mutable_variable binding then
                  if Ecx.is_captured_binding ~ecx binding then
                    emit_environment_element ~ecx binding |> Option.get
                  else
                    let mir_type = mir_type_of_loc ~ecx binding.loc |> Option.get in
                    Ecx.get_local_ptr_def_instr ~ecx binding.loc mir_type
                else
                  emit_variable_binding_value ~ecx binding |> Option.get
              in
              let element_ptr =
                mk_get_pointer_instr
                  ~block:(Ecx.get_current_block ~ecx)
                  ~type_:element_ty
                  ~ptr:env_ptr
                  ~offsets:[Instruction.GetPointer.FieldIndex element_idx]
                  ()
              in
              mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:element_ptr ~value:element_value)
          env_agg.elements;

        (* Cast to env type defined in closure agg before storing *)
        emit_cast_ptr ~ecx ~element_type:Byte ~ptr:env_ptr
    in

    (* Store environment in closure *)
    let (element_ty, element_idx) = lookup_element closure_agg "env" in
    let env_ptr =
      mk_get_pointer_instr
        ~block:(Ecx.get_current_block ~ecx)
        ~type_:element_ty
        ~ptr:closure_ptr
        ~offsets:[Instruction.GetPointer.FieldIndex element_idx]
        ()
    in
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:env_ptr ~value:env_ptr_value;

    Some closure_ptr

and emit_variable_binding_value ~ecx binding =
  (* First check if this is a captured value *)
  if Ecx.is_captured_binding ~ecx binding then
    (* Load captured binding from environment *)
    match emit_environment_element ~ecx binding with
    | None -> None
    | Some element_value ->
      (* Mutable variables are behind a pointer in the environment *)
      if Bindings.is_mutable_variable binding then
        Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:element_value)
      else
        Some element_value
  else
    match binding.declaration with
    (* Variables may be either globals or locals *)
    | VarDecl { tvar; _ } ->
      (match type_to_mir_type ~ecx (Types.Type.TVar tvar) with
      | None -> None
      | Some mir_type ->
        let ptr =
          if Bindings.is_module_decl binding then
            Ecx.get_global_pointer ~ecx binding |> Option.get
          else
            Ecx.get_local_ptr_def_instr ~ecx binding.loc mir_type
        in
        Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr))
    (* Function parameters can have their corresponding MIR value referenced directly *)
    | FunParamDecl { tvar }
    | ThisDecl { tvar } ->
      (match type_to_mir_type ~ecx (Types.Type.TVar tvar) with
      | None -> None
      | Some _ -> Some (Ecx.get_function_argument_value ~ecx binding))
    (* Match cases variables are locals, and must be loaded from their StackAlloc *)
    | MatchCaseVarDecl { tvar } ->
      (match type_to_mir_type ~ecx (Types.Type.TVar tvar) with
      | None -> None
      | Some mir_type ->
        let local_ptr = Ecx.get_local_ptr_def_instr ~ecx binding.loc mir_type in
        Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:local_ptr))
    | FunDecl _
    | CtorDecl _ ->
      failwith "Expected variable binding"

(* Emit a captured value in the current environment. For captured mutable variables this is a
   pointer to the value. *)
and emit_environment_element ~ecx binding =
  match ecx.current_func_context.env_agg with
  (* Environment may not exist or may not contain captured binding if it is zero size *)
  | None -> None
  | Some env_agg ->
    (match lookup_element_opt env_agg binding.name with
    | None -> None
    (* Get pointer to captured binding in environment *)
    | Some (element_ty, element_idx) ->
      let element_ptr =
        mk_get_pointer_instr
          ~block:(Ecx.get_current_block ~ecx)
          ~type_:element_ty
          ~ptr:(Option.get ecx.current_func_context.env_ptr)
          ~offsets:[Instruction.GetPointer.FieldIndex element_idx]
          ()
      in
      Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:element_ptr))

and emit_string_literal ~ecx loc value =
  (* Add string global string literal unless this is the empty string, in which case use null pointer *)
  let string_length = String.length value in
  let (string_global_ptr, needs_cast_to_byte_ptr) =
    if string_length <> 0 then
      (Ecx.add_mutable_string_literal ~ecx loc value, true)
    else
      (mk_null_ptr_lit Byte, false)
  in
  let string_pointer_type = mir_type_of_loc ~ecx loc |> Option.get in
  let string_type = cast_to_pointer_type string_pointer_type in
  let string_agg = cast_to_aggregate_type string_type in
  (* Call myte_alloc builtin to allocate space for string *)
  let agg_ptr_val =
    mk_call_builtin
      ~block:(Ecx.get_current_block ~ecx)
      Mir_builtin.myte_alloc
      [mk_int_lit_of_int32 Int32.one]
      [string_type]
  in
  (* Write all string literal fields *)
  let emit_field_store name value =
    let (element_ty, element_idx) = lookup_element string_agg name in
    let ptr =
      mk_get_pointer_instr
        ~block:(Ecx.get_current_block ~ecx)
        ~type_:element_ty
        ~ptr:agg_ptr_val
        ~offsets:[Instruction.GetPointer.FieldIndex element_idx]
        ()
    in
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr ~value
  in
  let length = mk_int_lit string_length in
  let string_byte_ptr =
    if needs_cast_to_byte_ptr then
      emit_cast_ptr ~ecx ~ptr:string_global_ptr ~element_type:Byte
    else
      string_global_ptr
  in
  emit_field_store "data" string_byte_ptr;
  emit_field_store "size" length;
  emit_field_store "capacity" length;
  agg_ptr_val

and emit_if_expression ~ecx if_ =
  let { If.loc; test; conseq; altern } = if_ in
  let mir_type = mir_type_of_loc ~ecx loc in

  (* Model if join by creating stack location to place results of branches as long as type is not
     zero sized. *)
  let result_ptr_val =
    match mir_type with
    | None -> None
    | Some type_ -> Some (mk_stack_alloc ~block:(Ecx.get_current_block ~ecx) ~type_)
  in

  (* Branch to conseq or altern blocks *)
  let test_val = emit_bool_expression ~ecx test in
  let conseq_block = Ecx.mk_block ~ecx in
  let altern_block = Ecx.mk_block ~ecx in
  let join_block = Ecx.mk_block ~ecx in
  Ecx.finish_block_branch ~ecx test_val conseq_block altern_block;

  (* Emit conseq, store result, and continue to join block *)
  Ecx.set_current_block ~ecx conseq_block;
  (match emit_block ~ecx ~is_expr:true conseq with
  | Some conseq_val ->
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:(Option.get result_ptr_val) ~value:conseq_val
  | None -> ());
  Ecx.finish_block_continue ~ecx join_block;

  (* Emit altern, store result, and continue to join block *)
  Ecx.set_current_block ~ecx altern_block;
  let altern_val =
    match altern with
    | Block block -> emit_block ~ecx ~is_expr:true block
    | If if_ -> emit_if_expression ~ecx if_
    | None -> failwith "If expression must have else branch"
  in
  (match altern_val with
  | Some altern_val ->
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:(Option.get result_ptr_val) ~value:altern_val
  | None -> ());
  Ecx.finish_block_continue ~ecx join_block;

  (* Join branches together and load result from stack location *)
  Ecx.set_current_block ~ecx join_block;
  match mir_type with
  | None -> None
  | Some _ -> Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:(Option.get result_ptr_val))

and emit_match_expression ~ecx match_ =
  let { Match.loc; args; cases } = match_ in
  let mir_type = mir_type_of_loc ~ecx loc in

  (* Model join of match expression by creating stack location to place results of cases *)
  let result_ptr_val =
    match mir_type with
    | None -> None
    | Some type_ -> Some (mk_stack_alloc ~block:(Ecx.get_current_block ~ecx) ~type_)
  in

  (* Emit args and decision tree *)
  let args = List.map (emit_expression ~ecx) args in
  let decision_tree = Mir_match_decision_tree.build_match_decision_tree ~ecx args cases in
  let join_block = Ecx.mk_block ~ecx in
  emit_match_decision_tree ~ecx ~join_block ~result_ptr:result_ptr_val ~alloc:true decision_tree;

  (* Join branches together and load result from stack location *)
  Ecx.set_current_block ~ecx join_block;
  match mir_type with
  | None -> None
  | Some _ -> Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:(Option.get result_ptr_val))

and emit_trait_object_promotion ~ecx expr_val trait_object_layout trait_object_instance =
  (* Call myte_alloc builtin to allocate space for trait object *)
  let trait_object_type = Type.Aggregate trait_object_instance.agg in
  let trait_object_ptr_val =
    mk_call_builtin
      ~block:(Ecx.get_current_block ~ecx)
      Mir_builtin.myte_alloc
      [mk_int_lit_of_int32 Int32.one]
      [trait_object_type]
  in
  (* Write trait object fields *)
  let emit_field_store name value =
    let (element_ty, element_idx) = lookup_element trait_object_instance.agg name in
    let ptr =
      mk_get_pointer_instr
        ~block:(Ecx.get_current_block ~ecx)
        ~type_:element_ty
        ~ptr:trait_object_ptr_val
        ~offsets:[Instruction.GetPointer.FieldIndex element_idx]
        ()
    in
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr ~value
  in
  (match expr_val with
  | None -> ()
  (* Force non-pointer types to be allocated on heap so that pointer can be stored in trait object *)
  | Some item_val when trait_object_instance.is_boxed ->
    let item_type = type_of_value item_val in
    let item_ptr =
      mk_call_builtin
        Mir_builtin.myte_alloc
        ~block:(Ecx.get_current_block ~ecx)
        [mk_int_lit_of_int32 Int32.one]
        [item_type]
    in
    mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:item_ptr ~value:item_val;
    emit_field_store "item" item_ptr
  | Some item_val -> emit_field_store "item" item_val);
  emit_field_store "vtable" trait_object_instance.vtable;

  (* Cast to general trait object *)
  let trait_object_general_type = Type.Pointer (Aggregate trait_object_layout.trait_object_agg) in
  Some
    (mk_cast
       ~block:(Ecx.get_current_block ~ecx)
       ~arg:trait_object_ptr_val
       ~type_:trait_object_general_type)

and emit_expression_access_chain_load ~ecx expr =
  match emit_expression_access_chain ~ecx expr with
  | None -> None
  | Some (GetPointerEmittedResult element_pointer_val) ->
    Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:element_pointer_val)
  | Some (InlinedValueResult value) -> Some value

and emit_expression_access_chain ~ecx expr : access_chain_result option =
  let open Expression in
  let open Instruction in
  let emit_get_pointer_instr target_ptr_val element_mir_ty offsets =
    let (pointer_offset, offsets) =
      match List.rev offsets with
      | GetPointer.PointerIndex pointer_idx :: rest_offsets -> (Some pointer_idx, rest_offsets)
      | offsets -> (None, offsets)
    in
    let element_pointer_val =
      mk_get_pointer_instr
        ~block:(Ecx.get_current_block ~ecx)
        ~pointer_offset
        ~type_:element_mir_ty
        ~ptr:target_ptr_val
        ~offsets
        ()
    in
    element_pointer_val
  in
  let maybe_emit_get_pointer_and_load_instrs root_val ty offsets =
    if offsets = [] then
      root_val
    else
      match (root_val, Ecx.to_mir_type ~ecx ty) with
      | (Some root_val, Some mir_type) ->
        let element_ptr_val = emit_get_pointer_instr root_val mir_type offsets in
        Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:element_ptr_val)
      | _ -> None
  in
  let rec emit_access_expression expr =
    match expr with
    | IndexedAccess ({ target; _ } as access) ->
      let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
      (match target_ty with
      | Types.Type.Tuple _ -> emit_tuple_indexed_access access
      | ADT { adt_sig; _ } when adt_sig == !Std_lib.vec_adt_sig -> emit_vec_indexed_access access
      | ADT { adt_sig; _ } when adt_sig == !Std_lib.array_adt_sig ->
        emit_array_indexed_access access
      | ADT { adt_sig = { variants; _ }; _ } when SMap.cardinal variants = 1 ->
        emit_tuple_indexed_access access
      | _ -> failwith "Target must be a tuple to pass type checking")
    | NamedAccess ({ loc; target; _ } as access)
      when not (Type_context.is_scope_named_access ~cx:ecx.pcx.type_ctx loc) ->
      let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
      (match target_ty with
      | Types.Type.ADT { adt_sig = { variants; _ }; _ } when SMap.cardinal variants = 1 ->
        emit_record_named_access access
      | _ -> failwith "Target must be a record to pass type checking")
    | _ -> (emit_expression ~ecx expr, [])
  and emit_tuple_indexed_access { IndexedAccess.target; index; _ } =
    let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
    let (target_root_val, target_offsets) = emit_access_expression target in
    let root_val =
      maybe_emit_get_pointer_and_load_instrs target_root_val target_ty target_offsets
    in
    (* Extract tuple element index from integer literal and add to GetPointer offsets *)
    let emit_get_pointer_index agg =
      match index with
      | IntLiteral { IntLiteral.raw; base; _ } ->
        let tuple_index = Integers.int64_of_string_opt raw base |> Option.get |> Int64.to_int in
        (match lookup_element_opt agg (TupleKeyCache.get_key tuple_index) with
        | Some (_, element_index) -> (root_val, [GetPointer.FieldIndex element_index])
        | None -> (None, []))
      | _ -> failwith "Index of a tuple must be an int literal to pass type checking"
    in
    match target_ty with
    | Tuple elements ->
      (match Ecx.instantiate_tuple ~ecx elements with
      | Some agg -> emit_get_pointer_index agg
      | None -> (None, []))
    (* If layout is aggregate, index like normal tuple, otherwise is single element tuple so do
       not need to index at all. *)
    | ADT { adt_sig; type_args } ->
      let layout = Ecx.get_mir_adt_layout ~ecx adt_sig type_args in
      (match layout with
      | Aggregate agg -> emit_get_pointer_index agg
      | InlineValue _ -> (root_val, [])
      | ZeroSize -> (None, [])
      | Variants _
      | InlineValueWithNiche _
      | PureEnum _ ->
        failwith "Invalid layout for indexed access")
    | _ -> failwith "Indexed access must be on tuple or ADT type"
  and emit_array_indexed_access { IndexedAccess.target; index; _ } =
    let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
    let (target_root_val, target_offsets) = emit_access_expression target in
    let root_val =
      maybe_emit_get_pointer_and_load_instrs target_root_val target_ty target_offsets
    in
    let index_val = emit_integer_expression ~ecx index in
    (root_val, [GetPointer.PointerIndex index_val])
  (* An indexed access on a Vec calls the Vec's `get` method *)
  and emit_vec_indexed_access { IndexedAccess.loc; target; index; _ } =
    let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
    let (target_root_val, target_offsets) = emit_access_expression target in
    let target_val =
      maybe_emit_get_pointer_and_load_instrs target_root_val target_ty target_offsets |> Option.get
    in
    let index_val = emit_expression ~ecx index |> Option.get in
    let vec_get_result = emit_call_vec_get ~ecx loc target_val index_val in
    (vec_get_result, [])
  and emit_record_named_access { NamedAccess.target; name = { name; _ }; _ } =
    let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
    let (target_root_val, target_offsets) = emit_access_expression target in
    let root_val =
      maybe_emit_get_pointer_and_load_instrs target_root_val target_ty target_offsets
    in
    (* Find MIR ADT layout for this record *)
    let (type_args, adt_sig) = Type_util.cast_to_adt_type target_ty in
    let layout = Ecx.get_mir_adt_layout ~ecx adt_sig type_args in
    match layout with
    (* If layout is aggregate, index its field with a GetPointer offset *)
    | Aggregate agg ->
      (* Find element index in the corresponding aggregate type *)
      (match lookup_element_opt agg name with
      | Some (_, element_idx) -> (root_val, [GetPointer.FieldIndex element_idx])
      | None -> (None, []))
    | ZeroSize -> (None, [])
    | Variants _
    | InlineValueWithNiche _
    | PureEnum _
    | InlineValue _ ->
      failwith "Invalid layout for record"
  in
  let (target_val, offsets) =
    match expr with
    | IndexedAccess _
    | NamedAccess _ ->
      emit_access_expression expr
    | _ -> failwith "Must be called on access expression"
  in
  match target_val with
  | None -> None
  | Some target_val when offsets = [] -> Some (InlinedValueResult target_val)
  | Some target_val ->
    (match mir_type_of_loc ~ecx (Ast_utils.expression_loc expr) with
    | None -> None
    | Some element_mir_ty ->
      Some (GetPointerEmittedResult (emit_get_pointer_instr target_val element_mir_ty offsets)))

and emit_method_call
    ~ecx
    ~(method_name : string)
    ~(receiver_val : Value.t option)
    ~(receiver_ty : Types.Type.t)
    ~(arg_vals : Value.t list)
    ~(method_instance_type_args : Types.Type.t list)
    ~(ret_type : Type.t option) =
  match receiver_ty with
  (* Trait objects perform dynamic dispatch *)
  | TraitObject { trait_sig; _ } ->
    let trait_object_layout = Ecx.get_trait_object_layout ~ecx trait_sig in
    let trait_object_agg = trait_object_layout.trait_object_agg in
    let vtable_index = SMap.find method_name trait_object_layout.vtable_indices in

    (* Load item and vtable from trait object *)
    let emit_field_load name =
      let (element_ty, element_idx) = lookup_element trait_object_agg name in
      let ptr =
        mk_get_pointer_instr
          ~block:(Ecx.get_current_block ~ecx)
          ~type_:element_ty
          ~ptr:(Option.get receiver_val)
          ~offsets:[Instruction.GetPointer.FieldIndex element_idx]
          ()
      in
      mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr
    in
    let item_val = emit_field_load "item" in
    let vtable_val = emit_field_load "vtable" in

    (* Load function from vtable *)
    let func_ptr =
      mk_get_pointer_instr
        ~block:(Ecx.get_current_block ~ecx)
        ~type_:Function
        ~ptr:vtable_val
        ~offsets:[Instruction.GetPointer.PointerIndex (mk_int_lit vtable_index)]
        ()
    in
    let func_val = mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:func_ptr in
    emit_call ~ecx ~func_val ~arg_vals ~receiver_val:(Some item_val) ~ret_type
  (* All other receivers perform static dispatch *)
  | _ ->
    (* If receiver has zero size must still pass in some argument - use zero size global *)
    let receiver_val =
      match receiver_val with
      | Some receiver_val -> receiver_val
      | None -> Ecx.get_zero_size_global_pointer ~ecx
    in
    let func_val =
      Ecx.get_method_function_value
        ~ecx
        ~method_name
        ~super_method_sig:None
        ~receiver_ty
        ~method_instance_type_args
    in
    emit_call ~ecx ~func_val ~arg_vals ~receiver_val:(Some receiver_val) ~ret_type

and emit_super_method_call
    ~ecx
    ~(method_name : string)
    ~(method_sig : Types.MethodSig.t)
    ~(receiver_val : Value.t option)
    ~(receiver_ty : Types.Type.t)
    ~(arg_vals : Value.t list)
    ~(method_instance_type_args : Types.Type.t list)
    ~(ret_type : Type.t option) =
  let receiver_val =
    match receiver_val with
    | Some receiver_val -> receiver_val
    | None -> Ecx.get_zero_size_global_pointer ~ecx
  in
  let func_val =
    Ecx.get_method_function_value
      ~ecx
      ~method_name
      ~super_method_sig:(Some method_sig)
      ~receiver_ty
      ~method_instance_type_args
  in
  emit_call ~ecx ~func_val ~arg_vals ~receiver_val:(Some receiver_val) ~ret_type

and emit_call
    ~ecx
    ~(func_val : Value.t)
    ~(arg_vals : Value.t list)
    ~(receiver_val : Value.t option)
    ~(ret_type : Type.t option) =
  (* Pass `this` value as first argument if this is a method call *)
  let arg_vals =
    match receiver_val with
    | None -> arg_vals
    | Some receiver_val -> receiver_val :: arg_vals
  in
  let builtin_functions = Lazy.force_val builtin_functions in
  match func_val.value with
  (* Generate Myte builtin function *)
  | Lit (MyteBuiltin name) ->
    let mk_func = SMap.find name builtin_functions in
    mk_func ~ecx arg_vals ret_type
  (* Generate regular calls *)
  | _ ->
    (match ret_type with
    | None ->
      mk_call_ ~block:(Ecx.get_current_block ~ecx) ~func:func_val ~args:arg_vals ~return:None;
      None
    | Some ret_type ->
      Some
        (mk_call
           ~block:(Ecx.get_current_block ~ecx)
           ~func:func_val
           ~args:arg_vals
           ~return:(Some ret_type)))

and builtin_functions =
  lazy
    ([
       (Std_lib.std_bool_bool_equals, emit_eq);
       (Std_lib.std_byte_byte_equals, emit_eq);
       (Std_lib.std_byte_byte_toInt, emit_std_byte_byte_toInt);
       (Std_lib.std_byte_byte_toLong, emit_std_byte_byte_toLong);
       (Std_lib.std_gc_collect, emit_std_gc_collect);
       (Std_lib.std_gc_getHeapSize, emit_std_gc_getHeapSize);
       (Std_lib.std_int_int_equals, emit_eq);
       (Std_lib.std_int_int_toByte, emit_std_int_int_toByte);
       (Std_lib.std_int_int_toLong, emit_std_int_int_toLong);
       (Std_lib.std_long_long_equals, emit_eq);
       (Std_lib.std_long_long_toByte, emit_std_long_long_toByte);
       (Std_lib.std_long_long_toInt, emit_std_long_long_toInt);
       (Std_lib.std_memory_array_copy, emit_std_memory_array_copy);
       (Std_lib.std_memory_array_isNull, emit_std_memory_array_isNull);
       (Std_lib.std_memory_array_new, emit_std_memory_array_new);
       (Std_lib.std_io_file_builtin_close, emit_std_io_close);
       (Std_lib.std_io_file_builtin_open, emit_std_io_open);
       (Std_lib.std_io_file_builtin_read, emit_std_io_read);
       (Std_lib.std_io_file_builtin_unlink, emit_std_io_unlink);
       (Std_lib.std_io_file_builtin_write, emit_std_io_write);
       (Std_lib.std_sys_exit, emit_std_sys_exit);
     ]
    |> List.to_seq
    |> SMap.of_seq)

and emit_eq ~ecx arg_vals _ =
  match arg_vals with
  | [left; right] -> Some (mk_cmp ~block:(Ecx.get_current_block ~ecx) ~cmp:Eq ~left ~right)
  | _ -> failwith "Expected two arguments"

and emit_std_byte_byte_toInt ~ecx arg_vals _ =
  Some (mk_sext ~block:(Ecx.get_current_block ~ecx) ~arg:(List.hd arg_vals) ~type_:Int)

and emit_std_byte_byte_toLong ~ecx arg_vals _ =
  Some (mk_sext ~block:(Ecx.get_current_block ~ecx) ~arg:(List.hd arg_vals) ~type_:Long)

and emit_std_gc_collect ~ecx _ _ =
  mk_call_builtin_no_return_ ~block:(Ecx.get_current_block ~ecx) Mir_builtin.myte_collect [];
  None

and emit_std_gc_getHeapSize ~ecx _ _ =
  Some (mk_call_builtin ~block:(Ecx.get_current_block ~ecx) Mir_builtin.myte_get_heap_size [] [])

and emit_std_int_int_toByte ~ecx arg_vals _ =
  Some (mk_trunc ~block:(Ecx.get_current_block ~ecx) ~arg:(List.hd arg_vals) ~type_:Byte)

and emit_std_int_int_toLong ~ecx arg_vals _ =
  Some (mk_sext ~block:(Ecx.get_current_block ~ecx) ~arg:(List.hd arg_vals) ~type_:Long)

and emit_std_long_long_toByte ~ecx arg_vals _ =
  Some (mk_trunc ~block:(Ecx.get_current_block ~ecx) ~arg:(List.hd arg_vals) ~type_:Byte)

and emit_std_long_long_toInt ~ecx arg_vals _ =
  Some (mk_trunc ~block:(Ecx.get_current_block ~ecx) ~arg:(List.hd arg_vals) ~type_:Int)

and emit_std_memory_array_copy ~ecx arg_vals _ =
  match arg_vals with
  | [dest_array; dest_index; src_array; src_index; count] ->
    let dest_ptr = emit_offset_ptr ~ecx dest_array dest_index in
    let src_ptr = emit_offset_ptr ~ecx src_array src_index in
    mk_call_builtin_no_return_
      ~block:(Ecx.get_current_block ~ecx)
      Mir_builtin.myte_copy
      [dest_ptr; src_ptr; count];
    None
  | _ -> failwith "Array.copy expects five arguments"

and emit_std_memory_array_isNull ~ecx arg_vals _ =
  let arg = List.hd arg_vals in
  let element_type = pointer_value_element_type arg in
  Some
    (mk_cmp
       ~block:(Ecx.get_current_block ~ecx)
       ~cmp:Eq
       ~left:arg
       ~right:(mk_null_ptr_lit element_type))

and emit_std_memory_array_new ~ecx arg_vals ret_type =
  (* Type must be supplied so create explicit zero size type if necessary *)
  let element_ty =
    match ret_type with
    | Some ret_type ->
      let element_ty = cast_to_pointer_type ret_type in
      element_ty
    | None -> Ecx.get_zero_size_type ~ecx
  in
  match arg_vals with
  (* An allocation of 0 represents the null pointer *)
  | [{ value = Lit (Int 0l); _ }] -> Some (mk_null_ptr_lit element_ty)
  | _ ->
    Some
      (mk_call_builtin
         ~block:(Ecx.get_current_block ~ecx)
         Mir_builtin.myte_alloc
         arg_vals
         [element_ty])

and emit_std_io_write ~ecx arg_vals _ =
  let arg_vals =
    match arg_vals with
    | [file_val; buffer_val; offset_val; size_val] ->
      let ptr_val = emit_offset_ptr ~ecx buffer_val offset_val in
      [file_val; ptr_val; size_val]
    | _ -> failwith "Expected four arguments"
  in
  Some (mk_call_builtin ~block:(Ecx.get_current_block ~ecx) Mir_builtin.myte_write arg_vals [])

and emit_std_io_read ~ecx arg_vals _ =
  Some (mk_call_builtin ~block:(Ecx.get_current_block ~ecx) Mir_builtin.myte_read arg_vals [])

and emit_std_io_open ~ecx arg_vals _ =
  Some (mk_call_builtin ~block:(Ecx.get_current_block ~ecx) Mir_builtin.myte_open arg_vals [])

and emit_std_io_close ~ecx arg_vals _ =
  Some (mk_call_builtin ~block:(Ecx.get_current_block ~ecx) Mir_builtin.myte_close arg_vals [])

and emit_std_io_unlink ~ecx arg_vals _ =
  Some (mk_call_builtin ~block:(Ecx.get_current_block ~ecx) Mir_builtin.myte_unlink arg_vals [])

and emit_std_sys_exit ~ecx arg_vals _ =
  mk_call_builtin_no_return_ ~block:(Ecx.get_current_block ~ecx) Mir_builtin.myte_exit arg_vals;
  None

and emit_call_vec_get ~ecx return_loc vec_val index_val =
  (* Get full name for Vec's `get` method *)
  let vec_get_method_sig = Types.AdtSig.lookup_method !Std_lib.vec_adt_sig "get" |> Option.get in
  let vec_get_binding = Bindings.get_value_binding ecx.pcx.bindings vec_get_method_sig.loc in
  let vec_get_name = mk_value_binding_name vec_get_binding in
  (* Find element type and instantiate Vec's `get` method *)
  let element_ty = type_of_loc ~ecx return_loc in
  let func =
    Ecx.get_generic_function_value
      ~ecx
      vec_get_name
      vec_get_method_sig.trait_sig.type_params
      [element_ty]
  in
  (* Emit call to Vec's `get` method *)
  let emit_call return =
    mk_call ~block:(Ecx.get_current_block ~ecx) ~func ~args:[vec_val; index_val] ~return
  in
  match Ecx.to_mir_type ~ecx element_ty with
  | None ->
    ignore (emit_call None);
    None
  | Some ret_ty -> Some (emit_call (Some ret_ty))

and emit_call_vec_set ~ecx element_ty_loc vec_val index_val expr_val =
  (* Get full name for Vec's `set` method *)
  let vec_get_method_sig = Types.AdtSig.lookup_method !Std_lib.vec_adt_sig "set" |> Option.get in
  let vec_get_binding = Bindings.get_value_binding ecx.Ecx.pcx.bindings vec_get_method_sig.loc in
  let vec_get_name = mk_value_binding_name vec_get_binding in
  (* Find element type and instantiate Vec's `set` method *)
  let element_ty = type_of_loc ~ecx element_ty_loc in
  let func =
    Ecx.get_generic_function_value
      ~ecx
      vec_get_name
      vec_get_method_sig.trait_sig.type_params
      [element_ty]
  in
  (* Emit call of Vec's `set` method *)
  let args =
    match expr_val with
    | Some expr_val -> [vec_val; index_val; expr_val]
    | None -> [vec_val; index_val]
  in
  mk_call_ ~block:(Ecx.get_current_block ~ecx) ~func ~args ~return:None

and emit_call_map_get ~ecx return_loc map_type_args map_val index_val =
  (* Get full name for Map's `get` method *)
  let map_get_method_sig = Types.AdtSig.lookup_method !Std_lib.map_adt_sig "get" |> Option.get in
  let map_get_binding = Bindings.get_value_binding ecx.pcx.bindings map_get_method_sig.loc in
  let map_get_name = mk_value_binding_name map_get_binding in
  (* Find element type and instantiate Map's `get` method *)
  let return_ty = type_of_loc ~ecx return_loc in
  let func =
    Ecx.get_generic_function_value
      ~ecx
      map_get_name
      map_get_method_sig.trait_sig.type_params
      map_type_args
  in
  (* Emit call to Map's `get` method *)
  let mir_type = Ecx.to_mir_type ~ecx return_ty |> Option.get in
  let args =
    match index_val with
    | Some index_val -> [map_val; index_val]
    | None -> [map_val]
  in
  mk_call ~block:(Ecx.get_current_block ~ecx) ~func ~args ~return:(Some mir_type)

and emit_call_map_add ~ecx map_type_args map_val index_val expr_val =
  (* Get full name for Map's `add` method *)
  let map_add_method_sig = Types.AdtSig.lookup_method !Std_lib.map_adt_sig "add" |> Option.get in
  let map_add_binding = Bindings.get_value_binding ecx.Ecx.pcx.bindings map_add_method_sig.loc in
  let map_add_name = mk_value_binding_name map_add_binding in
  (* Find element type and instantiate Map's `add` method *)
  let func =
    Ecx.get_generic_function_value
      ~ecx
      map_add_name
      map_add_method_sig.trait_sig.type_params
      map_type_args
  in
  (* Emit call of Map's `add` method *)
  let index_and_expr_args = List.filter_map (fun v -> v) [index_val; expr_val] in
  mk_call_
    ~block:(Ecx.get_current_block ~ecx)
    ~func
    ~args:(map_val :: index_and_expr_args)
    ~return:None

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
  let func =
    Ecx.get_generic_function_value
      ~ecx
      map_reserve_name
      map_reserve_method_sig.trait_sig.type_params
      map_type_args
  in
  (* Emit call of Map's `reserve` method *)
  mk_call_ ~block:(Ecx.get_current_block ~ecx) ~func ~args:[map_val; capacity_val] ~return:None

and emit_call_map_new ~ecx return_mir_type map_type_args =
  let decl_loc = Std_lib.lookup_stdlib_decl_loc Std_lib.std_map_map_new in
  let binding = Bindings.get_value_binding ecx.Ecx.pcx.bindings decl_loc in
  let func =
    match binding.declaration with
    | FunDecl { Bindings.FunctionDeclaration.type_params; _ } ->
      let func_name = mk_value_binding_name binding in
      Ecx.get_generic_function_value ~ecx func_name type_params map_type_args
    | _ -> failwith "Expected function declaration"
  in
  (* Emit call of Map's `new` function *)
  mk_call ~block:(Ecx.get_current_block ~ecx) ~func ~args:[] ~return:(Some return_mir_type)

and emit_call_set_add ~ecx set_type_args set_val element_val =
  (* Get full name for set's `add` method *)
  let set_add_method_sig = Types.AdtSig.lookup_method !Std_lib.set_adt_sig "add" |> Option.get in
  let set_add_binding = Bindings.get_value_binding ecx.Ecx.pcx.bindings set_add_method_sig.loc in
  let set_add_name = mk_value_binding_name set_add_binding in
  (* Find element type and instantiate set's `add` method *)
  let func =
    Ecx.get_generic_function_value
      ~ecx
      set_add_name
      set_add_method_sig.trait_sig.type_params
      set_type_args
  in
  (* Emit call of set's `add` method *)
  let args =
    match element_val with
    | Some element_val -> [set_val; element_val]
    | None -> [set_val]
  in
  mk_call_ ~block:(Ecx.get_current_block ~ecx) ~func ~args ~return:None

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
  let func =
    Ecx.get_generic_function_value
      ~ecx
      set_reserve_name
      set_reserve_method_sig.trait_sig.type_params
      set_type_args
  in
  (* Emit call of set's `reserve` method *)
  mk_call_ ~block:(Ecx.get_current_block ~ecx) ~func ~args:[set_val; capacity_val] ~return:None

and emit_call_set_new ~ecx return_mir_type set_type_args =
  let decl_loc = Std_lib.lookup_stdlib_decl_loc Std_lib.std_set_set_new in
  let binding = Bindings.get_value_binding ecx.Ecx.pcx.bindings decl_loc in
  let func =
    match binding.declaration with
    | FunDecl { Bindings.FunctionDeclaration.type_params; _ } ->
      let func_name = mk_value_binding_name binding in
      Ecx.get_generic_function_value ~ecx func_name type_params set_type_args
    | _ -> failwith "Expected function declaration"
  in
  (* Emit call of set's `new` function *)
  mk_call ~block:(Ecx.get_current_block ~ecx) ~func ~args:[] ~return:(Some return_mir_type)

(* If the index is nonzero, emit a GetPointer instruction to calculate the pointer's start *)
and emit_offset_ptr ~ecx ptr_val offset_val =
  match offset_val.value with
  | Lit (Int lit) when lit = Int32.zero -> ptr_val
  | _ ->
    let ptr_ty = pointer_value_element_type ptr_val in
    mk_get_pointer_instr
      ~block:(Ecx.get_current_block ~ecx)
      ~pointer_offset:(Some offset_val)
      ~type_:ptr_ty
      ~ptr:ptr_val
      ~offsets:[]
      ()

and emit_cast_ptr ~ecx ~element_type ~ptr =
  let ptr_type = Type.Pointer element_type in
  mk_cast ~block:(Ecx.get_current_block ~ecx) ~arg:ptr ~type_:ptr_type

and emit_load_tag ~ecx ~type_ ~agg_ptr =
  let tag_ptr = emit_cast_ptr ~ecx ~element_type:(type_ :> Type.t) ~ptr:agg_ptr in
  mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:tag_ptr

and emit_store_tag ~ecx ~tag ~agg_ptr =
  let tag_ptr = emit_cast_ptr ~ecx ~element_type:(type_of_value tag) ~ptr:agg_ptr in
  mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:tag_ptr ~value:tag

and emit_construct_enum_variant ~ecx ~name ~ty =
  let (type_args, adt_sig) = Type_util.cast_to_adt_type ty in
  let layout = Ecx.get_mir_adt_layout ~ecx adt_sig type_args in
  match layout with
  (* Enum constructor is a pure integer, and tag is its direct value *)
  | PureEnum { tags; _ } -> SMap.find name tags
  (* Enum constructor is part of a variant type, so allocate union aggregate and set tag *)
  | Variants { tags; union; _ } ->
    let union_ty = Type.Aggregate union in

    (* Call myte_alloc builtin to allocate space for variant's union aggregate *)
    let agg_ptr =
      mk_call_builtin
        ~block:(Ecx.get_current_block ~ecx)
        Mir_builtin.myte_alloc
        [mk_int_lit_of_int32 Int32.one]
        [union_ty]
    in

    (* Store enum tag *)
    let tag = SMap.find name tags in
    emit_store_tag ~ecx ~tag ~agg_ptr;
    agg_ptr
  (* Enum constructor is a known niche value *)
  | InlineValueWithNiche { niches; _ } -> SMap.find name niches
  | Aggregate _
  | InlineValue _
  | ZeroSize ->
    failwith "Invalid layout for enum"

and emit_construct_tuple_variant
    ~ecx ~(name : string) ~(ty : Types.Type.t) ~(mk_elements : (unit -> Value.t option) list) =
  (* Find MIR ADT layout this constructor *)
  let (type_args, adt_sig) = Type_util.cast_to_adt_type ty in
  let layout = Ecx.get_mir_adt_layout ~ecx adt_sig type_args in
  match layout with
  (* If layout is an aggregate, construct tuple *)
  | Aggregate agg ->
    let agg_ptr_val = emit_construct_tuple ~ecx ~agg ~mk_elements in
    Some agg_ptr_val
  (* If layout is a variant, construct tuple and set variant tag *)
  | Variants { tags; union; variants; _ } ->
    (* Call myte_alloc builtin to allocate space for variant type *)
    let union_ptr =
      mk_call_builtin
        ~block:(Ecx.get_current_block ~ecx)
        Mir_builtin.myte_alloc
        [mk_int_lit_of_int32 Int32.one]
        [Aggregate union]
    in
    (* Cast to variant type and store all fields *)
    let tuple_variant_agg = SMap.find name variants in
    let variant_ptr =
      emit_cast_ptr ~ecx ~ptr:union_ptr ~element_type:(Aggregate tuple_variant_agg)
    in
    let tag = SMap.find name tags in
    emit_store_tuple_fields
      ~ecx
      ~ptr:variant_ptr
      ~tag:(Some tag)
      ~agg:tuple_variant_agg
      ~mk_elements;
    Some union_ptr
  (* If layout is an inlined value then exactly one element must have a non-zero size value, which
     we use directly without actually constructing a tuple. *)
  | InlineValue _ ->
    List.fold_left
      (fun acc mk_element ->
        match mk_element () with
        | None -> acc
        | Some value -> Some value)
      None
      mk_elements
  (* If layout is an inlined value with niche then value is either the niche or it is the only
     element that has a non-zero size (of which there must be exactly one). *)
  | InlineValueWithNiche inline_niche_layout ->
    let element_values = List.map (fun mk_element -> mk_element ()) mk_elements in
    emit_construct_inline_niche_value ~ecx inline_niche_layout name element_values
  (* If layout is zero size all elements must still be emitted, as they may have side effect *)
  | ZeroSize ->
    List.iter (fun mk_element -> ignore (mk_element ())) mk_elements;
    None
  | PureEnum _ -> failwith "Invalid layout for tuple"

and emit_construct_tuple ~ecx ~(agg : Aggregate.t) ~(mk_elements : (unit -> Value.t option) list) :
    Value.t =
  (* Call myte_alloc builtin to allocate space for tuple *)
  let ptr =
    mk_call_builtin
      ~block:(Ecx.get_current_block ~ecx)
      Mir_builtin.myte_alloc
      [mk_int_lit_of_int32 Int32.one]
      [Aggregate agg]
  in
  emit_store_tuple_fields ~ecx ~ptr ~tag:None ~agg ~mk_elements;
  ptr

and emit_store_tuple_fields
    ~ecx
    ~(ptr : Value.t)
    ~(tag : Value.t option)
    ~(agg : Aggregate.t)
    ~(mk_elements : (unit -> Value.t option) list) =
  (* If this tuple is a variant, store tag in first element *)
  (match tag with
  | None -> ()
  | Some tag -> emit_store_tag ~ecx ~tag ~agg_ptr:ptr);

  (* Store each argument to the tuple constructor in space allocated for tuple *)
  List.iteri
    (fun i mk_element ->
      match mk_element () with
      | None -> ()
      | Some element_val ->
        let (element_ty, element_index) = lookup_element agg (TupleKeyCache.get_key i) in
        let element_ptr =
          mk_get_pointer_instr
            ~block:(Ecx.get_current_block ~ecx)
            ~type_:element_ty
            ~ptr
            ~offsets:[Instruction.GetPointer.FieldIndex element_index]
            ()
        in
        mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:element_ptr ~value:element_val)
    mk_elements

and emit_record_field_value ~ecx field =
  let { Ast.Expression.Record.Field.name; value; _ } = field in
  match value with
  | Some expr -> emit_expression ~ecx expr
  | None -> emit_expression ~ecx (Identifier name)

and emit_store_record_fields ~ecx ~ptr ~tag agg fields =
  let open Ast.Expression in
  (* If this record is a variant, store tag in first element *)
  (match tag with
  | None -> ()
  | Some tag -> emit_store_tag ~ecx ~tag ~agg_ptr:ptr);

  (* Store each argument to the record constructor in space allocated for record *)
  List.iter
    (fun ({ Record.Field.name = { name; _ }; _ } as field) ->
      (* Calculate offset for this element and store *)
      match emit_record_field_value ~ecx field with
      | None -> ()
      | Some element_val ->
        let (element_ty, element_idx) = lookup_element agg name in
        let element_ptr_val =
          mk_get_pointer_instr
            ~block:(Ecx.get_current_block ~ecx)
            ~type_:element_ty
            ~ptr
            ~offsets:[Instruction.GetPointer.FieldIndex element_idx]
            ()
        in
        mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:element_ptr_val ~value:element_val)
    fields

and emit_construct_inline_niche_value
    ~ecx
    (layout : Mir_adt_layout.InlineValueWithNiche.t)
    (variant_name : string)
    (element_values : Value.t option list) =
  let { Mir_adt_layout.InlineValueWithNiche.niches; type_; inlined_type; _ } = layout in
  let value = List.find (fun element -> element <> None) element_values in
  match SMap.find_opt variant_name niches with
  | Some niche -> Some niche
  | None ->
    (* Directly inline value if its type is not changed *)
    if types_equal type_ inlined_type then
      value
    else
      (* Otherwise extend value to result type *)
      Some (mk_zext ~block:(Ecx.get_current_block ~ecx) ~arg:(Option.get value) ~type_)

and emit_inline_niche_value_destructuring
    ~ecx (layout : Mir_adt_layout.InlineValueWithNiche.t) (variant_name : string) (value : Value.t)
    : Value.t option =
  let { Mir_adt_layout.InlineValueWithNiche.niches; type_; inlined_type; _ } = layout in
  if SMap.mem variant_name niches then
    None
  (* Directly use inline value if its type is not changed *)
  else if types_equal type_ inlined_type then
    Some value
  else
    (* Otherwise truncate value to type of inlined value *)
    Some (mk_trunc ~block:(Ecx.get_current_block ~ecx) ~arg:value ~type_:inlined_type)

and emit_bool_expression ~ecx expr =
  let value = emit_expression ~ecx expr |> Option.get in
  if type_of_value value <> Bool then failwith "Expected bool value";
  value

and emit_integer_expression ~ecx expr =
  let value = emit_expression ~ecx expr |> Option.get in
  match type_of_value value with
  | Byte
  | Int
  | Long ->
    value
  | _ -> failwith "Expected integer value"

and emit_numeric_expression ~ecx expr =
  let value = emit_expression ~ecx expr |> Option.get in
  match type_of_value value with
  | Byte
  | Int
  | Long
  | Double ->
    value
  | _ -> failwith "Expected numeric value"

and emit_function_expression ~ecx expr =
  let value = emit_expression ~ecx expr |> Option.get in
  if type_of_value value <> Function then failwith "Expected function value";
  value

and emit_block ~ecx ~is_expr block =
  let { Statement.Block.statements; _ } = block in
  if statements = [] then
    None
  else
    (* Emit statements, only last statement can be an expression *)
    let (first_statements, last_statement) = List_utils.split_last statements in
    List.iter (fun stmt -> ignore (emit_statement ~ecx ~is_expr:false stmt)) first_statements;
    let value = emit_statement ~ecx ~is_expr last_statement in
    if is_expr then
      value
    else
      None

and emit_statement ~ecx ~is_expr stmt : Value.t option =
  let open Statement in
  match stmt with
  | ExpressionStatement { loc = _; expr; is_value } ->
    let expr_value = emit_expression ~ecx expr in
    if is_expr && is_value then
      expr_value
    else
      None
  | Block block -> emit_block ~ecx ~is_expr block
  (*
   * ============================
   *        If Statement
   * ============================
   *)
  | If { loc = _; test; conseq; altern = None } ->
    (* Branch to conseq or join blocks *)
    let test_val = emit_bool_expression ~ecx test in
    let conseq_block = Ecx.mk_block ~ecx in
    let join_block = Ecx.mk_block ~ecx in
    Ecx.finish_block_branch ~ecx test_val conseq_block join_block;

    (* Emit conseq and continue to join block *)
    Ecx.set_current_block ~ecx conseq_block;
    ignore (emit_block ~ecx ~is_expr conseq);
    Ecx.finish_block_continue ~ecx join_block;

    (* Start join block *)
    Ecx.set_current_block ~ecx join_block;
    None
  | If ({ loc = _; test; conseq; altern } as if_) ->
    if is_expr then
      emit_if_expression ~ecx if_
    else
      (* Branch to conseq or altern blocks *)
      let test_val = emit_bool_expression ~ecx test in
      let conseq_block = Ecx.mk_block ~ecx in
      let altern_block = Ecx.mk_block ~ecx in
      let join_block = Ecx.mk_block ~ecx in
      Ecx.finish_block_branch ~ecx test_val conseq_block altern_block;

      (* Emit conseq and continue to join block *)
      Ecx.set_current_block ~ecx conseq_block;
      ignore (emit_block ~ecx ~is_expr:false conseq);
      Ecx.finish_block_continue ~ecx join_block;

      (* Emit altern and continue to join block *)
      Ecx.set_current_block ~ecx altern_block;
      (match altern with
      | Block block -> ignore (emit_block ~ecx ~is_expr:false block)
      | If if_ -> ignore (emit_statement ~ecx ~is_expr:false (If if_))
      | None -> failwith "Handled by previous case");
      Ecx.finish_block_continue ~ecx join_block;

      (* Start join block *)
      Ecx.set_current_block ~ecx join_block;
      None
  (*
   * ============================
   *         While Loop
   * ============================
   *)
  | While { loc = _; test; body } ->
    (* Set up blocks for loop *)
    let test_block = Ecx.mk_block ~ecx in
    let body_block = Ecx.mk_block ~ecx in
    let finish_block = Ecx.mk_block ~ecx in
    Ecx.finish_block_continue ~ecx test_block;

    (* Emit test block which branches to finish or body blocks *)
    Ecx.set_current_block ~ecx test_block;
    let test_val = emit_bool_expression ~ecx test in
    Ecx.finish_block_branch ~ecx test_val body_block finish_block;

    (* Emit body block which continues to test block *)
    Ecx.push_loop_context ~ecx finish_block test_block;
    Ecx.set_current_block ~ecx body_block;
    ignore (emit_block ~ecx ~is_expr:false body);
    Ecx.finish_block_continue ~ecx test_block;
    Ecx.pop_loop_context ~ecx;

    (* Start join block *)
    Ecx.set_current_block ~ecx finish_block;
    None
  (*
   * ============================
   *         For Loop
   * ============================
   *)
  | For { pattern; iterator; body; _ } ->
    (* Set up blocks for loop *)
    let test_block = Ecx.mk_block ~ecx in
    let body_block = Ecx.mk_block ~ecx in
    let finish_block = Ecx.mk_block ~ecx in

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
    Ecx.finish_block_continue ~ecx test_block;

    (* Test block starts by calling iterator's `next` method, returning item option *)
    Ecx.set_current_block ~ecx test_block;
    let option_val =
      emit_method_call
        ~ecx
        ~method_name:"next"
        ~receiver_val:iterator_val
        ~receiver_ty:iterator_ty
        ~arg_vals:[]
        ~method_instance_type_args:[]
        ~ret_type:(Ecx.to_mir_type ~ecx (Std_lib.mk_option_type item_ty))
      |> Option.get
    in

    (* Destructure item option, jumping to finish if none *)
    let item_val =
      emit_option_destructuring
        ~ecx
        ~option_val
        ~item_ty
        ~some_branch_block:body_block
        ~none_branch_block:finish_block
    in

    (* Body block then destructures payload to for loop bindings *)
    Ecx.set_current_block ~ecx body_block;
    item_val |> Option.iter (emit_alloc_destructuring ~ecx pattern);

    (* Body block contains body of for loop and continues to test block *)
    Ecx.push_loop_context ~ecx finish_block test_block;
    ignore (emit_block ~ecx ~is_expr:false body);
    Ecx.pop_loop_context ~ecx;
    Ecx.finish_block_continue ~ecx test_block;

    (* Start join block *)
    Ecx.set_current_block ~ecx finish_block;
    None
  (*
   * ============================
   *      Loop Control Flow
   * ============================
   *)
  | Continue _ ->
    let (_, continue_id) = Ecx.get_loop_context ~ecx in
    Ecx.finish_block_continue ~ecx continue_id;
    None
  | Break _ ->
    let (break_id, _) = Ecx.get_loop_context ~ecx in
    Ecx.finish_block_continue ~ecx break_id;
    None
  (*
   * ============================
   *           Return
   * ============================
   *)
  | Return { loc = _; arg } ->
    let arg = Option_utils.flat_map (emit_expression ~ecx) arg in
    (* Handle implicit return from main function *)
    let arg =
      if arg = None && ecx.current_func == ecx.program.main_func then
        Some (mk_int_lit_of_int32 0l)
      else
        arg
    in

    (* Store returned value at return pointer if a value is returned and continue to return block *)
    (match arg with
    | None -> ()
    | Some arg ->
      let return_pointer = Option.get ecx.current_func_context.return_pointer in
      mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:return_pointer ~value:arg);
    Ecx.finish_block_continue ~ecx ecx.current_func_context.return_block;
    None
  (*
   * ============================
   *         Assignment
   * ============================
   *)
  | Assignment { loc = _; op; lvalue; expr } ->
    (* Apply an operation given the left and right values, returning the result *)
    let emit_apply_op op left_val expr_val =
      let op =
        match op with
        | Assignment.Add -> Instruction.Add
        | Subtract -> Sub
        | Multiply -> Mul
        | Divide -> Div
        | Remainder -> Rem
        | BitwiseAnd -> And
        | BitwiseOr -> Or
        | BitwiseXor -> Xor
        | LeftShift -> Shl
        | ArithmeticRightShift -> Shr
        | LogicalRightShift -> Shrl
      in
      mk_binary ~block:(Ecx.get_current_block ~ecx) ~op ~left:left_val ~right:expr_val
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
          let pointer_val = mk_pointer_val () |> Option.get in
          let loaded_val = mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:pointer_val in
          (* Apply operation *)
          let expr_val = emit_expression ~ecx expr |> Option.get in
          Some (emit_apply_op op loaded_val expr_val)
      in
      match (mk_pointer_val (), store_val) with
      | (Some pointer_val, Some store_val) ->
        mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:pointer_val ~value:store_val
      | _ -> ()
    in

    (* Emit an assignment that stores at an expression access chain *)
    let emit_access_chain_assign lvalue =
      let mk_pointer_val () =
        match emit_expression_access_chain ~ecx lvalue with
        | None -> None
        | Some (GetPointerEmittedResult element_pointer_val) -> Some element_pointer_val
        | Some (InlinedValueResult _) -> failwith "Cannot store to inlined value"
      in
      (* TODO: Only emit element pointer once, but this requires fixing lea coalescing in asm *)
      emit_pointer_assign mk_pointer_val expr
    in

    (* Simple variable assignments are stored at the variable's pointer, whether global or local *)
    let emit_variable_assign id_loc =
      match mir_type_of_loc ~ecx (Ast_utils.expression_loc expr) with
      (* If zero width type no assignment occurs, but still emit expression as it may have side effects *)
      | None -> ignore (emit_expression ~ecx expr)
      | Some mir_type ->
        let binding = Bindings.get_value_binding ecx.pcx.bindings id_loc in
        let pointer_val =
          if Bindings.is_module_decl binding then
            Ecx.get_global_pointer ~ecx binding |> Option.get
          else if Ecx.is_captured_binding ~ecx binding && Bindings.is_mutable_variable binding then
            (* Captured mutable variables must be loaded from environment *)
            emit_environment_element ~ecx binding |> Option.get
          else
            Ecx.get_local_ptr_def_instr ~ecx id_loc mir_type
        in
        emit_pointer_assign (fun _ -> Some pointer_val) expr
    in

    (match lvalue with
    | Pattern (Identifier { loc; _ }) -> emit_variable_assign loc
    | Expression (NamedAccess { loc; name; _ })
      when Type_context.is_scope_named_access ~cx:ecx.pcx.type_ctx loc ->
      emit_variable_assign name.loc
    (* Pattern assignments are destructured with a decision tree *)
    | Pattern pattern ->
      if op <> None then failwith "Destructuring cannot be used in operator assignment";
      (match emit_expression ~ecx expr with
      (* No destructuring occurs for zero size type *)
      | None -> ()
      | Some expr_val ->
        let decision_tree =
          Mir_match_decision_tree.build_destructure_decision_tree ~ecx expr_val pattern
        in
        let join_block = Ecx.mk_block ~ecx in
        emit_match_decision_tree ~ecx ~join_block ~result_ptr:None ~alloc:false decision_tree;
        Ecx.set_current_block ~ecx join_block)
    | Expression (IndexedAccess { loc; target; index; _ } as expr_lvalue) ->
      let target_ty = type_of_loc ~ecx (Ast_utils.expression_loc target) in
      (match target_ty with
      (* If indexing a vector, call Vec's `get` and `set` methods instead of emitting access chain *)
      | ADT { adt_sig; _ } when adt_sig == !Std_lib.vec_adt_sig ->
        let target_val = emit_expression ~ecx target |> Option.get in
        let index_val = emit_expression ~ecx index |> Option.get in
        (match op with
        (* Standard assignment simply calls `set` method *)
        | None ->
          let expr_val = emit_expression ~ecx expr in
          emit_call_vec_set ~ecx loc target_val index_val expr_val
        (* Operator assignment gets old value with `get`, applies operator, then calls `set` *)
        | Some op ->
          let old_value = emit_call_vec_get ~ecx loc target_val index_val |> Option.get in
          let expr_val = emit_expression ~ecx expr |> Option.get in
          let new_val = emit_apply_op op old_value expr_val in
          emit_call_vec_set ~ecx loc target_val index_val (Some new_val))
      (* If indexing a map, call Map's `add` method instead of emitting access chain *)
      | ADT { adt_sig; type_args } when adt_sig == !Std_lib.map_adt_sig ->
        if op <> None then failwith "Map indexing cannot be used in operator assignment";
        let target_val = emit_expression ~ecx target |> Option.get in
        let index_val = emit_expression ~ecx index in
        let expr_val = emit_expression ~ecx expr in
        emit_call_map_add ~ecx type_args target_val index_val expr_val
      | _ -> emit_access_chain_assign expr_lvalue)
    | Expression (NamedAccess _ as expr_lvalue) -> emit_access_chain_assign expr_lvalue
    | _ -> failwith "Lvalue expression must be an access");
    None
  (*
   * ============================
   *     Variable Declaration
   * ============================
   *)
  | VariableDeclaration { pattern; init; _ } ->
    let init_val = emit_expression ~ecx init in
    init_val |> Option.iter (emit_alloc_destructuring ~ecx pattern);
    None
  (*
   * ============================
   *       Match Statement
   * ============================
   *)
  | Match ({ args; cases; _ } as match_) ->
    if is_expr then
      emit_match_expression ~ecx match_
    else
      (* Emit args and decision tree *)
      let args = List.map (emit_expression ~ecx) args in
      let decision_tree = Mir_match_decision_tree.build_match_decision_tree ~ecx args cases in
      let join_block = Ecx.mk_block ~ecx in
      emit_match_decision_tree ~ecx ~join_block ~result_ptr:None ~alloc:true decision_tree;
      Ecx.set_current_block ~ecx join_block;
      None
  | FunctionDeclaration _ -> failwith "TODO: Emit MIR for non-toplevel function declarations"

and emit_option_destructuring
    ~ecx
    ~(option_val : Value.t)
    ~(item_ty : Types.Type.t)
    ~(some_branch_block : Block.t)
    ~(none_branch_block : Block.t) =
  (* Find aggregate data for option *)
  let layout = Ecx.get_mir_adt_layout ~ecx !Std_lib.option_adt_sig [item_ty] in
  match layout with
  | Variants variants_layout ->
    let some_aggregate = SMap.find "Some" variants_layout.variants in
    let some_tag = SMap.find "Some" variants_layout.tags in

    (* Test block proceeds to load tag of option *)
    let tag_val = emit_load_tag ~ecx ~agg_ptr:option_val ~type_:variants_layout.tag_mir_type in

    (* Test block finishes by jumping to Some branch if option is a `Some` variant, otherwise test
       block finishes by jumping to None branch if option is a `None` variant. *)
    let test_val =
      mk_cmp ~block:(Ecx.get_current_block ~ecx) ~cmp:Eq ~left:tag_val ~right:some_tag
    in
    Ecx.finish_block_branch ~ecx test_val some_branch_block none_branch_block;

    (* Some branch starts by loading payload from `Some` variant *)
    Ecx.set_current_block ~ecx some_branch_block;
    (match Ecx.to_mir_type ~ecx item_ty with
    (* Do not load payload if `Some` item is a zero size type *)
    | None -> None
    | Some _ ->
      let (item_mir_type, item_index) = lookup_element some_aggregate (TupleKeyCache.get_key 0) in
      let some_val = emit_cast_ptr ~ecx ~ptr:option_val ~element_type:(Aggregate some_aggregate) in
      let some_value_ptr =
        mk_get_pointer_instr
          ~block:(Ecx.get_current_block ~ecx)
          ~type_:item_mir_type
          ~ptr:some_val
          ~offsets:[Instruction.GetPointer.FieldIndex item_index]
          ()
      in
      Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:some_value_ptr))
  | InlineValueWithNiche inline_niche_layout ->
    (* Test block tests if option is the niche *)
    let none_niche = SMap.find "None" inline_niche_layout.niches in
    let test_val =
      mk_cmp ~block:(Ecx.get_current_block ~ecx) ~cmp:Neq ~left:option_val ~right:none_niche
    in
    Ecx.finish_block_branch ~ecx test_val some_branch_block none_branch_block;

    (* Some branch destructures the inlined option value *)
    Ecx.set_current_block ~ecx some_branch_block;
    emit_inline_niche_value_destructuring ~ecx inline_niche_layout "Some" option_val
  | _ -> failwith "Expected variants layout"

and emit_result_destructuring
    ~ecx
    ~(result_val : Value.t)
    ~(ok_ty : Types.Type.t)
    ~(error_ty : Types.Type.t)
    ~(ok_branch_block : Block.t)
    ~(error_branch_block : Block.t) =
  (* Find aggregate data for result *)
  let layout = Ecx.get_mir_adt_layout ~ecx !Std_lib.result_adt_sig [ok_ty; error_ty] in
  match layout with
  | Variants variants_layout ->
    let ok_aggregate = SMap.find "Ok" variants_layout.variants in
    let error_aggregate = SMap.find "Error" variants_layout.variants in
    let ok_tag = SMap.find "Ok" variants_layout.tags in

    (* Test block proceeds to load tag of result *)
    let tag_val = emit_load_tag ~ecx ~agg_ptr:result_val ~type_:variants_layout.tag_mir_type in

    (* Test block finishes by jumping to Ok branch if option is an `Ok` variant, otherwise test
       block finishes by jumping to Error branch if option is an `Error` variant. *)
    let test_val = mk_cmp ~block:(Ecx.get_current_block ~ecx) ~cmp:Eq ~left:tag_val ~right:ok_tag in
    Ecx.finish_block_branch ~ecx test_val ok_branch_block error_branch_block;

    (* Ok branch starts by loading payload from `Ok` variant *)
    Ecx.set_current_block ~ecx ok_branch_block;
    let ok_item_val =
      match Ecx.to_mir_type ~ecx ok_ty with
      (* Do not load payload if `Ok` item is a zero size type *)
      | None -> None
      | Some _ ->
        let ok_val = emit_cast_ptr ~ecx ~ptr:result_val ~element_type:(Aggregate ok_aggregate) in
        let (ok_value_type, ok_item_index) =
          lookup_element ok_aggregate (TupleKeyCache.get_key 0)
        in
        let ok_value_ptr =
          mk_get_pointer_instr
            ~block:(Ecx.get_current_block ~ecx)
            ~type_:ok_value_type
            ~ptr:ok_val
            ~offsets:[Instruction.GetPointer.FieldIndex ok_item_index]
            ()
        in
        Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:ok_value_ptr)
    in

    (* Error branch starts by loading payload from `Error` variant *)
    Ecx.set_current_block ~ecx error_branch_block;
    let error_item_val =
      match Ecx.to_mir_type ~ecx error_ty with
      | None -> None
      | Some _ ->
        let error_val =
          emit_cast_ptr ~ecx ~ptr:result_val ~element_type:(Aggregate error_aggregate)
        in
        let (error_value_type, error_item_index) =
          lookup_element error_aggregate (TupleKeyCache.get_key 0)
        in
        let error_value_ptr =
          mk_get_pointer_instr
            ~block:(Ecx.get_current_block ~ecx)
            ~type_:error_value_type
            ~ptr:error_val
            ~offsets:[Instruction.GetPointer.FieldIndex error_item_index]
            ()
        in
        Some (mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:error_value_ptr)
    in

    (ok_item_val, error_item_val)
  | InlineValueWithNiche inline_niche_layout ->
    (* Choose comparison so that ok block is always continue and error is jump, no matter which
       variant is the niche. *)
    let (niche_name, niche_val) = SMap.choose inline_niche_layout.niches in
    let niche_test_cmp =
      if String.equal niche_name "Ok" then
        Instruction.Eq
      else
        Neq
    in
    let test_val =
      mk_cmp
        ~block:(Ecx.get_current_block ~ecx)
        ~cmp:niche_test_cmp
        ~left:result_val
        ~right:niche_val
    in
    Ecx.finish_block_branch ~ecx test_val ok_branch_block error_branch_block;
    (* Ok and error items are the inlined option val if they are not zero-sized *)
    Ecx.set_current_block ~ecx ok_branch_block;

    let ok_item_val =
      emit_inline_niche_value_destructuring ~ecx inline_niche_layout "Ok" result_val
    in
    Ecx.set_current_block ~ecx error_branch_block;
    let err_item_val =
      emit_inline_niche_value_destructuring ~ecx inline_niche_layout "Error" result_val
    in
    (ok_item_val, err_item_val)
  | _ -> failwith "Expected variants layout"

and emit_alloc_destructuring ~(ecx : Ecx.t) pattern value =
  match pattern with
  | Pattern.Identifier { loc; _ } ->
    let mir_type = type_of_value value in
    let binding = Bindings.get_value_binding ecx.pcx.bindings loc in
    if Bindings.is_module_decl binding then
      let global_ptr = Ecx.get_global_pointer ~ecx binding |> Option.get in
      mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:global_ptr ~value
    else
      let alloc_instr = Ecx.get_local_ptr_def_instr ~ecx loc mir_type in
      append_instruction (Ecx.get_current_block ~ecx) alloc_instr;
      mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:alloc_instr ~value
  | _ ->
    let decision_tree =
      Mir_match_decision_tree.build_destructure_decision_tree ~ecx value pattern
    in
    let join_block = Ecx.mk_block ~ecx in
    emit_match_decision_tree ~ecx ~join_block ~result_ptr:None ~alloc:true decision_tree;
    Ecx.set_current_block ~ecx join_block

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
  (* Maintain cache of blocks at beginning of each case body (including guard) *)
  let constructed_case_bodies = ref LocSet.empty in
  let case_body_block_cache = ref LocMap.empty in
  let get_case_body_block case_body_loc =
    match LocMap.find_opt case_body_loc !case_body_block_cache with
    | Some block -> block
    | None ->
      let block = Ecx.mk_block ~ecx in
      case_body_block_cache := LocMap.add case_body_loc block !case_body_block_cache;
      block
  in

  (* First emit stack allocations for every binding in decision tree. For each binding, save the
     pointer to its stack allocation. *)
  let rec emit_binding_allocations tree_node acc =
    match tree_node with
    | DecisionTree.Leaf { bindings; guard_fail_case; _ } ->
      let acc =
        List.fold_left
          (fun acc (_, binding_loc) ->
            match mir_type_of_loc ~ecx binding_loc with
            | None -> acc
            | Some mir_type ->
              let decl_loc = Bindings.get_decl_loc_from_value_use ecx.pcx.bindings binding_loc in
              (* The same binding may be defined in multiple places due to or patterns. Make sure to
                 only emit a single stack slot. *)
              if LocSet.mem decl_loc acc then
                acc
              else
                let stack_alloc_instr = Ecx.get_local_ptr_def_instr ~ecx decl_loc mir_type in
                append_instruction (Ecx.get_current_block ~ecx) stack_alloc_instr;
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
             match mir_type_of_loc ~ecx binding_loc with
             | None -> path_cache
             | Some mir_type ->
               let (binding_val, path_cache) = emit_load_pattern_path ~path_cache path in
               let binding = Bindings.get_value_binding ecx.pcx.bindings binding_loc in
               let decl_loc = binding.loc in
               (* Determine pointer value - may be a global if this is a destructuring assignment *)
               let ptr_val =
                 if Bindings.is_module_decl binding then
                   Ecx.get_global_pointer ~ecx binding |> Option.get
                 else
                   Ecx.get_local_ptr_def_instr ~ecx decl_loc mir_type
               in
               mk_store_ ~block:(Ecx.get_current_block ~ecx) ~ptr:ptr_val ~value:binding_val;
               path_cache)
           path_cache
           bindings);

      (* Then if case is guarded, emit guard expression and only proceed to case body if guard
         succeeds, otherwise jump to and emit guard fail case, which is itself a decision tree. *)
      (match case_node with
      (* Destructurings do not have a case body, so simply continue to join block *)
      | None -> Ecx.finish_block_continue ~ecx join_block
      | Some case_node ->
        let case_body_block = get_case_body_block case_node.loc in
        (match case_node.guard with
        | None -> Ecx.finish_block_continue ~ecx case_body_block
        | Some guard_expr ->
          let guard_test_val = emit_bool_expression ~ecx guard_expr in
          let guard_fail_block = Ecx.mk_block ~ecx in
          Ecx.finish_block_branch ~ecx guard_test_val case_body_block guard_fail_block;
          Ecx.set_current_block ~ecx guard_fail_block;
          emit_tree_node ~path_cache (Option.get guard_fail_case));

        (* Ensure that case body is only emitted once, and shared between leaves *)
        if not (LocSet.mem case_node.loc !constructed_case_bodies) then (
          constructed_case_bodies := LocSet.add case_node.loc !constructed_case_bodies;
          Ecx.set_current_block ~ecx case_body_block;

          (* Emit the right hand side of case *)
          let (body_val, body_ty) =
            match case_node.right with
            | Expression expr ->
              let body_ty = type_of_loc ~ecx (Ast_utils.expression_loc expr) in
              (emit_expression ~ecx expr, body_ty)
            | Statement stmt ->
              let body_ty = type_of_loc ~ecx (Ast_utils.statement_loc stmt) in
              (emit_statement ~ecx ~is_expr:(result_ptr <> None) stmt, body_ty)
          in

          (* Store result at result ptr if one is supplied and case does not diverge *)
          let diverges = body_ty = Never in
          (match result_ptr with
          | Some result_ptr when not diverges ->
            mk_store_
              ~block:(Ecx.get_current_block ~ecx)
              ~ptr:result_ptr
              ~value:(Option.get body_val)
          | _ -> ());

          (* Continue to match's overall join block at end of case body if case does not diverge *)
          if diverges then
            Ecx.finish_block_unreachable ~ecx
          else
            Ecx.finish_block_continue ~ecx join_block
        ))
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
        emit_decision_tree_if_else_chain ~path_cache [first_case] second_case (fun ctor ->
            let ctor_lit = mk_bool_lit (Ctor.cast_to_bool ctor) in
            mk_cmp ~block:(Ecx.get_current_block ~ecx) ~cmp:Eq ~left:scrutinee_val ~right:ctor_lit)
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
            let (value, _) = Ctor.cast_to_string ctor in
            let { Ecx.ImmutableString.value_global_val; size_global_val } =
              Ecx.add_immutable_string_literal ~ecx value
            in
            let size_val = mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:size_global_val in
            emit_method_call
              ~ecx
              ~method_name:"equalsImmutable"
              ~receiver_val:(Some scrutinee_val)
              ~receiver_ty:(Std_lib.mk_string_type ())
              ~arg_vals:[value_global_val; size_val]
              ~method_instance_type_args:[]
              ~ret_type:(Some Bool)
            |> Option.get)
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
        let scrutinee_type = type_of_value scrutinee_val in
        (* TODO: Emit better checks than linear if else chain - e.g. binary search, jump table *)
        emit_decision_tree_if_else_chain ~path_cache test_cases default_case (fun ctor ->
            let (value, _) = Ctor.cast_to_int ctor in
            let case_lit =
              match scrutinee_type with
              | Byte -> mk_byte_lit (Int64.to_int value)
              | Int -> mk_int_lit_of_int32 (Int64.to_int32 value)
              | Long -> mk_long_lit value
              | _ -> failwith "Expected integer value"
            in
            mk_cmp ~block:(Ecx.get_current_block ~ecx) ~cmp:Eq ~left:scrutinee_val ~right:case_lit)
      (* Variants are tested by loading their tag and checking against scrutinee in if-else chain *)
      | (Variant (_, adt_sig, type_args), _) ->
        (* Split out last case if there is no default, as it does not have to be explicitly tested *)
        let (test_cases, default_case) =
          match default_case with
          | None ->
            let (cases, (_, last_tree_node)) = List_utils.split_last cases in
            (cases, last_tree_node)
          | Some default_tree_node -> (cases, default_tree_node)
        in

        let emit_tag_compare_decision_tree scrutinee_tag_val tags path_cache =
          (* TODO: Emit better checks than linear if else chain - e.g. binary search, jump table *)
          emit_decision_tree_if_else_chain ~path_cache test_cases default_case (fun ctor ->
              let (name, _, _) = Ctor.cast_to_variant ctor in
              let tag_val = SMap.find name tags in
              mk_cmp
                ~block:(Ecx.get_current_block ~ecx)
                ~cmp:Eq
                ~left:scrutinee_tag_val
                ~right:tag_val)
        in

        let (scrutinee_val, path_cache) = emit_load_pattern_path ~path_cache scrutinee in

        let layout = Ecx.get_mir_adt_layout ~ecx adt_sig type_args in
        (match layout with
        (* If layout is a variant then load tag value so it can be tested *)
        | Variants layout ->
          let scrutinee_tag_val =
            emit_load_tag ~ecx ~agg_ptr:scrutinee_val ~type_:layout.tag_mir_type
          in
          emit_tag_compare_decision_tree scrutinee_tag_val layout.tags path_cache
        (* If layout is a pure enum then scrutinee is already tag value *)
        | PureEnum layout -> emit_tag_compare_decision_tree scrutinee_val layout.tags path_cache
        (* If layout is an inlined value with niches test against each niche value *)
        | InlineValueWithNiche { niches; inline_range; _ } ->
          emit_decision_tree_if_else_chain ~path_cache test_cases default_case (fun ctor ->
              let (name, _, _) = Ctor.cast_to_variant ctor in
              let (cmp, right_val) =
                match SMap.find_opt name niches with
                | Some niche_val -> (Instruction.Eq, niche_val)
                (* Test for inline range to see if this is an inline value *)
                | None ->
                  (match inline_range with
                  | NotEqual niche_val -> (Neq, niche_val)
                  | Below upper_bound_val -> (Lt, upper_bound_val))
              in
              mk_cmp ~block:(Ecx.get_current_block ~ecx) ~cmp ~left:scrutinee_val ~right:right_val)
        | _ -> failwith "Invalid layout for variants"))
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
        let emit_load_aggregate_element aggregate field_key pointer_val =
          let (element_type, element_index) = lookup_element aggregate field_key in
          let element_ptr =
            mk_get_pointer_instr
              ~block:(Ecx.get_current_block ~ecx)
              ~type_:element_type
              ~ptr:pointer_val
              ~offsets:[Instruction.GetPointer.FieldIndex element_index]
              ()
          in
          let element_value = mk_load ~block:(Ecx.get_current_block ~ecx) ~ptr:element_ptr in
          (element_value, IMap.add path_field_id element_value path_cache)
        in

        (* If already in the path cache, use already generated value for path *)
        match IMap.find_opt path_field_id path_cache with
        | Some path_value -> (path_value, path_cache)
        | None ->
          (match field with
          | PatternPath.Root _ -> failwith "Expected field"
          (* Tuple fields are simply looked up in the tuple type *)
          | TupleField { index; _ } ->
            let value_type = type_of_value value in
            let ptr_type = cast_to_pointer_type value_type in
            let aggregate = cast_to_aggregate_type ptr_type in
            let field_key = TupleKeyCache.get_key index in
            emit_load_aggregate_element aggregate field_key value
          (* Look up field in single variant (aggregate) types *)
          | VariantField { field; adt_sig; type_args; _ } when SMap.cardinal adt_sig.variants = 1 ->
            (match Ecx.get_mir_adt_layout ~ecx adt_sig type_args with
            | Aggregate agg ->
              let field_key = get_field_key field in
              emit_load_aggregate_element agg field_key value
            | _ -> failwith "Expected aggregate layout")
          (* For variant fields we must find the correct variant aggregate, fetch its field key,
             and cast the pointer to the variant aggregate type. *)
          | VariantField { field; variant_name; adt_sig; type_args; _ } ->
            (match Ecx.get_mir_adt_layout ~ecx adt_sig type_args with
            | Variants { variants; _ } ->
              let variant_aggregate = SMap.find variant_name variants in
              let field_key = get_field_key field in
              let variant_val =
                emit_cast_ptr ~ecx ~ptr:value ~element_type:(Aggregate variant_aggregate)
              in
              emit_load_aggregate_element variant_aggregate field_key variant_val
            (* Path must be for the already inlined value *)
            | InlineValueWithNiche inline_niche_layout ->
              let value =
                emit_inline_niche_value_destructuring ~ecx inline_niche_layout variant_name value
              in
              (Option.get value, path_cache)
            | _ -> failwith "Expected variants layout")))
      (Option.get root_value, path_cache)
      fields
  (* Emit a sequence of tests and resulting blocks given a list of tests cases with ctors, the default
     case if all test ctors fail, and a function to generate a boolean test value from a ctor. *)
  and emit_decision_tree_if_else_chain ~path_cache test_cases default_case gen_test_val =
    let emit_test (ctor, tree_node) is_last_test =
      let test_val = gen_test_val ctor in
      let pass_case_block = Ecx.mk_block ~ecx in
      (* The last test links directly to the default case, otherwise create a new block for the next test *)
      let fail_case_block =
        if is_last_test then
          Ecx.mk_block ~ecx
        else
          Ecx.mk_block ~ecx
      in
      Ecx.finish_block_branch ~ecx test_val pass_case_block fail_case_block;
      Ecx.set_current_block ~ecx pass_case_block;
      emit_tree_node ~path_cache tree_node;
      Ecx.set_current_block ~ecx fail_case_block
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

and type_of_loc ~ecx loc : Types.Type.t =
  let tvar_id = Type_context.get_tvar_from_loc ~cx:ecx.pcx.type_ctx loc in
  Ecx.find_rep_non_generic_type ~ecx (TVar tvar_id)

and mir_type_of_loc ~ecx loc : Type.t option =
  let tvar_id = Type_context.get_tvar_from_loc ~cx:ecx.pcx.type_ctx loc in
  type_to_mir_type ~ecx (Types.Type.TVar tvar_id)

and type_to_mir_type ~ecx ty : Type.t option =
  let ty = Ecx.find_rep_non_generic_type ~ecx ty in
  Ecx.to_mir_type ~ecx ty
