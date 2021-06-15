open Analyze_error
open Basic_collections
open Type_context

let rec build_type ~cx ty =
  let open Ast.Type in
  match ty with
  | Primitive { Primitive.kind; _ } ->
    let open Primitive in
    (match kind with
    | Unit -> Types.Unit
    | Byte -> Types.Byte
    | Int -> Types.Int
    | Long -> Types.Long
    | String -> Types.String
    | Bool -> Types.Bool)
  | Tuple { Tuple.elements; _ } -> Types.Tuple (List.map (build_type ~cx) elements)
  | Function { Function.params; return; _ } ->
    Types.Function { params = List.map (build_type ~cx) params; return = build_type ~cx return }
  | Identifier
      {
        Identifier.loc = full_loc;
        name = { Ast.ScopedIdentifier.name = { Ast.Identifier.loc; _ }; _ };
        type_params;
        _;
      } ->
    let type_param_arity_error actual expected =
      Type_context.add_error ~cx full_loc (IncorrectTypeParametersArity (actual, expected))
    in
    let type_args = List.map (build_type ~cx) type_params in
    let num_type_args = List.length type_args in
    let binding = Type_context.get_type_binding ~cx loc in
    (* Check if this is a builtin type *)
    (match Std_lib.lookup_stdlib_decl_loc binding.loc with
    | Some name when name = Std_lib.std_array_array ->
      (match type_args with
      | [element_type] -> Types.Array element_type
      | _ ->
        type_param_arity_error num_type_args 1;
        Types.Any)
    | _ ->
      (match binding.declaration with
      (* Type parameters can be used directly and do not take type parameters of their own *)
      | TypeParam _ when type_args <> [] ->
        type_param_arity_error num_type_args 0;
        Types.Any
      | TypeParam type_param -> Types.TypeParam (Bindings.TypeParamDeclaration.get type_param)
      (* Parameterized types must have correct arity *)
      | TypeAlias { type_params; body = _ } when List.length type_params <> num_type_args ->
        type_param_arity_error num_type_args (List.length type_params);
        Types.Any
      (* Substitute supplied type arguments for type parameters in body of type alias *)
      | TypeAlias { type_params; body } ->
        let type_param_and_args = List.combine type_params type_args in
        let subst_map =
          List.fold_left
            (fun map (type_param, type_arg) -> IMap.add type_param.Types.TypeParam.id type_arg map)
            IMap.empty
            type_param_and_args
        in
        Types.substitute_type_params subst_map body
      (* Pass type args to ADT if they have the correct arity *)
      | TypeDecl type_decl ->
        let adt_sig = Bindings.TypeDeclaration.get type_decl in
        if List.length adt_sig.type_params <> num_type_args then (
          type_param_arity_error num_type_args (List.length adt_sig.type_params);
          Types.Any
        ) else
          Types.ADT { adt_sig; type_args }))

and visit_type_declarations_prepass ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      (* Create empty ADT sig for each algebraic data type definition *)
      let open Ast.TypeDeclaration in
      match toplevel with
      | TypeDeclaration
          {
            name = { Ast.Identifier.loc = id_loc; name };
            decl = Tuple _ | Record _ | Variant _;
            type_params;
            _;
          } ->
        let binding = Type_context.get_type_binding ~cx id_loc in
        let adt_decl = Bindings.get_type_decl binding in
        let type_params = check_type_parameters ~cx type_params in
        let adt_sig = Types.mk_adt_sig name type_params in
        Bindings.TypeDeclaration.set adt_decl adt_sig
      | _ -> ())
    toplevels

and check_type_aliases_topologically ~cx modules =
  let open Ast.TypeDeclaration in
  let modules = List.map snd modules in
  try
    let aliases_in_topological_order = Type_alias.order_type_aliases ~cx modules in
    List.iter
      (fun alias ->
        match alias with
        | { loc = _; name; type_params; decl = Alias alias; builtin = _ } ->
          (* Save type paramters and type to alias declaration *)
          let binding = Type_context.get_type_binding ~cx name.loc in
          let alias_decl = Bindings.get_type_alias_decl binding in
          alias_decl.type_params <- check_type_parameters ~cx type_params;
          alias_decl.body <- build_type ~cx alias
        | _ -> failwith "Expected type alias")
      aliases_in_topological_order
  with Type_alias.CyclicTypeAliasesException (loc, name) ->
    Type_context.add_error ~cx loc (CyclicTypeAlias name)

and visit_type_declarations ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      let open Ast.TypeDeclaration in
      match toplevel with
      (*
       *
       * Builtin Type
       *
       *)
      | TypeDeclaration { builtin = true; _ } -> ()
      | TypeDeclaration { decl = Alias _; _ } -> ()
      (*
       * Algebraic Data Type
       *
       * Build variant signatures for each variant in this ADT. Each variant's constructor id is
       * also unified with ADT.
       *)
      | TypeDeclaration
          {
            loc = _;
            name = { Ast.Identifier.loc = id_loc; name };
            decl;
            type_params = _;
            builtin = _;
          } ->
        let binding = Type_context.get_type_binding ~cx id_loc in
        let adt_decl = Bindings.get_type_decl binding in
        let adt_sig = Bindings.TypeDeclaration.get adt_decl in
        let build_element_tys ~cx elements = List.map (build_type ~cx) elements in
        let build_field_tys ~cx fields =
          List.fold_left
            (fun field_tys field ->
              let { Record.Field.name = { Ast.Identifier.name; _ }; ty; _ } = field in
              let field_ty = build_type ~cx ty in
              SMap.add name field_ty field_tys)
            SMap.empty
            fields
        in
        let set_adt_sig ctor_decl_loc =
          let binding = Type_context.get_value_binding ~cx ctor_decl_loc in
          let ctor_decl = Bindings.get_ctor_decl binding in
          Bindings.ConstructorDeclaration.set ctor_decl adt_sig
        in
        (match decl with
        | Tuple { name = { Ast.Identifier.loc; _ }; elements; _ } ->
          set_adt_sig loc;
          let element_tys = build_element_tys ~cx elements in
          adt_sig.variant_sigs <- SMap.singleton name (Types.TupleVariantSig element_tys)
        | Record { name = { Ast.Identifier.loc; _ }; fields; _ } ->
          set_adt_sig loc;
          let field_tys = build_field_tys ~cx fields in
          adt_sig.variant_sigs <- SMap.singleton name (Types.RecordVariantSig field_tys)
        | Variant variants ->
          let variant_sigs =
            List.fold_left
              (fun variant_sigs variant ->
                let open Ast.Identifier in
                match variant with
                | EnumVariant { loc; name } ->
                  set_adt_sig loc;
                  SMap.add name Types.EnumVariantSig variant_sigs
                | TupleVariant { name = { loc; name }; elements; _ } ->
                  set_adt_sig loc;
                  let element_tys = build_element_tys ~cx elements in
                  SMap.add name (Types.TupleVariantSig element_tys) variant_sigs
                | RecordVariant { name = { loc; name }; fields; _ } ->
                  set_adt_sig loc;
                  let field_tys = build_field_tys ~cx fields in
                  SMap.add name (Types.RecordVariantSig field_tys) variant_sigs)
              SMap.empty
              variants
          in
          adt_sig.variant_sigs <- variant_sigs
        | Alias _ -> ())
      | _ -> ())
    toplevels

and visit_value_declarations ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | VariableDeclaration decl -> check_toplevel_variable_declaration_prepass ~cx decl
      | FunctionDeclaration decl -> check_toplevel_function_declaration_prepass ~cx decl
      | _ -> ())
    toplevels

and check_module ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | VariableDeclaration decl -> check_variable_declaration ~cx decl
      | FunctionDeclaration decl -> check_toplevel_function_declaration ~cx decl
      | TypeDeclaration _ -> ())
    toplevels

(* Prepass to fill types of all global variables (and error if they are unannotated) *)
and check_toplevel_variable_declaration_prepass ~cx decl =
  let open Ast.Statement.VariableDeclaration in
  let { loc; pattern; annot; _ } = decl in
  let (pattern_loc, pattern_tvar_id) = check_pattern ~cx pattern in
  match annot with
  | None -> Type_context.add_error ~cx loc ToplevelVarWithoutAnnotation
  | Some annot ->
    let annot_ty = build_type ~cx annot in
    Type_context.assert_unify ~cx pattern_loc annot_ty (Types.TVar pattern_tvar_id)

and check_variable_declaration ~cx decl =
  let open Ast.Statement.VariableDeclaration in
  let { loc; pattern; init; annot; _ } = decl in
  let (pattern_loc, pattern_tvar_id) = check_pattern ~cx pattern in
  let (expr_loc, expr_tvar_id) = check_expression ~cx init in
  match annot with
  | None ->
    (* If expression's type is fully resolved then use as type of id, otherwise error
       requesting an annotation. *)
    let rep_ty = Type_context.find_rep_type ~cx (TVar expr_tvar_id) in
    let unresolved_tvars = Types.get_all_tvars [rep_ty] in
    if unresolved_tvars = [] then
      Type_context.assert_unify ~cx expr_loc (TVar expr_tvar_id) (TVar pattern_tvar_id)
    else
      let partial =
        match rep_ty with
        | TVar _ -> None
        | _ -> Some (rep_ty, unresolved_tvars)
      in
      Type_context.add_error ~cx loc (CannotInferType (CannotInferTypeVariableDeclaration, partial))
  | Some annot ->
    let annot_ty = build_type ~cx annot in
    if Type_context.unify ~cx annot_ty (TVar pattern_tvar_id) then
      Type_context.assert_is_subtype ~cx expr_loc (TVar expr_tvar_id) annot_ty
    else
      Type_context.add_incompatible_types_error ~cx pattern_loc (TVar pattern_tvar_id) annot_ty

and check_type_parameters ~cx params =
  List.map
    (fun { Ast.TypeParameter.name = { Ast.Identifier.loc; name }; _ } ->
      let binding = Type_context.get_type_binding ~cx loc in
      let type_param_decl = Bindings.get_type_param_decl binding in
      let type_param = Types.TypeParam.mk name in
      Bindings.TypeParamDeclaration.set type_param_decl type_param;
      type_param)
    params

and check_toplevel_function_declaration_prepass ~cx decl = check_function_declaration_type ~cx decl

and check_toplevel_function_declaration ~cx decl =
  if not decl.builtin then check_function_declaration_body ~cx decl

(* Build the function type for a function declaration and set it as type of function identifier *)
and check_function_declaration_type ~cx decl =
  let open Ast.Function in
  let { name = { loc = id_loc; _ }; params; return; type_params; _ } = decl in
  let type_params = check_type_parameters ~cx type_params in
  let params = List.map (fun param -> build_type ~cx param.Param.annot) params in
  let return = Option.fold ~none:Types.Unit ~some:(fun return -> build_type ~cx return) return in

  (* Bind annotated function type and signature to function declaration *)
  let binding = Type_context.get_value_binding ~cx id_loc in
  let func_decl = Bindings.get_func_decl binding in
  func_decl.type_params <- type_params;
  func_decl.params <- params;
  func_decl.return <- return

(* Type check function declaration body, including binding types to function parameters and
   checking function return type. *)
and check_function_declaration_body ~cx decl =
  let open Ast.Function in
  let { name = { loc = id_loc; _ }; params; body; _ } = decl in

  (* Find param and return types for function *)
  let binding = Type_context.get_value_binding ~cx id_loc in
  let func_decl = Bindings.get_func_decl binding in

  (* Bind param id tvars to their annotated types *)
  List.combine params func_decl.params
  |> List.iter (fun ({ Param.name = { Ast.Identifier.loc; _ }; _ }, param_ty) ->
         let binding = Type_context.get_value_binding ~cx loc in
         let param_decl = Bindings.get_func_param_decl binding in
         ignore (Type_context.unify ~cx param_ty (TVar param_decl.tvar_id)));

  match body with
  | Expression expr ->
    (* Expression body must be subtype of return type *)
    let (expr_loc, expr_tvar_id) = check_expression ~cx expr in
    Type_context.assert_is_subtype ~cx expr_loc (TVar expr_tvar_id) func_decl.return
  | Block block ->
    let open Ast.Statement in
    let block_stmt = Block block in
    (* Annotate each return statement node with this function's return type *)
    Ast_utils.statement_visitor
      ~enter_functions:false
      ~f:(fun stmt ->
        match stmt with
        | Return { Return.loc; _ } -> Type_context.add_return_type ~cx loc func_decl.return
        | _ -> ())
      block_stmt;
    check_statement ~cx block_stmt

and check_function_declaration ~cx decl =
  check_function_declaration_type ~cx decl;
  check_function_declaration_body ~cx decl

and check_expression ~cx expr =
  let open Ast.Expression in
  match expr with
  | Unit { Unit.loc } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Types.Unit (TVar tvar_id));
    (loc, tvar_id)
  | IntLiteral { IntLiteral.loc; raw; base } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let int_literal_ty = Type_context.mk_int_literal_ty ~cx loc raw base in
    ignore (Type_context.unify ~cx int_literal_ty (TVar tvar_id));
    (loc, tvar_id)
  | StringLiteral { StringLiteral.loc; _ } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Types.String (TVar tvar_id));
    (loc, tvar_id)
  | BoolLiteral { BoolLiteral.loc; _ } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Types.Bool (TVar tvar_id));
    (loc, tvar_id)
  | Identifier { Ast.Identifier.loc = id_loc as loc; name }
  | ScopedIdentifier { Ast.ScopedIdentifier.loc; name = { Ast.Identifier.loc = id_loc; name }; _ }
    ->
    let open Types in
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let binding = Type_context.get_value_binding ~cx id_loc in
    let decl_ty =
      match binding.declaration with
      (* If id is a constructor look up corresponding ADT to use as type. Error on tuple and record
         constructors as they are handled elsewhere. *)
      | CtorDecl ctor_decl ->
        let adt_sig = Bindings.ConstructorDeclaration.get ctor_decl in
        (match SMap.find name adt_sig.variant_sigs with
        | EnumVariantSig ->
          if adt_sig.type_params = [] then
            Types.ADT { adt_sig; type_args = [] }
          else
            Types.refresh_adt_type_params adt_sig
        | TupleVariantSig elements ->
          Type_context.add_error ~cx loc (IncorrectTupleConstructorArity (0, List.length elements));
          Any
        | RecordVariantSig fields ->
          let field_names = SMap.fold (fun name _ names -> name :: names) fields [] |> List.rev in
          Type_context.add_error ~cx loc (MissingRecordConstructorFields field_names);
          Any)
      (* Id is for a function declaration. If function has type parameters then generate a fresh
         type variable for each type parameter and substitute into function type. *)
      | FunDecl func_decl ->
        if func_decl.type_params = [] then
          Types.Function { params = func_decl.params; return = func_decl.return }
        else
          let fresh_type_args = List.map (fun _ -> mk_tvar ()) func_decl.type_params in
          let fresh_type_arg_bindings =
            bind_type_params_to_args func_decl.type_params fresh_type_args
          in
          let fresh_params =
            List.map (Types.substitute_type_params fresh_type_arg_bindings) func_decl.params
          in
          let fresh_return =
            Types.substitute_type_params fresh_type_arg_bindings func_decl.return
          in
          Function { params = fresh_params; return = fresh_return }
      (* Otherwise identifier has same type as its declaration *)
      | FunParamDecl param_decl -> TVar param_decl.tvar_id
      | VarDecl var_decl -> TVar var_decl.tvar_id
    in
    ignore (Type_context.unify ~cx decl_ty (TVar tvar_id));
    (loc, tvar_id)
  | Tuple { Tuple.loc; elements } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let element_locs_and_tvar_ids = List.map (check_expression ~cx) elements in
    let element_tys = List.map (fun (_, tvar_id) -> Types.TVar tvar_id) element_locs_and_tvar_ids in
    ignore (Type_context.unify ~cx (Types.Tuple element_tys) (TVar tvar_id));
    (loc, tvar_id)
  | TypeCast { TypeCast.loc; expr; ty } ->
    let (expr_loc, expr_tvar_id) = check_expression ~cx expr in
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let ty = build_type ~cx ty in
    ignore (Type_context.unify ~cx ty (TVar tvar_id));
    (* Expr must be a subtype of annotated type *)
    Type_context.assert_is_subtype ~cx expr_loc (TVar expr_tvar_id) ty;
    (loc, tvar_id)
  (* 
   * ============================
   * Unary Operation
   * ============================
   *)
  | UnaryOperation { UnaryOperation.loc; op; operand } ->
    let open UnaryOperation in
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (operand_loc, operand_tvar_id) = check_expression ~cx operand in
    let operand_rep_ty = Type_context.find_rep_type ~cx (TVar operand_tvar_id) in
    let result_ty =
      match (operand_rep_ty, op) with
      | ((Byte | Int | Long | IntLiteral _), (Plus | Minus | Not))
      | (Bool, Not)
      | (Any, _) ->
        operand_rep_ty
      | _ ->
        let expected_tys =
          match op with
          | Plus
          | Minus ->
            [Types.Int]
          | Not -> [Types.Bool; Types.Int]
        in
        Type_context.add_error ~cx operand_loc (IncompatibleTypes (operand_rep_ty, expected_tys));
        Any
    in
    ignore (Type_context.unify ~cx result_ty (TVar tvar_id));
    (loc, tvar_id)
  (* 
   * ============================
   * Binary Operation
   * ============================
   *)
  | BinaryOperation { BinaryOperation.loc; op; left; right } ->
    let open BinaryOperation in
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (left_loc, left_tvar_id) = check_expression ~cx left in
    let (right_loc, right_tvar_id) = check_expression ~cx right in
    let is_int tvar_id =
      let rep_ty = Type_context.find_rep_type ~cx (TVar tvar_id) in
      match rep_ty with
      | Byte
      | Int
      | Long
      | IntLiteral _ ->
        true
      | _ -> false
    in
    let is_int_or_string tvar_id =
      let rep_ty = Type_context.find_rep_type ~cx (TVar tvar_id) in
      match rep_ty with
      | Byte
      | Int
      | Long
      | IntLiteral _
      | String ->
        true
      | _ -> false
    in
    let error_int loc tvar_id =
      Type_context.add_error
        ~cx
        loc
        (IncompatibleTypes (Type_context.find_rep_type ~cx (TVar tvar_id), [Types.Int]))
    in
    let error_int_or_string loc tvar_id =
      Type_context.add_error
        ~cx
        loc
        (IncompatibleTypes (Type_context.find_rep_type ~cx (TVar tvar_id), [Types.Int; Types.String]))
    in
    (match op with
    | Add ->
      (* If a child expression is an int or string propagate type to other child and expression *)
      if is_int_or_string left_tvar_id then (
        Type_context.assert_unify ~cx right_loc (TVar left_tvar_id) (TVar right_tvar_id);
        ignore (Type_context.unify ~cx (TVar left_tvar_id) (TVar tvar_id))
      ) else if is_int_or_string right_tvar_id then (
        Type_context.assert_unify ~cx left_loc (TVar right_tvar_id) (TVar left_tvar_id);
        ignore (Type_context.unify ~cx (TVar right_tvar_id) (TVar tvar_id))
      ) else (
        (* Otherwise force expression's type to be any to avoid erroring at uses *)
        error_int_or_string left_loc left_tvar_id;
        error_int_or_string right_loc right_tvar_id;
        ignore (Type_context.unify ~cx Any (TVar tvar_id))
      )
    | Subtract
    | Multiply
    | Divide
    | Remainder
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor
    | LeftShift
    | ArithmeticRightShift
    | LogicalRightShift ->
      (* If a child expression is an int propagate type to other child and expression *)
      if is_int left_tvar_id then (
        Type_context.assert_unify ~cx right_loc (TVar left_tvar_id) (TVar right_tvar_id);
        ignore (Type_context.unify ~cx (TVar left_tvar_id) (TVar tvar_id))
      ) else if is_int right_tvar_id then (
        Type_context.assert_unify ~cx left_loc (TVar right_tvar_id) (TVar left_tvar_id);
        ignore (Type_context.unify ~cx (TVar right_tvar_id) (TVar tvar_id))
      ) else (
        (* Otherwise force expression's type to be any to avoid erroring at uses *)
        error_int left_loc left_tvar_id;
        error_int right_loc right_tvar_id;
        ignore (Type_context.unify ~cx Any (TVar tvar_id))
      )
    | Equal
    | NotEqual ->
      Type_context.assert_unify ~cx right_loc (TVar left_tvar_id) (TVar right_tvar_id);
      ignore (Type_context.unify ~cx Types.Bool (TVar tvar_id))
    | LessThan
    | GreaterThan
    | LessThanOrEqual
    | GreaterThanOrEqual ->
      (* If a child expression is an int or string propagate type to other child *)
      if is_int_or_string left_tvar_id then
        Type_context.assert_unify ~cx right_loc (TVar left_tvar_id) (TVar right_tvar_id)
      else if is_int_or_string right_tvar_id then
        Type_context.assert_unify ~cx left_loc (TVar right_tvar_id) (TVar left_tvar_id)
      else (
        error_int_or_string left_loc left_tvar_id;
        error_int_or_string right_loc right_tvar_id
      );
      ignore (Type_context.unify ~cx Types.Bool (TVar tvar_id)));
    (loc, tvar_id)
  | LogicalAnd { LogicalAnd.loc; left; right }
  | LogicalOr { LogicalOr.loc; left; right } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (left_loc, left_tvar_id) = check_expression ~cx left in
    let (right_loc, right_tvar_id) = check_expression ~cx right in
    Type_context.assert_unify ~cx left_loc Types.Bool (TVar left_tvar_id);
    Type_context.assert_unify ~cx right_loc Types.Bool (TVar right_tvar_id);
    ignore (Type_context.unify ~cx Types.Bool (TVar tvar_id));
    (loc, tvar_id)
  (* 
   * ============================
   * Call or Tuple Constructor
   * ============================
   *)
  | Call { Call.loc; func; args } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    (* Determine if this call expression is a tuple constructor. If so handle tuple constructor
       directly by looking up ADT sig instead of recursing into function node. *)
    let is_ctor =
      match func with
      | Identifier { Ast.Identifier.loc; name }
      | ScopedIdentifier { Ast.ScopedIdentifier.name = { Ast.Identifier.loc; name }; _ } ->
        let binding = Type_context.get_value_binding ~cx loc in
        (match binding.declaration with
        | CtorDecl ctor_decl ->
          let adt_sig = Bindings.ConstructorDeclaration.get ctor_decl in
          (* This is an identifier reference of a decl type, so create fresh type args for this instance *)
          let adt = Types.refresh_adt_type_params adt_sig in
          (match SMap.find name adt_sig.variant_sigs with
          (* Error on incorrect number of arguments *)
          | TupleVariantSig elements when List.length elements <> List.length args ->
            Type_context.add_error
              ~cx
              loc
              (IncorrectTupleConstructorArity (List.length args, List.length elements));
            ignore (Type_context.unify ~cx Any (TVar tvar_id));
            true
          (* Supplied arguments must each be a subtype of the element types. Overall expression
             type is the ADT's type. *)
          | TupleVariantSig element_sigs ->
            let type_param_bindings = Types.get_adt_type_param_bindings adt in
            let args_locs_and_tvar_ids = List.map (check_expression ~cx) args in
            List.iter2
              (fun (arg_loc, arg_tvar_id) element_sig ->
                (* Substitute fresh type params for this instance in each element's signature *)
                let element_ty =
                  if adt_sig.type_params = [] then
                    element_sig
                  else
                    Types.substitute_type_params type_param_bindings element_sig
                in
                Type_context.assert_is_subtype ~cx arg_loc (TVar arg_tvar_id) element_ty)
              args_locs_and_tvar_ids
              element_sigs;
            ignore (Type_context.unify ~cx adt (TVar tvar_id));
            true
          (* Special error if record constructor is called as a function *)
          | RecordVariantSig _ ->
            Type_context.add_error ~cx loc (RecordConstructorCalled name);
            ignore (Type_context.unify ~cx Any (TVar tvar_id));
            true
          | EnumVariantSig -> false)
        | _ -> false)
      | _ -> false
    in
    (* Otherwise this is a regular function call *)
    ( if not is_ctor then
      let (func_loc, func_tvar_id) = check_expression ~cx func in
      let args_locs_and_tvar_ids = List.map (check_expression ~cx) args in
      let func_rep_ty = Type_context.find_rep_type ~cx (TVar func_tvar_id) in
      match func_rep_ty with
      (* Error on incorrect number of arguments *)
      | Function { params; _ } when List.length params <> List.length args ->
        Type_context.add_error
          ~cx
          loc
          (IncorrectFunctionArity (List.length args, List.length params));
        ignore (Type_context.unify ~cx Any (TVar tvar_id))
        (* Supplied arguments must each be a subtype of the annotated parameter type *)
      | Function { params; return } ->
        List.iter2
          (fun (arg_loc, arg_tvar_id) param ->
            Type_context.assert_is_subtype ~cx arg_loc (TVar arg_tvar_id) param)
          args_locs_and_tvar_ids
          params;
        ignore (Type_context.unify ~cx return (TVar tvar_id))
      (* Do not error on any being called *)
      | Any -> ignore (Type_context.unify ~cx Any (TVar tvar_id))
      (* Error if type other than a function is called *)
      | _ ->
        Type_context.add_error
          ~cx
          func_loc
          (NonFunctionCalled (Type_context.find_rep_type ~cx (TVar func_tvar_id)));
        ignore (Type_context.unify ~cx Any (TVar tvar_id)) );
    (loc, tvar_id)
  (*
   * ============================
   * Record Constructor
   * ============================
   *)
  | Record { Record.loc; name; fields } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    (* Determine whether scoped id is a record constructor *)
    let is_record_ty =
      match name with
      | Identifier { Ast.Identifier.loc = name_loc; name }
      | ScopedIdentifier { Ast.ScopedIdentifier.name = { Ast.Identifier.loc = name_loc; name }; _ }
        ->
        let binding = Type_context.get_value_binding ~cx name_loc in
        (match binding.declaration with
        | CtorDecl ctor_decl ->
          let adt_sig = Bindings.ConstructorDeclaration.get ctor_decl in
          (* This is an identifier reference of a decl type, so create fresh type args for this instance *)
          let adt = Types.refresh_adt_type_params adt_sig in
          (match SMap.find name adt_sig.variant_sigs with
          | RecordVariantSig field_sigs ->
            (* Recurse into fields and collect all fields that are not a part of this record *)
            let (field_args, unexpected_fields) =
              List.fold_left
                (fun (field_args, unexpected_fields)
                     { Record.Field.name = { Ast.Identifier.name; loc } as name_id; value; _ } ->
                  let field_arg =
                    match value with
                    | None -> check_expression ~cx (Identifier name_id)
                    | Some value -> check_expression ~cx value
                  in
                  if SMap.mem name field_sigs then
                    (SMap.add name field_arg field_args, unexpected_fields)
                  else
                    (field_args, (loc, name) :: unexpected_fields))
                (SMap.empty, [])
                fields
            in
            (* Collect all expected fields that are missing from this constructor invocation *)
            let missing_fields =
              SMap.fold
                (fun field_name _ missing_fields ->
                  if SMap.mem field_name field_args then
                    missing_fields
                  else
                    field_name :: missing_fields)
                field_sigs
                []
            in
            (* Error on unexpected or missing fields, only displaying missing fields if there are no
               unexpected fields. *)
            if unexpected_fields <> [] then
              List.iter
                (fun (loc, field_name) ->
                  Type_context.add_error
                    ~cx
                    loc
                    (UnexpectedRecordConstructorField (name, field_name)))
                (List.rev unexpected_fields)
            else if missing_fields <> [] then
              Type_context.add_error
                ~cx
                loc
                (MissingRecordConstructorFields (List.rev missing_fields));
            (* Supplied arguments must each be a subtype of the field types *)
            let type_param_bindings = Types.get_adt_type_param_bindings adt in
            SMap.iter
              (fun field_name (arg_loc, arg_tvar_id) ->
                (* Substitute fresh type args for this instance in each fields's signature *)
                let field_sig = SMap.find field_name field_sigs in
                let field_ty =
                  if adt_sig.type_params = [] then
                    field_sig
                  else
                    Types.substitute_type_params type_param_bindings field_sig
                in
                Type_context.assert_is_subtype ~cx arg_loc (TVar arg_tvar_id) field_ty)
              field_args;
            (* Result is algebraic data type unless the fields do not match,
               in which case propagate any *)
            let result_ty =
              if missing_fields = [] && unexpected_fields = [] then
                adt
              else
                Any
            in
            ignore (Type_context.unify ~cx result_ty (TVar tvar_id));
            true
          | _ -> false)
        | _ -> false)
      | _ -> false
    in
    (* Error if scoped id is not a record constructor *)
    if not is_record_ty then (
      Type_context.add_error ~cx (Ast_utils.expression_loc name) ExpectedRecordConstructor;
      ignore (Type_context.unify ~cx Any (TVar tvar_id))
    );
    (loc, tvar_id)
  (*
   * ============================
   * Indexed Access
   * ============================
   *)
  | IndexedAccess { IndexedAccess.loc; target; index } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (target_loc, target_tvar_id) = check_expression ~cx target in
    let (index_loc, index_tvar_id) = check_expression ~cx index in
    let check_tuple_indexed_access type_param_bindings elements =
      (* Verify that index is an int literal *)
      let index_rep_ty = Type_context.find_rep_type ~cx (TVar index_tvar_id) in
      match (index, index_rep_ty) with
      | (IntLiteral _, Types.IntLiteral { values = [(_, value)]; _ }) ->
        let value = Option.map Int64.to_int value in
        let ty =
          match value with
          | Some index when index >= 0 && index < List.length elements ->
            let element_ty = List.nth elements index in
            (* If there are type params, calculate type param to type arg bindings and subtitute
               type args for type params in sig element type. *)
            if IMap.is_empty type_param_bindings then
              element_ty
            else
              Types.substitute_type_params type_param_bindings element_ty
          | _ ->
            Type_context.add_error ~cx index_loc (TupleIndexOutOfBounds (List.length elements));
            Types.Any
        in
        ignore (Type_context.unify ~cx ty (TVar tvar_id))
      | _ ->
        Type_context.add_error ~cx index_loc TupleIndexIsNotLiteral;
        ignore (Type_context.unify ~cx Any (TVar tvar_id))
    in
    let check_array_indexed_access element_ty =
      (* Verify that index is an integer *)
      let index_rep_ty = Type_context.find_rep_type ~cx (TVar index_tvar_id) in
      match index_rep_ty with
      | Byte
      | Int
      | Long
      | IntLiteral _ ->
        ignore (Type_context.unify ~cx element_ty (TVar tvar_id))
      | ty ->
        Type_context.add_error ~cx index_loc (IndexIsNotInteger (Type_context.find_rep_type ~cx ty));
        ignore (Type_context.unify ~cx Any (TVar tvar_id))
    in
    let target_rep_ty = Type_context.find_rep_type ~cx (TVar target_tvar_id) in
    let is_indexable_ty =
      match target_rep_ty with
      (* Can index into tuple literal types *)
      | Tuple elements ->
        check_tuple_indexed_access IMap.empty elements;
        true
      (* Can only index into ADTs with a single tuple variant *)
      | ADT { adt_sig = { variant_sigs; _ }; _ } ->
        (match SMap.choose_opt variant_sigs with
        | Some (_, Types.TupleVariantSig element_sigs) when SMap.cardinal variant_sigs = 1 ->
          let type_param_bindings = Types.get_adt_type_param_bindings target_rep_ty in
          check_tuple_indexed_access type_param_bindings element_sigs;
          true
        | _ -> false)
      | Array element_ty ->
        check_array_indexed_access element_ty;
        true
      (* Propagate anys *)
      | Any ->
        ignore (Type_context.unify ~cx Any (TVar tvar_id));
        true
      | _ -> false
    in
    if not is_indexable_ty then (
      Type_context.add_error ~cx target_loc (NonIndexableIndexed target_rep_ty);
      ignore (Type_context.unify ~cx Any (TVar tvar_id))
    );
    (loc, tvar_id)
  (*
   * ============================
   * Named Access
   * ============================
   *)
  | NamedAccess { NamedAccess.loc; target; name = { Ast.Identifier.name = field_name; _ } } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (target_loc, target_tvar_id) = check_expression ~cx target in
    let target_rep_ty = Type_context.find_rep_type ~cx (TVar target_tvar_id) in
    let is_record_ty =
      match target_rep_ty with
      (* Can only index into ADTs with a single record variant *)
      | ADT { adt_sig = { name; variant_sigs; _ }; _ } ->
        (match SMap.choose_opt variant_sigs with
        | Some (_, Types.RecordVariantSig field_sigs) when SMap.cardinal variant_sigs = 1 ->
          (* Look up field in field signatures, erroring if field does not exist *)
          let result_ty =
            match SMap.find_opt field_name field_sigs with
            | None ->
              Type_context.add_error ~cx loc (NamedAccessNonexistentField (name, field_name));
              Types.Any
            | Some field_sig_ty ->
              (* If there are type params, calculate type param to type arg bindings and subtitute
                 type params for type args in sig field type. *)
              let type_param_bindings = Types.get_adt_type_param_bindings target_rep_ty in
              if type_param_bindings = IMap.empty then
                field_sig_ty
              else
                Types.substitute_type_params type_param_bindings field_sig_ty
          in
          ignore (Type_context.unify ~cx result_ty (TVar tvar_id));
          true
        | _ -> false)
      (* Propagate anys *)
      | Any ->
        ignore (Type_context.unify ~cx Any (TVar tvar_id));
        true
      | _ -> false
    in
    if not is_record_ty then (
      Type_context.add_error ~cx target_loc (NonAccessibleAccessed (field_name, target_rep_ty));
      ignore (Type_context.unify ~cx Types.Any (TVar tvar_id))
    );
    (loc, tvar_id)
  | Ternary _ -> failwith "TODO: Type checking for ternary expression"
  | Match _ -> failwith "TODO: Type check match expressions"

and check_pattern ~cx patt =
  let open Ast.Pattern in
  match patt with
  | Identifier { Ast.Identifier.loc; _ } ->
    let binding = Type_context.get_value_binding ~cx loc in
    let decl_tvar_id_opt =
      match binding.declaration with
      | VarDecl var_decl ->
        Some var_decl.tvar_id
        (* Represents an error, but should error in assignment.
           Cannot appear in variable declarations or match patterns. *)
      | CtorDecl _
      | FunDecl _
      | FunParamDecl _ ->
        None
    in
    let ty =
      match decl_tvar_id_opt with
      | Some tvar_id -> Types.TVar tvar_id
      | None -> Types.Any
    in
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx ty (TVar tvar_id));
    (loc, tvar_id)
  | Wildcard loc ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Any (TVar tvar_id));
    (loc, tvar_id)
  | Tuple { loc; name = None; elements } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let element_tys =
      List.map (fun element -> Types.TVar (snd (check_pattern ~cx element))) elements
    in
    let tuple_ty = Types.Tuple element_tys in
    ignore (Type_context.unify ~cx tuple_ty (Types.TVar tvar_id));
    (loc, tvar_id)
  | Tuple { loc; name = Some scoped_id; elements } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let element_locs_and_tvar_ids = List.map (fun element -> check_pattern ~cx element) elements in
    let tuple_adt_ty_opt =
      let { Ast.ScopedIdentifier.name = { Ast.Identifier.loc = name_loc; name }; _ } = scoped_id in
      let binding = Type_context.get_value_binding ~cx name_loc in
      match binding.declaration with
      | CtorDecl ctor_decl ->
        let adt_sig = Bindings.ConstructorDeclaration.get ctor_decl in
        let adt = Types.refresh_adt_type_params adt_sig in
        (match SMap.find name adt_sig.variant_sigs with
        | TupleVariantSig element_sigs when List.length element_sigs <> List.length elements ->
          Type_context.add_error
            ~cx
            loc
            (IncorrectTupleConstructorArity (List.length elements, List.length element_sigs));
          Some Types.Any
        | TupleVariantSig element_sigs ->
          List.iter2
            (fun (element_loc, element_tvar_id) element_sig_ty ->
              Type_context.assert_unify ~cx element_loc element_sig_ty (TVar element_tvar_id))
            element_locs_and_tvar_ids
            element_sigs;
          Some adt
        | _ -> None)
      | _ -> None
    in
    (* Error if scoped id is not a tuple constructor *)
    let ty =
      match tuple_adt_ty_opt with
      | None ->
        Type_context.add_error ~cx scoped_id.loc ExpectedTupleConstructor;
        Types.Any
      | Some ty -> ty
    in
    ignore (Type_context.unify ~cx ty (TVar tvar_id));
    (loc, tvar_id)
  | Record { loc; name = scoped_id; fields; _ } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let record_adt_ty_opt =
      let { Ast.ScopedIdentifier.name = { Ast.Identifier.loc = name_loc; name }; _ } = scoped_id in
      let binding = Type_context.get_value_binding ~cx name_loc in
      match binding.declaration with
      | CtorDecl ctor_decl ->
        let adt_sig = Bindings.ConstructorDeclaration.get ctor_decl in
        let adt = Types.refresh_adt_type_params adt_sig in
        (match SMap.find name adt_sig.variant_sigs with
        | RecordVariantSig field_sigs ->
          (* Recurse into fields and collect all fields that are not a part of this record *)
          let (field_params, unexpected_fields) =
            List.fold_left
              (fun (field_params, unexpected_fields) { Record.Field.name; value; _ } ->
                let { Ast.Identifier.name; loc } =
                  match name with
                  | None ->
                    (match value with
                    | Identifier id -> id
                    | _ -> failwith "Record shorthand field value must be an identifier")
                  | Some name -> name
                in
                let field_param = check_pattern ~cx value in
                if SMap.mem name field_sigs then
                  (SMap.add name field_param field_params, unexpected_fields)
                else
                  (field_params, (loc, name) :: unexpected_fields))
              (SMap.empty, [])
              fields
          in
          (* Collect all expected fields that are missing from this constructor invocation *)
          let missing_fields =
            SMap.fold
              (fun field_name _ missing_fields ->
                if SMap.mem field_name field_params then
                  missing_fields
                else
                  field_name :: missing_fields)
              field_sigs
              []
          in
          (* Error on unexpected or missing fields, only displaying missing fields if there are no
             unexpected fields. *)
          if unexpected_fields <> [] then
            List.iter
              (fun (loc, field_name) ->
                Type_context.add_error ~cx loc (UnexpectedRecordConstructorField (name, field_name)))
              (List.rev unexpected_fields)
          else if missing_fields <> [] then
            Type_context.add_error
              ~cx
              loc
              (MissingRecordConstructorFields (List.rev missing_fields));
          (* Supplied fields must each be a subtype of the field types *)
          SMap.iter
            (fun field_name (param_loc, param_tvar_id) ->
              let field_sig_ty = SMap.find field_name field_sigs in
              Type_context.assert_unify ~cx param_loc field_sig_ty (TVar param_tvar_id))
            field_params;
          (* Result is algebraic data type unless the fields do not match,
             in which case propagate any *)
          if missing_fields = [] && unexpected_fields = [] then
            Some adt
          else
            Some Any
        | _ -> None)
      | _ -> None
    in
    (* Error if scoped id is not a record constructor *)
    let ty =
      match record_adt_ty_opt with
      | None ->
        Type_context.add_error ~cx scoped_id.loc ExpectedRecordConstructor;
        Types.Any
      | Some ty -> ty
    in
    ignore (Type_context.unify ~cx ty (TVar tvar_id));
    (loc, tvar_id)
  | Literal _ -> failwith "TODO: Type check pattern literals"

and check_statement ~cx stmt =
  let open Ast.Statement in
  match stmt with
  | VariableDeclaration decl -> check_variable_declaration ~cx decl
  | FunctionDeclaration decl -> check_function_declaration ~cx decl
  | Expression (_, expr) -> ignore (check_expression ~cx expr)
  | Block { Block.statements; _ } -> List.iter (check_statement ~cx) statements
  | If { If.test; conseq; altern; _ } ->
    let (test_loc, test_tvar_id) = check_expression ~cx test in
    Type_context.assert_unify ~cx test_loc Bool (TVar test_tvar_id);
    check_statement ~cx conseq;
    Option.iter (check_statement ~cx) altern
  | While { While.test; body; _ } ->
    let (test_loc, test_tvar_id) = check_expression ~cx test in
    Type_context.assert_unify ~cx test_loc Bool (TVar test_tvar_id);
    check_statement ~cx body
  | Return { Return.loc; arg } ->
    let (arg_loc, arg_ty) =
      match arg with
      | None -> (loc, Types.Unit)
      | Some arg ->
        let (arg_loc, arg_tvar_id) = check_expression ~cx arg in
        (arg_loc, TVar arg_tvar_id)
    in
    (* Return argument must be subtype of function's return type stored in return type map *)
    let return_ty = LocMap.find loc (Type_context.get_return_types ~cx) in
    Type_context.assert_is_subtype ~cx arg_loc arg_ty return_ty
  | Break _
  | Continue _ ->
    ()
  (*
   * ============================
   * Assignment
   * ============================
   *)
  | Assignment { Assignment.lvalue; expr; _ } ->
    let lvalue_loc_and_tvar_opt =
      match lvalue with
      | Pattern pattern ->
        (* Check that every identifier referenced in lvalue can be reassigned *)
        let ids = Ast_utils.ids_of_pattern pattern in
        let has_error =
          List.fold_left
            (fun has_error { Ast.Identifier.loc; name } ->
              let binding = Type_context.get_value_binding ~cx loc in
              let add_invalid_assign_error kind =
                Type_context.add_error ~cx loc (InvalidAssignment (name, kind));
                true
              in
              match binding.declaration with
              | VarDecl { kind; _ } ->
                if kind = Ast.Statement.VariableDeclaration.Immutable then
                  add_invalid_assign_error InvalidAssignmentImmutableVariable
                else
                  has_error
              | FunDecl _ -> add_invalid_assign_error InvalidAssignmentFunction
              | FunParamDecl _ -> add_invalid_assign_error InvalidAssignmentFunctionParam
              | CtorDecl _ -> add_invalid_assign_error InvalidAssignmentConstructor)
            false
            ids
        in
        (* Check pattern, only checking against right hand side if no errors have been seen so far *)
        let loc_and_tvar_id = check_pattern ~cx pattern in
        if has_error then
          None
        else
          Some loc_and_tvar_id
      | Expression (IndexedAccess { Ast.Expression.IndexedAccess.loc; target; _ } as expr) ->
        (* Check lvalue expression *)
        let expr_loc_and_tvar_id = check_expression ~cx expr in
        (* If the lvalue is a tuple then error as tuple cannot have their fields assigned *)
        let target_tvar_id = Type_context.get_tvar_from_loc ~cx (Ast_utils.expression_loc target) in
        let target_rep_ty = Type_context.find_rep_type ~cx (TVar target_tvar_id) in
        (match target_rep_ty with
        (* Error for both anonymous tuples and named tuples (with no other variants) *)
        | Tuple _ ->
          Type_context.add_error ~cx loc (InvalidLValue InvalidLValueTuple);
          None
        | ADT { adt_sig = { variant_sigs; _ }; _ } when SMap.cardinal variant_sigs = 1 ->
          Type_context.add_error ~cx loc (InvalidLValue InvalidLValueTuple);
          None
        | _ -> Some expr_loc_and_tvar_id)
      | Expression expr -> Some (check_expression ~cx expr)
    in
    let (expr_loc, expr_tvar_id) = check_expression ~cx expr in
    (match lvalue_loc_and_tvar_opt with
    | Some (_, lvalue_tvar_id) ->
      Type_context.assert_is_subtype ~cx expr_loc (TVar expr_tvar_id) (TVar lvalue_tvar_id)
    | None -> ())
  | Match _ -> failwith "TODO: Type check match statements"

(* Resolve all IntLiteral placeholder types to an actual integer type. Infer as Int if all
   literals are within the Int range, otherwise infer as Long. *)
let resolve_unresolved_int_literals ~cx =
  while not (LocSet.is_empty (Type_context.get_unresolved_int_literals ~cx)) do
    let loc = LocSet.choose (Type_context.get_unresolved_int_literals ~cx) in
    let tvar = Type_context.get_tvar_from_loc ~cx loc in
    let ty = Type_context.find_rep_type ~cx (TVar tvar) in
    match ty with
    | IntLiteral ({ resolved = None; values; _ } as lit_ty) ->
      (* Default to int if all literals are within int range, otherwise use long *)
      let resolved_ty =
        List.fold_left
          (fun resolved_ty (_, value) ->
            match value with
            | Some value when Integers.is_out_of_int_range value -> Types.Long
            | Some _
            | None ->
              resolved_ty)
          Types.Int
          values
      in
      resolve_int_literal ~cx lit_ty resolved_ty
    | _ -> failwith "Unresolved int literal has already been resolved"
  done

(* Visit every expression, making sure that it has been resolved to a non-TVar type. *)
class ensure_expressions_typed_visitor ~cx =
  object
    inherit [unit] Ast_visitor.visitor as super

    method! function_ acc decl =
      let { Ast.Function.builtin; _ } = decl in
      if builtin then
        ()
      else
        super#function_ acc decl

    method! expression acc expr =
      let loc = Ast_utils.expression_loc expr in
      (match Type_context.get_tvar_from_loc_opt ~cx loc with
      (* Some expression nodes not appear in the tvar map, meaning they are never referenced and
         do not need to be checked. *)
      | None -> ()
      | Some tvar_id ->
        let rep_ty = Type_context.find_rep_type ~cx (TVar tvar_id) in
        (* Error if expression's type is not fully resolved *)
        let unresolved_tvars = Types.get_all_tvars [rep_ty] in
        if unresolved_tvars <> [] then
          let partial =
            match rep_ty with
            | TVar _ -> None
            | _ -> Some (rep_ty, unresolved_tvars)
          in
          Type_context.add_error ~cx loc (CannotInferType (CannotInferTypeExpression, partial)));
      super#expression acc expr
  end

let ensure_all_expression_are_typed ~cx modules =
  let visitor = new ensure_expressions_typed_visitor ~cx in
  List.iter (fun (_, module_) -> ignore (visitor#module_ () module_)) modules

let analyze ~cx modules =
  (* First visit type declarations, building type aliases *)
  List.iter (fun (_, module_) -> visit_type_declarations_prepass ~cx module_) modules;
  check_type_aliases_topologically ~cx modules;
  List.iter (fun (_, module_) -> visit_type_declarations ~cx module_) modules;
  List.iter (fun (_, module_) -> visit_value_declarations ~cx module_) modules;
  if Type_context.get_errors ~cx = [] then
    List.iter (fun (_, module_) -> check_module ~cx module_) modules;
  resolve_unresolved_int_literals ~cx;
  if Type_context.get_errors ~cx = [] then ensure_all_expression_are_typed ~cx modules;
  Type_context.set_errors ~cx (List.rev (Type_context.get_errors ~cx))
