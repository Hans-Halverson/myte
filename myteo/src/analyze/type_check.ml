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
  | Custom { Custom.name = { Ast.ScopedIdentifier.name = { Ast.Identifier.loc; _ }; _ }; _ } ->
    TVar (Type_context.get_tvar_id_from_type_use ~cx loc)

and visit_type_declarations_prepass ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      (* Create empty ADT sig for each algebraic data type definition *)
      let open Ast.TypeDeclaration in
      match toplevel with
      | TypeDeclaration
          { name = { Ast.Identifier.loc = id_loc; name }; decl = Tuple _ | Record _ | Variant _; _ }
        ->
        let tvar_id = Type_context.get_tvar_id_from_type_decl ~cx id_loc in
        let adt =
          Types.ADT { adt_sig = { name; tvar_sigs = []; variant_sigs = SMap.empty }; tparams = [] }
        in
        ignore (Type_context.unify ~cx adt (Types.TVar tvar_id))
      | _ -> ())
    toplevels

and visit_type_declarations ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      let open Ast.TypeDeclaration in
      match toplevel with
      (*
       * Type Alias
       * 
       * Type aliases have their tvar unified with the aliased type.
       *)
      | TypeDeclaration
          { loc; name = { Ast.Identifier.loc = id_loc; name }; type_params; decl = Alias alias } ->
        check_type_parameters ~cx type_params;
        let tvar_id = Type_context.get_tvar_id_from_type_decl ~cx id_loc in
        let ty = build_type ~cx alias in
        (* Check if the right hand side is a tvar that has been already unified with this type.
           This can only happen for recursive type aliases. *)
        let rep_ty1 = find_union_rep_type ~cx ty in
        let rep_ty2 = find_union_rep_type ~cx (TVar tvar_id) in
        let is_recursive =
          match (rep_ty1, rep_ty2) with
          | (TVar rep_tvar1, TVar rep_tvar2) when rep_tvar1 = rep_tvar2 -> true
          | _ -> false
        in
        if is_recursive || not (Type_context.unify ~cx ty (TVar tvar_id)) then
          Type_context.add_error ~cx loc (CyclicTypeAlias name)
      (*
       * Algebraic Data Type
       *
       * Build variant signatures for each variant in this ADT. Each variant's constructor id is
       * also unified with ADT.
       *)
      | TypeDeclaration { loc = _; name = { Ast.Identifier.loc = id_loc; name }; type_params; decl }
        ->
        check_type_parameters ~cx type_params;
        (* Get ADT signature from ADT id's tvar *)
        let tvar_id = Type_context.get_tvar_id_from_type_decl ~cx id_loc in
        let adt = Type_context.find_rep_type ~cx (TVar tvar_id) in
        let adt_sig =
          match adt with
          | Types.ADT { adt_sig; _ } -> adt_sig
          | _ -> failwith "Expected ADT"
        in
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
        let unify_constructor_with_adt ctor_decl_loc =
          let ctor_tvar = Type_context.get_tvar_id_from_value_decl ~cx ctor_decl_loc in
          ignore (Type_context.unify ~cx adt (TVar ctor_tvar))
        in
        (match decl with
        | Tuple { name = { Ast.Identifier.loc; _ }; elements; _ } ->
          unify_constructor_with_adt loc;
          let element_tys = build_element_tys ~cx elements in
          adt_sig.variant_sigs <- SMap.singleton name (Types.TupleVariantSig element_tys)
        | Record { name = { Ast.Identifier.loc; _ }; fields; _ } ->
          unify_constructor_with_adt loc;
          let field_tys = build_field_tys ~cx fields in
          adt_sig.variant_sigs <- SMap.singleton name (Types.RecordVariantSig field_tys)
        | Variant variants ->
          let variant_sigs =
            List.fold_left
              (fun variant_sigs variant ->
                let open Ast.Identifier in
                match variant with
                | EnumVariant { loc; name } ->
                  unify_constructor_with_adt loc;
                  SMap.add name Types.EnumVariantSig variant_sigs
                | TupleVariant { name = { loc; name }; elements; _ } ->
                  unify_constructor_with_adt loc;
                  let element_tys = build_element_tys ~cx elements in
                  SMap.add name (Types.TupleVariantSig element_tys) variant_sigs
                | RecordVariant { name = { loc; name }; fields; _ } ->
                  unify_constructor_with_adt loc;
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
      | FunctionDeclaration decl -> check_function_declaration ~cx ~decl_pass:true decl
      | _ -> ())
    toplevels

and check_module ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | VariableDeclaration decl -> check_variable_declaration ~cx decl
      | FunctionDeclaration decl -> check_function_declaration ~cx ~decl_pass:false decl
      | TypeDeclaration _ -> ())
    toplevels

(* Prepass to fill types of all global variables (and error if they are unannotated) *)
and check_toplevel_variable_declaration_prepass ~cx decl =
  let open Ast.Statement.VariableDeclaration in
  let { loc; pattern; annot; _ } = decl in
  (* TODO: Type check arbitrary patterns *)
  let { Ast.Identifier.loc = id_loc; name = _ } = List.hd (Ast_utils.ids_of_pattern pattern) in
  let tvar_id = Type_context.get_tvar_id_from_value_decl ~cx id_loc in
  match annot with
  | None -> Type_context.add_error ~cx loc ToplevelVarWithoutAnnotation
  | Some annot ->
    let annot_ty = build_type ~cx annot in
    ignore (Type_context.unify ~cx annot_ty (TVar tvar_id))

and check_variable_declaration ~cx decl =
  let open Ast.Statement.VariableDeclaration in
  let { loc; pattern; init; annot; _ } = decl in
  (* TODO: Type check arbitrary patterns *)
  let { Ast.Identifier.loc = id_loc; name } = List.hd (Ast_utils.ids_of_pattern pattern) in
  let tvar_id = Type_context.get_tvar_id_from_value_decl ~cx id_loc in
  let (expr_loc, expr_tvar_id) = check_expression ~cx init in
  match annot with
  | None ->
    (* If expression's type is fully resolved then use as type of id, otherwise error
       requesting an annotation. *)
    let rep_ty = Type_context.find_rep_type ~cx (TVar expr_tvar_id) in
    let unresolved_tvars = Types.get_all_tvars_with_duplicates rep_ty in
    if unresolved_tvars = [] then
      ignore (Type_context.unify ~cx (TVar expr_tvar_id) (TVar tvar_id))
    else
      (* TODO: Test this error once we support unresolved tvars *)
      let partial =
        match rep_ty with
        | TVar _ -> None
        | _ -> Some (rep_ty, List.hd unresolved_tvars)
      in
      Type_context.add_error ~cx loc (VarDeclNeedsAnnotation (name, partial))
  | Some annot ->
    let annot_ty = build_type ~cx annot in
    ignore (Type_context.unify ~cx annot_ty (TVar tvar_id));
    Type_context.assert_is_subtype ~cx expr_loc (TVar expr_tvar_id) (TVar tvar_id)

and check_type_parameters ~cx params =
  (* TODO: Add type checking for type parameters *)
  List.iter
    (fun { Ast.TypeParameter.loc; _ } ->
      let param_tvar_id = Type_context.get_tvar_id_from_type_decl ~cx loc in
      ignore (Type_context.unify ~cx Any (TVar param_tvar_id)))
    params

and check_function_declaration ~cx ~decl_pass decl =
  let open Ast.Identifier in
  let open Ast.Function in
  let open Ast.Function.Param in
  let { name = { loc = id_loc; _ }; params; return; body; type_params; loc = _ } = decl in
  check_type_parameters ~cx type_params;

  (* Bind annotated function type to function identifier *)
  let tvar_id = Type_context.get_tvar_id_from_value_decl ~cx id_loc in
  let param_tys = List.map (fun param -> build_type ~cx param.annot) params in
  let return_ty = Option.fold ~none:Types.Unit ~some:(fun return -> build_type ~cx return) return in
  let function_ty = Types.Function { params = param_tys; return = return_ty } in

  ignore (Type_context.unify ~cx function_ty (TVar tvar_id));
  if not decl_pass then begin
    (* Bind param id tvars to their annotated types *)
    List.combine params param_tys
    |> List.iter (fun (param, param_ty) ->
           let param_tvar_id = Type_context.get_tvar_id_from_value_decl ~cx param.name.loc in
           ignore (Type_context.unify ~cx param_ty (TVar param_tvar_id)));
    match body with
    | Expression expr ->
      (* Expression body must be subtype of return type *)
      let (expr_loc, expr_tvar_id) = check_expression ~cx expr in
      Type_context.assert_is_subtype ~cx expr_loc (TVar expr_tvar_id) return_ty
    | Block block ->
      let open Ast.Statement in
      let block_stmt = Block block in
      (* Annotate each return statement node with this function's return type *)
      Ast_utils.statement_visitor
        ~enter_functions:false
        ~f:(fun stmt ->
          match stmt with
          | Return { Return.loc; _ } ->
            Type_context.(cx.return_types <- LocMap.add loc return_ty cx.return_types)
          | _ -> ())
        block_stmt;
      check_statement ~cx block_stmt
  end

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
  | Identifier { Ast.Identifier.loc; name }
  | ScopedIdentifier { Ast.ScopedIdentifier.name = { Ast.Identifier.loc; name }; _ } ->
    let open Types in
    let open Bindings.ValueBinding in
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let binding = Type_context.get_source_value_binding ~cx loc in
    let decl_ty =
      match snd binding.declaration with
      (* If id is a constructor look up corresponding ADT to use as type. Error on tuple and record
         constructors as they are handled elsewhere. *)
      | CtorDecl ->
        let adt = Type_context.find_rep_type ~cx (TVar binding.tvar_id) in
        let adt_sig =
          match adt with
          | Types.ADT { adt_sig; _ } -> adt_sig
          | _ -> failwith "Expected ADT"
        in
        (match SMap.find name adt_sig.variant_sigs with
        | EnumVariantSig -> adt
        | TupleVariantSig elements ->
          Type_context.add_error ~cx loc (IncorrectTupleConstructorArity (0, List.length elements));
          Any
        | RecordVariantSig fields ->
          let field_names = SMap.fold (fun name _ names -> name :: names) fields [] |> List.rev in
          Type_context.add_error ~cx loc (MissingRecordConstructorFields field_names);
          Any)
      (* Otherwise identifier has same type as its declaration *)
      | _ -> TVar binding.tvar_id
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
      | ((Byte | Int | Long | IntLiteral _), (Plus | Minus))
      | (Bool, LogicalNot)
      | (Any, _) ->
        operand_rep_ty
      | _ ->
        let expected_ty =
          match op with
          | Plus
          | Minus ->
            Types.Int
          | LogicalNot -> Types.Bool
        in
        Type_context.add_error ~cx operand_loc (IncompatibleTypes (operand_rep_ty, [expected_ty]));
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
    | Divide ->
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
        let binding = Type_context.get_source_value_binding ~cx loc in
        (match snd binding.declaration with
        | CtorDecl ->
          let adt = Type_context.find_rep_type ~cx (TVar binding.tvar_id) in
          let adt_sig =
            match adt with
            | Types.ADT { adt_sig; _ } -> adt_sig
            | _ -> failwith "Expected ADT"
          in
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
          | TupleVariantSig elements ->
            let args_locs_and_tvar_ids = List.map (check_expression ~cx) args in
            List.iter2
              (fun (arg_loc, arg_tvar_id) element ->
                Type_context.assert_is_subtype ~cx arg_loc (TVar arg_tvar_id) element)
              args_locs_and_tvar_ids
              elements;
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
        let binding = Type_context.get_source_value_binding ~cx name_loc in
        (match snd binding.declaration with
        | CtorDecl ->
          let adt = Type_context.find_rep_type ~cx (TVar binding.tvar_id) in
          let adt_sig =
            match adt with
            | Types.ADT { adt_sig; _ } -> adt_sig
            | _ -> failwith "Expected ADT"
          in
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
            SMap.iter
              (fun field_name (arg_loc, arg_tvar_id) ->
                let field_sig_ty = SMap.find field_name field_sigs in
                Type_context.assert_is_subtype ~cx arg_loc (TVar arg_tvar_id) field_sig_ty)
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
    let check_tuple_indexed_access elements =
      let index_rep_ty = Type_context.find_rep_type ~cx (TVar index_tvar_id) in
      match (index, index_rep_ty) with
      | (IntLiteral _, Types.IntLiteral { values = [(_, value)]; _ }) ->
        let value = Option.map Int64.to_int value in
        let ty =
          match value with
          | Some index when index >= 0 && index < List.length elements -> List.nth elements index
          | _ ->
            Type_context.add_error ~cx index_loc (TupleIndexOutOfBounds (List.length elements));
            Types.Any
        in
        ignore (Type_context.unify ~cx ty (TVar tvar_id))
      | _ ->
        Type_context.add_error ~cx index_loc TupleIndexIsNotLiteral;
        ignore (Type_context.unify ~cx Any (TVar tvar_id))
    in
    let target_rep_ty = Type_context.find_rep_type ~cx (TVar target_tvar_id) in
    let is_indexable_ty =
      match target_rep_ty with
      (* Can index into tuple literal types *)
      | Tuple elements ->
        check_tuple_indexed_access elements;
        true
      (* Can only index into ADTs with a single tuple variant *)
      | ADT { adt_sig = { variant_sigs; _ }; _ } ->
        (match SMap.choose_opt variant_sigs with
        | Some (_, Types.TupleVariantSig element_sigs) when SMap.cardinal variant_sigs = 1 ->
          check_tuple_indexed_access element_sigs;
          true
        | _ -> false)
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
            | Some field_sig_ty -> field_sig_ty
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
      Type_context.add_error ~cx target_loc (NonAccessableAccessed (field_name, target_rep_ty));
      ignore (Type_context.unify ~cx Types.Any (TVar tvar_id))
    );
    (loc, tvar_id)
  | Ternary _ -> failwith "TODO: Type checking for ternary expression"

and check_statement ~cx stmt =
  let open Ast.Statement in
  match stmt with
  | VariableDeclaration decl -> check_variable_declaration ~cx decl
  | FunctionDeclaration decl -> check_function_declaration ~cx ~decl_pass:false decl
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
    let return_ty = Type_context.(LocMap.find loc cx.return_types) in
    Type_context.assert_is_subtype ~cx arg_loc arg_ty return_ty
  | Break _
  | Continue _ ->
    ()
  | Assignment { Assignment.pattern; expr; _ } ->
    (* TODO: Type check arbitrary patterns *)
    let { Ast.Identifier.loc = id_loc; _ } = List.hd (Ast_utils.ids_of_pattern pattern) in
    let tvar_id = Type_context.get_tvar_id_from_value_use ~cx id_loc in
    let (expr_loc, expr_tvar_id) = check_expression ~cx expr in
    Type_context.assert_is_subtype ~cx expr_loc (TVar expr_tvar_id) (TVar tvar_id)

let resolve_unresolved_int_literals ~cx =
  while not (LocSet.is_empty cx.unresolved_int_literals) do
    let loc = LocSet.choose cx.unresolved_int_literals in
    let tvar = LocMap.find loc cx.loc_to_tvar in
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

let analyze modules bindings =
  let cx = Type_context.mk ~bindings in
  (* First visit type declarations, building type aliases *)
  List.iter (fun (_, module_) -> visit_type_declarations_prepass ~cx module_) modules;
  List.iter (fun (_, module_) -> visit_type_declarations ~cx module_) modules;
  List.iter (fun (_, module_) -> visit_value_declarations ~cx module_) modules;
  if cx.errors = [] then List.iter (fun (_, module_) -> check_module ~cx module_) modules;
  resolve_unresolved_int_literals ~cx;
  cx.errors <- List.rev cx.errors;
  cx
