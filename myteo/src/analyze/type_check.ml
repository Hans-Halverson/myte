open Name_resolution
open Type_context
open Analyze_error

let rec build_type ~cx ty =
  let open Ast.Type in
  match ty with
  | Primitive { Primitive.kind; _ } ->
    let open Primitive in
    (match kind with
    | Unit -> Types.Unit
    | Int -> Types.Int
    | String -> Types.String
    | Bool -> Types.Bool)
  | Function { Function.params; return; _ } ->
    Types.Function { params = List.map (build_type ~cx) params; return = build_type ~cx return }
  | Custom { Custom.name = { Ast.ScopedIdentifier.name = { Ast.Identifier.loc; _ }; _ }; _ } ->
    TVar (Type_context.get_tvar_id_from_type_use ~cx loc)

and visit_type_declarations ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | TypeDeclaration
          { Ast.TypeDeclaration.loc; name = { Ast.Identifier.loc = id_loc; name }; ty; _ } ->
        let tvar_id = Type_context.get_tvar_id_from_type_decl ~cx id_loc in
        let ty = build_type ~cx ty in
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
          Type_context.add_error
            ~cx
            loc
            (RecursiveTypeAlias (name, find_rep_tvar_id ~cx tvar_id, find_rep_type ~cx ty))
      | _ -> ())
    toplevels

and visit_value_declarations ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | VariableDeclaration decl -> check_variable_declaration ~cx ~decl_pass:true decl
      | FunctionDeclaration decl -> check_function_declaration ~cx ~decl_pass:true decl
      | _ -> ())
    toplevels

and check_module ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | VariableDeclaration decl -> check_variable_declaration ~cx ~decl_pass:false decl
      | FunctionDeclaration decl -> check_function_declaration ~cx ~decl_pass:false decl
      | TypeDeclaration _ -> ())
    toplevels

and check_variable_declaration ~cx ~decl_pass decl =
  let open Ast.Statement.VariableDeclaration in
  let { loc; pattern; init; annot; _ } = decl in
  let { Ast.Identifier.loc = id_loc; name } = identifier_in_pattern pattern in
  let tvar_id = Type_context.get_tvar_id_from_value_decl ~cx id_loc in
  begin
    match annot with
    | None -> if decl_pass then Type_context.add_error ~cx loc ToplevelVarWithoutAnnotation
    | Some annot ->
      let annot_ty = build_type ~cx annot in
      ignore (Type_context.unify ~cx annot_ty (TVar tvar_id))
  end;
  if not decl_pass then
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
    | Some _ ->
      if not (Type_context.is_subtype ~cx (TVar expr_tvar_id) (TVar tvar_id)) then
        Type_context.add_error
          ~cx
          expr_loc
          (IncompatibleTypes
             ( Type_context.find_rep_type ~cx (TVar expr_tvar_id),
               Type_context.find_rep_type ~cx (TVar tvar_id) ))

and check_function_declaration ~cx ~decl_pass decl =
  let open Ast.Identifier in
  let open Ast.Function in
  let open Ast.Function.Param in
  let { name = { loc = id_loc; _ }; params; return; body; _ } = decl in

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
      (* TODO: Check that expr's return type is subtype of annotated return type *)
      ignore (check_expression ~cx expr)
    | Block block ->
      (* TODO: Check that every return statement's expr is subtype of annotated return type *)
      check_statement ~cx (Ast.Statement.Block block)
  end

and check_expression ~cx expr =
  let open Ast.Expression in
  match expr with
  | Unit { Unit.loc } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Types.Unit (TVar tvar_id));
    (loc, tvar_id)
  | IntLiteral { IntLiteral.loc; _ } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Types.Int (TVar tvar_id));
    (loc, tvar_id)
  | StringLiteral { StringLiteral.loc; _ } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Types.String (TVar tvar_id));
    (loc, tvar_id)
  | BoolLiteral { BoolLiteral.loc; _ } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Types.Bool (TVar tvar_id));
    (loc, tvar_id)
  | Identifier { Ast.Identifier.loc; _ }
  | ScopedIdentifier { Ast.ScopedIdentifier.name = { Ast.Identifier.loc; _ }; _ } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let decl_tvar_id = Type_context.get_tvar_id_from_value_use ~cx loc in
    ignore (Type_context.unify ~cx (TVar decl_tvar_id) (TVar tvar_id));
    (loc, tvar_id)
  | TypeCast { TypeCast.loc; expr; ty } ->
    let (expr_loc, expr_tvar_id) = check_expression ~cx expr in
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let ty = build_type ~cx ty in
    ignore (Type_context.unify ~cx ty (TVar tvar_id));
    (* Expr must be a subtype of annotated type *)
    if not (Type_context.is_subtype ~cx (TVar expr_tvar_id) ty) then
      Type_context.add_error
        ~cx
        expr_loc
        (IncompatibleTypes
           (Type_context.find_rep_type ~cx (TVar expr_tvar_id), Type_context.find_rep_type ~cx ty));
    (loc, tvar_id)
  (* TODO: Implement remaining expressions *)
  | UnaryOperation { UnaryOperation.loc; _ }
  | BinaryOperation { BinaryOperation.loc; _ }
  | LogicalAnd { LogicalAnd.loc; _ }
  | LogicalOr { LogicalOr.loc; _ }
  | Call { Call.loc; _ }
  | Access { Access.loc; _ } ->
    (loc, Type_context.mk_tvar_id ~cx ~loc)

and check_statement ~cx stmt =
  let open Ast.Statement in
  match stmt with
  | VariableDeclaration decl -> check_variable_declaration ~cx ~decl_pass:false decl
  | FunctionDeclaration decl -> check_function_declaration ~cx ~decl_pass:false decl
  | Expression (_, expr) -> ignore (check_expression ~cx expr)
  | Block { Block.statements; _ } -> List.iter (check_statement ~cx) statements
  | If { If.test; conseq; altern; _ } ->
    let (test_loc, test_tvar_id) = check_expression ~cx test in
    if not (Type_context.unify ~cx Bool (TVar test_tvar_id)) then
      Type_context.add_error
        ~cx
        test_loc
        (IncompatibleTypes (Type_context.find_rep_type ~cx (TVar test_tvar_id), Bool));
    check_statement ~cx conseq;
    Option.iter (check_statement ~cx) altern
  | Return _ ->
    (* TODO: Implement remaining expressions *)
    ()

let analyze modules bindings =
  let cx = Type_context.mk ~bindings in
  (* First visit type declarations, building type aliases *)
  List.iter (fun (_, module_) -> visit_type_declarations ~cx module_) modules;
  List.iter (fun (_, module_) -> visit_value_declarations ~cx module_) modules;
  if cx.errors = [] then List.iter (fun (_, module_) -> check_module ~cx module_) modules;
  List.rev cx.errors
