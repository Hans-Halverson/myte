open Analyze_error
open Basic_collections
open Types

let rec build_type ~cx ?(implicit_type_params = None) ty =
  let open Ast.Type in
  match ty with
  | Tuple { Tuple.elements; _ } ->
    Type.Tuple (List.map (build_type ~cx ~implicit_type_params) elements)
  | Function { Function.params; return; _ } ->
    Type.Function
      {
        type_args = [];
        params = List.map (build_type ~cx ~implicit_type_params) params;
        return = build_type ~cx ~implicit_type_params return;
      }
  | Identifier { Identifier.loc; name; type_params; _ } ->
    let type_param_arity_error actual expected =
      Type_context.add_error ~cx loc (IncorrectTypeParametersArity (actual, expected))
    in
    let type_args = List.map (build_type ~cx ~implicit_type_params) type_params in
    let num_type_args = List.length type_args in
    let binding = Type_context.get_type_binding ~cx name.name.loc in
    let mk_if_correct_arity arity mk_ty =
      if num_type_args = arity then
        mk_ty ()
      else (
        type_param_arity_error num_type_args arity;
        Type.Any
      )
    in
    (* Check if this is a builtin type *)
    (match Std_lib.lookup_stdlib_name binding.loc with
    | Some name when name = Std_lib.std_bool_bool -> mk_if_correct_arity 0 (fun _ -> Type.Bool)
    | Some name when name = Std_lib.std_byte_byte -> mk_if_correct_arity 0 (fun _ -> Type.Byte)
    | Some name when name = Std_lib.std_int_int -> mk_if_correct_arity 0 (fun _ -> Type.Int)
    | Some name when name = Std_lib.std_long_long -> mk_if_correct_arity 0 (fun _ -> Type.Long)
    | Some name when name = Std_lib.std_unit_unit -> mk_if_correct_arity 0 (fun _ -> Type.Unit)
    | Some name when name = Std_lib.std_string_string ->
      mk_if_correct_arity 0 Std_lib.mk_string_type
    | Some name when name = Std_lib.std_memory_array ->
      mk_if_correct_arity 1 (fun _ -> Type.Array (List.hd type_args))
    | _ ->
      (match binding.declaration with
      (* Type parameters can be used directly and do not take type parameters of their own *)
      | TypeParam type_param ->
        mk_if_correct_arity 0 (fun _ ->
            Type.TypeParam (Bindings.TypeParamDeclaration.get type_param))
      (* Substitute supplied type arguments for type parameters in body of type alias *)
      | TypeAlias { type_params; body } ->
        mk_if_correct_arity (List.length type_params) (fun _ ->
            let type_param_and_args = List.combine type_params type_args in
            let subst_map =
              List.fold_left
                (fun map (type_param, type_arg) -> IMap.add type_param.TypeParam.id type_arg map)
                IMap.empty
                type_param_and_args
            in
            Types.substitute_type_params subst_map body)
      (* Pass type args to ADT if they have the correct arity *)
      | TypeDecl type_decl ->
        let adt_sig = type_decl.adt_sig in
        mk_if_correct_arity (List.length adt_sig.type_params) (fun _ ->
            Type.ADT { adt_sig; type_args })
      (* A trait as a raw identifier is an implicit type parameter *)
      | TraitDecl trait_decl ->
        (match implicit_type_params with
        (* Implicit type parameters are only allowed in the types of function parameters *)
        | None ->
          Type_context.add_error ~cx loc ImplicitTypeParamOutsideFunction;
          Type.Any
        | Some implicit_type_params ->
          let trait_sig = trait_decl.trait_sig in
          let num_type_params = List.length trait_sig.type_params in
          if num_type_params <> num_type_args then (
            Type_context.add_error
              ~cx
              loc
              (IncorrectTypeParametersArity (num_type_args, num_type_params));
            Type.Any
          ) else
            (* Create implicit type parameter and save in list of implicits *)
            let bound = { TraitSig.trait_sig; type_args } in
            let type_param = TypeParam.mk ~name:Implicit ~bounds:[bound] in
            implicit_type_params := (type_param, loc) :: !implicit_type_params;
            Type.TypeParam type_param)))

and build_types_and_traits_prepass ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      let open Ast.TypeDeclaration in
      match toplevel with
      (* Create empty ADT sig for each algebraic data type definition *)
      | TypeDeclaration { name; decl = Tuple _ | Record _ | Variant _; type_params; _ } ->
        let binding = Type_context.get_type_binding ~cx name.loc in
        let type_decl = Bindings.get_type_decl binding in
        let type_params = build_type_parameters ~cx type_params in
        type_decl.adt_sig.type_params <- type_params
      (* Build type parameters for each trait *)
      | TraitDeclaration { name; type_params; _ } ->
        let binding = Type_context.get_type_binding_from_decl ~cx name.loc in
        let trait_decl = Bindings.get_trait_decl binding in
        let type_params = build_type_parameters ~cx type_params in
        trait_decl.trait_sig.type_params <- type_params
      | _ -> ())
    toplevels

and build_type_parameters ~cx params =
  List.map
    (fun { Ast.TypeParameter.name = { Ast.Identifier.loc; name }; bounds; _ } ->
      let binding = Type_context.get_type_binding ~cx loc in
      let type_param_decl = Bindings.get_type_param_decl binding in

      (* Build type parameter bounds, returning no bounds if error is encountered *)
      let (bounds, has_error) =
        List.fold_left
          (fun (bounds, has_error) { Ast.Type.Identifier.loc; name; type_params; _ } ->
            let binding = Type_context.get_type_binding ~cx name.name.loc in
            match binding.declaration with
            | TraitDecl trait_decl ->
              let trait_sig = trait_decl.trait_sig in
              let type_args = List.map (build_type ~cx) type_params in
              let num_type_args = List.length type_args in
              let num_type_params = List.length trait_sig.type_params in
              if num_type_args <> num_type_params then (
                Type_context.add_error
                  ~cx
                  loc
                  (IncorrectTypeParametersArity (num_type_args, num_type_params));
                ([], true)
              ) else
                let bound = { TraitSig.trait_sig; type_args } in
                (bound :: bounds, has_error)
            | _ ->
              Type_context.add_error ~cx loc NonTraitAsBound;
              ([], true))
          ([], false)
          bounds
      in
      let bounds =
        if has_error then
          []
        else
          List.rev bounds
      in
      let type_param = Types.TypeParam.mk ~name:(Explicit name) ~bounds in
      Bindings.TypeParamDeclaration.set type_param_decl type_param;
      type_param)
    params

and build_type_aliases ~cx modules =
  let open Ast.TypeDeclaration in
  let modules = List.map snd modules in
  try
    let aliases_in_topological_order = Type_alias.order_type_aliases ~cx modules in
    List.iter
      (fun alias ->
        match alias with
        | { loc = _; name; type_params; decl = Alias alias; builtin = _ } ->
          (* Save type parameters and type to alias declaration *)
          let binding = Type_context.get_type_binding ~cx name.loc in
          let alias_decl = Bindings.get_type_alias_decl binding in
          alias_decl.type_params <- build_type_parameters ~cx type_params;
          alias_decl.body <- build_type ~cx alias;
          (* Type alias parameters cannot have bounds *)
          List.iter
            (fun { Ast.TypeParameter.bounds; _ } ->
              if bounds <> [] then
                let full_loc = Loc.between (List.hd bounds).loc (List_utils.last bounds).loc in
                Type_context.add_error ~cx full_loc TypeAliasWithBounds)
            type_params
        | _ -> failwith "Expected type alias")
      aliases_in_topological_order
  with Type_alias.CyclicTypeAliasesException (loc, name) ->
    Type_context.add_error ~cx loc (CyclicTypeAlias name)

and build_type_declarations ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      let open Ast.TypeDeclaration in
      match toplevel with
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
        let type_decl = Bindings.get_type_decl binding in
        let adt_sig = type_decl.adt_sig in
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
        (match decl with
        | Tuple { elements; _ } ->
          let element_tys = build_element_tys ~cx elements in
          adt_sig.variants <- SMap.singleton name (AdtSig.Tuple element_tys)
        | Record { fields; _ } ->
          let field_tys = build_field_tys ~cx fields in
          adt_sig.variants <- SMap.singleton name (AdtSig.Record field_tys)
        | Variant variants ->
          let variant_sigs =
            List.fold_left
              (fun variant_sigs variant ->
                let open Ast.Identifier in
                match variant with
                | EnumVariant name -> SMap.add name.name AdtSig.Enum variant_sigs
                | TupleVariant { name; elements; _ } ->
                  let element_tys = build_element_tys ~cx elements in
                  SMap.add name.name (AdtSig.Tuple element_tys) variant_sigs
                | RecordVariant { name; fields; _ } ->
                  let field_tys = build_field_tys ~cx fields in
                  SMap.add name.name (AdtSig.Record field_tys) variant_sigs)
              SMap.empty
              variants
          in
          adt_sig.variants <- variant_sigs
        | Alias _ -> ())
      | _ -> ())
    toplevels

and build_trait_declarations ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  (* Build types for all traits *)
  List.iter
    (fun toplevel ->
      match toplevel with
      | TraitDeclaration { loc; name; methods; implemented; _ } ->
        let binding = Type_context.get_type_binding_from_decl ~cx name.loc in
        let trait_decl = Bindings.get_trait_decl binding in
        let trait_sig = trait_decl.trait_sig in

        (* Check arity of type params for type traits *)
        (match trait_sig.adt_sig with
        | None -> ()
        | Some adt_sig ->
          let num_type_params = List.length adt_sig.type_params in
          let num_trait_params = List.length trait_sig.type_params in
          if num_type_params <> num_trait_params then
            Type_context.add_error
              ~cx
              name.loc
              (IncorrectTypeParametersArity (num_trait_params, num_type_params)));

        (* Create `This` type alias for trait body *)
        let type_args =
          List.map (fun type_param -> Type.TypeParam type_param) trait_sig.type_params
        in
        let this_type =
          match trait_sig.adt_sig with
          (* ADT's `This` type is the ADT type *)
          | Some adt_sig ->
            (match Std_lib.get_primitive_type_for_adt_sig adt_sig with
            | Some primitive -> primitive
            | None -> Type.ADT { adt_sig; type_args })
          (* Trait's `This` type is a type parameter bounded by the trait *)
          | None ->
            let trait_bound = { TraitSig.trait_sig; type_args } in
            let type_param = TypeParam.mk ~name:(Explicit "This") ~bounds:[trait_bound] in
            trait_sig.this_type_param <- type_param;
            Type.TypeParam type_param
        in
        let this_type_binding = Type_context.get_type_binding ~cx loc in
        let this_type_alias = Bindings.get_type_alias_decl this_type_binding in
        this_type_alias.body <- this_type;

        (* Build types for all implemented traits *)
        List.iter
          (fun { Ast.TraitDeclaration.ImplementedTrait.name; type_args; _ } ->
            let type_args = List.map (build_type ~cx) type_args in
            let implemented = LocMap.find name.loc trait_decl.implemented in
            implemented.implemented_type_args <- type_args)
          implemented;
        (* Build types for all methods in trait *)
        List.iter (build_function_declaration ~cx) methods
      | _ -> ())
    toplevels

(* Check that each trait correctly implements all super traits. For a trait to implement a super
   trait, every method signature (and overridden method) of the super trait must have a
   corresponding method in the sub trait which is a subtype of the super method. *)
and build_trait_implementations ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | TraitDeclaration { loc; kind; name; implemented; _ } ->
        let open Bindings.TraitDeclaration in
        let binding = Type_context.get_type_binding_from_decl ~cx name.loc in
        let trait_decl = Bindings.get_trait_decl binding in
        let trait_sig = trait_decl.trait_sig in

        (* Fetch `This` type for trait body *)
        let this_type_binding = Type_context.get_type_binding ~cx loc in
        let this_type_alias = Bindings.get_type_alias_decl this_type_binding in

        (* Fill trait sig with base methods *)
        let this_instance =
          match this_type_alias.body with
          | Type.ADT { type_args; _ } -> { TraitSig.trait_sig; type_args }
          | TypeParam { bounds = [trait_instance]; _ } -> trait_instance
          | _ -> (* Primitive types in stdlib *) { TraitSig.trait_sig; type_args = [] }
        in
        SMap.iter
          (fun name { Bindings.FunctionDeclaration.loc; type_params; params; return; is_static; _ } ->
            if not is_static then
              let method_sig =
                {
                  MethodSig.loc;
                  trait_sig;
                  source_trait_instance = this_instance;
                  type_params;
                  params;
                  return;
                }
              in
              TraitSig.add_method trait_sig name method_sig)
          trait_decl.methods;

        (* Recursively traverse super traits, finding names of all non-overridden concrete methods.
           These methods can be ignored when checking for implemented methods, as they are already
           implemented in super traits (and will error in a super trait if incorrect). *)
        let rec gather_super_implemented_methods trait base_methods acc =
          let acc =
            SMap.fold
              (fun name { Bindings.FunctionDeclaration.is_signature; is_static; _ } acc ->
                if is_signature || is_static || SMap.mem name base_methods then
                  acc
                else
                  SSet.add name acc)
              trait.methods
              acc
          in
          LocMap.fold
            (fun _ { implemented_trait; _ } acc ->
              gather_super_implemented_methods implemented_trait base_methods acc)
            trait.implemented
            acc
        in
        let already_implemented_methods =
          LocMap.fold
            (fun _ { implemented_trait; _ } acc ->
              gather_super_implemented_methods implemented_trait trait_decl.methods acc)
            trait_decl.implemented
            SSet.empty
        in

        (* Check implementations of all super traits *)
        List.iter
          (fun { Ast.TraitDeclaration.ImplementedTrait.loc; name; _ } ->
            let implemented_trait = LocMap.find name.loc trait_decl.implemented in
            build_trait_implementation
              ~cx
              (trait_decl, kind, this_type_alias.body, loc, already_implemented_methods)
              implemented_trait
              IMap.empty)
          implemented
      | _ -> ())
    toplevels

(* Check whether the given base trait correctly implements all methods of a super trait
   (and transitively its super traits). Build up the methods and implemented traits for the given
   trait signature as we go. *)
and build_trait_implementation
    ~cx
    ( (trait_decl, trait_decl_kind, this_type, base_implemented_loc, already_implemented_methods) as
    base )
    { implemented_trait; implemented_type_args; _ }
    prev_type_param_bindings =
  (* Check that type param arity matches *)
  let num_type_args = List.length implemented_type_args in
  let num_type_params = List.length implemented_trait.trait_sig.type_params in
  if num_type_args <> num_type_params then
    Type_context.add_error
      ~cx
      base_implemented_loc
      (IncorrectTypeParametersArity (num_type_args, num_type_params))
  else
    (* Substitute type params from previous trait's body in implemented trait's type args,
       then create new type bindings for the body of the implemented trait. *)
    let implemented_type_args =
      List.map (Types.substitute_type_params prev_type_param_bindings) implemented_type_args
    in
    let type_param_bindings =
      Types.bind_type_params_to_args implemented_trait.trait_sig.type_params implemented_type_args
    in
    (* Substitute `This` type for base trait for `This` type of implemented trait *)
    let type_param_bindings =
      IMap.add implemented_trait.trait_sig.this_type_param.id this_type type_param_bindings
    in

    (* Add implemented trait to trait sig *)
    let implemented =
      { TraitSig.trait_sig = implemented_trait.trait_sig; type_args = implemented_type_args }
    in
    TraitSig.add_implemented trait_decl.trait_sig implemented;

    SMap.iter
      (fun method_name super_method ->
        let open Bindings.FunctionDeclaration in
        (* Add concrete method to trait sig *)
        ( if
          (not super_method.is_static)
          && ((not super_method.is_signature) || trait_decl_kind = Trait)
        then
          match SMap.find_opt method_name trait_decl.methods with
          (* If base method is defined this method has already been added to trait sig *)
          | Some _ -> ()
          (* Otherwise substitute propagated type params so that method is in terms of base trait *)
          | None ->
            let super_method_ty =
              Type.Function
                { type_args = []; params = super_method.params; return = super_method.return }
            in
            let super_method_ty =
              Types.substitute_type_params type_param_bindings super_method_ty
            in
            let (_, params, return) = Type_util.cast_to_function_type super_method_ty in
            let method_sig =
              {
                MethodSig.loc = super_method.loc;
                trait_sig = trait_decl.trait_sig;
                source_trait_instance = implemented;
                type_params = super_method.type_params;
                params;
                return;
              }
            in
            TraitSig.add_method trait_decl.trait_sig method_name method_sig );

        let is_already_implemented = SSet.mem method_name already_implemented_methods in
        if (not is_already_implemented) && not super_method.is_static then
          match SMap.find_opt method_name trait_decl.methods with
          | None ->
            if trait_decl_kind = Methods then
              Type_context.add_error
                ~cx
                base_implemented_loc
                (UnimplementedMethodSignature (method_name, implemented_trait.name))
          | Some sub_method ->
            (* Overridden methods must have same type parameter arity *)
            let num_sub_type_params = List.length sub_method.type_params in
            let num_super_type_params = List.length super_method.type_params in
            if num_sub_type_params <> num_super_type_params then
              Type_context.add_error
                ~cx
                sub_method.loc
                (IncorrectOverridenMethodTypeParametersArity
                   (method_name, implemented_trait.name, num_sub_type_params, num_super_type_params))
            else
              (* Add type param bindings from method's type parameters to bindings from trait *)
              let type_params = List.combine super_method.type_params sub_method.type_params in
              let type_param_bindings =
                List.fold_left
                  (fun map (super_param, sub_param) ->
                    IMap.add super_param.TypeParam.id (Type.TypeParam sub_param) map)
                  type_param_bindings
                  type_params
              in

              (* Substitute propagated type params in super method from base trait implementation,
                 then verify that sub method is a subtype of sup method. *)
              let sub_method_ty =
                Type.Function
                  { type_args = []; params = sub_method.params; return = sub_method.return }
              in
              let super_method_ty =
                Type.Function
                  { type_args = []; params = super_method.params; return = super_method.return }
              in
              let super_method_ty =
                Types.substitute_type_params type_param_bindings super_method_ty
              in
              if not (Type_context.is_subtype ~cx sub_method_ty super_method_ty) then
                let sub_rep_ty = Type_context.find_rep_type ~cx sub_method_ty in
                let sup_rep_ty = Type_context.find_rep_type ~cx super_method_ty in
                Type_context.add_error
                  ~cx
                  sub_method.loc
                  (IncompatibleOverridenMethodType
                     (method_name, implemented_trait.name, sub_rep_ty, sup_rep_ty)))
      implemented_trait.methods;

    (* Recursively check super traits of implemented trait *)
    LocMap.iter
      (fun _ super_trait -> build_trait_implementation ~cx base super_trait type_param_bindings)
      implemented_trait.implemented

and build_value_declarations ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | VariableDeclaration decl -> build_toplevel_variable_declaration ~cx decl
      | FunctionDeclaration decl -> build_function_declaration ~cx decl
      | _ -> ())
    toplevels

(* Prepass to fill types of all global variables (and error if they are unannotated) *)
and build_toplevel_variable_declaration ~cx decl =
  let open Ast.Statement.VariableDeclaration in
  let { loc; pattern; annot; _ } = decl in
  let (pattern_loc, pattern_tvar_id) = check_pattern ~cx pattern in
  match annot with
  | None -> Type_context.add_error ~cx loc ToplevelVarWithoutAnnotation
  | Some annot ->
    let annot_ty = build_type ~cx annot in
    Type_context.assert_unify ~cx pattern_loc annot_ty (Type.TVar pattern_tvar_id)

and check_module ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | VariableDeclaration decl -> check_variable_declaration ~cx decl
      | FunctionDeclaration decl -> if not decl.builtin then check_function_body ~cx decl
      | TraitDeclaration { loc; methods; _ } ->
        let this_type_binding = Type_context.get_type_binding ~cx loc in
        let this_type_alias = Bindings.get_type_alias_decl this_type_binding in
        List.iter
          (fun ({ Ast.Function.loc; body; static; _ } as method_) ->
            if body <> Signature then (
              (* Set up type of implicit `this` parameter for non-static methods  *)
              ( if not static then
                let this_param_binding = Type_context.get_value_binding ~cx loc in
                let this_param_decl = Bindings.get_func_param_decl this_param_binding in
                ignore (Type_context.unify ~cx this_type_alias.body (TVar this_param_decl.tvar)) );
              check_function_body ~cx method_
            ))
          methods
      | TypeDeclaration _ -> ())
    toplevels

(* Build the function type for a function declaration and set it as type of function identifier *)
and build_function_declaration ~cx decl =
  let open Ast.Function in
  let { name = { loc = id_loc; _ }; params; return; type_params; _ } = decl in
  let explicit_type_params = build_type_parameters ~cx type_params in
  let implicit_type_params = ref [] in
  let params =
    List.map
      (fun param ->
        build_type ~cx ~implicit_type_params:(Some implicit_type_params) param.Param.annot)
      params
  in
  let return = Option.fold ~none:Type.Unit ~some:(fun return -> build_type ~cx return) return in

  if Type_context.is_main_loc ~cx id_loc then check_main_declaration ~cx decl params return;

  (* Combine implicit type params with explicit type params in order they are declared *)
  let implicit_type_params =
    List.sort (fun (_, loc1) (_, loc2) -> Loc.compare loc1 loc2) !implicit_type_params
  in
  let type_params = explicit_type_params @ List.map fst implicit_type_params in

  (* Bind annotated function type and signature to function declaration *)
  let binding = Type_context.get_value_binding ~cx id_loc in
  let func_decl = Bindings.get_func_decl binding in
  func_decl.type_params <- type_params;
  func_decl.params <- params;
  func_decl.return <- return

(* Check whether the main function has the correct parameter and return types *)
and check_main_declaration ~cx decl params return =
  let has_valid_type_params = decl.type_params = [] in
  let has_valid_params =
    match params with
    | [] -> true
    | [Type.ADT { adt_sig = vec_adt_sig; type_args = [Type.ADT { adt_sig = string_adt_sig; _ }] }]
      when vec_adt_sig == !Std_lib.vec_adt_sig && string_adt_sig == !Std_lib.string_adt_sig ->
      true
    | _ -> false
  in
  let has_valid_return = return = Unit || return = Int in
  if (not has_valid_type_params) || (not has_valid_params) || not has_valid_return then
    Type_context.add_error ~cx decl.name.loc InvalidMainFunctionType

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

(* Type check function body, including binding types to function parameters and
   checking function return type. *)
and check_function_body ~cx decl =
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
         ignore (Type_context.unify ~cx param_ty (TVar param_decl.tvar)));

  match body with
  | Signature -> ()
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

and check_expression ~cx expr =
  let open Ast.Expression in
  match expr with
  (* 
   * ============================
   *         Literals
   * ============================
   *)
  | Unit { Unit.loc } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Type.Unit (TVar tvar_id));
    (loc, tvar_id)
  | IntLiteral { IntLiteral.loc; raw; base } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let int_literal_ty = Type_context.mk_int_literal_ty ~cx loc raw base in
    ignore (Type_context.unify ~cx int_literal_ty (TVar tvar_id));
    (loc, tvar_id)
  | StringLiteral { StringLiteral.loc; _ } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx (Std_lib.mk_string_type ()) (TVar tvar_id));
    (loc, tvar_id)
  | BoolLiteral { BoolLiteral.loc; _ } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Type.Bool (TVar tvar_id));
    (loc, tvar_id)
  (* 
   * ============================
   *         Identifiers
   * ============================
   *)
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
        let adt_sig = ctor_decl.adt_sig in
        (match SMap.find name adt_sig.variants with
        | Enum ->
          if adt_sig.type_params = [] then
            Type.ADT { adt_sig; type_args = [] }
          else
            Types.fresh_adt_instance adt_sig
        | Tuple elements ->
          Type_context.add_error ~cx loc (IncorrectTupleConstructorArity (0, List.length elements));
          Any
        | Record fields ->
          let field_names = SMap.fold (fun name _ names -> name :: names) fields [] |> List.rev in
          Type_context.add_error ~cx loc (MissingRecordConstructorFields field_names);
          Any)
      (* Id is for a function declaration. If function has type parameters then generate a fresh
         type variable for each type parameter and substitute into function type. *)
      | FunDecl func_decl ->
        if func_decl.type_params = [] then
          Type.Function { type_args = []; params = func_decl.params; return = func_decl.return }
        else
          Types.fresh_function_instance func_decl.type_params func_decl.params func_decl.return
      (* Otherwise identifier has same type as its declaration *)
      | FunParamDecl param_decl -> TVar param_decl.tvar
      | VarDecl var_decl -> TVar var_decl.tvar
    in
    ignore (Type_context.unify ~cx decl_ty (TVar tvar_id));
    (loc, tvar_id)
  (* 
   * ============================
   *       Tuple Literal
   * ============================
   *)
  | Tuple { Tuple.loc; elements } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let element_locs_and_tvar_ids = List.map (check_expression ~cx) elements in
    let element_tys = List.map (fun (_, tvar_id) -> Type.TVar tvar_id) element_locs_and_tvar_ids in
    ignore (Type_context.unify ~cx (Type.Tuple element_tys) (TVar tvar_id));
    (loc, tvar_id)
  (* 
   * ============================
   *         Type Cast
   * ============================
   *)
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
   *     Interpolated String
   * ============================
   *)
  | InterpolatedString { loc; parts; _ } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    List.iter
      (fun part ->
        match part with
        | InterpolatedString.String lit -> ignore (check_expression ~cx (StringLiteral lit))
        (* All interpolated expressions must implement ToString *)
        | Expression expr ->
          let (expr_loc, expr_tvar_id) = check_expression ~cx expr in
          let rep_ty = Type_context.find_rep_type ~cx (TVar expr_tvar_id) in
          (match rep_ty with
          (* We know that an IntLiteral will implement ToString *)
          | IntLiteral _ -> ()
          | _ ->
            if not (Type_context.implements_trait rep_ty !Std_lib.to_string_trait_sig) then
              Type_context.add_error ~cx expr_loc (InterpolatedExpressionRequiresToString rep_ty)))
      parts;
    ignore (Type_context.unify ~cx (Std_lib.mk_string_type ()) (TVar tvar_id));
    (loc, tvar_id)
  (* 
   * ============================
   *      Unary Operations
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
            [Type.Int]
          | Not -> [Type.Bool; Type.Int]
        in
        Type_context.add_error ~cx operand_loc (IncompatibleTypes (operand_rep_ty, expected_tys));
        Any
    in
    ignore (Type_context.unify ~cx result_ty (TVar tvar_id));
    (loc, tvar_id)
  (* 
   * ============================
   *      Binary Operations
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
      | Any
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
      | Any
      | Byte
      | Int
      | Long
      | IntLiteral _ ->
        true
      | ADT { adt_sig; _ } when adt_sig == !Std_lib.string_adt_sig -> true
      | _ -> false
    in
    let is_equatable tvar_id =
      let rep_ty = Type_context.find_rep_type ~cx (TVar tvar_id) in
      match rep_ty with
      (* We know that an IntLiteral will implement Equatable, but do not want to resolve it yet
         as it may be an expression such as `(x: Byte) == 1` where we can still infer its type. *)
      | IntLiteral _ -> true
      | _ -> Type_context.implements_trait rep_ty !Std_lib.equatable_trait_sig
    in
    let error_int loc tvar_id =
      Type_context.add_error
        ~cx
        loc
        (IncompatibleTypes (Type_context.find_rep_type ~cx (TVar tvar_id), [Type.Int]))
    in
    let error_int_or_string loc tvar_id =
      Type_context.add_error
        ~cx
        loc
        (IncompatibleTypes
           (Type_context.find_rep_type ~cx (TVar tvar_id), [Type.Int; Std_lib.mk_string_type ()]))
    in
    (match op with
    | Add
    | Subtract
    | Multiply
    | Divide
    | Remainder
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor ->
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
    | LeftShift
    | ArithmeticRightShift
    | LogicalRightShift ->
      (* If a left expression is an int propagate type to shift expression *)
      if is_int left_tvar_id then
        ignore (Type_context.unify ~cx (TVar left_tvar_id) (TVar tvar_id))
      else (
        error_int left_loc left_tvar_id;
        ignore (Type_context.unify ~cx Any (TVar tvar_id))
      );
      (* Right expression must also be an int, but does not need to be same type as left *)
      if not (is_int right_tvar_id) then error_int right_loc right_tvar_id
    (* All types can be compare with `is`, but types must be equal *)
    | Is ->
      Type_context.assert_unify ~cx right_loc (TVar left_tvar_id) (TVar right_tvar_id);
      ignore (Type_context.unify ~cx Type.Bool (TVar tvar_id))
    (* Types must be equal and implement the Equatable trait *)
    | Equal
    | NotEqual ->
      if is_equatable left_tvar_id then (
        Type_context.assert_unify ~cx right_loc (TVar left_tvar_id) (TVar right_tvar_id);
        ignore (Type_context.unify ~cx Type.Bool (TVar tvar_id))
      ) else if is_equatable right_tvar_id then (
        Type_context.assert_unify ~cx left_loc (TVar right_tvar_id) (TVar left_tvar_id);
        ignore (Type_context.unify ~cx Type.Bool (TVar tvar_id))
      ) else
        (* Otherwise force expression's type to be any to avoid erroring at uses *)
        let err_kind =
          if op = Equal then
            OperatorRequiresTraitEquals
          else
            OperatorRequirestRaitNotEquals
        in
        Type_context.add_error
          ~cx
          left_loc
          (OperatorRequiresTrait (err_kind, Type_context.find_rep_type ~cx (TVar left_tvar_id)));
        ignore (Type_context.unify ~cx Any (TVar tvar_id))
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
      ignore (Type_context.unify ~cx Type.Bool (TVar tvar_id)));
    (loc, tvar_id)
  | LogicalAnd { LogicalAnd.loc; left; right }
  | LogicalOr { LogicalOr.loc; left; right } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (left_loc, left_tvar_id) = check_expression ~cx left in
    let (right_loc, right_tvar_id) = check_expression ~cx right in
    Type_context.assert_unify ~cx left_loc Type.Bool (TVar left_tvar_id);
    Type_context.assert_unify ~cx right_loc Type.Bool (TVar right_tvar_id);
    ignore (Type_context.unify ~cx Type.Bool (TVar tvar_id));
    (loc, tvar_id)
  (* 
   * ============================
   *  Call or Tuple Constructor
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
          let adt_sig = ctor_decl.adt_sig in
          (* This is an identifier reference of a decl type, so create fresh type args for this instance *)
          let adt = Types.fresh_adt_instance adt_sig in
          (match SMap.find name adt_sig.variants with
          (* Error on incorrect number of arguments *)
          | Tuple elements when List.length elements <> List.length args ->
            Type_context.add_error
              ~cx
              loc
              (IncorrectTupleConstructorArity (List.length args, List.length elements));
            ignore (Type_context.unify ~cx Any (TVar tvar_id));
            true
          (* Supplied arguments must each be a subtype of the element types. Overall expression
             type is the ADT's type. *)
          | Tuple element_sigs ->
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
          | Record _ ->
            Type_context.add_error ~cx loc (RecordConstructorCalled name);
            ignore (Type_context.unify ~cx Any (TVar tvar_id));
            true
          | Enum -> false)
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
      | Function { type_args = _; params; return } ->
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
   *      Record Constructor
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
          let adt_sig = ctor_decl.adt_sig in
          (* This is an identifier reference of a decl type, so create fresh type args for this instance *)
          let adt = Types.fresh_adt_instance adt_sig in
          (match SMap.find name adt_sig.variants with
          | Record field_sigs ->
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
   *        Indexed Access
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
      | (IntLiteral _, IntLiteral { values = [(_, value)]; _ }) ->
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
            Type.Any
        in
        ignore (Type_context.unify ~cx ty (TVar tvar_id))
      | _ ->
        Type_context.add_error ~cx index_loc TupleIndexIsNotLiteral;
        ignore (Type_context.unify ~cx Any (TVar tvar_id))
    in
    let check_arrayish_indexed_access element_ty =
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
      (* Can index into Vec *)
      | ADT { adt_sig; type_args = [element_ty] } when adt_sig == !Std_lib.vec_adt_sig ->
        check_arrayish_indexed_access element_ty;
        true
      (* Can only index into ADTs with a single tuple variant *)
      | ADT { adt_sig = { variants; _ }; _ } ->
        (match SMap.choose_opt variants with
        | Some (_, Tuple element_sigs) when SMap.cardinal variants = 1 ->
          let type_param_bindings = Types.get_adt_type_param_bindings target_rep_ty in
          check_tuple_indexed_access type_param_bindings element_sigs;
          true
        | _ -> false)
      | Array element_ty ->
        check_arrayish_indexed_access element_ty;
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
   *        Named Access
   * ============================
   *)
  | NamedAccess { NamedAccess.loc; target; name } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (target_loc, target_tvar_id) = check_expression ~cx target in
    let target_rep_ty = Type_context.find_rep_type ~cx (TVar target_tvar_id) in

    (* Try to find a method with the given name in a set of trait sigs *)
    let try_resolve_method trait_sigs type_args =
      List.exists
        (fun { TraitSig.type_params = trait_type_params; methods; _ } ->
          match SMap.find_opt name.name methods with
          | None -> false
          | Some { MethodSig.type_params; params; return; _ } ->
            Type_context.add_method_use ~cx name.loc;
            (* Create fresh function type (refreshing only type params bound to the method itself).
               Also must substitute trait's type params with true type args for ADT, since method will
               be using trait's type args. *)
            let method_type = Types.fresh_function_instance type_params params return in
            let trait_type_param_bindings =
              Types.bind_type_params_to_args trait_type_params type_args
            in
            let method_type = Types.substitute_type_params trait_type_param_bindings method_type in
            ignore (Type_context.unify ~cx method_type (TVar tvar_id));
            true)
        trait_sigs
    in
    let try_resolve_adt_method { AdtSig.traits; _ } type_args =
      try_resolve_method traits type_args
    in
    let try_resolve_trait_bounds_method bounds =
      List.exists
        (fun { TraitSig.trait_sig; type_args } -> try_resolve_method [trait_sig] type_args)
        bounds
    in

    (* Try to resolve named access to a method access *)
    let is_resolved =
      match target_rep_ty with
      (* Propagate anys *)
      | Type.Any ->
        ignore (Type_context.unify ~cx Any (TVar tvar_id));
        true
      | Unit
      | Bool
      | Byte
      | Int
      | Long ->
        let adt_sig = Std_lib.get_primitive_adt_sig target_rep_ty in
        try_resolve_adt_method adt_sig []
      | IntLiteral lit_ty ->
        let resolved_ty = Type_context.resolve_int_literal_from_values ~cx lit_ty in
        let adt_sig = Std_lib.get_primitive_adt_sig resolved_ty in
        try_resolve_adt_method adt_sig []
      | ADT { adt_sig; type_args } -> try_resolve_adt_method adt_sig type_args
      | TypeParam { bounds; _ } -> try_resolve_trait_bounds_method bounds
      | TraitBound { bounds; _ } -> try_resolve_trait_bounds_method bounds
      | _ -> false
    in

    (* If named access does not resolve to a method, try to resolve to a field access *)
    let is_resolved =
      is_resolved
      ||
      match target_rep_ty with
      (* Can only index into ADTs with a single record variant *)
      | ADT { adt_sig = { variants; _ }; _ } ->
        (match SMap.choose_opt variants with
        | Some (_, Record field_sigs) when SMap.cardinal variants = 1 ->
          (match SMap.find_opt name.name field_sigs with
          | None -> false
          | Some field_sig_ty ->
            (* If there are type params, calculate type param to type arg bindings and substitute
               type params for type args in sig field type. *)
            let type_param_bindings = Types.get_adt_type_param_bindings target_rep_ty in
            let field_type = Types.substitute_type_params type_param_bindings field_sig_ty in
            ignore (Type_context.unify ~cx field_type (TVar tvar_id));
            true)
        | _ -> false)
      | _ -> false
    in

    if not is_resolved then (
      Type_context.add_error ~cx target_loc (UnresolvedNamedAccess (name.name, target_rep_ty));
      ignore (Type_context.unify ~cx Type.Any (TVar tvar_id))
    );
    (loc, tvar_id)
  | Ternary _ -> failwith "TODO: Type checking for ternary expression"
  | Match _ -> failwith "TODO: Type check match expressions"
  | Super _ -> failwith "TODO: Type check super expressions"

and check_pattern ~cx patt =
  let open Ast.Pattern in
  match patt with
  | Identifier { Ast.Identifier.loc; _ } ->
    let binding = Type_context.get_value_binding ~cx loc in
    let decl_tvar_id_opt =
      match binding.declaration with
      | VarDecl var_decl ->
        Some var_decl.tvar
        (* Represents an error, but should error in assignment.
           Cannot appear in variable declarations or match patterns. *)
      | CtorDecl _
      | FunDecl _
      | FunParamDecl _ ->
        None
    in
    let ty =
      match decl_tvar_id_opt with
      | Some tvar_id -> Type.TVar tvar_id
      | None -> Type.Any
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
      List.map (fun element -> Type.TVar (snd (check_pattern ~cx element))) elements
    in
    let tuple_ty = Type.Tuple element_tys in
    ignore (Type_context.unify ~cx tuple_ty (Type.TVar tvar_id));
    (loc, tvar_id)
  | Tuple { loc; name = Some scoped_id; elements } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let element_locs_and_tvar_ids = List.map (fun element -> check_pattern ~cx element) elements in
    let tuple_adt_ty_opt =
      let { Ast.ScopedIdentifier.name = { Ast.Identifier.loc = name_loc; name }; _ } = scoped_id in
      let binding = Type_context.get_value_binding ~cx name_loc in
      match binding.declaration with
      | CtorDecl ctor_decl ->
        let adt_sig = ctor_decl.adt_sig in
        let adt = Types.fresh_adt_instance adt_sig in
        (match SMap.find name adt_sig.variants with
        | Tuple element_sigs when List.length element_sigs <> List.length elements ->
          Type_context.add_error
            ~cx
            loc
            (IncorrectTupleConstructorArity (List.length elements, List.length element_sigs));
          Some Type.Any
        | Tuple element_sigs ->
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
        Type.Any
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
        let adt_sig = ctor_decl.adt_sig in
        let adt = Types.fresh_adt_instance adt_sig in
        (match SMap.find name adt_sig.variants with
        | Record field_sigs ->
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
        Type.Any
      | Some ty -> ty
    in
    ignore (Type_context.unify ~cx ty (TVar tvar_id));
    (loc, tvar_id)
  | Literal _ -> failwith "TODO: Type check pattern literals"

and check_statement ~cx stmt =
  let open Ast.Statement in
  match stmt with
  | VariableDeclaration decl -> check_variable_declaration ~cx decl
  | FunctionDeclaration decl ->
    build_function_declaration ~cx decl;
    check_function_body ~cx decl
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
      | None -> (loc, Type.Unit)
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
        | ADT { adt_sig = { variants; _ } as adt_sig; _ }
          when SMap.cardinal variants = 1 && adt_sig != !Std_lib.vec_adt_sig ->
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
    | IntLiteral lit_ty -> ignore (Type_context.resolve_int_literal_from_values ~cx lit_ty)
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
  List.iter (fun (_, module_) -> build_types_and_traits_prepass ~cx module_) modules;
  build_type_aliases ~cx modules;
  List.iter (fun (_, module_) -> build_type_declarations ~cx module_) modules;
  List.iter (fun (_, module_) -> build_trait_declarations ~cx module_) modules;
  if Type_context.get_errors ~cx = [] then
    List.iter (fun (_, module_) -> build_trait_implementations ~cx module_) modules;
  (* TODO: Check that type args satisfy super trait param bounds once trait implementations
     have been built. This needs to be done for all types created so far (ADTs, aliases, traits,
     trait implementations, and trait methods). *)
  if Type_context.get_errors ~cx = [] then
    List.iter (fun (_, module_) -> build_value_declarations ~cx module_) modules;
  if Type_context.get_errors ~cx = [] then
    List.iter (fun (_, module_) -> check_module ~cx module_) modules;
  resolve_unresolved_int_literals ~cx;
  if Type_context.get_errors ~cx = [] then ensure_all_expression_are_typed ~cx modules;
  Type_context.set_errors ~cx (List.rev (Type_context.get_errors ~cx))
