open Analyze_error
open Basic_collections
open Types

type trait_ctx =
  | TraitDisallowed
  | TraitImplicitParam of (TypeParam.t * Loc.t) list ref
  | TraitToBound

let rec build_type ~cx ?(check_type_param_bounds = true) ?(trait_ctx = TraitDisallowed) ty =
  let open Ast.Type in
  let build_type ty = build_type ~cx ~check_type_param_bounds ~trait_ctx ty in
  let type_param_arity_error loc actual expected =
    Type_context.add_error ~cx loc (IncorrectTypeParametersArity (actual, expected))
  in
  (* Check that bounds on type parameters are satisfied by type args, erroring if not *)
  let check_type_param_bounds_satisfied type_params type_args type_arg_nodes =
    if check_type_param_bounds then
      List_utils.iteri2
        (fun i type_param type_arg ->
          let rep_type_param =
            match Type_context.find_rep_type ~cx (TypeParam type_param) with
            | TypeParam rep_type_param -> rep_type_param
            | _ -> failwith "Rep type must be type param"
          in
          if rep_type_param.bounds <> [] then
            let rep_type_arg = Type_context.find_rep_type ~cx type_arg in
            if not (Type_context.type_satisfies_trait_bounds ~cx rep_type_arg rep_type_param.bounds)
            then
              let loc = Ast_utils.type_loc (List.nth type_arg_nodes i) in
              Type_context.add_error
                ~cx
                loc
                (IncompatibleTypes (rep_type_arg, [TypeParam rep_type_param])))
        type_params
        type_args
  in
  match ty with
  | Tuple { Tuple.elements; _ } -> Type.Tuple (List.map build_type elements)
  | Function { Function.params; return; _ } ->
    Type.Function
      { type_args = []; params = List.map build_type params; return = build_type return }
  | Identifier { Identifier.loc; name; type_args = type_arg_nodes; _ } ->
    let type_args = List.map build_type type_arg_nodes in
    let num_type_args = List.length type_args in
    let binding = Type_context.get_type_binding ~cx name.name.loc in
    let mk_if_correct_arity arity mk_ty =
      if num_type_args = arity then
        mk_ty ()
      else (
        type_param_arity_error loc num_type_args arity;
        Type.Any
      )
    in
    (* Check if this is a builtin type *)
    (match Std_lib.lookup_stdlib_name binding.loc with
    | Some name when name = Std_lib.std_bool_bool -> mk_if_correct_arity 0 (fun _ -> Type.Bool)
    | Some name when name = Std_lib.std_byte_byte -> mk_if_correct_arity 0 (fun _ -> Type.Byte)
    | Some name when name = Std_lib.std_int_int -> mk_if_correct_arity 0 (fun _ -> Type.Int)
    | Some name when name = Std_lib.std_long_long -> mk_if_correct_arity 0 (fun _ -> Type.Long)
    | Some name when name = Std_lib.std_never_never -> mk_if_correct_arity 0 (fun _ -> Type.Never)
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
        let num_type_params = List.length adt_sig.type_params in
        if num_type_args <> num_type_params then (
          type_param_arity_error loc num_type_args num_type_params;
          Type.Any
        ) else (
          check_type_param_bounds_satisfied adt_sig.type_params type_args type_arg_nodes;
          Type.ADT { adt_sig; type_args }
        )
      (* A trait as a raw identifier is an implicit type parameter *)
      | TraitDecl trait_decl ->
        (match trait_ctx with
        (* Implicit type parameters are only allowed in the types of function parameters *)
        | TraitDisallowed ->
          Type_context.add_error ~cx loc ImplicitTypeParamOutsideFunction;
          Type.Any
        | TraitImplicitParam _
        | TraitToBound ->
          let trait_sig = trait_decl.trait_sig in
          let num_type_params = List.length trait_sig.type_params in
          if num_type_params <> num_type_args then (
            type_param_arity_error loc num_type_args num_type_params;
            Type.Any
          ) else (
            check_type_param_bounds_satisfied trait_sig.type_params type_args type_arg_nodes;
            (* Create implicit type parameter and save in list of implicits *)
            let bound = { TraitSig.trait_sig; type_args } in
            match trait_ctx with
            | TraitImplicitParam implicit_type_params ->
              let type_param = TypeParam.mk ~name:Implicit ~bounds:[bound] in
              implicit_type_params := (type_param, loc) :: !implicit_type_params;
              Type.TypeParam type_param
            | _ -> Type.TraitBound bound
          ))))
  | Trait { trait = { loc; name; type_args = type_arg_nodes }; _ } ->
    let binding = Type_context.get_type_binding ~cx name.name.loc in
    let type_args = List.map build_type type_arg_nodes in
    (match binding.declaration with
    | TraitDecl { trait_sig = { can_be_trait_object = Some false; _ }; _ } ->
      Type_context.add_error ~cx loc InvalidTraitObject;
      Type.Any
    | TraitDecl { trait_sig; _ } ->
      if trait_sig.can_be_trait_object = None then
        Type_context.add_unchecked_trait_object_use ~cx trait_sig loc;
      let num_type_args = List.length type_args in
      let num_type_params = List.length trait_sig.type_params in
      if num_type_params <> num_type_args then (
        type_param_arity_error loc num_type_args num_type_params;
        Type.Any
      ) else (
        check_type_param_bounds_satisfied trait_sig.type_params type_args type_arg_nodes;
        Type.TraitObject { TraitSig.trait_sig; type_args }
      )
    | _ ->
      Type_context.add_error ~cx loc TraitObjectExpectsTrait;
      Type.Any)

(* Build type parameters for all types, traits, and aliases. Type parameter bounds are built, but
   are not yet checked for correctness. *)
and build_type_and_trait_parameters ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in

  (* Build type parameters without bounds for types, traits, and aliases. *)
  List.iter
    (fun toplevel ->
      let open Ast.TypeDeclaration in
      match toplevel with
      | TypeDeclaration { name; decl = Alias _; type_params; _ } ->
        let binding = Type_context.get_type_binding ~cx name.loc in
        let alias_decl = Bindings.get_type_alias_decl binding in
        alias_decl.type_params <- build_type_parameters ~cx type_params;
        (* Type alias parameters cannot have bounds *)
        List.iter
          (fun { Ast.TypeParameter.bounds; _ } ->
            if bounds <> [] then
              let full_loc = Loc.between (List.hd bounds).loc (List_utils.last bounds).loc in
              Type_context.add_error ~cx full_loc TypeAliasWithBounds)
          type_params
      | TypeDeclaration { name; decl = Builtin | Tuple _ | Record _ | Variant _; type_params; _ } ->
        let binding = Type_context.get_type_binding ~cx name.loc in
        let type_decl = Bindings.get_type_decl binding in
        let type_params = build_type_parameters ~cx type_params in
        type_decl.adt_sig.type_params <- type_params
      | TraitDeclaration { name; type_params; _ } ->
        let binding = Type_context.get_type_binding_from_decl ~cx name.loc in
        let trait_decl = Bindings.get_trait_decl binding in
        let type_params = build_type_parameters ~cx type_params in
        trait_decl.trait_sig.type_params <- type_params
      | _ -> ())
    toplevels;

  (* Add bounds to type parameters checking structure, but not checking trait implementation *)
  List.iter
    (fun toplevel ->
      let open Ast.TypeDeclaration in
      match toplevel with
      | TypeDeclaration { name; decl = Alias _; type_params; _ } ->
        let binding = Type_context.get_type_binding ~cx name.loc in
        let alias_decl = Bindings.get_type_alias_decl binding in
        build_type_parameter_bounds
          ~cx
          ~check_type_param_bounds:false
          type_params
          alias_decl.type_params
      | TypeDeclaration { name; decl = Builtin | Tuple _ | Record _ | Variant _; type_params; _ } ->
        let binding = Type_context.get_type_binding ~cx name.loc in
        let type_decl = Bindings.get_type_decl binding in
        build_type_parameter_bounds
          ~cx
          ~check_type_param_bounds:false
          type_params
          type_decl.adt_sig.type_params
      | TraitDeclaration { name; type_params; _ } ->
        let binding = Type_context.get_type_binding_from_decl ~cx name.loc in
        let trait_decl = Bindings.get_trait_decl binding in
        build_type_parameter_bounds
          ~cx
          ~check_type_param_bounds:false
          type_params
          trait_decl.trait_sig.type_params
      | _ -> ())
    toplevels

and build_type_parameters ~cx params =
  List.map
    (fun { Ast.TypeParameter.name = { Ast.Identifier.loc; name }; _ } ->
      let binding = Type_context.get_type_binding ~cx loc in
      let type_param_decl = Bindings.get_type_param_decl binding in
      let type_param = Types.TypeParam.mk ~name:(Explicit name) ~bounds:[] in
      Bindings.TypeParamDeclaration.set type_param_decl type_param;
      type_param)
    params

and build_type_parameter_bounds ~cx ~check_type_param_bounds param_nodes type_params =
  List.iter2
    (fun { Ast.TypeParameter.bounds; _ } type_param ->
      (* Build type parameter bounds, returning no bounds if error is encountered *)
      let (bounds, has_error) =
        List.fold_left
          (fun (bounds, has_error) { Ast.Type.Identifier.loc; name; type_args; _ } ->
            let binding = Type_context.get_type_binding ~cx name.name.loc in
            match binding.declaration with
            | TraitDecl trait_decl ->
              let trait_sig = trait_decl.trait_sig in
              let type_args = List.map (build_type ~cx ~check_type_param_bounds) type_args in
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
      if not has_error then type_param.TypeParam.bounds <- List.rev bounds)
    param_nodes
    type_params

and build_type_aliases ~cx modules =
  let open Ast.TypeDeclaration in
  let modules = List.map snd modules in
  match Type_alias.order_type_aliases ~cx modules with
  | Ok aliases_in_topological_order ->
    List.iter
      (fun alias ->
        match alias with
        | { name; decl = Alias alias; _ } ->
          let binding = Type_context.get_type_binding ~cx name.loc in
          let alias_decl = Bindings.get_type_alias_decl binding in
          alias_decl.body <- build_type ~cx ~check_type_param_bounds:false alias
        | _ -> ())
      aliases_in_topological_order
  | Error { loc; name; _ } -> Type_context.add_error ~cx loc (CyclicTypeAlias name.name)

(* Build the trait hierarchy for the program, determining which super traits are implemented by
   each trait and type. *)
and build_trait_hierarchy ~cx modules =
  let open Ast.TraitDeclaration in
  let get_trait_decl ~cx loc =
    Type_context.get_type_binding_from_decl ~cx loc |> Bindings.get_trait_decl
  in
  let set_this_type ~cx loc this_type =
    let this_type_binding = Type_context.get_type_binding ~cx loc in
    let this_type_alias = Bindings.get_type_alias_decl this_type_binding in
    this_type_alias.body <- this_type
  in

  (* Determine implemented traits for every trait, visiting in known topological order *)
  let ordered_traits = Type_context.get_ordered_traits ~cx in
  List.iter
    (fun { loc; name; implemented; _ } ->
      (* Fetch trait signature *)
      let trait_decl = get_trait_decl ~cx name.loc in
      let trait_sig = trait_decl.trait_sig in

      (* Create bounded type param to be used as `This` type within body of trait *)
      let type_args = List.map (fun param -> Type.TypeParam param) trait_sig.type_params in
      let trait_bound = { TraitSig.trait_sig; type_args } in
      let this_type_param = TypeParam.mk ~name:(Explicit "This") ~bounds:[trait_bound] in
      let this_type = Type.TypeParam this_type_param in

      set_this_type ~cx loc this_type;
      trait_sig.this_type_param <- this_type_param;

      (* Add all implemented traits to trait sig *)
      List.iter
        (fun implemented_trait ->
          build_implemented_traits ~cx trait_decl implemented_trait this_type)
        implemented)
    ordered_traits;

  (* Determine implemented traits for every type trait *)
  List.iter
    (fun (_, module_) ->
      List.iter
        (fun toplevel ->
          match toplevel with
          | Ast.Module.TraitDeclaration { kind = Methods; loc; name; implemented; _ } ->
            (* Fetch trait signature *)
            let trait_decl = get_trait_decl ~cx name.loc in
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

            (* ADT's `This` type is the ADT type *)
            let type_args = List.map (fun param -> Type.TypeParam param) trait_sig.type_params in
            let adt_sig = Option.get trait_sig.adt_sig in
            let this_type =
              match Std_lib.get_primitive_type_for_adt_sig adt_sig with
              | Some primitive -> primitive
              | None -> Type.ADT { adt_sig; type_args }
            in

            set_this_type ~cx loc this_type;

            (* Add all implemented traits to trait sig *)
            List.iter
              (fun implemented_trait ->
                build_implemented_traits ~cx trait_decl implemented_trait this_type)
              implemented
          | _ -> ())
        module_.Ast.Module.toplevels)
    modules

(* Add all implemented traits to a given trait decl's trait sig. This includes all super implemented
   traits, whose type args will be calculated in terms of the trait decl's params. *)
and build_implemented_traits ~cx trait_decl implemented_trait this_type =
  let { Ast.Type.Identifier.loc; name; type_args; _ } = implemented_trait in

  (* Build type args for implemented trait, and check arity of type args *)
  let type_args = List.map (build_type ~cx ~check_type_param_bounds:false) type_args in
  let trait_sig = (LocMap.find name.loc trait_decl.implemented).trait_sig in
  let num_type_args = List.length type_args in
  let num_type_params = List.length trait_sig.type_params in
  if num_type_args <> num_type_params then
    Type_context.add_error ~cx loc (IncorrectTypeParametersArity (num_type_args, num_type_params))
  else (
    (* Add implemented trait to trait sig *)
    TraitSig.add_implemented trait_decl.trait_sig loc { trait_sig; type_args };

    (* Create type param bindings for inside of trait sig, including binding `This` *)
    let type_param_bindings = Types.bind_type_params_to_args trait_sig.type_params type_args in
    let type_param_bindings = IMap.add trait_sig.this_type_param.id this_type type_param_bindings in

    (* Add all super implemented traits to trait sig, making sure to substitute type params in terms
       of sub trait sig. *)
    List.iter
      (fun (_, { TraitSig.trait_sig; type_args }) ->
        let type_args = List.map (Types.substitute_type_params type_param_bindings) type_args in
        TraitSig.add_implemented trait_decl.trait_sig loc { trait_sig; type_args })
      trait_sig.implemented
  )

(* Re-build all types built so far, this type checking that trait bounds are satisfied for any
   instantiated type parameters. Needs to recheck type and trait type parameters (as bounds may
   themselves contain parameterized types), type alias bodies, and traits' implemented types. *)
and recheck_type_parameters ~cx module_ =
  let recheck_type ~cx ty = ignore (build_type ~cx ~trait_ctx:(TraitImplicitParam (ref [])) ty) in
  let recheck_type_param ~cx { Ast.TypeParameter.bounds; _ } =
    List.iter (fun bound -> recheck_type ~cx (Ast.Type.Identifier bound)) bounds
  in
  List.iter
    (fun toplevel ->
      match toplevel with
      | Ast.Module.TypeDeclaration { type_params; decl; _ } ->
        List.iter (recheck_type_param ~cx) type_params;
        (match decl with
        | Alias body -> recheck_type ~cx body
        | _ -> ())
      | TraitDeclaration { type_params; implemented; _ } ->
        List.iter (recheck_type_param ~cx) type_params;
        List.iter
          (fun implemented -> recheck_type ~cx (Ast.Type.Identifier implemented))
          implemented
      | _ -> ())
    module_.Ast.Module.toplevels

and build_declarations ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | TypeDeclaration decl -> build_type_declaration ~cx decl
      | VariableDeclaration decl -> build_toplevel_variable_declaration ~cx decl
      | FunctionDeclaration decl -> build_function_declaration ~cx decl
      | TraitDeclaration { methods; _ } -> List.iter (build_function_declaration ~cx) methods)
    toplevels

(* Build signatures for algebraic data types. A signature for the entire type will be built
   for tuples and records, and a signature for each variant will be built for variant types. *)
and build_type_declaration ~cx decl =
  let open Ast.TypeDeclaration in
  let get_adt_sig loc =
    let binding = Type_context.get_type_binding ~cx loc in
    let type_decl = Bindings.get_type_decl binding in
    type_decl.adt_sig
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
  match decl with
  | { name; decl = Tuple { loc; elements; _ }; _ } ->
    let adt_sig = get_adt_sig name.loc in
    let element_tys = build_element_tys ~cx elements in
    let variant = { AdtSig.Variant.name = name.name; loc; kind = Tuple element_tys } in
    adt_sig.variants <- SMap.singleton name.name variant
  | { name; decl = Record { loc; fields; _ }; _ } ->
    let adt_sig = get_adt_sig name.loc in
    let field_tys = build_field_tys ~cx fields in
    let variant = { AdtSig.Variant.name = name.name; loc; kind = Record field_tys } in
    adt_sig.variants <- SMap.singleton name.name variant
  | { name; decl = Variant variants; _ } ->
    let adt_sig = get_adt_sig name.loc in
    let variant_sigs =
      List.fold_left
        (fun variant_sigs variant ->
          let open Ast.Identifier in
          match variant with
          | EnumVariant name ->
            let variant_sig = { AdtSig.Variant.name = name.name; loc = name.loc; kind = Enum } in
            SMap.add name.name variant_sig variant_sigs
          | TupleVariant { loc; name; elements; _ } ->
            let element_tys = build_element_tys ~cx elements in
            let variant_sig = { AdtSig.Variant.name = name.name; loc; kind = Tuple element_tys } in
            SMap.add name.name variant_sig variant_sigs
          | RecordVariant { loc; name; fields; _ } ->
            let field_tys = build_field_tys ~cx fields in
            let variant_sig = { AdtSig.Variant.name = name.name; loc; kind = Record field_tys } in
            SMap.add name.name variant_sig variant_sigs)
        SMap.empty
        variants
    in
    adt_sig.variants <- variant_sigs
  | _ -> ()

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

(* Build the function type for a function declaration and set it as type of function identifier *)
and build_function_declaration ~cx decl =
  let open Ast.Function in
  let { name = { loc = id_loc; _ }; params; return; type_params; body; _ } = decl in
  let explicit_type_params = build_type_parameters ~cx type_params in
  build_type_parameter_bounds ~cx ~check_type_param_bounds:true type_params explicit_type_params;

  (* Traits in function parameters are converted to bounded implicit type params *)
  let implicit_type_params = ref [] in
  let params =
    List.map
      (fun param ->
        build_type ~cx ~trait_ctx:(TraitImplicitParam implicit_type_params) param.Param.annot)
      params
  in

  (* Traits are only allowed in return type if function declaration is a method signature, where
     they are considered upper bounds, as a concrete type must be used in any implementation. *)
  let return =
    Option.fold
      ~none:Type.Unit
      ~some:(fun return ->
        if body = Signature then
          build_type ~cx ~trait_ctx:TraitToBound return
        else
          build_type ~cx return)
      return
  in

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
  let has_valid_params = params = [] in
  let has_valid_return = return = Unit || return = Int in
  if (not has_valid_type_params) || (not has_valid_params) || not has_valid_return then
    Type_context.add_error ~cx decl.name.loc InvalidMainFunctionType

(* Check that all traits correctly implement the methods of their super traits. Build method sigs
   on the checked trait sig as we go. *)
and check_trait_implementations ~cx modules =
  let check_trait_implementation ~cx { Ast.TraitDeclaration.loc; name; _ } =
    let binding = Type_context.get_type_binding_from_decl ~cx name.loc in
    let trait_decl = Bindings.get_trait_decl binding in
    let trait_sig = trait_decl.trait_sig in

    (* Fetch `This` type for trait body *)
    let this_type_binding = Type_context.get_type_binding ~cx loc in
    let this_type_alias = Bindings.get_type_alias_decl this_type_binding in

    (* Fill trait sig with base methods *)
    let (this_instance, this_type_param) =
      match this_type_alias.body with
      | Type.ADT { type_args; _ } -> ({ TraitSig.trait_sig; type_args }, None)
      | TypeParam ({ bounds = [trait_instance]; _ } as type_param) ->
        (trait_instance, Some type_param)
      | _ -> (* Primitive types in stdlib *) ({ TraitSig.trait_sig; type_args = [] }, None)
    in
    let open Bindings.FunctionDeclaration in
    SMap.iter
      (fun name { loc; type_params; params; return; is_static; is_signature; _ } ->
        if not is_static then
          let method_sig =
            {
              MethodSig.loc;
              trait_sig;
              source_trait_instance = this_instance;
              is_signature;
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
    let open Bindings.TraitDeclaration in
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
        (fun _ implemented_trait acc ->
          gather_super_implemented_methods implemented_trait base_methods acc)
        trait.implemented
        acc
    in
    let already_implemented_methods =
      LocMap.fold
        (fun _ implemented_trait acc ->
          gather_super_implemented_methods implemented_trait trait_decl.methods acc)
        trait_decl.implemented
        SSet.empty
    in

    List.iter
      (fun (loc, implemented) ->
        check_implemented_methods
          ~cx
          trait_sig
          implemented
          loc
          this_type_alias.body
          already_implemented_methods)
      trait_sig.implemented;

    (* For non-type traits, determine whether trait can become a trait object *)
    match this_type_param with
    | None -> ()
    | Some this_type_param ->
      (* Trait cannot be a trait object if any methods have type parameters or the `This` type *)
      let cannot_be_trait_object =
        SMap.exists
          (fun _ { MethodSig.type_params; params; return; _ } ->
            type_params <> []
            || List.exists (has_type_param this_type_param) params
            || has_type_param this_type_param return)
          trait_sig.methods
      in
      (* Or if it extends a trait that cannot be a trait object *)
      let cannot_be_trait_object =
        cannot_be_trait_object
        || List.exists
             (fun (_, { TraitSig.trait_sig; _ }) -> not (Option.get trait_sig.can_be_trait_object))
             trait_sig.implemented
      in
      trait_sig.can_be_trait_object <- Some (not cannot_be_trait_object)
  in

  (* Check traits in topological order, followed by all type traits *)
  let ordered_traits = Type_context.get_ordered_traits ~cx in
  List.iter (check_trait_implementation ~cx) ordered_traits;
  List.iter
    (fun (_, module_) ->
      List.iter
        (fun toplevel ->
          match toplevel with
          | Ast.Module.TraitDeclaration ({ kind = Methods; _ } as decl) ->
            check_trait_implementation ~cx decl
          | _ -> ())
        module_.Ast.Module.toplevels)
    modules

(* Check that a trait with the given trait sig implements all methods of a super trait *)
and check_implemented_methods
    ~cx
    (trait_sig : TraitSig.t)
    (implemented : TraitSig.instance)
    implemented_loc
    this_type
    already_implemented_methods =
  let is_trait = trait_sig.adt_sig = None in

  (* Substitute type params from previous trait's body in implemented trait's type args,
     then create new type bindings for the body of the implemented trait. *)
  let type_param_bindings =
    Types.bind_type_params_to_args implemented.trait_sig.type_params implemented.type_args
  in
  (* Substitute `This` type for base trait for `This` type of implemented trait *)
  let type_param_bindings =
    IMap.add implemented.trait_sig.this_type_param.id this_type type_param_bindings
  in

  SMap.iter
    (fun method_name (super_method : MethodSig.t) ->
      if not (MethodSig.is_inherited super_method) then (
        (* Add concrete method to trait sig *)
        ( if (not super_method.is_signature) || is_trait then
          match SMap.find_opt method_name trait_sig.methods with
          (* If base method is defined this method has already been added to trait sig *)
          | Some method_sig when not (MethodSig.is_inherited method_sig) -> ()
          (* Otherwise substitute propagated type params so that method is in terms of base trait *)
          | _ ->
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
                trait_sig;
                source_trait_instance = implemented;
                is_signature = super_method.is_signature;
                type_params = super_method.type_params;
                params;
                return;
              }
            in
            TraitSig.add_method trait_sig method_name method_sig );

        let is_already_implemented = SSet.mem method_name already_implemented_methods in
        if not is_already_implemented then
          match SMap.find_opt method_name trait_sig.methods with
          | Some sub_method when not (MethodSig.is_inherited sub_method) ->
            (* Overridden methods must have same type parameter arity *)
            let num_sub_type_params = List.length sub_method.type_params in
            let num_super_type_params = List.length super_method.type_params in
            if num_sub_type_params <> num_super_type_params then
              Type_context.add_error
                ~cx
                sub_method.loc
                (IncorrectOverridenMethodTypeParametersArity
                   ( method_name,
                     implemented.trait_sig.name,
                     num_sub_type_params,
                     num_super_type_params ))
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
              if
                not
                  (Type_context.is_subtype
                     ~cx
                     ~trait_object_promotion_loc:None
                     sub_method_ty
                     super_method_ty)
              then
                let sub_rep_ty = Type_context.find_rep_type ~cx sub_method_ty in
                let sup_rep_ty = Type_context.find_rep_type ~cx super_method_ty in
                Type_context.add_error
                  ~cx
                  sub_method.loc
                  (IncompatibleOverridenMethodType
                     (method_name, implemented.trait_sig.name, sub_rep_ty, sup_rep_ty))
          | _ ->
            if not is_trait then
              Type_context.add_error
                ~cx
                implemented_loc
                (UnimplementedMethodSignature (method_name, implemented.trait_sig.name))
      ))
    implemented.trait_sig.methods

and check_module ~cx module_ =
  let open Ast.Module in
  let { toplevels; _ } = module_ in
  List.iter
    (fun toplevel ->
      match toplevel with
      | VariableDeclaration decl -> check_variable_declaration ~cx ~is_toplevel:true decl
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

and check_variable_declaration ~cx ~is_toplevel decl =
  let open Ast.Statement.VariableDeclaration in
  let { loc; pattern; init; annot; _ } = decl in
  (* Toplevel variable declarations have already had pattern checked *)
  let (pattern_loc, pattern_tvar_id) =
    if is_toplevel then
      let pattern_loc = Ast_utils.pattern_loc pattern in
      let pattern_tvar_id = Type_context.get_tvar_from_loc ~cx pattern_loc in
      (pattern_loc, pattern_tvar_id)
    else
      check_pattern ~cx pattern
  in
  let (expr_loc, expr_tvar_id) = check_expression ~cx init in
  check_variable_declaration_bindings
    ~cx
    loc
    annot
    (pattern_loc, pattern_tvar_id)
    (expr_loc, Type.TVar expr_tvar_id)

and check_variable_declaration_bindings
    ~cx loc annot (pattern_loc, pattern_tvar_id) (expr_loc, expr_ty) =
  match annot with
  | None ->
    (* If expression's type is fully resolved then use as type of id, otherwise error
       requesting an annotation. *)
    let rep_ty = Type_context.find_rep_type ~cx expr_ty in
    let unresolved_tvars = Types.get_all_tvars [rep_ty] in
    if unresolved_tvars = [] then
      Type_context.assert_unify ~cx expr_loc expr_ty (TVar pattern_tvar_id)
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
      Type_context.assert_is_subtype ~cx expr_loc expr_ty annot_ty
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

  let return_ty = Type_context.find_rep_type ~cx func_decl.return in

  (* Check for exhaustiveness of returns *)
  (match return_ty with
  | Unit
  | Never ->
    ()
  | _ -> Exhaustive_returns.analyze_function ~cx decl);

  Type_context.push_current_function ~cx return_ty;

  (match body with
  | Signature -> ()
  | Expression expr -> check_function_expression_body ~cx expr return_ty
  | Block block -> check_function_block_body ~cx block return_ty);

  Type_context.pop_current_function ~cx

and check_function_expression_body ~cx expr return_ty =
  (* Expression body must be subtype of return type *)
  let (expr_loc, expr_tvar_id) = check_expression ~cx expr in
  Type_context.assert_is_subtype ~cx expr_loc (TVar expr_tvar_id) return_ty

and check_function_block_body ~cx block return_ty =
  let block_stmt = Ast.Statement.Block block in
  let (block_loc, block_tvar_id) = check_statement ~cx block_stmt in
  (* If return type is never, check that block diverges *)
  if return_ty = Type.Never then
    Type_context.assert_is_subtype ~cx block_loc (TVar block_tvar_id) return_ty

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
    let value = Integers.int64_of_string_opt raw base in
    let int_literal_ty = Type_context.mk_int_literal_ty ~cx loc value in
    ignore (Type_context.unify ~cx int_literal_ty (TVar tvar_id));
    (loc, tvar_id)
  | CharLiteral { CharLiteral.loc; value } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let value = Some (Integers.int64_of_char value) in
    let int_literal_ty = Type_context.mk_int_literal_ty ~cx loc value in
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
      | CtorDecl ctor_decl -> check_enum_variant ~cx name loc ctor_decl
      (* Id is for a function declaration. If function has type parameters then generate a fresh
         type variable for each type parameter and substitute into function type. *)
      | FunDecl func_decl ->
        if func_decl.type_params = [] then
          Type.Function { type_args = []; params = func_decl.params; return = func_decl.return }
        else
          Types.fresh_function_instance func_decl.type_params func_decl.params func_decl.return
      (* Otherwise identifier has same type as its declaration *)
      | FunParamDecl param_decl -> TVar param_decl.tvar
      | MatchCaseVarDecl var_decl -> TVar var_decl.tvar
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
  | InterpolatedString { loc; parts = [String _] } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx (Std_lib.mk_string_type ()) (TVar tvar_id));
    (loc, tvar_id)
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
            let implemented_trait =
              Type_context.get_implemented_trait rep_ty !Std_lib.to_string_trait_sig
            in
            if implemented_trait = None then
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
      | _ -> Type_context.get_implemented_trait rep_ty !Std_lib.equatable_trait_sig <> None
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
            OperatorRequiresTraitNotEquals
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
          (match (SMap.find name adt_sig.variants).kind with
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
  | Record { Record.loc; name; fields; rest } ->
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
          (match (SMap.find name adt_sig.variants).kind with
          | Record field_sigs ->
            (* Error if `...` is found - this can only be present on patterns *)
            (match rest with
            | None -> ()
            | Some rest_loc -> Type_context.add_error ~cx rest_loc RecordExpressionWithRest);
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
      Type_context.add_error
        ~cx
        (Ast_utils.expression_loc name)
        (ExpectedConstructorKind (false, true));
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
      (* Can index into Map *)
      | ADT { adt_sig; type_args = [key_ty; value_ty] } when adt_sig == !Std_lib.map_adt_sig ->
        Type_context.assert_is_subtype ~cx index_loc (TVar index_tvar_id) key_ty;
        let result_ty = Std_lib.mk_option_type value_ty in
        ignore (Type_context.unify ~cx result_ty (TVar tvar_id));
        true
      (* Can only index into ADTs with a single tuple variant *)
      | ADT { adt_sig = { variants; _ }; _ } ->
        (match SMap.choose_opt variants with
        | Some (_, { kind = Tuple element_sigs; _ }) when SMap.cardinal variants = 1 ->
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
    let try_resolve_trait_object_method { TraitSig.trait_sig; type_args } =
      try_resolve_method [trait_sig] type_args
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
      | BoundedExistential { bounds; _ } -> try_resolve_trait_bounds_method bounds
      | TraitObject trait -> try_resolve_trait_object_method trait
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
        | Some (_, { kind = Record field_sigs; _ }) when SMap.cardinal variants = 1 ->
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
  (*
   * ============================
   *      Ternary Expression
   * ============================
   *)
  | Ternary { loc; test; conseq; altern } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (test_loc, test_tvar_id) = check_expression ~cx test in
    let (_, conseq_tvar_id) = check_expression ~cx conseq in
    let (altern_loc, altern_tvar_id) = check_expression ~cx altern in
    Type_context.assert_unify ~cx test_loc Bool (TVar test_tvar_id);
    Type_context.assert_unify ~cx altern_loc (TVar conseq_tvar_id) (TVar altern_tvar_id);
    ignore (Type_context.unify ~cx (TVar conseq_tvar_id) (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *       Match Expression
   * ============================
   *)
  | Match ({ loc; _ } as match_) ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    check_match ~cx ~is_expr:true match_ (Type.TVar tvar_id);
    (loc, tvar_id)
  (*
   * ============================
   *         Vec Literal
   * ============================
   *)
  | VecLiteral { loc; elements } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let element_tvar_id =
      if elements = [] then
        (* If there are no elements create new, unresolved tvar *)
        TVar.mk ()
      else
        (* If there are elements, all element types must match *)
        let element_locs_and_tvar_ids = List.map (check_expression ~cx) elements in
        let ((_, first_tvar_id), rest_locs_and_tvar_ids) =
          List_utils.split_first element_locs_and_tvar_ids
        in
        List.iter
          (fun (element_loc, element_tvar_id) ->
            Type_context.assert_unify ~cx element_loc (TVar first_tvar_id) (TVar element_tvar_id))
          rest_locs_and_tvar_ids;
        first_tvar_id
    in
    ignore
      (Type_context.unify
         ~cx
         (ADT { adt_sig = !Std_lib.vec_adt_sig; type_args = [TVar element_tvar_id] })
         (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *         Map Literal
   * ============================
   *)
  | MapLiteral { loc; entries } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (key_tvar_id, value_tvar_id) =
      if entries = [] then
        (* If there are no entries create new, unresolved tvars for key and value types *)
        (TVar.mk (), TVar.mk ())
      else
        (* If there are entries, all key and value types must match *)
        let entry_locs_and_tvar_ids =
          List.map
            (fun { MapLiteral.Entry.key; value; _ } ->
              let key_loc_and_tvar_id = check_expression ~cx key in
              let value_loc_and_tvar_id = check_expression ~cx value in
              (key_loc_and_tvar_id, value_loc_and_tvar_id))
            entries
        in
        let (((_, first_key_tvar_id), (_, first_value_tvar_id)), rest_locs_and_tvar_ids) =
          List_utils.split_first entry_locs_and_tvar_ids
        in
        List.iter
          (fun ((key_loc, key_tvar_id), (value_loc, value_tvar_id)) ->
            Type_context.assert_unify ~cx key_loc (TVar first_key_tvar_id) (TVar key_tvar_id);
            Type_context.assert_unify ~cx value_loc (TVar first_value_tvar_id) (TVar value_tvar_id))
          rest_locs_and_tvar_ids;
        (first_key_tvar_id, first_value_tvar_id)
    in
    ignore
      (Type_context.unify
         ~cx
         (ADT { adt_sig = !Std_lib.map_adt_sig; type_args = [TVar key_tvar_id; TVar value_tvar_id] })
         (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *         Set Literal
   * ============================
   *)
  | SetLiteral { loc; elements } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let element_tvar_id =
      if elements = [] then
        (* If there are no elements create new, unresolved tvar *)
        TVar.mk ()
      else
        (* If there are elements, all element types must match *)
        let element_locs_and_tvar_ids = List.map (check_expression ~cx) elements in
        let ((_, first_tvar_id), rest_locs_and_tvar_ids) =
          List_utils.split_first element_locs_and_tvar_ids
        in
        List.iter
          (fun (element_loc, element_tvar_id) ->
            Type_context.assert_unify ~cx element_loc (TVar first_tvar_id) (TVar element_tvar_id))
          rest_locs_and_tvar_ids;
        first_tvar_id
    in
    ignore
      (Type_context.unify
         ~cx
         (ADT { adt_sig = !Std_lib.set_adt_sig; type_args = [TVar element_tvar_id] })
         (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *      Anonymous Function
   * ============================
   *)
  | AnonymousFunction ({ loc; params; return; body } as func) ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in

    (* Build params and return type, creating overall function type *)
    let params =
      List.map
        (fun { Ast.Function.Param.name = { loc; _ }; annot; _ } ->
          let binding = Type_context.get_value_binding ~cx loc in
          let param_decl = Bindings.get_func_param_decl binding in
          let param_ty = build_type ~cx annot in
          ignore (Type_context.unify ~cx param_ty (TVar param_decl.tvar));
          param_ty)
        params
    in
    let return = Option.fold ~none:Type.Unit ~some:(fun return -> build_type ~cx return) return in
    let func_ty = Type.Function { type_args = []; params; return } in

    (* Check for exhaustiveness of returns *)
    (match return with
    | Unit
    | Never ->
      ()
    | _ -> Exhaustive_returns.analyze_anonymous_function ~cx func);

    Type_context.push_current_function ~cx return;

    (* Check anonymous function body *)
    (match body with
    | Expression expr -> check_function_expression_body ~cx expr return
    | Block block -> check_function_block_body ~cx block return);

    Type_context.pop_current_function ~cx;

    ignore (Type_context.unify ~cx func_ty (TVar tvar_id));
    (loc, tvar_id)
  | Super _ -> failwith "TODO: Type check super expressions"

and check_match ~cx ~is_expr match_ match_ty =
  let open Ast.Match in
  let { args; cases; _ } = match_ in
  let num_args = List.length args in

  (* Find arg types - if multiple args are present, wrap in tuple *)
  let arg_tys = List.map (fun arg -> Type.TVar (snd (check_expression ~cx arg))) args in
  let arg_ty =
    match arg_tys with
    | [arg_ty] -> arg_ty
    | _ -> Tuple arg_tys
  in

  let right_hand_ty =
    if is_expr then
      match_ty
    else
      Unit
  in

  let case_evaluates_to_value =
    List.fold_left
      (fun case_evaluates_to_value { Case.pattern; guard; right; _ } ->
        (* Pattern must have same type as arg *)
        let (pattern_loc, pattern_tvar_id) = check_pattern ~cx pattern in
        Type_context.assert_unify ~cx pattern_loc arg_ty (TVar pattern_tvar_id);
        if num_args <> 1 then check_multiple_args_pattern ~cx pattern;

        (* Guard must evaluate to a boolean *)
        (match guard with
        | None -> ()
        | Some guard ->
          let (guard_loc, guard_tvar_id) = check_expression ~cx guard in
          Type_context.assert_unify ~cx guard_loc Bool (TVar guard_tvar_id));

        (* All cases must evaluate to same result value *)
        let (rhs_loc, rhs_tvar_id) =
          match right with
          | Expression expr -> check_expression ~cx expr
          | Statement stmt -> check_statement ~cx stmt
        in
        let rhs_rep_ty = Type_context.find_rep_type ~cx (TVar rhs_tvar_id) in
        (* If right hand side never produces a value then ignore case *)
        match rhs_rep_ty with
        | Never -> case_evaluates_to_value
        | _ ->
          Type_context.assert_unify ~cx rhs_loc right_hand_ty rhs_rep_ty;
          true)
      false
      cases
  in

  (* If all cases diverge and do not evaluate to value, entire match never produces a value *)
  if not case_evaluates_to_value then ignore (Type_context.unify ~cx Never match_ty)

(* Verify that a pattern for a match with multiple args has the correct structure. Bindings and
   variables cannot appear at the top level. Do not error on other patterns, as type checking will
   fail for those instead. *)
and check_multiple_args_pattern ~cx patt =
  let open Ast.Pattern in
  let error () =
    Type_context.add_error ~cx (Ast_utils.pattern_loc patt) InvalidMultipleArgumentsPattern
  in
  match patt with
  | Binding _ -> error ()
  (* Error on only variable bindings, not enum constructors *)
  | Identifier { name = { loc; _ }; _ } ->
    let binding = Type_context.get_value_binding ~cx loc in
    (match binding.declaration with
    | MatchCaseVarDecl _ -> error ()
    | _ -> ())
  | Or { left; right; _ } ->
    check_multiple_args_pattern ~cx left;
    check_multiple_args_pattern ~cx right
  | _ -> ()

and check_pattern ~cx patt =
  let open Ast.Pattern in
  match patt with
  (*
   * ============================
   *     Variables Pattern
   * ============================
   *)
  | Identifier { Ast.ScopedIdentifier.name = { loc; name }; _ } ->
    let binding = Type_context.get_value_binding ~cx loc in
    let decl_ty_opt =
      match binding.declaration with
      | VarDecl var_decl -> Some (Type.TVar var_decl.tvar)
      | MatchCaseVarDecl var_decl -> Some (Type.TVar var_decl.tvar)
      | CtorDecl ctor_decl ->
        Some (check_enum_variant ~cx name loc ctor_decl)
        (* Represents an error, but should error in assignment.
           Cannot appear in variable declarations or match patterns. *)
      | FunDecl _
      | FunParamDecl _ ->
        None
    in
    let ty =
      match decl_ty_opt with
      | Some ty -> ty
      | None -> Type.Any
    in
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx ty (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *       Binding Pattern
   * ============================
   *)
  | Binding { loc; pattern; name } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let name_loc_tvar_id = Type_context.mk_tvar_id ~cx ~loc:name.loc in
    let (_, pattern_tvar_id) = check_pattern ~cx pattern in
    (* Find type of variable declaration *)
    let binding = Type_context.get_value_binding ~cx name.loc in
    let decl_ty_opt =
      match binding.declaration with
      | VarDecl var_decl -> Some (Type.TVar var_decl.tvar)
      | MatchCaseVarDecl var_decl ->
        Some (Type.TVar var_decl.tvar)
        (* Represents an error, but should error in assignment.
           Cannot appear in variable declarations or match patterns. *)
      | CtorDecl _
      | FunDecl _
      | FunParamDecl _ ->
        None
    in
    let decl_ty =
      match decl_ty_opt with
      | Some ty -> ty
      | None -> Type.Any
    in
    (* Variable must have same type as pattern - note that variable could be declared in multiple or
       patterns so declaration may already have been unified with a type, so assert_unify is needed. *)
    Type_context.assert_unify ~cx name.loc decl_ty (TVar pattern_tvar_id);
    ignore (Type_context.unify ~cx (TVar pattern_tvar_id) (TVar tvar_id));
    ignore (Type_context.unify ~cx (TVar name_loc_tvar_id) (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *      Wildcard Pattern
   * ============================
   *)
  | Wildcard loc ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    (loc, tvar_id)
  (*
   * ============================
   *    Named Wildcard Pattern
   * ============================
   *)
  | NamedWildcard { loc; name } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let binding = Type_context.get_value_binding ~cx name.name.loc in
    let ctor_decl = Bindings.get_ctor_decl binding in
    let adt_sig = ctor_decl.adt_sig in
    (* Check that constructor is for a tuple or record variant *)
    (match (SMap.find name.name.name adt_sig.variants).kind with
    | Tuple _
    | Record _ ->
      ()
    | Enum -> Type_context.add_error ~cx name.loc (ExpectedConstructorKind (true, true)));
    let ty = Types.fresh_adt_instance adt_sig in
    ignore (Type_context.unify ~cx ty (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *      Literal Patterns
   * ============================
   *)
  | Literal (Unit { loc }) ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Unit (TVar tvar_id));
    (loc, tvar_id)
  | Literal (Bool { loc; _ }) ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx Bool (TVar tvar_id));
    (loc, tvar_id)
  | Literal (String { loc; _ }) ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    ignore (Type_context.unify ~cx (Std_lib.mk_string_type ()) (TVar tvar_id));
    (loc, tvar_id)
  | Literal (Int { loc; raw; base }) ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let value = Integers.int64_of_string_opt raw base in
    let int_literal_ty = Type_context.mk_int_literal_ty ~cx loc value in
    ignore (Type_context.unify ~cx int_literal_ty (TVar tvar_id));
    (loc, tvar_id)
  | Literal (Char { loc; value }) ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let value = Some (Integers.int64_of_char value) in
    let int_literal_ty = Type_context.mk_int_literal_ty ~cx loc value in
    ignore (Type_context.unify ~cx int_literal_ty (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *         Or Pattern
   * ============================
   *)
  | Or { loc; left; right } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (_, left_tvar_id) = check_pattern ~cx left in
    let (right_loc, right_tvar_id) = check_pattern ~cx right in
    ignore (Type_context.unify ~cx (TVar left_tvar_id) (TVar tvar_id));
    Type_context.assert_unify ~cx right_loc (TVar right_tvar_id) (TVar tvar_id);
    (loc, tvar_id)
  (*
   * ============================
   *    Tuple Literal Pattern
   * ============================
   *)
  | Tuple { loc; name = None; elements } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let element_tys =
      List.map (fun element -> Type.TVar (snd (check_pattern ~cx element))) elements
    in
    let tuple_ty = Type.Tuple element_tys in
    ignore (Type_context.unify ~cx tuple_ty (Type.TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *  Tuple Constructor Pattern
   * ============================
   *)
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
        (match (SMap.find name adt_sig.variants).kind with
        | Tuple element_sigs when List.length element_sigs <> List.length elements ->
          Type_context.add_error
            ~cx
            loc
            (IncorrectTupleConstructorArity (List.length elements, List.length element_sigs));
          Some Type.Any
        | Tuple element_sigs ->
          let type_param_bindings = Types.get_adt_type_param_bindings adt in
          List.iter2
            (fun (element_loc, element_tvar_id) element_sig_ty ->
              let element_ty = Types.substitute_type_params type_param_bindings element_sig_ty in
              Type_context.assert_unify ~cx element_loc element_ty (TVar element_tvar_id))
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
        Type_context.add_error ~cx scoped_id.loc (ExpectedConstructorKind (true, false));
        Type.Any
      | Some ty -> ty
    in
    ignore (Type_context.unify ~cx ty (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ==============================
   *   Record Constructor Pattern
   * ==============================
   *)
  | Record { loc; name = scoped_id; fields; rest } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let record_adt_ty_opt =
      let { Ast.ScopedIdentifier.name = { Ast.Identifier.loc = name_loc; name }; _ } = scoped_id in
      let binding = Type_context.get_value_binding ~cx name_loc in
      match binding.declaration with
      | CtorDecl ctor_decl ->
        let adt_sig = ctor_decl.adt_sig in
        let adt = Types.fresh_adt_instance adt_sig in
        (match (SMap.find name adt_sig.variants).kind with
        | Record field_sigs ->
          (* Recurse into fields and collect all fields that are not a part of this record *)
          let (field_params, unexpected_fields) =
            List.fold_left
              (fun (field_params, unexpected_fields) { Record.Field.name; value; _ } ->
                let { Ast.Identifier.name; loc } =
                  match name with
                  | None ->
                    (match value with
                    | Identifier { scopes = []; name; _ } -> name
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
          let has_unexpected_fields = unexpected_fields <> [] in
          let has_missing_fields = missing_fields <> [] && not rest in
          if has_unexpected_fields then
            List.iter
              (fun (loc, field_name) ->
                Type_context.add_error ~cx loc (UnexpectedRecordConstructorField (name, field_name)))
              (List.rev unexpected_fields)
          else if has_missing_fields then
            Type_context.add_error
              ~cx
              loc
              (MissingRecordConstructorFields (List.rev missing_fields));
          (* Supplied fields must each be a subtype of the field types *)
          let type_param_bindings = Types.get_adt_type_param_bindings adt in
          SMap.iter
            (fun field_name (param_loc, param_tvar_id) ->
              let field_sig_ty = SMap.find field_name field_sigs in
              let field_ty = Types.substitute_type_params type_param_bindings field_sig_ty in
              Type_context.assert_unify ~cx param_loc field_ty (TVar param_tvar_id))
            field_params;
          (* Result is algebraic data type unless the fields do not match,
             in which case propagate any *)
          if (not has_unexpected_fields) && not has_missing_fields then
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
        Type_context.add_error ~cx scoped_id.loc (ExpectedConstructorKind (false, true));
        Type.Any
      | Some ty -> ty
    in
    ignore (Type_context.unify ~cx ty (TVar tvar_id));
    (loc, tvar_id)

and check_enum_variant ~cx name loc ctor_decl =
  let adt_sig = ctor_decl.Bindings.TypeDeclaration.adt_sig in
  match (SMap.find name adt_sig.variants).kind with
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
    Any

and check_statement ~cx stmt : Loc.t * Types.TVar.t =
  let open Ast.Statement in
  match stmt with
  (*
   * ============================
   *     Variable Declaration
   * ============================
   *)
  | VariableDeclaration ({ loc; _ } as decl) ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    check_variable_declaration ~cx ~is_toplevel:false decl;
    ignore (Type_context.unify ~cx Unit (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *     Function Declaration
   * ============================
   *)
  | FunctionDeclaration ({ loc; _ } as decl) ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    build_function_declaration ~cx decl;
    check_function_body ~cx decl;
    ignore (Type_context.unify ~cx Unit (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *    Expression Statement
   * ============================
   *)
  | Expression (loc, expr) ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (_, expr_tvar_id) = check_expression ~cx expr in
    (* Propagate never type from expression if expression diverges *)
    let expr_rep_ty = Type_context.find_rep_type ~cx (TVar expr_tvar_id) in
    let expr_stmt_ty =
      if expr_rep_ty = Never then
        Type.Never
      else
        Unit
    in
    ignore (Type_context.unify ~cx expr_stmt_ty (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *       Block Statement
   * ============================
   *)
  | Block { Block.loc; statements } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (is_reachable, first_unreachable_loc) =
      List.fold_left
        (fun ((is_reachable, first_unreachable_loc) as acc) stmt ->
          let (stmt_loc, stmt_tvar_id) = check_statement ~cx stmt in
          match (is_reachable, first_unreachable_loc) with
          (* Save loc of first statement that is unreachable in block *)
          | (false, None) -> (false, Some stmt_loc)
          (* Do not collect locs of later unreachable statements, as first will error *)
          | (false, Some _) -> acc
          (* An inner statement that diverges makes later statements unreachable *)
          | (true, _) ->
            let stmt_rep_ty = Type_context.find_rep_type ~cx (TVar stmt_tvar_id) in
            (stmt_rep_ty <> Never, None))
        (true, None)
        statements
    in
    (* Error on first unreachable statement in block *)
    (match first_unreachable_loc with
    | None -> ()
    | Some loc -> Type_context.add_error ~cx loc UnreachableStatement);
    (* Block diverges if end of block (beyond last statement) is never reached *)
    let stmt_ty =
      if is_reachable then
        Type.Unit
      else
        Never
    in
    ignore (Type_context.unify ~cx stmt_ty (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *         If Statement
   * ============================
   *)
  | If { If.loc; test; conseq; altern } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (test_loc, test_tvar_id) = check_expression ~cx test in
    Type_context.assert_unify ~cx test_loc Bool (TVar test_tvar_id);
    let (_, conseq_tvar_id) = check_statement ~cx conseq in
    let stmt_ty =
      match altern with
      | None -> Type.Unit
      | Some altern ->
        let (_, altern_tvar_id) = check_statement ~cx altern in
        (* If diverges only if both conseq and altern exist and both diverge *)
        let conseq_ty = Type_context.find_rep_type ~cx (TVar conseq_tvar_id) in
        let altern_ty = Type_context.find_rep_type ~cx (TVar altern_tvar_id) in
        if conseq_ty = Never && altern_ty = Never then
          Never
        else
          Unit
    in
    ignore (Type_context.unify ~cx stmt_ty (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *           Return
   * ============================
   *)
  | Return { Return.loc; arg } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (arg_loc, arg_ty) =
      match arg with
      | None -> (loc, Type.Unit)
      | Some arg ->
        let (arg_loc, arg_tvar_id) = check_expression ~cx arg in
        (arg_loc, TVar arg_tvar_id)
    in
    (* Return argument must be subtype of current function's return type *)
    let return_ty = Type_context.get_current_function ~cx in
    Type_context.assert_is_subtype ~cx arg_loc arg_ty return_ty;
    ignore (Type_context.unify ~cx Never (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *           Break
   * ============================
   *)
  | Break { Break.loc } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    if not (Type_context.in_loop ~cx) then Type_context.add_error ~cx loc BreakOutsideLoop;
    ignore (Type_context.unify ~cx Never (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *          Continue
   * ============================
   *)
  | Continue { Continue.loc } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    if not (Type_context.in_loop ~cx) then Type_context.add_error ~cx loc ContinueOutsideLoop;
    ignore (Type_context.unify ~cx Never (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *          While Loop
   * ============================
   *)
  | While { While.loc; test; body } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let (test_loc, test_tvar_id) = check_expression ~cx test in
    Type_context.assert_unify ~cx test_loc Bool (TVar test_tvar_id);
    Type_context.enter_loop ~cx;
    ignore (check_statement ~cx body);
    Type_context.exit_loop ~cx;
    ignore (Type_context.unify ~cx Unit (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *          For Loop
   * ============================
   *)
  | For { For.loc; pattern; annot; iterator; body } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    (* Expression must be an iterator - extract its element type *)
    let (iter_loc, iter_tvar_id) = check_expression ~cx iterator in
    let iter_rep_ty = Type_context.find_rep_type ~cx (TVar iter_tvar_id) in
    let iter_trait_instance =
      Type_context.get_implemented_trait iter_rep_ty !Std_lib.iterable_trait_sig
    in
    let element_ty =
      match iter_trait_instance with
      | None ->
        Type_context.add_error ~cx iter_loc (ForLoopRequiresIterable iter_rep_ty);
        Type.Any
      | Some { type_args; _ } -> List.hd type_args
    in
    (* Check bindings and optional annotation against iterator element type *)
    let (pattern_loc, pattern_tvar_id) = check_pattern ~cx pattern in
    check_variable_declaration_bindings
      ~cx
      pattern_loc
      annot
      (pattern_loc, pattern_tvar_id)
      (iter_loc, element_ty);
    Type_context.enter_loop ~cx;
    ignore (check_statement ~cx body);
    Type_context.exit_loop ~cx;
    ignore (Type_context.unify ~cx Unit (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *         Assignment
   * ============================
   *)
  | Assignment { Assignment.loc; op = None; lvalue; expr } ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    let lvalue_loc_and_ty_opt =
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
              | MatchCaseVarDecl _ -> add_invalid_assign_error InvalidAssignmentMatchCaseVariable
              | CtorDecl ctor_decl ->
                (* Only add error on Enum variant, Tuple and Record variants will error due to
                   lack of arguments in check_pattern. *)
                (match
                   (SMap.find name ctor_decl.Bindings.TypeDeclaration.adt_sig.variants).kind
                 with
                | Enum -> add_invalid_assign_error InvalidAssignmentConstructor
                | Tuple _
                | Record _ ->
                  true))
            false
            ids
        in
        (* Check pattern, only checking against right hand side if no errors have been seen so far *)
        let (patt_loc, patt_tvar_id) = check_pattern ~cx pattern in
        if has_error then
          None
        else
          Some (patt_loc, Type.TVar patt_tvar_id)
      | Expression (IndexedAccess { Ast.Expression.IndexedAccess.loc; target; _ } as expr) ->
        (* Check lvalue expression *)
        let (expr_loc, expr_tvar_id) = check_expression ~cx expr in
        (* If the lvalue is a tuple then error as tuple cannot have their fields assigned *)
        let target_tvar_id = Type_context.get_tvar_from_loc ~cx (Ast_utils.expression_loc target) in
        let target_rep_ty = Type_context.find_rep_type ~cx (TVar target_tvar_id) in
        (match target_rep_ty with
        (* Error for both anonymous tuples and named tuples (with no other variants) *)
        | Tuple _ ->
          Type_context.add_error ~cx loc (InvalidLValue InvalidLValueTuple);
          None
        (* Vecs can have indexed assignment, argument type is same as element type *)
        | ADT { adt_sig; _ } when adt_sig == !Std_lib.vec_adt_sig ->
          Some (expr_loc, TVar expr_tvar_id)
        (* Maps can have indexed assignment, must unpack expression type option to find value type *)
        | ADT { adt_sig; _ } when adt_sig == !Std_lib.map_adt_sig ->
          (match Type_context.find_rep_type ~cx (TVar expr_tvar_id) with
          | ADT { adt_sig; type_args = [element_ty] } when adt_sig == !Std_lib.option_adt_sig ->
            Some (expr_loc, element_ty)
          | _ -> failwith "Expected option type")
        | ADT { adt_sig = { variants; _ }; _ } when SMap.cardinal variants = 1 ->
          Type_context.add_error ~cx loc (InvalidLValue InvalidLValueTuple);
          None
        | _ -> Some (expr_loc, TVar expr_tvar_id))
      | Expression expr ->
        let (expr_loc, expr_tvar_id) = check_expression ~cx expr in
        Some (expr_loc, TVar expr_tvar_id)
    in
    let (expr_loc, expr_tvar_id) = check_expression ~cx expr in
    (match lvalue_loc_and_ty_opt with
    | Some (_, lvalue_ty) ->
      Type_context.assert_is_subtype ~cx expr_loc (TVar expr_tvar_id) lvalue_ty
    | None -> ());
    ignore (Type_context.unify ~cx Unit (TVar tvar_id));
    (loc, tvar_id)
  (*
   * ============================
   *       Match Statement
   * ============================
   *)
  | Match ({ loc; _ } as match_) ->
    let tvar_id = Type_context.mk_tvar_id ~cx ~loc in
    check_match ~cx ~is_expr:false match_ (TVar tvar_id);
    (loc, tvar_id)
  | Assignment { op = Some _; _ } -> failwith "TODO: Type check operator assignment"

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
    inherit Ast_visitor.visitor as super

    method! function_ decl =
      let { Ast.Function.builtin; _ } = decl in
      if builtin then
        ()
      else
        super#function_ decl

    method! expression expr =
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
      super#expression expr
  end

let ensure_all_expression_are_typed ~cx modules =
  let visitor = new ensure_expressions_typed_visitor ~cx in
  List.iter (fun (_, module_) -> ignore (visitor#module_ module_)) modules

let analyze ~cx modules =
  let if_no_errors f = if Type_context.get_errors ~cx = [] then f () in
  (* First build parameters for all types and traits *)
  List.iter (fun (_, module_) -> build_type_and_trait_parameters ~cx module_) modules;
  (* Build all type aliases in program (in topological order). This must be done before any other
     types are built, so that type aliases can be substituted when building types. *)
  build_type_aliases ~cx modules;
  (* Build trait hierarchy for program *)
  build_trait_hierarchy ~cx modules;
  (* With trait hierarchy built, recheck types already built to verify that trait bounds are
     satisfied for all type instantiations. *)
  if_no_errors (fun _ ->
      List.iter (fun (_, module_) -> recheck_type_parameters ~cx module_) modules);
  if_no_errors (fun _ -> List.iter (fun (_, module_) -> build_declarations ~cx module_) modules);
  if_no_errors (fun _ -> check_trait_implementations ~cx modules);
  if_no_errors (fun _ -> Type_context.resolve_unchecked_trait_object_uses ~cx);
  if_no_errors (fun _ -> List.iter (fun (_, module_) -> check_module ~cx module_) modules);
  resolve_unresolved_int_literals ~cx;
  if_no_errors (fun _ -> ensure_all_expression_are_typed ~cx modules);
  Type_context.set_errors ~cx (List.rev (Type_context.get_errors ~cx))
