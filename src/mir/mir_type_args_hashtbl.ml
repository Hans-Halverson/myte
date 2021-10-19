open Basic_collections
open Types

(* Type arguments for use in hash tables after type checking. Only the following types are allowed:
 *   - Primitive types
 *   - Tuples
 *   - Functions
 *   - Algebraic data types
 *   - Trait objects
 *
 * These types are hashed/equated/stringified from the perspective of MIR, which may not match the
 * exact structure during type checking. For example all functions are represented identically in
 * in MIR, so function types are differentiated here.
 *)
module TypeArgs = struct
  type t = Type.t list

  let equal (tys1 : t) (tys2 : t) =
    let rec types_equal ty1 ty2 =
      match (ty1, ty2) with
      | (Type.Never, Type.Never)
      | (Unit, Unit)
      | (Bool, Bool)
      | (Byte, Byte)
      | (Int, Int)
      | (Long, Long)
      | (Function _, Function _) ->
        true
      | (Tuple elements1, Tuple elements2) -> List.for_all2 types_equal elements1 elements2
      | ( ADT { adt_sig = adt_sig1; type_args = type_args1 },
          ADT { adt_sig = adt_sig2; type_args = type_args2 } ) ->
        adt_sig1.id == adt_sig2.id && List.for_all2 types_equal type_args1 type_args2
      | ( TraitObject { trait_sig = trait_sig1; type_args = type_args1 },
          TraitObject { trait_sig = trait_sig2; type_args = type_args2 } ) ->
        trait_sig1.id == trait_sig2.id && List.for_all2 types_equal type_args1 type_args2
      | _ -> false
    in
    List.length tys1 = List.length tys2 && List.for_all2 types_equal tys1 tys2

  let hash (tys : t) =
    (* Boost hash combiner *)
    let hash_nums (ns : int list) =
      List.fold_left (fun hash n -> hash lxor (n + 0x9e3779b9 + (hash lsl 6) + (hash asr 2))) 0 ns
    in
    let rec hash ty =
      match ty with
      | Type.Never -> 0
      | Unit -> 1
      | Bool -> 2
      | Byte -> 3
      | Int -> 4
      | Long -> 5
      | Function _ -> 6
      | Tuple elements -> hash_nums (7 :: List.map hash elements)
      | ADT { adt_sig = { id; _ }; type_args } -> hash_nums (8 :: id :: List.map hash type_args)
      | TraitObject { trait_sig = { id; _ }; type_args } ->
        hash_nums (9 :: id :: List.map hash type_args)
      | Any
      | TVar _
      | TypeParam _
      | TraitBound _
      | IntLiteral _
      | BoundedExistential _ ->
        failwith "Not allowed as value in MIR"
    in
    hash_nums (List.map hash tys)

  let rec to_string ~(pcx : Program_context.t) (tys : t) =
    let rec ty_to_string ty =
      let ty_with_args_to_string loc name type_args =
        let full_name =
          match name with
          | Some name -> name
          | None ->
            let binding = Bindings.get_type_binding pcx.bindings loc in
            Mir_emit_utils.mk_type_binding_name binding
        in
        if type_args = [] then
          full_name
        else
          full_name ^ "<" ^ types_to_string type_args ^ ">"
      in
      match ty with
      | Type.Never -> "Never"
      | Unit -> "Unit"
      | Bool -> "Bool"
      | Byte -> "Byte"
      | Int -> "Int"
      | Long -> "Long"
      | Function _ -> "Function"
      | Tuple elements -> "(" ^ types_to_string elements ^ ")"
      | ADT { adt_sig = { loc; id; _ }; type_args } ->
        ty_with_args_to_string loc (IMap.find_opt id (Lazy.force adt_short_names)) type_args
      | TraitObject { trait_sig = { loc; _ }; type_args } ->
        ty_with_args_to_string loc None type_args
      | Any
      | TVar _
      | TypeParam _
      | TraitBound _
      | IntLiteral _
      | BoundedExistential _ ->
        failwith "Not allowed as value in MIR"
    and types_to_string type_args = String.concat "," (List.map ty_to_string type_args) in
    if tys = [] then
      ""
    else
      "<" ^ types_to_string tys ^ ">"

  (* Map from ADT signature id to the short name for that ADT in MIR. Lazily created so that
     ADT signatures for all types will be filled before creation. *)
  and adt_short_names =
    Lazy.from_fun (fun _ ->
        [
          (!Std_lib.string_adt_sig.id, "String");
          (!Std_lib.option_adt_sig.id, "Option");
          (!Std_lib.result_adt_sig.id, "Result");
          (!Std_lib.vec_adt_sig.id, "Vec");
          (!Std_lib.map_adt_sig.id, "Map");
          (!Std_lib.set_adt_sig.id, "Set");
        ]
        |> List.to_seq
        |> IMap.of_seq)
end

module TypeArgsHashtbl = Hashtbl.Make (TypeArgs)
