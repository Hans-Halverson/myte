open Basic_collections

module rec TVar : sig
  type t = int

  val mk : unit -> t
end = struct
  type t = TVar.t

  let max_id = ref 0

  let mk () =
    let id = !max_id in
    max_id := id + 1;
    id
end

and TypeParam : sig
  type id = int

  type t = {
    id: id;
    name: name;
    bounds: TraitSig.instance list;
  }

  and name =
    | Implicit
    | Explicit of string

  val mk : name:name -> bounds:TraitSig.instance list -> t
end = struct
  type id = int

  type t = {
    id: id;
    name: name;
    bounds: TraitSig.instance list;
  }

  and name =
    | Implicit
    | Explicit of string

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id

  let mk ~name ~bounds = { id = mk_id (); name; bounds }
end

and Function : sig
  type t = {
    (* Type arguments for this function. These type arguments are essentially metadata and should
       not be used by the type checker. They allow us to extract the type args this function was
       instantiated with after type checking is complete. *)
    type_args: Type.t list;
    params: Type.t list;
    return: Type.t;
  }
end =
  Function

and IntLiteral : sig
  type t = {
    (* Locations of int literals (and their tvars) along with the value of the int literal at
       that location (or None if it is out of range for 64 bit ints) *)
    mutable values: (Loc.t * Int64.t option) list;
    (* Integer type this int literal is resolved to, if it has been resolved *)
    mutable resolved: Type.t option;
  }
end =
  IntLiteral

and TraitBound : sig
  type t = {
    mutable bounds: TraitSig.instance list;
    mutable resolved: Type.t option;
  }
end =
  TraitBound

and AdtSig : sig
  type id = int

  type t = {
    id: id;
    name: string;
    mutable type_params: TypeParam.t list;
    mutable variants: variant SMap.t;
    mutable traits: TraitSig.t list;
  }

  and variant =
    | Enum
    | Tuple of Type.t list
    | Record of Type.t SMap.t

  and instance = {
    adt_sig: t;
    type_args: Type.t list;
  }

  val mk : name:string -> t

  val empty : t
end = struct
  type id = int

  type t = {
    id: id;
    name: string;
    mutable type_params: TypeParam.t list;
    mutable variants: variant SMap.t;
    mutable traits: TraitSig.t list;
  }

  and variant =
    | Enum
    | Tuple of Type.t list
    | Record of Type.t SMap.t

  and instance = {
    adt_sig: t;
    type_args: Type.t list;
  }

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id

  let mk ~name = { id = mk_id (); name; type_params = []; variants = SMap.empty; traits = [] }

  let empty = { id = 0; name = ""; type_params = []; variants = SMap.empty; traits = [] }
end

and FunctionSig : sig
  type t = {
    type_params: TypeParam.t list;
    params: Type.t list;
    return: Type.t;
  }
end =
  FunctionSig

and TraitSig : sig
  type id = int

  type t = {
    id: id;
    name: string;
    mutable type_params: TypeParam.t list;
    mutable methods: FunctionSig.t SMap.t;
    mutable implemented: instance list;
    mutable this_type_param_id: TypeParam.id;
  }

  and instance = {
    trait_sig: t;
    type_args: Type.t list;
  }

  val mk : name:string -> t

  val add_method : t -> string -> FunctionSig.t -> unit

  val add_implemented : t -> instance -> unit
end = struct
  type id = int

  type t = {
    id: id;
    name: string;
    mutable type_params: TypeParam.t list;
    mutable methods: FunctionSig.t SMap.t;
    mutable implemented: instance list;
    mutable this_type_param_id: TypeParam.id;
  }

  and instance = {
    trait_sig: t;
    type_args: Type.t list;
  }

  let max_id = ref 0

  let mk_id () =
    let id = !max_id in
    max_id := id + 1;
    id

  let mk ~name =
    {
      id = mk_id ();
      name;
      type_params = [];
      methods = SMap.empty;
      implemented = [];
      this_type_param_id = 0;
    }

  let add_method trait_sig name func_sig =
    trait_sig.methods <- SMap.add name func_sig trait_sig.methods

  let add_implemented trait_sig implemented =
    trait_sig.implemented <- implemented :: trait_sig.implemented
end

and Type : sig
  type t =
    | TVar of TVar.t
    | TypeParam of TypeParam.t
    | Any
    | Unit
    | Bool
    | Byte
    | Int
    | Long
    | Array of t
    | Tuple of t list
    | Function of Function.t
    | ADT of AdtSig.instance
    | IntLiteral of IntLiteral.t
    | TraitBound of TraitBound.t
end =
  Type

let get_all_tvars_with_duplicates ty =
  let rec inner acc ty =
    let open Type in
    match ty with
    | TVar tvar_id -> tvar_id :: acc
    | Tuple elements -> List.fold_left inner acc elements
    | Array element -> inner acc element
    | Function { type_args = _; params; return } ->
      let acc = List.fold_left inner acc params in
      inner acc return
    | ADT { type_args; _ } -> List.fold_left inner acc type_args
    | _ -> acc
  in
  inner [] ty |> List.rev

let get_all_tvars tys =
  let tvars_with_duplicates = List.map get_all_tvars_with_duplicates tys |> List.concat in
  let rec remove_duplicates seen rest =
    match rest with
    | [] -> []
    | hd :: tl ->
      if ISet.mem hd seen then
        remove_duplicates seen tl
      else
        hd :: remove_duplicates (ISet.add hd seen) tl
  in
  remove_duplicates ISet.empty tvars_with_duplicates

let rec substitute_type_params type_params ty =
  let open Type in
  let substitute_trait_bounds bounds =
    List.map
      (fun { TraitSig.trait_sig; type_args } ->
        { TraitSig.trait_sig; type_args = List.map (substitute_type_params type_params) type_args })
      bounds
  in
  match ty with
  | Any
  | Unit
  | Bool
  | Byte
  | Int
  | Long
  | TVar _
  | IntLiteral { resolved = None; _ } ->
    ty
  | IntLiteral { resolved = Some resolved; _ }
  | TraitBound { resolved = Some resolved; _ } ->
    substitute_type_params type_params resolved
  | TraitBound { resolved = None; bounds } ->
    let bounds' = substitute_trait_bounds bounds in
    TraitBound { resolved = None; bounds = bounds' }
  | Array element -> Array (substitute_type_params type_params element)
  | Tuple elements -> Tuple (List.map (substitute_type_params type_params) elements)
  | Function { type_args; params; return } ->
    let params' = List.map (substitute_type_params type_params) params in
    let return' = substitute_type_params type_params return in
    Function { type_args; params = params'; return = return' }
  | ADT { adt_sig; type_args } ->
    ADT { adt_sig; type_args = List.map (substitute_type_params type_params) type_args }
  | TypeParam { TypeParam.id; name; bounds } ->
    (match IMap.find_opt id type_params with
    | None ->
      let bounds' = substitute_trait_bounds bounds in
      TypeParam { TypeParam.id; name; bounds = bounds' }
    (* Avoid infinite loop when substituting type parameter for itself *)
    | Some (TypeParam { id = new_id; _ } as ty) when new_id = id -> ty
    | Some ty -> substitute_type_params type_params ty)

(* Generate map of type params bound to type args to be used for type param substitution. *)
let bind_type_params_to_args type_params type_args =
  let type_params_and_args = List.combine type_params type_args in
  List.fold_left
    (fun map (type_param, ty) -> IMap.add type_param.TypeParam.id ty map)
    IMap.empty
    type_params_and_args

let get_adt_type_param_bindings ty =
  match ty with
  | Type.ADT { adt_sig = { type_params; _ }; type_args } ->
    bind_type_params_to_args type_params type_args
  | _ -> failwith "Expected ADT"

(* Generate fresh type arguments for a set of type params *)
let refresh_type_params type_params =
  (* TODO: Handle substituting params in bounds (e.g. <T: TraitBound<T>>) *)
  List.map
    (fun { TypeParam.bounds; _ } ->
      if bounds = [] then
        Type.TVar (TVar.mk ())
      else
        Type.TraitBound { resolved = None; bounds })
    type_params

(* Generate a new ADT type with fresh type args for this ADT signature *)
let fresh_adt_instance (adt_sig : AdtSig.t) =
  Type.ADT { adt_sig; type_args = refresh_type_params adt_sig.type_params }

(* Generate a new function type with fresh type args for this function signature *)
let fresh_function_instance type_params params return =
  let type_args = refresh_type_params type_params in
  let type_arg_bindings = bind_type_params_to_args type_params type_args in
  let params = List.map (substitute_type_params type_arg_bindings) params in
  let return = substitute_type_params type_arg_bindings return in
  Type.Function { type_args; params; return }

let concat_and_wrap (pre, post) elements = pre ^ String.concat ", " elements ^ post

let rec pp_with_names ~tvar_to_name ty =
  let open Type in
  let pp_name_with_args name args =
    if args = [] then
      name
    else
      let pp_args = List.map (pp_with_names ~tvar_to_name) args in
      concat_and_wrap (name ^ "<", ">") pp_args
  in
  let pp_trait_bounds bounds =
    let pp_bounds =
      List.map
        (fun { TraitSig.trait_sig = { name; _ }; type_args } -> pp_name_with_args name type_args)
        bounds
    in
    String.concat " & " pp_bounds
  in
  match ty with
  | Any -> "Any"
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Byte -> "Byte"
  | Int -> "Int"
  | Long -> "Long"
  | Array element -> "Array<" ^ pp_with_names ~tvar_to_name element ^ ">"
  | Tuple elements ->
    let element_names = List.map (pp_with_names ~tvar_to_name) elements in
    concat_and_wrap ("(", ")") element_names
  | Function { type_args = _; params; return } ->
    let pp_function_part ty =
      let pp_param = pp_with_names ~tvar_to_name ty in
      match ty with
      | Function _ -> "(" ^ pp_param ^ ")"
      | _ -> pp_param
    in
    let pp_params =
      match params with
      | [param] -> pp_function_part param
      | _ ->
        let pp_params = List.map (fun param -> pp_with_names ~tvar_to_name param) params in
        concat_and_wrap ("(", ")") pp_params
    in
    pp_params ^ " -> " ^ pp_function_part return
  | ADT { adt_sig = { name; _ }; type_args } -> pp_name_with_args name type_args
  | TVar tvar_id ->
    let x = IMap.find tvar_id tvar_to_name in
    x
  | TypeParam { TypeParam.name; id = _; bounds } ->
    (match name with
    | Implicit -> "implicit " ^ pp_trait_bounds bounds
    | Explicit name ->
      if bounds = [] then
        name
      else
        name ^ ": " ^ pp_trait_bounds bounds)
  | IntLiteral { resolved = Some resolved; _ }
  | TraitBound { resolved = Some resolved; _ } ->
    pp_with_names ~tvar_to_name resolved
  | IntLiteral { resolved = None; _ } -> "<Integer>"
  | TraitBound { resolved = None; bounds } -> pp_trait_bounds bounds

let name_id_to_string name_id =
  let quot = name_id / 26 in
  let rem = name_id mod 26 in
  let letter = Char.escaped (Char.chr (0x61 + rem)) in
  if quot = 0 then
    letter
  else
    letter ^ string_of_int (quot + 1)

let rec get_next_name next_name_id used_names =
  let name = name_id_to_string next_name_id in
  if SSet.mem name used_names then
    get_next_name (next_name_id + 1) used_names
  else
    (name, next_name_id + 1)

let rec build_tvar_to_name ~use_tvar_ids (tvar_to_name_acc, names_acc) next_name_id tvar_ids =
  match tvar_ids with
  | [] -> (tvar_to_name_acc, names_acc)
  | hd :: tl ->
    if IMap.mem hd tvar_to_name_acc then
      build_tvar_to_name ~use_tvar_ids (tvar_to_name_acc, names_acc) next_name_id tl
    else
      let (next_name, next_name_id) =
        if use_tvar_ids then
          (string_of_int hd, next_name_id)
        else
          get_next_name next_name_id names_acc
      in
      build_tvar_to_name
        ~use_tvar_ids
        (IMap.add hd next_name tvar_to_name_acc, SSet.add next_name names_acc)
        next_name_id
        tl

let pps_with_tvar_map ?(tvar_to_name = IMap.empty) ?(use_tvar_ids = false) tys =
  let all_tvars = get_all_tvars tys in
  let used_names = IMap.fold (fun _ name acc -> SSet.add name acc) tvar_to_name SSet.empty in
  let (tvar_to_name, _) = build_tvar_to_name ~use_tvar_ids (tvar_to_name, used_names) 0 all_tvars in
  (List.map (pp_with_names ~tvar_to_name) tys, tvar_to_name)

let pps ?(tvar_to_name = IMap.empty) ?(use_tvar_ids = false) tys =
  fst (pps_with_tvar_map ~tvar_to_name ~use_tvar_ids tys)

let pp ?(tvar_to_name = IMap.empty) ?(use_tvar_ids = false) ty =
  pps ~tvar_to_name ~use_tvar_ids [ty] |> List.hd

let get_adt_sig adt =
  match adt with
  | Type.ADT { adt_sig; _ } -> adt_sig
  | _ -> failwith "Expected ADT"

let get_tuple_variant adt_sig name =
  let open AdtSig in
  match SMap.find name adt_sig.variants with
  | Tuple element_sigs -> element_sigs
  | _ -> failwith "Expected Tuple"

let get_record_variant adt_sig name =
  let open AdtSig in
  match SMap.find name adt_sig.variants with
  | Record field_sigs -> field_sigs
  | _ -> failwith "Expected Record"
