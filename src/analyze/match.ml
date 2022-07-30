open Basic_collections
open Types

(*
 * Pattern matching exhaustiveness and case reachability analysis
 *
 * Algorithms are based on the paper "Warnings for pattern matching" by Luc Maranget found at:
 * http://moscova.inria.fr/~maranget/papers/warn/index.html
 *)

module Ctor = struct
  type t = {
    ctor: ctor;
    ty: Type.t;
    loc: Loc.t;
  }

  and ctor =
    | Unit
    | Bool of bool
    | Int of Int64.t
    | String of string
    | Tuple
    | Variant of string

  let sub_patterns_length { ctor; ty; _ } =
    match ctor with
    | Unit
    | Bool _
    | Int _
    | String _ ->
      0
    | Tuple -> List.length (Type_util.cast_to_tuple_type ty)
    | Variant name ->
      let (_, adt_sig) = Type_util.cast_to_adt_type ty in
      (match (SMap.find name adt_sig.variants).kind with
      | Enum -> 0
      | Tuple elements -> List.length elements
      | Record fields -> SMap.cardinal fields)

  let equal { ctor = c1; _ } { ctor = c2; _ } =
    match (c1, c2) with
    | (Unit, Unit)
    | (Tuple, Tuple) ->
      true
    | (Bool b1, Bool b2) -> b1 = b2
    | (Int i1, Int i2) -> Int64.equal i1 i2
    | (String s1, String s2) -> s1 = s2
    | (Variant n1, Variant n2) -> n1 = n2
    | _ -> false
end

type pattern =
  | Wildcard of Loc.t
  | Constructor of Ctor.t * pattern list
  | Or of pattern list

type pattern_vector = pattern list

type pattern_matrix = pattern_vector list

(* A wildcard without a location. Can be used for all wildcards except those constructed directly
   from pattern AST nodes. *)
let wildcard = Wildcard Loc.none

let wildcards_vector n = List_utils.make n wildcard

module Completeness = struct
  type t =
    | Complete of Ctor.t list
    | Incomplete of missing

  and missing =
    | Enumerable of Ctor.t list
    | Unenumerable
end

type reconstruct_op =
  | AddWildcard
  | AddCtors of Ctor.t list
  | ReconstructCtor of Ctor.t

module type UsefulResultType = sig
  type t

  val useful : t

  val useless : t

  val create_witness : bool

  val combine_complete_signature : t list -> t

  val combine_or : (t * pattern) list -> t

  val reconstruct_head : t -> reconstruct_op -> t
end

module ExhaustiveResult = struct
  type witness = Witness of pattern list

  type t = witness list

  let useful = [Witness []]

  let useless = []

  let create_witness = true

  let combine_complete_signature results = List.concat results

  let combine_or results_and_patterns = List.concat (List.map fst results_and_patterns)

  let reconstruct_head result op =
    match op with
    | AddWildcard -> List.map (fun (Witness patterns) -> Witness (wildcard :: patterns)) result
    | AddCtors ctors ->
      let patterns =
        List.map
          (fun ctor ->
            let wildcard_subpatterns = wildcards_vector (Ctor.sub_patterns_length ctor) in
            Constructor (ctor, wildcard_subpatterns))
          ctors
      in
      let witness_head =
        match patterns with
        | [pattern] -> pattern
        | _ -> Or patterns
      in
      List.map (fun (Witness pattern) -> Witness (witness_head :: pattern)) result
    | ReconstructCtor ctor ->
      let ctor_sub_patterns_length = Ctor.sub_patterns_length ctor in
      List.map
        (fun (Witness patterns) ->
          let (ctor_sub_patterns, rest) = List_utils.split_at ctor_sub_patterns_length patterns in
          Witness (Constructor (ctor, ctor_sub_patterns) :: rest))
        result

  let witness result =
    let pattern_of_patterns patterns =
      match patterns with
      | [pattern] -> pattern
      | _ -> Constructor ({ Ctor.ctor = Tuple; ty = Tuple []; loc = Loc.none }, patterns)
    in
    match result with
    | [] -> None
    | [Witness patterns] -> Some (pattern_of_patterns patterns)
    | witnesses ->
      let patterns = List.map (fun (Witness patterns) -> pattern_of_patterns patterns) witnesses in
      Some (Or patterns)
end

module ReachableResult = struct
  (* Set of useless patterns *)
  type t =
    (* No useless patterns *)
    | Empty
    (* All branches of or pattern are useless *)
    | Full
    (* Some patterns are useless *)
    | Set of pattern list

  let useful = Empty

  let useless = Full

  let create_witness = false

  (* Combination of useless results of or patterns from paper *)
  let combine_or results_and_patterns =
    let ((first_result, first_pattern), rest) = List_utils.split_first results_and_patterns in
    (* Keep track of all Full patterns seen previously, so that they can all be added to set if
       a non-Full result is encountered. *)
    let (result, _) =
      List.fold_left
        (fun results result ->
          match (results, result) with
          | ((Empty, _), (Empty, _)) -> (Empty, [])
          | ((Full, patterns), (Full, pattern)) -> (Full, pattern :: patterns)
          | ((Empty, _), (Full, pattern)) -> (Set [pattern], [])
          | ((Full, patterns), (Empty, _)) -> (Set patterns, [])
          | ((Empty, _), (Set patterns, _))
          | ((Set patterns, _), (Empty, _)) ->
            (Set patterns, [])
          | ((Full, patterns1), (Set patterns2, _)) -> (Set (patterns1 @ patterns2), [])
          | ((Set patterns, _), (Full, pattern)) -> (Set (pattern :: patterns), [])
          | ((Set patterns1, _), (Set patterns2, _)) -> (Set (patterns1 @ patterns2), []))
        (first_result, [first_pattern])
        rest
    in
    result

  (* Combination of all possible ctor branches from complete signature is set intersection,
     since a pattern is only unreachable if it is unreachable in all specialized branches. *)
  let combine_complete_signature results =
    let (first, rest) = List_utils.split_first results in
    List.fold_left
      (fun r1 r2 ->
        match (r1, r2) with
        | (Empty, _)
        | (_, Empty) ->
          Empty
        | (Full, other)
        | (other, Full) ->
          other
        | (Set s1, Set s2) ->
          (* Collect locs for all top level patterns in s1 *)
          let rec add_pattern_locs locs pattern =
            match pattern with
            | Wildcard loc
            | Constructor ({ loc; _ }, _) ->
              LocSet.add loc locs
            | Or patterns -> List.fold_left add_pattern_locs locs patterns
          in
          let to_keep_locs = List.fold_left add_pattern_locs LocSet.empty s1 in

          (* Filter top level patterns in s2 to only the locs collected from s1 *)
          let rec filter_pattern kept_patterns pattern =
            match pattern with
            | Wildcard loc
            | Constructor ({ loc; _ }, _)
              when LocSet.mem loc to_keep_locs ->
              pattern :: kept_patterns
            | Wildcard _
            | Constructor _ ->
              kept_patterns
            | Or patterns -> List.fold_left filter_pattern kept_patterns patterns
          in
          let intersection = List.fold_left filter_pattern [] s2 in

          Set intersection)
      first
      rest

  let reconstruct_head result _ = result
end

(* Determine whether the head constructors of a matrix form a complete signature. If so, return
   a list of all signatures. Otherwise return the missing signatures, if they can be enumerated.

   Only return enumerated missing signature if the `create_witnesses` flag is present. *)
let signature_completeness ~create_witness matrix =
  let rec gather_ctors acc pattern =
    match pattern with
    | Wildcard _ -> acc
    | Constructor (ctor, _) -> ctor :: acc
    | Or patterns -> List.fold_left gather_ctors acc patterns
  in
  let head_ctors = List.fold_left (fun acc row -> gather_ctors acc (List.hd row)) [] matrix in
  match head_ctors with
  (* If there are no head constructors, signature is incomplete *)
  | [] -> Completeness.Incomplete Unenumerable
  | { ty; _ } :: _ ->
    let mk_ctor ctor = { Ctor.ctor; ty; loc = Loc.none } in
    (match ty with
    (* Unit and Tuple types have a single constructor, so when a head constructor is present the
       signature is necessarily complete. *)
    | Unit -> Complete [mk_ctor Unit]
    | Tuple _ -> Complete [mk_ctor Tuple]
    (* Bytes can realistically be enumerated, so check for completeness *)
    | Byte ->
      let used_ints =
        List.fold_left
          (fun acc { Ctor.ctor; _ } ->
            match ctor with
            | Int value -> I64Set.add value acc
            | _ -> acc)
          I64Set.empty
          head_ctors
      in
      if I64Set.cardinal used_ints = 256 then
        Complete []
      else if create_witness then
        (* Find first unused byte, starting at zero and wrapping around *)
        let rec find_unused_byte candidate =
          if I64Set.mem candidate used_ints then
            if Int64.equal candidate Integers.max_signed_byte then
              find_unused_byte Integers.min_signed_byte
            else
              find_unused_byte (Int64.succ candidate)
          else
            candidate
        in
        let unused_byte = find_unused_byte 0L in
        Incomplete (Enumerable [mk_ctor (Int unused_byte)])
      else
        Incomplete Unenumerable
    (* Integer types larger than a byte cannot realistically be enumerated, so assume incompleteness *)
    | Int
    | Long ->
      if create_witness then
        let used_ints =
          List.fold_left
            (fun acc { Ctor.ctor; _ } ->
              match ctor with
              | Int value -> I64Set.add value acc
              | _ -> acc)
            I64Set.empty
            head_ctors
        in
        (* Find lowest positive unused int - technically this could exceed the maximum Int size,
           but that would require an inconceivably large match statement. *)
        let rec find_unused_int candidate =
          if I64Set.mem candidate used_ints then
            find_unused_int (Int64.succ candidate)
          else
            candidate
        in
        let unused_int = find_unused_int 0L in
        Incomplete (Enumerable [mk_ctor (Int unused_int)])
      else
        Incomplete Unenumerable
    (* The set of strings is infinite, so necessarily incomplete *)
    | ADT { adt_sig; _ } when adt_sig == !Std_lib.string_adt_sig ->
      if create_witness then
        (* If creating a witness, search for first string of "*"s that is not present in head ctors *)
        let used_strings =
          List.fold_left
            (fun acc { Ctor.ctor; _ } ->
              match ctor with
              | String value -> SSet.add value acc
              | _ -> acc)
            SSet.empty
            head_ctors
        in
        let rec find_unused_string candidate =
          if SSet.mem candidate used_strings then
            find_unused_string (candidate ^ "*")
          else
            candidate
        in
        let string_witness = find_unused_string "" in
        Incomplete (Enumerable [mk_ctor (String string_witness)])
      else
        Incomplete Unenumerable
    (* At least one bool constructor is present, so check for all bool values and report the
       missing one if incomplete. *)
    | Bool ->
      let all_values = BSet.add true (BSet.singleton false) in
      let missing_values =
        List.fold_left
          (fun acc { Ctor.ctor; _ } ->
            match ctor with
            | Bool value -> BSet.remove value acc
            | _ -> acc)
          all_values
          head_ctors
      in
      if BSet.is_empty missing_values then
        Complete [mk_ctor (Bool true); mk_ctor (Bool false)]
      else if create_witness then
        Incomplete (Enumerable [mk_ctor (Bool (BSet.choose missing_values))])
      else
        Incomplete Unenumerable
    (* Check for all variants and report the missing ones if incomplete *)
    | ADT { adt_sig; _ } ->
      let missing_variants =
        List.fold_left
          (fun acc { Ctor.ctor; _ } ->
            match ctor with
            | Variant name -> SMap.remove name acc
            | _ -> acc)
          adt_sig.variants
          head_ctors
      in
      if SMap.is_empty missing_variants then
        let all_variant_ctors =
          SMap.fold (fun name _ acc -> mk_ctor (Variant name) :: acc) adt_sig.variants []
        in
        Complete (List.rev all_variant_ctors)
      else if create_witness then
        let missing_variant_ctors =
          SMap.fold (fun name _ acc -> mk_ctor (Variant name) :: acc) missing_variants []
        in
        Incomplete (Enumerable (List.rev missing_variant_ctors))
      else
        Incomplete Unenumerable
    | _ -> failwith "Invalid head constructor type")

(* Specialized matrix from paper *)
let rec specialize_matrix ctor matrix =
  List.filter_map
    (fun row ->
      let (row_head, row_rest) = List_utils.split_first row in
      match row_head with
      | Constructor (row_head_ctor, sub_patterns) when Ctor.equal ctor row_head_ctor ->
        Some [sub_patterns @ row_rest]
      | Constructor _ -> None
      | Wildcard _ ->
        let wildcard_sub_patternss = wildcards_vector (Ctor.sub_patterns_length ctor) in
        Some [wildcard_sub_patternss @ row_rest]
      | Or row_heads ->
        let expanded_matrix = List.map (fun row_head -> row_head :: row_rest) row_heads in
        Some (specialize_matrix ctor expanded_matrix))
    matrix
  |> List.flatten

(* Default matrix from paper *)
let rec default_matrix matrix =
  List.filter_map
    (fun row ->
      let (row_head, row_rest) = List_utils.split_first row in
      match row_head with
      | Constructor _ -> None
      | Wildcard _ -> Some [row_rest]
      | Or row_heads ->
        let expanded_matrix = List.map (fun row_head -> row_head :: row_rest) row_heads in
        Some (default_matrix expanded_matrix))
    matrix
  |> List.flatten

module UsefulAnalyzer (UsefulResult : UsefulResultType) = struct
  (* Implementation of `useful` function from paper. Takes a pattern matrix and a pattern vector,
     and returns a witness of a value that matches the pattern vector but does not match any pattern
     in the pattern matrix, if such a value exists. If such a witness exists, the pattern vector is
     deemed "useful" with respect to the pattern matrix.

     Only create a full witness if the `create_witness` flag is set, otherwise return a sigil witness
     whose presence indicates a useful result.

     `is_guarded` flag specifies whether the checked vector is under a match guard, in which case
     subpatterns should not be added to matrix in useless *)
  let rec useful (matrix : pattern_matrix) (vector : pattern_vector) ~(is_guarded : bool) :
      UsefulResult.t =
    match vector with
    (* Base case - is useful if there are no rows in matrix *)
    | [] ->
      if matrix = [] then
        UsefulResult.useful
      else
        UsefulResult.useless
    (* Inductive case 1 - head is a constructor, so specialize matrix and recurse *)
    | Constructor (ctor, sub_patterns) :: rest_patterns ->
      let specialized_matrix = specialize_matrix ctor matrix in
      let patterns = sub_patterns @ rest_patterns in
      let useful_result = useful ~is_guarded specialized_matrix patterns in
      UsefulResult.reconstruct_head useful_result (ReconstructCtor ctor)
    | Wildcard _ :: rest_vector ->
      (match signature_completeness ~create_witness:UsefulResult.create_witness matrix with
      (* Inductive case 2a - head is wildcard and matrix's head constructors form a complete
         signature. Create a specialized matrix and recurse for every constructor in signature. *)
      | Complete all_ctors ->
        let useful_results =
          List.map
            (fun ctor ->
              let specialized_matrix = specialize_matrix ctor matrix in
              let specialized_vector =
                wildcards_vector (Ctor.sub_patterns_length ctor) @ rest_vector
              in
              let useful_result = useful ~is_guarded specialized_matrix specialized_vector in
              (* If there are witnesses of usefulness recreate constructor at head of witness, where
                 witness contains subpatterns for this constructor at its head. *)
              UsefulResult.reconstruct_head useful_result (ReconstructCtor ctor))
            all_ctors
        in
        UsefulResult.combine_complete_signature useful_results
      (* Inductive case 2b - head is wildcard and matrix's head constructors do not form a complete
         signature. Create the default matrix and recurse. *)
      | Incomplete missing ->
        let default_matrix = default_matrix matrix in
        let useful_result = useful ~is_guarded default_matrix rest_vector in
        (* If there are witnesses of usefulness add prefix for missing constructors. Unenumerable
           missing constructors can add the generic wildcard pattern, while if there are known missing
           constructors add a union of them with wildcard subpatterns. *)
        let reconstruct_op =
          match missing with
          | Unenumerable -> AddWildcard
          | Enumerable missing_ctors -> AddCtors missing_ctors
        in
        UsefulResult.reconstruct_head useful_result reconstruct_op)
    | Or or_sub_patterns :: rest_vector ->
      let (_, useful_results_and_patterns) =
        List.fold_left
          (fun (matrix, useful_results_and_patterns) or_sub_pattern ->
            let vector = or_sub_pattern :: rest_vector in
            let useful_result = useful ~is_guarded matrix vector in
            (* Add vector pattern to matrix to detect unreachable branches in same or pattern,
               but do not add if entire pattern is guarded. *)
            let matrix =
              if is_guarded then
                matrix
              else
                matrix @ [vector]
            in
            (matrix, (useful_result, or_sub_pattern) :: useful_results_and_patterns))
          (matrix, [])
          or_sub_patterns
      in
      UsefulResult.combine_or useful_results_and_patterns
end

module ExhaustiveAnalyzer = UsefulAnalyzer (ExhaustiveResult)
module ReachableAnalyzer = UsefulAnalyzer (ReachableResult)

(* Convert a match case node into a pattern vector for use in exhaustiveness/reachability checking *)
let pattern_vector_of_pattern_node ~cx pattern_node =
  let type_of_loc loc =
    let tvar_id = Type_context.get_tvar_from_loc ~cx loc in
    Type_context.find_rep_type ~cx (TVar tvar_id)
  in
  let rec pattern_of_pattern_node pattern =
    match pattern with
    (* Wildcards and literals are emitted directly *)
    | Ast.Pattern.Wildcard loc -> Wildcard loc
    | Binding { pattern; _ } -> pattern_of_pattern_node pattern
    | Literal (Unit { loc }) -> Constructor ({ Ctor.ctor = Unit; ty = Unit; loc }, [])
    | Literal (Bool { loc; value }) -> Constructor ({ Ctor.ctor = Bool value; ty = Bool; loc }, [])
    | Literal (String { loc; value }) ->
      Constructor ({ Ctor.ctor = String value; ty = Std_lib.mk_string_type (); loc }, [])
    | Literal (Int { loc; raw; base; _ }) ->
      let ty = type_of_loc loc in
      let value = Integers.int64_of_string_opt raw base |> Option.get in
      Constructor ({ Ctor.ctor = Int value; ty; loc }, [])
    | Literal (Char { loc; value }) ->
      let ty = type_of_loc loc in
      Constructor ({ Ctor.ctor = Int (Integers.int64_of_char value); ty; loc }, [])
    (* Identifier pattern may be for an enum variant, otherwise it is a variable and can be
       treated as a wildcard for exhaustiveness/usefulness checking. *)
    | Identifier { loc; name; _ } ->
      let binding = Type_context.get_value_binding ~cx name.loc in
      (match binding.declaration with
      | CtorDecl _ ->
        let ty = type_of_loc name.loc in
        Constructor ({ Ctor.ctor = Variant name.name; ty; loc }, [])
      | _ -> Wildcard loc)
    (* Named wildcard patterns create sub patterns of same length as tuple elements/record fields
       which consist of all wildcards. *)
    | NamedWildcard { loc; name } ->
      let name = name.name.name in
      let ty = type_of_loc loc in
      let (_, adt_sig) = Type_util.cast_to_adt_type ty in
      (match (SMap.find name adt_sig.variants).kind with
      | Tuple elements ->
        let elements = wildcards_vector (List.length elements) in
        Constructor ({ Ctor.ctor = Variant name; ty; loc }, elements)
      | Record fields ->
        let fields = wildcards_vector (SMap.cardinal fields) in
        Constructor ({ Ctor.ctor = Variant name; ty; loc }, fields)
      | Enum -> failwith "Expected tuple or record variant")
    | Tuple { loc; name = None; elements } ->
      let ty = type_of_loc loc in
      let elements = List.map pattern_of_pattern_node elements in
      Constructor ({ Ctor.ctor = Tuple; ty; loc }, elements)
    | Tuple { loc; name = Some name; elements } ->
      let ty = type_of_loc loc in
      let elements = List.map pattern_of_pattern_node elements in
      Constructor ({ Ctor.ctor = Variant name.name.name; ty; loc }, elements)
    | Record { loc; name; fields; _ } ->
      (* Fetch field sigs from ADT sig *)
      let name = name.name.name in
      let ty = type_of_loc loc in
      let (_, adt_sig) = Type_util.cast_to_adt_type ty in
      let field_sigs =
        match (SMap.find name adt_sig.variants).kind with
        | Record field_sigs -> field_sigs
        | _ -> failwith "Expected record variant"
      in
      (* Create map from field name to its corresponding pattern *)
      let fields =
        List.fold_left
          (fun acc { Ast.Pattern.Record.Field.name; value; _ } ->
            let name =
              match name with
              | Some { name; _ } -> name
              (* Shorthand patterns must have identifier value *)
              | None ->
                (match value with
                | Identifier { name = { name; _ }; _ } -> name
                | _ -> failwith "Value shorthand must be an identifier")
            in
            SMap.add name (pattern_of_pattern_node value) acc)
          SMap.empty
          fields
      in
      (* Add patterns to vector in alphabetical order of their fields, adding wildcard if field
         is not present in pattern. *)
      let fields =
        SMap.fold
          (fun field_name _ acc ->
            match SMap.find_opt field_name fields with
            | None -> wildcard :: acc
            | Some pattern -> pattern :: acc)
          field_sigs
          []
      in
      Constructor ({ Ctor.ctor = Variant name; ty; loc }, List.rev fields)
    | Or or_ ->
      (* Or patterns are associative so flatten all or patterns recursively gathered from either side *)
      let rec gather_or_branches acc or_ =
        let open Ast.Pattern in
        let { Or.left; right; _ } = or_ in
        let acc =
          match left with
          | Or or_ -> gather_or_branches acc or_
          | _ -> pattern_of_pattern_node left :: acc
        in
        match right with
        | Or or_ -> gather_or_branches acc or_
        | _ -> pattern_of_pattern_node right :: acc
      in
      let or_branches = gather_or_branches [] or_ in
      Or (List.rev or_branches)
  in
  [pattern_of_pattern_node pattern_node]

(* Whether a vector of patterns can be compressed to a single wildcard pattern for display *)
let rec can_compress_to_wildcard vector =
  List.for_all
    (fun pattern ->
      match pattern with
      | Wildcard _ -> true
      | Constructor ({ ctor = Tuple; _ }, sub_patterns) -> can_compress_to_wildcard sub_patterns
      (* Single variant ADTs may be compressed *)
      | Constructor ({ ctor = Variant _; ty; _ }, sub_patterns) ->
        let (_, adt_sig) = Type_util.cast_to_adt_type ty in
        SMap.cardinal adt_sig.variants = 1 && can_compress_to_wildcard sub_patterns
      | Constructor _ -> false
      | Or patterns -> can_compress_to_wildcard patterns)
    vector

let string_of_pattern pattern =
  let buf = Buffer.create 16 in
  let add_char c = Buffer.add_char buf c in
  let add_string str = Buffer.add_string buf str in
  (* Tuple may be compressed to wildcard if all fields can be compressed to wildcards *)
  let rec add_tuple_pattern elements =
    if can_compress_to_wildcard elements then
      add_char '_'
    else (
      add_char '(';
      let num_elements = List.length elements in
      List.iteri
        (fun i pattern ->
          add_pattern pattern;
          if i <> num_elements - 1 then add_string ", ")
        elements;
      add_char ')'
    )
  and add_pattern pattern =
    match pattern with
    | _ when can_compress_to_wildcard [pattern] -> add_char '_'
    | Wildcard _ -> add_char '_'
    | Or patterns ->
      let num_patterns = List.length patterns in
      List.iteri
        (fun i pattern ->
          add_pattern pattern;
          if i <> num_patterns - 1 then add_string " | ")
        patterns
    | Constructor ({ ctor = Unit; _ }, _) -> add_string "()"
    | Constructor ({ ctor = Bool b; _ }, _) -> add_string (Bool.to_string b)
    | Constructor ({ ctor = Int i; _ }, _) -> add_string (Int64.to_string i)
    | Constructor ({ ctor = String s; _ }, _) ->
      add_char '"';
      add_string s;
      add_char '"'
    | Constructor ({ ctor = Tuple; _ }, sub_patterns) -> add_tuple_pattern sub_patterns
    | Constructor ({ ctor = Variant name; ty; _ }, sub_patterns) ->
      let (_, adt_sig) = Type_util.cast_to_adt_type ty in
      (match (SMap.find name adt_sig.variants).kind with
      | Enum -> add_string name
      | Tuple _ ->
        add_string name;
        add_char ' ';
        add_tuple_pattern sub_patterns
      | Record fields ->
        (* Only print fields that cannot be compressed to wildcards, then print `...` if any fields
           were compressed to wildcards. If all fields can be compressed to wildcards print wildcard
           instead of record pattern. *)
        let fields = SMap.bindings fields in
        let num_fields = List.length fields in
        add_string name;
        add_char ' ';
        let non_wildcard_fields =
          List.fold_left2
            (fun acc (name, _) sub_pattern ->
              if not (can_compress_to_wildcard [sub_pattern]) then
                SMap.add name sub_pattern acc
              else
                acc)
            SMap.empty
            fields
            sub_patterns
        in
        let num_non_wildcard_fields = SMap.cardinal non_wildcard_fields in
        if num_non_wildcard_fields = 0 then
          add_char '_'
        else (
          add_string "{ ";
          ignore
            (SMap.fold
               (fun name _ i ->
                 (match SMap.find_opt name non_wildcard_fields with
                 | None -> ()
                 | Some pattern ->
                   add_string name;
                   add_string ": ";
                   add_pattern pattern;
                   if i <> num_fields - 1 then add_string ", ");
                 i + 1)
               non_wildcard_fields
               0);
          if num_non_wildcard_fields <> num_fields then add_string "...";
          add_string " }"
        ))
  in
  add_pattern pattern;
  Buffer.contents buf

(* If there is only one argument to match, exhaustiveness check vector is a single wildcard.
   Otherwise exhaustiveness check vector is a tuple containing as many wildcards as args. *)
let args_wildcard_vector ~cx (match_ : Ast.Match.t) =
  if List.length match_.args = 1 then
    [wildcard]
  else
    let arg_tys =
      List.map
        (fun arg ->
          let tvar_id = Type_context.get_tvar_from_loc ~cx (Ast_utils.expression_loc arg) in
          Type_context.find_rep_type ~cx (TVar tvar_id))
        match_.args
    in
    let arg_wildcards = wildcards_vector (List.length match_.args) in
    [Constructor ({ Ctor.ctor = Tuple; ty = Tuple arg_tys; loc = Loc.none }, arg_wildcards)]

class match_analyzer ~cx =
  object (this)
    inherit Ast_visitor.visitor as super

    val mutable errors : (Loc.t * Analyze_error.t) list = []

    method errors = errors

    method add_error loc error = errors <- (loc, error) :: errors

    method check_reachable_result full_loc result =
      match result with
      | ReachableResult.Empty -> ()
      | Full -> this#add_error full_loc UnreachablePattern
      | Set patterns ->
        List.iter
          (fun pattern ->
            match pattern with
            | Wildcard loc -> this#add_error loc UnreachablePattern
            | Constructor ({ loc; _ }, _) -> this#add_error loc UnreachablePattern
            | _ -> ())
          patterns

    method check_exhaustive_result full_loc result =
      match ExhaustiveResult.witness result with
      | None -> ()
      | Some witness_pattern ->
        let witness_string = string_of_pattern witness_pattern in
        this#add_error full_loc (InexhaustiveMatch witness_string)

    method! match_ match_ =
      (* Build pattern matrix and check for reachability of each case *)
      let matrix =
        List.fold_left
          (fun prev_rows case ->
            let row = pattern_vector_of_pattern_node ~cx case.Ast.Match.Case.pattern in
            let is_guarded = case.guard <> None in

            (* Check for reachability of case. A match case is unreachable if it is not useful with
               respect to the matrix of all cases that appear above it in the match statement. *)
            let reachable_result = ReachableAnalyzer.useful ~is_guarded prev_rows row in
            this#check_reachable_result (Ast_utils.pattern_loc case.pattern) reachable_result;

            (* Do not include case in pattern matrix if it has a guard, as the guard could fail *)
            if case.guard = None then
              prev_rows @ [row]
            else
              prev_rows)
          []
          match_.cases
      in

      (* Check exhaustiveness. A match statement is exhaustive if a wildcard vector is useful
         after the entire matrix of match cases. *)
      let wildcard_vector = args_wildcard_vector ~cx match_ in
      let exhaustive_result = ExhaustiveAnalyzer.useful ~is_guarded:false matrix wildcard_vector in
      this#check_exhaustive_result match_.loc exhaustive_result

    (* Analyze exhaustiveness and reachability for a single pattern, e.g. in a variable declaration
       or assignment. *)
    method analyze_destructure pattern =
      let loc = Ast_utils.pattern_loc pattern in
      let row = pattern_vector_of_pattern_node ~cx pattern in

      (* Check for reachability of subpatterns *)
      let reachable_result = ReachableAnalyzer.useful ~is_guarded:false [] row in
      this#check_reachable_result loc reachable_result;

      (* Check exhaustiveness of single row matrix against the wildcard pattern, as the single row
         must match all possible inputs. *)
      let exhaustive_result = ExhaustiveAnalyzer.useful ~is_guarded:false [row] [wildcard] in
      this#check_exhaustive_result loc exhaustive_result

    method! variable_declaration decl =
      this#analyze_destructure decl.pattern;
      super#variable_declaration decl

    method! assignment assign =
      (match assign.lvalue with
      | Pattern pattern -> this#analyze_destructure pattern
      | Expression _ -> ());
      super#assignment assign

    method! for_ for_ =
      this#analyze_destructure for_.pattern;
      super#for_ for_

    method! match_test match_test =
      let loc = Ast_utils.pattern_loc match_test.pattern in
      let row = pattern_vector_of_pattern_node ~cx match_test.pattern in

      (* Check for reachability but not exhaustiveness of subpatterns *)
      let reachable_result = ReachableAnalyzer.useful ~is_guarded:false [] row in
      this#check_reachable_result loc reachable_result;

      super#match_test match_test
  end

let analyze ~cx modules =
  let analyzer = new match_analyzer ~cx in
  List.iter (fun (_, module_) -> analyzer#module_ module_) modules;
  analyzer#errors
