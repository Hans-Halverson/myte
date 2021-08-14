open Basic_collections
module Ecx = Mir_emit_context

(*
 * Creation of decision trees for pattern matching, for use in compiling match expressions to MIR.
 *
 * Algorithms are based on the paper "Compiling Pattern Matching to Good Decision Trees"
 * by Luc Maranget found at: http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf
 *)

module Ctor = struct
  type t =
    | Unit
    | Bool of bool
    | Int of Int64.t * (* Is byte *) bool
    | String of string * Loc.t
    | Tuple of (* Arity *) int
    | Variant of string * Types.AdtSig.t * (* Type args *) Types.Type.t list

  let arity ctor =
    match ctor with
    | Unit
    | Bool _
    | Int _
    | String _ ->
      0
    | Tuple arity -> arity
    | Variant (name, adt_sig, _) ->
      (match (SMap.find name adt_sig.variants).kind with
      | Enum -> 0
      | Tuple elements -> List.length elements
      | Record fields -> SMap.cardinal fields)

  (* Guaranteed that constructors being compared have same type, so some checks can be omitted *)
  let equal c1 c2 =
    match (c1, c2) with
    | (Unit, Unit)
    | (Tuple _, Tuple _) ->
      true
    | (Bool b1, Bool b2) -> b1 = b2
    | (Int (i1, _), Int (i2, _)) -> Int64.equal i1 i2
    | (String (s1, _), String (s2, _)) -> s1 = s2
    | (Variant (n1, _, _), Variant (n2, _, _)) -> n1 = n2
    | _ -> false

  let cast_to_bool ctor =
    match ctor with
    | Bool value -> value
    | _ -> failwith "Expected bool constructor"

  let cast_to_int ctor =
    match ctor with
    | Int (value, is_bool) -> (value, is_bool)
    | _ -> failwith "Expected int constructor"

  let cast_to_string ctor =
    match ctor with
    | String (value, loc) -> (value, loc)
    | _ -> failwith "Expected string constructor"

  let cast_to_variant ctor =
    match ctor with
    | Variant (name, adt_sig, type_args) -> (name, adt_sig, type_args)
    | _ -> failwith "Expected variant constructor"
end

module PatternPath = struct
  type t =
    | Root of Mir.cf_value
    | TupleField of {
        parent: t;
        index: int;
      }
    | VariantField of {
        parent: t;
        field: variant_field;
        variant_name: string;
        adt_sig: Types.AdtSig.t;
        type_args: Types.Type.t list;
      }

  and variant_field =
    | TupleIndex of int
    | RecordField of string

  let mk kind = kind
end

module DecisionTree = struct
  type t =
    | Leaf of {
        case_node: Ast.Match.Case.t;
        (* If this case is guarded, tree for when the guard fails. Filled after creation. *)
        mutable guard_fail_case: t option;
      }
    | Test of {
        scrutinee: PatternPath.t;
        cases: (Ctor.t * t) list;
        default_case: t option;
      }
end

type pattern =
  | Wildcard
  | Constructor of Ctor.t * pattern list
  | Or of pattern list

type pattern_vector = pattern list

type pattern_matrix = (pattern_vector * Ast.Match.Case.t) list

let wildcards_vector n = List_utils.make n Wildcard

(* Find the signature for a column, returning a tuple containing a list of all unique head
   constructors in that column, as well as whether the signature is complete. *)
let find_signature column =
  let rec gather_ctors acc pattern =
    match pattern with
    | Wildcard -> acc
    | Constructor (ctor, _) -> ctor :: acc
    | Or patterns -> List.fold_left gather_ctors acc patterns
  in
  let head_ctors = List.fold_left (fun acc pattern -> gather_ctors acc pattern) [] column in
  match head_ctors with
  | [] -> ([], false)
  (* Unit and Tuple types have a single constructor, so complete if a single constructor exists *)
  | ((Unit | Tuple _) as ctor) :: _ -> ([ctor], true)
  (* Gather all integer constructors, which may be complete if they are bytes *)
  | Int (_, is_byte) :: _ ->
    let ctors =
      List.fold_left
        (fun acc ctor ->
          let (value, _) = Ctor.cast_to_int ctor in
          I64Map.add value ctor acc)
        I64Map.empty
        head_ctors
    in
    let ctors = I64Map.fold (fun _ ctor ctors -> ctor :: ctors) ctors [] in
    (* Only Bytes can realistically be fully enumerated *)
    let is_complete =
      if is_byte then
        List.length ctors = 256
      else
        false
    in
    (ctors, is_complete)
  (* Gather all string constructors, which are necessarily incomplete *)
  | String _ :: _ ->
    let ctors =
      List.fold_left
        (fun acc ctor ->
          let (value, _) = Ctor.cast_to_string ctor in
          SMap.add value ctor acc)
        SMap.empty
        head_ctors
    in
    let ctors = SMap.fold (fun _ ctor ctors -> ctor :: ctors) ctors [] in
    (ctors, false)
  (* Gather all bool constructors *)
  | Bool _ :: _ ->
    let ctors =
      List.fold_left
        (fun acc ctor ->
          let value = Ctor.cast_to_bool ctor in
          BMap.add value ctor acc)
        BMap.empty
        head_ctors
    in
    let ctors = BMap.fold (fun _ ctor ctors -> ctor :: ctors) ctors [] in
    (ctors, List.length ctors = 2)
  (* Gather all variant constructors *)
  | Variant (_, adt_sig, _) :: _ ->
    let ctors =
      List.fold_left
        (fun acc ctor ->
          let (name, _, _) = Ctor.cast_to_variant ctor in
          SMap.add name ctor acc)
        SMap.empty
        head_ctors
    in
    let ctors = SMap.fold (fun _ ctor ctors -> ctor :: ctors) ctors [] in
    (ctors, List.length ctors = SMap.cardinal adt_sig.variants)

(* Specialized matrix from paper. Specialize along column i using the given constructor. *)
let rec specialize_matrix i ctor matrix =
  List.filter_map
    (fun (row, case_node) ->
      let (row_pre, target_pattern, row_post) = List_utils.split_around i row in
      match target_pattern with
      | Constructor (target_head_ctor, sub_patterns) when Ctor.equal ctor target_head_ctor ->
        Some [(row_pre @ sub_patterns @ row_post, case_node)]
      | Constructor _ -> None
      | Wildcard ->
        let wildcard_sub_patterns = wildcards_vector (Ctor.arity ctor) in
        Some [(row_pre @ wildcard_sub_patterns @ row_post, case_node)]
      | Or target_heads ->
        let expanded_matrix =
          List.map
            (fun target_head -> (row_pre @ (target_head :: row_post), case_node))
            target_heads
        in
        Some (specialize_matrix i ctor expanded_matrix))
    matrix
  |> List.flatten

(* Default matrix from paper. Create default matrix along column i. *)
let rec default_matrix i matrix =
  List.filter_map
    (fun (row, case_node) ->
      let (row_pre, target_pattern, row_rest) = List_utils.split_around i row in
      match target_pattern with
      | Constructor _ -> None
      | Wildcard -> Some [(row_pre @ row_rest, case_node)]
      | Or target_heads ->
        let expanded_matrix =
          List.map
            (fun target_head -> (row_pre @ (target_head :: row_rest), case_node))
            target_heads
        in
        Some (default_matrix i expanded_matrix))
    matrix
  |> List.flatten

(* Specialize a single column in the scrutinee vector given a target constructor, removing the item
   at that column and expanding its subfields in place if it has any. *)
let specialize_scrutinee_vector column_index ctor scrutinee_vector =
  let (pre_scrutinees, parent, post_scrutinees) =
    List_utils.split_around column_index scrutinee_vector
  in
  let rec mk_tuple_scrutinees i acc =
    match i with
    | 0 -> acc
    | _ ->
      mk_tuple_scrutinees (i - 1) (PatternPath.(mk (TupleField { parent; index = i - 1 })) :: acc)
  in
  let rec mk_tuple_variant_scrutinees variant_name adt_sig type_args i acc =
    match i with
    | 0 -> acc
    | _ ->
      mk_tuple_variant_scrutinees
        variant_name
        adt_sig
        type_args
        (i - 1)
        ( PatternPath.(
            mk
              (VariantField { parent; field = TupleIndex (i - 1); variant_name; adt_sig; type_args }))
        :: acc )
  in
  let expanded_scrutinee =
    match ctor with
    | Ctor.Tuple arity -> mk_tuple_scrutinees arity []
    | Variant (variant_name, adt_sig, type_args) ->
      (match (SMap.find variant_name adt_sig.variants).kind with
      | Enum -> []
      | Tuple elements ->
        mk_tuple_variant_scrutinees variant_name adt_sig type_args (List.length elements) []
      | Record fields ->
        let fields =
          SMap.fold
            (fun name _ acc ->
              PatternPath.(
                mk
                  (VariantField
                     { parent; field = RecordField name; variant_name; adt_sig; type_args }))
              :: acc)
            fields
            []
        in
        List.rev fields)
    | _ -> []
  in
  pre_scrutinees @ expanded_scrutinee @ post_scrutinees

(* Analogue to default matrix for scrutinee vector, removing column at index from scrutinee vector *)
let default_scrutinee_vector column_index scrutinee_vector =
  let (pre_scrutinees, _, post_scrutinees) =
    List_utils.split_around column_index scrutinee_vector
  in
  pre_scrutinees @ post_scrutinees

let rec build ~(ecx : Ecx.t) scrutinee_vals case_nodes =
  let num_scrutinees = List.length scrutinee_vals in
  let pattern_matrix =
    List.map
      (fun case_node ->
        (pattern_vector_of_case_node ~cx:ecx.pcx.type_ctx ~num_scrutinees case_node, case_node))
      case_nodes
  in
  let scrutinee_vector =
    List.map (fun scrutinee_val -> PatternPath.(mk (Root scrutinee_val))) scrutinee_vals
  in
  build_decision_tree ~prev_guard:None scrutinee_vector pattern_matrix

and build_decision_tree ~prev_guard scrutinee_vector matrix =
  (* If the previous case ended in a guard, connect its fail case to the next tree created in its
     ungaurded matrix. *)
  let connect_prev_guard tree =
    match prev_guard with
    | Some (DecisionTree.Leaf leaf) -> leaf.guard_fail_case <- Some tree
    | _ -> ()
  in

  (* If the first row is all patterns (or equivalently - empty), we have hit a leaf node where the
     right hand of a case (or guard) can be executed. *)
  let (first_row, first_case_node) = List.hd matrix in
  let first_row_all_patterns =
    List.for_all
      (fun pattern ->
        match pattern with
        | Wildcard -> true
        | _ -> false)
      first_row
  in
  if first_row_all_patterns then (
    let leaf_node = DecisionTree.Leaf { case_node = first_case_node; guard_fail_case = None } in
    connect_prev_guard leaf_node;

    (* If this case is guarded we must connect its fail case to the tree created for its unguarded matrix *)
    ( if first_case_node.guard <> None then
      let unguarded_matrix = List.tl matrix in
      ignore (build_decision_tree ~prev_guard:(Some leaf_node) scrutinee_vector unguarded_matrix) );
    leaf_node
  ) else
    (* If first row is not all patterns, we must select a column to test using heuristics *)
    let (column_index, signature, is_complete) = choose_column matrix in
    let scrutinee = List.nth scrutinee_vector column_index in

    (* Build decision trees on the specialized matrix for each constructor in the chosen column *)
    let cases =
      List.fold_left
        (fun cases ctor ->
          let scrutinee_vector = specialize_scrutinee_vector column_index ctor scrutinee_vector in
          let specialized_matrix = specialize_matrix column_index ctor matrix in
          let tree = build_decision_tree ~prev_guard:None scrutinee_vector specialized_matrix in
          (ctor, tree) :: cases)
        []
        signature
    in

    (* And add default case if column does not form a complete signature *)
    let default_case =
      if is_complete then
        None
      else
        let scrutinee_vector = default_scrutinee_vector column_index scrutinee_vector in
        let default_matrix = default_matrix column_index matrix in
        Some (build_decision_tree ~prev_guard:None scrutinee_vector default_matrix)
    in
    let test_node = DecisionTree.Test { scrutinee; cases; default_case } in
    connect_prev_guard test_node;
    test_node

(* Choose a column to test on next using heuristics from paper. Return both the index of the column,
   the column's signature, and whether the signature is complete. *)
and choose_column matrix =
  (* Initially all columns are candidates *)
  let ((first_row, _), _) = List_utils.split_first matrix in
  let (_, all_columns) =
    List.fold_left
      (fun (i, all_columns) _ -> (i + 1, ISet.add i all_columns))
      (0, ISet.empty)
      first_row
  in

  (* Transpose matrix to list of columns, and set up column signatures to be lazily generated *)
  let transposed_matrix = List_utils.transpose (List.map fst matrix) in
  let column_signatures = List.map (fun column -> lazy (find_signature column)) transposed_matrix in

  (* Use cascade of heuristics, moving to next heuristic in case of ties *)
  let rec run_heuristics columns heuristics =
    match heuristics with
    | [] -> failwith "Must always be at least one heuristic"
    | [heuristic] ->
      let columns = run_heuristic heuristic columns transposed_matrix column_signatures in
      ISet.choose columns
    | heuristic :: next_heuristics ->
      let columns = run_heuristic heuristic columns transposed_matrix column_signatures in
      if ISet.cardinal columns = 1 then
        ISet.choose columns
      else
        run_heuristics columns next_heuristics
  in
  let column = run_heuristics all_columns all_heuristics in
  let (signature, is_complete) = Lazy.force (List.nth column_signatures column) in
  (column, signature, is_complete)

(* Run a heuristic function on a set of candidate columns, returning the set of columns with the
   highest heuristic score. *)
and run_heuristic heuristic candidate_columns transposed_matrix signatures =
  let (scores, max_score, _) =
    List.fold_left2
      (fun (scores, max_score, column_index) column signature ->
        if ISet.mem column_index candidate_columns then
          let score = heuristic column signature in
          ((column_index, score) :: scores, max max_score score, column_index + 1)
        else
          (scores, max_score, column_index + 1))
      ([], Int.min_int, 0)
      transposed_matrix
      signatures
  in
  List.fold_left
    (fun selected_columns (column_index, score) ->
      if score = max_score then
        ISet.add column_index selected_columns
      else
        selected_columns)
    ISet.empty
    scores

(* Score for a column is index of the latest row such that all rows up that that row do not have
   a wildcard in that column. Return indexes of columns with the highest score. *)
and column_prefix_heuristic column signature =
  match column with
  | []
  | Wildcard :: _ ->
    0
  | (Constructor _ | Or _) :: rest -> 1 + column_prefix_heuristic rest signature

(* Score for a column is -(# of all unique constructors in its signature), with an additional -1
   added if the signature is incomplete. *)
and small_branching_factor_heuristic _ signature =
  let (ctors, is_complete) = Lazy.force signature in
  let signature_size_score = -List.length ctors in
  if is_complete then
    signature_size_score
  else
    signature_size_score - 1

(* Score for a column is -(sum of the arities of all unique constructors in its signature) *)
and arity_heuristic _ signature =
  let (ctors, _) = Lazy.force signature in
  let arity_sum = List.fold_left (fun sum ctor -> sum + Ctor.arity ctor) 0 ctors in
  -arity_sum

and all_heuristics = [column_prefix_heuristic; small_branching_factor_heuristic; arity_heuristic]

and pattern_vector_of_case_node ~cx ~num_scrutinees case_node =
  let type_of_loc loc =
    let tvar_id = Type_context.get_tvar_from_loc ~cx loc in
    Type_context.find_rep_type ~cx (TVar tvar_id)
  in
  let rec pattern_of_pattern_node pattern =
    match pattern with
    (* Wildcards and literals are emitted directly *)
    | Ast.Pattern.Wildcard _ -> Wildcard
    | Binding { pattern; _ } -> pattern_of_pattern_node pattern
    | Literal (Unit _) -> Constructor (Unit, [])
    | Literal (Bool { value; _ }) -> Constructor (Bool value, [])
    | Literal (String { loc; value }) -> Constructor (String (value, loc), [])
    | Literal (Int { loc; raw; base; _ }) ->
      let ty = type_of_loc loc in
      let value = Integers.int64_of_string_opt raw base |> Option.get in
      Constructor (Int (value, ty = Bool), [])
    (* Identifier pattern may be for an enum variant, otherwise it is a variable and can be
       treated as a wildcard for exhaustiveness/usefulness checking. *)
    | Identifier { name; _ } ->
      let binding = Type_context.get_value_binding ~cx name.loc in
      (match binding.declaration with
      | CtorDecl _ ->
        let ty = type_of_loc name.loc in
        let (type_args, adt_sig) = Type_util.cast_to_adt_type ty in
        Constructor (Variant (name.name, adt_sig, type_args), [])
      | _ -> Wildcard)
    (* Named wildcard patterns create sub patterns of same length as tuple elements/record fields
       which consist of all wildcards. *)
    | NamedWildcard { loc; name } ->
      let name = name.name.name in
      let ty = type_of_loc loc in
      let (type_args, adt_sig) = Type_util.cast_to_adt_type ty in
      (match (SMap.find name adt_sig.variants).kind with
      | Tuple elements ->
        let elements = wildcards_vector (List.length elements) in
        Constructor (Variant (name, adt_sig, type_args), elements)
      | Record fields ->
        let fields = wildcards_vector (SMap.cardinal fields) in
        Constructor (Variant (name, adt_sig, type_args), fields)
      | Enum -> failwith "Expected tuple or record variant")
    | Tuple { name = None; elements; _ } ->
      let elements = List.map pattern_of_pattern_node elements in
      Constructor (Tuple (List.length elements), elements)
    | Tuple { loc; name = Some name; elements } ->
      let ty = type_of_loc loc in
      let (type_args, adt_sig) = Type_util.cast_to_adt_type ty in
      let elements = List.map pattern_of_pattern_node elements in
      Constructor (Variant (name.name.name, adt_sig, type_args), elements)
    | Record { loc; name; fields; _ } ->
      (* Fetch field sigs from ADT sig *)
      let name = name.name.name in
      let ty = type_of_loc loc in
      let (type_args, adt_sig) = Type_util.cast_to_adt_type ty in
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
            | None -> Wildcard :: acc
            | Some pattern -> pattern :: acc)
          field_sigs
          []
      in
      Constructor (Variant (name, adt_sig, type_args), List.rev fields)
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
  let pattern = pattern_of_pattern_node case_node.Ast.Match.Case.pattern in
  if num_scrutinees = 1 then
    [pattern]
  else
    (* If there are multiple scrutinees, the top level pattern is either a tuple that is fake and
       should be unwrapped into its constituent patterns, or a wildcard that should be expanded. *)
    match pattern with
    | Wildcard -> wildcards_vector num_scrutinees
    | Constructor (Tuple _, sub_patterns) -> sub_patterns
    | _ -> failwith "Unexpected pattern"
