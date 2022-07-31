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

module Pattern = struct
  type t = {
    pattern: pattern;
    bindings: Loc.t list;
  }

  and pattern =
    | Wildcard
    | Constructor of Ctor.t * t list
    | Or of t list
end

module PatternPath = struct
  type t =
    | Root of Mir.Value.t option
    | TupleField of {
        id: int;
        parent: t;
        index: int;
      }
    | VariantField of {
        id: int;
        parent: t;
        field: variant_field;
        variant_name: string;
        adt_sig: Types.AdtSig.t;
        type_args: Types.Type.t list;
      }

  and variant_field =
    | TupleIndex of int
    | RecordField of string

  let max_id = ref 0

  let mk_new_id () =
    let id = !max_id in
    max_id := id + 1;
    id

  let get_field_id path =
    match path with
    | Root _ -> failwith "Expected field"
    | TupleField { id; _ }
    | VariantField { id; _ } ->
      id

  let get_field_parent path =
    match path with
    | Root _ -> failwith "Expected field"
    | TupleField { parent; _ }
    | VariantField { parent; _ } ->
      parent
end

module AstNodes = struct
  type t = {
    loc: Loc.t;
    guard: Ast.Expression.t option;
    body: body;
  }

  and body =
    (* AST nodes to emit for the body *)
    | Expression of Ast.Expression.t
    | Statement of Ast.Statement.t
    (* A particular MIR block to jump to as the body *)
    | Block of Mir.Block.t
end

module DecisionTree = struct
  type t =
    | Leaf of {
        (* The match case AST nodes this leaf corresponds to, or None if this is a destructuring *)
        ast_nodes: AstNodes.t option;
        mutable bindings: (PatternPath.t * Loc.t) list;
        (* If this case is guarded, tree for when the guard fails. Filled after creation. *)
        mutable guard_fail_case: t option;
      }
    | Test of {
        scrutinee: PatternPath.t;
        cases: (Ctor.t * t) list;
        default_case: t option;
      }
end

module CaseRow = struct
  type t = {
    (* The match case AST nodes this row corresponds to, or None if this is a destructuring *)
    ast_nodes: AstNodes.t option;
    (* The pattern vector corresponding to this case *)
    row: Pattern.t list;
    (* All binding collected for this case so far. Invariant: all wildcards with bindings at the top
       level of row must exist in this list. Since row will be all wildcards when leaf node is
       generated, we are guaranteed that all bindings will be added to thist list. *)
    bindings: (PatternPath.t * Loc.t) list;
  }
end

type pattern_matrix = CaseRow.t list

let wildcard = { Pattern.pattern = Wildcard; bindings = [] }

let wildcards_vector n = List_utils.make n wildcard

(* Find the signature for a column, returning a tuple containing a list of all unique head
   constructors in that column, as well as whether the signature is complete. *)
let find_signature column =
  let rec gather_ctors acc pattern =
    match pattern.Pattern.pattern with
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

(* Specialized matrix from paper. Specialize along column i using the given constructor.

   Also take a list of the paths for each constructor field after is expanded. If there are are any
   bindings in the expanded sub patterns, add them to the row along with their path. *)
let rec specialize_matrix i ctor ctor_path ctor_field_paths matrix =
  List.filter_map
    (fun ({ CaseRow.row; bindings; _ } as case_row) ->
      let (row_pre, target_pattern, row_post) = List_utils.split_around i row in
      match target_pattern.pattern with
      | Constructor (target_head_ctor, sub_patterns) when Ctor.equal ctor target_head_ctor ->
        (* Preserve case row bindings invariant. Collect newly discovered bindings along with their path. *)
        let bindings =
          List.fold_left2
            (fun acc sub_pattern sub_pattern_path ->
              match sub_pattern with
              | { Pattern.bindings; _ } when bindings <> [] ->
                List.fold_left
                  (fun acc binding_loc -> (sub_pattern_path, binding_loc) :: acc)
                  acc
                  bindings
              | _ -> acc)
            bindings
            sub_patterns
            ctor_field_paths
        in
        Some [{ case_row with row = row_pre @ sub_patterns @ row_post; bindings }]
      | Constructor _ -> None
      | Wildcard ->
        let wildcard_sub_patterns = wildcards_vector (Ctor.arity ctor) in
        Some [{ case_row with row = row_pre @ wildcard_sub_patterns @ row_post }]
      | Or target_heads ->
        let expanded_matrix =
          List.map
            (fun target_head ->
              (* Preserve case row bindings invariant. Collect newly discovered bindings inside the
                 or pattern, along with their path. *)
              let bindings =
                List.fold_left
                  (fun acc binding_loc -> (ctor_path, binding_loc) :: acc)
                  case_row.bindings
                  target_head.Pattern.bindings
              in
              { case_row with row = row_pre @ (target_head :: row_post); bindings })
            target_heads
        in
        Some (specialize_matrix i ctor ctor_path ctor_field_paths expanded_matrix))
    matrix
  |> List.flatten

(* Default matrix from paper. Create default matrix along column i. *)
let rec default_matrix i matrix =
  List.filter_map
    (fun ({ CaseRow.row; _ } as case_row) ->
      let (row_pre, target_pattern, row_rest) = List_utils.split_around i row in
      match target_pattern.pattern with
      | Constructor _ -> None
      | Wildcard -> Some [{ case_row with row = row_pre @ row_rest }]
      | Or target_heads ->
        let expanded_matrix =
          List.map
            (fun target_head -> { case_row with row = row_pre @ (target_head :: row_rest) })
            target_heads
        in
        Some (default_matrix i expanded_matrix))
    matrix
  |> List.flatten

(* Specialize a single column in the scrutinee vector given a target constructor, removing the item
   at that column and expanding its subfields in place if it has any. Returns both the specialized
   scrutinee, and its expanded section. *)
let specialize_scrutinee_vector column_index ctor scrutinee_vector =
  let (pre_scrutinees, parent, post_scrutinees) =
    List_utils.split_around column_index scrutinee_vector
  in
  let rec mk_tuple_paths i acc =
    match i with
    | 0 -> acc
    | _ ->
      mk_tuple_paths
        (i - 1)
        (PatternPath.(TupleField { id = mk_new_id (); parent; index = i - 1 }) :: acc)
  in
  let rec mk_tuple_variant_paths variant_name adt_sig type_args i acc =
    match i with
    | 0 -> acc
    | _ ->
      let path_field =
        PatternPath.(
          VariantField
            {
              id = mk_new_id ();
              parent;
              field = TupleIndex (i - 1);
              variant_name;
              adt_sig;
              type_args;
            })
      in
      mk_tuple_variant_paths variant_name adt_sig type_args (i - 1) (path_field :: acc)
  in
  let mk_record_variant_paths variant_name adt_sig type_args fields =
    let fields =
      SMap.fold
        (fun name _ acc ->
          PatternPath.(
            VariantField
              {
                id = mk_new_id ();
                parent;
                field = RecordField name;
                variant_name;
                adt_sig;
                type_args;
              })
          :: acc)
        fields
        []
    in
    List.rev fields
  in
  let expanded_scrutinee =
    match ctor with
    | Ctor.Tuple arity -> mk_tuple_paths arity []
    | Variant (variant_name, adt_sig, type_args) ->
      (match (SMap.find variant_name adt_sig.variants).kind with
      | Enum -> []
      | Tuple elements ->
        mk_tuple_variant_paths variant_name adt_sig type_args (List.length elements) []
      | Record fields -> mk_record_variant_paths variant_name adt_sig type_args fields)
    | _ -> []
  in
  (pre_scrutinees @ expanded_scrutinee @ post_scrutinees, expanded_scrutinee)

(* Analogue to default matrix for scrutinee vector, removing column at index from scrutinee vector *)
let default_scrutinee_vector column_index scrutinee_vector =
  let (pre_scrutinees, _, post_scrutinees) =
    List_utils.split_around column_index scrutinee_vector
  in
  pre_scrutinees @ post_scrutinees

let rec build_match_decision_tree
    ~(ecx : Ecx.t) (scrutinee_vals : Mir.Value.t option list) (case_nodes : Ast.Match.Case.t list) =
  let pattern_matrix =
    List.map (fun case_node -> rows_of_case_node ~ecx ~scrutinee_vals case_node) case_nodes
    |> List.flatten
  in
  let scrutinee_vector =
    List.map (fun scrutinee_val -> PatternPath.Root scrutinee_val) scrutinee_vals
  in
  build_decision_tree ~prev_guard:None scrutinee_vector pattern_matrix

and build_destructure_decision_tree
    ~(ecx : Ecx.t) (value : Mir.Value.t) (pattern_node : Ast.Pattern.t) =
  let pattern_matrix = rows_of_destructuring_node ~ecx (Some value) pattern_node None in
  let scrutinee_vector = [PatternPath.Root (Some value)] in
  build_decision_tree ~prev_guard:None scrutinee_vector pattern_matrix

and build_match_test_decision_tree
    ~(ecx : Ecx.t)
    (scrutinee_val : Mir.Value.t option)
    (pattern : Ast.Pattern.t)
    (true_ast_nodes : AstNodes.t)
    (false_ast_nodes : AstNodes.t) =
  let test_pattern_rows =
    rows_of_destructuring_node ~ecx scrutinee_val pattern (Some true_ast_nodes)
  in
  let wildcard_row =
    {
      CaseRow.ast_nodes = Some false_ast_nodes;
      row = [{ Pattern.pattern = Wildcard; bindings = [] }];
      bindings = [];
    }
  in
  let pattern_matrix = test_pattern_rows @ [wildcard_row] in
  let scrutinee_vector = [PatternPath.Root scrutinee_val] in
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
  let ({ CaseRow.row = first_row; _ } as first_case) = List.hd matrix in
  let first_row_all_patterns =
    List.for_all
      (fun pattern ->
        match pattern.Pattern.pattern with
        | Wildcard -> true
        | _ -> false)
      first_row
  in
  if first_row_all_patterns then (
    let ast_nodes = first_case.ast_nodes in
    let leaf_node =
      DecisionTree.Leaf { ast_nodes; bindings = first_case.bindings; guard_fail_case = None }
    in
    connect_prev_guard leaf_node;

    (* If this case is guarded we must connect its fail case to the tree created for its unguarded matrix *)
    (match ast_nodes with
    | Some { guard = Some _; _ } ->
      let unguarded_matrix = List.tl matrix in
      ignore (build_decision_tree ~prev_guard:(Some leaf_node) scrutinee_vector unguarded_matrix)
    | _ -> ());
    leaf_node
  ) else
    (* If first row is not all patterns, we must select a column to test using heuristics *)
    let (column_index, signature, is_complete) = choose_column matrix in
    let scrutinee = List.nth scrutinee_vector column_index in

    (* Build decision trees on the specialized matrix for each constructor in the chosen column *)
    let cases =
      List.fold_left
        (fun cases ctor ->
          let (scrutinee_vector, ctor_field_paths) =
            specialize_scrutinee_vector column_index ctor scrutinee_vector
          in
          let specialized_matrix =
            specialize_matrix column_index ctor scrutinee ctor_field_paths matrix
          in
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
  let ({ CaseRow.row = first_row; _ }, _) = List_utils.split_first matrix in
  let (_, all_columns) =
    List.fold_left
      (fun (i, all_columns) _ -> (i + 1, ISet.add i all_columns))
      (0, ISet.empty)
      first_row
  in

  (* Transpose matrix to list of columns, and set up column signatures to be lazily generated *)
  let transposed_matrix = List_utils.transpose (List.map (fun { CaseRow.row; _ } -> row) matrix) in
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
  | { Pattern.pattern = Wildcard; _ } :: _ ->
    0
  | { pattern = Constructor _ | Or _; _ } :: rest -> 1 + column_prefix_heuristic rest signature

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

and pattern_of_pattern_node ~(ecx : Ecx.t) ~bindings ~scrutinee pattern =
  let collect_root_binding root_val_opt loc =
    match root_val_opt with
    | None -> ()
    | Some root_val -> bindings := (PatternPath.Root root_val, loc) :: !bindings
  in
  let type_of_loc loc =
    let tvar_id = Type_context.get_tvar_from_loc ~cx:ecx.pcx.type_ctx loc in
    Ecx.find_rep_non_generic_type ~ecx (TVar tvar_id)
  in
  let mk_pattern pattern = { Pattern.pattern; bindings = [] } in
  let rec pattern_of_pattern_node ~root_val_opt pattern =
    match pattern with
    (* Wildcards and literals are emitted directly *)
    | Ast.Pattern.Wildcard _ -> wildcard
    (* Attach binding to inner pattern. Collect binding if at root level. *)
    | Binding { pattern; name; _ } ->
      collect_root_binding root_val_opt name.loc;
      let pattern = pattern_of_pattern_node ~root_val_opt pattern in
      { pattern with bindings = name.loc :: pattern.bindings }
    | Literal (Unit _) -> mk_pattern (Constructor (Unit, []))
    | Literal (Bool { value; _ }) -> mk_pattern (Constructor (Bool value, []))
    | Literal (String { loc; value }) -> mk_pattern (Constructor (String (value, loc), []))
    | Literal (Int { loc; raw; base; _ }) ->
      let ty = type_of_loc loc in
      let value = Integers.int64_of_string_opt raw base |> Option.get in
      mk_pattern (Constructor (Int (value, ty = Byte), []))
    | Literal (Char { loc; value }) ->
      let ty = type_of_loc loc in
      mk_pattern (Constructor (Int (Integers.int64_of_char value, ty = Byte), []))
    (* Identifier pattern may be for an enum variant, otherwise it is a variable and can be treated
       as a wildcard. Collect binding if at root level. *)
    | Identifier { name; _ } ->
      let binding = Type_context.get_value_binding ~cx:ecx.pcx.type_ctx name.loc in
      (match binding.declaration with
      | CtorDecl _ ->
        let ty = type_of_loc name.loc in
        let (type_args, adt_sig) = Type_util.cast_to_adt_type ty in
        mk_pattern (Constructor (Variant (name.name, adt_sig, type_args), []))
      | _ ->
        collect_root_binding root_val_opt name.loc;
        { pattern = Wildcard; bindings = [name.loc] })
    (* Named wildcard patterns create sub patterns of same length as tuple elements/record fields
       which consist of all wildcards. *)
    | NamedWildcard { loc; name } ->
      let name = name.name.name in
      let ty = type_of_loc loc in
      let (type_args, adt_sig) = Type_util.cast_to_adt_type ty in
      (match (SMap.find name adt_sig.variants).kind with
      | Tuple elements ->
        let elements = wildcards_vector (List.length elements) in
        mk_pattern (Constructor (Variant (name, adt_sig, type_args), elements))
      | Record fields ->
        let fields = wildcards_vector (SMap.cardinal fields) in
        mk_pattern (Constructor (Variant (name, adt_sig, type_args), fields))
      | Enum -> failwith "Expected tuple or record variant")
    | Tuple { name = None; elements; _ } ->
      let elements = List.map (pattern_of_pattern_node ~root_val_opt:None) elements in
      mk_pattern (Constructor (Tuple (List.length elements), elements))
    | Tuple { loc; name = Some name; elements } ->
      let ty = type_of_loc loc in
      let (type_args, adt_sig) = Type_util.cast_to_adt_type ty in
      let elements = List.map (pattern_of_pattern_node ~root_val_opt:None) elements in
      mk_pattern (Constructor (Variant (name.name.name, adt_sig, type_args), elements))
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
            SMap.add name (pattern_of_pattern_node ~root_val_opt:None value) acc)
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
      mk_pattern (Constructor (Variant (name, adt_sig, type_args), List.rev fields))
    | Or or_ ->
      (* Or patterns are associative so flatten all or patterns recursively gathered from either side *)
      let rec gather_or_branches acc or_ =
        let open Ast.Pattern in
        let { Or.left; right; _ } = or_ in
        let acc =
          match left with
          | Or or_ -> gather_or_branches acc or_
          | _ -> pattern_of_pattern_node ~root_val_opt left :: acc
        in
        match right with
        | Or or_ -> gather_or_branches acc or_
        | _ -> pattern_of_pattern_node ~root_val_opt right :: acc
      in
      let or_branches = gather_or_branches [] or_ in
      mk_pattern (Or (List.rev or_branches))
  in
  pattern_of_pattern_node ~root_val_opt:(Some scrutinee) pattern

and rows_of_case_node ~ecx ~scrutinee_vals case_node =
  let pattern_node = case_node.Ast.Match.Case.pattern in
  let body =
    match case_node.right with
    | Expression expr -> AstNodes.Expression expr
    | Statement stmt -> Statement stmt
  in
  let ast_nodes = Some { AstNodes.loc = case_node.loc; guard = case_node.guard; body } in
  match scrutinee_vals with
  | [scrutinee] ->
    let bindings = ref [] in
    let row = [pattern_of_pattern_node ~ecx ~bindings ~scrutinee pattern_node] in
    [{ CaseRow.ast_nodes; row; bindings = !bindings }]
  | scrutinees ->
    (* Recursively expand or patterns at top level *)
    let rec rows_of_top_level pattern_node scrutinees =
      match pattern_node with
      | Ast.Pattern.Wildcard _ ->
        [{ CaseRow.ast_nodes; row = wildcards_vector (List.length scrutinees); bindings = [] }]
      (* Top level tuple patterns are unwrapped, and have bindings collected *)
      | Tuple { elements; _ } ->
        let bindings = ref [] in
        let row =
          List.fold_left2
            (fun acc pattern_node scrutinee ->
              let pattern = pattern_of_pattern_node ~ecx ~bindings ~scrutinee pattern_node in
              pattern :: acc)
            []
            elements
            scrutinee_vals
        in
        [{ CaseRow.ast_nodes; row = List.rev row; bindings = !bindings }]
      | Or { left; right; _ } ->
        rows_of_top_level left scrutinees @ rows_of_top_level right scrutinees
      | _ -> failwith "Invalid pattern for multiple match arguments"
    in
    rows_of_top_level pattern_node scrutinees

and rows_of_destructuring_node ~ecx value pattern_node ast_nodes =
  let bindings = ref [] in
  let row = [pattern_of_pattern_node ~ecx ~bindings ~scrutinee:value pattern_node] in
  [{ CaseRow.ast_nodes; row; bindings = !bindings }]
