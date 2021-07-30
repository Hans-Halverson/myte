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
    ty: Type.t;
    ctor: ctor;
  }

  and ctor =
    | Unit
    | Bool of bool
    | Int of Int64.t
    | String of string
    | Tuple
    | Variant of string

  let unit_ctor = { ctor = Unit; ty = Unit }

  let true_ctor = { ctor = Bool true; ty = Bool }

  let false_ctor = { ctor = Bool false; ty = Bool }

  let mk_bool_ctor value =
    if value then
      true_ctor
    else
      false_ctor

  let sub_patterns_length { ctor; ty } =
    match ctor with
    | Unit
    | Bool _
    | Int _
    | String _ ->
      0
    | Tuple -> List.length (Type_util.cast_to_tuple_type ty)
    | Variant name ->
      let (_, adt_sig) = Type_util.cast_to_adt_type ty in
      (match SMap.find name adt_sig.variants with
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

module Completeness = struct
  type t =
    | Complete of Ctor.t list
    | Incomplete of missing

  and missing =
    | Enumerable of Ctor.t list
    | Unenumerable
end

type pattern =
  | Wildcard
  | Constructor of Ctor.t * pattern list

let wildcards_vector n = List_utils.make n Wildcard

type pattern_vector = pattern list

type pattern_matrix = pattern_vector list

type witness = Witness of pattern list

(* A list of witnesses of non-exhaustiveness, if empty then check was exhaustive *)
type useful_result = witness list

(* Determine whether the head constructors of a matrix form a complete signature. If so, return
   a list of all signatures. Otherwise return the missing signatures, if they can be enumerated.
   
   Only return enumerated missing signature if the `create_witnesses` flag is present. *)
let signature_completeness ~create_witness matrix =
  let head_ctors =
    List.filter_map
      (fun row ->
        match List.hd row with
        | Wildcard -> None
        | Constructor (ctor, _) -> Some ctor)
      matrix
  in
  match head_ctors with
  (* If there are no head constructors, signature is incomplete *)
  | [] -> Completeness.Incomplete Unenumerable
  | { ty; _ } :: _ ->
    (match ty with
    (* Unit and Tuple types have a single constructor, so when a head constructor is present the
       signature is necessarily complete. *)
    | Unit -> Complete [Ctor.unit_ctor]
    | Tuple _ -> Complete [{ Ctor.ctor = Tuple; ty }]
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
        Incomplete (Enumerable [{ Ctor.ctor = Int unused_byte; ty }])
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
        Incomplete (Enumerable [{ Ctor.ctor = Int unused_int; ty }])
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
        Incomplete
          (Enumerable [{ Ctor.ctor = String string_witness; ty = Std_lib.mk_string_type () }])
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
        Complete [Ctor.true_ctor; Ctor.false_ctor]
      else if create_witness then
        Incomplete (Enumerable [Ctor.mk_bool_ctor (BSet.choose missing_values)])
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
          SMap.fold (fun name _ acc -> { Ctor.ctor = Variant name; ty } :: acc) adt_sig.variants []
        in
        Complete all_variant_ctors
      else if create_witness then
        let missing_variant_ctors =
          SMap.fold (fun name _ acc -> { Ctor.ctor = Variant name; ty } :: acc) missing_variants []
        in
        Incomplete (Enumerable missing_variant_ctors)
      else
        Incomplete Unenumerable
    | _ -> failwith "Invalid head constructor type")

(* Specialized matrix from paper *)
let specialize_matrix ctor matrix =
  List.filter_map
    (fun row ->
      let (row_head, row_rest) = List_utils.split_first row in
      match row_head with
      | Constructor (row_head_ctor, sub_patterns) when Ctor.equal ctor row_head_ctor ->
        Some (sub_patterns @ row_rest)
      | Constructor _ -> None
      | Wildcard ->
        let wildcard_sub_patternss = wildcards_vector (Ctor.sub_patterns_length ctor) in
        Some (wildcard_sub_patternss @ row_rest))
    matrix

(* Default matrix from paper *)
let default_matrix matrix =
  List.filter_map
    (fun row ->
      let (row_head, row_rest) = List_utils.split_first row in
      match row_head with
      | Constructor _ -> None
      | Wildcard -> Some row_rest)
    matrix

(* Implementation of `useful` function from paper. Takes a pattern matrix and a pattern vector,
   and returns a witness of a value that matches the pattern vector but does not match any pattern
   in the pattern matrix, if such a value exists. If such a witness exists, the pattern vector is
   deemed "useful" with respect to the pattern matrix.
   
   Only create a full witness if the `create_witness` flag is set, otherwise return a sigil witness
   whose presence indicates a useful result. *)
let rec useful ~create_witness (matrix : pattern_matrix) (vector : pattern_vector) : useful_result =
  match vector with
  (* Base case - is useful if there are no rows in matrix *)
  | [] ->
    if matrix = [] then
      [Witness []]
    else
      []
  (* Inductive case 1 - head is a constructor, so specialize matrix and recurse *)
  | Constructor (ctor, sub_patterns) :: rest_patterns ->
    let specialized_matrix = specialize_matrix ctor matrix in
    let patterns = sub_patterns @ rest_patterns in
    useful ~create_witness specialized_matrix patterns
  | Wildcard :: rest_vector ->
    (match signature_completeness ~create_witness matrix with
    (* Inductive case 2a - head is wildcard and matrix's head constructors form a complete
       signature. Create a specialized matrix and recurse for every constructor in signature. *)
    | Complete all_ctors ->
      let useful_results =
        List.map
          (fun ctor ->
            let ctor_sub_patterns_length = Ctor.sub_patterns_length ctor in
            let specialized_matrix = specialize_matrix ctor matrix in
            let specialized_vector = wildcards_vector ctor_sub_patterns_length @ rest_vector in
            let useful_result = useful ~create_witness specialized_matrix specialized_vector in
            (* If there are witnesses of usefulness recreate constructor at head of witness, where
               witness contains subpatterns for this constructor at its head. *)
            if create_witness then
              List.map
                (fun (Witness patterns) ->
                  let (ctor_sub_patterns, rest) =
                    List_utils.split_at ctor_sub_patterns_length patterns
                  in
                  Witness (Constructor (ctor, ctor_sub_patterns) :: rest))
                useful_result
            else
              useful_result)
          all_ctors
      in
      List.flatten useful_results
    (* Inductive case 2b - head is wildcard and matrix's head constructors do not form a complete
       signature. Create the default matrix and recurse. *)
    | Incomplete missing ->
      let default_matrix = default_matrix matrix in
      let useful_result = useful ~create_witness default_matrix rest_vector in
      (* If there are witnesses of usefulness add prefix for missing constructors. Unenumerable
         missing constructors can add the generic wildcard pattern, while if there are known missing
         constructors add a chosen missing constructor with wildcard subpatterns. *)
      if create_witness then
        List.map
          (fun (Witness pattern) ->
            match missing with
            | Unenumerable -> Witness (Wildcard :: pattern)
            | Enumerable missing_ctors ->
              let missing_ctor = List.hd missing_ctors in
              let wildcard_subpatterns = wildcards_vector (Ctor.sub_patterns_length missing_ctor) in
              Witness (Constructor (missing_ctor, wildcard_subpatterns) :: pattern))
          useful_result
      else
        useful_result)

(* Convert a match case node into a pattern vector for use in exhaustiveness/reachability checking *)
let pattern_vector_of_case_node ~cx case_node =
  let type_of_loc loc =
    let tvar_id = Type_context.get_tvar_from_loc ~cx loc in
    Type_context.find_rep_type ~cx (TVar tvar_id)
  in
  let rec pattern_of_pattern_node pattern =
    match pattern with
    (* Wildcards and literals are emitted directly *)
    | Ast.Pattern.Wildcard _ -> Wildcard
    | Literal (Unit _) -> Constructor (Ctor.unit_ctor, [])
    | Literal (Bool { value; _ }) -> Constructor (Ctor.mk_bool_ctor value, [])
    | Literal (String { value; _ }) ->
      Constructor ({ Ctor.ctor = String value; ty = Std_lib.mk_string_type () }, [])
    | Literal (Int { loc; raw; base; _ }) ->
      let ty = type_of_loc loc in
      let value = Integers.int64_of_string_opt raw base |> Option.get in
      Constructor ({ Ctor.ctor = Int value; ty }, [])
    (* Identifier pattern may be for an enum variant, otherwise it is a variable and can be
       treated as a wildcard for exhaustiveness/usefulness checking. *)
    | Identifier { name; _ } ->
      let binding = Type_context.get_value_binding ~cx name.loc in
      (match binding.declaration with
      | CtorDecl _ ->
        let ty = type_of_loc name.loc in
        Constructor ({ Ctor.ctor = Variant name.name; ty }, [])
      | _ -> Wildcard)
    (* Named wildcard patterns create sub patterns of same length as tuple elements/record fields
       which consist of all wildcards. *)
    | NamedWildcard { loc; name } ->
      let name = name.name.name in
      let ty = type_of_loc loc in
      let (_, adt_sig) = Type_util.cast_to_adt_type ty in
      (match SMap.find name adt_sig.variants with
      | Tuple elements ->
        let elements = wildcards_vector (List.length elements) in
        Constructor ({ Ctor.ctor = Variant name; ty }, elements)
      | Record fields ->
        let fields = wildcards_vector (SMap.cardinal fields) in
        Constructor ({ Ctor.ctor = Variant name; ty }, fields)
      | Enum -> failwith "Expected tuple or record variant")
    | Tuple { loc; name = None; elements } ->
      let ty = type_of_loc loc in
      let elements = List.map pattern_of_pattern_node elements in
      Constructor ({ Ctor.ctor = Tuple; ty }, elements)
    | Tuple { loc; name = Some name; elements } ->
      let ty = type_of_loc loc in
      let elements = List.map pattern_of_pattern_node elements in
      Constructor ({ Ctor.ctor = Variant name.name.name; ty }, elements)
    | Record { loc; name; fields; _ } ->
      (* Fetch field sigs from ADT sig *)
      let name = name.name.name in
      let ty = type_of_loc loc in
      let (_, adt_sig) = Type_util.cast_to_adt_type ty in
      let field_sigs =
        match SMap.find name adt_sig.variants with
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
      Constructor ({ Ctor.ctor = Variant name; ty }, List.rev fields)
  in
  [pattern_of_pattern_node case_node.Ast.Match.Case.pattern]

(* Whether a vector of patterns can be compressed to a single wildcard pattern for display *)
let rec can_compress_to_wildcard vector =
  List.for_all
    (fun pattern ->
      match pattern with
      | Wildcard -> true
      | Constructor ({ ctor = Tuple; _ }, sub_patterns) -> can_compress_to_wildcard sub_patterns
      (* Single variant ADTs may be compressed *)
      | Constructor ({ ctor = Variant _; ty }, sub_patterns) ->
        let (_, adt_sig) = Type_util.cast_to_adt_type ty in
        SMap.cardinal adt_sig.variants = 1 && can_compress_to_wildcard sub_patterns
      | Constructor _ -> false)
    vector

let string_of_pattern_vector vector =
  let buf = Buffer.create 16 in
  let add_char c = Buffer.add_char buf c in
  let add_string str = Buffer.add_string buf str in
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
    | Wildcard -> add_char '_'
    | Constructor ({ ctor = Unit; _ }, _) -> add_string "()"
    | Constructor ({ ctor = Bool b; _ }, _) -> add_string (Bool.to_string b)
    | Constructor ({ ctor = Int i; _ }, _) -> add_string (Int64.to_string i)
    | Constructor ({ ctor = String s; _ }, _) ->
      add_char '"';
      add_string s;
      add_char '"'
    | Constructor ({ ctor = Tuple; _ }, sub_patterns) -> add_tuple_pattern sub_patterns
    | Constructor ({ ctor = Variant name; ty }, sub_patterns) ->
      let (_, adt_sig) = Type_util.cast_to_adt_type ty in
      (match SMap.find name adt_sig.variants with
      | Enum -> add_string name
      | Tuple _ ->
        add_string name;
        add_char ' ';
        add_tuple_pattern sub_patterns
      | Record fields ->
        let fields = SMap.bindings fields in
        add_string name;
        add_char ' ';
        if can_compress_to_wildcard sub_patterns then
          add_char '_'
        else (
          add_string "{ ";
          let num_fields = List.length sub_patterns in
          List_utils.iteri2
            (fun i (name, _) pattern ->
              add_string name;
              add_string ": ";
              add_pattern pattern;
              if i <> num_fields - 1 then add_string ", ")
            fields
            sub_patterns;
          add_string " }"
        ))
  in
  (match vector with
  | [pattern] -> add_pattern pattern
  | patterns -> add_tuple_pattern patterns);
  Buffer.contents buf

(* If there is only one argument to match, exhaustiveness check vector is a single wildcard.
   Otherwise exhaustiveness check vector is a tuple containing as many wildcards as args. *)
let args_wildcard_vector ~cx (match_ : Ast.Match.t) =
  if List.length match_.args = 1 then
    [Wildcard]
  else
    let arg_tys =
      List.map
        (fun arg ->
          let tvar_id = Type_context.get_tvar_from_loc ~cx (Ast_utils.expression_loc arg) in
          Type_context.find_rep_type ~cx (TVar tvar_id))
        match_.args
    in
    let arg_wildcards = wildcards_vector (List.length match_.args) in
    [Constructor ({ Ctor.ctor = Tuple; ty = Tuple arg_tys }, arg_wildcards)]

class match_analyzer ~cx =
  object (this)
    inherit [unit] Ast_visitor.visitor

    val mutable errors : (Loc.t * Analyze_error.t) list = []

    method errors = errors

    method add_error loc error = errors <- (loc, error) :: errors

    method! match_ _ match_ =
      (* Build pattern matrix and check for reachability of each case *)
      let matrix =
        List.fold_left
          (fun prev_rows case ->
            let row = pattern_vector_of_case_node ~cx case in

            (* Check for reachability of case. A match case is unreachable if it is not useful with
               respect to the matrix of all cases that appear above it in the match statement. *)
            let useful_result = useful ~create_witness:false prev_rows row in
            ( if useful_result = [] then
              let loc = Ast_utils.pattern_loc case.pattern in
              this#add_error loc UnreachableMatchCase );

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
      let useful_result = useful ~create_witness:true matrix wildcard_vector in
      if useful_result <> [] then
        let (Witness vector) = List.hd useful_result in
        let witness_string = string_of_pattern_vector vector in
        this#add_error match_.loc (InexhaustiveMatch witness_string)
  end

let analyze ~cx modules =
  let analyzer = new match_analyzer ~cx in
  List.iter (fun (_, module_) -> analyzer#module_ () module_) modules;
  analyzer#errors
