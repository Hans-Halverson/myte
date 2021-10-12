open Ast
open Analyze_error

type exhaustive =
  | BlockMissingReturn of Loc.t
  | IfMissingAltern of Loc.t
  | MatchInexhaustiveCase of Loc.t
  | AtomicInexhaustiveStatement
  | Exhaustive

let add_error ~cx loc err = Type_context.add_error ~cx loc err

let rec exhaustive ~cx stmt =
  let open Statement in
  match stmt with
  | Return _ -> Exhaustive
  | Expression _
  | Assignment _
  | While _
  | For _
  | Break _
  | Continue _
  | VariableDeclaration _
  | FunctionDeclaration _ ->
    AtomicInexhaustiveStatement
  | Block { Block.loc; statements; _ } ->
    let rec visit_statements ~prev_exhaustive stmts =
      match stmts with
      | [] ->
        (match prev_exhaustive with
        | AtomicInexhaustiveStatement -> BlockMissingReturn loc
        | other -> other)
      | stmt :: stmts ->
        if prev_exhaustive = Exhaustive then
          Exhaustive
        else
          let exhaustive = exhaustive ~cx stmt in
          visit_statements ~prev_exhaustive:exhaustive stmts
    in
    visit_statements ~prev_exhaustive:AtomicInexhaustiveStatement statements
  | If { If.loc; conseq; altern; _ } ->
    (match exhaustive ~cx conseq with
    | Exhaustive ->
      (match altern with
      | None -> IfMissingAltern loc
      | Some altern -> exhaustive ~cx altern)
    | inexhaustive -> inexhaustive)
  | Match match_ -> exhaustive_match ~cx match_

and exhaustive_match ~cx match_ =
  let { Match.cases; _ } = match_ in
  let inhexaustive_opt =
    List.fold_left
      (fun acc { Match.Case.loc; right; _ } ->
        match acc with
        | Some _ -> acc
        | None ->
          (match right with
          | Match.Case.Statement stmt ->
            (match exhaustive ~cx stmt with
            | Exhaustive -> acc
            | inexhaustive -> Some inexhaustive)
          | Expression (Match match_) ->
            (match exhaustive_match ~cx match_ with
            | Exhaustive -> acc
            | inexhaustive -> Some inexhaustive)
          | _ -> Some (MatchInexhaustiveCase loc)))
      None
      cases
  in
  match inhexaustive_opt with
  | None -> Exhaustive
  | Some inexhaustive -> inexhaustive

let analyze_function ~cx func =
  let { Function.name; body; _ } = func in
  match body with
  | Signature
  | Expression _ ->
    ()
  | Block ({ Statement.Block.loc; _ } as block) ->
    let exhaustive = exhaustive ~cx (Statement.Block block) in
    (match exhaustive with
    | BlockMissingReturn loc
    | IfMissingAltern loc
    | MatchInexhaustiveCase loc ->
      add_error ~cx (Loc.point_end loc) (InexhaustiveReturn name)
    | AtomicInexhaustiveStatement -> add_error ~cx (Loc.point_end loc) (InexhaustiveReturn name)
    | _ -> ())
