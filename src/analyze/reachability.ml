open Ast

type reachability =
  | Reachable
  | Unreachable of unreachable_reason

and unreachable_reason =
  | AfterReturn
  | AfterBreak
  | AfterContinue
  | Complex

type cx = {
  mutable errors: (Loc.t * Analyze_error.t) list;
  mutable num_loops: int;
}

let mk_cx () = { errors = []; num_loops = 0 }

let add_error ~cx loc err = cx.errors <- (loc, err) :: cx.errors

let enter_loop ~cx = cx.num_loops <- cx.num_loops + 1

let exit_loop ~cx = cx.num_loops <- cx.num_loops - 1

let in_loop ~cx = cx.num_loops > 0

let join_reachabilities reachabilities =
  match reachabilities with
  | [] -> Reachable
  | hd :: tl ->
    List.fold_left
      (fun r1 r2 ->
        match (r1, r2) with
        | (Reachable, _)
        | (_, Reachable) ->
          Reachable
        | (Unreachable reason1, Unreachable reason2) ->
          if reason1 = reason2 then
            r1
          else
            Unreachable Complex)
      hd
      tl

let rec visit_statement ~cx stmt =
  let open Statement in
  match stmt with
  | Expression _
  | Assignment _
  | VariableDeclaration _ ->
    Reachable
  | Return _ -> Unreachable AfterReturn
  | Break { loc } ->
    if not (in_loop ~cx) then add_error ~cx loc Analyze_error.BreakOutsideLoop;
    Unreachable AfterBreak
  | Continue { loc } ->
    if not (in_loop ~cx) then add_error ~cx loc Analyze_error.ContinueOutsideLoop;
    Unreachable AfterContinue
  | Block { Block.statements; _ } -> visit_statements ~cx ~reachability:Reachable statements
  | If { conseq; altern; _ } ->
    let conseq_reachability = visit_statement ~cx conseq in
    let altern_reachability =
      Option_utils.value_map (visit_statement ~cx) ~default:Reachable altern
    in
    join_reachabilities [conseq_reachability; altern_reachability]
  | Match { cases; _ } ->
    let case_reachabilities =
      List.map
        (fun { Match.Case.right; _ } ->
          match right with
          | Match.Case.Statement stmt -> visit_statement ~cx stmt
          | Expression (Match match_) -> visit_statement ~cx (Match match_)
          | _ -> Reachable)
        cases
    in
    join_reachabilities case_reachabilities
  | While { While.body; _ }
  | For { For.body; _ } ->
    enter_loop ~cx;
    ignore (visit_statement ~cx body);
    exit_loop ~cx;
    Reachable
  | FunctionDeclaration func ->
    visit_function ~cx func;
    Reachable

and visit_statements ~cx ~reachability stmts =
  match stmts with
  | [] -> reachability
  | stmt :: stmts ->
    (match reachability with
    | Unreachable reason ->
      let loc = Ast_utils.statement_loc stmt in
      let error_reason =
        match reason with
        | Complex -> None
        | AfterBreak -> Some Analyze_error.AfterBreak
        | AfterContinue -> Some Analyze_error.AfterContinue
        | AfterReturn -> Some Analyze_error.AfterReturn
      in
      add_error ~cx loc (Analyze_error.UnreachableStatement error_reason);
      reachability
    | Reachable ->
      let reachability = visit_statement ~cx stmt in
      visit_statements ~cx ~reachability stmts)

and visit_function ~cx func =
  let open Function in
  match func.body with
  | Signature
  | Expression _ ->
    ()
  | Block { statements; _ } -> ignore (visit_statements ~cx ~reachability:Reachable statements)

let analyze mod_ =
  let open Module in
  let cx = mk_cx () in
  List.iter
    (fun toplevel ->
      match toplevel with
      | FunctionDeclaration func -> visit_function ~cx func
      | TraitDeclaration { methods; _ } -> List.iter (visit_function ~cx) methods
      | VariableDeclaration _
      | TypeDeclaration _ ->
        ())
    mod_.toplevels;
  List.rev cx.errors
