open Ast
open Analyze_error

type exhaustive =
  | BlockMissingReturn of Loc.t
  | IfMissingAltern of Loc.t
  | AtomicInexhaustiveStatement
  | Exhaustive

class analyzer =
  object (this)
    inherit [unit, unit] Ast_visitor.visitor as super

    val mutable errors = []

    method add_error loc err = errors <- (loc, err) :: errors

    method errors () = List.rev errors

    method exhaustive stmt =
      let open Statement in
      match stmt with
      | Return _ -> Exhaustive
      | Expression _
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
            if prev_exhaustive = Exhaustive then (
              this#add_error (Ast_utils.statement_loc stmt) UnreachableStatementAfterReturn;
              Exhaustive
            ) else
              let exhaustive = this#exhaustive stmt in
              visit_statements ~prev_exhaustive:exhaustive stmts
        in
        visit_statements ~prev_exhaustive:AtomicInexhaustiveStatement statements
      | If { If.loc; conseq; altern; _ } ->
        (match this#exhaustive conseq with
        | Exhaustive ->
          (match altern with
          | None -> IfMissingAltern loc
          | Some altern -> this#exhaustive altern)
        | inexhaustive -> inexhaustive)

    method! function_ acc func =
      let { Function.name; body; return; _ } = func in
      begin
        match body with
        | Expression _ -> ()
        | Block ({ Statement.Block.loc; _ } as block) ->
          let exhaustive = this#exhaustive (Statement.Block block) in
          (* Functions that return unit do not need exhaustive returns *)
          let requires_exhaustive =
            let open Type in
            match return with
            | None
            | Some (Primitive { Primitive.kind = Primitive.Unit; _ }) ->
              false
            | Some _ -> true
          in
          (match exhaustive with
          | BlockMissingReturn loc
          | IfMissingAltern loc
            when requires_exhaustive ->
            this#add_error (Loc.point_end loc) (InexhaustiveReturn name)
          | AtomicInexhaustiveStatement when requires_exhaustive ->
            this#add_error (Loc.point_end loc) (InexhaustiveReturn name)
          | _ -> ())
      end;
      super#function_ acc func
  end

let analyze mod_ =
  let analyzer = new analyzer in
  analyzer#module_ () mod_;
  analyzer#errors ()
