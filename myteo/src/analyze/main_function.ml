open Ast
open Analyze_error

type program_result =
  | MissingMain
  | SingleMain
  | MultipleMains of Loc.t

let programs_end_loc progs =
  match List.rev progs with
  | [] -> failwith "There is always at least one program"
  | { Program.loc; _ } :: _ -> Loc.point_end loc

let analyze_program acc prog =
  let { Program.statements; _ } = prog in
  List.fold_left
    (fun acc stmt ->
      match stmt with
      | Statement.FunctionDeclaration { Function.name = { Identifier.name = "main"; loc; _ }; _ } ->
        (match acc with
        | MissingMain -> SingleMain
        | SingleMain -> MultipleMains loc
        | MultipleMains _ -> acc)
      | _ -> acc)
    acc
    statements

let analyze progs =
  let result = List.fold_left (fun acc prog -> analyze_program acc prog) MissingMain progs in
  match result with
  | SingleMain -> []
  | MissingMain -> [(programs_end_loc progs, MissingMainFunction)]
  | MultipleMains loc -> [(loc, MultipleMainFunctions)]
