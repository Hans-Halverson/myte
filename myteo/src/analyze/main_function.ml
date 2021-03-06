open Ast
open Analyze_error

type module_result =
  | MissingMain
  | SingleMain of Loc.t
  | MultipleMains of Loc.t

let analyze_module acc mod_ =
  let open Module in
  let { toplevels; _ } = mod_ in
  List.fold_left
    (fun acc toplevel ->
      match toplevel with
      | FunctionDeclaration { Function.name = { Identifier.name = "main"; loc; _ }; _ } ->
        (match acc with
        | MissingMain -> SingleMain loc
        | SingleMain _ -> MultipleMains loc
        | MultipleMains _ -> acc)
      | _ -> acc)
    acc
    toplevels

let analyze mods =
  let result = List.fold_left (fun acc mod_ -> analyze_module acc mod_) MissingMain mods in
  match result with
  | SingleMain loc -> (Some loc, [])
  | MissingMain -> (None, [(Ast_utils.modules_end_loc mods, MissingMainFunction)])
  | MultipleMains loc -> (None, [(loc, MultipleMainFunctions)])
