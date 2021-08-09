module Ocx = Mir_optimize_context

let optimize ir =
  let ocx = Ocx.mk ir in
  Fold_constants.fold_constants_and_prune ~ocx;
  Mir_normalizer.normalize ~ocx;
  ocx.program

let transform_for_assembly ir =
  let ocx = Ocx.mk ir in
  Fold_constants.fold_constants_and_prune ~ocx;
  Mir_normalizer.normalize ~ocx;
  ocx.program

let transform_for_dump_ir ir =
  let ocx = Ocx.mk ir in
  Mir_normalizer.consolidate_adjacent_blocks ~ocx;
  Mir_normalizer.remove_empty_init_func ~ocx;
  ocx.program

module MirTransform = struct
  type t =
    | ConstantFolding
    | SSADestruction

  let compare = Stdlib.compare

  let of_string str =
    match str with
    | "constant-folding" -> Some ConstantFolding
    | "ssa-destruction" -> Some SSADestruction
    | _ -> None
end

module MirTransformSet = Set.Make (MirTransform)

let apply_transforms ir transforms =
  let ocx = Ocx.mk ir in

  let apply_if_enabled opt f = if MirTransformSet.mem opt transforms then f () in
  apply_if_enabled ConstantFolding (fun () ->
      Fold_constants.fold_constants_and_prune ~ocx;
      Mir_normalizer.normalize ~ocx);

  let program = ocx.program in
  apply_if_enabled SSADestruction (fun () -> ignore (Mir_ssa_destruction.destruct_ssa program));
  ocx.program
