let optimize program =
  Fold_constants.fold_constants_and_prune ~program;
  Mir_normalizer.normalize ~program;
  program

let transform_for_assembly program =
  Fold_constants.fold_constants_and_prune ~program;
  Mir_normalizer.normalize ~program;
  program

let transform_for_dump_ir program =
  Mir_normalizer.normalize ~program;
  program

module MirTransform = struct
  type t =
    | Normalize
    | ConstantFolding
    | SSADestruction

  let compare = Stdlib.compare

  let of_string str =
    match str with
    | "normalize" -> Some Normalize
    | "constant-folding" -> Some ConstantFolding
    | "ssa-destruction" -> Some SSADestruction
    | _ -> None
end

module MirTransformSet = Set.Make (MirTransform)

let apply_transforms program transforms =
  let apply_if_enabled opt f = if MirTransformSet.mem opt transforms then f () in

  apply_if_enabled Normalize (fun () -> Mir_normalizer.normalize ~program);

  apply_if_enabled ConstantFolding (fun () ->
      Fold_constants.fold_constants_and_prune ~program;
      Mir_normalizer.normalize ~program);

  apply_if_enabled SSADestruction (fun () -> ignore (Mir_ssa_destruction.destruct_ssa program));
  program
