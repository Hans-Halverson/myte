module MirTransform = struct
  type t =
    | Optimize
    | SSADestruction

  let compare = Stdlib.compare

  let of_string str =
    match str with
    | "optimize" -> Some Optimize
    | "ssa-destruction" -> Some SSADestruction
    | _ -> None
end

module MirTransformSet = Set.Make (MirTransform)

let apply_transforms ir transforms =
  let ir =
    if MirTransformSet.mem Optimize transforms then
      Mir_optimize.optimize ir
    else
      ir
  in
  let ir =
    if MirTransformSet.mem SSADestruction transforms then
      Ssa_destruction.destruct_ssa ir
    else
      ir
  in
  ir
