module MirTransform = struct
  type t =
    | Normalize
    | ConstantFolding
    | SimplifyInstructions
    | SSADestruction

  let compare = Stdlib.compare

  let of_string str =
    match str with
    | "normalize" -> Some Normalize
    | "constant-folding" -> Some ConstantFolding
    | "simplify-instructions" -> Some SimplifyInstructions
    | "ssa-destruction" -> Some SSADestruction
    | _ -> None
end

let apply_transforms (program : Mir.Program.t) (transforms : MirTransform.t list) =
  List.iter
    (fun transform ->
      match transform with
      | MirTransform.Normalize -> Mir_normalizer.normalize ~program
      | SimplifyInstructions -> Simplify_instructions.simplify_instructions ~program
      | ConstantFolding -> Fold_constants.fold_constants_and_prune ~program
      | SSADestruction -> Mir_ssa_destruction.destruct_ssa program)
    transforms

(* Full set of optimizations *)
let optimize program = apply_transforms program [SimplifyInstructions; ConstantFolding; Normalize]

(* Minimal transformations needed to emit assembly, without optimization set *)
let transform_for_assembly program = apply_transforms program [ConstantFolding; Normalize]

let transform_for_dump_ir program = apply_transforms program [Normalize]
