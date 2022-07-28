module MirTransform = struct
  type t =
    | Normalize
    | ConstantFolding
    | SimplifyInstructions
    (* Dead instruction elimination *)
    | DIE
    | SSADestruction

  let compare = Stdlib.compare

  let of_string str =
    match str with
    | "normalize" -> Some Normalize
    | "constant-folding" -> Some ConstantFolding
    | "simplify-instructions" -> Some SimplifyInstructions
    | "die" -> Some DIE
    | "ssa-destruction" -> Some SSADestruction
    | _ -> None
end

let apply_transforms (program : Mir.Program.t) (transforms : MirTransform.t list) =
  List.iter
    (fun transform ->
      match transform with
      | MirTransform.Normalize -> Mir_normalizer.run ~program
      | SimplifyInstructions -> Simplify_instructions.run ~program
      | ConstantFolding -> Fold_constants.run ~program
      | DIE -> Dead_instruction_elimination.run ~program
      | SSADestruction -> Mir_ssa_destruction.run program)
    transforms

(* Full set of optimizations *)
let optimize program =
  apply_transforms program [SimplifyInstructions; ConstantFolding; DIE; Normalize]

(* Minimal transformations needed to emit assembly, without optimization set *)
let transform_for_assembly program = apply_transforms program [ConstantFolding; Normalize]

let transform_for_dump_ir program = apply_transforms program [Normalize]
