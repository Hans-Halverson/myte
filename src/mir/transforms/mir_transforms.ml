module MirTransform = struct
  type t =
    (* Promote variables to registers in SSA form *)
    | SSA
    | Normalize
    | ConstantFolding
    | SimplifyInstructions
    (* Dead instruction elimination *)
    | DIE
    | SSADestruction

  let compare = Stdlib.compare

  let of_string str =
    match str with
    | "ssa" -> Some SSA
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
      | MirTransform.SSA -> Ssa.run ~program
      | Normalize -> Normalizer.run ~program
      | SimplifyInstructions -> Simplify_instructions.run ~program
      | ConstantFolding -> Fold_constants.run ~program
      | DIE -> Dead_instruction_elimination.run ~program
      | SSADestruction -> Ssa_destruction.run ~program)
    transforms

(* Full set of optimizations *)
let optimize program =
  apply_transforms
    program
    [SSA; SimplifyInstructions; ConstantFolding; DIE; Normalize; SSADestruction]

(* Minimal transformations needed to emit assembly, without optimization set *)
let transform_for_assembly program =
  apply_transforms program [SSA; ConstantFolding; Normalize; SSADestruction]

let transform_for_dump_ir program = apply_transforms program [SSA; Normalize]
