module MirTransform = struct
  type t =
    (* Promote variables to registers in SSA form *)
    | SSA
    | Normalize
    | ConstantFolding
    | SimplifyInstructions
    | Inline
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
    | "inline" -> Some Inline
    | "die" -> Some DIE
    | "ssa-destruction" -> Some SSADestruction
    | _ -> None
end

let apply_transforms
    ~(program : Mir.Program.t) ~(pcx : Program_context.t) (transforms : MirTransform.t list) =
  List.iter
    (fun transform ->
      match transform with
      | MirTransform.SSA -> Ssa.run ~program
      | Normalize -> Normalizer.run ~program
      | SimplifyInstructions -> Simplify_instructions.run ~program
      | ConstantFolding -> Fold_constants.run ~program
      | Inline -> Inlining.run ~program ~pcx
      | DIE -> Dead_instruction_elimination.run ~program
      | SSADestruction -> Ssa_destruction.run ~program)
    transforms

(* Full set of optimizations *)
let optimize ~program ~pcx =
  apply_transforms
    ~program
    ~pcx
    [SSA; Inline; SimplifyInstructions; ConstantFolding; DIE; Normalize]

(* Minimal transformations needed when not optimizing *)
let non_optimized_transforms ~program ~pcx =
  apply_transforms ~program ~pcx [SSA; Inline; ConstantFolding; Normalize]

(* Transforms need for lowering to assembly *)
let transform_for_assembly ~program ~pcx = apply_transforms ~program ~pcx [SSADestruction]

let transform_for_dump_ir ~program ~pcx = apply_transforms ~program ~pcx [SSA; Inline; Normalize]
