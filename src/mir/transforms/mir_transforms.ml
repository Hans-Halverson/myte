module MirTransform = struct
  type t =
    (* Promote variables to registers in SSA form *)
    | SSA
    | Normalize
    | ConstantFolding
    | SimplifyInstructions
    | Inline
    | SROA
    (* Dead instruction elimination *)
    | DIE
    | JumpThreading
    | SSADestruction

  let compare = Stdlib.compare

  let of_string str =
    match str with
    | "ssa" -> Some SSA
    | "normalize" -> Some Normalize
    | "constant-folding" -> Some ConstantFolding
    | "simplify-instructions" -> Some SimplifyInstructions
    | "inline" -> Some Inline
    | "sroa" -> Some SROA
    | "die" -> Some DIE
    | "jump-threading" -> Some JumpThreading
    | "ssa-destruction" -> Some SSADestruction
    | _ -> None

  let to_string transform =
    match transform with
    | SSA -> "ssa"
    | Normalize -> "normalize"
    | ConstantFolding -> "constant-folding"
    | SimplifyInstructions -> "simplify-instructions"
    | Inline -> "inline"
    | SROA -> "sroa"
    | DIE -> "die"
    | JumpThreading -> "jump-threading"
    | SSADestruction -> "ssa-destruction"
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
      | SROA -> Sroa.run ~program
      | DIE -> Dead_instruction_elimination.run ~program
      | JumpThreading -> Jump_threading.run ~program
      | SSADestruction -> Ssa_destruction.run ~program)
    transforms

(* Full set of optimizations *)
let optimize ~program ~pcx =
  apply_transforms
    ~program
    ~pcx
    [
      (* Initial round of transforms *)
      SSA;
      Inline;
      (* Eliminate dead and unnecessary uses (e.g. single argument phis) *)
      SimplifyInstructions;
      SROA;
      (* Sparse conditional constant propagation *)
      SimplifyInstructions;
      ConstantFolding;
      Normalize;
      (* Jump threading and clean up afterwards *)
      JumpThreading;
      SimplifyInstructions;
      ConstantFolding;
      (* Clean up output *)
      Normalize;
    ]

(* Minimal transformations needed when not optimizing *)
let non_optimized_transforms ~program ~pcx =
  apply_transforms ~program ~pcx [SSA; Inline; ConstantFolding; Normalize]

(* Transforms need for lowering to assembly *)
let transform_for_assembly ~program ~pcx = apply_transforms ~program ~pcx [SSADestruction]

let transform_for_dump_ir ~program ~pcx = apply_transforms ~program ~pcx [SSA; Inline; Normalize]
