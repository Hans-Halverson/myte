module Ocx = Mir_optimize_context

let optimize ir =
  let ocx = Ocx.mk ir in
  Fold_constants.fold_constants_and_prune ~ocx;
  Ocx.normalize ~ocx;
  ocx.program
