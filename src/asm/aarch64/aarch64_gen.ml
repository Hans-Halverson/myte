open Asm
open Aarch64_gen_context

let init () = Aarch64_builtin.init ()

let gen ir =
  init ();

  (* Generate virtual assembly *)
  let gcx = Gcx.mk () in
  Aarch64_codegen.gen ~gcx ir;

  (* Optionally dump virtual assembly to stdout *)
  if Opts.dump_virtual_asm () then (
    print_string (Aarch64_pp.pp_program ~gcx);
    exit 0
  );

  (* Perform register allocation for each function *)
  FunctionSet.iter (fun func -> Aarch64_register_allocation.run ~gcx ~func) gcx.funcs;

  (* Color and allocate physical stack slots for virtual stack slots *)
  Aarch64_stack_coloring.color_stack_slots ~gcx;
  Aarch64_stack_frame.write_stack_frame ~gcx;

  Aarch64_simplify_cfg.compress_branch_aliases ~gcx;
  Aarch64_simplify_cfg.simplify_branches ~gcx;

  (* Optionally dump assembly to stdout *)
  if Opts.dump_asm () || Opts.dump_full_asm () then begin
    print_string (Aarch64_pp.pp_program ~gcx);
    exit 0
  end;

  gcx
