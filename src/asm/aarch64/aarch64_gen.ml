open Asm
open Aarch64_gen_context
open Basic_collections

let gen ir =
  (* Prevent generating init functions which are always generated.
     TODO: Remove this once init functions can be generated *)
  Mir.filter_stdlib ir;
  ir.funcs <- SMap.remove Mir.init_func_name ir.funcs;

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

  Aarch64_stack_frame.write_stack_frame ~gcx;

  Aarch64_simplify_cfg.compress_branch_aliases ~gcx;
  Aarch64_simplify_cfg.simplify_branches ~gcx;

  (* Optionally dump assembly to stdout *)
  if Opts.dump_asm () || Opts.dump_full_asm () then begin
    print_string (Aarch64_pp.pp_program ~gcx);
    exit 0
  end;

  gcx
