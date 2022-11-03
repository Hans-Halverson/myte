open Asm
open X86_64_gen_context

let init () = X86_64_builtin.init ()

let gen ir =
  init ();

  (* Generate virtual assembly *)
  let gcx = Gcx.mk () in
  X86_64_codegen.gen ~gcx ir;

  (* Optionally dump virtual assembly to stdout *)
  if Opts.dump_virtual_asm () then (
    print_string (X86_64_pp.pp_program ~gcx);
    exit 0
  );

  (* Perform register allocation for each function *)
  FunctionSet.iter (fun func -> X86_64_register_allocation.run ~gcx ~func) gcx.funcs;

  (* Color and allocate physical stack slots for virtual stack slots *)
  X86_64_stack_coloring.color_stack_slots ~gcx;
  X86_64_stack_frame.write_stack_frame ~gcx;

  (* Apply optimizations *)
  X86_64_simplify_cfg.compress_jump_aliases ~gcx;
  X86_64_address_propagation.run ~gcx;
  X86_64_peephole.run_peephole_optimizations ~gcx;
  X86_64_simplify_cfg.simplify_jumps ~gcx;

  (* Optionally dump assembly to stdout *)
  if Opts.dump_asm () || Opts.dump_full_asm () then begin
    print_string (X86_64_pp.pp_program ~gcx);
    exit 0
  end;

  gcx
