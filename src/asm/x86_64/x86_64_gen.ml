open X86_64_gen_context
open X86_64_instructions

let gen_program ir =
  (* Filter out stdlib for printing *)
  let ir =
    (* TODO: Only use parts of stdlib that we need to, currently strip it all out *)
    if X86_64_utils.any_dump_asm () && not (Opts.dump_stdlib ()) then
      Mir.filter_stdlib ir
    else
      ir
  in

  (* Generate virtual assembly *)
  let gcx = Gcx.mk () in
  X86_64_gen_virtual.gen ~gcx ir;

  (* Optionally dump virtual assembly to stdout *)
  if Opts.dump_virtual_asm () then (
    print_string (X86_64_pp.pp_program ~gcx);
    exit 0
  );

  (* Perform register allocation for each function *)
  FunctionSet.iter (fun func -> X86_64_register_allocation.run ~gcx ~func) gcx.funcs;

  (* Color and allocate physical stack slots for virtual stack slots *)
  X86_64_stack_coloring.allocate_stack_slots ~gcx;

  (* Simplify program, write function prologue and epilogue *)
  Gcx.remove_redundant_moves ~gcx;
  Gcx.compress_jump_aliases ~gcx;
  X86_64_stack_frame.write_function_prologues ~gcx;
  X86_64_stack_frame.write_function_epilogues ~gcx;

  (* Apply optimizations *)
  X86_64_address_propagation.run ~gcx;
  X86_64_peephole.run_peephole_optimizations ~gcx;
  Gcx.remove_redundant_jumps ~gcx;

  (* Optionally dump assembly to stdout *)
  if Opts.dump_asm () || Opts.dump_full_asm () then begin
    print_string (X86_64_pp.pp_program ~gcx);
    exit 0
  end;
  gcx
