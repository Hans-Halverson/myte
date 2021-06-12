open Basic_collections
open X86_gen_context
open X86_register_allocation

let gen_x86_program ir =
  (* Filter out stdlib for printing *)
  let ir =
    if Opts.dump_virtual_asm () || Opts.dump_asm () then
      Mir.filter_stdlib ir
    else
      ir
  in

  (* Generate virtual assembly *)
  let gcx = Gcx.mk () in
  X86_gen_virtual.gen ~gcx ir;

  (* Optionally dump virtual assembly to stdout *)
  if Opts.dump_virtual_asm () then (
    print_string (X86_pp.pp_x86_program ~gcx);
    exit 0
  );

  (* Perform register allocation for each function *)
  IMap.iter
    (fun _ func ->
      let register_allocator = RegisterAllocator.mk ~gcx ~func in
      RegisterAllocator.allocate_registers ~ra:register_allocator)
    gcx.funcs_by_id;

  (* Color and allocate physical stack slots for virtual stack slots *)
  X86_stack_coloring.allocate_stack_slots ~gcx;

  (* Simplify program, write function prologue and epilogue *)
  Gcx.compress_jump_aliases ~gcx;
  Gcx.remove_redundant_instructions ~gcx;
  X86_calling_conventions.write_function_prologues ~gcx;
  X86_calling_conventions.write_function_epilogues ~gcx;

  (* Optionally dump assembly to stdout *)
  if Opts.dump_asm () then begin
    print_string (X86_pp.pp_x86_program ~gcx);
    exit 0
  end;
  gcx
