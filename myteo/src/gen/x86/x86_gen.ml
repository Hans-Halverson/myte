open Basic_collections
open X86_gen_context
open X86_register_allocation

let gen_x86_program ir =
  let gcx = Gcx.mk () in
  X86_gen_virtual.gen ~gcx ir;
  if Opts.dump_virtual_asm () then (
    print_string (X86_pp.pp_x86_program ~gcx);
    exit 0
  );
  IMap.iter
    (fun _ func ->
      let register_allocator = RegisterAllocator.mk ~gcx ~func in
      RegisterAllocator.allocate_registers ~ra:register_allocator)
    gcx.funcs_by_id;
  X86_stack_coloring.allocate_stack_slots ~gcx;
  Gcx.compress_jump_aliases ~gcx;
  Gcx.remove_redundant_instructions ~gcx;
  X86_calling_conventions.write_function_prologues ~gcx;
  X86_calling_conventions.write_function_epilogues ~gcx;
  if Opts.dump_asm () then (
    print_string (X86_pp.pp_x86_program ~gcx);
    exit 0
  );
  gcx
