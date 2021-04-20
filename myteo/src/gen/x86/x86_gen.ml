open X86_gen_context

let gen_x86_program ir =
  let gcx = Gcx.mk () in
  X86_gen_virtual.gen ~gcx ir;
  if Opts.dump_virtual_asm () then (
    print_string (X86_pp.pp_x86_program ~gcx);
    exit 0
  );
  X86_register_allocation.allocate_registers ~gcx;
  if Opts.dump_asm () then (
    print_string (X86_pp.pp_x86_program ~gcx);
    exit 0
  );
  gcx
