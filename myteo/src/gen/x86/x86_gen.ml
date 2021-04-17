open X86_gen_context

let gen_x86_program ir =
  let gcx = Gcx.mk () in
  let program = X86_gen_virtual.gen ~gcx ir in
  (gcx, program)
