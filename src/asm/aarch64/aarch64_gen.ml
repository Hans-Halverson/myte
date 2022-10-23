open Aarch64_gen_context

let gen ir =
  (* Generate virtual assembly *)
  let gcx = Gcx.mk () in
  Aarch64_codegen.gen ~gcx ir;

  (* Optionally dump virtual assembly to stdout *)
  if Opts.dump_virtual_asm () then (
    print_string (Aarch64_pp.pp_program ~gcx);
    exit 0
  );

  (* Optionally dump assembly to stdout *)
  if Opts.dump_asm () || Opts.dump_full_asm () then begin
    print_string (Aarch64_pp.pp_program ~gcx);
    exit 0
  end;

  gcx
