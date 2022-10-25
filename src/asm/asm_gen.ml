open Basic_collections

let should_filter_stdlib () = Asm_utils.any_dump_asm () && not (Opts.dump_stdlib ())

let preprocess_ir (ir : Mir.Program.t) =
  (* Add empty init function if one does not exist *)
  if (not (Asm_utils.any_dump_asm ())) && not (SMap.mem Mir.init_func_name ir.funcs) then (
    let func = Mir_builders.mk_function ~name:Mir.init_func_name in
    let block = Mir_builders.mk_block ~func in
    func.start_block <- block;
    Mir_builders.mk_ret_ ~block ~arg:None;
    ir.funcs <- SMap.add Mir.init_func_name func ir.funcs
  )
