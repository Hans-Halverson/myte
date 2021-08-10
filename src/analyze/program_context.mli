type t = {
  mutable modules: (string * Ast.Module.t) list;
  mutable bindings: Bindings.Bindings.t;
  mutable module_tree: Module_tree.t;
  mutable main_loc: Loc.t option;
  type_ctx: Type_context.t;
}

val mk_pcx : unit -> t

val add_resolved_modules : pcx:t -> (string * Ast.Module.t) list -> unit

val set_module_tree : pcx:t -> Module_tree.t -> unit

val set_main_loc : pcx:t -> Loc.t option -> unit
