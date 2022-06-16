open Basic_collections
open Bindings

type t = {
  (* A list containing a copy of every AST along with the filename *)
  mutable modules: (string * Ast.Module.t) list;
  mutable bindings: Bindings.t;
  mutable module_tree: Module_tree.t;
  mutable main_loc: Loc.t option;
  type_ctx: Type_context.t;
}

let mk_pcx () =
  let bindings = Bindings.mk () in
  {
    modules = [];
    bindings;
    module_tree = SMap.empty;
    main_loc = None;
    type_ctx = Type_context.mk ~bindings;
  }

let add_modules ~pcx modules = pcx.modules <- modules @ pcx.modules

let set_module_tree ~pcx module_tree = pcx.module_tree <- module_tree

let set_main_loc ~pcx main_loc =
  match main_loc with
  | None -> ()
  | Some main_loc ->
    pcx.main_loc <- Some main_loc;
    Type_context.(set_main_loc ~cx:pcx.type_ctx main_loc)
