open Basic_collections
open Bindings

type t = {
  (* A list containing a copy of every AST after symbol resolution (along with the filename) *)
  mutable modules: (string * Ast.Module.t) list;
  mutable bindings: Bindings.t;
  mutable module_tree: Module_tree.t;
  mutable main_loc: Loc.t option;
  type_ctx: Type_context.t;
}

let mk_pcx () =
  {
    modules = [];
    bindings = Bindings.empty;
    module_tree = SMap.empty;
    main_loc = None;
    type_ctx = Type_context.mk ();
  }

let add_resolved_modules ~pcx modules = pcx.modules <- modules @ pcx.modules

let add_bindings ~pcx bindings =
  pcx.bindings <- Bindings.merge pcx.bindings bindings;
  Type_context.set_new_bindings ~cx:pcx.type_ctx pcx.bindings

let set_module_tree ~pcx module_tree = pcx.module_tree <- module_tree

let set_main_loc ~pcx main_loc =
  match main_loc with
  | None -> ()
  | Some _ -> pcx.main_loc <- main_loc
