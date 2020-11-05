open Basic_collections

type var_id = int

module rec Program : sig
  type t = {
    main_id: Block.id;
    blocks: Block.t IMap.t;
    globals: Global.t LocMap.t;
    funcs: Function.t LocMap.t;
  }
end =
  Program

and Block : sig
  type t = {
    id: id;
    label: label;
    instructions: (Loc.t * Instruction.t) list;
    next: next;
  }

  and id = int

  and label =
    | Label of string
    | DebugLabel of string

  and next =
    | Halt
    | Branch of {
        jump: id;
        continue: id;
      }
end =
  Block

and Global : sig
  type t = {
    loc: Loc.t;
    id: var_id;
    init: Block.id list;
  }
end =
  Global

and Function : sig
  type t = {
    loc: Loc.t;
    body: Block.id list;
  }
end =
  Function

and Instruction : sig
  type t =
    (* Literal loads *)
    | LoadUnit of var_id
    | LoadInt of var_id * int
    | LoadString of var_id * string
    | LoadBool of var_id * bool
    (* Logical ops *)
    | LogNot of var_id * var_id
    | LogAnd of var_id * var_id * var_id
    | LogOr of var_id * var_id * var_id
    (* Unary int ops *)
    | NegInt of var_id * var_id
    (* Binary int ops *)
    | AddInt of var_id * var_id * var_id
    | SubInt of var_id * var_id * var_id
    | MulInt of var_id * var_id * var_id
    | DivInt of var_id * var_id * var_id
    | EqInt of var_id * var_id * var_id
    | NeqInt of var_id * var_id * var_id
    | LtInt of var_id * var_id * var_id
    | LteqInt of var_id * var_id * var_id
    | GtInt of var_id * var_id * var_id
    | GteqInt of var_id * var_id * var_id
end =
  Instruction

let max_block_id = ref 0

let mk_block_id () =
  let block_id = !max_block_id in
  max_block_id := block_id + 1;
  block_id

let max_var_id = ref 0

let mk_var_id () =
  let var_id = !max_var_id in
  max_var_id := var_id + 1;
  var_id
