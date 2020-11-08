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
    instructions: (Loc.t * Instruction.t) list;
    next: next;
  }

  and id = int

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
    name: string;
    var_id: var_id;
    ty: ValueType.t;
    init: Block.id list;
  }
end =
  Global

and Function : sig
  type t = {
    loc: Loc.t;
    name: string;
    params: (var_id * ValueType.t) list;
    return_ty: ValueType.t;
    body: Block.id list;
  }
end =
  Function

and Instruction : sig
  module LitValue : sig
    type t =
      | Unit
      | Int of int
      | String of string
      | Bool of bool
  end

  module NumericType : sig
    type t = Int
  end

  type t =
    | Lit of var_id * LitValue.t
    | Store of var_id * Loc.t
    | Ret of var_id option
    (* Logical ops *)
    | LogNot of var_id * var_id
    | LogAnd of var_id * var_id * var_id
    | LogOr of var_id * var_id * var_id
    (* Unary numeric ops *)
    | Neg of NumericType.t * var_id * var_id
    (* Binary numeric ops *)
    | Add of NumericType.t * var_id * var_id * var_id
    | Sub of NumericType.t * var_id * var_id * var_id
    | Mul of NumericType.t * var_id * var_id * var_id
    | Div of NumericType.t * var_id * var_id * var_id
    | Eq of NumericType.t * var_id * var_id * var_id
    | Neq of NumericType.t * var_id * var_id * var_id
    | Lt of NumericType.t * var_id * var_id * var_id
    | LtEq of NumericType.t * var_id * var_id * var_id
    | Gt of NumericType.t * var_id * var_id * var_id
    | GtEq of NumericType.t * var_id * var_id * var_id
end =
  Instruction

and ValueType : sig
  type t =
    | Unit
    | Int
    | String
    | Bool
end =
  ValueType

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
