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
    | Continue of id
    | Branch of {
        test: Instruction.BoolValue.t;
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
  module UnitValue : sig
    type t =
      | Lit
      | Var of var_id
  end

  module BoolValue : sig
    type t =
      | Lit of bool
      | Var of var_id
  end

  module StringValue : sig
    type t =
      | Lit of string
      | Var of var_id
  end

  module NumericValue : sig
    type t =
      | IntLit of int
      | IntVar of var_id
  end

  module Value : sig
    type t =
      | Unit of UnitValue.t
      | Numeric of NumericValue.t
      | String of StringValue.t
      | Bool of BoolValue.t
  end

  type t =
    | Mov of var_id * Value.t
    | Ret of Value.t option
    (* Logical ops *)
    | LogNot of var_id * BoolValue.t
    | LogAnd of var_id * BoolValue.t * BoolValue.t
    | LogOr of var_id * BoolValue.t * BoolValue.t
    (* Unary numeric ops *)
    | Neg of var_id * NumericValue.t
    (* Binary numeric ops *)
    | Add of var_id * NumericValue.t * NumericValue.t
    | Sub of var_id * NumericValue.t * NumericValue.t
    | Mul of var_id * NumericValue.t * NumericValue.t
    | Div of var_id * NumericValue.t * NumericValue.t
    | Eq of var_id * NumericValue.t * NumericValue.t
    | Neq of var_id * NumericValue.t * NumericValue.t
    | Lt of var_id * NumericValue.t * NumericValue.t
    | LtEq of var_id * NumericValue.t * NumericValue.t
    | Gt of var_id * NumericValue.t * NumericValue.t
    | GtEq of var_id * NumericValue.t * NumericValue.t
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

let type_of_value v =
  let open Instruction in
  match v with
  | Value.Unit _ -> ValueType.Unit
  | Value.Bool _ -> ValueType.Bool
  | Value.String _ -> ValueType.String
  | Value.Numeric (IntLit _ | IntVar _) -> ValueType.Int

let var_value_of_type var_id ty =
  let open Instruction in
  match ty with
  | ValueType.Unit -> Value.Unit (Var var_id)
  | ValueType.Bool -> Value.Bool (Var var_id)
  | ValueType.String -> Value.String (Var var_id)
  | ValueType.Int -> Value.Numeric (IntVar var_id)

let var_id_of_value_opt v =
  let open Instruction in
  match v with
  | Value.Unit (Var var_id)
  | Value.Bool (Var var_id)
  | Value.String (Var var_id)
  | Value.Numeric (IntVar var_id) ->
    Some var_id
  | _ -> None
