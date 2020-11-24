open Basic_collections

type var_id = int

module rec Instruction : sig
  module UnitValue : sig
    type 'a t =
      | Lit
      | Var of 'a
  end

  module BoolValue : sig
    type 'a t =
      | Lit of bool
      | Var of 'a
  end

  module StringValue : sig
    type 'a t =
      | Lit of string
      | Var of 'a
  end

  module NumericValue : sig
    type 'a t =
      | IntLit of int
      | IntVar of 'a
  end

  module Value : sig
    type 'a t =
      | Unit of 'a UnitValue.t
      | Numeric of 'a NumericValue.t
      | String of 'a StringValue.t
      | Bool of 'a BoolValue.t
  end

  type 'a t =
    | Mov of var_id * 'a Value.t
    | Ret of 'a Value.t option
    | Phi of var_id * var_id list
    (* Globals *)
    | LoadGlobal of var_id * Loc.t
    | StoreGlobal of Loc.t * 'a Value.t
    (* Logical ops *)
    | LogNot of var_id * 'a BoolValue.t
    | LogAnd of var_id * 'a BoolValue.t * 'a BoolValue.t
    | LogOr of var_id * 'a BoolValue.t * 'a BoolValue.t
    (* Unary numeric ops *)
    | Neg of var_id * 'a NumericValue.t
    (* Binary numeric ops *)
    | Add of var_id * 'a NumericValue.t * 'a NumericValue.t
    | Sub of var_id * 'a NumericValue.t * 'a NumericValue.t
    | Mul of var_id * 'a NumericValue.t * 'a NumericValue.t
    | Div of var_id * 'a NumericValue.t * 'a NumericValue.t
    | Eq of var_id * 'a NumericValue.t * 'a NumericValue.t
    | Neq of var_id * 'a NumericValue.t * 'a NumericValue.t
    | Lt of var_id * 'a NumericValue.t * 'a NumericValue.t
    | LtEq of var_id * 'a NumericValue.t * 'a NumericValue.t
    | Gt of var_id * 'a NumericValue.t * 'a NumericValue.t
    | GtEq of var_id * 'a NumericValue.t * 'a NumericValue.t
end =
  Instruction

type ssa_instruction = var_id Instruction.t

type cf_instruction = cf_var Instruction.t

and cf_var =
  | Id of var_id
  | Local of Loc.t

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
    instructions: (Loc.t * ssa_instruction) list;
    next: var_id next;
  }

  and id = int

  and 'a next =
    | Halt
    | Continue of id
    | Branch of {
        test: 'a Instruction.BoolValue.t;
        continue: id;
        jump: id;
      }
end =
  Block

and Global : sig
  type t = {
    loc: Loc.t;
    name: string;
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

let mk_continue continue = Block.Continue continue

let mk_branch test continue jump = Block.Branch { test; continue; jump }
