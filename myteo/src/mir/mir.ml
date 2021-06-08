open Basic_collections

type var_id = int

type instr_id = int

module ValueType = struct
  type t =
    | Unit
    | Byte
    | Int
    | Long
    | String
    | Bool
    | Function
end

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
      | ByteLit of int
      | ByteVar of 'a
      | IntLit of Int32.t
      | IntVar of 'a
      | LongLit of Int64.t
      | LongVar of 'a
  end

  module FunctionValue : sig
    type 'a t =
      | Lit of string
      | Var of 'a
  end

  module Value : sig
    type 'a t =
      | Unit of 'a UnitValue.t
      | Numeric of 'a NumericValue.t
      | String of 'a StringValue.t
      | Bool of 'a BoolValue.t
      | Function of 'a FunctionValue.t
  end

  type 'var t = instr_id * 'var t'

  and 'var t' =
    | Mov of 'var * 'var Value.t
    | Call of 'var * (* Return type *) ValueType.t * 'var FunctionValue.t * 'var Value.t list
    | Ret of 'var Value.t option
    (* Globals *)
    | LoadGlobal of 'var * string
    | StoreGlobal of string * 'var Value.t
    (* Logical ops *)
    | LogNot of 'var * 'var BoolValue.t
    | LogAnd of 'var * 'var BoolValue.t * 'var BoolValue.t
    | LogOr of 'var * 'var BoolValue.t * 'var BoolValue.t
    (* Bitwise ops *)
    | BitNot of 'var * 'var NumericValue.t
    | BitAnd of 'var * 'var NumericValue.t * 'var NumericValue.t
    | BitOr of 'var * 'var NumericValue.t * 'var NumericValue.t
    | BitXor of 'var * 'var NumericValue.t * 'var NumericValue.t
    | Shl of 'var * 'var NumericValue.t * 'var NumericValue.t
    (* Arithmetic right shift *)
    | Shr of 'var * 'var NumericValue.t * 'var NumericValue.t
    (* Logical right shift *)
    | Shrl of 'var * 'var NumericValue.t * 'var NumericValue.t
    (* Unary numeric ops *)
    | Neg of 'var * 'var NumericValue.t
    (* Binary numeric ops *)
    | Add of 'var * 'var NumericValue.t * 'var NumericValue.t
    | Sub of 'var * 'var NumericValue.t * 'var NumericValue.t
    | Mul of 'var * 'var NumericValue.t * 'var NumericValue.t
    | Div of 'var * 'var NumericValue.t * 'var NumericValue.t
    | Rem of 'var * 'var NumericValue.t * 'var NumericValue.t
    (* Comparisons *)
    | Eq of 'var * 'var NumericValue.t * 'var NumericValue.t
    | Neq of 'var * 'var NumericValue.t * 'var NumericValue.t
    | Lt of 'var * 'var NumericValue.t * 'var NumericValue.t
    | LtEq of 'var * 'var NumericValue.t * 'var NumericValue.t
    | Gt of 'var * 'var NumericValue.t * 'var NumericValue.t
    | GtEq of 'var * 'var NumericValue.t * 'var NumericValue.t
end =
  Instruction

module rec Program : sig
  type 'var t = {
    mutable main_id: Block.id;
    mutable blocks: 'var Block.t IMap.t;
    mutable globals: 'var Global.t SMap.t;
    mutable funcs: Function.t SMap.t;
    mutable modules: Module.t SMap.t;
  }
end =
  Program

and Module : sig
  type t = {
    name: string;
    mutable globals: SSet.t;
    mutable funcs: SSet.t;
  }
end =
  Module

and Block : sig
  type 'var t = {
    id: id;
    source: source;
    mutable phis: 'var phi list;
    mutable instructions: 'var Instruction.t list;
    mutable next: 'var next;
  }

  and id = int

  and 'var phi = ValueType.t * 'var * 'var IMap.t

  and 'var next =
    | Halt
    | Continue of id
    | Branch of {
        test: 'var Instruction.BoolValue.t;
        continue: id;
        jump: id;
      }

  and source =
    | FunctionBody of string
    | GlobalInit of string
end =
  Block

and Global : sig
  type 'var t = {
    name: string;
    loc: Loc.t;
    ty: ValueType.t;
    mutable init_start_block: Block.id;
    init_val: 'var Instruction.Value.t;
  }
end =
  Global

and Function : sig
  type t = {
    name: string;
    loc: Loc.t;
    params: (Loc.t * var_id * ValueType.t) list;
    return_ty: ValueType.t;
    mutable body_start_block: Block.id;
  }
end =
  Function

type cf_var =
  | Id of var_id
  | Local of Loc.t

type cf_instruction = cf_var Instruction.t

type ssa_instruction = var_id Instruction.t

type cf_block = cf_var Block.t

type ssa_block = var_id Block.t

type cf_program = cf_var Program.t

type ssa_program = var_id Program.t

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

let max_instr_id = ref 0

let mk_instr_id () =
  let instr_id = !max_instr_id in
  max_instr_id := instr_id + 1;
  instr_id

let type_of_value v =
  let open Instruction in
  match v with
  | Value.Unit _ -> ValueType.Unit
  | Value.Bool _ -> ValueType.Bool
  | Value.String _ -> ValueType.String
  | Value.Numeric (ByteLit _ | ByteVar _) -> ValueType.Byte
  | Value.Numeric (IntLit _ | IntVar _) -> ValueType.Int
  | Value.Numeric (LongLit _ | LongVar _) -> ValueType.Long
  | Value.Function _ -> ValueType.Function

let var_value_of_type var_id ty =
  let open Instruction in
  match ty with
  | ValueType.Unit -> Value.Unit (Var var_id)
  | ValueType.Bool -> Value.Bool (Var var_id)
  | ValueType.String -> Value.String (Var var_id)
  | ValueType.Byte -> Value.Numeric (ByteVar var_id)
  | ValueType.Int -> Value.Numeric (IntVar var_id)
  | ValueType.Long -> Value.Numeric (LongVar var_id)
  | ValueType.Function -> Value.Function (Var var_id)

let mk_continue continue = Block.Continue continue

let mk_branch test continue jump = Block.Branch { test; continue; jump }

let rec map_value ~f value =
  let open Instruction.Value in
  match value with
  | Unit v -> Unit (map_unit_value ~f v)
  | String v -> String (map_string_value ~f v)
  | Bool v -> Bool (map_bool_value ~f v)
  | Numeric v -> Numeric (map_numeric_value ~f v)
  | Function v -> Function (map_function_value ~f v)

and map_unit_value ~f value =
  let open Instruction.UnitValue in
  match value with
  | Lit -> Lit
  | Var var -> Var (f var)

and map_string_value ~f value =
  let open Instruction.StringValue in
  match value with
  | Lit lit -> Lit lit
  | Var var -> Var (f var)

and map_bool_value ~f value =
  let open Instruction.BoolValue in
  match value with
  | Lit lit -> Lit lit
  | Var var -> Var (f var)

and map_numeric_value ~f value =
  let open Instruction.NumericValue in
  match value with
  | ByteLit lit -> ByteLit lit
  | ByteVar var -> ByteVar (f var)
  | IntLit lit -> IntLit lit
  | IntVar var -> IntVar (f var)
  | LongLit lit -> LongLit lit
  | LongVar var -> LongVar (f var)

and map_function_value ~f value =
  let open Instruction.FunctionValue in
  match value with
  | Lit lit -> Lit lit
  | Var var -> Var (f var)

let get_block ~ir block_id = IMap.find block_id ir.Program.blocks
