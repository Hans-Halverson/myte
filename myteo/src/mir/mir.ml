open Basic_collections

type var_id = int

type instr_id = int

module rec Type : sig
  type t =
    [ `UnitT
    | `BoolT
    | `StringT
    | `IntT
    | `ByteT
    | `LongT
    | `FunctionT
    | `PointerT of t
    | `AggregateT of Aggregate.t
    ]
end =
  Type

and Aggregate : sig
  type t = {
    name: string;
    loc: Loc.t;
    elements: Type.t list;
  }
end =
  Aggregate

and Value : sig
  type 'a unit_value =
    [ `UnitV of 'a
    | `UnitL
    ]

  type 'a bool_value =
    [ `BoolV of 'a
    | `BoolL of bool
    ]

  type 'a string_value =
    [ `StringV of 'a
    | `StringL of string
    ]

  type 'a function_value =
    [ `FunctionV of 'a
    | `FunctionL of string
    ]

  type 'a pointer_value =
    [ `PointerV of Type.t * 'a
    | `PointerL of Type.t * Int64.t
    ]

  type 'a numeric_value =
    [ `IntV of 'a
    | `IntL of Int32.t
    | `ByteV of 'a
    | `ByteL of int
    | `LongV of 'a
    | `LongL of Int64.t
    ]

  type 'a aggregate_value = [ `AggregateV of Aggregate.t * 'a ]

  type 'a t =
    [ 'a unit_value
    | 'a bool_value
    | 'a string_value
    | 'a numeric_value
    | 'a function_value
    | 'a pointer_value
    | 'a aggregate_value
    ]
end =
  Value

and Instruction : sig
  type 'var t = instr_id * 'var t'

  and 'var t' =
    | Mov of 'var * 'var Value.t
    | Call of 'var * (* Return type *) Type.t * 'var Value.function_value * 'var Value.t list
    | Ret of 'var Value.t option
    (* Globals *)
    | LoadGlobal of 'var * string
    | StoreGlobal of string * 'var Value.t
    (* Logical ops *)
    | LogNot of 'var * 'var Value.bool_value
    | LogAnd of 'var * 'var Value.bool_value * 'var Value.bool_value
    | LogOr of 'var * 'var Value.bool_value * 'var Value.bool_value
    (* Bitwise ops *)
    | BitNot of 'var * 'var Value.numeric_value
    | BitAnd of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | BitOr of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | BitXor of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | Shl of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    (* Arithmetic right shift *)
    | Shr of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    (* Logical right shift *)
    | Shrl of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    (* Unary numeric ops *)
    | Neg of 'var * 'var Value.numeric_value
    (* Binary numeric ops *)
    | Add of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | Sub of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | Mul of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | Div of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | Rem of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    (* Comparisons *)
    | Eq of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | Neq of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | Lt of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | LtEq of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | Gt of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | GtEq of 'var * 'var Value.numeric_value * 'var Value.numeric_value
end =
  Instruction

and Program : sig
  type 'var t = {
    mutable main_id: Block.id;
    mutable blocks: 'var Block.t IMap.t;
    mutable globals: 'var Global.t SMap.t;
    mutable funcs: Function.t SMap.t;
    mutable types: Aggregate.t SMap.t;
    mutable modules: Module.t SMap.t;
  }
end =
  Program

and Module : sig
  type t = {
    name: string;
    mutable globals: SSet.t;
    mutable funcs: SSet.t;
    mutable types: SSet.t;
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

  and 'var phi = Type.t * 'var * 'var IMap.t

  and 'var next =
    | Halt
    | Continue of id
    | Branch of {
        test: 'var Value.bool_value;
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
    ty: Type.t;
    mutable init_start_block: Block.id;
    init_val: 'var Value.t;
  }
end =
  Global

and Function : sig
  type t = {
    name: string;
    loc: Loc.t;
    params: (Loc.t * var_id * Type.t) list;
    return_ty: Type.t;
    mutable body_start_block: Block.id;
  }
end =
  Function

type cf_var =
  | Id of var_id
  | Local of Loc.t

type ssa_value = var_id Value.t

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

let type_of_value (v : 'a Value.t) : Type.t =
  match v with
  | `UnitV _
  | `UnitL ->
    `UnitT
  | `BoolV _
  | `BoolL _ ->
    `BoolT
  | `StringV _
  | `StringL _ ->
    `StringT
  | `ByteV _
  | `ByteL _ ->
    `ByteT
  | `IntV _
  | `IntL _ ->
    `IntT
  | `LongV _
  | `LongL _ ->
    `LongT
  | `FunctionV _
  | `FunctionL _ ->
    `FunctionT
  | `PointerV (ty, _)
  | `PointerL (ty, _) ->
    `PointerT ty
  | `AggregateV (agg, _) -> `AggregateT agg

let var_value_of_type var_id (ty : Type.t) : 'a Value.t =
  match ty with
  | `UnitT -> `UnitV var_id
  | `BoolT -> `BoolV var_id
  | `StringT -> `StringV var_id
  | `ByteT -> `ByteV var_id
  | `IntT -> `IntV var_id
  | `LongT -> `LongV var_id
  | `FunctionT -> `FunctionV var_id
  | `PointerT ty -> `PointerV (ty, var_id)
  | `AggregateT agg -> `AggregateV (agg, var_id)

let mk_continue continue = Block.Continue continue

let mk_branch test continue jump = Block.Branch { test; continue; jump }

let rec map_value ~(f : 'a -> 'b) (value : 'a Value.t) : 'b Value.t =
  match value with
  | (`UnitL | `StringL _ | `PointerL _) as lit -> lit
  | `UnitV v -> `UnitV (f v)
  | `StringV v -> `StringV (f v)
  | (`BoolL _ | `BoolV _) as v -> (map_bool_value ~f v :> 'b Value.t)
  | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _ | `LongV _) as v ->
    (map_numeric_value ~f v :> 'b Value.t)
  | (`FunctionL _ | `FunctionV _) as v -> (map_function_value ~f v :> 'b Value.t)
  | `PointerV (ty, v) -> `PointerV (ty, f v)
  | `AggregateV (agg, v) -> `AggregateV (agg, f v)

and map_bool_value ~(f : 'a -> 'b) (value : 'a Value.bool_value) : 'b Value.bool_value =
  match value with
  | `BoolL _ as lit -> lit
  | `BoolV v -> `BoolV (f v)

and map_numeric_value ~(f : 'a -> 'b) (value : 'a Value.numeric_value) : 'b Value.numeric_value =
  match value with
  | (`ByteL _ | `IntL _ | `LongL _) as lit -> lit
  | `ByteV v -> `ByteV (f v)
  | `IntV v -> `IntV (f v)
  | `LongV v -> `LongV (f v)

and map_function_value ~(f : 'a -> 'b) (value : 'a Value.function_value) : 'b Value.function_value =
  match value with
  | `FunctionL _ as lit -> lit
  | `FunctionV v -> `FunctionV (f v)

let get_block ~ir block_id = IMap.find block_id ir.Program.blocks
