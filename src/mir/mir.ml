open Basic_collections
open Mir_type

type var_id = int

type instr_id = int

type label = string

module rec Value : sig
  type bool_value =
    [ `BoolV of var_id
    | `BoolL of bool
    ]

  type function_value =
    [ `FunctionV of var_id
    | `FunctionL of label
    ]

  type pointer_value =
    [ `PointerV of Type.t * var_id
    | `PointerL of Type.t * label
    ]

  type long_value =
    [ `LongV of var_id
    | `LongL of Int64.t
    ]

  type numeric_value =
    [ `IntV of var_id
    | `IntL of Int32.t
    | `ByteV of var_id
    | `ByteL of int
    | long_value
    | bool_value
    ]

  type array_value =
    [ `ArrayV of Type.t * int * var_id
    | `ArrayStringL of string
    | `ArrayVtableL of int * function_value list
    ]

  type t =
    [ numeric_value
    | function_value
    | pointer_value
    | array_value
    ]

  (* Value subsets for instructions *)
  and comparable_value =
    [ numeric_value
    | pointer_value
    ]
end =
  Value

and Instruction : sig
  module GetPointer : sig
    type offset =
      | PointerIndex of Value.numeric_value
      | FieldIndex of int

    type 'var t = {
      var_id: var_id;
      return_ty: Type.t;
      pointer: Value.pointer_value;
      pointer_offset: Value.numeric_value option;
      offsets: offset list;
    }
  end

  module Call : sig
    type 'a t = {
      return: (var_id * Type.t) option;
      func: 'a;
      args: Value.t list;
    }
  end

  type comparison =
    | Eq
    | Neq
    | Lt
    | LtEq
    | Gt
    | GtEq

  type unary_operation =
    (* Numeric negate *)
    | Neg
    (* Bitwise not *)
    | Not

  type binary_operation =
    (* Numeric operations *)
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    (* Bitwise operations *)
    | And
    | Or
    | Xor
    (* Logical left shift *)
    | Shl
    (* Arithmetic right shift *)
    | Shr
    (* Logical right shift *)
    | Shrl

  type t = instr_id * t'

  and t' =
    | Call of Value.function_value Call.t
    | CallBuiltin of Builtin.t Call.t
    | Ret of Value.t option
    (* Memory operations *)
    | StackAlloc of var_id * Type.t
    | Load of var_id * Value.pointer_value
    | Store of Value.pointer_value * Value.t
    (* Memory offset operations *)
    | GetPointer of var_id GetPointer.t
    (* Unary operations *)
    | Unary of unary_operation * var_id * Value.numeric_value
    (* Binary operations *)
    | Binary of binary_operation * var_id * Value.numeric_value * Value.numeric_value
    (* Comparison *)
    | Cmp of comparison * var_id * Value.comparable_value * Value.comparable_value
    (* Conversions *)
    | Trunc of var_id * Value.numeric_value * Type.numeric_type
    | SExt of var_id * Value.numeric_value * Type.numeric_type
    (* Only generated during MIR destruction *)
    | Mov of var_id * Value.t
end =
  Instruction

and Program : sig
  type t = {
    mutable main_label: label;
    mutable blocks: Block.t IMap.t;
    mutable globals: Global.t SMap.t;
    mutable funcs: Function.t SMap.t;
    mutable types: Aggregate.t SMap.t;
  }
end =
  Program

and Block : sig
  type t = {
    id: id;
    func: label;
    mutable phis: phi list;
    mutable instructions: Instruction.t list;
    mutable next: next;
  }

  and id = int

  and phi = Type.t * var_id * Value.t IMap.t

  and next =
    | Halt
    | Continue of id
    | Branch of {
        test: Value.bool_value;
        continue: id;
        jump: id;
      }
end =
  Block

and Global : sig
  type t = {
    name: label;
    loc: Loc.t;
    ty: Type.t;
    mutable init_val: Value.t option;
    is_constant: bool;
  }
end =
  Global

and Function : sig
  type t = {
    name: label;
    loc: Loc.t;
    params: (Loc.t * var_id * Type.t) list;
    return_ty: Type.t option;
    mutable body_start_block: Block.id;
  }
end =
  Function

and Builtin : sig
  type t = {
    name: string;
    mk_return_ty: Type.t list -> Type.t option;
  }
end =
  Builtin

let init_func_name = "_init"

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

let type_of_value (v : Value.t) : Type.t =
  match v with
  | `BoolV _
  | `BoolL _ ->
    `BoolT
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
  | `ArrayV (ty, size, _) -> `ArrayT (ty, size)
  | `ArrayStringL str -> `ArrayT (`ByteT, String.length str)
  | `ArrayVtableL (size, _) -> `ArrayT (`FunctionT, size)

let pointer_value_element_type (ptr : Value.pointer_value) : Type.t =
  match ptr with
  | `PointerL (ty, _)
  | `PointerV (ty, _) ->
    ty

let var_value_of_type var_id (ty : Type.t) : Value.t =
  match ty with
  | `BoolT -> `BoolV var_id
  | `ByteT -> `ByteV var_id
  | `IntT -> `IntV var_id
  | `LongT -> `LongV var_id
  | `FunctionT -> `FunctionV var_id
  | `PointerT ty -> `PointerV (ty, var_id)
  | `ArrayT (ty, size) -> `ArrayV (ty, size, var_id)
  | `AggregateT _ -> failwith "Cannot create variable for Aggregate, must be behind pointer"

let cast_to_bool_value (v : Value.t) : Value.bool_value =
  match v with
  | (`BoolL _ | `BoolV _) as v -> v
  | _ -> failwith "Expected bool value"

let cast_to_numeric_value (v : Value.t) : Value.numeric_value =
  match v with
  | (`BoolL _ | `BoolV _ | `ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _ | `LongV _) as v -> v
  | _ -> failwith "Expected numeric value"

let cast_to_function_value (v : Value.t) : Value.function_value =
  match v with
  | (`FunctionL _ | `FunctionV _) as v -> v
  | _ -> failwith "Expected function value"

let cast_to_pointer_value (v : Value.t) : Value.pointer_value =
  match v with
  | (`PointerL _ | `PointerV _) as v -> v
  | _ -> failwith "Expected pointer value"

let cast_to_array_value (v : Value.t) : Value.array_value =
  match v with
  | (`ArrayStringL _ | `ArrayVtableL _ | `ArrayV _) as v -> v
  | _ -> failwith "Expected array value"

let cast_to_comparable_value (v : Value.t) : Value.comparable_value =
  match v with
  | ( `BoolL _ | `BoolV _ | `ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _ | `LongV _
    | `PointerL _ | `PointerV _ ) as v ->
    v
  | _ -> failwith "Expected comparable value"

let cast_pointer_value (v : Value.pointer_value) (mir_type : Type.t) : Value.pointer_value =
  match v with
  | `PointerL (_, label) -> `PointerL (mir_type, label)
  | `PointerV (_, var_id) -> `PointerV (mir_type, var_id)

let is_literal (v : Value.t) : bool =
  match v with
  | `FunctionL _
  | `PointerL _
  | `BoolL _
  | `ByteL _
  | `IntL _
  | `LongL _
  | `ArrayStringL _
  | `ArrayVtableL _ ->
    true
  | `BoolV _
  | `ByteV _
  | `IntV _
  | `LongV _
  | `FunctionV _
  | `PointerV _
  | `ArrayV _ ->
    false

let is_bool_value (v : Value.numeric_value) : bool =
  match v with
  | `BoolL _
  | `BoolV _ ->
    true
  | _ -> false

let var_id_of_value_opt (v : Value.t) : var_id option =
  match v with
  | `FunctionL _
  | `PointerL _
  | `BoolL _
  | `ByteL _
  | `IntL _
  | `LongL _
  | `ArrayStringL _
  | `ArrayVtableL _ ->
    None
  | `BoolV var_id
  | `ByteV var_id
  | `IntV var_id
  | `LongV var_id
  | `FunctionV var_id
  | `PointerV (_, var_id)
  | `ArrayV (_, _, var_id) ->
    Some var_id

let rec values_equal (v1 : Value.t) (v2 : Value.t) : bool =
  match (v1, v2) with
  | (`BoolL b1, `BoolL b2) -> b1 = b2
  | (`ByteL b1, `ByteL b2) -> b1 = b2
  | (`IntL i1, `IntL i2) -> Int32.equal i1 i2
  | (`LongL l1, `LongL l2) -> Int64.equal l1 l2
  | (`FunctionL label1, `FunctionL label2) -> label1 = label2
  | (`PointerL (_, label1), `PointerL (_, label2)) -> label1 = label2
  | (`ArrayStringL str1, `ArrayStringL str2) -> String.equal str1 str2
  | (`ArrayVtableL (size1, labels1), `ArrayVtableL (size2, labels2)) ->
    size1 = size2
    && List.for_all2 (fun f1 f2 -> values_equal (f1 :> Value.t) (f2 :> Value.t)) labels1 labels2
  | (`BoolV var_id1, `BoolV var_id2)
  | (`ByteV var_id1, `ByteV var_id2)
  | (`IntV var_id1, `IntV var_id2)
  | (`LongV var_id1, `LongV var_id2)
  | (`FunctionV var_id1, `FunctionV var_id2)
  | (`PointerV (_, var_id1), `PointerV (_, var_id2))
  | (`ArrayV (_, _, var_id1), `ArrayV (_, _, var_id2)) ->
    var_id1 = var_id2
  | _ -> false

let mk_continue continue = Block.Continue continue

let mk_branch test continue jump = Block.Branch { test; continue; jump }

let rec map_value ~f (value : Value.t) : Value.t =
  match value with
  | (`BoolL _ | `BoolV _) as v -> (map_bool_value ~f v :> Value.t)
  | (`LongL _ | `LongV _) as v -> (map_long_value ~f v :> Value.t)
  | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _) as v -> (map_numeric_value ~f v :> Value.t)
  | (`FunctionL _ | `FunctionV _) as v -> (map_function_value ~f v :> Value.t)
  | (`PointerL _ | `PointerV _) as v -> (map_pointer_value ~f v :> Value.t)
  | (`ArrayStringL _ | `ArrayVtableL _ | `ArrayV _) as v -> (map_array_value ~f v :> Value.t)

and map_bool_value ~f (value : Value.bool_value) : Value.bool_value =
  match value with
  | `BoolL _ as lit -> lit
  | `BoolV v -> `BoolV (f v)

and map_long_value ~f (value : Value.long_value) : Value.long_value =
  match value with
  | `LongL _ as lit -> lit
  | `LongV v -> `LongV (f v)

and map_numeric_value ~f (value : Value.numeric_value) : Value.numeric_value =
  match value with
  | (`ByteL _ | `IntL _) as lit -> lit
  | `ByteV v -> `ByteV (f v)
  | `IntV v -> `IntV (f v)
  | (`LongL _ | `LongV _) as v -> (map_long_value ~f v :> Value.numeric_value)
  | (`BoolL _ | `BoolV _) as v -> (map_bool_value ~f v :> Value.numeric_value)

and map_function_value ~f (value : Value.function_value) : Value.function_value =
  match value with
  | `FunctionL _ as lit -> lit
  | `FunctionV v -> `FunctionV (f v)

and map_pointer_value ~f (value : Value.pointer_value) : Value.pointer_value =
  match value with
  | `PointerL _ as lit -> lit
  | `PointerV (ty, v) -> `PointerV (ty, f v)

and map_array_value ~f (value : Value.array_value) : Value.array_value =
  match value with
  | (`ArrayStringL _ | `ArrayVtableL _) as lit -> lit
  | `ArrayV (ty, size, v) -> `ArrayV (ty, size, f v)

and map_comparable_value ~f (value : Value.comparable_value) : Value.comparable_value =
  match value with
  | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _ | `LongV _ | `BoolL _ | `BoolV _) as v ->
    (map_numeric_value ~f v :> Value.comparable_value)
  | (`PointerL _ | `PointerV _) as v -> (map_pointer_value ~f v :> Value.comparable_value)

let get_block ~ir block_id = IMap.find block_id ir.Program.blocks

let int64_of_literal lit =
  match lit with
  | `ByteL lit -> Int64.of_int lit
  | `IntL lit -> Int64.of_int32 lit
  | `LongL lit -> lit

let std_lib_string_prefix = ".stdS"

let has_std_lib_string_prefix name =
  String.length name >= 5 && String.sub name 0 5 = std_lib_string_prefix

let filter_stdlib (program : Program.t) =
  let filter_stdlib_names smap =
    SMap.filter
      (fun name _ ->
        (not (Std_lib.has_std_lib_prefix name)) && not (has_std_lib_string_prefix name))
      smap
  in
  {
    program with
    Program.globals = filter_stdlib_names program.globals;
    funcs = filter_stdlib_names program.funcs;
  }
