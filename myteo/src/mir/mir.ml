open Basic_collections
open Mir_type

type var_id = int

type instr_id = int

type label = string

module rec Value : sig
  type 'a unit_value =
    [ `UnitV of 'a
    | `UnitL
    ]

  type 'a bool_value =
    [ `BoolV of 'a
    | `BoolL of bool
    ]

  type 'a function_value =
    [ `FunctionV of 'a
    | `FunctionL of label
    ]

  type 'a pointer_value =
    [ `PointerV of Type.t * 'a
    | `PointerL of Type.t * label
    ]

  type 'a long_value =
    [ `LongV of 'a
    | `LongL of Int64.t
    ]

  type 'a numeric_value =
    [ `IntV of 'a
    | `IntL of Int32.t
    | `ByteV of 'a
    | `ByteL of int
    | 'a long_value
    ]

  type 'a aggregate_value = [ `AggregateV of Aggregate.t * 'a ]

  type 'a array_value =
    [ `ArrayV of Type.t * int * 'a
    | `ArrayL of Type.t * int * string
    ]

  type 'a t =
    [ 'a unit_value
    | 'a bool_value
    | 'a numeric_value
    | 'a function_value
    | 'a pointer_value
    | 'a aggregate_value
    | 'a array_value
    ]

  (* Value subsets for instructions *)
  and 'var comparable_value =
    [ 'var unit_value
    | 'var bool_value
    | 'var numeric_value
    | 'var pointer_value
    ]
end =
  Value

and Instruction : sig
  module GetPointer : sig
    type 'var offset =
      | PointerIndex of 'var Value.numeric_value
      | FieldIndex of int

    type 'var t = {
      var_id: 'var;
      return_ty: Type.t;
      pointer: 'var Value.pointer_value;
      pointer_offset: 'var Value.numeric_value option;
      offsets: 'var offset list;
    }
  end

  type 'var t = instr_id * 'var t'

  and 'var t' =
    | Mov of 'var * 'var Value.t
    | Call of ('var, 'var Value.function_value) call
    | CallBuiltin of ('var, Builtin.t) call
    | Ret of 'var Value.t option
    (* Memory operations *)
    | Load of 'var * 'var Value.pointer_value
    | Store of 'var Value.pointer_value * 'var Value.t
    (* Memory offset operations *)
    | GetPointer of 'var GetPointer.t
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
    | Eq of 'var * 'var Value.comparable_value * 'var Value.comparable_value
    | Neq of 'var * 'var Value.comparable_value * 'var Value.comparable_value
    | Lt of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | LtEq of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | Gt of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    | GtEq of 'var * 'var Value.numeric_value * 'var Value.numeric_value
    (* Conversions *)
    | Trunc of 'var * 'var Value.numeric_value * Type.numeric_type
    | SExt of 'var * 'var Value.numeric_value * Type.numeric_type

  and ('var, 'a) call =
    (* Return var *)
    'var * (* Return type *) Type.t * (* Function *) 'a * (* Arguments *) 'var Value.t list
end =
  Instruction

and Program : sig
  type 'var t = {
    mutable main_id: Block.id;
    mutable blocks: 'var Block.t IMap.t;
    mutable globals: 'var Global.t SMap.t;
    mutable funcs: Function.t SMap.t;
    mutable types: Aggregate.t SMap.t;
  }
end =
  Program

and Block : sig
  type 'var t = {
    id: id;
    func: label;
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
end =
  Block

and Global : sig
  type 'var t = {
    name: label;
    loc: Loc.t;
    ty: Type.t;
    mutable init_val: 'var Value.t option;
  }
end =
  Global

and Function : sig
  type t = {
    name: label;
    loc: Loc.t;
    params: (Loc.t * var_id * Type.t) list;
    return_ty: Type.t;
    mutable body_start_block: Block.id;
  }
end =
  Function

and Builtin : sig
  type t = {
    name: string;
    mk_return_ty: Type.t list -> Type.t;
  }
end =
  Builtin

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

let type_of_value (v : 'a Value.t) : Type.t =
  match v with
  | `UnitV _
  | `UnitL ->
    `UnitT
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
  | `AggregateV (agg, _) -> `AggregateT agg
  | `ArrayV (ty, size, _)
  | `ArrayL (ty, size, _) ->
    `ArrayT (ty, size)

let pointer_value_element_type (ptr : 'a Value.pointer_value) : Type.t =
  match ptr with
  | `PointerL (ty, _)
  | `PointerV (ty, _) ->
    ty

let var_value_of_type var_id (ty : Type.t) : 'a Value.t =
  match ty with
  | `UnitT -> `UnitV var_id
  | `BoolT -> `BoolV var_id
  | `ByteT -> `ByteV var_id
  | `IntT -> `IntV var_id
  | `LongT -> `LongV var_id
  | `FunctionT -> `FunctionV var_id
  | `PointerT ty -> `PointerV (ty, var_id)
  | `AggregateT agg -> `AggregateV (agg, var_id)
  | `ArrayT (ty, size) -> `ArrayV (ty, size, var_id)

let cast_to_bool_value (v : 'a Value.t) : 'a Value.bool_value =
  match v with
  | (`BoolL _ | `BoolV _) as v -> v
  | _ -> failwith "Expected bool value"

let cast_to_numeric_value (v : 'a Value.t) : 'a Value.numeric_value =
  match v with
  | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _ | `LongV _) as v -> v
  | _ -> failwith "Expected numeric value"

let cast_to_comparable_value (v : 'a Value.t) : 'a Value.comparable_value =
  match v with
  | ( `UnitL | `UnitV _ | `BoolL _ | `BoolV _ | `ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _
    | `LongV _ | `PointerL _ | `PointerV _ ) as v ->
    v
  | _ -> failwith "Expected comprable value"

(* Whether this value is a statically known constant *)
let is_static_constant (v : 'a Value.t) : bool =
  match v with
  (* All variables are not statically known *)
  | `UnitV _
  | `BoolV _
  | `ByteV _
  | `IntV _
  | `LongV _
  | `FunctionV _
  | `PointerV _
  | `AggregateV _
  | `ArrayV _
  (* Function and pointer literals refer to labels. However for position independent code, the
     address of labels cannot be known at compile time, all we know is an offset. *)
  | `FunctionL _
  | `PointerL _ ->
    false
  | `UnitL
  | `BoolL _
  | `ByteL _
  | `IntL _
  | `LongL _
  | `ArrayL _ ->
    true

let mk_continue continue = Block.Continue continue

let mk_branch test continue jump = Block.Branch { test; continue; jump }

let rec map_value ~(f : 'a -> 'b) (value : 'a Value.t) : 'b Value.t =
  match value with
  | (`UnitL | `UnitV _) as v -> (map_unit_value ~f v :> 'b Value.t)
  | (`BoolL _ | `BoolV _) as v -> (map_bool_value ~f v :> 'b Value.t)
  | (`LongL _ | `LongV _) as v -> (map_long_value ~f v :> 'b Value.t)
  | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _) as v -> (map_numeric_value ~f v :> 'b Value.t)
  | (`FunctionL _ | `FunctionV _) as v -> (map_function_value ~f v :> 'b Value.t)
  | (`PointerL _ | `PointerV _) as v -> (map_pointer_value ~f v :> 'b Value.t)
  | `AggregateV (agg, v) -> `AggregateV (agg, f v)
  | (`ArrayL _ | `ArrayV _) as v -> (map_array_value ~f v :> 'b Value.t)

and map_unit_value ~(f : 'a -> 'b) (value : 'a Value.unit_value) : 'b Value.unit_value =
  match value with
  | `UnitL as lit -> lit
  | `UnitV v -> `UnitV (f v)

and map_bool_value ~(f : 'a -> 'b) (value : 'a Value.bool_value) : 'b Value.bool_value =
  match value with
  | `BoolL _ as lit -> lit
  | `BoolV v -> `BoolV (f v)

and map_long_value ~(f : 'a -> 'b) (value : 'a Value.long_value) : 'b Value.long_value =
  match value with
  | `LongL _ as lit -> lit
  | `LongV v -> `LongV (f v)

and map_numeric_value ~(f : 'a -> 'b) (value : 'a Value.numeric_value) : 'b Value.numeric_value =
  match value with
  | (`ByteL _ | `IntL _) as lit -> lit
  | `ByteV v -> `ByteV (f v)
  | `IntV v -> `IntV (f v)
  | (`LongL _ | `LongV _) as v -> (map_long_value ~f v :> 'b Value.numeric_value)

and map_function_value ~(f : 'a -> 'b) (value : 'a Value.function_value) : 'b Value.function_value =
  match value with
  | `FunctionL _ as lit -> lit
  | `FunctionV v -> `FunctionV (f v)

and map_pointer_value ~(f : 'a -> 'b) (value : 'a Value.pointer_value) : 'b Value.pointer_value =
  match value with
  | `PointerL _ as lit -> lit
  | `PointerV (ty, v) -> `PointerV (ty, f v)

and map_array_value ~(f : 'a -> 'b) (value : 'a Value.array_value) : 'b Value.array_value =
  match value with
  | `ArrayL _ as lit -> lit
  | `ArrayV (ty, size, v) -> `ArrayV (ty, size, f v)

and map_comparable_value ~(f : 'a -> 'b) (value : 'a Value.comparable_value) :
    'b Value.comparable_value =
  match value with
  | (`UnitL | `UnitV _) as v -> (map_unit_value ~f v :> 'b Value.comparable_value)
  | (`BoolL _ | `BoolV _) as v -> (map_bool_value ~f v :> 'b Value.comparable_value)
  | (`LongL _ | `LongV _) as v -> (map_long_value ~f v :> 'b Value.comparable_value)
  | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _) as v ->
    (map_numeric_value ~f v :> 'b Value.comparable_value)
  | (`PointerL _ | `PointerV _) as v -> (map_pointer_value ~f v :> 'b Value.comparable_value)

let get_block ~ir block_id = IMap.find block_id ir.Program.blocks

let int64_of_literal lit =
  match lit with
  | `ByteL lit -> Int64.of_int lit
  | `IntL lit -> Int64.of_int32 lit
  | `LongL lit -> lit

(* Look up an element by name in an aggregate type, throwing if no element with that name is found.
   Return a tuple of the element's type and its index in the aggregate. *)
let lookup_element agg name =
  let open Aggregate in
  let rec inner elements i =
    match elements with
    | [] -> failwith "Field not defined for aggregate"
    | (Some element_name, element_ty) :: _ when element_name = name -> (element_ty, i)
    | _ :: tl -> inner tl (i + 1)
  in
  inner agg.elements 0

let std_lib_string_prefix = ".stdS"

let has_std_lib_string_prefix name =
  String.length name >= 5 && String.sub name 0 5 = std_lib_string_prefix

let filter_stdlib (program : ssa_program) =
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
