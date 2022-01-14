open Basic_collections
open Mir_type

type label = string

module rec Value : sig
  type id = int

  type t =
    | Instr of Instruction.t
    | Lit of Literal.t
    | Argument of Argument.t

  val compare : Value.t -> Value.t -> int
end = struct
  type id = int

  type t =
    | Instr of Instruction.t
    | Lit of Literal.t
    | Argument of Argument.t

  let compare v1 v2 =
    match (v1, v2) with
    | ( (Instr { id = id1; _ } | Argument { id = id1; _ }),
        (Instr { id = id2; _ } | Argument { id = id2; _ }) ) ->
      Int.compare id1 id2
    | (Lit _, Lit _) -> 0
    | (Lit _, _) -> -1
    | (_, Lit _) -> 1
end

and Literal : sig
  type t =
    | Bool of bool
    | Byte of int
    | Int of Int32.t
    | Long of Int64.t
    | Pointer of Type.t * label
    | NullPointer of Type.t
    | Function of label
    | ArrayString of string
    | ArrayVtable of int * label list
end =
  Literal

and Argument : sig
  type t = {
    id: Value.id;
    func: label;
    type_: Type.t;
    (* Location of argument declaration identifier *)
    decl_loc: Loc.t;
  }
end =
  Argument

and Instruction : sig
  (* Represents choosing a value depending on which basic block was previously visited *)
  module Phi : sig
    type t = {
      (* Map from preceding basic block id to the value to choose if that basic block was visited *)
      mutable args: Value.t IMap.t;
    }
  end

  module GetPointer : sig
    type offset =
      | PointerIndex of (* Numeric *) Value.t
      | FieldIndex of int

    (* Instruction type is result of GetPointer instruction, a pointer to indexed element type *)
    type t = {
      pointer: (* Pointer *) Value.t;
      pointer_offset: (* Numeric *) Value.t option;
      offsets: offset list;
    }
  end

  module Call : sig
    type t = {
      func: func;
      args: Value.t list;
      (* If true then instruction's type_ is call's return type. If false then instruction's
         type_ is undefined *)
      has_return: bool;
    }

    and func =
      | Value of (* Function *) Value.t
      | Builtin of Builtin.t
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

  type func =
    | FuncBuiltin of Builtin.t
    | FuncValue of (* Function *) Value.t

  and instr =
    | Call of Call.t
    | Ret of Value.t option
    (* Memory operations *)
    | StackAlloc of (* Type of allocated value *) Type.t (* Instruction type is pointer type *)
    | Load of (* Pointer *) Value.t
    | Store of (* Pointer *) Value.t * (* Stored value, any type *) Value.t
    (* Memory offset operations *)
    | GetPointer of GetPointer.t
    (* Unary operations *)
    | Unary of unary_operation * (* Numeric *) Value.t
    (* Binary operations *)
    | Binary of binary_operation * (* Numeric *) Value.t * (* Numeric *) Value.t
    (* Comparison *)
    | Cmp of comparison * (* Comparable *) Value.t * (* Comparable *) Value.t
    (* Conversions *)
    | Cast of Value.t (* Instruction type is type value is cast to *)
    | Trunc of (* Numeric *) Value.t (* Instruction type is type value is truncated to *)
    | SExt of (* Numeric *) Value.t (* Instruction type is type value is sign extended to *)
    (* Only generated during MIR destruction *)
    | Mov of Value.t
    | Phi of Phi.t

  and t = {
    id: Value.id;
    mutable type_: Type.t;
    mutable instr: instr;
  }
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
  type id = int

  type t = {
    id: id;
    func: label;
    mutable instructions: Instruction.t list;
    mutable next: next;
  }

  and next =
    | Halt
    | Continue of id
    | Branch of {
        test: (* Bool *) Value.t;
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
    params: Argument.t list;
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

let max_value_id : Value.id ref = ref 0

let mk_value_id () : Value.id =
  let value_id = !max_value_id in
  max_value_id := value_id + 1;
  value_id

let rec type_of_value (v : Value.t) : Type.t =
  match v with
  | Instr { type_; _ } -> type_
  | Argument { type_; _ } -> type_
  | Lit lit -> type_of_literal lit

and type_of_literal (lit : Literal.t) : Type.t =
  match lit with
  | Literal.Bool _ -> Bool
  | Byte _ -> Byte
  | Int _ -> Int
  | Long _ -> Long
  | Function _ -> Function
  | Pointer (ty, _) -> Pointer ty
  | NullPointer ty -> Pointer ty
  | ArrayString str -> Array (Byte, String.length str)
  | ArrayVtable (size, _) -> Array (Function, size)

let pointer_value_element_type (ptr : Value.t) : Type.t =
  match type_of_value ptr with
  | Pointer element_type -> element_type
  | _ -> failwith "Expected pointer type"

let cast_to_function_literal (func : Value.t) : label =
  match func with
  | Lit (Function label) -> label
  | _ -> failwith "Expected function literal"

let is_literal (v : Value.t) : bool =
  match v with
  | Lit _ -> true
  | _ -> false

let is_bool_value (v : Value.t) : bool = type_of_value v = Bool

let is_function_value (v : Value.t) : bool = type_of_value v = Function

let is_numeric_value (v : Value.t) : bool = is_numeric_type (type_of_value v)

let is_pointer_value (v : Value.t) : bool =
  match type_of_value v with
  | Pointer _ -> true
  | _ -> false

let is_comparable_value (v : Value.t) : bool =
  match type_of_value v with
  | Bool
  | Byte
  | Int
  | Long
  | Pointer _ ->
    true
  | _ -> false

let rec values_equal (v1 : Value.t) (v2 : Value.t) : bool =
  match (v1, v2) with
  | (Instr { id = id1; _ }, Instr { id = id2; _ }) -> id1 = id2
  | (Lit lit1, Lit lit2) -> literals_equal lit1 lit2
  | (Argument { id = id1; _ }, Argument { id = id2; _ }) -> id1 = id2
  | _ -> false

and literals_equal (lit1 : Literal.t) (lit2 : Literal.t) : bool =
  match (lit1, lit2) with
  | (Bool b1, Bool b2) -> b1 = b2
  | (Byte b1, Byte b2) -> b1 = b2
  | (Int i1, Int i2) -> Int32.equal i1 i2
  | (Long l1, Long l2) -> Int64.equal l1 l2
  | (Function label1, Function label2) -> label1 = label2
  | (Pointer (_, label1), Pointer (_, label2)) -> label1 = label2
  | (ArrayString str1, ArrayString str2) -> String.equal str1 str2
  | (ArrayVtable (size1, labels1), ArrayVtable (size2, labels2)) ->
    size1 = size2 && List.for_all2 String.equal labels1 labels2
  | _ -> false

let values_have_same_type (v1 : Value.t) (v2 : Value.t) : bool =
  types_equal (type_of_value v1) (type_of_value v2)

let is_shift_op (op : Instruction.binary_operation) : bool =
  match op with
  | Shl
  | Shr
  | Shrl ->
    true
  | _ -> false

let cast_to_instruction (value : Value.t) : Instruction.t =
  match value with
  | Instr instr -> instr
  | _ -> failwith "Expected instruction value"

let cast_to_phi (instr : Instruction.t) : Instruction.Phi.t =
  match instr with
  | { instr = Instruction.Phi phi; _ } -> phi
  | _ -> failwith "Expected phi instruction"

let mk_continue continue = Block.Continue continue

let mk_branch test continue jump = Block.Branch { test; continue; jump }

let get_block ~ir block_id = IMap.find block_id ir.Program.blocks

let int64_of_literal lit =
  match lit with
  | Literal.Byte lit -> Int64.of_int lit
  | Int lit -> Int64.of_int32 lit
  | Long lit -> lit
  | _ -> failwith "Expected byte, int, or long"

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

let block_has_phis (block : Block.t) : bool =
  match block.instructions with
  | { instr = Phi _; _ } :: _ -> true
  | _ -> false

let block_get_phis (block : Block.t) : Instruction.Phi.t list =
  let rec inner instrs acc =
    match instrs with
    | { Instruction.instr = Phi phi; _ } :: rest -> inner rest (phi :: acc)
    | _ -> List.rev acc
  in
  inner block.instructions []

let block_iter_phis (block : Block.t) (f : Instruction.Phi.t -> unit) =
  let rec visit_phis instrs =
    match instrs with
    | { Instruction.instr = Phi phi; _ } :: rest ->
      f phi;
      visit_phis rest
    | _ -> ()
  in
  visit_phis block.instructions

let block_filter_phis (block : Block.t) (f : Value.id -> Instruction.Phi.t -> bool) =
  let rec filter_phis instrs acc =
    match instrs with
    | ({ Instruction.instr = Phi phi; id; _ } as instr) :: rest ->
      let acc =
        if f id phi then
          instr :: acc
        else
          acc
      in
      filter_phis rest acc
    | rest -> List.rev acc @ rest
  in
  block.instructions <- filter_phis block.instructions []

let block_fold_phis
    (block : Block.t) (f : Instruction.t -> Instruction.Phi.t -> 'a -> 'a) (acc : 'a) : 'a =
  let rec visit_phis instrs acc =
    match instrs with
    | ({ Instruction.instr = Phi phi; _ } as instr) :: rest ->
      let acc = f instr phi acc in
      visit_phis rest acc
    | _ -> acc
  in
  visit_phis block.instructions acc

let block_clear_phis (block : Block.t) = block_filter_phis block (fun _ _ -> false)

module VSet = Set.Make (Value)
module VMap = Map.Make (Value)
module VVMMap = MultiMap.Make (Value) (Value)
