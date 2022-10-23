open Basic_collections
open Mir_type

type label = string

(* A use of a value, such as an operand in an instruction or the initializer for globals. Creates
   a link between values and their uses allowing one to traverse between them and iterate over all
   uses. *)
module rec Use : sig
  type t = {
    (* The value that being used at this use. For example in a Binary instruction each of the two
       operands is a use and this is its value. *)
    mutable value: Value.t;
    (* The user that is using the use. For example a Binary instruction is a user and each of its
       two operands is a use whose user points back to the Binary instruction. *)
    user: Value.t;
    (* Uses form a circular doubly linked list *)
    mutable prev: Use.t;
    mutable next: Use.t;
  }
end =
  Use

and Value : sig
  type id = int

  type value =
    | Instr of Instruction.t
    | Lit of Literal.t
    | Argument of Argument.t

  type t = {
    id: id;
    mutable value: value;
    (* All uses of this value, forming a circular linked list. None if there are no uses. *)
    mutable uses: Use.t option;
  }

  val compare : Value.t -> Value.t -> int
end = struct
  type id = int

  type value =
    | Instr of Instruction.t
    | Lit of Literal.t
    | Argument of Argument.t

  type t = {
    id: id;
    mutable value: value;
    mutable uses: Use.t option;
  }

  let compare v1 v2 =
    match (v1.value, v2.value) with
    | ((Instr _ | Argument _), (Instr _ | Argument _)) -> Int.compare v1.id v2.id
    | (Lit _, Lit _) -> 0
    | (Lit _, _) -> -1
    | (_, Lit _) -> 1
end

and Literal : sig
  type t =
    | Bool of bool
    | Byte of Int8.t
    | Int of Int32.t
    | Long of Int64.t
    | Double of Float.t
    | Global of Global.t
    | NullPointer of Type.t
    | Function of Function.t
    | ArrayString of string
    | ArrayVtable of int * (* Function literals *) Use.t list
    (* A closure aggregate for a known function, where the environment is the null pointer. Also
       contains closure type. *)
    | AggregateClosure of Type.t * (* Function literal *) Use.t
end =
  Literal

and Argument : sig
  type t = {
    func: Function.t;
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
      (* Map from preceding basic block to the value to choose if that basic block was visited *)
      mutable args: Use.t BlockMap.t;
    }
  end

  module GetPointer : sig
    type 'a offset =
      | PointerIndex of (* Integer *) 'a
      | FieldIndex of int

    type value_offset = Value.t offset

    type use_offset = Use.t offset

    (* Instruction type is result of GetPointer instruction, a pointer to indexed element type *)
    type t = {
      pointer: (* Pointer *) Use.t;
      pointer_offset: (* Integer *) Use.t option;
      offsets: use_offset list;
    }
  end

  module Call : sig
    type t = {
      func: func;
      args: Use.t list;
      (* If true then instruction's type_ is call's return type. If false then instruction's
         type_ is undefined *)
      has_return: bool;
    }

    and func =
      | Value of (* Function *) Use.t
      | MirBuiltin of Builtin.t
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
    (* Integer operation *)
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

  and instr =
    | Phi of Phi.t
    | Call of Call.t
    (* Memory operations *)
    | StackAlloc of (* Type of allocated value *) Type.t (* Instruction type is pointer type *)
    | Load of (* Pointer *) Use.t
    | Store of (* Pointer *) Use.t * (* Stored value, any type *) Use.t
    (* Memory offset operations *)
    | GetPointer of GetPointer.t
    (* Unary operations *)
    | Unary of unary_operation * (* Numeric *) Use.t
    (* Binary operations *)
    | Binary of binary_operation * (* Numeric *) Use.t * (* Numeric *) Use.t
    (* Comparison *)
    | Cmp of comparison * (* Comparable *) Use.t * (* Comparable *) Use.t
    (* Conversions *)
    | Cast of Use.t (* Instruction type is type value is cast to *)
    | Trunc of (* Integer *) Use.t (* Instruction type is type value is truncated to *)
    | SExt of (* Integer *) Use.t (* Instruction type is type value is sign extended to *)
    | ZExt of (* Integer *) Use.t (* Instruction type is type value is zero extended to *)
    | IntToFloat of Use.t (* Instruction type is type value is converted to *)
    | FloatToInt of Use.t (* Instruction type is type value is converted to *)
    (* Block terminators *)
    | Ret of Use.t option
    | Continue of Block.t
    | Branch of {
        test: (* Bool *) Use.t;
        continue: Block.t;
        jump: Block.t;
      }
    | Unreachable
    (* Only generated during MIR destruction *)
    | Mov of Use.t

  and t = {
    mutable type_: Type.t;
    mutable instr: instr;
    mutable block: Block.t;
    (* Circular, doubly linked list of instructions in block *)
    mutable prev: Value.t;
    mutable next: Value.t;
  }
end =
  Instruction

and Program : sig
  type t = {
    mutable globals: Global.t SMap.t;
    mutable funcs: Function.t SMap.t;
    mutable types: Aggregate.t SMap.t;
    mutable main_func: Function.t;
  }
end =
  Program

and Block : sig
  type id

  type t = {
    id: id;
    func: Function.t;
    mutable instructions: instructions option;
    mutable prev_blocks: BlockSet.t;
  }

  (* Circular doubly linked list of all instructions in block *)
  and instructions = {
    mutable first: Value.t;
    mutable last: Value.t;
  }

  val compare : t -> t -> int

  val mk_id : unit -> id

  val id_to_string : Block.id -> string
end = struct
  type id = int

  type t = {
    id: id;
    func: Function.t;
    mutable instructions: instructions option;
    mutable prev_blocks: BlockSet.t;
  }

  and instructions = {
    mutable first: Value.t;
    mutable last: Value.t;
  }

  let compare b1 b2 = Int.compare b1.id b2.id

  let max_block_id = ref 0

  let mk_id () =
    let block_id = !max_block_id in
    max_block_id := block_id + 1;
    block_id

  let id_to_string id = string_of_int id
end

and Global : sig
  type t = {
    name: label;
    loc: Loc.t;
    type_: Type.t;
    mutable init_val: Use.t option;
    is_constant: bool;
    (* The value that represents this global *)
    mutable value: Value.t;
  }
end =
  Global

and Function : sig
  type t = {
    name: label;
    mutable loc: Loc.t;
    mutable params: Value.t list;
    mutable return_type: Type.t option;
    mutable start_block: Block.t;
    mutable blocks: BlockSet.t;
    (* The value that represents this function *)
    mutable value: Value.t;
  }

  val compare : t -> t -> int

  val equal : t -> t -> bool
end = struct
  type t = {
    name: label;
    mutable loc: Loc.t;
    mutable params: Value.t list;
    mutable return_type: Type.t option;
    mutable start_block: Block.t;
    mutable blocks: BlockSet.t;
    mutable value: Value.t;
  }

  let compare f1 f2 = String.compare f1.name f2.name

  let equal f1 f2 = compare f1 f2 == 0
end

and Builtin : sig
  type t = {
    name: string;
    mk_return_ty: Type.t list -> Type.t option;
  }
end =
  Builtin

and ValueCollection : (MultiMap.KEY_AND_VALUE_TYPE with type t = Value.t) = MakeCollection (Value)
and BlockCollection : MultiMap.KEY_AND_VALUE_TYPE = MakeCollection (Block)
and FunctionCollection : MultiMap.KEY_AND_VALUE_TYPE = MakeCollection (Function)
and VSet : (Set.S with type elt = Value.t) = ValueCollection.Set
and VMap : (Map.S with type key = Value.t) = ValueCollection.Map
and BlockSet : (Set.S with type elt = Block.t) = BlockCollection.Set
and BlockMap : (Map.S with type key = Block.t) = BlockCollection.Map
and FunctionSet : (Set.S with type elt = Function.t) = FunctionCollection.Set
and FunctionMap : (Map.S with type key = Function.t) = FunctionCollection.Map

module VVMMap = MultiMap.Make (VMap) (VSet)

module BlockMMap = MultiMap.Make (BlockMap) (BlockSet)

module BlockIMMap = MultiMap.Make (BlockMap) (ISet)

let init_func_name = "_init"

let max_value_id : Value.id ref = ref 0

let mk_value_id () : Value.id =
  let value_id = !max_value_id in
  max_value_id := value_id + 1;
  value_id

(* A placeholder function *)
let rec null_function : Function.t =
  {
    Function.name = "Null function";
    loc = Loc.none;
    params = [];
    return_type = None;
    start_block = null_block;
    blocks = BlockSet.empty;
    value = null_value;
  }

(* A placeholder block, to avoid having to represent Option<Block> with its extra allocation *)
and null_block : Block.t =
  {
    Block.id = Block.mk_id ();
    func = null_function;
    instructions = None;
    prev_blocks = BlockSet.empty;
  }

and null_value_value : Value.value = Lit (Bool true)

and null_value : Value.t = { Value.id = 0; value = null_value_value; uses = None }

let rec type_of_use (use : Use.t) : Type.t = type_of_value use.value

and type_of_value (v : Value.t) : Type.t =
  match v.value with
  | Instr { type_; _ } -> type_
  | Argument { type_; _ } -> type_
  | Lit lit -> type_of_literal lit

and type_of_literal (lit : Literal.t) : Type.t =
  match lit with
  | Literal.Bool _ -> Bool
  | Byte _ -> Byte
  | Int _ -> Int
  | Long _ -> Long
  | Double _ -> Double
  | Function _ -> Function
  | Global global -> Pointer global.type_
  | NullPointer ty -> Pointer ty
  | ArrayString str -> Array (Byte, String.length str)
  | ArrayVtable (size, _) -> Array (Function, size)
  | AggregateClosure (type_, _) -> type_

let pointer_value_element_type (ptr : Value.t) : Type.t =
  match type_of_value ptr with
  | Pointer element_type -> element_type
  | _ -> failwith "Expected pointer type"

let cast_to_function_literal (func : Value.t) : Function.t =
  match func.value with
  | Lit (Function func) -> func
  | _ -> failwith "Expected function literal"

let is_literal (v : Value.t) : bool =
  match v.value with
  | Lit _ -> true
  | _ -> false

let is_bool_value (v : Value.t) : bool = type_of_value v = Bool

let is_function_value (v : Value.t) : bool = type_of_value v = Function

let is_integer_value (v : Value.t) : bool = is_integer_type (type_of_value v)

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
  | Double
  | Pointer _ ->
    true
  | _ -> false

let rec values_equal (v1 : Value.t) (v2 : Value.t) : bool =
  match (v1.value, v2.value) with
  | (Instr _, Instr _) -> v1.id == v2.id
  | (Lit lit1, Lit lit2) -> literals_equal lit1 lit2
  | (Argument _, Argument _) -> v1.id == v2.id
  | _ -> false

and literals_equal (lit1 : Literal.t) (lit2 : Literal.t) : bool =
  match (lit1, lit2) with
  | (Bool b1, Bool b2) -> b1 = b2
  | (Byte b1, Byte b2) -> b1 = b2
  | (Int i1, Int i2) -> Int32.equal i1 i2
  | (Long l1, Long l2) -> Int64.equal l1 l2
  | (Double f1, Double f2) -> Float.equal f1 f2
  | (Function func1, Function func2) -> func1 == func2
  | (Global global1, Global global2) -> global1 == global2
  | (ArrayString str1, ArrayString str2) -> String.equal str1 str2
  | (ArrayVtable (size1, funcs1), ArrayVtable (size2, funcs2)) ->
    size1 = size2 && List.for_all2 (fun func1 func2 -> func1 == func2) funcs1 funcs2
  | _ -> false

let values_have_same_type (v1 : Value.t) (v2 : Value.t) : bool =
  types_equal (type_of_value v1) (type_of_value v2)

let invert_comparison (comparison : Instruction.comparison) : Instruction.comparison =
  match comparison with
  | Eq -> Neq
  | Neq -> Eq
  | Lt -> GtEq
  | LtEq -> Gt
  | Gt -> LtEq
  | GtEq -> Lt

let is_shift_op (op : Instruction.binary_operation) : bool =
  match op with
  | Shl
  | Shr
  | Shrl ->
    true
  | _ -> false

let cast_to_instruction (value : Value.t) : Instruction.t =
  match value.value with
  | Instr instr -> instr
  | _ -> failwith "Expected instruction value"

let cast_to_argument (value : Value.t) : Argument.t =
  match value.value with
  | Argument arg -> arg
  | _ -> failwith "Expected argument value"

let cast_to_phi (instr : Instruction.t) : Instruction.Phi.t =
  match instr with
  | { instr = Instruction.Phi phi; _ } -> phi
  | _ -> failwith "Expected phi instruction"

let int64_of_literal lit =
  match lit with
  | Literal.Byte lit -> Int8.to_int64 lit
  | Int lit -> Int64.of_int32 lit
  | Long lit -> lit
  | _ -> failwith "Expected byte, int, or long"

let std_lib_string_prefix = ".stdS"

let has_std_lib_string_prefix name =
  String.length name >= 5 && String.sub name 0 5 = std_lib_string_prefix

let std_lib_immutable_string_prefix = ".stdIS"

let has_std_lib_immutable_string_prefix name =
  String.length name >= 6 && String.sub name 0 6 = std_lib_immutable_string_prefix

let has_std_lib_prefix name =
  Std_lib.has_std_lib_prefix name
  || has_std_lib_string_prefix name
  || has_std_lib_immutable_string_prefix name

let filter_stdlib (program : Program.t) =
  let filter_stdlib_names smap = SMap.filter (fun name _ -> not (has_std_lib_prefix name)) smap in
  program.globals <- filter_stdlib_names program.globals;
  program.funcs <- filter_stdlib_names program.funcs;
  program

let get_terminator_value (block : Block.t) : Value.t option =
  match block.instructions with
  | Some { last; _ } -> Some last
  | None -> None

let get_terminator (block : Block.t) : Instruction.t option =
  match get_terminator_value block with
  | Some { value = Instr ({ instr = Ret _ | Continue _ | Branch _ | Unreachable; _ } as last); _ }
    ->
    Some last
  | _ -> None

let string_of_block_set (blocks : Block.t Seq.t) : string =
  let elements =
    blocks
    |> List.of_seq
    |> List.map (fun block -> Block.(id_to_string block.id))
    |> String.concat ", "
  in
  "(" ^ elements ^ ")"

let string_of_instr_set (instr_values : Value.t Seq.t) : string =
  let elements =
    instr_values
    |> List.of_seq
    |> List.map (fun instr_value -> string_of_int instr_value.Value.id)
    |> String.concat ", "
  in
  "(" ^ elements ^ ")"
