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
      (* Map from preceding basic block to the value to choose if that basic block was visited *)
      mutable args: Value.t BlockMap.t;
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
    mutable prev: Instruction.t;
    mutable next: Instruction.t;
    mutable block: Block.t;
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
  type id = int

  type t = {
    id: id;
    func: Function.t;
    mutable instructions: instructions option;
    mutable next: next;
    mutable prev_blocks: BlockSet.t;
  }

  (* Circular doubly linked list of all instructions in block *)
  and instructions = {
    mutable first: Instruction.t;
    mutable last: Instruction.t;
  }

  and next =
    | Halt
    | Continue of Block.t
    | Branch of {
        test: (* Bool *) Value.t;
        continue: Block.t;
        jump: Block.t;
      }

  val compare : t -> t -> int
end = struct
  type id = int

  type t = {
    id: id;
    func: Function.t;
    mutable instructions: instructions option;
    mutable next: next;
    mutable prev_blocks: BlockSet.t;
  }

  and instructions = {
    mutable first: Instruction.t;
    mutable last: Instruction.t;
  }

  and next =
    | Halt
    | Continue of Block.t
    | Branch of {
        test: Value.t;
        continue: Block.t;
        jump: Block.t;
      }

  let compare b1 b2 = Int.compare b1.id b2.id
end

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
    return_type: Type.t option;
    mutable start_block: Block.t;
    mutable blocks: BlockSet.t;
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

and BlockSet : (Set.S with type elt = Block.t) = Set.Make (Block)
and BlockMap : (Map.S with type key = Block.t) = Map.Make (Block)

and BlockMMap : (MultiMap.S with type key = Block.t and type value = Block.t) =
  MultiMap.Make (Block) (Block)

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

(* A placeholder function *)
let rec null_function : Function.t =
  {
    Function.name = "Null function";
    loc = Loc.none;
    params = [];
    return_type = None;
    start_block = null_block;
    blocks = BlockSet.empty;
  }

(* A placeholder block, to avoid having to represent Option<Block> with its extra allocation *)
and null_block : Block.t =
  {
    Block.id = mk_block_id ();
    func = null_function;
    instructions = None;
    next = Halt;
    prev_blocks = BlockSet.empty;
  }

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
  program.globals <- filter_stdlib_names program.globals;
  program.funcs <- filter_stdlib_names program.funcs;
  program

let has_no_instructions (block : Block.t) : bool = block.instructions = None

let has_single_instruction (block : Block.t) : bool =
  match block.instructions with
  | Some { first; last } when first == last -> true
  | _ -> false

let iter_instructions (block : Block.t) (f : Instruction.t -> unit) =
  match block.instructions with
  | None -> ()
  | Some { first; last } ->
    let rec iter current last f =
      (* Save next in case instruction is modified *)
      let next = current.Instruction.next in
      f current;
      if current != last then iter next last f
    in
    iter first last f

(* Utility function to check if an instruction list has a valid structure *)
let assert_valid_list (block : Block.t) =
  match block.instructions with
  | None -> ()
  | Some { first; last } ->
    if first.prev != last || last.next != first then failwith "List must be circular";
    let rec iter current last =
      if current.Instruction.next.prev != current then failwith "Link is not bidirectional";
      if current.block != block then failwith "Instruction does not have correct block";
      if current.next != last then iter current.next last
    in
    iter first last

let add_link (i1 : Instruction.t) (i2 : Instruction.t) =
  i1.next <- i2;
  i2.prev <- i1

(* Prepend an instruction to the beginning of a block's instruction list *)
let prepend_instruction (block : Block.t) (instr : Instruction.t) =
  instr.block <- block;
  match block.instructions with
  | None -> block.instructions <- Some { first = instr; last = instr }
  | Some ({ first; last } as list) ->
    add_link instr first;
    add_link last instr;
    list.first <- instr

(* Append an instruction to the end of a block's instruction list *)
let append_instruction (block : Block.t) (instr : Instruction.t) =
  instr.block <- block;
  match block.instructions with
  | None -> block.instructions <- Some { first = instr; last = instr }
  | Some ({ first; last } as list) ->
    add_link last instr;
    add_link instr first;
    list.last <- instr

(* Remove an instruction from a block's instruction list *)
let remove_instruction (block : Block.t) (instr : Instruction.t) =
  if (* Instruction list is circular, so check if single element list *)
     instr.next == instr then
    block.instructions <- None
  else
    let prev = instr.prev in
    let next = instr.next in
    prev.next <- next;
    next.prev <- prev;
    let list = Option.get block.instructions in
    if list.first == instr then list.first <- next;
    if list.last == instr then list.last <- prev

(* Concatenate the instructions in the second block to the end of the first block. 
   This is a destructive operation on the second block's instructions. *)
let concat_instructions (b1 : Block.t) (b2 : Block.t) =
  iter_instructions b2 (fun instr -> instr.block <- b1);
  match (b1.instructions, b2.instructions) with
  | (_, None) -> ()
  | (None, (Some _ as instrs)) -> b1.instructions <- instrs
  | (Some ({ first = first1; last = last1 } as list), Some { first = first2; last = last2 }) ->
    add_link last1 first2;
    add_link last2 first1;
    list.last <- last2

let filter_instructions (block : Block.t) (f : Instruction.t -> bool) =
  iter_instructions block (fun instr -> if not (f instr) then remove_instruction block instr)

let fold_instructions (block : Block.t) (acc : 'a) (f : Instruction.t -> 'a -> 'a) : 'a =
  match block.instructions with
  | None -> acc
  | Some { first; last } ->
    let rec fold current last f acc =
      let acc' = f current acc in
      if current == last then
        acc'
      else
        fold current.Instruction.next last f acc'
    in
    fold first last f acc

let block_has_phis (block : Block.t) : bool =
  match block.instructions with
  | Some { first = { instr = Phi _; _ }; _ } -> true
  | _ -> false

let block_get_phis (block : Block.t) : Instruction.Phi.t list =
  fold_instructions block [] (fun instr acc ->
      match instr with
      | { instr = Phi phi; _ } -> phi :: acc
      | _ -> acc)

let block_iter_phis (block : Block.t) (f : Instruction.Phi.t -> unit) =
  iter_instructions block (fun instr ->
      match instr with
      | { instr = Phi phi; _ } -> f phi
      | _ -> ())

let block_filter_phis (block : Block.t) (f : Value.id -> Instruction.Phi.t -> bool) =
  iter_instructions block (fun instr ->
      match instr with
      | { instr = Phi phi; id; _ } -> if not (f id phi) then remove_instruction block instr
      | _ -> ())

let block_fold_phis
    (block : Block.t) (acc : 'a) (f : Instruction.t -> Instruction.Phi.t -> 'a -> 'a) : 'a =
  fold_instructions block acc (fun instr acc ->
      match instr with
      | { instr = Phi phi; _ } -> f instr phi acc
      | _ -> acc)

let block_clear_phis (block : Block.t) = block_filter_phis block (fun _ _ -> false)

(* Return the set of all blocks that this block branches to *)
let get_next_blocks (block : Block.t) : BlockSet.t =
  match block.next with
  | Halt -> BlockSet.empty
  | Continue continue -> BlockSet.singleton continue
  | Branch { test = _; jump; continue } -> BlockSet.add jump (BlockSet.singleton continue)

let func_iter_blocks (func : Function.t) (f : Block.t -> unit) = BlockSet.iter f func.blocks

let program_iter_blocks (program : Program.t) (f : Block.t -> unit) =
  SMap.iter (fun _ func -> func_iter_blocks func f) program.funcs

(* Map block's next block from a block to another block. Do not update any phi references. *)
let map_next_block (block : Block.t) ~(from : Block.t) ~(to_ : Block.t) =
  let map_next_block maybe_from =
    if maybe_from == from then (
      from.prev_blocks <- BlockSet.remove block from.prev_blocks;
      to_.prev_blocks <- BlockSet.add block to_.prev_blocks;
      to_
    ) else
      maybe_from
  in
  block.next <-
    (match block.next with
    | Halt -> Halt
    | Continue continue_block -> Continue (map_next_block continue_block)
    | Branch { test; continue; jump } ->
      let new_continue = map_next_block continue in
      let new_jump = map_next_block jump in
      (* If both branches point to same label convert to continue *)
      if new_continue == new_jump then
        Continue new_continue
      else
        (* Otherwise create branch to new block *)
        Branch { test; continue = new_continue; jump = new_jump })

(* Split an edge between two blocks, inserting an empty block in the middle *)
let split_block_edge (prev_block : Block.t) (next_block : Block.t) : Block.t =
  let func = prev_block.func in
  let new_block =
    {
      Block.id = mk_block_id ();
      func;
      instructions = None;
      next = Continue next_block;
      prev_blocks = BlockSet.singleton prev_block;
    }
  in
  func.blocks <- BlockSet.add new_block func.blocks;
  map_next_block prev_block ~from:next_block ~to_:new_block;
  new_block

let string_of_block_set (blocks : BlockSet.t) : string =
  let elements =
    BlockSet.to_seq blocks
    |> List.of_seq
    |> List.map (fun block -> string_of_int block.Block.id)
    |> String.concat ", "
  in
  "(" ^ elements ^ ")"

module VSet = Set.Make (Value)
module VMap = Map.Make (Value)
module VVMMap = MultiMap.Make (Value) (Value)
