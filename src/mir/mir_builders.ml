open Basic_collections
open Mir
open Mir_type

(*
 * ============================
 *           Values
 * ============================
 *)

let uninit_value : Value.value = Value.Lit (Bool true)

let mk_value (value : Value.value) : Value.t = { value; uses = None }

let mk_uninit_value () : Value.t = { value = uninit_value; uses = None }

(*
 * ============================
 *          Literals
 * ============================
 *)

let mk_bool_lit (b : bool) : Value.t = mk_value (Lit (Bool b))

let mk_byte_lit (n : int) : Value.t = mk_value (Lit (Byte n))

let mk_int_lit (n : int) : Value.t = mk_value (Lit (Int (Int32.of_int n)))

let mk_int_lit_of_int32 (n : Int32.t) : Value.t = mk_value (Lit (Int n))

let mk_long_lit (n : Int64.t) : Value.t = mk_value (Lit (Long n))

let mk_null_ptr_lit (type_ : Type.t) : Value.t = mk_value (Lit (NullPointer type_))

let mk_myte_builtin_lit (func : string) : Value.t = mk_value (Lit (MyteBuiltin func))

let mk_array_string_lit (string : string) : Value.t = mk_value (Lit (ArrayString string))

let mk_array_vtable_lit (funcs : Function.t list) : Value.t =
  let size = List.length funcs in
  mk_value (Lit (ArrayVtable (size, funcs)))

(*
 * ============================
 *   Instruction Constructors
 * ============================
 *)

(* Creates an instruction that is not yet part of a block *)
let rec mk_blockless_instr ~(type_ : Type.t) ~(instr : Instruction.instr) : Instruction.t =
  let rec instruction =
    {
      Instruction.id = mk_value_id ();
      type_;
      instr;
      prev = instruction;
      next = instruction;
      block = null_block;
    }
  in
  instruction

(* Create an instruction and appends it the end of a block *)
and mk_instr ~(block : Block.t) ~(type_ : Type.t) ~(instr : Instruction.instr) : Instruction.t =
  let instruction = mk_blockless_instr ~type_ ~instr in
  append_instruction block instruction;
  instruction

and mk_blockless_phi ~(type_ : Type.t) ~(args : Value.t BlockMap.t) : Value.t =
  let value = mk_uninit_value () in
  let args = BlockMap.map (fun arg -> user_add_use ~user:value ~use:arg) args in
  let instr = mk_blockless_instr ~type_ ~instr:(Phi { args }) in
  value.value <- Instr instr;
  value

and mk_blockless_mov ~(arg : Value.t) : Value.t =
  let value = mk_uninit_value () in
  let arg_use = user_add_use ~user:value ~use:arg in
  let instr = mk_blockless_instr ~type_:(type_of_value arg) ~instr:(Mov arg_use) in
  value.value <- Instr instr;
  value

and mk_blockless_stack_alloc ~(type_ : Type.t) : Value.t =
  let instr = mk_blockless_instr ~type_:(Pointer type_) ~instr:(StackAlloc type_) in
  mk_value (Instr instr)

and mk_stack_alloc ~(block : Block.t) ~(type_ : Type.t) : Value.t =
  let instr = mk_instr ~block ~type_:(Pointer type_) ~instr:(StackAlloc type_) in
  mk_value (Instr instr)

and mk_load ~(block : Block.t) ~(ptr : Value.t) : Value.t =
  match type_of_value ptr with
  | Pointer type_ ->
    let value = mk_uninit_value () in
    let ptr_use = user_add_use ~user:value ~use:ptr in
    let instr = mk_instr ~block ~type_ ~instr:(Load ptr_use) in
    value.value <- Instr instr;
    value
  | _ -> failwith "Load argument must be a pointer type"

and mk_store_ ~(block : Block.t) ~(ptr : Value.t) ~(value : Value.t) : unit =
  if not (types_equal (pointer_value_element_type ptr) (type_of_value value)) then
    failwith "Stored pointer and value types do not match";
  let instr_value = mk_uninit_value () in
  let ptr_use = user_add_use ~user:instr_value ~use:ptr in
  let value_use = user_add_use ~user:instr_value ~use:value in
  let instr = mk_instr ~block ~type_:no_return_type ~instr:(Store (ptr_use, value_use)) in
  instr_value.value <- Instr instr

and mk_get_pointer_instr
    ~(block : Block.t)
    ?(pointer_offset : Value.t option = None)
    ~(type_ : Type.t)
    ~(ptr : Value.t)
    ~(offsets : Instruction.GetPointer.value_offset list)
    () : Value.t =
  if not (is_pointer_value ptr) then failwith "GetPointer argument must be a pointer type";
  let value = mk_uninit_value () in
  let ptr_use = user_add_use ~user:value ~use:ptr in
  let pointer_offset_use =
    Option.map (fun offset -> user_add_use ~user:value ~use:offset) pointer_offset
  in
  let use_offsets =
    List.map
      (fun offset ->
        match offset with
        | Instruction.GetPointer.PointerIndex arg ->
          Instruction.GetPointer.PointerIndex (user_add_use ~user:value ~use:arg)
        | FieldIndex index -> FieldIndex index)
      offsets
  in
  let instr =
    mk_instr
      ~block
      ~type_:(Pointer type_)
      ~instr:
        (GetPointer
           { pointer = ptr_use; pointer_offset = pointer_offset_use; offsets = use_offsets })
  in
  value.value <- Instr instr;
  value

and mk_call ~(block : Block.t) ~(func : Value.t) ~(args : Value.t list) ~(return : Type.t option) :
    Value.t =
  if not (is_function_value func) then failwith "Call function argument must have function type";
  let value = mk_uninit_value () in
  let func_use = user_add_use ~user:value ~use:func in
  let arg_uses = List.map (fun arg -> user_add_use ~user:value ~use:arg) args in
  let (type_, has_return) =
    match return with
    | Some type_ -> (type_, true)
    | None -> (no_return_type, false)
  in
  let instr =
    mk_instr ~block ~type_ ~instr:(Call { func = Value func_use; args = arg_uses; has_return })
  in
  value.value <- Instr instr;
  value

and mk_call_ ~block ~func ~args ~return : unit = ignore (mk_call ~block ~func ~args ~return)

and mk_call_builtin
    ~(block : Block.t) (builtin : Builtin.t) (args : Value.t list) (mk_return_ty_args : Type.t list)
    : Value.t =
  let value = mk_uninit_value () in
  let arg_uses = List.map (fun arg -> user_add_use ~user:value ~use:arg) args in
  let return_type = builtin.mk_return_ty mk_return_ty_args |> Option.get in
  let instr =
    mk_instr
      ~block
      ~type_:return_type
      ~instr:(Call { func = MirBuiltin builtin; args = arg_uses; has_return = true })
  in
  value.value <- Instr instr;
  value

and mk_call_builtin_no_return_ ~(block : Block.t) (builtin : Builtin.t) (args : Value.t list) =
  let value = mk_uninit_value () in
  let arg_uses = List.map (fun arg -> user_add_use ~user:value ~use:arg) args in
  let instr =
    mk_instr
      ~block
      ~type_:no_return_type
      ~instr:(Call { func = MirBuiltin builtin; args = arg_uses; has_return = false })
  in
  value.value <- Instr instr

and mk_ret_ ~(block : Block.t) ~(arg : Value.t option) =
  let value = mk_uninit_value () in
  let arg_use = Option.map (fun arg -> user_add_use ~user:value ~use:arg) arg in
  let instr = mk_instr ~block ~type_:no_return_type ~instr:(Ret arg_use) in
  value.value <- Instr instr

and mk_unary ~(block : Block.t) ~(op : Instruction.unary_operation) ~(arg : Value.t) : Value.t =
  if not (is_numeric_value arg) then failwith "Unary argument must be numeric value";
  let value = mk_uninit_value () in
  let arg_use = user_add_use ~user:value ~use:arg in
  let instr = mk_instr ~block ~type_:(type_of_value arg) ~instr:(Unary (op, arg_use)) in
  value.value <- Instr instr;
  value

and mk_binary
    ~(block : Block.t) ~(op : Instruction.binary_operation) ~(left : Value.t) ~(right : Value.t) :
    Value.t =
  let is_shift_op = is_shift_op op in
  if is_shift_op && not (is_numeric_value left && is_numeric_value right) then
    failwith "Shift arguments must be numeric"
  else if
    (not is_shift_op)
    && not (is_numeric_value left && is_numeric_value right && values_have_same_type left right)
  then
    failwith "Binary arguments must be numeric and have the same type";
  let value = mk_uninit_value () in
  let left_use = user_add_use ~user:value ~use:left in
  let right_use = user_add_use ~user:value ~use:right in
  let instr =
    mk_instr ~block ~type_:(type_of_value left) ~instr:(Binary (op, left_use, right_use))
  in
  value.value <- Instr instr;
  value

and mk_cmp ~(block : Block.t) ~(cmp : Instruction.comparison) ~(left : Value.t) ~(right : Value.t) :
    Value.t =
  if not (is_comparable_value left && is_comparable_value right && values_have_same_type left right)
  then
    failwith "Cmp arguments must be numeric or pointers and have the same type";
  let value = mk_uninit_value () in
  let left_use = user_add_use ~user:value ~use:left in
  let right_use = user_add_use ~user:value ~use:right in
  let instr = mk_instr ~block ~type_:Bool ~instr:(Cmp (cmp, left_use, right_use)) in
  value.value <- Instr instr;
  value

and mk_cast ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  if not (is_pointer_value arg && is_pointer_type type_) then
    failwith "Cast arguments must be pointers";
  let value = mk_uninit_value () in
  let arg_use = user_add_use ~user:value ~use:arg in
  let instr = mk_instr ~block ~type_ ~instr:(Cast arg_use) in
  value.value <- Instr instr;
  value

and mk_trunc ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  let arg_type = type_of_value arg in
  if
    not
      ( is_numeric_type arg_type
      && is_numeric_type type_
      && size_of_type arg_type >= size_of_type type_ )
  then
    failwith
      "Trunc arguments must be numeric with type argument having smaller size than value argument";
  let value = mk_uninit_value () in
  let arg_use = user_add_use ~user:value ~use:arg in
  let instr = mk_instr ~block ~type_ ~instr:(Trunc arg_use) in
  value.value <- Instr instr;
  value

and mk_sext ~(block : Block.t) ~(arg : Value.t) ~(type_ : Type.t) : Value.t =
  let arg_type = type_of_value arg in
  if
    not
      ( is_numeric_type arg_type
      && is_numeric_type type_
      && size_of_type arg_type <= size_of_type type_ )
  then
    failwith
      "SExt arguments must be numeric with type argument having larger size than value argument";
  let value = mk_uninit_value () in
  let arg_use = user_add_use ~user:value ~use:arg in
  let instr = mk_instr ~block ~type_ ~instr:(SExt arg_use) in
  value.value <- Instr instr;
  value

and mk_unreachable_ ~(block : Block.t) : unit =
  ignore (mk_instr ~block ~type_:no_return_type ~instr:Unreachable)

and mk_continue_ ~(block : Block.t) ~(continue : Block.t) : unit =
  ignore (mk_instr ~block ~type_:no_return_type ~instr:(Continue continue))

and mk_branch_ ~(block : Block.t) ~(test : Value.t) ~(continue : Block.t) ~(jump : Block.t) : unit =
  let value = mk_uninit_value () in
  let test_use = user_add_use ~user:value ~use:test in
  let instr =
    mk_instr ~block ~type_:no_return_type ~instr:(Branch { test = test_use; continue; jump })
  in
  value.value <- Instr instr

(*
 * ============================
 *           Globals
 * ============================
 *)
and mk_global
    ~(name : label)
    ~(loc : Loc.t)
    ~(type_ : Type.t)
    ~(init_val : Value.t option)
    ~(is_constant : bool) : Global.t =
  let value = mk_uninit_value () in
  let init_val_use = Option.map (fun init_val -> user_add_use ~user:value ~use:init_val) init_val in
  let global = { Global.loc; name; type_; init_val = init_val_use; is_constant; value } in
  value.value <- Lit (Global global);
  global

and global_set_init ~(global : Global.t) ~(init : Value.t option) =
  (* TODO: Check if the old and new value are the same *)
  (match global.init_val with
  | None -> ()
  | Some old_init_use -> remove_use ~use:old_init_use);
  let init_use = Option.map (fun init -> user_add_use ~user:global.value ~use:init) init in
  global.init_val <- init_use

(*
 * ============================
 *          Functions
 * ============================
 *)
and mk_function ~(name : label) : Function.t =
  let value = mk_uninit_value () in
  let func =
    {
      Function.name;
      loc = Loc.none;
      params = [];
      return_type = None;
      start_block = null_block;
      blocks = BlockSet.empty;
      value;
    }
  in
  value.value <- Lit (Function func);
  func

and mk_argument ~(func : Function.t) ~(decl_loc : Loc.t) ~(type_ : Type.t) : Value.t =
  let argument = { Argument.id = mk_value_id (); type_; func; decl_loc } in
  mk_value (Argument argument)

and func_iter_blocks (func : Function.t) (f : Block.t -> unit) = BlockSet.iter f func.blocks

(*
 * ============================
 *            Uses
 * ============================
 *)
and add_use_link (u1 : Use.t) (u2 : Use.t) =
  u1.next <- u2;
  u2.prev <- u1

and user_add_use ~(user : Value.t) ~(use : Value.t) =
  match use.uses with
  | None ->
    let rec use_node = { Use.value = use; prev = use_node; next = use_node; user } in
    use.uses <- Some use_node;
    use_node
  | Some first_use ->
    let rec use_node = { Use.value = use; prev = use_node; next = use_node; user } in
    add_use_link use_node first_use.next;
    add_use_link first_use use_node;
    use_node

and remove_use ~(use : Use.t) =
  let value = use.value in
  if use.next == use then
    value.uses <- None
  else
    let prev = use.prev in
    let next = use.next in
    add_use_link prev next;
    if Option.get value.uses == use then value.uses <- Some use.next

(*
 * ============================
 *      Block Instructions
 * ============================
 *)
and has_single_instruction (block : Block.t) : bool =
  match block.instructions with
  | Some { first; last } when first == last -> true
  | _ -> false

and add_instr_link (i1 : Instruction.t) (i2 : Instruction.t) =
  i1.next <- i2;
  i2.prev <- i1

(* Prepend an instruction to the beginning of a block's instruction list *)
and prepend_instruction (block : Block.t) (instr : Instruction.t) =
  instr.block <- block;
  match block.instructions with
  | None -> block.instructions <- Some { first = instr; last = instr }
  | Some ({ first; last } as list) ->
    add_instr_link instr first;
    add_instr_link last instr;
    list.first <- instr

(* Append an instruction to the end of a block's instruction list *)
and append_instruction (block : Block.t) (instr : Instruction.t) =
  instr.block <- block;
  match block.instructions with
  | None -> block.instructions <- Some { first = instr; last = instr }
  | Some ({ first; last } as list) ->
    add_instr_link last instr;
    add_instr_link instr first;
    list.last <- instr

(* Insert an instruction immediately before another instruction in a block's instruction list *)
and insert_instruction_before ~(before : Instruction.t) (instr : Instruction.t) =
  let block = before.block in
  instr.block <- block;
  match block.instructions with
  | None -> failwith "Block must have before instruction"
  | Some list ->
    let prev_instr = before.prev in
    add_instr_link prev_instr instr;
    add_instr_link instr before;
    if list.first == before then list.first <- instr

(* Remove an instruction from a block's instruction list *)
and remove_instruction (block : Block.t) (instr : Instruction.t) =
  if (* Instruction list is circular, so check if single element list *)
     instr.next == instr then
    block.instructions <- None
  else
    let prev = instr.prev in
    let next = instr.next in
    add_instr_link prev next;
    let list = Option.get block.instructions in
    if list.first == instr then list.first <- next;
    if list.last == instr then list.last <- prev

(* Concatenate the instructions in the second block to the end of the first block. 
   This is a destructive operation on the second block's instructions. Removes the first block's
   terminator instruction. *)
and concat_instructions (b1 : Block.t) (b2 : Block.t) =
  (* Remove terminator from first block *)
  (match get_terminator b1 with
  | Some terminator -> remove_instruction b1 terminator
  | None -> ());
  (* Concatenate lists of instructions *)
  iter_instructions b2 (fun instr -> instr.Instruction.block <- b1);
  match (b1.instructions, b2.instructions) with
  | (_, None) -> ()
  | (None, (Some _ as instrs)) -> b1.instructions <- instrs
  | (Some ({ first = first1; last = last1 } as list), Some { first = first2; last = last2 }) ->
    add_instr_link last1 first2;
    add_instr_link last2 first1;
    list.last <- last2

(* Utility function to check if an instruction list has a valid structure *)
and assert_valid_list (block : Block.t) =
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

and iter_instructions (block : Block.t) (f : Instruction.t -> unit) =
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

and filter_instructions (block : Block.t) (f : Instruction.t -> bool) =
  iter_instructions block (fun instr -> if not (f instr) then remove_instruction block instr)

and fold_instructions : 'a. Block.t -> 'a -> (Instruction.t -> 'a -> 'a) -> 'a =
 fun block acc f ->
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

(*
 * ============================
 *         Block Phis
 * ============================
 *)
and block_has_phis (block : Block.t) : bool =
  match block.instructions with
  | Some { first = { instr = Phi _; _ }; _ } -> true
  | _ -> false

and block_get_phis (block : Block.t) : Instruction.Phi.t list =
  fold_instructions block [] (fun instr acc ->
      match instr with
      | { instr = Phi phi; _ } -> phi :: acc
      | _ -> acc)

and block_iter_phis (block : Block.t) (f : Instruction.Phi.t -> unit) =
  iter_instructions block (fun instr ->
      match instr with
      | { instr = Phi phi; _ } -> f phi
      | _ -> ())

and block_filter_phis (block : Block.t) (f : Value.id -> Instruction.Phi.t -> bool) =
  iter_instructions block (fun instr ->
      match instr with
      | { instr = Phi phi; id; _ } -> if not (f id phi) then remove_instruction block instr
      | _ -> ())

and block_fold_phis (block : Block.t) (acc : 'a) (f : Instruction.Phi.t -> 'a -> 'a) : 'a =
  fold_instructions block acc (fun instr acc ->
      match instr with
      | { instr = Phi phi; _ } -> f phi acc
      | _ -> acc)

and block_clear_phis (block : Block.t) = block_filter_phis block (fun _ _ -> false)

(*
 * ============================
 *        Block Graph
 * ============================
 *)
and add_block_link (prev_block : Block.t) (next_block : Block.t) =
  next_block.prev_blocks <- BlockSet.add prev_block next_block.prev_blocks

and remove_block_link (prev_block : Block.t) (next_block : Block.t) =
  next_block.prev_blocks <- BlockSet.remove prev_block next_block.prev_blocks

(* Return the set of all blocks that this block branches to *)
and get_next_blocks (block : Block.t) : BlockSet.t =
  match get_terminator block with
  | Some { instr = Continue continue; _ } -> BlockSet.singleton continue
  | Some { instr = Branch { test = _; jump; continue }; _ } ->
    BlockSet.add jump (BlockSet.singleton continue)
  | _ -> BlockSet.empty

(*
 * ============================
 *     Block Graph Mutation
 * ============================
 *)

(* An empty block can be removed only if it continues to a single block, and is not needed by any
   phi nodes in its succeeding block. *)
and can_remove_block (block : Block.t) =
  has_single_instruction block
  &&
  match get_terminator block with
  | Some { instr = Continue continue_block; _ } ->
    (* A block is needed if any of its previous blocks appear in a phi node of the next block, with
       a different value than the value from this block. A block is also needed if it is the start
       block and the next block has any phi nodes. If we were to remove this block, the value from
       its branch would be lost in the phi node. *)
    let is_start_block = block.func.start_block == block in

    let continue_block_phis = block_get_phis continue_block in
    let block_needed_for_phi =
      (continue_block_phis <> [] && is_start_block)
      || List.exists
           (fun { Instruction.Phi.args; _ } ->
             BlockMap.exists
               (fun prev_block prev_block_arg ->
                 if BlockSet.mem prev_block block.prev_blocks then
                   not (values_equal prev_block_arg.Use.value (BlockMap.find block args).value)
                 else
                   false)
               args)
           continue_block_phis
    in
    let function_start_self_loop = is_start_block && continue_block == block in
    (not block_needed_for_phi) && not function_start_self_loop
  | _ -> false

and remove_block (block : Block.t) =
  (* This may be the first block in a function. If so, update the function to point to the
     next block as the start. *)
  let func = block.func in
  (match get_terminator block with
  | Some { instr = Continue continue_block; _ } ->
    if func.start_block == block then func.start_block <- continue_block
  | _ -> ());
  (match get_terminator block with
  (* Only when removing unreachable blocks from branch pruning, which could include return block.
     Remove any instances of block from previous blocks. *)
  | Some { instr = Unreachable; _ } ->
    BlockSet.iter
      (fun prev_block ->
        match get_terminator prev_block with
        | Some ({ instr = Continue _; _ } as term_instr) -> term_instr.instr <- Unreachable
        | Some ({ instr = Branch { test = _; continue; jump }; _ } as term_instr) ->
          term_instr.instr <-
            ( if continue == block then
              if jump == block then
                Unreachable
              else
                Continue jump
            else
              Continue continue )
        | _ -> failwith "Previous block must have branching terminator")
      block.prev_blocks
  | Some { instr = Continue next_block; _ } ->
    (* Update phis in next block to reference previous blocks instead of removed block *)
    map_phi_backreferences_for_block block block.prev_blocks next_block;

    (* Rewrite next of previous blocks to point to next block instead of removed block *)
    BlockSet.iter
      (fun prev_block -> map_next_block prev_block ~from:block ~to_:next_block)
      block.prev_blocks
  | _ -> ());

  (* Remove references to this removed block from phi nodes of next blocks *)
  let next_blocks = get_next_blocks block in
  BlockSet.iter (fun next_block -> remove_phi_backreferences_for_block block next_block) next_blocks;
  (* Remove prev pointers from next blocks to this removed block *)
  let next_blocks = get_next_blocks block in
  BlockSet.iter
    (fun next_block -> next_block.prev_blocks <- BlockSet.remove block next_block.prev_blocks)
    next_blocks;
  (* Remove block from remaining maps in context *)
  func.blocks <- BlockSet.remove block func.blocks

(* Merge adjacent blocks b1 and b2. Must only be called if b1 and b2 can be merged, meaning
   b1 only continues to b2 and b2 has no other previous blocks. *)
and merge_adjacent_blocks block1 block2 =
  let open Block in
  let map_block block =
    if block == block2 then
      block1
    else
      block
  in
  (* Use b2's next, but take care to reference b1 instead of b2 in the case of self references *)
  (match get_terminator block2 with
  | Some ({ instr = Continue continue; _ } as term_instr) ->
    term_instr.instr <- Continue (map_block continue)
  | Some ({ instr = Branch { test; continue; jump }; _ } as term_instr) ->
    term_instr.instr <- Branch { test; continue = map_block continue; jump = map_block jump }
  | _ -> ());
  concat_instructions block1 block2;
  (* References to the b2 block in phi nodes of blocks that succeed b2 should be rewritten
     to now reference b1 instead. *)
  let next_blocks = get_next_blocks block2 in
  BlockSet.iter
    (fun next_block ->
      map_phi_backreferences_for_block block2 (BlockSet.singleton block1) next_block)
    next_blocks;
  (* Set prev pointers for blocks that succeed b2 to point to b1 instead *)
  BlockSet.iter
    (fun next_block ->
      remove_block_link block2 next_block;
      add_block_link block1 next_block)
    next_blocks;
  remove_block_link block1 block2;
  (* Remove b2 from remaining maps in context *)
  let func = block2.func in
  func.blocks <- BlockSet.remove block2 func.blocks

(* Split an edge between two blocks, inserting an empty block in the middle *)
and split_block_edge (prev_block : Block.t) (next_block : Block.t) : Block.t =
  let func = prev_block.func in
  let new_block =
    {
      Block.id = Block.mk_id ();
      func;
      instructions = None;
      prev_blocks = BlockSet.singleton prev_block;
    }
  in
  mk_continue_ ~block:new_block ~continue:next_block;
  func.blocks <- BlockSet.add new_block func.blocks;
  map_next_block prev_block ~from:next_block ~to_:new_block;
  new_block

(* Map block's next block from a block to another block. Do not update any phi references. *)
and map_next_block (block : Block.t) ~(from : Block.t) ~(to_ : Block.t) =
  let map_next_block maybe_from =
    if maybe_from == from then (
      from.prev_blocks <- BlockSet.remove block from.prev_blocks;
      to_.prev_blocks <- BlockSet.add block to_.prev_blocks;
      to_
    ) else
      maybe_from
  in
  match get_terminator block with
  | Some ({ instr = Continue continue; _ } as term_instr) ->
    term_instr.instr <- Continue (map_next_block continue)
  | Some ({ instr = Branch { test; jump; continue }; _ } as term_instr) ->
    let new_continue = map_next_block continue in
    let new_jump = map_next_block jump in
    (* If both branches point to same block convert to continue *)
    term_instr.instr <-
      ( if new_continue == new_jump then
        Continue new_continue
      else
        Branch { test; continue = new_continue; jump = new_jump } )
  | _ -> ()

(* Remove all references to a block from phi nodes of on of its next blocks.
   This may be needed when removing a block or block link. *)
and remove_phi_backreferences_for_block block_to_remove next_block =
  block_iter_phis next_block (fun phi ->
      phi.args <- BlockMap.filter (fun prev_block _ -> prev_block != block_to_remove) phi.args)

(* Replace all references to old_block_id in the phis of a block with new_block_ids. Note that there
   may be multiple new_block_ids, so a single phi argument may be expanded to multiple arguments.
   This may be needed when editing the program. *)
and map_phi_backreferences_for_block old_block new_blocks block_to_edit =
  block_iter_phis block_to_edit (fun ({ args; _ } as phi) ->
      match BlockMap.find_opt old_block args with
      | None -> ()
      | Some value ->
        let args_without_old_block = BlockMap.remove old_block args in
        phi.args <-
          BlockSet.fold
            (fun new_block args -> BlockMap.add new_block value args)
            new_blocks
            args_without_old_block)

(*
 * ============================
 *           Program
 * ============================
 *)
and program_iter_blocks (program : Program.t) (f : Block.t -> unit) =
  SMap.iter (fun _ func -> func_iter_blocks func f) program.funcs
