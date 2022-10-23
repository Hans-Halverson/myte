open Basic_collections
open Mir
open Mir_builders
open Mir_builtin
open Mir_type
open Ssa

(* Scalars must be myte_alloc calls or GetPointers that have only field indices. Scalars are
   implemented such that two GetPointers are equal if they point to the same memory location.

   This allows us to map from a set of equivalent scalars to the same value in maps. *)
module Scalar = struct
  type t = Value.t

  let rec compare_offsets offsets1 offsets2 =
    let open Instruction.GetPointer in
    match (offsets1, offsets2) with
    | ([], []) -> 0
    | ([], _ :: _) -> -1
    | (_ :: _, []) -> 1
    | (FieldIndex field1 :: tl1, FieldIndex field2 :: tl2) ->
      if field1 == field2 then
        compare_offsets tl1 tl2
      else
        Int.compare field1 field2
    | _ -> failwith "Expected FieldIndex"

  let rec compare v1 v2 =
    let open Instruction in
    let v1_instr = cast_to_instruction v1 in
    let v2_instr = cast_to_instruction v2 in
    match (v1_instr.instr, v2_instr.instr) with
    | (Call _, GetPointer _) -> -1
    | (GetPointer _, Call _) -> 1
    | (Call _, Call _) -> Int.compare v1.id v2.id
    | ( GetPointer { pointer = pointer1; offsets = offsets1; _ },
        GetPointer { pointer = pointer2; offsets = offsets2; _ } ) ->
      let pointer_compare = compare pointer1.value pointer2.value in
      if pointer_compare != 0 then
        pointer_compare
      else
        compare_offsets offsets1 offsets2
    | _ -> failwith "Expected Call or GetPointer"
end

module ScalarCollection : MultiMap.KEY_AND_VALUE_TYPE with type t = Value.t = MakeCollection (Scalar)
module ScalarSet = ScalarCollection.Set
module ScalarMap = ScalarCollection.Map

module ScalarValueMMap = MultiMap.Make (ScalarMap) (VSet)

let is_myte_alloc (instr : Instruction.t) : bool =
  match instr.instr with
  | Call { func = MirBuiltin mir_builtin; _ } when mir_builtin == myte_alloc -> true
  | _ -> false

let is_pure_field_offset_with_base get_pointer_instr base_pointer =
  let open Instruction.GetPointer in
  get_pointer_instr.pointer.value == base_pointer
  && get_pointer_instr.pointer_offset == None
  && List.for_all
       (function
         | FieldIndex _ -> true
         | _ -> false)
       get_pointer_instr.offsets

let escape_analysis ~func =
  let non_escaped_allocs = ref VSet.empty in

  (* Visit derived pointer tree in postorder *)
  let rec does_instruction_escape (instr_value : Value.t) : bool =
    let has_escaped = ref false in
    let mark_escaped () = has_escaped := true in

    value_iter_uses ~value:instr_value (fun use ->
        match use.user.value with
        | Instr instr ->
          (match instr.instr with
          (* If a pointer derived with GetPointer escapes then the base pointer also escapes *)
          | GetPointer get_pointer_instr
            when is_pure_field_offset_with_base get_pointer_instr instr_value ->
            if does_instruction_escape use.user then mark_escaped ()
          (* Conservatively consider any use other than a load or store to count as an escape *)
          | Store (pointer_use, arg_use)
            when pointer_use.value == instr_value && arg_use.value != instr_value ->
            ()
          | Load pointer_use when pointer_use.value == instr_value -> ()
          | _ -> mark_escaped ())
        | _ -> ());

    !has_escaped
  in

  func_iter_blocks func (fun block ->
      iter_instructions block (fun instr_value instr ->
          if is_myte_alloc instr then
            if not (does_instruction_escape instr_value) then
              non_escaped_allocs := VSet.add instr_value !non_escaped_allocs));

  !non_escaped_allocs

let calculate_scalar_items ~non_escaped_allocs =
  let scalar_items = ref ScalarValueMMap.empty in
  let scalar_def_instrs = ref VSet.empty in

  let rec visit instr_value =
    scalar_def_instrs := VSet.add instr_value !scalar_def_instrs;
    (match (cast_to_instruction instr_value).instr with
    | GetPointer _ -> scalar_items := ScalarValueMMap.add instr_value instr_value !scalar_items
    | _ -> ());
    value_iter_uses ~value:instr_value (fun use ->
        match use.user.value with
        | Instr { instr = GetPointer _; _ } -> visit use.user
        | _ -> ())
  in
  VSet.iter visit non_escaped_allocs;

  (!scalar_items, !scalar_def_instrs)

module SROAContext = struct
  type promotable_item = Scalar.t

  type t = {
    mutable phi_to_scalar_item: Scalar.t VMap.t;
    mutable stacks: Value.t Stack.t ScalarMap.t;
    (* Map from scalar item to all instructions that are promoted to that scalar item *)
    scalar_items: ScalarValueMMap.t;
    (* All promoted alloc instructions and the GetPointers that are derived from them *)
    scalar_def_instrs: VSet.t;
  }

  let mk ~scalar_items ~scalar_def_instrs =
    { phi_to_scalar_item = VMap.empty; stacks = ScalarMap.empty; scalar_items; scalar_def_instrs }

  let check_promotable_instr ~cx instr_value _ = VSet.mem instr_value cx.scalar_def_instrs

  let iter_promotable_items ~cx f = ScalarValueMMap.iter (fun key _ -> f key) cx.scalar_items

  (* Iterate over the blocks that contain definitions (aka stores) for a given promoted memory value *)
  let iter_promotable_item_def_blocks ~cx scalar_item f =
    let iter_scalar_item_def_instrs f =
      let instrs = ScalarValueMMap.find_all scalar_item cx.scalar_items in
      VSet.iter f instrs
    in

    iter_scalar_item_def_instrs (fun def_instr ->
        value_iter_uses ~value:def_instr (fun use ->
            match use.user.value with
            | Instr { instr = Store (ptr_use, _); block = def_block; _ }
              when ScalarValueMMap.contains scalar_item ptr_use.value cx.scalar_items ->
              f def_block
            | _ -> ()))

  let create_phi_node ~cx scalar_item =
    let type_ = cast_to_pointer_type (type_of_value scalar_item) in
    let phi = mk_blockless_phi ~type_ ~args:BlockMap.empty in
    cx.phi_to_scalar_item <- VMap.add phi scalar_item cx.phi_to_scalar_item;
    phi

  let get_phi_promoted_item ~cx instr_value instr =
    match instr.Instruction.instr with
    | Phi _ -> VMap.find_opt instr_value cx.phi_to_scalar_item
    | _ -> None

  let get_store_promoted_item ~cx _ instr =
    match instr.Instruction.instr with
    | Store (ptr_use, arg_use) when VSet.mem ptr_use.value cx.scalar_def_instrs ->
      Some (ptr_use.value, arg_use)
    | _ -> None

  let get_load_promoted_item ~cx _ instr =
    match instr.Instruction.instr with
    | Load ptr_use when VSet.mem ptr_use.value cx.scalar_def_instrs -> Some ptr_use.value
    | _ -> None

  let get_promoted_item_def_stack ~cx scalar_item =
    match ScalarMap.find_opt scalar_item cx.stacks with
    | Some stack -> stack
    | None ->
      let stack = Stack.create () in
      cx.stacks <- ScalarMap.add scalar_item stack cx.stacks;
      stack

  let debug_string_of_promotable_item scalar_item = string_of_int scalar_item.Value.id
end

module _ : SSA_TRANFORMER_CONTEXT = SSAContext

module SROA = SSATransformer (SROAContext)

let run ~program =
  program_iter_funcs program (fun func ->
      let non_escaped_allocs = escape_analysis ~func in
      let (scalar_items, scalar_def_instrs) = calculate_scalar_items ~non_escaped_allocs in
      let cx = SROAContext.mk ~scalar_items ~scalar_def_instrs in
      SROA.run ~cx ~func)
