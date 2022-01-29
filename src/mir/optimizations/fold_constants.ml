open Basic_collections
open Mir
open Mir_builders
open Mir_type
open Mir_visitor

type folded_constant =
  | ByteConstant of int
  | IntConstant of Int32.t
  | LongConstant of Int64.t
  | BoolConstant of bool
  | FunctionConstant of Function.t

type conversion_op =
  | TruncOp of Type.t
  | SExtOp of Type.t

let apply_unary_operation op x =
  match (op, x) with
  | (Instruction.Neg, BoolConstant x) -> BoolConstant x
  | (Neg, ByteConstant x) -> ByteConstant (-x)
  | (Neg, IntConstant x) -> IntConstant (Int32.neg x)
  | (Neg, LongConstant x) -> LongConstant (Int64.neg x)
  | (Not, BoolConstant x) -> BoolConstant (not x)
  | (Not, ByteConstant x) -> ByteConstant (lnot x)
  | (Not, IntConstant x) -> IntConstant (Int32.lognot x)
  | (Not, LongConstant x) -> LongConstant (Int64.lognot x)
  | ((Neg | Not), FunctionConstant _) -> failwith "Invalid operation"

let apply_binary_operation op x y =
  let trunc_integer_to_byte x =
    match x with
    | ByteConstant x -> x
    | IntConstant x -> Integers.trunc_int_to_byte x
    | LongConstant x -> Integers.trunc_long_to_byte x
    | _ -> failwith "Invalid operation"
  in
  match (op, x, y) with
  | (Instruction.Add, BoolConstant x, BoolConstant y) -> BoolConstant (x <> y)
  | (Add, ByteConstant x, ByteConstant y) -> ByteConstant (x + y)
  | (Add, IntConstant x, IntConstant y) -> IntConstant (Int32.add x y)
  | (Add, LongConstant x, LongConstant y) -> LongConstant (Int64.add x y)
  | (Sub, BoolConstant x, BoolConstant y) -> BoolConstant (x <> y)
  | (Sub, ByteConstant x, ByteConstant y) -> ByteConstant (x - y)
  | (Sub, IntConstant x, IntConstant y) -> IntConstant (Int32.sub x y)
  | (Sub, LongConstant x, LongConstant y) -> LongConstant (Int64.sub x y)
  | (Mul, BoolConstant x, BoolConstant y) -> BoolConstant (x && y)
  | (Mul, ByteConstant x, ByteConstant y) -> ByteConstant (x * y)
  | (Mul, IntConstant x, IntConstant y) -> IntConstant (Int32.mul x y)
  | (Mul, LongConstant x, LongConstant y) -> LongConstant (Int64.mul x y)
  | (Div, BoolConstant x, BoolConstant y) ->
    if not y then
      failwith "Division by zero"
    else
      BoolConstant x
  | (Div, ByteConstant x, ByteConstant y) -> ByteConstant (x / y)
  | (Div, IntConstant x, IntConstant y) -> IntConstant (Int32.div x y)
  | (Div, LongConstant x, LongConstant y) -> LongConstant (Int64.div x y)
  | (Rem, BoolConstant _, BoolConstant y) ->
    if not y then
      failwith "Division by zero"
    else
      BoolConstant false
  | (Rem, ByteConstant x, ByteConstant y) -> ByteConstant (x mod y)
  | (Rem, IntConstant x, IntConstant y) -> IntConstant (Int32.rem x y)
  | (Rem, LongConstant x, LongConstant y) -> LongConstant (Int64.rem x y)
  | (And, BoolConstant x, BoolConstant y) -> BoolConstant (x && y)
  | (And, ByteConstant x, ByteConstant y) -> ByteConstant (x land y)
  | (And, IntConstant x, IntConstant y) -> IntConstant (Int32.logand x y)
  | (And, LongConstant x, LongConstant y) -> LongConstant (Int64.logand x y)
  | (Or, BoolConstant x, BoolConstant y) -> BoolConstant (x || y)
  | (Or, ByteConstant x, ByteConstant y) -> ByteConstant (x lor y)
  | (Or, IntConstant x, IntConstant y) -> IntConstant (Int32.logor x y)
  | (Or, LongConstant x, LongConstant y) -> LongConstant (Int64.logor x y)
  | (Xor, BoolConstant x, BoolConstant y) -> BoolConstant (x <> y)
  | (Xor, ByteConstant x, ByteConstant y) -> ByteConstant (x lxor y)
  | (Xor, IntConstant x, IntConstant y) -> IntConstant (Int32.logxor x y)
  | (Xor, LongConstant x, LongConstant y) -> LongConstant (Int64.logxor x y)
  | (Shl, BoolConstant x, BoolConstant y) -> BoolConstant (x && not y)
  | (Shl, ByteConstant x, y) -> ByteConstant (x lsl trunc_integer_to_byte y)
  | (Shl, IntConstant x, y) -> IntConstant (Int32.shift_left x (trunc_integer_to_byte y))
  | (Shl, LongConstant x, y) -> LongConstant (Int64.shift_left x (trunc_integer_to_byte y))
  | (Shr, BoolConstant x, BoolConstant _) -> BoolConstant x
  | (Shr, ByteConstant x, y) -> ByteConstant (x asr trunc_integer_to_byte y)
  | (Shr, IntConstant x, y) -> IntConstant (Int32.shift_right x (trunc_integer_to_byte y))
  | (Shr, LongConstant x, y) -> LongConstant (Int64.shift_right x (trunc_integer_to_byte y))
  | (Shrl, BoolConstant x, BoolConstant y) -> BoolConstant (x && not y)
  | (Shrl, ByteConstant x, y) -> ByteConstant (Int.logand x 0xFF lsr trunc_integer_to_byte y)
  | (Shrl, IntConstant x, y) -> IntConstant (Int32.shift_right_logical x (trunc_integer_to_byte y))
  | (Shrl, LongConstant x, y) ->
    LongConstant (Int64.shift_right_logical x (trunc_integer_to_byte y))
  | _ -> failwith "Invalid operation"

let apply_conversion op x =
  match (op, x) with
  | (TruncOp Bool, IntConstant x) -> BoolConstant (Integers.trunc_int_to_bool x)
  | (TruncOp Bool, LongConstant x) -> BoolConstant (Integers.trunc_long_to_bool x)
  | (TruncOp Byte, IntConstant x) -> ByteConstant (Integers.trunc_int_to_byte x)
  | (TruncOp Byte, LongConstant x) -> ByteConstant (Integers.trunc_long_to_byte x)
  | (TruncOp Int, LongConstant x) -> IntConstant (Integers.trunc_long_to_int x)
  | (SExtOp Int, BoolConstant x) ->
    IntConstant
      ( if x then
        1l
      else
        0l )
  | (SExtOp Int, ByteConstant x) -> IntConstant (Int32.of_int x)
  | (SExtOp Long, BoolConstant x) ->
    LongConstant
      ( if x then
        1L
      else
        0L )
  | (SExtOp Long, ByteConstant x) -> LongConstant (Int64.of_int x)
  | (SExtOp Long, IntConstant x) -> LongConstant (Int64.of_int32 x)
  | _ -> failwith "Invalid operation"

let fold_constants_compare x y =
  match (x, y) with
  | (BoolConstant x, BoolConstant y) -> Bool.compare x y
  | (ByteConstant x, ByteConstant y) -> Int.compare x y
  | (IntConstant x, IntConstant y) -> Int32.compare x y
  | (LongConstant x, LongConstant y) -> Int64.compare x y
  | _ -> failwith "Invalid operation"

let folded_constants_equal c1 c2 =
  match (c1, c2) with
  | (ByteConstant i1, ByteConstant i2) -> i1 = i2
  | (IntConstant i1, IntConstant i2) -> Int32.equal i1 i2
  | (LongConstant i1, LongConstant i2) -> Int64.equal i1 i2
  | (BoolConstant b1, BoolConstant b2) -> b1 = b2
  | (FunctionConstant f1, FunctionConstant f2) -> f1 == f2
  | _ -> false

let mir_value_of_constant constant =
  let open Mir_builders in
  match constant with
  | BoolConstant b -> mk_bool_lit b
  | ByteConstant i -> mk_byte_lit i
  | IntConstant i -> mk_int_lit_of_int32 i
  | LongConstant i -> mk_long_lit i
  | FunctionConstant f -> f.value

(* Perform iterative passes to calculate folded constants for all instructions.
   Additionally prune dead branches, some of which may be exposed by constant folding. *)
class calc_constants_visitor ~program =
  object (this)
    inherit IRVisitor.t ~program

    val mutable instr_constants : folded_constant IMap.t = IMap.empty

    val mutable global_constants : folded_constant SMap.t = SMap.empty

    (* Whether a new constant was created on this pass *)
    val mutable has_new_constant = false

    (* Set of all instructions that have been removed during this run *)
    val mutable removed_instr_ids : ISet.t = ISet.empty

    method add_constant instr_id value =
      if IMap.mem instr_id instr_constants then
        ()
      else (
        instr_constants <- IMap.add instr_id value instr_constants;
        has_new_constant <- true
      )

    method add_global_constant name value =
      global_constants <- SMap.add name value global_constants;
      has_new_constant <- true;
      program.globals <- SMap.remove name program.globals

    method lookup_constant instr_id = IMap.find_opt instr_id instr_constants

    method get_instr_constants () = instr_constants

    method! run () =
      (* Fold constants until fixed point is found *)
      let rec iter () =
        visited_blocks <- BlockSet.empty;
        has_new_constant <- false;
        this#find_constant_globals ();
        this#visit_program ();
        this#fold_global_inits ();
        (* Remove pruned blocks *)
        program_iter_blocks program (fun block ->
            if not (BlockSet.mem block visited_blocks) then (
              remove_block block;
              (* Collect all removed instructions so they can be excluded from constant folding *)
              let gatherer = new Mir_normalizer.var_gatherer ~program in
              gatherer#visit_instructions ~block;
              removed_instr_ids <- ISet.union gatherer#value_ids removed_instr_ids
            ));
        if has_new_constant then
          iter ()
        else
          ()
      in
      iter ()

    method find_constant_globals () =
      SMap.iter
        (fun name global ->
          match global.Global.init_val with
          (* Skip if global cannot be constant or has already been marked as constant *)
          | None
          | Some _
            when (not global.is_constant) || SMap.mem name global_constants ->
            ()
          | Some { value = { value = Instr instr; _ }; _ } ->
            (match IMap.find_opt instr.id instr_constants with
            | Some constant -> this#add_global_constant name constant
            | None -> ())
          (* Globals initialized with constant value have constant propagated *)
          | Some { value = { value = Lit lit; _ }; _ } ->
            (match lit with
            | Bool lit -> this#add_global_constant name (BoolConstant lit)
            | Byte lit -> this#add_global_constant name (ByteConstant lit)
            | Int lit -> this#add_global_constant name (IntConstant lit)
            | Long lit -> this#add_global_constant name (LongConstant lit)
            | Function lit -> this#add_global_constant name (FunctionConstant lit)
            | _ -> ())
          | _ -> ())
        program.globals

    (* Constant folding may determine that globals are initialized to a constant. These take the
       form of stores to globals in the init function, which should be removed. *)
    method fold_global_inits () =
      match SMap.find_opt init_func_name program.funcs with
      | None -> ()
      | Some init_func ->
        let mapper =
          object (mapper)
            inherit Mir_mapper.InstructionsMapper.t ~program

            method! map_instruction instruction =
              match instruction.instr with
              | Store
                  ( { value = { value = Lit (Global global); _ }; _ },
                    { value = { value = Instr { id = instr_id; _ }; _ }; _ } ) ->
                (match IMap.find_opt instr_id instr_constants with
                | Some constant ->
                  mapper#mark_instruction_removed ();
                  if global.is_constant then
                    this#add_global_constant global.name constant
                  else
                    global_set_init ~global ~init:(Some (mir_value_of_constant constant))
                | None -> ())
              | _ -> ()
          end
        in
        mapper#map_function init_func

    method! visit_block block =
      if this#check_visited_block block then
        ()
      else (
        iter_instructions block this#visit_instruction;
        (* Check for branches that can be pruned *)
        match get_terminator block with
        | Some { instr = Continue continue; _ } -> this#visit_block continue
        | Some ({ instr = Branch { test; continue; jump }; _ } as term_instr) ->
          (* Determine whether test is a constant value *)
          let test_constant_opt =
            match test.value.value with
            | Lit (Bool lit) -> Some lit
            | Instr instr ->
              (match this#lookup_constant instr.id with
              | None -> None
              | Some (BoolConstant test) -> Some test
              | Some _ -> failwith "Expected BoolConstant")
            | _ -> None
          in
          (match test_constant_opt with
          | None ->
            this#visit_block continue;
            this#visit_block jump
          | Some test_constant ->
            (* Determine which branch should be pruned *)
            let (to_continue, to_prune) =
              if test_constant then
                (continue, jump)
              else
                (jump, continue)
            in
            (* Remove block link and set to continue to unpruned block *)
            remove_block_link block to_prune;
            remove_phi_backreferences_for_block block to_prune;
            term_instr.instr <- Continue to_continue;
            has_new_constant <- true;
            (* Only contine to remaining unpruned block *)
            this#visit_block to_continue)
        | _ -> ()
      )

    (* Visit all phi nodes and propagate constants through if possible *)
    method! visit_phi_node instr { args } =
      match this#lookup_constant instr.id with
      | Some _ -> ()
      | None ->
        (* Gather all constant args in phi node *)
        let (constants, is_constant) =
          BlockMap.fold
            (fun _ arg_val (constants, is_constant) ->
              match arg_val.Use.value.value with
              | Value.Instr instr ->
                if ISet.mem instr.id removed_instr_ids then
                  (constants, is_constant)
                else (
                  match this#lookup_constant instr.id with
                  | None -> (constants, false)
                  | Some constant -> (constant :: constants, is_constant)
                )
              | Lit (Bool b) -> (BoolConstant b :: constants, is_constant)
              | Lit (Byte b) -> (ByteConstant b :: constants, is_constant)
              | Lit (Int i) -> (IntConstant i :: constants, is_constant)
              | Lit (Long l) -> (LongConstant l :: constants, is_constant)
              | Lit (Function label) -> (FunctionConstant label :: constants, is_constant)
              | _ -> (constants, false))
            args
            ([], true)
        in
        (* If all non-removed args are the same constant, propagate constant through as value
           replacing uses of phi instruction. *)
        if is_constant && constants <> [] then
          let constant = List.hd constants in
          let other_constants = List.tl constants in
          let is_single_constant =
            List.for_all
              (fun other_constant -> folded_constants_equal constant other_constant)
              other_constants
          in
          if is_single_constant then this#add_constant instr.id constant

    method! visit_instruction _ instr =
      let get_lit_opt (use : Use.t) =
        match use.value.value with
        | Value.Lit (Bool b) -> Some (BoolConstant b)
        | Lit (Byte b) -> Some (ByteConstant b)
        | Lit (Int i) -> Some (IntConstant i)
        | Lit (Long l) -> Some (LongConstant l)
        | Lit (Function label) -> Some (FunctionConstant label)
        | Instr instr -> IMap.find_opt instr.id instr_constants
        | _ -> None
      in
      let try_fold_conversion instr_id arg op =
        match get_lit_opt arg with
        | None -> ()
        | Some arg -> this#add_constant instr_id (apply_conversion op arg)
      in
      let try_fold_comparison instr_id left right f =
        match (get_lit_opt left, get_lit_opt right) with
        | (Some left, Some right) ->
          this#add_constant instr_id (BoolConstant (f (fold_constants_compare left right) 0))
        | _ -> ()
      in
      match instr.instr with
      | Unary (op, arg) ->
        (match get_lit_opt arg with
        | None -> ()
        | Some arg -> this#add_constant instr.id (apply_unary_operation op arg))
      | Binary (op, left, right) ->
        (match (get_lit_opt left, get_lit_opt right) with
        | (Some left, Some right) ->
          this#add_constant instr.id (apply_binary_operation op left right)
        | _ -> ())
      | Cmp (cmp, left, right) ->
        let cmp_f =
          match cmp with
          | Eq -> ( == )
          | Neq -> ( <> )
          | Lt -> ( < )
          | LtEq -> ( <= )
          | Gt -> ( > )
          | GtEq -> ( >= )
        in
        try_fold_comparison instr.id left right cmp_f
      | Trunc arg -> try_fold_conversion instr.id arg (TruncOp instr.type_)
      | SExt arg -> try_fold_conversion instr.id arg (SExtOp instr.type_)
      (* Propagate global constants through pointers *)
      | Load { value = { value = Lit (Global global); _ }; _ } ->
        (match SMap.find_opt global.name global_constants with
        | None -> ()
        | Some constant -> this#add_constant instr.id constant)
      | Phi phi -> this#visit_phi_node instr phi
      | _ -> ()
  end

class update_constants_mapper ~program instr_constants =
  let var_map = IMap.map mir_value_of_constant instr_constants in
  object (this)
    inherit Mir_mapper.rewrite_vals_mapper ~program var_map as super

    (* Remove instructions that have been folded to constants *)
    method! map_instruction instr =
      if IMap.mem instr.id instr_constants then this#mark_instruction_removed ();
      super#map_instruction instr
  end

let fold_constants_and_prune ~program =
  let calc_visitor = new calc_constants_visitor ~program in
  ignore (calc_visitor#run ());
  let instr_constants = calc_visitor#get_instr_constants () in
  let update_constants_mapper = new update_constants_mapper ~program instr_constants in
  program_iter_blocks program (fun block -> update_constants_mapper#map_block block)
