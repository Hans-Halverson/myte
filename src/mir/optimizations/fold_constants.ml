open Basic_collections
open Mir
open Mir_type
open Mir_visitor
module Ocx = Mir_optimize_context

type folded_constant =
  | ByteConstant of int
  | IntConstant of Int32.t
  | LongConstant of Int64.t
  | BoolConstant of bool
  | FunctionConstant of string

type conversion_op =
  | TruncOp of Type.numeric_type
  | SExtOp of Type.numeric_type

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
  | (TruncOp `BoolT, IntConstant x) -> BoolConstant (Integers.trunc_int_to_bool x)
  | (TruncOp `BoolT, LongConstant x) -> BoolConstant (Integers.trunc_long_to_bool x)
  | (TruncOp `ByteT, IntConstant x) -> ByteConstant (Integers.trunc_int_to_byte x)
  | (TruncOp `ByteT, LongConstant x) -> ByteConstant (Integers.trunc_long_to_byte x)
  | (TruncOp `IntT, LongConstant x) -> IntConstant (Integers.trunc_long_to_int x)
  | (SExtOp `IntT, BoolConstant x) ->
    IntConstant
      ( if x then
        1l
      else
        0l )
  | (SExtOp `IntT, ByteConstant x) -> IntConstant (Int32.of_int x)
  | (SExtOp `LongT, BoolConstant x) ->
    LongConstant
      ( if x then
        1L
      else
        0L )
  | (SExtOp `LongT, ByteConstant x) -> LongConstant (Int64.of_int x)
  | (SExtOp `LongT, IntConstant x) -> LongConstant (Int64.of_int32 x)
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
  | (FunctionConstant s1, FunctionConstant s2) -> s1 = s2
  | _ -> false

let mir_value_of_constant constant =
  match constant with
  | BoolConstant b -> `BoolL b
  | ByteConstant i -> `ByteL i
  | IntConstant i -> `IntL i
  | LongConstant i -> `LongL i
  | FunctionConstant f -> `FunctionL f

(* Perform iterative passes to calculate folded constants for all variables.
   Additionally prune dead branches, some of which may be exposed by constant folding. *)
class calc_constants_visitor ~ocx =
  object (this)
    inherit IRVisitor.t ~program:ocx.Ocx.program

    val mutable var_id_constants : folded_constant IMap.t = IMap.empty

    val mutable global_constants : folded_constant SMap.t = SMap.empty

    (* Whether a new constant was created on this pass *)
    val mutable has_new_constant = false

    (* Set of all variables that have been removed during this run *)
    val mutable removed_vars : ISet.t = ISet.empty

    method add_constant var_id value =
      if IMap.mem var_id var_id_constants then
        ()
      else (
        var_id_constants <- IMap.add var_id value var_id_constants;
        has_new_constant <- true
      )

    method add_global_constant name value =
      global_constants <- SMap.add name value global_constants;
      has_new_constant <- true;
      ocx.program.globals <- SMap.remove name ocx.program.globals

    method lookup_constant var_id = IMap.find_opt var_id var_id_constants

    method get_var_id_constants () = var_id_constants

    method! run () =
      (* Fold constants until fixed point is found *)
      let rec iter () =
        visited_blocks <- ISet.empty;
        has_new_constant <- false;
        this#find_constant_globals ();
        this#visit_program ();
        this#fold_global_inits ();
        (* Remove pruned blocks *)
        IMap.iter
          (fun block_id block ->
            if not (ISet.mem block_id visited_blocks) then (
              Ocx.remove_block ~ocx block_id;
              (* Collect all removed variables so they can be excluded from constant folding *)
              let gatherer = new Mir_normalizer.var_gatherer ~program:ocx.program in
              List.iter (gatherer#visit_phi_node ~block) block.phis;
              gatherer#visit_instructions ~block block.instructions;
              removed_vars <- ISet.union gatherer#vars removed_vars
            ))
          ocx.program.blocks;
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
          | Some _ when (not global.is_constant) || SMap.mem name global_constants -> ()
          | None -> ()
          | Some init_val ->
            (match var_id_of_value_opt init_val with
            | Some var_id ->
              (match IMap.find_opt var_id var_id_constants with
              | Some constant -> this#add_global_constant name constant
              | None -> ())
            (* Globals initialized with constant value have constant propagated *)
            | None ->
              (match init_val with
              | `BoolL lit -> this#add_global_constant name (BoolConstant lit)
              | `ByteL lit -> this#add_global_constant name (ByteConstant lit)
              | `IntL lit -> this#add_global_constant name (IntConstant lit)
              | `LongL lit -> this#add_global_constant name (LongConstant lit)
              | `FunctionL lit -> this#add_global_constant name (FunctionConstant lit)
              | _ -> ())))
        ocx.program.globals

    (* Constant folding may determine that globals are initialized to a constant. These take the
       form of stores to globals in the init function, which should be removed. *)
    method fold_global_inits () =
      match SMap.find_opt init_func_name ocx.Ocx.program.funcs with
      | None -> ()
      | Some init_func ->
        let mapper =
          object (mapper)
            inherit Mir_mapper.InstructionsMapper.t ~program:ocx.Ocx.program

            method! map_instruction ~block:_ ((_, instr) as instruction) =
              (match instr with
              | Store (`PointerL (_, name), value) ->
                (match (SMap.find_opt name ocx.program.globals, var_id_of_value_opt value) with
                | (Some global, Some var_id) ->
                  (match IMap.find_opt var_id var_id_constants with
                  | Some constant ->
                    mapper#mark_instruction_removed ();
                    if global.is_constant then
                      this#add_global_constant name constant
                    else
                      global.init_val <- Some (mir_value_of_constant constant)
                  | None -> ())
                | _ -> ())
              | _ -> ());
              [instruction]
          end
        in
        mapper#map_function init_func

    method! visit_block block =
      if this#check_visited_block block.id then
        ()
      else (
        List.iter (this#visit_phi_node ~block) block.phis;
        List.iter (this#visit_instruction ~block) block.instructions;
        (* Check for branches that can be pruned *)
        match block.next with
        | Halt -> ()
        | Continue continue -> this#visit_block (Ocx.get_block ~ocx continue)
        | Branch { test; continue; jump } ->
          (* Determine whether test is a constant value *)
          let test_constant_opt =
            match test with
            | `BoolL lit -> Some lit
            | `BoolV var_id ->
              (match this#lookup_constant var_id with
              | None -> None
              | Some (BoolConstant test) -> Some test
              | Some _ -> failwith "Expected BoolConstant")
          in
          (match test_constant_opt with
          | None ->
            this#visit_block (Ocx.get_block ~ocx continue);
            this#visit_block (Ocx.get_block ~ocx jump)
          | Some test_constant ->
            (* Determine which branch should be pruned *)
            let (to_continue, to_prune) =
              if test_constant then
                (continue, jump)
              else
                (jump, continue)
            in
            (* Remove block link and set to continue to unpruned block *)
            Ocx.remove_block_link ~ocx block.id to_prune;
            Ocx.remove_phi_backreferences_for_block ~ocx block.id to_prune;
            block.next <- Continue to_continue;
            has_new_constant <- true;
            (* Only contine to remaining unpruned block *)
            this#visit_block (Ocx.get_block ~ocx to_continue))
      )

    (* Visit all phi nodes and propagate constants through if possible *)
    method! visit_phi_node ~block:_ (_, var_id, args) =
      match this#lookup_constant var_id with
      | Some _ -> ()
      | None ->
        (* Gather all constant args in phi node *)
        let (constants, is_constant) =
          IMap.fold
            (fun _ arg_val (constants, is_constant) ->
              match arg_val with
              | `ByteV var_id
              | `IntV var_id
              | `LongV var_id
              | `BoolV var_id
              | `FunctionV var_id ->
                if ISet.mem var_id removed_vars then
                  (constants, is_constant)
                else (
                  match this#lookup_constant var_id with
                  | None -> (constants, false)
                  | Some constant -> (constant :: constants, is_constant)
                )
              | `BoolL b -> (BoolConstant b :: constants, is_constant)
              | `ByteL b -> (ByteConstant b :: constants, is_constant)
              | `IntL i -> (IntConstant i :: constants, is_constant)
              | `LongL l -> (LongConstant l :: constants, is_constant)
              | `FunctionL label -> (FunctionConstant label :: constants, is_constant)
              | `PointerL _
              | `PointerV _
              | `ArrayStringL _
              | `ArrayVtableL _
              | `ArrayV _ ->
                (constants, false))
            args
            ([], true)
        in
        (* If all non-removed args are the same constant, propagate constant through
           to result variable *)
        if is_constant && constants <> [] then
          let constant = List.hd constants in
          let other_constants = List.tl constants in
          let is_single_constant =
            List.for_all
              (fun other_constant -> folded_constants_equal constant other_constant)
              other_constants
          in
          if is_single_constant then this#add_constant var_id constant

    method! visit_instruction ~block:_ instruction =
      let get_bool_lit_opt value =
        match value with
        | `BoolL lit -> Some (BoolConstant lit)
        | `BoolV var_id ->
          (match IMap.find_opt var_id var_id_constants with
          | None -> None
          | Some (BoolConstant _ as lit) -> Some lit
          | _ -> failwith "Expected bool value")
      in
      let get_numeric_lit_opt value =
        match value with
        | (`BoolL _ | `BoolV _) as value -> get_bool_lit_opt value
        | `ByteL lit -> Some (ByteConstant lit)
        | `ByteV var_id ->
          (match IMap.find_opt var_id var_id_constants with
          | None -> None
          | Some (ByteConstant _ as lit) -> Some lit
          | _ -> failwith "Expected numeric value")
        | `IntL lit -> Some (IntConstant lit)
        | `IntV var_id ->
          (match IMap.find_opt var_id var_id_constants with
          | None -> None
          | Some (IntConstant _ as lit) -> Some lit
          | _ -> failwith "Expected numeric value")
        | `LongL lit -> Some (LongConstant lit)
        | `LongV var_id ->
          (match IMap.find_opt var_id var_id_constants with
          | None -> None
          | Some (LongConstant _ as lit) -> Some lit
          | _ -> failwith "Expected numeric value")
      in
      let get_comparable_lit_opt value =
        match value with
        | (`ByteL _ | `ByteV _ | `IntL _ | `IntV _ | `LongL _ | `LongV _) as value ->
          get_numeric_lit_opt value
        | (`BoolL _ | `BoolV _) as value -> get_bool_lit_opt value
        | `PointerL _
        | `PointerV _ ->
          None
      in
      let try_fold_conversion var_id arg op =
        match get_numeric_lit_opt arg with
        | None -> ()
        | Some arg -> this#add_constant var_id (apply_conversion op arg)
      in
      let try_fold_comparison var_id left right f =
        match (get_comparable_lit_opt left, get_comparable_lit_opt right) with
        | (Some left, Some right) ->
          this#add_constant var_id (BoolConstant (f (fold_constants_compare left right) 0))
        | _ -> ()
      in
      match snd instruction with
      | Unary (op, var_id, arg) ->
        (match get_numeric_lit_opt arg with
        | None -> ()
        | Some arg -> this#add_constant var_id (apply_unary_operation op arg))
      | Binary (op, var_id, left, right) ->
        (match (get_numeric_lit_opt left, get_numeric_lit_opt right) with
        | (Some left, Some right) -> this#add_constant var_id (apply_binary_operation op left right)
        | _ -> ())
      | Cmp (cmp, var_id, left, right) ->
        let cmp_f =
          match cmp with
          | Eq -> ( == )
          | Neq -> ( <> )
          | Lt -> ( < )
          | LtEq -> ( <= )
          | Gt -> ( > )
          | GtEq -> ( >= )
        in
        try_fold_comparison var_id left right cmp_f
      | Trunc (var_id, arg, ty) -> try_fold_conversion var_id arg (TruncOp ty)
      | SExt (var_id, arg, ty) -> try_fold_conversion var_id arg (SExtOp ty)
      (* Propagate global constants through pointers *)
      | Load (var_id, `PointerL (_, label)) ->
        (match SMap.find_opt label global_constants with
        | None -> ()
        | Some constant -> this#add_constant var_id constant)
      | _ -> ()
  end

class update_constants_mapper ~ocx var_id_constants =
  let var_map = IMap.map mir_value_of_constant var_id_constants in
  object (this)
    inherit Mir_mapper.rewrite_vars_mapper ~program:ocx.Ocx.program var_map

    (* If the result is a constant the entire instruction should be removed unless the result
       appears in non-constant phis. If the result appears in non-constant phis then the
       instruction should be replaced with a move of the constant to the result variable. *)
    method! map_result_variable ~block:_ var_id =
      if IMap.mem var_id var_id_constants then this#mark_instruction_removed ();
      var_id
  end

let fold_constants_and_prune ~ocx =
  let calc_visitor = new calc_constants_visitor ~ocx in
  ignore (calc_visitor#run ());
  let var_id_constants = calc_visitor#get_var_id_constants () in
  let update_constants_mapper = new update_constants_mapper ~ocx var_id_constants in
  IMap.iter (fun _ block -> update_constants_mapper#map_block block) ocx.Ocx.program.blocks
