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

type bool_constant_op = LogNotOp

type numeric_constant_op =
  | NegOp
  | NotOp
  | TruncOp of Type.numeric_type
  | SExtOp of Type.numeric_type

type numeric_constants_op =
  | AddOp
  | SubOp
  | MulOp
  | DivOp
  | RemOp
  | AndOp
  | OrOp
  | XorOp
  | ShlOp
  | ShrOp
  | ShrlOp

let fold_bool_constant op x =
  match (op, x) with
  | (LogNotOp, BoolConstant x) -> BoolConstant (not x)
  | _ -> failwith "Invalid operation"

let fold_bool_constants op x y =
  match (op, x, y) with
  | _ -> failwith "Invalid operation"

let fold_numeric_constant op x =
  match (op, x) with
  | (NegOp, ByteConstant x) -> ByteConstant (-x)
  | (NegOp, IntConstant x) -> IntConstant (Int32.neg x)
  | (NegOp, LongConstant x) -> LongConstant (Int64.neg x)
  | (NotOp, ByteConstant x) -> ByteConstant (lnot x)
  | (NotOp, IntConstant x) -> IntConstant (Int32.lognot x)
  | (NotOp, LongConstant x) -> LongConstant (Int64.lognot x)
  | (TruncOp `ByteT, IntConstant x) -> ByteConstant (Integers.trunc_int_to_byte x)
  | (TruncOp `ByteT, LongConstant x) -> ByteConstant (Integers.trunc_long_to_byte x)
  | (TruncOp `IntT, LongConstant x) -> IntConstant (Integers.trunc_long_to_int x)
  | (SExtOp `IntT, ByteConstant x) -> IntConstant (Int32.of_int x)
  | (SExtOp `LongT, ByteConstant x) -> LongConstant (Int64.of_int x)
  | (SExtOp `LongT, IntConstant x) -> LongConstant (Int64.of_int32 x)
  | _ -> failwith "Invalid operation"

let fold_numeric_constants op x y =
  let trunc_integer_to_byte x =
    match x with
    | ByteConstant x -> x
    | IntConstant x -> Integers.trunc_int_to_byte x
    | LongConstant x -> Integers.trunc_long_to_byte x
    | _ -> failwith "Invalid operation"
  in
  match (op, x, y) with
  | (AddOp, BoolConstant x, BoolConstant y) -> BoolConstant (x <> y)
  | (AddOp, ByteConstant x, ByteConstant y) -> ByteConstant (x + y)
  | (AddOp, IntConstant x, IntConstant y) -> IntConstant (Int32.add x y)
  | (AddOp, LongConstant x, LongConstant y) -> LongConstant (Int64.add x y)
  | (SubOp, BoolConstant x, BoolConstant y) -> BoolConstant (x <> y)
  | (SubOp, ByteConstant x, ByteConstant y) -> ByteConstant (x - y)
  | (SubOp, IntConstant x, IntConstant y) -> IntConstant (Int32.sub x y)
  | (SubOp, LongConstant x, LongConstant y) -> LongConstant (Int64.sub x y)
  | (MulOp, BoolConstant x, BoolConstant y) -> BoolConstant (x && y)
  | (MulOp, ByteConstant x, ByteConstant y) -> ByteConstant (x * y)
  | (MulOp, IntConstant x, IntConstant y) -> IntConstant (Int32.mul x y)
  | (MulOp, LongConstant x, LongConstant y) -> LongConstant (Int64.mul x y)
  | (DivOp, BoolConstant x, BoolConstant y) ->
    if not y then
      failwith "Division by zero"
    else
      BoolConstant x
  | (DivOp, ByteConstant x, ByteConstant y) -> ByteConstant (x / y)
  | (DivOp, IntConstant x, IntConstant y) -> IntConstant (Int32.div x y)
  | (DivOp, LongConstant x, LongConstant y) -> LongConstant (Int64.div x y)
  | (RemOp, BoolConstant _, BoolConstant y) ->
    if not y then
      failwith "Division by zero"
    else
      BoolConstant false
  | (RemOp, ByteConstant x, ByteConstant y) -> ByteConstant (x mod y)
  | (RemOp, IntConstant x, IntConstant y) -> IntConstant (Int32.rem x y)
  | (RemOp, LongConstant x, LongConstant y) -> LongConstant (Int64.rem x y)
  | (AndOp, BoolConstant x, BoolConstant y) -> BoolConstant (x && y)
  | (AndOp, ByteConstant x, ByteConstant y) -> ByteConstant (x land y)
  | (AndOp, IntConstant x, IntConstant y) -> IntConstant (Int32.logand x y)
  | (AndOp, LongConstant x, LongConstant y) -> LongConstant (Int64.logand x y)
  | (OrOp, BoolConstant x, BoolConstant y) -> BoolConstant (x || y)
  | (OrOp, ByteConstant x, ByteConstant y) -> ByteConstant (x lor y)
  | (OrOp, IntConstant x, IntConstant y) -> IntConstant (Int32.logor x y)
  | (OrOp, LongConstant x, LongConstant y) -> LongConstant (Int64.logor x y)
  | (XorOp, BoolConstant x, BoolConstant y) -> BoolConstant (x <> y)
  | (XorOp, ByteConstant x, ByteConstant y) -> ByteConstant (x lxor y)
  | (XorOp, IntConstant x, IntConstant y) -> IntConstant (Int32.logxor x y)
  | (XorOp, LongConstant x, LongConstant y) -> LongConstant (Int64.logxor x y)
  | (ShlOp, BoolConstant x, BoolConstant y) -> BoolConstant (x && not y)
  | (ShlOp, ByteConstant x, y) -> ByteConstant (x lsl trunc_integer_to_byte y)
  | (ShlOp, IntConstant x, y) -> IntConstant (Int32.shift_left x (trunc_integer_to_byte y))
  | (ShlOp, LongConstant x, y) -> LongConstant (Int64.shift_left x (trunc_integer_to_byte y))
  | (ShrOp, BoolConstant x, BoolConstant _) -> BoolConstant x
  | (ShrOp, ByteConstant x, y) -> ByteConstant (x asr trunc_integer_to_byte y)
  | (ShrOp, IntConstant x, y) -> IntConstant (Int32.shift_right x (trunc_integer_to_byte y))
  | (ShrOp, LongConstant x, y) -> LongConstant (Int64.shift_right x (trunc_integer_to_byte y))
  | (ShrlOp, BoolConstant x, BoolConstant y) -> BoolConstant (x && not y)
  | (ShrlOp, ByteConstant x, y) -> ByteConstant (Int.logand x 0xFF lsr trunc_integer_to_byte y)
  | (ShrlOp, IntConstant x, y) ->
    IntConstant (Int32.shift_right_logical x (trunc_integer_to_byte y))
  | (ShrlOp, LongConstant x, y) ->
    LongConstant (Int64.shift_right_logical x (trunc_integer_to_byte y))
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
      let try_fold_bool_constant var_id arg op =
        match get_bool_lit_opt arg with
        | None -> ()
        | Some arg -> this#add_constant var_id (fold_bool_constant op arg)
      in
      let try_fold_numeric_constant var_id arg op =
        match get_numeric_lit_opt arg with
        | None -> ()
        | Some arg -> this#add_constant var_id (fold_numeric_constant op arg)
      in
      let try_fold_numeric_constants op var_id left right =
        match (get_numeric_lit_opt left, get_numeric_lit_opt right) with
        | (Some left, Some right) -> this#add_constant var_id (fold_numeric_constants op left right)
        | _ -> ()
      in
      let try_fold_equatable var_id left right f =
        match (get_comparable_lit_opt left, get_comparable_lit_opt right) with
        | (Some left, Some right) ->
          this#add_constant var_id (BoolConstant (f (fold_constants_compare left right) 0))
        | _ -> ()
      in
      let try_fold_comparison var_id left right f =
        match (get_numeric_lit_opt left, get_numeric_lit_opt right) with
        | (Some left, Some right) ->
          this#add_constant var_id (BoolConstant (f (fold_constants_compare left right) 0))
        | _ -> ()
      in
      match snd instruction with
      | Neg (var_id, arg) -> try_fold_numeric_constant var_id arg NegOp
      | Add (var_id, left, right) -> try_fold_numeric_constants AddOp var_id left right
      | Sub (var_id, left, right) -> try_fold_numeric_constants SubOp var_id left right
      | Mul (var_id, left, right) -> try_fold_numeric_constants MulOp var_id left right
      | Div (var_id, left, right) -> try_fold_numeric_constants DivOp var_id left right
      | Rem (var_id, left, right) -> try_fold_numeric_constants RemOp var_id left right
      | LogNot (var_id, arg) -> try_fold_bool_constant var_id arg LogNotOp
      | BitNot (var_id, arg) -> try_fold_numeric_constant var_id arg NotOp
      | And (var_id, left, right) -> try_fold_numeric_constants AndOp var_id left right
      | Or (var_id, left, right) -> try_fold_numeric_constants OrOp var_id left right
      | Xor (var_id, left, right) -> try_fold_numeric_constants XorOp var_id left right
      | Shl (var_id, left, right) -> try_fold_numeric_constants ShlOp var_id left right
      | Shr (var_id, left, right) -> try_fold_numeric_constants ShrOp var_id left right
      | Shrl (var_id, left, right) -> try_fold_numeric_constants ShrlOp var_id left right
      | Eq (var_id, left, right) -> try_fold_equatable var_id left right ( == )
      | Neq (var_id, left, right) -> try_fold_equatable var_id left right ( <> )
      | Lt (var_id, left, right) -> try_fold_comparison var_id left right ( < )
      | LtEq (var_id, left, right) -> try_fold_comparison var_id left right ( <= )
      | Gt (var_id, left, right) -> try_fold_comparison var_id left right ( > )
      | GtEq (var_id, left, right) -> try_fold_comparison var_id left right ( >= )
      | Trunc (var_id, arg, ty) -> try_fold_numeric_constant var_id arg (TruncOp ty)
      | SExt (var_id, arg, ty) -> try_fold_numeric_constant var_id arg (SExtOp ty)
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
