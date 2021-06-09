open Basic_collections
open Mir
open Mir.Value
open Mir_visitor
module Ocx = Mir_optimize_context

type folded_constant =
  | UnitConstant
  | ByteConstant of int
  | IntConstant of Int32.t
  | LongConstant of Int64.t
  | BoolConstant of bool
  | FunctionConstant of string

type numeric_constant_op =
  | NegOp
  | NotOp

type numeric_constants_op =
  | AddOp
  | SubOp
  | MulOp
  | DivOp
  | RemOp
  | BitAndOp
  | BitOrOp
  | BitXorOp
  | ShlOp
  | ShrOp
  | ShrlOp

let fold_numeric_constant op x =
  match (op, x) with
  | (NegOp, ByteConstant x) -> ByteConstant (-x)
  | (NegOp, IntConstant x) -> IntConstant (Int32.neg x)
  | (NegOp, LongConstant x) -> LongConstant (Int64.neg x)
  | (NotOp, ByteConstant x) -> ByteConstant (lnot x)
  | (NotOp, IntConstant x) -> IntConstant (Int32.lognot x)
  | (NotOp, LongConstant x) -> LongConstant (Int64.lognot x)
  | _ -> failwith "Invalid operation"

let fold_numeric_constants op x y =
  match (op, x, y) with
  | (AddOp, ByteConstant x, ByteConstant y) -> ByteConstant (x + y)
  | (AddOp, IntConstant x, IntConstant y) -> IntConstant (Int32.add x y)
  | (AddOp, LongConstant x, LongConstant y) -> LongConstant (Int64.add x y)
  | (SubOp, ByteConstant x, ByteConstant y) -> ByteConstant (x - y)
  | (SubOp, IntConstant x, IntConstant y) -> IntConstant (Int32.sub x y)
  | (SubOp, LongConstant x, LongConstant y) -> LongConstant (Int64.sub x y)
  | (MulOp, ByteConstant x, ByteConstant y) -> ByteConstant (x * y)
  | (MulOp, IntConstant x, IntConstant y) -> IntConstant (Int32.mul x y)
  | (MulOp, LongConstant x, LongConstant y) -> LongConstant (Int64.mul x y)
  | (DivOp, ByteConstant x, ByteConstant y) -> ByteConstant (x / y)
  | (DivOp, IntConstant x, IntConstant y) -> IntConstant (Int32.div x y)
  | (DivOp, LongConstant x, LongConstant y) -> LongConstant (Int64.div x y)
  | (RemOp, ByteConstant x, ByteConstant y) -> ByteConstant (x mod y)
  | (RemOp, IntConstant x, IntConstant y) -> IntConstant (Int32.rem x y)
  | (RemOp, LongConstant x, LongConstant y) -> LongConstant (Int64.rem x y)
  | (BitAndOp, ByteConstant x, ByteConstant y) -> ByteConstant (x land y)
  | (BitAndOp, IntConstant x, IntConstant y) -> IntConstant (Int32.logand x y)
  | (BitAndOp, LongConstant x, LongConstant y) -> LongConstant (Int64.logand x y)
  | (BitOrOp, ByteConstant x, ByteConstant y) -> ByteConstant (x lor y)
  | (BitOrOp, IntConstant x, IntConstant y) -> IntConstant (Int32.logor x y)
  | (BitOrOp, LongConstant x, LongConstant y) -> LongConstant (Int64.logor x y)
  | (BitXorOp, ByteConstant x, ByteConstant y) -> ByteConstant (x lxor y)
  | (BitXorOp, IntConstant x, IntConstant y) -> IntConstant (Int32.logxor x y)
  | (BitXorOp, LongConstant x, LongConstant y) -> LongConstant (Int64.logxor x y)
  | (ShlOp, ByteConstant x, ByteConstant y) -> ByteConstant (x lsl y)
  | (ShlOp, IntConstant x, IntConstant y) -> IntConstant (Int32.shift_left x (Int32.to_int y))
  | (ShlOp, LongConstant x, LongConstant y) -> LongConstant (Int64.shift_left x (Int64.to_int y))
  | (ShrOp, ByteConstant x, ByteConstant y) -> ByteConstant (x asr y)
  | (ShrOp, IntConstant x, IntConstant y) -> IntConstant (Int32.shift_right x (Int32.to_int y))
  | (ShrOp, LongConstant x, LongConstant y) -> LongConstant (Int64.shift_right x (Int64.to_int y))
  | (ShrlOp, ByteConstant x, ByteConstant y) -> ByteConstant (Int.logand x 0xFF lsr y)
  | (ShrlOp, IntConstant x, IntConstant y) ->
    IntConstant (Int32.shift_right_logical x (Int32.to_int y))
  | (ShrlOp, LongConstant x, LongConstant y) ->
    LongConstant (Int64.shift_right_logical x (Int64.to_int y))
  | _ -> failwith "Invalid operation"

let fold_constants_compare x y =
  match (x, y) with
  | (ByteConstant x, ByteConstant y) -> Int.compare x y
  | (IntConstant x, IntConstant y) -> Int32.compare x y
  | (LongConstant x, LongConstant y) -> Int64.compare x y
  | _ -> failwith "Invalid operation"

let folded_constants_equal c1 c2 =
  match (c1, c2) with
  | (UnitConstant, UnitConstant) -> true
  | (ByteConstant i1, ByteConstant i2) -> i1 = i2
  | (IntConstant i1, IntConstant i2) -> Int32.equal i1 i2
  | (LongConstant i1, LongConstant i2) -> Int64.equal i1 i2
  | (BoolConstant b1, BoolConstant b2) -> b1 = b2
  | (FunctionConstant s1, FunctionConstant s2) -> s1 = s2
  | _ -> false

let mir_value_of_constant constant =
  let open Value in
  match constant with
  | UnitConstant -> `UnitV (Lit ())
  | BoolConstant b -> `BoolV (Lit b)
  | ByteConstant i -> `ByteV (Lit i)
  | IntConstant i -> `IntV (Lit i)
  | LongConstant i -> `LongV (Lit i)
  | FunctionConstant f -> `FunctionV (Lit f)

(* Perform iterative passes to calculate folded constants for all variables.
   Additionally prune dead branches, some of which may be exposed by constant folding. *)
class calc_constants_visitor ~ocx =
  object (this)
    inherit IRVisitor.t ~program:ocx.Ocx.program

    val mutable var_id_constants : folded_constant IMap.t = IMap.empty

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

    method lookup_constant var_id = IMap.find_opt var_id var_id_constants

    method get_var_id_constants () = var_id_constants

    method get_removed_vars () = removed_vars

    method! run () =
      (* Fold constants until fixed point is found *)
      let rec iter () =
        visited_blocks <- ISet.empty;
        has_new_constant <- false;
        this#visit_program ();
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
            | `BoolV (Lit lit) -> Some lit
            | `BoolV (Var var_id) ->
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
            (fun _ arg_var_id (constants, is_constant) ->
              if ISet.mem arg_var_id removed_vars then
                (constants, is_constant)
              else
                match this#lookup_constant arg_var_id with
                | None -> (constants, false)
                | Some constant -> (constant :: constants, is_constant))
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
        | `BoolV (Lit lit) -> Some lit
        | `BoolV (Var var_id) ->
          (match IMap.find_opt var_id var_id_constants with
          | None -> None
          | Some (BoolConstant i) -> Some i
          | _ -> failwith "Expected bool value")
      in
      let get_numeric_lit_opt value =
        match value with
        | `ByteV (Lit lit) -> Some (ByteConstant lit)
        | `ByteV (Var var_id) ->
          (match IMap.find_opt var_id var_id_constants with
          | None -> None
          | Some (ByteConstant _ as lit) -> Some lit
          | _ -> failwith "Expected numeric value")
        | `IntV (Lit lit) -> Some (IntConstant lit)
        | `IntV (Var var_id) ->
          (match IMap.find_opt var_id var_id_constants with
          | None -> None
          | Some (IntConstant _ as lit) -> Some lit
          | _ -> failwith "Expected numeric value")
        | `LongV (Lit lit) -> Some (LongConstant lit)
        | `LongV (Var var_id) ->
          (match IMap.find_opt var_id var_id_constants with
          | None -> None
          | Some (LongConstant _ as lit) -> Some lit
          | _ -> failwith "Expected numeric value")
      in
      let get_function_lit_opt value =
        match value with
        | `FunctionV (Lit lit) -> Some lit
        | `FunctionV (Var var_id) ->
          (match IMap.find_opt var_id var_id_constants with
          | None -> None
          | Some (FunctionConstant i) -> Some i
          | _ -> failwith "Expected function value")
      in
      let try_fold_bool_constant var_id arg f =
        match get_bool_lit_opt arg with
        | None -> ()
        | Some arg -> this#add_constant var_id (BoolConstant (f arg))
      in
      let try_fold_numeric_constant var_id arg op =
        match get_numeric_lit_opt arg with
        | None -> ()
        | Some arg -> this#add_constant var_id (fold_numeric_constant op arg)
      in
      let try_fold_bool_constants var_id left right f =
        match (get_bool_lit_opt left, get_bool_lit_opt right) with
        | (Some left, Some right) -> this#add_constant var_id (BoolConstant (f left right))
        | _ -> ()
      in
      let try_fold_numeric_constants op var_id left right =
        match (get_numeric_lit_opt left, get_numeric_lit_opt right) with
        | (Some left, Some right) -> this#add_constant var_id (fold_numeric_constants op left right)
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
      | LogNot (var_id, arg) -> try_fold_bool_constant var_id arg not
      | LogAnd (var_id, left, right) -> try_fold_bool_constants var_id left right ( && )
      | LogOr (var_id, left, right) -> try_fold_bool_constants var_id left right ( || )
      | BitNot (var_id, arg) -> try_fold_numeric_constant var_id arg NotOp
      | BitAnd (var_id, left, right) -> try_fold_numeric_constants BitAndOp var_id left right
      | BitOr (var_id, left, right) -> try_fold_numeric_constants BitOrOp var_id left right
      | BitXor (var_id, left, right) -> try_fold_numeric_constants BitXorOp var_id left right
      | Shl (var_id, left, right) -> try_fold_numeric_constants ShlOp var_id left right
      | Shr (var_id, left, right) -> try_fold_numeric_constants ShrOp var_id left right
      | Shrl (var_id, left, right) -> try_fold_numeric_constants ShrlOp var_id left right
      | Eq (var_id, left, right) -> try_fold_comparison var_id left right ( == )
      | Neq (var_id, left, right) -> try_fold_comparison var_id left right ( <> )
      | Lt (var_id, left, right) -> try_fold_comparison var_id left right ( < )
      | LtEq (var_id, left, right) -> try_fold_comparison var_id left right ( <= )
      | Gt (var_id, left, right) -> try_fold_comparison var_id left right ( > )
      | GtEq (var_id, left, right) -> try_fold_comparison var_id left right ( >= )
      | Mov (var_id, value) ->
        let constant =
          match value with
          | `UnitV _ -> Some UnitConstant
          | `StringV _ -> None
          | `FunctionV _ as v -> get_function_lit_opt v |> Option.map (fun x -> FunctionConstant x)
          | `BoolV _ as v -> get_bool_lit_opt v |> Option.map (fun x -> BoolConstant x)
          | (`ByteV _ | `IntV _ | `LongV _) as v -> get_numeric_lit_opt v
          | `PointerV _ -> None
        in
        (match constant with
        | None -> ()
        | Some constant -> this#add_constant var_id constant)
      | _ -> ()
  end

class update_constants_mapper ~ocx var_id_constants constants_in_phis =
  object (this)
    inherit Mir_mapper.InstructionsMapper.t ~ocx

    (* If the result is a constant the entire instruction should be removed unless the result
       appears in non-constant phis. If the result appears in non-constant phis then the
       instruction should be replaced with a move of the constant to the result variable. *)
    method! map_result_variable ~block:_ var_id =
      (match IMap.find_opt var_id var_id_constants with
      | None -> ()
      | Some constant ->
        if ISet.mem var_id constants_in_phis then
          this#replace_instruction
            [(mk_instr_id (), Instruction.Mov (var_id, mir_value_of_constant constant))]
        else
          this#mark_instruction_removed ());
      var_id

    (* Convert constant vars to constant values in MIR *)
    method! map_bool_value ~block:_ value =
      match value with
      | `BoolV (Lit _) -> value
      | `BoolV (Var var_id) ->
        (match IMap.find_opt var_id var_id_constants with
        | Some (BoolConstant const) -> `BoolV (Lit const)
        | _ -> value)

    method! map_numeric_value ~block:_ value =
      match value with
      | `ByteV (Lit _) -> value
      | `ByteV (Var var_id) ->
        (match IMap.find_opt var_id var_id_constants with
        | Some (ByteConstant const) -> `ByteV (Lit const)
        | _ -> value)
      | `IntV (Lit _) -> value
      | `IntV (Var var_id) ->
        (match IMap.find_opt var_id var_id_constants with
        | Some (IntConstant const) -> `IntV (Lit const)
        | _ -> value)
      | `LongV (Lit _) -> value
      | `LongV (Var var_id) ->
        (match IMap.find_opt var_id var_id_constants with
        | Some (LongConstant const) -> `LongV (Lit const)
        | _ -> value)
  end

(* Find constant variables that appear in non-constant phis, as these variables will need to keep
   a definition as constants cannot be inlined into phis. *)
let find_constants_in_phis ~ocx var_id_constants removed_vars =
  let constants_in_phis = ref ISet.empty in
  IMap.iter
    (fun _ block ->
      List.iter
        (fun (_, var_id, args) ->
          if (not (IMap.mem var_id var_id_constants)) && not (ISet.mem var_id removed_vars) then
            IMap.iter
              (fun _ arg_var_id ->
                if IMap.mem arg_var_id var_id_constants && not (ISet.mem arg_var_id removed_vars)
                then
                  constants_in_phis := ISet.add arg_var_id !constants_in_phis)
              args)
        block.Block.phis)
    ocx.Ocx.program.blocks;
  !constants_in_phis

let fold_constants_and_prune ~ocx =
  let calc_visitor = new calc_constants_visitor ~ocx in
  ignore (calc_visitor#run ());
  let var_id_constants = calc_visitor#get_var_id_constants () in
  let removed_vars = calc_visitor#get_removed_vars () in
  let constants_in_phis = find_constants_in_phis ~ocx var_id_constants removed_vars in
  let update_constants_mapper =
    new update_constants_mapper ~ocx var_id_constants constants_in_phis
  in
  IMap.iter
    (fun _ block ->
      let open Block in
      (* Remove constant phis, however if the phi result variable appears in some non-constant
         phi then an instruction should inserted moving the constant to the phi result variable. *)
      block.phis <-
        List.filter
          (fun (_, var_id, _) ->
            match IMap.find_opt var_id var_id_constants with
            | None -> true
            | Some constant ->
              if ISet.mem var_id constants_in_phis then
                block.instructions <-
                  (mk_instr_id (), Mov (var_id, mir_value_of_constant constant))
                  :: block.instructions;
              false)
          block.phis;
      block.instructions <- update_constants_mapper#map_instructions ~block block.instructions)
    ocx.Ocx.program.blocks
