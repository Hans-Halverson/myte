open Basic_collections
open Mir
open Mir_visitor
module Ocx = Mir_optimize_context

type folded_constant =
  | UnitConstant
  | IntConstant of Int64.t
  | BoolConstant of bool
  | FunctionConstant of string

let folded_constants_equal c1 c2 =
  match (c1, c2) with
  | (UnitConstant, UnitConstant) -> true
  | (IntConstant i1, IntConstant i2) -> Int64.equal i1 i2
  | (BoolConstant b1, BoolConstant b2) -> b1 = b2
  | (FunctionConstant s1, FunctionConstant s2) -> s1 = s2
  | _ -> false

let mir_value_of_constant constant =
  let open Instruction in
  let open Value in
  match constant with
  | UnitConstant -> Unit UnitValue.Lit
  | BoolConstant b -> Bool (BoolValue.Lit b)
  | IntConstant i -> Numeric (NumericValue.IntLit i)
  | FunctionConstant f -> Function (FunctionValue.Lit f)

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
            | Lit lit -> Some lit
            | Var var_id ->
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
      let open Instruction in
      let get_bool_lit_opt value =
        let open BoolValue in
        match value with
        | Lit lit -> Some lit
        | Var var_id ->
          (match IMap.find_opt var_id var_id_constants with
          | None -> None
          | Some (BoolConstant i) -> Some i
          | _ -> failwith "Expected bool value")
      in
      let get_numeric_lit_opt value =
        let open NumericValue in
        match value with
        | IntLit lit -> Some lit
        | IntVar var_id ->
          (match IMap.find_opt var_id var_id_constants with
          | None -> None
          | Some (IntConstant i) -> Some i
          | _ -> failwith "Expected numeric value")
      in
      let get_function_lit_opt value =
        let open FunctionValue in
        match value with
        | Lit lit -> Some lit
        | Var var_id ->
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
      let try_fold_numeric_constant var_id arg f =
        match get_numeric_lit_opt arg with
        | None -> ()
        | Some arg -> this#add_constant var_id (IntConstant (f arg))
      in
      let try_fold_bool_constants var_id left right f =
        match (get_bool_lit_opt left, get_bool_lit_opt right) with
        | (Some left, Some right) -> this#add_constant var_id (BoolConstant (f left right))
        | _ -> ()
      in
      let try_fold_numeric_constants var_id left right f =
        match (get_numeric_lit_opt left, get_numeric_lit_opt right) with
        | (Some left, Some right) -> this#add_constant var_id (IntConstant (f left right))
        | _ -> ()
      in
      let try_fold_comparison var_id left right f =
        match (get_numeric_lit_opt left, get_numeric_lit_opt right) with
        | (Some left, Some right) ->
          this#add_constant var_id (BoolConstant (f (Int64.compare left right) 0))
        | _ -> ()
      in
      match snd instruction with
      | Neg (var_id, arg) -> try_fold_numeric_constant var_id arg Int64.neg
      | Add (var_id, left, right) -> try_fold_numeric_constants var_id left right Int64.add
      | Sub (var_id, left, right) -> try_fold_numeric_constants var_id left right Int64.sub
      | Mul (var_id, left, right) -> try_fold_numeric_constants var_id left right Int64.mul
      | Div (var_id, left, right) -> try_fold_numeric_constants var_id left right Int64.div
      | LogNot (var_id, arg) -> try_fold_bool_constant var_id arg not
      | LogAnd (var_id, left, right) -> try_fold_bool_constants var_id left right ( && )
      | LogOr (var_id, left, right) -> try_fold_bool_constants var_id left right ( || )
      | Eq (var_id, left, right) -> try_fold_comparison var_id left right ( == )
      | Neq (var_id, left, right) -> try_fold_comparison var_id left right ( <> )
      | Lt (var_id, left, right) -> try_fold_comparison var_id left right ( < )
      | LtEq (var_id, left, right) -> try_fold_comparison var_id left right ( <= )
      | Gt (var_id, left, right) -> try_fold_comparison var_id left right ( > )
      | GtEq (var_id, left, right) -> try_fold_comparison var_id left right ( >= )
      | Mov (var_id, value) ->
        let open Instruction.Value in
        let constant =
          match value with
          | Unit _ -> Some UnitConstant
          | String _ -> None
          | Function v -> get_function_lit_opt v |> Option.map (fun x -> FunctionConstant x)
          | Bool v -> get_bool_lit_opt v |> Option.map (fun x -> BoolConstant x)
          | Numeric v -> get_numeric_lit_opt v |> Option.map (fun x -> IntConstant x)
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
      | Lit _ -> value
      | Var var_id ->
        (match IMap.find_opt var_id var_id_constants with
        | Some (BoolConstant const) -> Lit const
        | _ -> value)

    method! map_numeric_value ~block:_ value =
      match value with
      | IntLit _ -> value
      | IntVar var_id ->
        (match IMap.find_opt var_id var_id_constants with
        | Some (IntConstant const) -> IntLit const
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
