open Basic_collections
open Mir
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

(* Perform iterative passes to calculate folded constants for all variables.
   Additionally prune dead branches, some of which may be exposed by constant folding. *)
class calc_constants_visitor ~ocx =
  object (this)
    inherit Ocx.IRVisitor.t ~ocx

    val mutable var_id_constants : folded_constant IMap.t = IMap.empty

    (* Whether a new constant was created on this pass *)
    val mutable has_new_constant = false

    (* Set of all blocks which had an incoming path removed, and may potentially be pruned *)
    val mutable prune_candidates : ISet.t = ISet.empty

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
              let gatherer = new Ocx.var_gatherer ~ocx in
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
        List.iter this#visit_phi_node block.phis;
        List.iter (this#visit_instruction ~block) block.instructions;
        this#visit_branch_test block;
        (* Visit the next blocks *)
        match block.next with
        | Halt -> ()
        | Continue next_block -> this#visit_block (Ocx.get_block ~ocx next_block)
        | Branch { test = _; continue; jump } ->
          this#visit_block (Ocx.get_block ~ocx continue);
          this#visit_block (Ocx.get_block ~ocx jump)
      )

    (* Visit all phi nodes and propagate constants through if possible *)
    method visit_phi_node (var_id, sources) =
      match this#lookup_constant var_id with
      | Some _ -> ()
      | None ->
        (* Gather all constant sources in phi node *)
        let (constants, is_constant) =
          List.fold_left
            (fun (constants, is_constant) source_id ->
              if ISet.mem source_id removed_vars then
                (constants, is_constant)
              else
                match this#lookup_constant source_id with
                | None -> (constants, false)
                | Some constant -> (constant :: constants, is_constant))
            ([], true)
            sources
        in
        (* If all non-removed sources are the same constant, propogate constant through
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

    method visit_branch_test block =
      (* Check for branches that can be pruned *)
      match block.next with
      | Halt
      | Continue _ ->
        ()
      | Branch { test; continue; jump } ->
        let prune_branch_from_test test =
          (* Determine which branch should be pruned *)
          let (to_continue, to_prune) =
            if test then
              (continue, jump)
            else
              (jump, continue)
          in
          (* Remove block link and set to continue to unpruned block *)
          Ocx.remove_block_link ~ocx block.id to_prune;
          prune_candidates <- ISet.add block.id prune_candidates;
          block.next <- Continue to_continue;
          has_new_constant <- true
        in
        (match test with
        | Lit lit -> prune_branch_from_test lit
        | Var var_id ->
          (match this#lookup_constant var_id with
          | None -> ()
          | Some (BoolConstant test) -> prune_branch_from_test test
          | Some _ -> failwith "Expected BoolConstant"))

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

class update_constants_mapper ~ocx var_id_constants =
  object (this)
    inherit Mir_mapper.InstructionsMapper.t ~ocx

    (* If the result is a constant the entire instruction should be removed *)
    method! map_result_variable ~block:_ ~instruction:_ var_id =
      if IMap.mem var_id var_id_constants then this#mark_instruction_removed ();
      var_id

    (* Convert constant vars to constant values in MIR *)
    method! map_bool_value ~block:_ ~instruction:_ value =
      match value with
      | Lit _ -> value
      | Var var_id ->
        (match IMap.find_opt var_id var_id_constants with
        | Some (BoolConstant const) -> Lit const
        | _ -> value)

    method! map_numeric_value ~block:_ ~instruction:_ value =
      match value with
      | IntLit _ -> value
      | IntVar var_id ->
        (match IMap.find_opt var_id var_id_constants with
        | Some (IntConstant const) -> IntLit const
        | _ -> value)
  end

let update_constants ~ocx var_id_constants =
  let mapper = new update_constants_mapper ~ocx var_id_constants in
  let all_blocks = ocx.program.blocks in
  IMap.iter
    (fun _ block ->
      let open Block in
      block.instructions <- mapper#map_instructions ~block block.instructions)
    all_blocks

let fold_constants_and_prune ~ocx =
  let calc_visitor = new calc_constants_visitor ~ocx in
  ignore (calc_visitor#run ());
  let var_id_constants = calc_visitor#get_var_id_constants () in
  update_constants ~ocx var_id_constants
