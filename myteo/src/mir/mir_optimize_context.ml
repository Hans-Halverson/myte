open Basic_collections
open Mir

type t = {
  (* The program we are optimizing, is internally mutable *)
  program: ssa_program;
  (* Block id to the blocks it jumps to *)
  mutable next_blocks: ISet.t IMap.t;
  (* Block id to the previous blocks that jump to it *)
  mutable prev_blocks: ISet.t IMap.t;
  (* Variable id to the set of blocks that use it.
     Not updated on block or variable deletions! *)
  mutable var_use_blocks: ISet.t IMap.t;
}

let get_block ~ocx block_id = IMap.find block_id ocx.program.blocks

let add_block_link ~ocx prev_block next_block =
  let add_to_multimap key value mmap =
    let new_values =
      match IMap.find_opt key mmap with
      | None -> ISet.singleton value
      | Some values -> ISet.add value values
    in
    IMap.add key new_values mmap
  in
  ocx.next_blocks <- add_to_multimap prev_block next_block ocx.next_blocks;
  ocx.prev_blocks <- add_to_multimap next_block prev_block ocx.prev_blocks

let remove_block_link ~ocx prev_block next_block =
  let remove_from_multimap key value mmap =
    match IMap.find_opt key mmap with
    | None -> mmap
    | Some values ->
      if ISet.cardinal values = 1 then
        IMap.remove key mmap
      else
        IMap.add key (ISet.remove value values) mmap
  in
  ocx.next_blocks <- remove_from_multimap prev_block next_block ocx.next_blocks;
  ocx.prev_blocks <- remove_from_multimap next_block prev_block ocx.prev_blocks

let is_empty_block (block : var_id Block.t) =
  block.instructions = []
  && block.phis = []
  &&
  match block.next with
  | Halt -> true
  | Continue _ -> true
  | Branch _ -> false

let remove_block ~ocx block_id =
  let remove_from_multimap key value mmap =
    match IMap.find_opt key mmap with
    | None -> mmap
    | Some values ->
      if ISet.cardinal values = 1 then
        IMap.remove key mmap
      else
        IMap.add key (ISet.remove value values) mmap
  in
  let block = get_block ~ocx block_id in
  (match block.source with
  | GlobalInit name ->
    let global = SMap.find name ocx.program.globals in
    global.init <- List.filter (( != ) block_id) global.init
  | FunctionBody name ->
    let func = SMap.find name ocx.program.funcs in
    func.body <- List.filter (( != ) block_id) func.body);
  (match IMap.find_opt block_id ocx.prev_blocks with
  | None -> ()
  | Some prev_blocks ->
    ISet.iter
      (fun prev_block_id ->
        let prev_block = get_block ~ocx prev_block_id in
        match block.next with
        | Halt -> prev_block.next <- Halt
        | Continue next_id ->
          let map_id id =
            if id = block_id then (
              add_block_link ~ocx prev_block_id next_id;
              next_id
            ) else
              id
          in
          prev_block.next <-
            (match prev_block.next with
            | Halt -> Halt
            | Continue id -> Continue (map_id id)
            | Branch { test; continue; jump } ->
              let new_continue = map_id continue in
              let new_jump = map_id jump in
              (* If both branches points to same label convert to continue *)
              if new_continue = new_jump then
                Continue new_continue
              else
                (* Otherwise create branch to new block *)
                Branch { test; continue = new_continue; jump = new_jump })
        | Branch _ -> ())
      prev_blocks);
  ocx.program.blocks <- IMap.remove block_id ocx.program.blocks;
  (match IMap.find_opt block_id ocx.next_blocks with
  | None -> ()
  | Some next_blocks ->
    (* Remove prev pointers from next blocks to this removed block *)
    ISet.iter
      (fun next_block_id ->
        ocx.prev_blocks <- remove_from_multimap next_block_id block_id ocx.prev_blocks)
      next_blocks;
    ocx.next_blocks <- IMap.remove block_id ocx.next_blocks);
  match IMap.find_opt block_id ocx.prev_blocks with
  | None -> ()
  | Some prev_blocks ->
    (* Remove next pointers from prev blocks to this removed block *)
    ISet.iter
      (fun prev_block_id ->
        ocx.next_blocks <- remove_from_multimap prev_block_id block_id ocx.next_blocks)
      prev_blocks;
    ocx.prev_blocks <- IMap.remove block_id ocx.prev_blocks

module IRVisitor = struct
  class t ~ocx =
    object (this)
      val program : ssa_program = ocx.program

      val mutable visited_blocks : ISet.t = ISet.empty

      val mutable constant_vars : var_id Instruction.Value.t IMap.t = IMap.empty

      method check_visited_block block_id =
        if ISet.mem block_id visited_blocks then
          true
        else (
          visited_blocks <- ISet.add block_id visited_blocks;
          false
        )

      method reset_visited_blocks () = visited_blocks <- ISet.empty

      method run () =
        this#visit_program ();
        this#on_complete ()

      method on_complete () = ()

      method visit_program () =
        SMap.iter (fun _ global -> this#visit_global global) program.globals;
        SMap.iter (fun _ func -> this#visit_function func) program.funcs

      method visit_global global =
        let block = get_block ~ocx (List.hd global.init) in
        this#visit_block block

      method visit_function func =
        let block = get_block ~ocx (List.hd func.body) in
        this#visit_block block

      method visit_block (block : var_id Block.t) =
        if this#check_visited_block block.id then
          ()
        else (
          this#visit_instructions ~block block.instructions;
          this#visit_next block.next
        )

      method visit_next next =
        match next with
        | Halt -> ()
        | Continue id -> this#visit_block (get_block ~ocx id)
        | Branch { continue; jump; _ } ->
          this#visit_block (get_block ~ocx continue);
          this#visit_block (get_block ~ocx jump)

      method visit_instructions ~block instructions =
        List.iter (this#visit_instruction ~block) instructions

      method visit_instruction ~block ((_, instr) as instruction) =
        match instr with
        | Mov (result, arg) ->
          this#visit_result_variable ~block ~instruction result;
          this#visit_value ~block ~instruction arg
        | Call (ret, func, args) ->
          this#visit_result_variable ~block ~instruction ret;
          this#visit_function_value ~block ~instruction func;
          List.iter (this#visit_value ~block ~instruction) args
        | Ret arg_opt -> Option.iter (this#visit_value ~block ~instruction) arg_opt
        | LoadGlobal (result, _name) -> this#visit_result_variable ~block ~instruction result
        | StoreGlobal (_name, arg) -> this#visit_value ~block ~instruction arg
        | LogNot (result, arg) ->
          this#visit_result_variable ~block ~instruction result;
          this#visit_bool_value ~block ~instruction arg
        | LogAnd (result, left, right)
        | LogOr (result, left, right) ->
          this#visit_result_variable ~block ~instruction result;
          this#visit_bool_value ~block ~instruction left;
          this#visit_bool_value ~block ~instruction right
        | Neg (result, arg) ->
          this#visit_result_variable ~block ~instruction result;
          this#visit_numeric_value ~block ~instruction arg
        | Add (result, left, right)
        | Sub (result, left, right)
        | Mul (result, left, right)
        | Div (result, left, right)
        | Eq (result, left, right)
        | Neq (result, left, right)
        | Lt (result, left, right)
        | LtEq (result, left, right)
        | Gt (result, left, right)
        | GtEq (result, left, right) ->
          this#visit_result_variable ~block ~instruction result;
          this#visit_numeric_value ~block ~instruction left;
          this#visit_numeric_value ~block ~instruction right

      method visit_result_variable ~block:_ ~instruction:_ _var_id = ()

      method visit_use_variable ~block:_ ~instruction:_ _var_id = ()

      method visit_value ~block ~instruction value =
        match value with
        | Unit value -> this#visit_unit_value ~block ~instruction value
        | Bool value -> this#visit_bool_value ~block ~instruction value
        | String value -> this#visit_string_value ~block ~instruction value
        | Numeric value -> this#visit_numeric_value ~block ~instruction value
        | Function value -> this#visit_function_value ~block ~instruction value

      method visit_unit_value ~block ~instruction value =
        match value with
        | Lit -> ()
        | Var var_id -> this#visit_use_variable ~block ~instruction var_id

      method visit_bool_value ~block ~instruction value =
        match value with
        | Lit _ -> ()
        | Var var_id -> this#visit_use_variable ~block ~instruction var_id

      method visit_string_value ~block ~instruction value =
        match value with
        | Lit _ -> ()
        | Var var_id -> this#visit_use_variable ~block ~instruction var_id

      method visit_numeric_value ~block ~instruction value =
        match value with
        | IntLit _ -> ()
        | IntVar var_id -> this#visit_use_variable ~block ~instruction var_id

      method visit_function_value ~block ~instruction value =
        match value with
        | Lit _ -> ()
        | Var var_id -> this#visit_use_variable ~block ~instruction var_id
    end
end

class init_visitor ~ocx =
  object (this)
    inherit IRVisitor.t ~ocx

    method! visit_block block =
      if this#check_visited_block block.id then
        ()
      else (
        if not (IMap.mem block.id ocx.next_blocks) then
          ocx.next_blocks <- IMap.add block.id ISet.empty ocx.next_blocks;
        if not (IMap.mem block.id ocx.prev_blocks) then
          ocx.prev_blocks <- IMap.add block.id ISet.empty ocx.prev_blocks;
        List.iter (this#visit_instruction ~block) block.instructions;
        (match block.next with
        | Halt -> ()
        | Continue next -> add_block_link ~ocx block.id next
        | Branch { test; continue; jump } ->
          (match test with
          | Lit _ -> ()
          | Var var_id -> this#add_use_variable ~block var_id);
          add_block_link ~ocx block.id continue;
          add_block_link ~ocx block.id jump);
        this#visit_next block.next
      )

    method! visit_use_variable ~block ~instruction:_ var_id = this#add_use_variable ~block var_id

    method add_use_variable ~block var_id =
      let new_blocks =
        match IMap.find_opt var_id ocx.var_use_blocks with
        | None -> ISet.singleton block.id
        | Some blocks -> ISet.add block.id blocks
      in
      ocx.var_use_blocks <- IMap.add var_id new_blocks ocx.var_use_blocks
  end

let mk program =
  let ocx =
    { program; next_blocks = IMap.empty; prev_blocks = IMap.empty; var_use_blocks = IMap.empty }
  in
  let init_visitor = new init_visitor ~ocx in
  init_visitor#run ();
  ocx

class var_gatherer ~ocx =
  object
    inherit IRVisitor.t ~ocx

    val mutable vars : ISet.t = ISet.empty

    method vars = vars

    method! visit_result_variable ~block:_ ~instruction:_ var_id = vars <- ISet.add var_id vars
  end

let normalize ~ocx =
  let open Block in
  (* Gather all vars defined in program *)
  let gatherer = new var_gatherer ~ocx in
  gatherer#run ();
  let vars = gatherer#vars in
  (* Update and potentially prune phi nodes with missing vars *)
  IMap.iter
    (fun _ block ->
      block.phis <-
        List.filter_map
          (fun (dest, sources) ->
            let new_sources =
              List.filter_map
                (fun source ->
                  if ISet.mem source vars then
                    Some source
                  else
                    None)
                sources
            in
            if new_sources = [] then
              None
            else
              Some (dest, new_sources))
          block.phis)
    ocx.program.blocks;
  (* Find and remove empty blocks *)
  IMap.iter
    (fun block_id block -> if is_empty_block block then remove_block ~ocx block_id)
    ocx.program.blocks;
  (* Consolidate blocks into a single large block when possible *)
  IMap.iter
    (fun block_id block ->
      (* Can only consolidate this block if it has exactly one previous block which continues to it *)
      match IMap.find_opt block_id ocx.prev_blocks with
      | None -> ()
      | Some prev_blocks ->
        if ISet.cardinal prev_blocks = 1 then
          let prev_block_id = ISet.choose prev_blocks in
          let prev_block = get_block ~ocx prev_block_id in
          (match prev_block.next with
          | Continue _ when block_id <> prev_block_id ->
            (* Move phis and instructions to previous block before removing this block *)
            prev_block.phis <- prev_block.phis @ block.phis;
            prev_block.instructions <- prev_block.instructions @ block.instructions;
            remove_block ~ocx block_id
          | _ -> ()))
    ocx.program.blocks
