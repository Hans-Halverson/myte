open Basic_collections
open Mir
open Mir_builders

(*
 * Create and query dominator trees.
 * 
 * Algorithms are based on the paper "A fast algorithm for finding dominators in a flowgraph"
 * by Lengauer and Tarjan.
 *)

module DominatorTree = struct
  type t = {
    (* Key is a block, and value is the immediate dominator for that block. In the dominator tree,
       the idom is the parent pointer of the block. *)
    mutable idoms: Block.t BlockMap.t;
    (* Root of the dominator tree (the function's start block) *)
    root: Block.t;
    (* Child nodes in the dominator tree. Parent is the immediate dominator of the child nodes. *)
    mutable children: BlockMMap.t;
    mutable dominance_frontiers: BlockMMap.t;
  }
end

type cx = {
  mutable dom: int array;
  mutable parent: int array;
  mutable ancestor: int array;
  mutable child: int array;
  mutable vertex: int array;
  mutable label: int array;
  mutable semi: int array;
  mutable size: int array;
  mutable bucket: ISet.t array;
  mutable n: int;
  mutable block_to_num: int BlockMap.t;
  mutable num_to_block: Block.t IMap.t;
}

let mk_cx ~num_blocks ~block_to_num ~num_to_block =
  let arr_length = num_blocks + 1 in
  let mk_arr () = Array.make arr_length 0 in
  {
    dom = mk_arr ();
    parent = mk_arr ();
    ancestor = mk_arr ();
    child = mk_arr ();
    vertex = mk_arr ();
    label = mk_arr ();
    semi = mk_arr ();
    size = mk_arr ();
    bucket = Array.make arr_length ISet.empty;
    n = 0;
    block_to_num;
    num_to_block;
  }

let debug_print_dominator_tree ~(dt : DominatorTree.t) =
  let depth = ref 0 in
  let rec visit node =
    let indent = String.concat "" (List.init !depth (fun _ -> "| ")) in
    Printf.eprintf "%s%s\n" indent Block.(id_to_string node.id);
    let children = BlockMMap.find_all node dt.children in
    depth := !depth + 1;
    BlockSet.iter visit children;
    depth := !depth - 1
  in
  visit dt.root

let block_of_num ~cx num = IMap.find num cx.num_to_block

let num_of_block ~cx block = BlockMap.find block cx.block_to_num

let rec dfs ~cx block =
  let v = num_of_block ~cx block in
  cx.n <- cx.n + 1;
  cx.semi.(v) <- cx.n;
  cx.label.(v) <- v;
  cx.vertex.(cx.n) <- v;
  cx.size.(v) <- 1;
  iter_next_blocks block (fun next_block ->
      let w = num_of_block ~cx next_block in
      if cx.semi.(w) == 0 then (
        cx.parent.(w) <- v;
        dfs ~cx next_block
      ))

let rec compress ~cx v =
  let ancestor_v = cx.ancestor.(v) in
  let ancestor_ancestor_v = cx.ancestor.(ancestor_v) in
  if ancestor_ancestor_v != 0 then (
    compress ~cx ancestor_v;
    let label_ancestor_v = cx.label.(ancestor_v) in
    if cx.semi.(label_ancestor_v) < cx.semi.(cx.label.(v)) then cx.label.(v) <- label_ancestor_v;
    cx.ancestor.(v) <- ancestor_ancestor_v
  )

(* Simple link-eval from paper *)
let eval ~cx v =
  if cx.ancestor.(v) == 0 then
    v
  else (
    compress ~cx v;
    cx.label.(v)
  )

let link ~cx v w = cx.ancestor.(w) <- v

(* Optimized link-eval from paper.

   TODO: Fix and use optimized algorithm *)

(*
  let eval ~cx v =
  let ancestor_v = cx.ancestor.(v) in
  let label_v = cx.label.(v) in
  if ancestor_v == 0 then
    label_v
  else (
    compress ~cx v;
    let label_ancestor_v = cx.label.(ancestor_v) in
    if cx.semi.(label_ancestor_v) >= cx.semi.(label_v) then
      label_v
    else
      label_ancestor_v
  )

let link ~cx v w =
  let s = ref w in
  while cx.semi.(cx.label.(w)) < cx.semi.(cx.label.(cx.child.(!s))) do
    let child_s = cx.child.(!s) in
    let child_child_s = cx.child.(child_s) in
    let size_s = cx.size.(!s) in
    if size_s + cx.size.(child_child_s) >= 2 * cx.size.(child_s) then (
      cx.ancestor.(child_s) <- !s;
      cx.child.(!s) <- child_child_s
    ) else (
      cx.size.(child_s) <- size_s;
      cx.ancestor.(!s) <- child_s;
      s := child_s
    )
  done;
  cx.label.(!s) <- cx.label.(w);
  cx.size.(v) <- cx.size.(v) + cx.size.(w);
  if cx.size.(v) < 2 * cx.size.(w) then (
    let tmp = !s in
    s := cx.child.(v);
    cx.child.(v) <- tmp
  );
  while !s != 0 do
    cx.ancestor.(!s) <- v;
    s := cx.child.(!s)
  done
*)

let lengauer_tarjans ~cx (func : Function.t) =
  (* Step 1 from paper *)
  dfs ~cx func.start_block;

  (* Step 2 from paper *)
  for i = cx.n downto 2 do
    let w = cx.vertex.(i) in
    let block_w = block_of_num ~cx w in
    BlockSet.iter
      (fun prev_block ->
        let v = num_of_block ~cx prev_block in
        let u = eval ~cx v in
        if cx.semi.(u) < cx.semi.(w) then cx.semi.(w) <- cx.semi.(u))
      block_w.prev_blocks;

    let vertex_semi_w = cx.vertex.(cx.semi.(w)) in
    cx.bucket.(vertex_semi_w) <- ISet.add w cx.bucket.(vertex_semi_w);
    link ~cx cx.parent.(w) w;

    (* Step 3 from paper *)
    ISet.iter
      (fun v ->
        let u = eval ~cx v in
        cx.dom.(v) <-
          (if cx.semi.(u) < cx.semi.(v) then
            u
          else
            cx.parent.(w)))
      cx.bucket.(cx.parent.(w));

    cx.bucket.(cx.parent.(w)) <- ISet.empty
  done;

  (* Step 4 from paper *)
  for i = 2 to cx.n do
    let w = cx.vertex.(i) in
    if cx.dom.(w) != cx.vertex.(cx.semi.(w)) then cx.dom.(w) <- cx.dom.(cx.dom.(w))
  done;

  let r = num_of_block ~cx func.start_block in
  cx.dom.(r) <- 0

let build_idoms ~(dt : DominatorTree.t) ~(func : Function.t) =
  (* Set up mapping of blocks to numbers *)
  let num_blocks = ref 0 in
  let block_to_num = ref BlockMap.empty in
  let num_to_block = ref IMap.empty in

  func_iter_blocks func (fun block ->
      let block_num = !num_blocks + 1 in
      num_blocks := block_num;
      block_to_num := BlockMap.add block block_num !block_to_num;
      num_to_block := IMap.add block_num block !num_to_block);

  (* Run lengauer tarjans on number space to generate number space dominator tree *)
  let cx = mk_cx ~num_blocks:!num_blocks ~block_to_num:!block_to_num ~num_to_block:!num_to_block in
  lengauer_tarjans ~cx func;

  (* Convert from numbers back to blocks *)
  Array.iteri
    (fun key value ->
      if key != 0 && value != 0 then (
        let key_block = block_of_num ~cx key in
        let value_block = block_of_num ~cx value in
        dt.idoms <- BlockMap.add key_block value_block dt.idoms;
        dt.children <- BlockMMap.add value_block key_block dt.children
      ))
    cx.dom

let get_idom ~(dt : DominatorTree.t) block = BlockMap.find block dt.idoms

let get_children ~(dt : DominatorTree.t) block = BlockMMap.find_all block dt.children

let get_dominance_frontier ~(dt : DominatorTree.t) block =
  BlockMMap.find_all block dt.dominance_frontiers

let build_dominance_frontiers ~(dt : DominatorTree.t) ~(func : Function.t) =
  (* Bottom up traversal of dominator tree *)
  let rec visit block =
    let children = BlockMMap.find_all block dt.children in
    BlockSet.iter visit children;

    iter_next_blocks block (fun next_block ->
        if get_idom ~dt next_block != block then
          dt.dominance_frontiers <- BlockMMap.add block next_block dt.dominance_frontiers);

    BlockSet.iter
      (fun child_block ->
        BlockSet.iter
          (fun dom_frontier_block ->
            if get_idom ~dt dom_frontier_block != block then
              dt.dominance_frontiers <-
                BlockMMap.add block dom_frontier_block dt.dominance_frontiers)
          (get_dominance_frontier ~dt child_block))
      children
  in
  visit func.start_block

let build_dominator_tree ~(func : Function.t) =
  let dt =
    {
      DominatorTree.idoms = BlockMap.empty;
      root = func.start_block;
      children = BlockMMap.empty;
      dominance_frontiers = BlockMMap.empty;
    }
  in
  build_idoms ~dt ~func;
  build_dominance_frontiers ~dt ~func;
  dt
