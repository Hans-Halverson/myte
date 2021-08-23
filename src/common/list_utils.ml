let rec make n init =
  if n <= 0 then
    []
  else
    init :: make (n - 1) init

let rec last lst =
  match lst with
  | [] -> failwith "Expected nonempty list"
  | [last] -> last
  | _ :: rest -> last rest

let split_first lst =
  match lst with
  | [] -> failwith "Expected nonempty list"
  | hd :: tl -> (hd, tl)

let split_last lst =
  let rec helper acc lst =
    match lst with
    | [] -> failwith "Expected nonempty list"
    | [last] -> (List.rev acc, last)
    | hd :: tl -> helper (hd :: acc) tl
  in
  helper [] lst

(* Split a list at a given index, return the list up to (but not including) that index, and the
   list starting at that index. *)
let split_at i lst =
  let rec helper i acc lst =
    match (i, lst) with
    | (0, _)
    | (_, []) ->
      (List.rev acc, lst)
    | (i, hd :: tl) -> helper (i - 1) (hd :: acc) tl
  in
  helper i [] lst

(* Split a list around a given index, which must be in the range [0, length - 1] inclusive. 
   Return the list up to (but not including) that index, the item at that index, and the list
   starting after that index. *)
let split_around i lst =
  let rec helper i acc lst =
    match (i, lst) with
    | (_, []) -> failwith "Index must be less than length of list"
    | (0, hd :: tl) -> (List.rev acc, hd, tl)
    | (i, hd :: tl) -> helper (i - 1) (hd :: acc) tl
  in
  helper i [] lst

let rec drop_last lst =
  match lst with
  | []
  | [_] ->
    []
  | element :: rest -> element :: drop_last rest

let rec split3 lst =
  match lst with
  | [] -> ([], [], [])
  | (hd1, hd2, hd3) :: tl ->
    let (tl1, tl2, tl3) = split3 tl in
    (hd1 :: tl1, hd2 :: tl2, hd3 :: tl3)

let hd_opt lst =
  match lst with
  | [] -> None
  | hd :: _ -> Some hd

let rec drop i lst =
  match (i, lst) with
  | (_, []) -> []
  | (i, lst) when i <= 0 -> lst
  | (i, _ :: tl) -> drop (i - 1) tl

let rec take i lst =
  match (i, lst) with
  | (0, _)
  | (_, []) ->
    []
  | (i, hd :: tl) -> hd :: take (i - 1) tl

let rec find_map_opt f lst =
  match lst with
  | [] -> None
  | hd :: tl ->
    (match f hd with
    | None -> find_map_opt f tl
    | Some _ as result -> result)

let iteri2 f l1 l2 =
  let rec helper i f l1 l2 =
    match (l1, l2) with
    | ([], []) -> ()
    | (hd1 :: tl1, hd2 :: tl2) ->
      f i hd1 hd2;
      helper (i + 1) f tl1 tl2
    | (_, _) -> failwith "Lists must have same length"
  in
  helper 0 f l1 l2

(* Transpose a matrix represented as a list of lists *)
let rec transpose matrix =
  match matrix with
  | []
  | [] :: _ ->
    []
  | matrix -> List.map List.hd matrix :: transpose (List.map List.tl matrix)
