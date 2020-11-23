let compare_opt f o1 o2 =
  match (o1, o2) with
  | (None, None) -> 0
  | (None, Some _) -> -1
  | (Some _, None) -> 1
  | (Some x1, Some x2) -> f x1 x2

let to_list o =
  match o with
  | None -> []
  | Some x -> [x]
