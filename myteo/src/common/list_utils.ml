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
