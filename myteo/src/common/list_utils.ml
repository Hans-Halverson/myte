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
