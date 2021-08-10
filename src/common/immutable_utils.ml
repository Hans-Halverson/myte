let id_map f x x' gen =
  let x'' = f x in
  if x'' == x then
    x'
  else
    gen x''

let id_map_list f xs =
  let (has_changed, xs') =
    List.fold_left
      (fun (has_changed, xs') x ->
        let x' = f x in
        (has_changed || x != x', x' :: xs'))
      (false, [])
      xs
  in
  if has_changed then
    List.rev xs'
  else
    xs

let id_map_opt f x =
  match x with
  | None -> None
  | Some x' ->
    let x'' = f x' in
    if x' == x'' then
      x
    else
      Some x''
