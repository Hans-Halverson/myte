let concat_with_conjunction conj strs =
  match strs with
  | [] -> ""
  | [str] -> str
  | [str1; str2] -> str1 ^ " " ^ conj ^ " " ^ str2
  | strs ->
    let (strs, last_str) = List_utils.split_last strs in
    String.concat ", " strs ^ ", " ^ conj ^ " " ^ last_str

let concat_with_and = concat_with_conjunction "and"

let concat_with_or = concat_with_conjunction "or"
