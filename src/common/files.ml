let absolute_root = Sys.getcwd ()

let absolute_root_len = String.length absolute_root

let absolute_root_with_trailing = absolute_root ^ "/"

let absolute_root_with_trailing_len = String.length absolute_root_with_trailing

let strip_root str =
  if
    String.length str >= absolute_root_with_trailing_len
    && String.equal (String.sub str 0 absolute_root_with_trailing_len) absolute_root_with_trailing
  then
    String.sub
      str
      absolute_root_with_trailing_len
      (String.length str - absolute_root_with_trailing_len)
  else
    str

let join_parts parts =
  let rec inner prefix parts =
    match parts with
    | [] -> prefix
    | part :: rest -> inner (Filename.concat prefix part) rest
  in
  match parts with
  | [] -> ""
  | [part] -> part
  | first :: rest -> inner first rest
