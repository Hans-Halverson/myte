type pos = {
  line: int;
  col: int;
}

type t = {
  file: string option;
  start: pos;
  _end: pos;
}

let first_pos = { line = 1; col = 0 }

let between { file; start; _ } { _end; _ } = { file; start; _end }

let pos_to_string { line; col } = Printf.sprintf "%d:%d" line col

let to_string ?(source=false) loc =
  if source then
    match loc.file with
    | None -> Printf.sprintf ":%s-%s" (pos_to_string loc.start) (pos_to_string loc._end)
    | Some source -> Printf.sprintf "%s:%s-%s" source (pos_to_string loc.start) (pos_to_string loc._end)
  else
    Printf.sprintf "%s-%s" (pos_to_string loc.start) (pos_to_string loc._end)
