type pos = {
  line: int;
  col: int;
}

type t = {
  file: string option;
  start: pos;
  _end: pos;
}

let pos_to_string { line; col } = Printf.sprintf "%d:%d" line col

let first_pos = { line = 1; col = 0 }