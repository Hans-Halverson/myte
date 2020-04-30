type pos = {
  line: int;
  col: int;
}

type t = {
  source: Source.t option;
  start: pos;
  _end: pos;
}

let first_pos = { line = 1; col = 0 }

let between { source; start; _ } { _end; _ } = { source; start; _end }

let pos_to_string { line; col } = Printf.sprintf "%d:%d" line col

let is_single_line loc = loc.start.line = loc._end.line

let to_string ?(source=false) loc =
  let source =
    if source then
      match loc.source with
      | None -> ":"
      | Some (Source.File file) -> Printf.sprintf "%s:" file
      | Some (Source.String _) -> "<STRING>:"
    else ""
  in
  Printf.sprintf "%s%s-%s" source (pos_to_string loc.start) (pos_to_string loc._end)
