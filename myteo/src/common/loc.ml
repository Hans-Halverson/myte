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

let point ?(source = None) pos =
  { source; start = pos; _end = { line = pos.line; col = pos.col + 1 } }

let point_start loc = point ~source:loc.source loc.start

let point_end loc = point ~source:loc.source { loc._end with col = max 0 (loc._end.col - 1) }

let pos_to_string { line; col } = Printf.sprintf "%d:%d" line col

let is_single_line loc = loc.start.line = loc._end.line

let to_string ?(source = false) loc =
  let source =
    if source then
      match loc.source with
      | None -> ":"
      | Some (Source.File file) -> Printf.sprintf "%s:" file
      | Some (Source.String _) -> "<STRING>:"
    else
      ""
  in
  Printf.sprintf "%s%s-%s" source (pos_to_string loc.start) (pos_to_string loc._end)
