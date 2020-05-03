open Pp

(* The number of digits in the base-10 representation of a number *)
let num_digits n = int_of_float (log10 (float_of_int n)) + 1

(* Pad a line number with whitespace for displaying in error messages *)
let pad_number n max_num_digits =
  let num_digits = num_digits n in
  Printf.sprintf " %d%s " n (String.make (max_num_digits - num_digits) ' ')

let print_summary_line loc message =
  let source =
    match loc with
    | { Loc.source = Some (Source.File file); _ } -> Printf.sprintf "%s:" (Files.strip_root file)
    | { Loc.source = Some (Source.String _); _ } -> "<STRING>:"
    | _ -> ""
  in
  Printf.sprintf
    "%sERROR: %s%s%d:%d %s%s\n"
    (style ~text:Red ~decorations:[Bold] ())
    (style ~decorations:[Bold] ())
    source
    Loc.(loc.start.line)
    Loc.(loc.start.col)
    (reset ())
    message

let print_single_line loc snippet =
  let open Loc in
  let { start = { line = start_line; col = start_col }; _end = { col = end_col; _ }; _ } = loc in
  let max_num_digits = num_digits start_line in
  let padded_line_num = pad_number start_line max_num_digits in
  let end_col =
    if end_col = start_col then
      end_col + 1
    else
      end_col
  in
  let padded_carets =
    Printf.sprintf "%s%s" (String.make start_col ' ') (String.make (end_col - start_col) '^')
  in
  let line = List.nth_opt snippet 0 |> Option.value ~default:"" in
  Printf.sprintf "%s%s|%s %s" (style ~decorations:[Bold] ()) padded_line_num (reset ()) line
  ^ "\n"
  ^ Printf.sprintf
      "%s%s|%s %s%s\n"
      (style ~decorations:[Bold] ())
      (String.make (max_num_digits + 2) ' ')
      (style ~text:Red ~decorations:[Bold] ())
      padded_carets
      (reset ())

let print_first_line loc snippet =
  let open Loc in
  let max_num_digits = num_digits loc._end.line in
  let padded_line_num = pad_number loc.start.line max_num_digits in
  let line = List.nth_opt snippet 0 |> Option.value ~default:"" in
  let padded_carets =
    Printf.sprintf
      "%s%s"
      (String.make loc.start.col ' ')
      (String.make (String.length line - loc.start.col) '^')
  in
  Printf.sprintf
    "%s%s|%s / %s%s"
    (style ~decorations:[Bold] ())
    padded_line_num
    (style ~text:Red ~decorations:[Bold] ())
    (reset ())
    line
  ^ "\n"
  ^ Printf.sprintf
      "%s%s|%s | %s%s\n"
      (style ~decorations:[Bold] ())
      (String.make (max_num_digits + 2) ' ')
      (style ~text:Red ~decorations:[Bold] ())
      padded_carets
      (reset ())

let print_last_line loc snippet =
  let open Loc in
  let max_num_digits = num_digits loc._end.line in
  let padded_line_num = pad_number loc._end.line max_num_digits in
  let line = List.nth_opt snippet (loc._end.line - loc.start.line) |> Option.value ~default:"" in
  let padded_carets = String.make (loc._end.col + 1) '^' in
  Printf.sprintf
    "%s%s|%s | %s%s"
    (style ~decorations:[Bold] ())
    padded_line_num
    (style ~text:Red ~decorations:[Bold] ())
    (reset ())
    line
  ^ "\n"
  ^ Printf.sprintf
      "%s%s|%s \\ %s%s\n"
      (style ~decorations:[Bold] ())
      (String.make (max_num_digits + 2) ' ')
      (style ~text:Red ~decorations:[Bold] ())
      padded_carets
      (reset ())

let print_middle_line loc line_num snippet =
  let open Loc in
  let max_num_digits = num_digits loc._end.line in
  let padded_line_num = pad_number line_num max_num_digits in
  let line = List.nth snippet (line_num - loc.start.line) in
  let padded_carets = String.make (String.length line) '^' in
  Printf.sprintf
    "%s%s|%s | %s%s"
    (style ~decorations:[Bold] ())
    padded_line_num
    (style ~text:Red ~decorations:[Bold] ())
    (reset ())
    line
  ^ "\n"
  ^ Printf.sprintf
      "%s %s |%s | %s%s\n"
      (style ~decorations:[Bold] ())
      (String.make max_num_digits ' ')
      (style ~text:Red ~decorations:[Bold] ())
      padded_carets
      (reset ())

let print_separator_line loc =
  let open Loc in
  let max_num_digits = num_digits loc._end.line in
  let padding = String.make (max_num_digits - 1) ' ' in
  Printf.sprintf
    "%s ...%s%s |%s\n"
    (style ~decorations:[Bold] ())
    padding
    (style ~text:Red ~decorations:[Bold] ())
    (reset ())

let snip_string loc lines =
  let open Loc in
  let start_line = loc.start.line in
  let end_line = loc._end.line in
  let (_, rev_snippet) =
    List.fold_left
      (fun (i, lines) line ->
        if start_line - 1 <= i && i <= end_line - 1 then
          (i + 1, line :: lines)
        else
          (i + 1, lines))
      (0, [])
      lines
  in
  List.rev rev_snippet

let snippet loc =
  let open Loc in
  match loc.source with
  | None -> []
  | Some (Source.File file) -> Io.file_read_lines_between file loc.start.line loc._end.line
  | Some (Source.String str) -> snip_string loc (String.split_on_char '\n' str)

let pp loc message =
  let open Loc in
  let snippet = snippet loc in
  let summary = print_summary_line loc message in
  let lines =
    let start_line = loc.start.line in
    let end_line = loc._end.line in
    let off i = start_line + i in
    let num_lines = end_line - start_line + 1 in
    let first () = print_first_line loc snippet in
    let last () = print_last_line loc snippet in
    let mid i = print_middle_line loc i snippet in
    match num_lines with
    | 1 -> [print_single_line loc snippet]
    | 2 -> [first (); last ()]
    | 3 -> [first (); mid (off 1); last ()]
    | 4 -> [first (); mid (off 1); mid (off 2); last ()]
    | 5 -> [first (); mid (off 1); mid (off 2); mid (off 3); last ()]
    | _ ->
      [first (); mid (off 1); mid (off 2); print_separator_line loc; mid (end_line - 1); last ()]
  in
  let lines = summary :: lines in
  String.concat "" lines
