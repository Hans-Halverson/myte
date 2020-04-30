type t =
  | UnknownToken of string
  | UnexpectedToken of { actual: Token.t; expected: Token.t option }

exception Fatal of (Loc.t * t)

let opt_formatting format = if Opts.print_plain () then "" else format

let reset_attributes () = opt_formatting "\u{001B}[0m"
let bold_attribute () = opt_formatting "\u{001B}[1m"
let red_color () = opt_formatting "\u{001B}[31m"
let reset_color () = opt_formatting "\u{001B}[39m"

let fatal err = raise (Fatal err)

let to_string error =
  match error with
  | UnknownToken raw -> Printf.sprintf "Unexpected token \"%s\"" raw
  | UnexpectedToken { actual = T_EOF; expected = None } -> "Unexpected <EOF>"
  | UnexpectedToken { actual; expected = None } ->
    Printf.sprintf "Unexpected token \"%s\"" (Token.to_string actual)
  | UnexpectedToken { actual; expected = Some expected } ->
    Printf.sprintf
      "Unexpected token \"%s\", expected \"%s\""
      (Token.to_string actual)
      (Token.to_string expected)

(* The number of digits in the base-10 representation of a number *)
let num_digits n =
  int_of_float (log10 (float_of_int n)) + 1

(* Pad a line number with whitespace for displaying in error messages *)
let pad_number n max_num_digits =
  let num_digits = num_digits n in
  Printf.sprintf " %d%s " n (String.make (max_num_digits - num_digits) ' ')

let rec print (loc, error) =
  let source =
    match loc with
    | { Loc.file = Some file; _ } -> Printf.sprintf "%s:" (Files.strip_root file)
    | _ -> ""
  in
  let lines =
    Printf.sprintf
      "%s%s%d:%d%s ERROR: %s%s"
      (bold_attribute ())
      source
      Loc.(loc.start.line)
      Loc.(loc.start.col)
      (red_color ())
      (reset_attributes ())
      (to_string error)
  in
  if Loc.is_single_line loc then
    lines ^ "\n" ^ (print_single_line loc)
  else
    lines

and print_single_line loc =
  let open Loc in
  let { start = { line = start_line; col = start_col }; _end = { col = end_col; _  }; _ } = loc in
  let max_num_digits = num_digits start_line in
  let padded_line_num = pad_number start_line max_num_digits in
  let padded_carets =
    Printf.sprintf "%s%s"
      (String.make start_col ' ')
      (String.make (end_col - start_col + 1) '^')
  in
  (* TODO *) let line = "<<<LINE GOES HERE>>>" in
  Printf.sprintf "%s%s|%s %s" (bold_attribute ()) padded_line_num (reset_attributes ()) line ^
  "\n" ^
  Printf.sprintf
    "%s%s|%s %s%s"
    (bold_attribute ())
    (String.make(max_num_digits + 2) ' ')
    (red_color ())
    padded_carets
    (reset_attributes ())