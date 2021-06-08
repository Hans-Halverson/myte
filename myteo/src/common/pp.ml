let opt_formatting format =
  if Opts.print_plain () then
    ""
  else
    format

type color =
  | Default
  | Black
  | Red
  | Green
  | White

type decorations =
  | Bold
  | Dim

let text_color = function
  | Black -> "\u{001B}[30m"
  | Red -> "\u{001B}[31m"
  | Green -> "\u{001B}[32m"
  | White -> "\u{001B}[37m"
  | Default -> "\u{001B}[39m"

let background_color = function
  | Black -> "\u{001B}[40m"
  | Red -> "\u{001B}[41m"
  | Green -> "\u{001B}[42m"
  | White -> "\u{001B}[107m"
  | Default -> "\u{001B}[49m"

let decoration = function
  | Bold -> "\u{001B}[1m"
  | Dim -> "\u{001B}[2m"

let reset () = opt_formatting "\u{001B}[0m"

let style ?(text = Default) ?(background = Default) ?(decorations = []) () =
  let decorations = String.concat "" (List.map decoration decorations) in
  opt_formatting
    (Printf.sprintf "%s%s%s" (text_color text) (background_color background) decorations)

let red_and_bold = style ~text:Red ~decorations:[Bold] ()

let green_and_bold = style ~text:Green ~decorations:[Bold] ()
