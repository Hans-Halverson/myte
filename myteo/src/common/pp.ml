let opt_formatting format =
  if Opts.print_plain () then
    ""
  else
    format

let reset () = opt_formatting "\u{001B}[0m"

let bold () = opt_formatting "\u{001B}[1m"

let red () = opt_formatting "\u{001B}[31m"

let red_background () = opt_formatting "\u{001B}[41m"

let green_background () = opt_formatting "\u{001B}[42m"

let reset_color () = opt_formatting "\u{001B}[39m"
