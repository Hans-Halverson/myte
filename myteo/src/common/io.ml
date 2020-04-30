let chan_read_contents chan =
  let rec read_line lines =
    try
      let line = input_line chan in
      line :: read_line lines
    with
      End_of_file -> List.rev lines
  in
  let lines = read_line [] in
  let contents = String.concat "\n" lines in
  contents


let file_read_lines file =
  let file_in = open_in file in
  let rec read_lines lines =
    try
      let line = input_line file_in in
      line :: read_lines lines
    with
      End_of_file -> List.rev lines
  in
  read_lines []

let file_read_lines_between file start _end =
  let file_in = open_in file in
  let rec read_lines i lines =
    try
      let line = input_line file_in in
      if start <= i && i <= _end then
        line :: read_lines (i + 1) lines
      else
        read_lines (i + 1) lines
    with
      End_of_file -> List.rev lines
  in
  read_lines 1 []