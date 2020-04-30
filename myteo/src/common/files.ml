let absolute_root = Sys.getcwd ()

let absolute_root_len = String.length absolute_root

let strip_root str =
  if String.equal (String.sub str 0 absolute_root_len) absolute_root then
    String.sub str absolute_root_len ((String.length str) - absolute_root_len)
  else
    str
