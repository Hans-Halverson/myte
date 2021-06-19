let macos_runtime_basename = "macos_runtime.S"

let macos_runtime_file_path =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let absolute_dir = Filename.concat root relative_dir in
  Filename.concat absolute_dir macos_runtime_basename
