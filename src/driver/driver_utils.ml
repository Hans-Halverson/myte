let assembler_env_variable = "ASSEMBLER"

let linker_env_variable = "LINKER"

let clang_env_variable = "CLANG"

let assembler_path =
  match Sys.getenv_opt assembler_env_variable with
  | Some path -> path
  | None -> "as"

let linker_path =
  match Sys.getenv_opt linker_env_variable with
  | Some path -> path
  | None -> "ld"

let clang_path =
  match Sys.getenv_opt clang_env_variable with
  | Some path -> path
  | None -> "clang"

let print_errors errors =
  let errors = List.sort (fun (loc1, _) (loc2, _) -> Loc.compare loc1 loc2) errors in
  Printf.printf
    "%s"
    (String.concat
       "\n"
       (List.map
          (fun (loc, err) ->
            if loc <> Loc.none then
              Error_pp.pp loc err
            else
              Error_pp.print_message_line err)
          errors))

let print_error_message msg = print_string (Error_pp.print_message_line msg)
