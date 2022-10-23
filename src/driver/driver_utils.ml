let assembler_env_variable = "MYTE_AS"

let linker_env_variable = "MYTE_LD"

let cc_env_variable = "MYTE_CC"

let assembler_path () =
  match Sys.getenv_opt assembler_env_variable with
  | Some path -> path
  | None ->
    (match Target.target_system () with
    | Darwin -> "as"
    | Linux when Target.target_architecture () == Target.host_architecture () -> "as"
    | Linux -> Target.gcc_target_triple !Target.target ^ "-as")

let linker_path () =
  match Sys.getenv_opt linker_env_variable with
  | Some path -> path
  | None -> "ld"

let cc_path () =
  match Sys.getenv_opt cc_env_variable with
  | Some path -> path
  | None ->
    (match Target.target_system () with
    | Darwin -> "clang"
    | Linux when Target.target_architecture () == Target.host_architecture () -> "cc"
    | Linux -> Target.gcc_target_triple !Target.target ^ "-gcc")

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
