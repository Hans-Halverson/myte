open Myte_test

type node =
  | Dir of string * node list
  | Test of Test.t

type config = string option

(* List of commands - first part is title, second is command itself *)
type commands = (string * string) list

let exp_file_name = "EXP"

let config_file_name = "CONFIG"

let myte_file_suffix = ".myte"

let exp_file_suffix = ".exp"

let config_file_suffix = ".config"

let skip_rest_marker = "\nSKIP_REST"

let write_file file contents =
  let file_out = open_out file in
  output_string file_out contents;
  close_out file_out

let parse_config_file config_file =
  try
    let config_in = open_in config_file in
    let config_contents = Io.chan_read_contents config_in in
    close_in config_in;
    Some config_contents
  with
  | Sys_error _ -> None

let mk_diff_snippet s1 s2 =
  let s1_lines = String.split_on_char '\n' s1 in
  let s2_lines = String.split_on_char '\n' s2 in
  (* Pad shorter file to length of longer file with blank lines *)
  let s1_num_lines = List.length s1_lines in
  let s2_num_lines = List.length s2_lines in
  let (s1_lines, s2_lines) =
    if s1_num_lines > s2_num_lines then
      (s1_lines, s2_lines @ List_utils.make (s1_num_lines - s2_num_lines) "")
    else if s1_num_lines < s2_num_lines then
      (s1_lines @ List_utils.make (s2_num_lines - s1_num_lines) "", s2_lines)
    else
      (s1_lines, s2_lines)
  in
  let (line_num, snippet) =
    List.fold_left2
      (fun (i, first_diff) s1 s2 ->
        match first_diff with
        | Some _ -> (i, first_diff)
        | None ->
          if String.equal s1 s2 then
            (i + 1, None)
          else
            let extract_snippet lines prefix =
              let snip_lines = List_utils.drop (i - 1) lines |> List_utils.take 5 in
              let snip_lines = List.map (fun line -> prefix ^ line) snip_lines in
              let snip_lines = String.concat "\n" snip_lines in
              snip_lines ^ Pp.reset ()
            in
            ( i,
              Some
                (extract_snippet s1_lines (Pp.red_and_bold ^ "- ")
                ^ "\n"
                ^ extract_snippet s2_lines (Pp.green_and_bold ^ "+ ")) ))
      (1, None)
      s1_lines
      s2_lines
  in
  (line_num, Option.value ~default:"" snippet)

let run_snapshot_test ~commands ~config ~record ~myte_files ~exp_file =
  let formatted_commands = commands ~config myte_files in
  let use_error_title_prefix = List.length formatted_commands <> 1 in
  let title_prefix command_title =
    if use_error_title_prefix then
      command_title ^ ": "
    else
      ""
  in
  let commands_results =
    List.map
      (fun (command_title, formatted_command) ->
        (* Run command in separate process and read its stdout *)
        let process_stdout = Unix.open_process_in formatted_command in
        let act_contents = Io.chan_read_contents process_stdout in
        begin
          match Unix.close_process_in process_stdout with
          | Unix.WEXITED _ -> ()
          | _ -> failwith (Printf.sprintf "Command: %s failed to complete" formatted_command)
        end;
        (* Optionally re-record snapshot *)
        let record_snapshot () = if record then write_file exp_file act_contents in
        (* Read contents of exp file *)
        let exp_contents_result =
          try
            let exp_in = open_in exp_file in
            let exp_contents = Io.chan_read_contents exp_in in
            close_in exp_in;
            Ok exp_contents
          with
          | Sys_error err ->
            Error
              (Printf.sprintf
                 "%sReading %s failed with error: %s"
                 (title_prefix command_title)
                 exp_file
                 err)
        in
        let exp_contents = Result.value exp_contents_result ~default:"" in

        (* Handle the skip rest marker by truncating actual and expected results *)
        let (act_contents, exp_contents) =
          if String.ends_with ~suffix:skip_rest_marker act_contents then
            let act_contents =
              String.sub
                act_contents
                0
                (String.length act_contents - String.length skip_rest_marker - 1)
            in
            let exp_contents =
              String.sub
                exp_contents
                0
                (min (String.length exp_contents) (String.length act_contents))
            in
            (act_contents, exp_contents)
          else
            (act_contents, exp_contents)
        in

        if Result.is_error exp_contents_result then (
          record_snapshot ();
          Test.Failed (Result.get_error exp_contents_result)
        ) else if String.equal act_contents exp_contents then
          Test.Passed
        else (
          record_snapshot ();
          (* Extract diff to display *)
          match mk_diff_snippet exp_contents act_contents with
          | (line_num, diff_snippet) ->
            (* Format error message *)
            Test.Failed
              (Printf.sprintf
                 "%sActual and expected differ on line %d:\n%s"
                 (title_prefix command_title)
                 line_num
                 diff_snippet)
        ))
      formatted_commands
  in
  let failed_messages =
    List.fold_left
      (fun acc result ->
        match result with
        | Test.Passed -> acc
        | Test.Failed message -> message :: acc)
      []
      commands_results
  in
  if failed_messages = [] then
    Test.Passed
  else
    Test.Failed (String.concat "\n" (List.rev failed_messages))

let rec node_of_file
    ~(record : bool) (absolute_file : string) (commands : config:config -> string list -> commands)
    : node option =
  if Sys.is_directory absolute_file then
    let files = Array.to_list (Sys.readdir absolute_file) in
    let files = List.sort String.compare files in
    if List.exists (String.equal exp_file_name) files then
      (* The existence of an EXP file makes a test directory *)
      let myte_files =
        files
        |> List.filter (fun file -> Filename.check_suffix file myte_file_suffix)
        |> List.map (Filename.concat absolute_file)
      in
      let exp_file = Filename.concat absolute_file exp_file_name in
      let config = parse_config_file (Filename.concat absolute_file config_file_name) in
      let run () = run_snapshot_test ~commands ~config ~record ~myte_files ~exp_file in
      Some (Test { Test.name = Filename.basename absolute_file; run })
    else
      (* If there is no EXP file this is a regular directory *)
      let nodes =
        List.filter_map
          (fun file ->
            let absolute_file = Filename.concat absolute_file file in
            node_of_file ~record absolute_file commands)
          files
      in
      Some (Dir (Filename.basename absolute_file, nodes))
  else if Filename.check_suffix absolute_file myte_file_suffix then
    (* Myte files without neighboring EXP file are single file tests *)
    let name = Filename.chop_suffix absolute_file myte_file_suffix in
    let myte_files = [absolute_file] in
    let exp_file = name ^ exp_file_suffix in
    let config = parse_config_file (name ^ config_file_suffix) in
    let run () = run_snapshot_test ~commands ~config ~record ~exp_file ~myte_files in
    Some (Test { Test.name = Filename.basename name; run })
  else
    None

let rec suite_of_nodes (dir : string) (nodes : node list) : Suite.t option =
  let (tests, suites) =
    List.fold_left
      (fun (tests, suites) node ->
        match node with
        | Test test -> (test :: tests, suites)
        | Dir (dir, nodes) ->
          (match suite_of_nodes dir nodes with
          | None -> (tests, suites)
          | Some suite -> (tests, suite :: suites)))
      ([], [])
      nodes
  in
  match (tests, suites) with
  | ([], []) -> None
  | _ -> Some { Suite.name = dir; tests; suites }

let suite
    ~(record : bool) (absolute_dir : string) (commands : config:config -> string list -> commands) :
    Suite.t =
  let fail_no_tests () =
    failwith (Printf.sprintf "Expected %s to recursively contain tests" absolute_dir)
  in
  match node_of_file ~record absolute_dir commands with
  | None -> fail_no_tests ()
  | Some (Test _) -> failwith (Printf.sprintf "Expected %s to not contain tests" absolute_dir)
  | Some (Dir (dir, nodes)) ->
    (match suite_of_nodes dir nodes with
    | None -> fail_no_tests ()
    | Some suite -> suite)
