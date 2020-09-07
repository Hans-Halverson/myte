open Myte_test

type node =
  | Dir of string * node list
  | Test of Test.t

type config = string option

let exp_file_name = "EXP"

let config_file_name = "CONFIG"

let myte_file_suffix = ".myte"

let exp_file_suffix = ".exp"

let config_file_suffix = ".config"

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
  with Sys_error _ -> None

let first_diff_lines s1 s2 =
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
  let (line_num, lines) =
    List.fold_left2
      (fun (i, first_diff) s1 s2 ->
        match first_diff with
        | Some _ -> (i, first_diff)
        | None ->
          if String.equal s1 s2 then
            (i + 1, None)
          else
            (i, Some (s1, s2)))
      (1, None)
      s1_lines
      s2_lines
  in
  (line_num, Option.value ~default:("", "") lines)

let run_snapshot_test ~command ~config ~record ~myte_files ~exp_file =
  (* Run command in separate process and read its stdout *)
  let formatted_command = command ~config myte_files in
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
    with Sys_error err -> Error (Printf.sprintf "Reading %s failed with error: %s" exp_file err)
  in
  let exp_contents = Result.value exp_contents_result ~default:"" in
  if Result.is_error exp_contents_result then (
    record_snapshot ();
    Test.Failed (Result.get_error exp_contents_result)
  ) else if String.equal act_contents exp_contents then
    Test.Passed
  else (
    record_snapshot ();
    (* Extract diff to display *)
    match first_diff_lines exp_contents act_contents with
    | (line_num, (exp_line, act_line)) ->
      (* Format error message *)
      Test.Failed
        (Printf.sprintf
           "Actual and expected differ on line %d:\n  Expected | %s\n  Actual   | %s"
           line_num
           exp_line
           act_line)
  )

let rec node_of_file
    ~(record : bool) (absolute_file : string) (command : config:config -> string list -> string) :
    node option =
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
      let run () = run_snapshot_test ~command ~config ~record ~myte_files ~exp_file in
      Some (Test { Test.name = Filename.basename absolute_file; run })
    else
      (* If there is no EXP file this is a regular directory *)
      let nodes =
        List.filter_map
          (fun file ->
            let absolute_file = Filename.concat absolute_file file in
            node_of_file ~record absolute_file command)
          files
      in
      Some (Dir (Filename.basename absolute_file, nodes))
  else if Filename.check_suffix absolute_file myte_file_suffix then
    (* Myte files without neighboring EXP file are single file tests *)
    let name = Filename.chop_suffix absolute_file myte_file_suffix in
    let myte_files = [absolute_file] in
    let exp_file = name ^ exp_file_suffix in
    let config = parse_config_file (name ^ config_file_suffix) in
    let run () = run_snapshot_test ~command ~config ~record ~exp_file ~myte_files in
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
    ~(record : bool) (absolute_dir : string) (command : config:config -> string list -> string) :
    Suite.t =
  let fail_no_tests () =
    failwith (Printf.sprintf "Expected %s to recursively contain tests" absolute_dir)
  in
  match node_of_file ~record absolute_dir command with
  | None -> fail_no_tests ()
  | Some (Test _) -> failwith (Printf.sprintf "Expected %s to not contain tests" absolute_dir)
  | Some (Dir (dir, nodes)) ->
    (match suite_of_nodes dir nodes with
    | None -> fail_no_tests ()
    | Some suite -> suite)
