open Myte_test

type node =
  | Dir of string * node list
  | Test of Test.t

let exp_file_name = "OUT"

let myte_file_suffix = ".myte"

let read_all_in chan =
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

let write_file file contents =
  let file_out = open_out file in
  output_string file_out contents;
  close_out file_out

let first_diff_lines s1 s2 =
  let s1_lines = String.split_on_char '\n' s1 in
  let s2_lines = String.split_on_char '\n' s2 in
  (* Pad shorter file to length of longer file with blank lines *)
  let s1_num_lines = List.length s1_lines in
  let s2_num_lines = List.length s2_lines in
  let (s1_lines, s2_lines) =
    if s1_num_lines > s2_num_lines then
      (s1_lines, s2_lines @ List.init (s1_num_lines - s2_num_lines) (fun _ -> ""))
    else if s1_num_lines < s2_num_lines then
      (s1_lines @ List.init (s2_num_lines - s1_num_lines) (fun _ -> ""), s2_lines)
    else
      (s1_lines, s2_lines)
  in
  List.fold_left2
    (fun (i, first_diff) s1 s2 ->
       match first_diff with
       | Some _ -> (i, first_diff)
       | None -> if String.equal s1 s2 then (i + 1, None) else (i, Some (s1, s2)))
    (1, None)
    s1_lines
    s2_lines

let rec node_of_dir ~(record : bool) (absolute_dir : string) (command : string list -> string) : node option =
  if not (Sys.is_directory absolute_dir) then
    None
  else
    let files = Array.to_list (Sys.readdir absolute_dir) in
    let _ = List.sort String.compare files in
    (* The existence of an .exp file makes a test directory *)
    let is_test = List.exists (String.equal exp_file_name) files in
    match is_test with
    | false ->
      let nodes =
        List.filter_map
          (fun file ->
             let absolute_file = Filename.concat absolute_dir file in
             node_of_dir ~record absolute_file command)
          files
      in
      Some (Dir (Filename.basename absolute_dir, nodes))
    | true ->
      let run () = 
        (* Find absolute paths of all myte files in test directory *)
        let myte_files =
          List.filter (fun file -> Filename.check_suffix file myte_file_suffix) files
        in
        let absolute_myte_files = List.map (Filename.concat absolute_dir) myte_files in
        (* Run command in separate process and read its stdout *)
        let formatted_command = command absolute_myte_files in
        let process_in = Unix.open_process_in formatted_command in
        let act_contents = read_all_in process_in in
        begin
          match Unix.close_process_in process_in with
          | Unix.WEXITED _ -> ()
          | _ -> failwith (Printf.sprintf "Command: %s failed to complete" formatted_command)
        end;
        (* Read contents of exp file *)
        let absolute_exp_file = Filename.concat absolute_dir exp_file_name in
        let exp_in = open_in absolute_exp_file in
        let exp_contents = read_all_in exp_in in
        close_in exp_in;

        if String.equal act_contents exp_contents then
          Test.Passed
        else
          (* Extract diff to display *)
          let (line_num, (exp_line, act_line)) =
            match first_diff_lines exp_contents act_contents with
            | _, None -> failwith "Expected and actual files are expected to differ"
            | line_num, Some lines -> (line_num, lines)
          in
          (* Optionally re-record snapshot *)
          if record then
            write_file absolute_exp_file act_contents;
          (* Format error message *)
          Test.Failed (
            Printf.sprintf
              ("Actual and expected differ on line %d:\n  Expected | %s\n  Actual   | %s")
              line_num exp_line act_line)
      in
      Some (Test { Test.name = Filename.basename absolute_dir; run})

let rec suite_of_nodes (dir : string) (nodes : node list) : Suite.t option =
  let (tests, suites) =
    List.fold_left 
      (fun (tests, suites) node ->
         match node with
         | Test test -> (test :: tests, suites)
         | Dir (dir, nodes) ->
           match suite_of_nodes dir nodes with
           | None -> (tests, suites)
           | Some suite -> (tests, suite :: suites))
      ([], [])
      nodes
  in
  match (tests, suites) with
  | ([], []) -> None
  | (_ :: _, _ :: _) -> failwith (Printf.sprintf "%s contains both tests and directories" dir)
  | (_, []) -> Some (dir, Suite.Tests tests)
  | ([], _ ) -> Some (dir, Suite.Group suites)

let suite ~(record : bool) (absolute_dir : string) (command : string list -> string) : Suite.t' =
  let fail_no_tests () =
    failwith (Printf.sprintf "Expected %s to recursively contain tests" absolute_dir)
  in
  match node_of_dir ~record absolute_dir command with
  | None -> fail_no_tests ()
  | Some (Test _) -> failwith (Printf.sprintf "Expected %s to not contain tests" absolute_dir)
  | Some (Dir (dir, nodes)) ->
    (match suite_of_nodes dir nodes with
     | None -> fail_no_tests ()
     | Some (_, suite) -> suite)
