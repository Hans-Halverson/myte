open Myte_test

let build_and_run_parser ~root opts =
  let command =
    Printf.sprintf
      {|
      %s/scripts/build_self_hosted %s &&
      %s/scripts/run_tests --bin %s/build/self-hosted-myte --exclude self_hosted --filter parser
    |}
      root
      opts
      root
      root
  in

  let process_stdout = Unix.open_process_in command in
  ignore (Io.chan_read_contents process_stdout);
  match Unix.close_process_in process_stdout with
  | Unix.WEXITED 0 -> ()
  | WEXITED exit_code -> fail (Printf.sprintf "Exited with error code %d" exit_code)
  | _ -> fail "Did not complete successfully"

let tests () =
  let root = Sys.getcwd () in
  [
    ("parser_tests", (fun _ -> build_and_run_parser ~root ""));
    ("parser_tests_opt", (fun _ -> build_and_run_parser ~root "-O"));
  ]

let suite =
  {
    Suite.name = "self_hosted";
    suites = [];
    tests = List.map (fun (name, f) -> { Test.name; run = run_unit_test f }) (tests ());
  }
