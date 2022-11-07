open Myte_test

let build_and_run_parser ~root ~target ~run_prefix opts =
  let target_triple = Target.target_triple target in
  let command =
    Printf.sprintf
      {|
        %s/scripts/build_self_hosted --target %s %s &&
        %s/scripts/run_tests --bin "%s %s/build/self-hosted-myte" --exclude self_hosted --filter parser
      |}
      root
      target_triple
      opts
      root
      run_prefix
      root
  in

  let process_stdout = Unix.open_process_in command in
  ignore (Io.chan_read_contents process_stdout);
  match Unix.close_process_in process_stdout with
  | Unix.WEXITED 0 -> ()
  | WEXITED exit_code -> fail (Printf.sprintf "Exited with error code %d" exit_code)
  | _ -> fail "Did not complete successfully"

let tests ~target ~run_prefix =
  let root = Sys.getcwd () in
  [
    ("parser_tests", (fun _ -> build_and_run_parser ~root ~target ~run_prefix ""));
    ("parser_tests_opt", (fun _ -> build_and_run_parser ~root ~target ~run_prefix "-O"));
  ]

let suite ~cross =
  let host = !Target.host in
  let (target, run_prefix) =
    if cross == None || Target.equal host (Option.get cross) then
      (host, "")
    else
      let cross_target = Option.get cross in
      (cross_target, Cross.get_cross_run_prefix ~host ~cross_target)
  in
  {
    Suite.name = "self_hosted";
    suites = [];
    tests =
      List.map (fun (name, f) -> { Test.name; run = run_unit_test f }) (tests ~target ~run_prefix);
  }
