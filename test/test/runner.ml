open Myte_test

let regexp_matches regexp name =
  try
    let _ = Str.search_forward regexp name 0 in
    true
  with
  | Not_found -> false

let should_run ~filter ~exclude name =
  let is_filtered =
    match filter with
    | None -> true
    | Some filter -> regexp_matches filter name
  in
  let is_excluded =
    match exclude with
    | None -> false
    | Some exclude -> regexp_matches exclude name
  in
  is_filtered && not is_excluded

let rec run_suite ~filter ~exclude path suite =
  let { Suite.name; suites; tests } = suite in
  let suites =
    List.map
      (fun ({ Suite.name; _ } as suite) -> run_suite ~filter ~exclude (name :: path) suite)
      suites
  in
  let tests =
    List.map
      (fun { Test.name = test_name; run } ->
        let name_parts = List.rev (test_name :: path) in
        let full_name = String.concat "/" name_parts in
        let result =
          if should_run ~filter ~exclude full_name then
            Some (run ())
          else
            None
        in
        { TestResult.name = test_name; result })
      tests
  in
  { SuiteResult.name; suites; tests }

let run_suites ~filter ~exclude suites =
  List.map (fun ({ Suite.name; _ } as suite) -> run_suite ~filter ~exclude [name] suite) suites
