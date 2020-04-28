open Myte_test

let should_run filter name =
  match filter with
  | None -> true
  | Some filter ->
    try
      let _ = Str.search_forward filter name 0 in
      true
    with
      Not_found -> false

let rec run_suite filter path suite =
  let { Suite.name; suites; tests } = suite in
  let suites =
    List.map (fun ({Suite.name; _} as suite) -> run_suite filter (name :: path) suite) suites
  in
  let tests =
    List.map
      (fun { Test.name = test_name; run } ->
         let name_parts = List.rev (test_name :: path) in
         let full_name = String.concat "/" name_parts in
         let result = if should_run filter full_name then
             Some (run ())
           else
             None
         in
         { TestResult.name = test_name; result })
      tests
  in
  { SuiteResult.name; suites; tests}

let run_suites filter suites =
  List.map (fun ({Suite.name; _ } as suite) -> run_suite filter [name] suite) suites