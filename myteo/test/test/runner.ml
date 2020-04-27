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

let rec run_suite filter path (name, suite) =
  let result = 
    match suite with
    | Suite.Group suites ->
      let results = List.map (fun (name, suite) -> run_suite filter (name :: path) (name, suite)) suites in
      SuiteResult.Group results
    | Suite.Tests tests ->
      let results =
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
      SuiteResult.Tests results
  in
  (name, result)

let run_suites filter suites = List.map (fun (name, suite) -> run_suite filter [name] (name, suite)) suites