open Myte_test

module rec CollatedSuiteResult : sig
  type t = {
    name: string;
    num_results: (int * int * int);
    suites: CollatedSuiteResult.t list;
    tests: CollatedTestResult.t list;
  }
end = CollatedSuiteResult

and CollatedTestResult : sig
  type t = {
    name: string;
    full_name: string;
    result: Test.result;
  }
end = CollatedTestResult

let collate_result suite_result =
  let rec collate_result_inner parents { SuiteResult.name; suites; tests } =
    let (num_results, suites) =
      List.fold_left
        (fun ((total_num_fail, total_num_pass, total_num_skip), col_results) result ->
           let ({ CollatedSuiteResult.num_results = (num_fail, num_pass, num_skip); _ } as col_result) =
             collate_result_inner (name :: parents) result
           in
           let num_results =
             (total_num_fail + num_fail, total_num_pass + num_pass, total_num_skip + num_skip)
           in
           (num_results, col_result :: col_results))
        ((0, 0, 0), [])
        suites
    in
    let (num_results, tests) =
      List.fold_left
        (fun ((num_fail, num_pass, num_skip), col_results) { TestResult.name; result } ->
           let num_results =
             match result with
             | None -> (num_fail, num_pass, num_skip + 1)
             | Some Test.Passed -> (num_fail, num_pass + 1, num_skip)
             | Some (Test.Failed _) -> (num_fail + 1, num_pass, num_skip)
           in
           let names = List.rev (name :: parents) in
           let full_name = String.concat "/" names in
           let col_results =
             match result with
             | None -> col_results
             | Some result -> { CollatedTestResult.name; full_name; result } :: col_results
           in
           (num_results, col_results))
        (num_results, [])
        tests
    in
    { CollatedSuiteResult.name; num_results; suites; tests }
  in
  collate_result_inner [] suite_result

let collate_results results = List.map collate_result results

let has_failed results =
  List.exists (fun { CollatedSuiteResult.num_results = (num_fail, _, _); _ } -> num_fail > 0) results

let has_passed results =
  List.exists (fun { CollatedSuiteResult.num_results = (_, num_pass, _); _ } -> num_pass > 0) results

let pp ~tree results =
  let buf = Buffer.create 16 in
  let add_string str = Buffer.add_string buf str in
  let add_strings strs = List.iter add_string strs in
  let add_indent indent = add_string (String.make (2 * indent) ' ') in
  let pp_suite_summary () =
    let (num_fail, num_pass, num_skip) =
      List.fold_left
        (fun (total_num_fail, total_num_pass, total_num_skip) result ->
           let { CollatedSuiteResult.num_results = (num_fail, num_pass, num_skip); _ } = result in
           (total_num_fail + num_fail, total_num_pass + num_pass, total_num_skip + num_skip))
        (0, 0, 0)
        results
    in
    add_strings [
      "Failed: "; 
      string_of_int num_fail;
      ", Passed: "; 
      string_of_int num_pass; 
      ", Skipped: "; 
      string_of_int num_skip; 
      "\n" ]
  in
  let rec pp_failure_tree { CollatedSuiteResult.name; num_results; suites; tests } indent =
    (* Print group header *)
    let (num_fail, num_pass, num_skip) = num_results in
    let num_ran = num_fail + num_pass in
    add_indent indent;
    add_strings [
      name;
      ": (";
      string_of_int num_pass;
      "/";
      string_of_int num_ran; 
      " passed, ";
      string_of_int num_skip;
      " skipped)\n" ];
    (* Print group children *)
    List.iter (fun suite -> pp_failure_tree suite (indent + 1)) suites;
    List.iter
      (fun test ->
         let { CollatedTestResult.name; result; _ } = test in
         let result_string = match result with
           | Test.Passed -> "PASSED"
           | Test.Failed _ -> "FAILED"
         in
         add_indent (indent + 1);
         add_strings [name; ": "; result_string; "\n"])
      tests
  in
  let pp_failure_summary () =
    let rec pp_failure { CollatedSuiteResult.suites; tests; _ } =
      List.iter pp_failure suites;
      List.iter
        (fun { CollatedTestResult.full_name; result; _ } ->
           match result with
           | Test.Passed -> ()
           | Test.Failed _ ->
             add_indent 1;
             add_string full_name;
             add_string "\n")
        tests
    in
    add_string "Failed Tests:\n";
    List.iter pp_failure results;
    add_string "\n"
  in
  let rec pp_failure_details { CollatedSuiteResult.suites; tests; _ } =
    List.iter pp_failure_details suites;
    List.iter
      (fun { CollatedTestResult.full_name; result; _ } ->
         match result with
         | Test.Passed -> ()
         | Test.Failed message -> add_strings ["FAILED "; full_name; ":\n"; message; "\n\n"])
      tests
  in
  if has_failed results then (
    List.iter pp_failure_details results;
    if tree then (
      List.iter (fun result -> pp_failure_tree result 0) results;
      add_string "\n");
    pp_failure_summary ()
  ) else if has_passed results then (
    add_string "All tests passed!\n";
  )else (
    add_string "No tests ran.\n"
  );
  pp_suite_summary ();
  Buffer.contents buf