open Basic_collections

module rec Test : sig
  type t = {
    name: string;
    run: unit -> result;
  }

  and result =
    | Passed
    | Failed of string
end =
  Test

and TestResult : sig
  type 'a t = {
    name: string;
    result: 'a;
  }
end =
  TestResult

and Suite : sig
  type t = {
    name: string;
    suites: Suite.t list;
    tests: Test.t list;
  }
end =
  Suite

and SuiteResult : sig
  type result_or_skip = Test.result option

  and t = {
    name: string;
    suites: t list;
    tests: result_or_skip TestResult.t list;
  }
end =
  SuiteResult

exception TestFailedException of string

let pp_iset iset =
  let members = ISet.to_seq iset |> List.of_seq |> List.map string_of_int |> String.concat ", " in
  "(" ^ members ^ ")"

let fail msg = raise (TestFailedException msg)

let assert_ints_equal x y = if not (x = y) then fail (Printf.sprintf "Expected %d to equal %d" x y)

let assert_iset_equals iset expected =
  let expected_set = ISet.of_list expected in
  if not (ISet.equal iset expected_set) then
    fail (Printf.sprintf "Expected %s to equal %s" (pp_iset iset) (pp_iset expected_set))

let run_unit_test f _ =
  try
    f ();
    Test.Passed
  with
  | TestFailedException msg -> Test.Failed (Printf.sprintf "%s\n%s" msg (Printexc.get_backtrace ()))
  | _ -> Test.Failed (Printexc.get_backtrace ())
