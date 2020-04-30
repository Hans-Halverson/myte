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
