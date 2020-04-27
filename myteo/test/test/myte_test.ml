module rec Test: sig
  type t = {
    name: string;
    run: unit -> result;
  }

  and result =
    | Passed
    | Failed of string
end = Test

and TestResult : sig
  type 'a t = {
    name: string;
    result: 'a;
  }
end = TestResult

and Suite : sig
  type t = string * t'

  and t' =
    | Group of Suite.t list
    | Tests of Test.t list
end = Suite

and SuiteResult : sig
  type t = string * t'

  and t' =
    | Group of SuiteResult.t list
    | Tests of result_or_skip TestResult.t list

  and result_or_skip = Test.result option
end = SuiteResult