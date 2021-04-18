open Myte_test

let suite () = { Suite.name = "gen/x86"; suites = [Liveness_analysis.suite]; tests = [] }
