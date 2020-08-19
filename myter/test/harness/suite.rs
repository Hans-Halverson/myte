enum Result {
    Passed,
    Failed(String),
}

struct Test {
    name: String,
    run: fn() -> Result,
}

struct TestResult<T> {
    name: String,
    result: T,
}

struct Suite {
    name: String,
    suites: Vec<Suite>,
    tests: Vec<Test>,
}

struct SuiteResult {
    name: String,
    suites: Vec<SuiteResult>,
    tests: Vec<TestResult<Option<Result>>>,
}
