const express = require("express");
const { run } = require("./runner");

const app = express();
app.use(express.json());

const PLACEHOLDER_RESULTS = `Error: Expected type \`String\` but found \`Int\`
./test/analyze/type_check/vec/\u{001B}[1mindexing.myte:10:3-7
 10 |   (x[0]: String);
    |    ^^^^

\u{001B}[31mError: Expected type \`Vec<Int>\` but found \`Int\`
./test/analyze/type_check/vec/indexing.myte:11:3-7
 11 |   (x[0]: Vec<Int>);\u{001B}[0m
    |    ^^^^

Error: Expected type \`Vec<Vec<Int>>\` but found \`Vec<Vec<String>>\`
./test/analyze/type_check/vec/indexing.myte:21:3-7
 21 |   (x[0]: Vec<Vec<Int>>);
    |    ^^^^

Error: \u{001B}[33mExpected type \u{001B}[32m\`Vec<Int>\` but\u{001B}[0m found \`Vec<String>\`
./test/analyze/type_check/vec/indexing.myte:22:3-10
 22 |   (x[0][0]: Vec<Int>);
    |    ^^^^^^^

Error: Expected type \`Vec<Vec<String>>\` but found \`String\`
./test/analyze/type_check/vec/indexing.myte:23:3-13
 23 |   (x[0][0][0]: Vec<Vec<String>>);
    |    ^^^^^^^^^^`;

// Simple development server - run myte and return response
app.post("/run", (request, response) => {
  const { program, command } = request.body;
  run(program, command).then(
    (body) => {
      response.setHeader("Content-Type", "application/json");
      response.send(JSON.stringify(body));
    },
    () => {
      response.setHeader("Content-Type", "application/json");
      response.send(JSON.stringify({ error: true }));
    }
  );
});

// Expose entire directory
express.static.mime.define({ "application/wasm": ["wasm"] });

app.use(express.static("."));

app.listen(8080);
