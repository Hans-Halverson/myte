import { RUN_COMMAND } from "./commands";
import Header from "./header";
import Panes from "./panes";
import { useState } from "react";

const INITIAL_EDITOR_TEXT = `module playground

import std.io.println

fun main() {
  println("Hello world");
}
`;

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

export default function App() {
  const [command, setCommand] = useState(RUN_COMMAND);
  const [editorText, setEditorText] = useState(INITIAL_EDITOR_TEXT);
  const [isRunning, setIsRunning] = useState(false);
  const [results, setResults] = useState("");

  function onSubmit() {
    setIsRunning(true);
    setTimeout(() => {
      setIsRunning(false);
      setResults(PLACEHOLDER_RESULTS);
    }, 3000);
  }

  return (
    <div className="root">
      <Header
        command={command}
        onChangeCommand={setCommand}
        onSubmit={onSubmit}
      />
      <Panes
        editorText={editorText}
        setEditorText={setEditorText}
        isRunning={isRunning}
        results={results}
      />
    </div>
  );
}
