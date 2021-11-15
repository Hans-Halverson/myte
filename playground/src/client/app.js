const Commands = require("../commands");
import Header from "./header";
import Panes from "./panes";
import RunRequests from "./runRequest";
import { useState } from "react";

const INITIAL_EDITOR_TEXT = `module playground

import std.io.println

fun main() {
  println("Hello world");
}
`;

export default function App() {
  const [command, setCommand] = useState(Commands.Execute);
  const [editorText, setEditorText] = useState(INITIAL_EDITOR_TEXT);
  const [isRunning, setIsRunning] = useState(false);
  const [results, setResults] = useState("");

  console.log(editorText);
  console.log(results);

  function onSubmit() {
    setIsRunning(true);
    RunRequests.send(editorText, command.id).then((results) => {
      if (!RunRequests.hasInflightRequest()) {
        setIsRunning(false);
      }

      if (results != null) {
        setResults(results);
      }
    });
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
