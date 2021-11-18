import Header from "./header";
import Panes from "./panes";
import RunRequests from "./runRequest";
import {
  INITIAL_EDITOR_TEXT,
  INITIAL_SETTINGS,
  storeSettings,
} from "./storage";
import { useState } from "react";

export default function App() {
  const [editorText, setEditorText] = useState(INITIAL_EDITOR_TEXT);
  const [settings, setSettings] = useState(INITIAL_SETTINGS);
  const [isRunning, setIsRunning] = useState(false);
  const [results, setResults] = useState("");

  function onSubmit() {
    setIsRunning(true);
    RunRequests.send(editorText, settings.command.id).then((results) => {
      if (!RunRequests.hasInflightRequest()) {
        setIsRunning(false);
      }

      if (results != null) {
        setResults(results);
      }
    });
  }

  function onChangeSettings(settings) {
    storeSettings(settings);
    setSettings(settings);
  }

  return (
    <div className="root">
      <Header
        settings={settings}
        onChangeSettings={onChangeSettings}
        onSubmit={onSubmit}
      />
      <Panes
        editorText={editorText}
        setEditorText={setEditorText}
        isRunning={isRunning}
        results={results}
        settings={settings}
      />
    </div>
  );
}
