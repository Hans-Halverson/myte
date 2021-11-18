import { useCallback, useEffect, useState } from "react";

import EditorPane from "./editorPane";
import PaneDivider from "./paneDivider";
import ResultsPane from "./resultsPane";

const MIN_PANE_WIDTH = 100;

export default function Panes(props) {
  const [windowHeight, setWindowHeight] = useState(window.innerHeight);
  const [windowWidth, setWindowWidth] = useState(window.innerWidth);

  // Width taken up by just the panes
  const panesContentsWidth = windowWidth - 36;
  const [editorWidth, setEditorWidth] = useState(panesContentsWidth / 2);

  const onResize = useCallback(() => {
    setWindowHeight(window.innerHeight);
    setWindowWidth(window.innerWidth);

    const ratio = window.innerWidth / windowWidth;
    setEditorWidth(editorWidth * ratio);
  }, [editorWidth, windowWidth]);

  useEffect(() => {
    window.addEventListener("resize", onResize);
    return () => window.removeEventListener("resize", onResize);
  }, [onResize]);

  // Editor width must be above min size, and cannot force results to be below min size
  let cappedEditorWidth = Math.max(editorWidth, MIN_PANE_WIDTH);
  cappedEditorWidth = Math.min(
    cappedEditorWidth,
    panesContentsWidth - MIN_PANE_WIDTH
  );

  const resultsPaneWidth = panesContentsWidth - cappedEditorWidth;
  const height = windowHeight - 64;

  return (
    <div className="panes">
      <EditorPane
        initialText={props.editorText}
        setText={props.setEditorText}
        height={height}
        width={cappedEditorWidth}
      />
      <PaneDivider offset={cappedEditorWidth} setOffset={setEditorWidth} />
      <ResultsPane
        results={props.results}
        settings={props.settings}
        isRunning={props.isRunning}
        height={height - 16}
        width={resultsPaneWidth}
      />
    </div>
  );
}
