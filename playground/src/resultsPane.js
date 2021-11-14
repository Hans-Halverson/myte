import { useEffect, useRef } from "react";
import { INITIAL } from "vscode-textmate";
import { Parser } from "./ansi";
import monaco from "./monaco";
import { COLOR_TO_ID, DARK_THEME_NAME, MYTE_LANG_REGISTRY } from "./theme";

function initTokensProvider() {
  const parser = new Parser();
  const encodedLanguageId = monaco.languages.getEncodedLanguageId("ansi");
  const tokensProvider = {
    getInitialState() {
      return INITIAL;
    },

    tokenizeEncoded(line, state) {
      const spans = parser.appendLine(line);

      const tokens = [];
      for (const span of spans) {
        tokens.push(span.offset);

        // Build metadata for token
        const foregroundColorId = COLOR_TO_ID[span.foregroundColor] ?? 0;
        const backgroundColorId = COLOR_TO_ID[span.backgroundColor] ?? 0;

        const metadata =
          (encodedLanguageId |
            (span.attributeFlags << 11) |
            (foregroundColorId << 14) |
            (backgroundColorId << 23)) >>>
          0;

        tokens.push(metadata);
      }

      return { tokens, endState: state };
    },
  };

  monaco.languages.setTokensProvider("ansi", tokensProvider);
}

export default function ResultsPane(props) {
  const rootRef = useRef(null);
  const editorRef = useRef(null);

  useEffect(() => {
    if (rootRef.current) {
      MYTE_LANG_REGISTRY.then(() => initTokensProvider());

      const model = monaco.editor.createModel(
        props.results,
        "ansi",
        monaco.Uri.file("result.ansi")
      );

      const editor = monaco.editor.create(rootRef.current, {
        model,
        automaticLayout: true,
        fontSize: 14,
        lineNumbers: false,
        minimap: {
          enabled: false,
        },
        scrollbar: {
          vertical: "hidden",
        },
        overviewRulerBorder: false,
        overviewRulerLanes: 0,
        hideCursorInOverviewRuler: true,
        scrollBeyondLastLine: false,
        theme: DARK_THEME_NAME,
        // Disable interaction with editor
        readOnly: true,
        domReadOnly: true,
        folding: false,
        contextmenu: false,
        // Disable all highlights
        selectionHighlight: false,
        occurrencesHighlight: false,
        renderLineHighlight: false,
        foldingHighlight: false,
        matchBrackets: "never",
        renderWhitespace: "none",
        guides: { indentation: false },
      });

      editorRef.current = editor;

      return () => editor.dispose();
    }
  }, []);

  useEffect(() => {
    if (editorRef.current == null) {
      return;
    }

    editorRef.current.setValue(props.results);
  }, [props.results]);

  return (
    <div className="pane resultsPane">
      <div
        ref={rootRef}
        style={{ height: `${props.height}px`, width: `${props.width}px` }}
      />
      {props.isRunning && (
        <div className="resultsPaneOverlay">
          <div className="spinner resultsPaneSpinner" />
        </div>
      )}
    </div>
  );
}
