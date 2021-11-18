import { useEffect, useRef } from "react";
import { INITIAL } from "vscode-textmate";
import { Color, Parser } from "./ansi";
import { create } from "./editor";
import monaco from "./monaco";
import { COLOR_TO_ID, MYTE_LANG_REGISTRY } from "./theme";

const parser = new Parser();

function setTokensProvider(useColors) {
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

        if (useColors) {
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
        } else if (span.foregroundColor === Color.Hidden) {
          tokens.push(
            (encodedLanguageId | (COLOR_TO_ID[Color.Hidden] << 14)) >>> 0
          );
        } else {
          tokens.push(encodedLanguageId >>> 0);
        }
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
      const model = monaco.editor.createModel(
        props.results,
        "ansi",
        monaco.Uri.file("result.ansi")
      );

      const editor = create(rootRef.current, {
        model,
        lineNumbers: false,
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

  // Reset parser for every new page of results so styling doesn't flow to next page
  useEffect(() => {
    parser.clear();
  }, [props.results]);

  useEffect(() => {
    MYTE_LANG_REGISTRY.then(() =>
      setTokensProvider(props.settings.shouldStyleTerminalOutput)
    );
  }, [props.settings.shouldStyleTerminalOutput]);

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
