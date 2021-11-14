import { INITIAL } from "vscode-textmate";
import { useEffect, useRef } from "react";
import monaco from "./monaco";
import { DARK_THEME_NAME, MYTE_LANG_REGISTRY } from "./theme";

function initTokensProvider() {
  const languageId = monaco.languages.getEncodedLanguageId("myte");
  MYTE_LANG_REGISTRY.then((registry) =>
    registry.loadGrammarWithConfiguration("source.myte", languageId, {})
  ).then((grammar) => {
    const tokensProvider = {
      getInitialState() {
        return INITIAL;
      },

      tokenizeEncoded(line, state) {
        const tokenizeLineResult2 = grammar.tokenizeLine2(line, state);
        const endState = tokenizeLineResult2.ruleStack;
        let { tokens } = tokenizeLineResult2;

        return { tokens, endState };
      },
    };

    monaco.languages.setTokensProvider("myte", tokensProvider);
  });
}

export default function Editor(props) {
  const editorRef = useRef(null);
  useEffect(() => {
    initTokensProvider();

    const model = monaco.editor.createModel(
      props.initialText,
      "myte",
      monaco.Uri.file("playground.myte")
    );

    const editor = monaco.editor.create(editorRef.current, {
      model,
      automaticLayout: true,
      fontSize: 14,
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
      padding: { top: 8, bottom: 8 },
      theme: DARK_THEME_NAME,
    });

    return () => editor.dispose();
  }, []);

  return (
    <div
      className="pane editorPane"
      ref={editorRef}
      style={{
        height: `${props.height}px`,
        width: `${props.width}px`,
      }}
    />
  );
}
