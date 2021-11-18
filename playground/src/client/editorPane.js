import { INITIAL } from "vscode-textmate";
import { useEffect, useRef } from "react";
import { create } from "./editor";
import monaco from "./monaco";
import { storeEditorText } from "./storage";
import { MYTE_LANG_REGISTRY } from "./theme";

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

    const editor = create(editorRef.current, {
      model,
      padding: { top: 8, bottom: 8 },
    });

    model.onDidChangeContent(() => {
      const value = model.getValue();
      storeEditorText(value);
      props.setText(value);
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
