import monaco from "./monaco";
import { DARK_THEME_NAME } from "./theme";

export function create(rootElement, options) {
  return monaco.editor.create(rootElement, {
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
    theme: DARK_THEME_NAME,
    ...options,
  });
}
