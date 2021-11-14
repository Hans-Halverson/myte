import {
  createOnigScanner,
  createOnigString,
  loadWASM,
} from "vscode-oniguruma";
import onigumuraWasm from "vscode-oniguruma/release/onig.wasm";
import { Registry } from "vscode-textmate";
import { Color } from "./ansi";
import monaco from "./monaco";
import OneDarkProTheme from "../OneDark-Pro.json";
import languageConfig from "../../tools/vscode-extensions/myte/language-configuration.json";
import syntaxConfig from "../../tools/vscode-extensions/myte/syntaxes/myte.tmLanguage.json";

export const DARK_THEME_NAME = "one-dark-pro";
export const DARK_THEME = OneDarkProTheme;

initMyteLanguage();
initAnsiLanguage();
initTheme();

export const COLOR_TO_ID = {};
const COLOR_VALUE_TO_ID = {};

const THEME_COLOR_NAME_TO_COLOR = {
  "terminal.hidden": Color.Hidden,
  "terminal.ansiBlack": Color.Black,
  "terminal.ansiBlue": Color.Blue,
  "terminal.ansiBrightBlack": Color.BrightBlack,
  "terminal.ansiBrightBlue": Color.BrightBlue,
  "terminal.ansiBrightCyan": Color.BrightCyan,
  "terminal.ansiBrightGreen": Color.BrightGreen,
  "terminal.ansiBrightMagenta": Color.BrightMagenta,
  "terminal.ansiBrightRed": Color.BrightRed,
  "terminal.ansiBrightWhite": Color.BrightWhite,
  "terminal.ansiBrightYellow": Color.BrightYellow,
  "terminal.ansiCyan": Color.Cyan,
  "terminal.ansiGreen": Color.Green,
  "terminal.ansiMagenta": Color.Magenta,
  "terminal.ansiRed": Color.Red,
  "terminal.ansiWhite": Color.White,
  "terminal.ansiYellow": Color.Yellow,
};

export const MYTE_LANG_REGISTRY = createMyteLangRegistry();

function initMyteLanguage() {
  monaco.languages.register({
    id: "myte",
    extensions: [".myte"],
    aliases: ["Myte"],
  });

  monaco.languages.setLanguageConfiguration("myte", languageConfig);
}

function initAnsiLanguage() {
  monaco.languages.register({
    id: "ansi",
    extensions: [".ansi"],
    aliases: ["Ansi"],
  });
}

function initTheme() {
  OneDarkProTheme.settings.forEach((setting) => {
    if (typeof setting.scope !== "string") {
      setting.scope = setting.scope.join(",");
    }
  });

  const theme = {
    base: "vs-dark",
    inherit: "true",
    colors: OneDarkProTheme.colors,
    rules: OneDarkProTheme.settings.map(
      ({ scope, settings: { foreground, background, fontStyle } }) => ({
        token: scope,
        foreground,
        background,
        fontStyle,
      })
    ),
  };

  monaco.editor.defineTheme(DARK_THEME_NAME, theme);
}

function createMyteLangRegistry() {
  const registry = fetch(onigumuraWasm)
    .then(loadWASM)
    .then(() => {
      return new Registry({
        onigLib: Promise.resolve({
          createOnigScanner,
          createOnigString,
        }),
        loadGrammar: (scopeName) => {
          if (scopeName === "source.myte") {
            return syntaxConfig;
          }
        },
        theme: DARK_THEME,
      });
    });

  // Add more specific color
  const headNode = document.getElementsByTagName("head")[0];
  const styles = document.createElement("style");
  styles.type = "text/css";
  styles.media = "screen";
  headNode.appendChild(styles);

  registry.then((registry) => {
    const colorValues = registry.getColorMap();

    // Create reverse index from color value to id
    colorValues.forEach(
      (colorValue, index) =>
        (COLOR_VALUE_TO_ID[colorValue.toUpperCase()] = index)
    );

    // Create reverse index from color enum to id
    for (const themeColorName in OneDarkProTheme.colors) {
      const color = THEME_COLOR_NAME_TO_COLOR[themeColorName];
      if (color != null) {
        const colorValue = OneDarkProTheme.colors[themeColorName];
        COLOR_TO_ID[color] = COLOR_VALUE_TO_ID[colorValue.toUpperCase()];
      }
    }

    styles.innerHTML = generateTokensCSSForColorMap(colorValues);
  });

  return registry;
}

function generateTokensCSSForColorMap(colorMap) {
  let rules = [];
  for (let i = 1, len = colorMap.length; i < len; i++) {
    let color = colorMap[i];
    rules[i] = `.pane .mtk${i} { color: ${color}; }`;
  }
  rules.push(".pane .mtk1.mtk1 { color: #abb2bf; } ");
  rules.push(".pane .mtki { font-style: italic; }");
  rules.push(".pane .mtkb { font-weight: bold; }");
  rules.push(
    ".pane .mtku { text-decoration: underline; text-underline-position: under; }"
  );

  // Add hidden style attached to fake color
  const hiddenColorId = COLOR_TO_ID[Color.Hidden];
  rules.push(
    `.pane .mtk${hiddenColorId} { position: absolute; visibility: hidden; }`
  );

  return rules.join("\n");
}
