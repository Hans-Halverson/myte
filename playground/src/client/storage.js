const Commands = require("../server/commands");

const EDITOR_TEXT_KEY = "editor-text";
const SETTINGS_KEY = "settings";

const DEFAULT_EDITOR_TEXT = `module playground

import std.io.println

fun main() {
  println("Hello world");
}
`;

const DEFAULT_SETTINGS = {
  command: Commands.Execute,
  shouldStyleTerminalOutput: true,
  shouldOptimize: false,
};

export const INITIAL_EDITOR_TEXT = getInitialEditorText();

export const INITIAL_SETTINGS = getInitialSettings();

function getInitialEditorText() {
  const storedText = window.localStorage.getItem(EDITOR_TEXT_KEY);
  if (storedText == null || storedText === "") {
    window.localStorage.setItem(EDITOR_TEXT_KEY, DEFAULT_EDITOR_TEXT);
    return DEFAULT_EDITOR_TEXT;
  }

  return storedText;
}

function getInitialSettings() {
  const storedSettings = window.localStorage.getItem(SETTINGS_KEY);
  if (storedSettings == null || storedSettings === "") {
    window.localStorage.setItem(SETTINGS_KEY, JSON.stringify(DEFAULT_SETTINGS));
    return DEFAULT_SETTINGS;
  }

  return JSON.parse(storedSettings);
}

export function storeEditorText(editorText) {
  window.localStorage.setItem(EDITOR_TEXT_KEY, editorText);
}

export function storeSettings(settings) {
  window.localStorage.setItem(SETTINGS_KEY, JSON.stringify(settings));
}
