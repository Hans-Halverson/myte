export const Color = {
  Default: 0,
  Hidden: 1,
  Black: 10,
  Red: 11,
  Green: 12,
  Yellow: 13,
  Blue: 14,
  Magenta: 15,
  Cyan: 16,
  White: 17,
  BrightBlack: 20,
  BrightRed: 21,
  BrightGreen: 22,
  BrightYellow: 23,
  BrightBlue: 24,
  BrightMagenta: 25,
  BrightCyan: 26,
  BrightWhite: 27,
};

export const AttributeFlags = {
  None: 0,
  Italic: 1 << 0,
  Bold: 1 << 1,
  Underline: 1 << 2,
};

export const DefaultStyle = {
  backgroundColor: Color.Default,
  foregroundColor: Color.Default,
  attributeFlags: 0,
};

export class Parser {
  lines = [];
  lineSpans = [];

  _finalStyle = { ...DefaultStyle };

  clear() {
    this.lines.splice(0);
    this.lineSpans.splice(0);
    this._finalStyle = { ...DefaultStyle };
  }

  appendLine(text) {
    const spans = this._parseLine(text, this._finalStyle);

    this.lineSpans.push(spans);
    this.lines.push(text);

    return spans;
  }

  _parseLine(text, style) {
    const spans = [];

    let textOffset = 0;
    let index = 0;

    while (index < text.length) {
      if (text.codePointAt(index) !== 0x1b) {
        let escOffset = text.indexOf("\x1b", index);
        if (escOffset === -1) escOffset = text.length;

        spans.push({
          ...style,
          offset: textOffset,
          length: escOffset - textOffset,
        });

        textOffset = escOffset;
        index = escOffset;
        continue;
      }

      if (index === text.length - 1) {
        break;
      }

      if (text[index + 1] !== "[") {
        index += 1;
        continue;
      }

      const mOffset = text.indexOf("m", index + 2);
      if (mOffset === -1) {
        index += 1;
        continue;
      }

      const argString = text.substring(index + 2, mOffset);
      if (!/^[0-9;]*$/.test(argString)) {
        index = mOffset;
        continue;
      }

      spans.push({
        ...style,
        offset: index,
        length: mOffset - index + 1,
        foregroundColor: Color.Hidden,
      });

      const args = argString
        .split(";")
        .filter((arg) => arg !== "")
        .map((arg) => parseInt(arg, 10));
      if (args.length === 0) args.push(0);

      this._applyCodes(args, style);

      textOffset = mOffset + 1;
      index = mOffset + 1;
    }

    if (textOffset !== index) {
      spans.push({ ...style, offset: textOffset, length: index - textOffset });
    }

    return spans;
  }

  _applyCodes(args, style) {
    for (let argIndex = 0; argIndex < args.length; argIndex += 1) {
      const code = args[argIndex];

      switch (code) {
        case 0:
          Object.assign(style, DefaultStyle);
          break;

        case 1:
          style.attributeFlags |= AttributeFlags.Bold;
          break;

        case 3:
          style.attributeFlags |= AttributeFlags.Italic;
          break;

        case 4:
          style.attributeFlags |= AttributeFlags.Underline;
          break;

        case 30:
        case 31:
        case 32:
        case 33:
        case 34:
        case 35:
        case 36:
        case 37:
          style.foregroundColor = Color.Black + (code - 30);
          break;

        case 39:
          style.foregroundColor = DefaultStyle.foregroundColor;
          break;

        case 40:
        case 41:
        case 42:
        case 43:
        case 44:
        case 45:
        case 46:
        case 47:
          style.backgroundColor = Color.Black + (code - 40);
          break;

        case 49:
          style.backgroundColor = DefaultStyle.backgroundColor;
          break;

        case 90:
        case 91:
        case 92:
        case 93:
        case 94:
        case 95:
        case 96:
        case 97:
          style.foregroundColor = Color.BrightBlack + (code - 90);
          break;

        case 100:
        case 101:
        case 102:
        case 103:
        case 104:
        case 105:
        case 106:
        case 107:
          style.backgroundColor = Color.BrightBlack | (code - 100);
          break;
      }
    }
  }
}
