export type SymbolKindName =
  | "function"
  | "struct"
  | "enum"
  | "trait"
  | "module"
  | "type"
  | "const"
  | "variable";

export interface ZincSymbol {
  name: string;
  kind: SymbolKindName;
  line: number;
  character: number;
}

const DECL_PATTERNS: Array<{ kind: SymbolKindName; re: RegExp }> = [
  { kind: "function", re: /\b(?:pub\s+)?(?:unsafe\s+)?(?:async\s+)?fn\s+([A-Za-z_][A-Za-z0-9_]*)/g },
  { kind: "struct", re: /\b(?:pub\s+)?struct\s+([A-Za-z_][A-Za-z0-9_]*)/g },
  { kind: "enum", re: /\b(?:pub\s+)?enum\s+([A-Za-z_][A-Za-z0-9_]*)/g },
  { kind: "trait", re: /\b(?:pub\s+)?(?:unsafe\s+)?trait\s+([A-Za-z_][A-Za-z0-9_]*)/g },
  { kind: "module", re: /\b(?:pub\s+)?mod\s+([A-Za-z_][A-Za-z0-9_]*)/g },
  { kind: "type", re: /\b(?:pub\s+)?type\s+([A-Za-z_][A-Za-z0-9_]*)/g },
  { kind: "const", re: /\b(?:pub\s+)?const\s+([A-Za-z_][A-Za-z0-9_]*)/g },
  { kind: "variable", re: /\blet\s+(?:mut\s+)?([A-Za-z_][A-Za-z0-9_]*)/g },
];

function stripCommentsAndStrings(source: string): string {
  let out = "";
  let i = 0;
  while (i < source.length) {
    // line comment
    if (source[i] === "/" && source[i + 1] === "/") {
      out += "  ";
      i += 2;
      while (i < source.length && source[i] !== "\n") {
        out += " ";
        i++;
      }
      continue;
    }
    // block comment /* */
    if (source[i] === "/" && source[i + 1] === "*") {
      out += "  ";
      i += 2;
      while (i < source.length && !(source[i] === "*" && source[i + 1] === "/")) {
        out += source[i] === "\n" ? "\n" : " ";
        i++;
      }
      if (i < source.length) {
        out += "  ";
        i += 2;
      }
      continue;
    }
    // strings
    if (source[i] === '"' || source[i] === "'") {
      const quote = source[i];
      out += " ";
      i++;
      while (i < source.length) {
        if (source[i] === "\\" && i + 1 < source.length) {
          out += "  ";
          i += 2;
          continue;
        }
        if (source[i] === quote) {
          out += " ";
          i++;
          break;
        }
        out += source[i] === "\n" ? "\n" : " ";
        i++;
      }
      continue;
    }
    out += source[i];
    i++;
  }
  return out;
}

function offsetToLineChar(source: string, offset: number): { line: number; character: number } {
  let line = 0;
  let last = 0;
  for (let i = 0; i < offset && i < source.length; i++) {
    if (source[i] === "\n") {
      line++;
      last = i + 1;
    }
  }
  return { line, character: offset - last };
}

/** Heuristic symbol index used until compiler-as-library APIs expose HIR/AST. */
export function extractSymbols(source: string): ZincSymbol[] {
  const cleaned = stripCommentsAndStrings(source);
  const symbols: ZincSymbol[] = [];
  const seen = new Set<string>();

  for (const { kind, re } of DECL_PATTERNS) {
    re.lastIndex = 0;
    let m: RegExpExecArray | null;
    while ((m = re.exec(cleaned)) !== null) {
      const name = m[1];
      const pos = offsetToLineChar(source, m.index + m[0].lastIndexOf(name));
      const key = `${kind}:${name}:${pos.line}:${pos.character}`;
      if (seen.has(key)) {
        continue;
      }
      seen.add(key);
      symbols.push({
        name,
        kind,
        line: pos.line,
        character: pos.character,
      });
    }
  }

  return symbols;
}

export function wordAtPosition(
  source: string,
  line: number,
  character: number
): string | undefined {
  const lines = source.split(/\r?\n/);
  if (line < 0 || line >= lines.length) {
    return undefined;
  }
  const text = lines[line];
  if (character < 0 || character > text.length) {
    return undefined;
  }
  const isIdent = (ch: string | undefined) =>
    !!ch && /[A-Za-z0-9_]/.test(ch);

  // Cursor must sit on an identifier, or at EOL immediately after one
  // (for completion). Spaces between tokens are not identifiers.
  const onIdent = character < text.length && isIdent(text[character]);
  const atEolAfterIdent =
    character === text.length && character > 0 && isIdent(text[character - 1]);
  if (!onIdent && !atEolAfterIdent) {
    return undefined;
  }

  let start = Math.min(character, text.length);
  if (!isIdent(text[start]) && start > 0) {
    start--;
  }
  while (start > 0 && isIdent(text[start - 1])) {
    start--;
  }
  let end = start;
  while (end < text.length && isIdent(text[end])) {
    end++;
  }
  if (start === end) {
    return undefined;
  }
  return text.slice(start, end);
}
export function findDefinition(
  source: string,
  line: number,
  character: number,
  symbols: ZincSymbol[]
): ZincSymbol | undefined {
  const word = wordAtPosition(source, line, character);
  if (!word) {
    return undefined;
  }
  // Prefer the earliest declaration with that name.
  return symbols.find((s) => s.name === word);
}
