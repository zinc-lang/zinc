/**
 * Lightweight Zinc formatter.
 * Replaced later by a compiler-backed formatter when available.
 */
export function formatZinc(source: string): string {
  const lines = source.replace(/\t/g, "    ").split(/\r?\n/);
  const formatted: string[] = [];
  let indent = 0;
  let blankRun = 0;

  for (const raw of lines) {
    const trimmedRight = raw.replace(/[ \t]+$/g, "");
    const trimmed = trimmedRight.trim();

    if (trimmed.length === 0) {
      blankRun++;
      if (blankRun <= 2) {
        formatted.push("");
      }
      continue;
    }
    blankRun = 0;

    const leadingClosers = countLeadingClosers(trimmed);
    const indentForLine = Math.max(0, indent - leadingClosers);
    formatted.push("    ".repeat(indentForLine) + trimmed);

    const { opens, closes } = countBraces(trimmed);
    indent = Math.max(0, indent + opens - closes);
  }

  let out = formatted.join("\n");
  if (!out.endsWith("\n")) {
    out += "\n";
  }
  return out;
}

function countLeadingClosers(line: string): number {
  let n = 0;
  for (const ch of line) {
    if (ch === "}" || ch === ")" || ch === "]") {
      n++;
    } else {
      break;
    }
  }
  return n;
}

function countBraces(line: string): { opens: number; closes: number } {
  let opens = 0;
  let closes = 0;
  let inString: string | null = null;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (inString) {
      if (ch === "\\" && i + 1 < line.length) {
        i++;
        continue;
      }
      if (ch === inString) {
        inString = null;
      }
      continue;
    }
    if (ch === '"' || ch === "'") {
      inString = ch;
      continue;
    }
    if (ch === "/" && line[i + 1] === "/") {
      break;
    }
    if (ch === "{" || ch === "(" || ch === "[") {
      opens++;
    } else if (ch === "}" || ch === ")" || ch === "]") {
      closes++;
    }
  }
  return { opens, closes };
}
