export type DiagnosticSeverityName =
  | "error"
  | "warning"
  | "information"
  | "hint";

export interface Position {
  line: number;
  character: number;
}

export interface Range {
  start: Position;
  end: Position;
}

export interface ZincDiagnostic {
  message: string;
  severity: DiagnosticSeverityName;
  range: Range;
  source: string;
}

const HEADER_RE =
  /^\[(Error|Warning|Note|ICE|错误|警告|提示)\]:\s*(.+)\s*$/;
const LOCATION_RE =
  /^(?:At|位置):\s*(.+)\((\d+):(\d+)\)\s*$/;
const CARET_RE = /^(\s*)(\^+)\s*$/;

function severityOf(tag: string): DiagnosticSeverityName {
  switch (tag) {
    case "Error":
    case "错误":
    case "ICE":
      return "error";
    case "Warning":
    case "警告":
      return "warning";
    case "Note":
    case "提示":
      return "information";
    default:
      return "information";
  }
}

function pathsMatch(reported: string, expected: string): boolean {
  const a = reported.replace(/\\/g, "/");
  const b = expected.replace(/\\/g, "/");
  return a === b || a.endsWith("/" + b.split("/").pop()) || b.endsWith("/" + a.split("/").pop());
}

/**
 * Parse `zinc --check-only --speak=en|zh` human-readable diagnostics.
 * When a machine-readable compiler API lands, swap the caller; keep this
 * parser for CLI fallback.
 */
export function parseZincDiagnostics(
  output: string,
  filePath: string
): ZincDiagnostic[] {
  const lines = output.split(/\r?\n/);
  const diags: ZincDiagnostic[] = [];

  for (let i = 0; i < lines.length; i++) {
    const header = HEADER_RE.exec(lines[i]);
    if (!header) {
      continue;
    }
    const severity = severityOf(header[1]);
    const message = header[2].trim();

    let range: Range = {
      start: { line: 0, character: 0 },
      end: { line: 0, character: 1 },
    };
    let matchedFile = false;

    if (i + 1 < lines.length) {
      const loc = LOCATION_RE.exec(lines[i + 1]);
      if (loc) {
        const reportedPath = loc[1];
        matchedFile = pathsMatch(reportedPath, filePath);
        const line = Math.max(0, Number(loc[2]) - 1);
        const character = Math.max(0, Number(loc[3]) - 1);
        let endCharacter = character + 1;

        // Optional: "N | source" then caret line
        if (i + 3 < lines.length) {
          const caret = CARET_RE.exec(lines[i + 3]);
          if (caret) {
            endCharacter = character + caret[2].length;
          }
        }

        range = {
          start: { line, character },
          end: { line, character: Math.max(endCharacter, character + 1) },
        };
        i += 1;
      }
    }

    if (!matchedFile) {
      continue;
    }

    diags.push({
      message,
      severity,
      range,
      source: "zinc",
    });
  }

  return diags;
}
