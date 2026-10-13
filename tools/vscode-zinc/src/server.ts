import {
  createConnection,
  TextDocuments,
  ProposedFeatures,
  InitializeResult,
  TextDocumentSyncKind,
  DiagnosticSeverity,
  type Diagnostic,
  type InitializeParams,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { CliCompilerBackend, type CompilerBackend } from "./lib/compiler";
import { parseZincDiagnostics } from "./lib/diagnostics";

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

let compilerPath = process.env.ZINC_COMPILER ?? "zinc";
let backend: CompilerBackend = new CliCompilerBackend(compilerPath);

connection.onInitialize((params: InitializeParams): InitializeResult => {
  const init = params.initializationOptions as { compilerPath?: string } | undefined;
  if (init?.compilerPath) {
    compilerPath = init.compilerPath;
    backend = new CliCompilerBackend(compilerPath);
  }
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      // Definition / completion / formatting live in extension.ts (extension host).
    },
  };
});

connection.onInitialized(() => {
  connection.workspace
    .getConfiguration("zinc")
    .then((cfg: { compilerPath?: string } | null) => {
      if (cfg?.compilerPath) {
        compilerPath = cfg.compilerPath;
        backend = new CliCompilerBackend(compilerPath);
      }
    })
    .catch(() => {
      /* configuration optional */
    });
});

const pendingValidations = new Map<string, NodeJS.Timeout>();

function scheduleValidate(doc: TextDocument): void {
  const prev = pendingValidations.get(doc.uri);
  if (prev) {
    clearTimeout(prev);
  }
  const handle = setTimeout(() => {
    pendingValidations.delete(doc.uri);
    void validateTextDocument(doc);
  }, 300);
  pendingValidations.set(doc.uri, handle);
}

documents.onDidChangeContent((change) => {
  scheduleValidate(change.document);
});

documents.onDidOpen((event) => {
  scheduleValidate(event.document);
});

documents.onDidClose((event) => {
  const prev = pendingValidations.get(event.document.uri);
  if (prev) {
    clearTimeout(prev);
    pendingValidations.delete(event.document.uri);
  }
  connection.sendDiagnostics({ uri: event.document.uri, diagnostics: [] });
});

async function validateTextDocument(doc: TextDocument): Promise<void> {
  if (doc.languageId !== "zinc" && !doc.uri.endsWith(".zn")) {
    return;
  }

  const filePath = uriToPath(doc.uri);
  let checkPath = filePath;
  let tempPath: string | undefined;

  const diskMatches =
    !!filePath &&
    fs.existsSync(filePath) &&
    fs.readFileSync(filePath, "utf8") === doc.getText();

  if (!diskMatches) {
    const base = filePath ? path.basename(filePath) : "buffer.zn";
    tempPath = path.join(os.tmpdir(), `zinc-lsp-${process.pid}-${base}`);
    fs.writeFileSync(tempPath, doc.getText(), "utf8");
    checkPath = tempPath;
  }

  if (!checkPath) {
    return;
  }

  const result = await backend.checkFile(checkPath);
  const combined = `${result.stdout}\n${result.stderr}`;
  const parsed = parseZincDiagnostics(combined, checkPath);
  if (filePath && filePath !== checkPath) {
    parsed.push(...parseZincDiagnostics(combined, filePath));
  }

  const seen = new Set<string>();
  const diagnostics: Diagnostic[] = [];
  for (const d of parsed) {
    const key = `${d.message}@${d.range.start.line}:${d.range.start.character}`;
    if (seen.has(key)) {
      continue;
    }
    seen.add(key);
    diagnostics.push({
      severity: toLspSeverity(d.severity),
      range: d.range,
      message: d.message,
      source: d.source,
    });
  }

  if (!result.ok && diagnostics.length === 0) {
    const errText = (result.stderr || result.stdout || "zinc check failed").trim();
    diagnostics.push({
      severity: DiagnosticSeverity.Error,
      range: {
        start: { line: 0, character: 0 },
        end: { line: 0, character: 1 },
      },
      message: errText.split("\n")[0] ?? "zinc check failed",
      source: "zinc",
    });
  }

  connection.sendDiagnostics({ uri: doc.uri, diagnostics });

  if (tempPath) {
    try {
      fs.unlinkSync(tempPath);
    } catch {
      /* ignore */
    }
  }
}

function toLspSeverity(
  s: "error" | "warning" | "information" | "hint"
): DiagnosticSeverity {
  switch (s) {
    case "error":
      return DiagnosticSeverity.Error;
    case "warning":
      return DiagnosticSeverity.Warning;
    case "hint":
      return DiagnosticSeverity.Hint;
    default:
      return DiagnosticSeverity.Information;
  }
}

function uriToPath(uri: string): string | undefined {
  try {
    if (uri.startsWith("file:")) {
      return fileURLToPath(uri);
    }
    return uri;
  } catch {
    return undefined;
  }
}

documents.listen(connection);
connection.listen();
