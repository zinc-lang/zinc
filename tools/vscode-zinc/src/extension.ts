import * as path from "node:path";
import {
  workspace,
  ExtensionContext,
  window,
  languages,
  commands,
  debug,
  Range,
  Position,
  CompletionItem,
  CompletionItemKind,
  DebugAdapterExecutable,
  type TextDocument,
  type CancellationToken,
  type FormattingOptions,
  type TextEdit,
  type Definition,
  type DebugSession,
  type DebugAdapterDescriptor,
  type DebugConfiguration,
  type ProviderResult,
  type WorkspaceFolder,
} from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";
import { extractSymbols, findDefinition } from "./lib/symbols";
import { formatZinc } from "./lib/formatter";
import { completeAt } from "./lib/completion";

let client: LanguageClient | undefined;

const ZINC_SELECTOR = [
  { language: "zinc", scheme: "file" },
  { language: "zinc", scheme: "untitled" },
];

export async function activate(context: ExtensionContext): Promise<void> {
  registerEditorProviders(context);
  registerDebugger(context);
  await startLanguageServer(context);

  context.subscriptions.push(
    commands.registerCommand("zinc.restartServer", async () => {
      await startLanguageServer(context);
      void window.showInformationMessage("Zinc language server restarted");
    })
  );
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}

function registerEditorProviders(context: ExtensionContext): void {
  context.subscriptions.push(
    languages.registerDefinitionProvider(ZINC_SELECTOR, {
      provideDefinition(document, position): Definition | undefined {
        const text = document.getText();
        const found = findDefinition(
          text,
          position.line,
          position.character,
          extractSymbols(text)
        );
        if (!found) {
          return undefined;
        }
        return {
          uri: document.uri,
          range: new Range(
            new Position(found.line, found.character),
            new Position(found.line, found.character + found.name.length)
          ),
        };
      },
    }),
    languages.registerDocumentFormattingEditProvider(ZINC_SELECTOR, {
      provideDocumentFormattingEdits(
        document: TextDocument,
        _options: FormattingOptions,
        _token: CancellationToken
      ): TextEdit[] {
        const original = document.getText();
        const formatted = formatZinc(original);
        if (formatted === original) {
          return [];
        }
        return [
          {
            range: new Range(
              document.positionAt(0),
              document.positionAt(original.length)
            ),
            newText: formatted,
          },
        ];
      },
    }),
    languages.registerCompletionItemProvider(
      ZINC_SELECTOR,
      {
        provideCompletionItems(document, position) {
          const text = document.getText();
          return completeAt(
            text,
            position.line,
            position.character,
            extractSymbols(text)
          ).map((item) => {
            const ci = new CompletionItem(item.label, toVsCompletionKind(item.kind));
            ci.detail = item.detail;
            return ci;
          });
        },
      },
      "."
    )
  );
}

function toVsCompletionKind(kind: string): CompletionItemKind {
  switch (kind) {
    case "keyword":
      return CompletionItemKind.Keyword;
    case "function":
      return CompletionItemKind.Function;
    case "struct":
    case "enum":
    case "trait":
    case "type":
      return CompletionItemKind.Struct;
    case "module":
      return CompletionItemKind.Module;
    case "const":
      return CompletionItemKind.Constant;
    case "variable":
      return CompletionItemKind.Variable;
    default:
      return CompletionItemKind.Text;
  }
}

function registerDebugger(context: ExtensionContext): void {
  context.subscriptions.push(
    debug.registerDebugAdapterDescriptorFactory("zinc", {
      createDebugAdapterDescriptor(
        _session: DebugSession
      ): ProviderResult<DebugAdapterDescriptor> {
        const adapter = context.asAbsolutePath(path.join("out", "debugAdapter.js"));
        return new DebugAdapterExecutable(process.execPath, [adapter]);
      },
    }),
    debug.registerDebugConfigurationProvider("zinc", {
      resolveDebugConfiguration(
        folder: WorkspaceFolder | undefined,
        config: DebugConfiguration
      ): ProviderResult<DebugConfiguration> {
        if (!config.type) {
          config.type = "zinc";
        }
        if (!config.request) {
          config.request = "launch";
        }
        if (!config.name) {
          config.name = "Zinc: Launch current file";
        }
        if (!config.program) {
          const editor = window.activeTextEditor;
          if (editor?.document.languageId === "zinc") {
            config.program = editor.document.uri.fsPath;
          }
        }
        if (!config.cwd) {
          config.cwd =
            folder?.uri.fsPath ?? workspace.workspaceFolders?.[0]?.uri.fsPath;
        }
        if (!config.outDir) {
          config.outDir = path.join(config.cwd ?? "/tmp", "out", "debug-run");
        }
        if (!config.compilerPath) {
          config.compilerPath =
            workspace.getConfiguration("zinc").get<string>("compilerPath") ??
            "zinc";
        }
        if (!config.program) {
          void window.showErrorMessage(
            "Zinc debug: open a .zn file or set `program` in launch.json"
          );
          return undefined;
        }
        return config;
      },
    })
  );
}

async function startLanguageServer(context: ExtensionContext): Promise<void> {
  const serverModule = context.asAbsolutePath(path.join("out", "server.js"));
  const serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: { execArgv: ["--nolazy", "--inspect=6009"] },
    },
  };

  const compilerPath =
    workspace.getConfiguration("zinc").get<string>("compilerPath") ?? "zinc";

  const clientOptions: LanguageClientOptions = {
    documentSelector: ZINC_SELECTOR,
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.zn"),
    },
    initializationOptions: {
      compilerPath,
    },
  };

  if (client) {
    try {
      await client.stop();
    } catch {
      /* ignore */
    }
  }

  client = new LanguageClient(
    "zincLanguageServer",
    "Zinc Language Server",
    serverOptions,
    clientOptions
  );

  context.subscriptions.push(client);
  try {
    await client.start();
  } catch (err: unknown) {
    const message = err instanceof Error ? err.message : String(err);
    void window.showErrorMessage(
      `Zinc language server failed to start: ${message}`
    );
  }
}
