# vscode-zinc

VS Code / Cursor support for the Zinc language (`.zn` files).

## Features

- Syntax highlighting, brackets, comments
- Snippets: `fn`, `struct`, `impl`, `match`, `mod`, `println`, `printlnf`, `print`, `f"`
- LSP diagnostics via `zinc --check-only`
- Go to definition, completion, document formatting
- Debug adapter: compile and run

## Reliable development (recommended)

**Use the Extension Development Host (F5), not repeated VSIX installs.**

### Step by step

1. From the **zinc repo root** in Cursor, run `cd tools/vscode-zinc && npm install`.
2. Open **Run and Debug** and choose **Run Zinc Extension** (or press **F5**).
3. A second Cursor/VS Code window opens (Extension Development Host) with `examples/`.
4. Open `hello.zn`. Confirm the status bar language is **Zinc**.
5. In that window’s settings, set:

   ```json
   "zinc.compilerPath": "/absolute/path/to/zinc/out/stage1/bin/zinc"
   ```

6. Edit extension code under `tools/vscode-zinc/src/`. The watch task recompiles; reload the Host window with **Ctrl+R** if needed.
7. Before UI checks: `npm test`, then `npm run test:ui` (real VS Code Extension Host via `@vscode/test-electron` + `xvfb-run`).  
   Full gate: `npm run test:thorough`.

### Why not VSIX every time?

Installing a VSIX into the main Cursor window fights remote extension caching, can leave two Zinc extensions active, and hides activation failures. F5 loads your working tree directly.

### Optional: install into this Cursor for smoke test

```bash
cd tools/vscode-zinc
./scripts/install-dev.sh
```

Then **Developer: Reload Window**. Uninstall before going back to F5:

```bash
cursor --uninstall-extension F001.zinc
```

## Agent skill

This repo includes `.cursor/skills/vscode-zinc-lsp/` so Cursor Agent follows the F5 workflow and Zinc syntax rules automatically when editing this extension.

## Settings

| Setting | Default | Description |
|---------|---------|-------------|
| `zinc.compilerPath` | `zinc` | Path to the Zinc compiler |
| `zincLanguageServer.trace.server` | `off` | LSP trace (`off` / `messages` / `verbose`) |

## Package / publish

Build a VSIX (needs Node ≥ 20 for `@vscode/vsce`):

```bash
npm run package
```

### VS Code Marketplace

1. Create a publisher whose ID matches `package.json` → `"publisher"` (currently `F001`) at  
   [Marketplace Management](https://marketplace.visualstudio.com/manage).
2. Create an Azure DevOps PAT with **Marketplace → Manage**  
   ([docs](https://code.visualstudio.com/api/working-with-extensions/publishing-extension)).
3. Publish:

```bash
export VSCE_PAT='your-pat'
npm run publish:marketplace
```

Extension id will be `F001.zinc`.

### Open VSX (optional, used by some VS Code forks)

```bash
export OVSX_PAT='your-open-vsx-token'
npm run publish:openvsx
```

For local Cursor smoke-tests only (not Marketplace): `./scripts/install-dev.sh`.
