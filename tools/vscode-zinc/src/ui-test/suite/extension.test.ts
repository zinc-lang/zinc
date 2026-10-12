import * as assert from "node:assert";
import * as path from "node:path";
import * as fs from "node:fs";
import {
  window,
  workspace,
  extensions,
  commands,
  languages,
  Uri,
  Range,
  Position,
  WorkspaceEdit,
  TextEdit,
  CompletionTriggerKind,
} from "vscode";

suite("Zinc extension UI", () => {
  const examplesDir = path.resolve(__dirname, "../../../examples");
  const helloPath = path.join(examplesDir, "hello.zn");
  const compilerPath = path.resolve(
    __dirname,
    "../../../../../out/stage1/bin/zinc"
  );

  suiteSetup(async () => {
    const ext = extensions.getExtension("F001.zinc");
    assert.ok(ext, "F001.zinc extension should be present in test host");
    await ext.activate();

    if (fs.existsSync(compilerPath)) {
      await workspace
        .getConfiguration("zinc")
        .update("compilerPath", compilerPath, true);
    }
  });

  test("activates and contributes Zinc language", async () => {
    const ext = extensions.getExtension("F001.zinc");
    assert.ok(ext?.isActive);
    const langs = await languages.getLanguages();
    assert.ok(langs.includes("zinc"));
  });

  test("opens hello.zn as language zinc", async () => {
    const doc = await workspace.openTextDocument(Uri.file(helloPath));
    const editor = await window.showTextDocument(doc);
    assert.strictEqual(editor.document.languageId, "zinc");
    assert.ok(editor.document.getText().includes("struct Point"));
  });

  test("go to definition resolves Point", async () => {
    const doc = await workspace.openTextDocument(Uri.file(helloPath));
    await window.showTextDocument(doc);

    // `let p: Point` usage line
    const text = doc.getText();
    const lines = text.split(/\r?\n/);
    const line = lines.findIndex(
      (l) => l.includes("let p: Point") && !l.trimStart().startsWith("//")
    );
    const character = lines[line].indexOf("Point");
    const pos = new Position(line, character);

    const defs = (await commands.executeCommand(
      "vscode.executeDefinitionProvider",
      doc.uri,
      pos
    )) as Array<{ uri: Uri; range: Range }>;

    assert.ok(defs && defs.length > 0, "expected at least one definition");
    assert.strictEqual(defs[0].uri.toString(), doc.uri.toString());
    assert.strictEqual(defs[0].range.start.line, 1); // struct Point
  });

  test("go to definition resolves distance", async () => {
    const doc = await workspace.openTextDocument(Uri.file(helloPath));
    await window.showTextDocument(doc);
    const lines = doc.getText().split(/\r?\n/);
    const line = lines.findIndex((l) => l.includes("distance(p, p)"));
    const character = lines[line].indexOf("distance");

    const defs = (await commands.executeCommand(
      "vscode.executeDefinitionProvider",
      doc.uri,
      new Position(line, character)
    )) as Array<{ range: Range }>;

    assert.ok(defs?.length);
    assert.strictEqual(defs[0].range.start.line, 6); // fn distance
  });

  test("format document indents under-indented let", async () => {
    // Use a disposable copy so we can apply edits without mutating hello.zn on disk.
    const src = fs.readFileSync(helloPath, "utf8");
    assert.match(src, /^let p: Point/m);
    const tmpPath = path.join(examplesDir, "_ui_format.zn");
    fs.writeFileSync(tmpPath, src, "utf8");

    try {
      const doc = await workspace.openTextDocument(Uri.file(tmpPath));
      await window.showTextDocument(doc);

      const edits = (await commands.executeCommand(
        "vscode.executeFormatDocumentProvider",
        doc.uri,
        { tabSize: 4, insertSpaces: true }
      )) as TextEdit[];

      assert.ok(edits && edits.length > 0, "expected formatting edits");
      const we = new WorkspaceEdit();
      we.set(doc.uri, edits);
      const ok = await workspace.applyEdit(we);
      assert.ok(ok, "applyEdit failed");
      assert.match(doc.getText(), /^    let p: Point/m);
    } finally {
      try {
        fs.unlinkSync(tmpPath);
      } catch {
        /* ignore */
      }
    }
  });

  test("completion suggests distance for prefix di", async () => {
    const tmp = await workspace.openTextDocument({
      language: "zinc",
      content: "fn distance() {}\nfn main() {\n    di\n}\n",
    });
    await window.showTextDocument(tmp);
    const list = (await commands.executeCommand(
      "vscode.executeCompletionItemProvider",
      tmp.uri,
      new Position(2, 6),
      undefined,
      CompletionTriggerKind.Invoke
    )) as { items: Array<{ label: string | { label: string } }> };

    const labels = list.items.map((i) =>
      typeof i.label === "string" ? i.label : i.label.label
    );
    assert.ok(labels.includes("distance"), `got: ${labels.slice(0, 30).join(",")}`);
  });

  test("restart language server command is registered", async () => {
    const all = await commands.getCommands(true);
    assert.ok(all.includes("zinc.restartServer"));
  });

  test("diagnostics arrive for a type error (if zinc available)", async function () {
    if (!fs.existsSync(compilerPath)) {
      this.skip();
    }

    const badPath = path.join(examplesDir, "_ui_bad.zn");
    fs.writeFileSync(
      badPath,
      'fn main() {\n    let x: Int = "oops";\n}\n',
      "utf8"
    );

    try {
      const doc = await workspace.openTextDocument(Uri.file(badPath));
      await window.showTextDocument(doc);

      // Wait for language server diagnostics (debounced ~300ms + compile)
      let diags = languages.getDiagnostics(doc.uri);
      for (let i = 0; i < 40 && diags.length === 0; i++) {
        await sleep(250);
        diags = languages.getDiagnostics(doc.uri);
      }

      assert.ok(
        diags.length > 0,
        "expected diagnostics from zinc language server"
      );
      assert.ok(
        diags.some((d) => d.severity === 0 /* Error */ || d.message.length > 0)
      );
    } finally {
      try {
        fs.unlinkSync(badPath);
      } catch {
        /* ignore */
      }
    }
  });
});

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
