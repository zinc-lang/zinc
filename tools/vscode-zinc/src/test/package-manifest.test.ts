import assert from "node:assert/strict";
import { describe, it } from "node:test";
import fs from "node:fs";
import path from "node:path";
import { extensionRoot } from "./helpers";

describe("package.json contributes", () => {
  const root = extensionRoot();
  const pkg = JSON.parse(
    fs.readFileSync(path.join(root, "package.json"), "utf8")
  ) as {
    name: string;
    publisher: string;
    main: string;
    activationEvents: string[];
    contributes: {
      languages: Array<{ id: string; extensions: string[] }>;
      grammars: Array<{ path: string; language: string }>;
      snippets: Array<{ path: string; language: string }>;
      debuggers: Array<{ type: string; program: string }>;
      configurationDefaults: Record<string, Record<string, unknown>>;
      commands: Array<{ command: string }>;
    };
  };

  it("identifies as F001.zinc language extension", () => {
    assert.equal(pkg.name, "zinc");
    assert.equal(pkg.publisher, "F001");
    assert.equal(pkg.contributes.languages[0].id, "zinc");
    assert.ok(pkg.contributes.languages[0].extensions.includes(".zn"));
  });

  it("activates on zinc language and debug", () => {
    assert.ok(pkg.activationEvents.includes("onLanguage:zinc"));
    assert.ok(pkg.activationEvents.includes("onDebugResolve:zinc"));
  });

  it("points main and contributed paths at existing files", () => {
    assert.ok(fs.existsSync(path.join(root, pkg.main)));
    for (const g of pkg.contributes.grammars) {
      assert.ok(fs.existsSync(path.join(root, g.path)), g.path);
    }
    for (const s of pkg.contributes.snippets) {
      assert.ok(fs.existsSync(path.join(root, s.path)), s.path);
    }
    const dbg = pkg.contributes.debuggers.find((d) => d.type === "zinc");
    assert.ok(dbg);
    assert.ok(fs.existsSync(path.join(root, dbg!.program)), dbg!.program);
  });

  it("sets default formatter to F001.zinc", () => {
    assert.equal(
      pkg.contributes.configurationDefaults["[zinc]"]["editor.defaultFormatter"],
      "F001.zinc"
    );
  });

  it("registers restart server command", () => {
    assert.ok(
      pkg.contributes.commands.some((c) => c.command === "zinc.restartServer")
    );
  });
});

describe("TextMate grammar", () => {
  it("is valid JSON with source.zinc scope", () => {
    const grammar = JSON.parse(
      fs.readFileSync(
        path.join(extensionRoot(), "syntaxes/zinc.tmLanguage.json"),
        "utf8"
      )
    ) as { scopeName: string; patterns: unknown[] };
    assert.equal(grammar.scopeName, "source.zinc");
    assert.ok(Array.isArray(grammar.patterns));
    assert.ok(grammar.patterns.length > 0);
  });
});
