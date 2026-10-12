import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { extractSymbols, findDefinition, wordAtPosition } from "../lib/symbols";

describe("extractSymbols edge cases", () => {
  it("ignores fn names inside line comments", () => {
    const src = `// fn ghost() {}\nfn real() {}\n`;
    const names = extractSymbols(src).map((s) => s.name);
    assert.deepEqual(names, ["real"]);
  });

  it("ignores identifiers inside strings", () => {
    const src = `fn main() {\n    let s = "fn fake()";\n}\n`;
    const names = extractSymbols(src).map((s) => s.name);
    assert.ok(names.includes("main"));
    assert.ok(names.includes("s"));
    assert.ok(!names.includes("fake"));
  });

  it("handles pub unsafe async fn", () => {
    const src = `pub unsafe async fn run() {}\n`;
    const sym = extractSymbols(src).find((s) => s.name === "run");
    assert.equal(sym?.kind, "function");
  });

  it("handles let mut", () => {
    const src = `fn main() { let mut x: Int = 1; }\n`;
    const sym = extractSymbols(src).find((s) => s.name === "x");
    assert.equal(sym?.kind, "variable");
  });
});

describe("wordAtPosition / findDefinition edge cases", () => {
  it("supports unicode-free ascii identifiers with underscores", () => {
    const text = "let foo_bar = 1;";
    assert.equal(wordAtPosition(text, 0, 6), "foo_bar");
  });

  it("prefers earliest declaration when names collide", () => {
    const src = `fn dup() {}\nfn other() { dup(); }\nfn dup() {}\n`;
    // second dup is invalid zinc but indexer may keep both; find returns first
    const symbols = extractSymbols(src);
    const line = src.split("\n").findIndex((l) => l.includes("dup();"));
    const col = src.split("\n")[line].indexOf("dup");
    const found = findDefinition(src, line, col, symbols);
    assert.equal(found?.line, 0);
  });
});
