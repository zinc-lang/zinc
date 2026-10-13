import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { completeAt } from "../lib/completion";
import { extractSymbols } from "../lib/symbols";

describe("completeAt", () => {
  it("suggests keywords matching the prefix", () => {
    const items = completeAt("fn main() { le", 0, 14, []);
    const labels = items.map((i) => i.label);
    assert.ok(labels.includes("let"));
    assert.ok(!labels.includes("struct"));
  });

  it("suggests document symbols matching the prefix", () => {
    const src = "fn distance() {}\nfn main() { di";
    const symbols = extractSymbols(src);
    const items = completeAt(src, 1, 14, symbols);
    assert.ok(items.some((i) => i.label === "distance"));
  });

  it("returns keyword completions at empty prefix", () => {
    const items = completeAt("fn main() { ", 0, 12, []);
    assert.ok(items.length > 10);
    assert.ok(items.some((i) => i.label === "return"));
    assert.ok(items.some((i) => i.label === "struct"));
  });

  it("deduplicates overlapping keyword and symbol names", () => {
    const src = "fn main() {}\nfn mai";
    const symbols = extractSymbols("fn main() {}");
    const items = completeAt(src, 1, 6, symbols);
    const mains = items.filter((i) => i.label === "main");
    assert.equal(mains.length, 1);
  });
});
