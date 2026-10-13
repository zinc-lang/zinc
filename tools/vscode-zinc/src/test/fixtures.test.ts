import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { completeAt } from "../lib/completion";
import { formatZinc } from "../lib/formatter";
import {
  extractSymbols,
  findDefinition,
  wordAtPosition,
} from "../lib/symbols";
import { readFixture } from "./helpers";

describe("examples/hello.zn fixture", () => {
  const src = readFixture("examples/hello.zn");
  const lines = src.split(/\r?\n/);

  it("extracts Point, distance, main, and locals", () => {
    const names = new Set(extractSymbols(src).map((s) => s.name));
    for (const n of ["Point", "distance", "main", "dx", "p", "d"]) {
      assert.ok(names.has(n), `missing symbol ${n}`);
    }
  });

  it("go-to-definition resolves Point and distance usages", () => {
    const symbols = extractSymbols(src);
    const pointLine = lines.findIndex(
      (l) => l.includes("let p: Point") && !l.trimStart().startsWith("//")
    );
    const pointCol = lines[pointLine].indexOf("Point");
    const pointDef = findDefinition(src, pointLine, pointCol, symbols);
    assert.equal(pointDef?.kind, "struct");
    assert.equal(pointDef?.line, 1);

    const distLine = lines.findIndex((l) => l.includes("distance(p, p)"));
    const distCol = lines[distLine].indexOf("distance");
    const distDef = findDefinition(src, distLine, distCol, symbols);
    assert.equal(distDef?.kind, "function");
    assert.equal(distDef?.name, "distance");
  });

  it("formats under-indented let in main", () => {
    assert.match(src, /^let p: Point/m);
    const formatted = formatZinc(src);
    assert.match(formatted, /^    let p: Point/m);
    assert.ok(formatted.endsWith("\n"));
  });

  it("completes distance and Point from document symbols", () => {
    const symbols = extractSymbols(src);
    const distLine = lines.findIndex((l) => l.includes("distance(p, p)"));
    // prefix "di" before distance call — use a synthetic buffer
    const buf = "fn main() {\n    di\n}";
    const items = completeAt(buf, 1, 6, symbols);
    assert.ok(items.some((i) => i.label === "distance"));

    const pointItems = completeAt("let x: Po", 0, 9, symbols);
    assert.ok(pointItems.some((i) => i.label === "Point"));
  });

  it("wordAtPosition ignores comment text", () => {
    const commentLine = lines.findIndex((l) =>
      l.includes("Ctrl+click")
    );
    assert.equal(
      wordAtPosition(src, commentLine, lines[commentLine].indexOf("Point")),
      "Point"
    );
  });
});
