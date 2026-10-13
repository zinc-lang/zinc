import assert from "node:assert/strict";
import { describe, it } from "node:test";
import {
  extractSymbols,
  findDefinition,
  wordAtPosition,
  type ZincSymbol,
} from "../lib/symbols";

const sample = `mod demo {
    struct Point {
        x: Int,
        y: Int,
    }

    enum Color {
        Red,
        Blue,
    }

    trait Drawable {
        fn draw(&self);
    }

    type Alias = Point;

    const MAX: Int = 10;

    fn distance(a: Point, b: Point) -> Int {
        let dx: Int = a.x;
        return dx;
    }
}
`;

describe("extractSymbols", () => {
  it("extracts top-level and nested declarations", () => {
    const symbols = extractSymbols(sample);
    const names = symbols.map((s) => s.name).sort();
    assert.deepEqual(names, [
      "Alias",
      "Color",
      "Drawable",
      "MAX",
      "Point",
      "demo",
      "distance",
      "draw",
      "dx",
    ].sort());
  });

  it("records symbol kinds", () => {
    const byName = Object.fromEntries(
      extractSymbols(sample).map((s) => [s.name, s.kind])
    );
    assert.equal(byName.Point, "struct");
    assert.equal(byName.Color, "enum");
    assert.equal(byName.Drawable, "trait");
    assert.equal(byName.distance, "function");
    assert.equal(byName.demo, "module");
    assert.equal(byName.Alias, "type");
    assert.equal(byName.MAX, "const");
    assert.equal(byName.dx, "variable");
  });
});

describe("wordAtPosition", () => {
  it("finds identifier under cursor", () => {
    const text = "let point: Point = p;";
    assert.equal(wordAtPosition(text, 0, 12), "Point");
    assert.equal(wordAtPosition(text, 0, 4), "point");
    assert.equal(wordAtPosition(text, 0, 3), undefined);
  });
});

describe("findDefinition", () => {
  it("resolves goto-definition for a known symbol", () => {
    const symbols = extractSymbols(sample);
    const pointUseLine = sample.split("\n").findIndex((l) => l.includes("type Alias"));
    const col = sample.split("\n")[pointUseLine].indexOf("Point");
    const found = findDefinition(sample, pointUseLine, col, symbols);
    assert.ok(found);
    assert.equal(found!.name, "Point");
    assert.equal(found!.kind, "struct");
  });

  it("returns undefined when symbol is unknown", () => {
    const symbols: ZincSymbol[] = extractSymbols(sample);
    assert.equal(findDefinition("fn main() { Missing(); }", 0, 14, symbols), undefined);
  });
});
