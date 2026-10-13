import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { ZINC_KEYWORDS } from "../lib/keywords";

describe("ZINC_KEYWORDS", () => {
  it("includes core control-flow and declaration keywords", () => {
    for (const kw of [
      "fn",
      "let",
      "struct",
      "enum",
      "trait",
      "impl",
      "mod",
      "match",
      "if",
      "else",
      "while",
      "for",
      "return",
      "pub",
      "use",
      "box",
      "mut",
      "async",
      "await",
    ]) {
      assert.ok(ZINC_KEYWORDS.includes(kw), `missing ${kw}`);
    }
  });

  it("has no duplicates", () => {
    assert.equal(ZINC_KEYWORDS.length, new Set(ZINC_KEYWORDS).size);
  });
});
