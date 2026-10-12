import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { formatZinc } from "../lib/formatter";

describe("formatZinc edge cases", () => {
  it("keeps } else { at the outer indent and indents the body", () => {
    const input = `fn main() {
if true {
let a: Int = 1;
} else {
let b: Int = 2;
}
}
`;
    const out = formatZinc(input);
    assert.match(out, /^    if true \{/m);
    assert.match(out, /^        let a: Int = 1;/m);
    assert.match(out, /^    \} else \{/m);
    assert.match(out, /^        let b: Int = 2;/m);
  });

  it("does not treat braces inside comments as structure", () => {
    const input = `fn main() {
// { ignored
let x: Int = 1;
}
`;
    const out = formatZinc(input);
    assert.match(out, /^    \/\/ \{ ignored/m);
    assert.match(out, /^    let x: Int = 1;/m);
  });

  it("is idempotent on already-formatted input", () => {
    const once = formatZinc(`fn main() {\n    let x: Int = 1;\n}\n`);
    const twice = formatZinc(once);
    assert.equal(twice, once);
  });
});
