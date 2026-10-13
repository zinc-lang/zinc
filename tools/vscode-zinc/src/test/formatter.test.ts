import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { formatZinc } from "../lib/formatter";

describe("formatZinc", () => {
  it("converts tabs to spaces and trims trailing whitespace", () => {
    const input = "fn main() {\t\n\tlet x: Int = 1;   \n}\n";
    const out = formatZinc(input);
    assert.equal(out, "fn main() {\n    let x: Int = 1;\n}\n");
  });

  it("ensures a trailing newline", () => {
    assert.equal(formatZinc("fn main() {}"), "fn main() {}\n");
  });

  it("collapses more than two consecutive blank lines", () => {
    const input = "fn a() {}\n\n\n\nfn b() {}\n";
    assert.equal(formatZinc(input), "fn a() {}\n\n\nfn b() {}\n");
  });

  it("indents nested blocks by 4 spaces", () => {
    const input = `fn main() {
if true {
let x: Int = 1;
}
}
`;
    const expected = `fn main() {
    if true {
        let x: Int = 1;
    }
}
`;
    assert.equal(formatZinc(input), expected);
  });

  it("does not change string contents when reindenting", () => {
    const input = `fn main() {
let s: Str = "  keep  ";
}
`;
    const out = formatZinc(input);
    assert.match(out, /"  keep  "/);
    assert.match(out, /^    let s/m);
  });
});
