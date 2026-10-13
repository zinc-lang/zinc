import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { parseZincDiagnostics } from "../lib/diagnostics";

describe("parseZincDiagnostics", () => {
  it("parses English error diagnostics with line/column", () => {
    const output = `[Error]: illegal type cast: from Str to Int
At: /tmp/example.zn(1:26)
1 | fn main() { let x: Int = "bad"; }
                             ^^^^^
`;
    const diags = parseZincDiagnostics(output, "/tmp/example.zn");
    assert.equal(diags.length, 1);
    assert.equal(diags[0].severity, "error");
    assert.equal(diags[0].message, "illegal type cast: from Str to Int");
    assert.equal(diags[0].range.start.line, 0);
    assert.equal(diags[0].range.start.character, 25);
  });

  it("parses Chinese diagnostics", () => {
    const output = `[错误]: 表达式类型推导失败, 请为表达式显式指定类型
位置: /home/u/a.zn(3:10)
3 |     println(1);
            ^^^^^^^^^^^
`;
    const diags = parseZincDiagnostics(output, "/home/u/a.zn");
    assert.equal(diags.length, 1);
    assert.equal(diags[0].severity, "error");
    assert.match(diags[0].message, /表达式类型推导失败/);
    assert.equal(diags[0].range.start.line, 2);
    assert.equal(diags[0].range.start.character, 9);
  });

  it("parses warnings and notes", () => {
    const output = `[Warning]: unused variable \`x\`
At: /tmp/w.zn(2:9)
[Note]: defined here
At: /tmp/w.zn(2:9)
`;
    const diags = parseZincDiagnostics(output, "/tmp/w.zn");
    assert.equal(diags.length, 2);
    assert.equal(diags[0].severity, "warning");
    assert.equal(diags[1].severity, "information");
  });

  it("ignores diagnostics for other files", () => {
    const output = `[Error]: boom
At: /tmp/other.zn(1:1)
`;
    const diags = parseZincDiagnostics(output, "/tmp/example.zn");
    assert.equal(diags.length, 0);
  });

  it("returns empty list for clean compiler output", () => {
    const output = `------- time consumed -------
SyntaxTree built : 0 ms
[TOTAL]          : 10 ms
-----------------------------
`;
    assert.deepEqual(parseZincDiagnostics(output, "/tmp/ok.zn"), []);
  });

  it("uses caret span length when present", () => {
    const output = `[Error]: bad
At: /tmp/c.zn(1:5)
1 | abcde
        ^^^
`;
    const diags = parseZincDiagnostics(output, "/tmp/c.zn");
    assert.equal(diags.length, 1);
    assert.equal(diags[0].range.end.character - diags[0].range.start.character, 3);
  });
});
