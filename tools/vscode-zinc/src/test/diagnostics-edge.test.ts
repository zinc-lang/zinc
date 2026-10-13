import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { parseZincDiagnostics } from "../lib/diagnostics";

describe("parseZincDiagnostics edge cases", () => {
  it("matches by basename when absolute paths differ in prefix", () => {
    const output = `[Error]: boom
At: /other/prefix/file.zn(4:2)
`;
    const diags = parseZincDiagnostics(output, "/workspace/file.zn");
    assert.equal(diags.length, 1);
    assert.equal(diags[0].range.start.line, 3);
    assert.equal(diags[0].range.start.character, 1);
  });

  it("parses ICE as error", () => {
    const output = `[ICE]: compiler bug
At: /tmp/a.zn(1:1)
`;
    const diags = parseZincDiagnostics(output, "/tmp/a.zn");
    assert.equal(diags[0].severity, "error");
  });

  it("handles CRLF line endings", () => {
    const output =
      "[Error]: x\r\nAt: /tmp/a.zn(2:3)\r\n2 | abc\r\n    ^^\r\n";
    const diags = parseZincDiagnostics(output, "/tmp/a.zn");
    assert.equal(diags.length, 1);
    assert.equal(diags[0].range.start.line, 1);
    assert.equal(diags[0].range.start.character, 2);
    assert.equal(diags[0].range.end.character, 4);
  });

  it("parses multiple diagnostics in one stream", () => {
    const output = `[Error]: first
At: /tmp/m.zn(1:1)
[Error]: second
At: /tmp/m.zn(2:5)
`;
    const diags = parseZincDiagnostics(output, "/tmp/m.zn");
    assert.equal(diags.length, 2);
    assert.equal(diags[0].message, "first");
    assert.equal(diags[1].message, "second");
  });
});
