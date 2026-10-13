import assert from "node:assert/strict";
import { describe, it } from "node:test";
import path from "node:path";
import { binaryOutputPath, buildCompileArgs } from "../lib/debugLaunch";

describe("debugLaunch edge cases", () => {
  it("strips only the final .zn extension", () => {
    assert.equal(
      binaryOutputPath("foo.bar.zn", "/out"),
      path.join("/out", "foo.bar")
    );
  });

  it("handles relative program paths", () => {
    assert.equal(
      binaryOutputPath("examples/hello.zn", "out/debug-run"),
      path.join("out/debug-run", "hello")
    );
  });

  it("keeps speak=en and out-dir ordering stable", () => {
    const args = buildCompileArgs("/a/b.zn", "/tmp/o");
    assert.equal(args[0], "--out-dir=/tmp/o");
    assert.equal(args[1], "--speak=en");
    assert.equal(args[2], "/a/b.zn");
  });
});
