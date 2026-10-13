import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { binaryOutputPath, buildCompileArgs } from "../lib/debugLaunch";

describe("debugLaunch helpers", () => {
  it("derives binary path from program and outDir", () => {
    assert.equal(
      binaryOutputPath("/proj/src/hello.zn", "/proj/out"),
      "/proj/out/hello"
    );
  });

  it("builds zinc compile args with out-dir", () => {
    assert.deepEqual(
      buildCompileArgs("/proj/src/hello.zn", "/proj/out"),
      ["--out-dir=/proj/out", "--speak=en", "/proj/src/hello.zn"]
    );
  });
});
