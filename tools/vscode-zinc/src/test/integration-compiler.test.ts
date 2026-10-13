import assert from "node:assert/strict";
import { describe, it } from "node:test";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { spawnSync } from "node:child_process";
import { CliCompilerBackend } from "../lib/compiler";
import { parseZincDiagnostics } from "../lib/diagnostics";
import { formatZinc } from "../lib/formatter";
import { binaryOutputPath, buildCompileArgs } from "../lib/debugLaunch";
import {
  extensionRoot,
  readFixture,
  zincAvailable,
  zincCompilerPath,
} from "./helpers";

describe("compiler integration", () => {
  it("typechecks examples/hello.zn", async (t) => {
    if (!zincAvailable()) {
      t.skip("zinc binary missing");
      return;
    }
    const backend = new CliCompilerBackend(zincCompilerPath());
    const hello = path.join(extensionRoot(), "examples/hello.zn");
    const result = await backend.checkFile(hello);
    assert.equal(result.ok, true, result.stdout + result.stderr);
    assert.deepEqual(
      parseZincDiagnostics(result.stdout + "\n" + result.stderr, hello),
      []
    );
  });

  it("round-trips a type error through CLI + diagnostic parser", async (t) => {
    if (!zincAvailable()) {
      t.skip("zinc binary missing");
      return;
    }
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "zinc-ext-int-"));
    const file = path.join(dir, "bad.zn");
    fs.writeFileSync(
      file,
      'fn main() {\n    let x: Int = "oops";\n}\n',
      "utf8"
    );
    const backend = new CliCompilerBackend(zincCompilerPath());
    const result = await backend.checkFile(file);
    assert.equal(result.ok, false);
    const diags = parseZincDiagnostics(
      `${result.stdout}\n${result.stderr}`,
      file
    );
    assert.ok(diags.length >= 1, `expected diagnostics, got: ${result.stdout}\n${result.stderr}`);
    assert.equal(diags[0].severity, "error");
    assert.equal(diags[0].range.start.line, 1);
    assert.ok(diags[0].range.start.character >= 0);
  });

  it("formats then still typechecks hello.zn", async (t) => {
    if (!zincAvailable()) {
      t.skip("zinc binary missing");
      return;
    }
    const formatted = formatZinc(readFixture("examples/hello.zn"));
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "zinc-ext-fmt-"));
    const file = path.join(dir, "hello_fmt.zn");
    fs.writeFileSync(file, formatted, "utf8");
    const result = await new CliCompilerBackend(zincCompilerPath()).checkFile(
      file
    );
    assert.equal(result.ok, true, result.stdout + result.stderr);
  });

  it("debug launch args compile hello.zn to a binary", (t) => {
    if (!zincAvailable()) {
      t.skip("zinc binary missing");
      return;
    }
    const hello = path.join(extensionRoot(), "examples/hello.zn");
    const outDir = fs.mkdtempSync(path.join(os.tmpdir(), "zinc-ext-run-"));
    const args = buildCompileArgs(hello, outDir);
    const proc = spawnSync(zincCompilerPath(), args, { encoding: "utf8" });
    assert.equal(proc.status, 0, proc.stdout + proc.stderr);
    const bin = binaryOutputPath(hello, outDir);
    assert.ok(fs.existsSync(bin), `missing binary ${bin}`);
    const run = spawnSync(bin, [], { encoding: "utf8" });
    assert.equal(run.status, 0, run.stdout + run.stderr);
    assert.match(run.stdout, /d=/);
  });
});
