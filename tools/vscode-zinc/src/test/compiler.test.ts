import assert from "node:assert/strict";
import { describe, it } from "node:test";
import {
  CliCompilerBackend,
  type CheckResult,
  type CompilerBackend,
} from "../lib/compiler";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

describe("CliCompilerBackend", () => {
  const zincPath = process.env.ZINC_COMPILER
    ?? path.resolve(__dirname, "../../../../out/stage1/bin/zinc");

  it("exposes a CompilerBackend interface for future library reuse", () => {
    const backend: CompilerBackend = new CliCompilerBackend(zincPath);
    assert.equal(typeof backend.checkFile, "function");
  });

  it("reports diagnostics for an invalid file when zinc is available", async (t) => {
    if (!fs.existsSync(zincPath)) {
      t.skip(`zinc binary not found at ${zincPath}`);
      return;
    }
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "zinc-lsp-"));
    const file = path.join(dir, "bad.zn");
    fs.writeFileSync(file, 'fn main() { let x: Int = "bad"; }\n');
    const backend = new CliCompilerBackend(zincPath);
    const result: CheckResult = await backend.checkFile(file);
    assert.equal(result.ok, false);
    assert.ok(result.stdout.length + result.stderr.length > 0);
    assert.match(result.stdout + result.stderr, /Error|错误/);
  });

  it("succeeds for a valid check-only file when zinc is available", async (t) => {
    if (!fs.existsSync(zincPath)) {
      t.skip(`zinc binary not found at ${zincPath}`);
      return;
    }
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "zinc-lsp-"));
    const file = path.join(dir, "ok.zn");
    fs.writeFileSync(file, "fn main() {\n    let x: Int = 1;\n}\n");
    const backend = new CliCompilerBackend(zincPath);
    const result = await backend.checkFile(file);
    assert.equal(result.ok, true);
  });
});
