import { spawn } from "node:child_process";

/**
 * Abstraction over compiler analysis. Today this shells out to the zinc CLI;
 * once frontend is available as a library, add a NativeCompilerBackend that
 * calls those APIs without changing the language server surface.
 */
export interface CheckResult {
  ok: boolean;
  stdout: string;
  stderr: string;
  exitCode: number | null;
}

export interface CompilerBackend {
  checkFile(filePath: string): Promise<CheckResult>;
}

export class CliCompilerBackend implements CompilerBackend {
  constructor(
    private readonly compilerPath: string,
    private readonly extraArgs: string[] = []
  ) {}

  checkFile(filePath: string): Promise<CheckResult> {
    const args = [
      "--check-only",
      "--speak=en",
      ...this.extraArgs,
      filePath,
    ];
    return new Promise((resolve) => {
      const child = spawn(this.compilerPath, args, {
        env: process.env,
      });
      let stdout = "";
      let stderr = "";
      child.stdout.on("data", (chunk: Buffer) => {
        stdout += chunk.toString("utf8");
      });
      child.stderr.on("data", (chunk: Buffer) => {
        stderr += chunk.toString("utf8");
      });
      child.on("error", (err) => {
        resolve({
          ok: false,
          stdout,
          stderr: `${stderr}\n${err.message}`,
          exitCode: null,
        });
      });
      child.on("close", (code) => {
        resolve({
          ok: code === 0,
          stdout,
          stderr,
          exitCode: code,
        });
      });
    });
  }
}
