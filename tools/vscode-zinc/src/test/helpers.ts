import fs from "node:fs";
import path from "node:path";

export function zincCompilerPath(): string {
  return (
    process.env.ZINC_COMPILER ??
    path.resolve(__dirname, "../../../../out/stage1/bin/zinc")
  );
}

export function zincAvailable(): boolean {
  return fs.existsSync(zincCompilerPath());
}

export function extensionRoot(): string {
  return path.resolve(__dirname, "../..");
}

export function readFixture(relativePath: string): string {
  return fs.readFileSync(path.join(extensionRoot(), relativePath), "utf8");
}
