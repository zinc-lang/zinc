import path from "node:path";

export function binaryOutputPath(program: string, outDir: string): string {
  const base = path.basename(program, path.extname(program));
  return path.join(outDir, base);
}

export function buildCompileArgs(program: string, outDir: string): string[] {
  return [`--out-dir=${outDir}`, "--speak=en", program];
}
