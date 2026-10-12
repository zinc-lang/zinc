import * as path from "node:path";
import { runTests } from "@vscode/test-electron";

async function main(): Promise<void> {
  try {
    const extensionDevelopmentPath = path.resolve(__dirname, "../..");
    const extensionTestsPath = path.resolve(__dirname, "./suite/index");
    const fixturesPath = path.resolve(extensionDevelopmentPath, "examples");

    await runTests({
      extensionDevelopmentPath,
      extensionTestsPath,
      launchArgs: [
        fixturesPath,
        "--disable-extensions",
        "--disable-workspace-trust",
      ],
    });
  } catch (err) {
    console.error("Failed to run UI tests");
    console.error(err);
    process.exit(1);
  }
}

void main();
