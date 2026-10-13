import {
  LoggingDebugSession,
  InitializedEvent,
  TerminatedEvent,
  OutputEvent,
  Thread,
} from "@vscode/debugadapter";
import type { DebugProtocol } from "@vscode/debugprotocol";
import { spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { binaryOutputPath, buildCompileArgs } from "./lib/debugLaunch";

interface LaunchRequestArguments extends DebugProtocol.LaunchRequestArguments {
  program: string;
  args?: string[];
  cwd?: string;
  compilerPath?: string;
  outDir?: string;
}

class ZincDebugSession extends LoggingDebugSession {
  private static THREAD_ID = 1;
  private child: ChildProcessWithoutNullStreams | undefined;

  public constructor() {
    super("zinc-debug.txt");
    this.setDebuggerLinesStartAt1(true);
    this.setDebuggerColumnsStartAt1(true);
  }

  protected initializeRequest(
    response: DebugProtocol.InitializeResponse,
    _args: DebugProtocol.InitializeRequestArguments
  ): void {
    response.body = response.body ?? {};
    response.body.supportsConfigurationDoneRequest = false;
    response.body.supportsEvaluateForHovers = false;
    response.body.supportsStepBack = false;
    this.sendResponse(response);
    this.sendEvent(new InitializedEvent());
  }

  protected async launchRequest(
    response: DebugProtocol.LaunchResponse,
    args: LaunchRequestArguments
  ): Promise<void> {
    const program = args.program;
    const cwd = args.cwd ?? path.dirname(program);
    const outDir = args.outDir ?? path.join(cwd, "out");
    const compiler = args.compilerPath || process.env.ZINC_COMPILER || "zinc";

    try {
      fs.mkdirSync(outDir, { recursive: true });
      await this.compile(compiler, program, outDir, cwd);
      const binary = binaryOutputPath(program, outDir);
      if (!fs.existsSync(binary)) {
        throw new Error(`Compiled binary not found: ${binary}`);
      }
      this.sendEvent(new OutputEvent(`Running ${binary}\n`, "console"));
      this.runBinary(binary, args.args ?? [], cwd);
      this.sendResponse(response);
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      this.sendEvent(new OutputEvent(`${message}\n`, "stderr"));
      this.sendErrorResponse(response, 1, message);
      this.sendEvent(new TerminatedEvent());
    }
  }

  protected threadsRequest(response: DebugProtocol.ThreadsResponse): void {
    response.body = {
      threads: [new Thread(ZincDebugSession.THREAD_ID, "main")],
    };
    this.sendResponse(response);
  }

  protected continueRequest(
    response: DebugProtocol.ContinueResponse,
    _args: DebugProtocol.ContinueArguments
  ): void {
    response.body = { allThreadsContinued: true };
    this.sendResponse(response);
  }

  protected disconnectRequest(
    response: DebugProtocol.DisconnectResponse,
    _args: DebugProtocol.DisconnectArguments
  ): void {
    this.child?.kill();
    this.sendResponse(response);
  }

  private compile(
    compiler: string,
    program: string,
    outDir: string,
    cwd: string
  ): Promise<void> {
    const args = buildCompileArgs(program, outDir);
    return new Promise((resolve, reject) => {
      const child = spawn(compiler, args, { cwd });
      let stderr = "";
      child.stdout.on("data", (d: Buffer) => {
        this.sendEvent(new OutputEvent(d.toString("utf8"), "stdout"));
      });
      child.stderr.on("data", (d: Buffer) => {
        const text = d.toString("utf8");
        stderr += text;
        this.sendEvent(new OutputEvent(text, "stderr"));
      });
      child.on("error", (err) => reject(err));
      child.on("close", (code) => {
        if (code === 0) {
          resolve();
        } else {
          reject(new Error(stderr.trim() || `zinc exited with code ${code}`));
        }
      });
    });
  }

  private runBinary(binary: string, args: string[], cwd: string): void {
    this.child = spawn(binary, args, { cwd });
    this.child.stdout.on("data", (d: Buffer) => {
      this.sendEvent(new OutputEvent(d.toString("utf8"), "stdout"));
    });
    this.child.stderr.on("data", (d: Buffer) => {
      this.sendEvent(new OutputEvent(d.toString("utf8"), "stderr"));
    });
    this.child.on("close", () => {
      this.sendEvent(new TerminatedEvent());
    });
  }
}

ZincDebugSession.run(ZincDebugSession);
