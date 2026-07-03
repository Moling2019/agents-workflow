// agents-workflow integration: push structured status + reply to Emacs.
//
// Uses omp's ExtensionAPI (loaded via --hook, which omp treats as --extension).
// The factory receives `pi: ExtensionAPI` and registers event handlers with
// pi.on(...).  No screen scraping — status and replies come from deterministic
// lifecycle events.
//
// Events used:
// - before_provider_request → mark agent "working" (turn started)
// - turn_end                → mark agent "idle" + push last assistant reply
//
// The agent identity (AGENTS_WORKFLOW_AGENT / AGENTS_WORKFLOW_NAME, set by
// agents-workflow at launch) is forwarded so the push routes to the right
// agent even when several omp agents share one worktree.  Harmless no-op
// when Emacs isn't running or no matching agent exists.
//
// Load with: omp --hook /path/to/agents-workflow-omp-hook.ts

import type { ExtensionAPI } from "@oh-my-pi/pi-coding-agent";

const agent = process.env.AGENTS_WORKFLOW_AGENT ?? "";
const wf = process.env.AGENTS_WORKFLOW_NAME ?? "";

function emacsEval(form: string, ...args: string[]) {
  const cmd = ["emacsclient", "--eval", form, ...args];
  try {
    Bun.spawn(cmd, { stdout: "ignore", stderr: "ignore" });
  } catch {
    /* emacs not running */
  }
}

function pushStatus(status: string, directory: string) {
  const form =
    `(agents-workflow-handle-omp-status ` +
    `${JSON.stringify(status)} ${JSON.stringify(directory)} ` +
    `${JSON.stringify(agent)} ${JSON.stringify(wf)})`;
  emacsEval(form);
}

interface MessageEntry {
  type: "message";
  message: {
    role: string;
    content: string | Array<{ type: string; text?: string } | string>;
  };
}

function isMessageEntry(v: unknown): v is MessageEntry {
  return (
    typeof v === "object" &&
    v !== null &&
    "type" in v &&
    v.type === "message" &&
    "message" in v &&
    typeof (v as MessageEntry).message === "object" &&
    "role" in (v as MessageEntry).message
  );
}

function extractAssistantText(entries: unknown[]): string {
  let text = "";
  for (const entry of entries) {
    if (!isMessageEntry(entry)) continue;
    if (entry.message.role !== "assistant") continue;
    const content = entry.message.content;
    if (typeof content === "string") {
      if (content.trim()) text = content;
      continue;
    }
    if (!Array.isArray(content)) continue;
    const parts = content
      .filter(
        (p) =>
          typeof p === "string" ||
          (typeof p === "object" && p !== null && p.type === "text" && typeof p.text === "string"),
      )
      .map((p) => (typeof p === "string" ? p : p.text ?? ""))
      .join("");
    if (parts.trim()) text = parts;
  }
  return text;
}

function getSessionId(sm: unknown): string {
  if (typeof sm !== "object" || sm === null) return "";
  if (!("getSessionFile" in sm) || typeof sm.getSessionFile !== "function") return "";
  const file = sm.getSessionFile() as string;
  if (!file) return "";
  const base = file.split("/").pop() ?? "";
  // <timestamp>_<sessionId>.jsonl
  const match = base.match(/^[^_]+_(.+)\.jsonl$/);
  return match ? match[1] : "";
}

function getBranch(sm: unknown): unknown[] {
  if (typeof sm !== "object" || sm === null) return [];
  if (!("getBranch" in sm) || typeof sm.getBranch !== "function") return [];
  const result = sm.getBranch();
  return Array.isArray(result) ? result : [];
}

export default function (pi: ExtensionAPI) {
  pi.on("before_provider_request", async (_event, ctx) => {
    pushStatus("working", ctx.cwd);
  });

  pi.on("turn_end", async (_event, ctx) => {
    const directory = ctx.cwd;
    const sessionId = getSessionId(ctx.sessionManager);
    const entries = getBranch(ctx.sessionManager);
    const text = extractAssistantText(entries);

    const form =
      `(agents-workflow-handle-omp-reply ` +
      `${JSON.stringify(sessionId)} ${JSON.stringify(directory)} ` +
      `${JSON.stringify(agent)} ${JSON.stringify(wf)})`;
    emacsEval(form, text);
    pushStatus("idle", directory);
  });
}
