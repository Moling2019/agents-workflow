// agents-workflow integration: push the model's finished reply to Emacs.
//
// On `session.idle' (turn end) this fetches the session's last assistant
// message text via the in-process OpenCode client (cheap local HTTP — no
// extra `opencode export' process) and forwards it to Emacs, mirroring
// Claude Code's Stop hook.  The text is passed as a trailing emacsclient
// arg and read from `server-eval-args-left' by
// `agents-workflow-handle-opencode-reply'.  The agent identity
// (AGENTS_WORKFLOW_AGENT / AGENTS_WORKFLOW_NAME, set by agents-workflow at
// launch) is forwarded too, so the push routes to the right agent even when
// several opencode agents share one worktree.  Harmless no-op when Emacs
// isn't running or no matching agent exists.
import type { Plugin } from "@opencode-ai/plugin"

export const AgentsWorkflow: Plugin = async ({ client, $, directory }) => {
  return {
    event: async ({ event }: { event: any }) => {
      if (event?.type !== "session.idle") return
      const sessionID = event?.properties?.sessionID
      if (!sessionID) return
      let text = ""
      try {
        const res: any = await client.session.messages({ path: { id: sessionID } })
        const msgs: any[] = res?.data ?? res ?? []
        for (const m of msgs) {
          if (m?.info?.role === "assistant") {
            const t = (m?.parts ?? [])
              .filter((p: any) => p?.type === "text")
              .map((p: any) => p?.text ?? "")
              .join("")
            if (t.trim()) text = t
          }
        }
      } catch (_) { /* ignore fetch errors */ }
      const agent = process.env.AGENTS_WORKFLOW_AGENT ?? ""
      const wf = process.env.AGENTS_WORKFLOW_NAME ?? ""
      const form =
        `(agents-workflow-handle-opencode-reply ` +
        `${JSON.stringify(sessionID)} ${JSON.stringify(directory ?? "")} ` +
        `${JSON.stringify(agent)} ${JSON.stringify(wf)})`
      try {
        await $`emacsclient --eval ${form} ${text}`.quiet().nothrow()
      } catch (_) { /* emacs not running / no emacsclient */ }
    },
  }
}
