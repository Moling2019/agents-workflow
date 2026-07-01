# Turn-end reply push (codex & opencode)

The dashboard's Last Output column is populated by an event-driven **push**
at turn end (mirroring Claude Code's Stop hook), not by polling session
storage. Two small external pieces wire the CLIs to Emacs; install them once
per machine.

## codex

`bin/agents-workflow-codex-notify` receives codex's `agent-turn-complete`
event (which carries `last-assistant-message`) and forwards it to
`emacsclient` → `agents-workflow-handle-codex-reply`. Enable it by adding a
top-level key to `~/.codex/config.toml` (before the first `[table]`):

```toml
notify = ["/ABS/PATH/TO/agents-workflow/bin/agents-workflow-codex-notify"]
```

The agent is matched by the codex process's working directory (inherited by
the notify subprocess).

## opencode

`opencode-plugin/agents-workflow.ts` listens for `session.idle`, fetches the
last assistant message via the in-process OpenCode client, and forwards it to
`emacsclient` → `agents-workflow-handle-opencode-reply`. Install it globally:

```sh
mkdir -p ~/.config/opencode/plugin
cp opencode-plugin/agents-workflow.ts ~/.config/opencode/plugin/
```

The agent is matched by session-id (backfilled on first push) then by
directory.

Both hooks are global (fire for every codex/opencode session); when no
agents-workflow agent matches, the Emacs receiver is a harmless no-op. If
Emacs isn't running, both fail silently.
