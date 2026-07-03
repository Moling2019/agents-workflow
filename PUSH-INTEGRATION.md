# Turn-end reply push (codex, opencode & omp)

The dashboard's Last Output column is populated by an event-driven **push**
at turn end (mirroring Claude Code's Stop hook), not by polling session
storage. Three small external pieces wire the CLIs to Emacs; install them
once per machine.

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

## omp

`omp-plugin/agents-workflow-omp-hook.ts` listens for omp's turn-complete
event and forwards the last assistant message to `emacsclient` →
`agents-workflow-handle-omp-reply`.  Load it by adding `--hook
/ABS/PATH/TO/agents-workflow/omp-plugin/agents-workflow-omp-hook.ts` to
the omp launch flags (agents-workflow does this automatically).

The agent is matched by (workflow, agent-name) then session-id then
directory.

Both hooks are global (fire for every codex/opencode/omp session); when no
agents-workflow agent matches, the Emacs receiver is a harmless no-op. If
Emacs isn't running, all three fail silently.
