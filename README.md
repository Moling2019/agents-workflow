# agents-workflow.el

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs: 30.0+](https://img.shields.io/badge/Emacs-30.0%2B-blueviolet.svg)](https://www.gnu.org/software/emacs/)

Orchestrate multiple AI coding agent sessions from Emacs. Run Claude Code
and OpenAI Codex CLI side by side with a unified dashboard, worktree
isolation, event-driven triggers, and autonomous background agents.

<!-- TODO: screenshot -->

## Features

- **Multi-agent dashboard** -- see all agents, their status, and last output
  in a single tabulated buffer with auto-refresh
- **Interactive + autonomous agents** -- interactive agents run in eat
  terminals; autonomous agents run headlessly via `--print` mode
- **Claude + Codex backends** -- each agent can use either Claude Code or
  OpenAI Codex CLI as its backend
- **Git worktree integration** -- agents can work in isolated worktrees
  created automatically from the project repo
- **Cross-agent communication** -- agents can ask each other questions and
  receive routed responses via `agents-workflow-ask-agent`
- **Event/trigger system** -- react to agent completion, status changes, and
  custom events with configurable trigger functions
- **Workflow definitions** -- declarative `.eld` files describe agents,
  directories, triggers, and panels
- **Session persistence** -- save and restore agent session IDs across Emacs
  restarts so agents resume where they left off
- **Optional panels** -- plug in Databricks job monitoring, Jira boards, or
  Slack message monitoring as dashboard panels

## Dependencies

- Emacs 30.0+
- [claude-code.el](https://github.com/stevemolitor/claude-code.el) (required)
- [all-the-icons](https://github.com/domtronn/all-the-icons.el) (required for dashboard icons)
- [eat](https://codeberg.org/akib/emacs-eat) (required by claude-code.el)

## Installation

### use-package with vc (Emacs 30+)

```elisp
(use-package agents-workflow
  :vc (:url "https://github.com/moling2019/agents-workflow.el"
       :rev :newest)
  :after claude-code
  :commands (agents-workflow-dashboard
             agents-workflow-start
             agents-workflow-stop
             agents-workflow-load-file
             agents-workflow-load-directory))
```

### Manual

Clone the repo and add it to your load path:

```elisp
(add-to-list 'load-path "/path/to/agents-workflow.el")
(require 'agents-workflow)
```

## Quick Start

### 1. Create a workflow definition

Save a `.eld` file (e.g., `~/.emacs.d/agents-workflow/projects/my-project.eld`):

```elisp
(:name "my-project"
 :directory "~/code/my-project"
 :agents
 ((:name "frontend"
   :type interactive
   :directory "~/code/my-project/frontend")
  (:name "backend"
   :type interactive
   :directory "~/code/my-project/backend")
  (:name "reviewer"
   :type autonomous
   :backend claude
   :system-prompt "You are a code reviewer. Review recent changes and report issues."
   :interval 300)))
```

### 2. Load and start

```
M-x agents-workflow-load-file RET my-project.eld RET
M-x agents-workflow-start RET my-project RET
M-x agents-workflow-dashboard
```

### 3. Interact

The dashboard shows all agents with their status. Click an agent name or
press `RET` to switch to its terminal buffer. Use the keybindings below
to manage the workflow.

### Keybindings (dashboard buffer)

| Key   | Command                         | Description                |
|-------|---------------------------------|----------------------------|
| `g`   | revert-buffer                   | Refresh dashboard          |
| `q`   | quit-window                     | Close dashboard            |
| `RET` | (on agent row)                  | Switch to agent buffer     |

### Commands

| Command                            | Description                                    |
|------------------------------------|------------------------------------------------|
| `agents-workflow-dashboard`        | Open the dashboard buffer                      |
| `agents-workflow-start`            | Start a registered workflow                    |
| `agents-workflow-stop`             | Stop a running workflow                        |
| `agents-workflow-pause`            | Pause event/trigger processing                 |
| `agents-workflow-resume`           | Resume event/trigger processing                |
| `agents-workflow-load-file`        | Load a `.eld` workflow definition              |
| `agents-workflow-load-directory`   | Load all `.eld` files from a directory         |
| `agents-workflow-save-state`       | Save session state (agent IDs, status)         |
| `agents-workflow-load-state`       | Restore session state                          |
| `agents-workflow-clear-state`      | Clear saved session state                      |
| `agents-workflow-ask-agent`        | Send a question from one agent to another      |

## Configuration

Key `defcustom` variables:

| Variable                                  | Default                                       | Description                                |
|-------------------------------------------|-----------------------------------------------|--------------------------------------------|
| `agents-workflow-projects-directory`      | `~/.emacs.d/agents-workflow/projects`         | Directory for `.eld` workflow definitions  |
| `agents-workflow-elisp-directory`         | `~/.emacs.d/agents-workflow/elisp`            | Directory for convention-based system prompts |
| `agents-workflow-last-output-lines`       | `100`                                         | Lines to extract from agent buffer for dashboard |
| `agents-workflow-auto-save-state`         | `t`                                           | Auto-save session state on workflow changes |

### Agent types

Each agent in a workflow definition supports these properties:

| Property             | Values                        | Description                                |
|----------------------|-------------------------------|--------------------------------------------|
| `:name`              | string                        | Agent identifier                           |
| `:type`              | `interactive` / `autonomous`  | Terminal or headless                       |
| `:backend`           | `claude` / `codex`            | CLI tool to use                            |
| `:directory`         | path                          | Working directory                          |
| `:system-prompt`     | string                        | Inline system prompt                       |
| `:system-prompt-file`| path                          | Path to system prompt file                 |
| `:prompt-template`   | string                        | Template for autonomous agent prompts      |
| `:interval`          | number                        | Seconds between autonomous runs            |
| `:timeout`           | number                        | Seconds before autonomous agent times out  |
| `:worktree`          | `t` / `nil`                   | Create a git worktree for this agent       |
| `:extra-directories` | list of paths                 | Additional directories to pass to the CLI  |

### Optional panels

The dashboard can include optional panels. Enable them in your workflow
definition:

```elisp
(:name "my-project"
 :directory "~/code/my-project"
 :panels ("databricks" "jira" "slack")
 :agents (...))
```

Each panel has its own `defcustom` variables for configuration:

- **databricks-runs** -- `databricks-runs-python`, `databricks-runs-env-file`, `databricks-runs-cli-profile`, `databricks-runs-repo-dir`
- **jira-board** -- `jira-board-python`, `jira-board-env-file`, `jira-board-site`, `jira-board-project`
- **slack-monitor** -- `slack-monitor-cache-file`, `slack-monitor-ignore-file`
- **github-prs** -- `github-prs-author`, `github-prs-state`, `github-prs-limit`, `github-prs-refresh-interval`, `github-prs-enrich`, `github-prs-gh-program`

### Personalization vs. package defaults

This package ships with **generic defaults** -- most panel variables default to
`nil` or a neutral value like `"python3"` because they refer to site-specific
resources (an internal Atlassian tenant, a Databricks CLI profile, a Python
venv that has the right libs installed, a `.env` file with credentials).

Every user must set these in their own init file. If a panel shows "Wrong type
argument: stringp, nil" on refresh, one of its required variables is unset.

Example `use-package` block (adapt paths for your setup):

```elisp
(use-package jira-board
  :custom
  (jira-board-python    "~/my-project/.venv/bin/python") ; needs `requests`
  (jira-board-env-file  "~/my-project/.env")             ; JIRA_EMAIL + JIRA_API_TOKEN
  (jira-board-site      "yourcompany.atlassian.net")
  (jira-board-project   "PROJ"))

(use-package databricks-runs
  :custom
  (databricks-runs-python      "~/my-project/.venv/bin/python") ; with databricks SDK
  (databricks-runs-env-file    "~/my-project/.env")
  (databricks-runs-cli-profile "your-databricks-profile")       ; from ~/.databrickscfg
  (databricks-runs-repo-dir    "~/my-project"))                 ; where utils.databricks_job_submit lives
```

## Architecture

```
agents-workflow.el          Main package: workflow engine, agent lifecycle,
                            event system, session persistence
claude-dashboard.el         Generic panel-based dashboard framework
codex-cli.el                OpenAI Codex CLI terminal management
databricks-runs.el          Optional: Databricks job run monitoring panel
jira-board.el               Optional: Jira issue board panel
slack-monitor.el            Optional: Slack message monitoring panel
github-prs.el               Optional: GitHub PRs monitoring panel
```

`agents-workflow.el` depends on `claude-code.el` for Claude backend support
and `codex-cli.el` for Codex backend support. The dashboard is built on
`claude-dashboard.el`, a generic composable panel framework. Optional panels
(`databricks-runs`, `jira-board`, `slack-monitor`) are loaded with
`(require ... nil t)` and degrade gracefully when absent or unconfigured.

### Status detection

Agent status is detected via terminal title sequences (OSC 0). Claude Code
sets the terminal title to spinner characters while working and a special
idle marker when waiting for input. `agents-workflow` hooks into the
terminal's title-change callback to track transitions between `running`,
`idle`, and `waiting` states.

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
