;;; agents-workflow.el --- Orchestrate multiple AI coding agent sessions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Moling Zhang
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (claude-code "0.4.5") (all-the-icons "5.0.0"))
;; Keywords: tools, ai
;; URL: https://github.com/moling2019/agents-workflow.el

;;; Commentary:
;; An Emacs package for orchestrating multiple AI coding agent sessions
;; (Claude Code, OpenAI Codex CLI) as a coordinated workflow.  Tracks
;; interactive coding agents alongside autonomous background agents with
;; a unified dashboard, event/trigger system, and optional monitoring panels.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'all-the-icons)
(require 'claude-dashboard)
(require 'codex-cli)
(require 'databricks-runs nil t)
(require 'jira-board nil t)
(require 'slack-monitor nil t)

;; Forward declarations for claude-code.el
(declare-function claude-code "claude-code")
(declare-function claude-code-new-instance "claude-code")
(declare-function claude-code--find-all-claude-buffers "claude-code")
(declare-function claude-code--find-claude-buffers-for-directory "claude-code")
(declare-function claude-code--buffer-p "claude-code")
(declare-function claude-code--do-send-command "claude-code")
(declare-function claude-code--extract-directory-from-buffer-name "claude-code")
(declare-function claude-code--extract-instance-name-from-buffer-name "claude-code")
(declare-function claude-code--start "claude-code")
(declare-function claude-code--notify "claude-code")
(declare-function claude-code--buffer-name "claude-code")
(declare-function claude-code--term-make "claude-code")
(declare-function claude-code--term-configure "claude-code")
(declare-function claude-code--term-setup-keymap "claude-code")
(declare-function claude-code--term-customize-faces "claude-code")
(defvar claude-code-event-hook)
(defvar claude-code-terminal-backend)
(defvar claude-code-program)
(defvar claude-code-program-switches)
(defvar claude-code-process-environment-functions)
(defvar claude-code-start-hook)

;; Forward declare defcustoms used before their definitions
(defvar agents-workflow-elisp-directory)
(defvar agents-workflow-last-output-lines)

;; Forward declarations for eat
(declare-function eat-term-set-parameter "eat")
(defvar eat-terminal)

;;;; Customization group
(defgroup agents-workflow nil
  "Orchestrate multiple AI coding agent sessions."
  :group 'tools
  :prefix "agents-workflow-")

;;;; Core data structures

(cl-defstruct agents-workflow-agent
  "An agent managed by the workflow system.
TYPE is either `interactive' (eat terminal via claude-code.el) or
`autonomous' (background process via claude --print).
BACKEND selects the CLI tool: `claude' (default) or `codex'."
  (name "" :type string)
  (type 'interactive :type symbol)
  (backend 'claude :type symbol)
  (status 'idle :type symbol)
  (directory "" :type string)
  (buffer nil)
  (last-output nil :type (or null string))
  (last-activity nil)
  (on-complete nil :type (or null function))
  (metadata nil :type list)
  ;; Autonomous agent fields (also used by interactive agents)
  (system-prompt nil :type (or null string))
  (system-prompt-file nil :type (or null string))
  (prompt-template nil :type (or null string))
  (interval nil :type (or null number))
  (timeout 120 :type number)
  (queue nil :type list)
  ;; Session persistence
  (session-id nil :type (or null string))
  ;; Git worktree support
  (worktree-path nil :type (or null string))
  ;; Cross-repo support
  (extra-directories nil :type list)
  ;; Internal
  (process nil)
  (timer nil))

(cl-defstruct agents-workflow
  "A workflow orchestrating multiple agents."
  (name "" :type string)
  (directory "" :type string)
  (context-file nil :type (or null string))
  (agents nil :type list)
  (triggers nil :type list)
  (panels nil :type list)
  (state 'stopped :type symbol)
  (paused nil :type boolean))

(defun agents-workflow-agent-all-directories (agent)
  "Return all directories for AGENT: primary + extras."
  (cons (agents-workflow-agent-directory agent)
        (mapcar (lambda (ed) (plist-get ed :directory))
                (agents-workflow-agent-extra-directories agent))))

(defun agents-workflow-agent-extra-dir-count (agent)
  "Return count of extra directories for AGENT."
  (length (agents-workflow-agent-extra-directories agent)))

(defvar agents-workflow--registry (make-hash-table :test 'equal)
  "Hash table of defined workflows, keyed by name.")

(defvar agents-workflow-panel-registry
  '(("databricks" . databricks-runs-panel)
    ("jira" . jira-board-panel)
    ("slack" . slack-monitor-panel)
    ("github" . github-prs-panel))
  "Alist mapping panel name strings to constructor functions.
Each constructor is a zero-arg function returning a panel plist.")

(defvar agents-workflow--expanded-agents (make-hash-table :test 'equal)
  "Hash table of agent names currently showing expanded directory view.")

;;;; Backend abstraction

(defconst agents-workflow-backend-registry
  '((claude
     :title-idle-pattern "✳"
     :dashboard-types
     ((interactive :icon "computer" :face (:foreground "#89b4fa"))
      (configured  :icon "settings" :face (:foreground "#fab387"))
      (autonomous  :icon "memory"   :face (:foreground "#cba6f7"))))
    (codex
     :title-idle-pattern nil
     :dashboard-types
     ((interactive :icon "code"    :face (:foreground "#10a37f"))
      (configured  :icon "tune"    :face (:foreground "#74aa9c"))
      (autonomous  :icon "android" :face (:foreground "#10a37f")))))
  "Backend registry mapping symbols to configuration plists.
Program and switches are managed via defcustoms, not stored here.")

(defun agents-workflow--backend-get (backend key)
  "Get KEY from BACKEND's registry entry."
  (plist-get (alist-get backend agents-workflow-backend-registry) key))

(defun agents-workflow--backend-program (backend)
  "Return the program name for BACKEND."
  (pcase backend
    ('claude claude-code-program)
    ('codex  codex-cli-program)
    (_ (error "Unknown backend: %s" backend))))

(defun agents-workflow--backend-base-switches (backend)
  "Return the base CLI switches for BACKEND."
  (pcase backend
    ('claude claude-code-program-switches)
    ('codex  codex-cli-program-switches)
    (_ nil)))

(defun agents-workflow--backend-type-config (backend type)
  "Return the dashboard config plist for TYPE under BACKEND."
  (let ((types (agents-workflow--backend-get backend :dashboard-types)))
    (alist-get type types)))

(defun agents-workflow--get (name)
  "Get workflow by NAME from registry."
  (gethash name agents-workflow--registry))

(defun agents-workflow--remove (name)
  "Remove workflow NAME from registry."
  (remhash name agents-workflow--registry))

(defun agents-workflow--read-prompt-file (path)
  "Read and return trimmed contents of prompt file at PATH, or nil."
  (when (and path (file-exists-p (expand-file-name path)))
    (with-temp-buffer
      (insert-file-contents (expand-file-name path))
      (string-trim (buffer-string)))))

(defun agents-workflow--resolve-system-prompt (plist)
  "Resolve the system prompt from PLIST.
If `:system-prompt' is set, use it directly.
If `:system-prompt-file' is set, read the file contents.
Returns the prompt string or nil."
  (or (plist-get plist :system-prompt)
      (agents-workflow--read-prompt-file
       (plist-get plist :system-prompt-file))))

(defun agents-workflow--convention-system-prompt (agent-name)
  "Look up a system prompt by convention for AGENT-NAME.
Checks `agents-workflow-elisp-directory'/AGENT-NAME/system-prompt.md."
  (agents-workflow--read-prompt-file
   (expand-file-name (format "%s/system-prompt.md" agent-name)
                     agents-workflow-elisp-directory)))

(defun agents-workflow--plist-to-agent (plist directory)
  "Create an agent struct from a PLIST spec with DIRECTORY.
If PLIST contains a `:directory' key, use that instead of DIRECTORY."
  (make-agents-workflow-agent
   :name (plist-get plist :name)
   :type (or (plist-get plist :type) 'interactive)
   :backend (or (plist-get plist :backend) 'claude)
   :status 'idle
   :directory (or (plist-get plist :directory) directory)
   :system-prompt (agents-workflow--resolve-system-prompt plist)
   :system-prompt-file (plist-get plist :system-prompt-file)
   :prompt-template (plist-get plist :prompt-template)
   :interval (plist-get plist :interval)
   :timeout (or (plist-get plist :timeout) 120)
   :metadata (plist-get plist :metadata)
   :worktree-path (plist-get plist :worktree-path)
   :extra-directories (plist-get plist :extra-directories)))

(cl-defun agents-workflow-define (name &key directory context-file agents triggers panels)
  "Define a workflow NAME with DIRECTORY, CONTEXT-FILE, AGENTS, TRIGGERS, PANELS.
AGENTS is a list of plists, each describing an agent.
TRIGGERS is a list of trigger plists.
PANELS is a list of panel name strings (e.g. (\"databricks\")) selecting
which optional panels to include in the dashboard.
CONTEXT-FILE, if non-nil, is a path (relative to DIRECTORY or absolute)
to a markdown file whose contents are sent to every agent on startup."
  (let* ((dir (expand-file-name (or directory default-directory)))
         (agent-structs (mapcar (lambda (spec) (agents-workflow--plist-to-agent spec dir))
                                agents))
         (wf (make-agents-workflow
              :name name
              :directory dir
              :context-file context-file
              :agents agent-structs
              :triggers triggers
              :panels panels
              :state 'stopped)))
    (puthash name wf agents-workflow--registry)
    wf))

;;;; Event system

(defun agents-workflow--trigger-matches-p (trigger event)
  "Return non-nil if TRIGGER matches EVENT.
TRIGGER is a plist with :on, optional :from, optional :when,
optional :match (regex against :output), and :do."
  (and (eq (plist-get trigger :on) (plist-get event :event))
       (or (null (plist-get trigger :from))
           (equal (plist-get trigger :from) (plist-get event :agent)))
       (or (null (plist-get trigger :match))
           (let ((output (or (plist-get event :output) "")))
             (string-match-p (plist-get trigger :match) output)))
       (or (null (plist-get trigger :when))
           (funcall (plist-get trigger :when) event))))

(defun agents-workflow--emit (workflow event)
  "Emit EVENT in WORKFLOW, dispatching to matching triggers.
Does nothing if workflow is paused or not running."
  (when (and (eq (agents-workflow-state workflow) 'running)
             (not (agents-workflow-paused workflow)))
    (dolist (trigger (agents-workflow-triggers workflow))
      (when (agents-workflow--trigger-matches-p trigger event)
        (funcall (plist-get trigger :do) workflow event)))))

;;;; Dashboard formatting

(defconst agents-workflow--status-config
  '((running  :icon-fn agents-workflow--spinner-icon :face (:foreground "#a6e3a1"))
    (waiting  :icon "local_cafe"                     :face (:foreground "#f9e2af"))
    (idle     :icon "brightness_3"                   :face (:foreground "#6c7086"))
    (polling  :icon "sync"                           :face (:foreground "#89b4fa"))
    (error    :icon "error_outline"                  :face (:foreground "#f38ba8"))
    (done     :icon "check_circle"                   :face (:foreground "#a6e3a1")))
  "Status display configuration: icon name and face for each status.")

(defconst agents-workflow--type-config
  '((interactive :icon "computer" :face (:foreground "#89b4fa"))
    (configured  :icon "settings" :face (:foreground "#fab387"))
    (autonomous  :icon "memory"   :face (:foreground "#cba6f7")))
  "Agent type display configuration: icon and face.
`configured' is a display-only type for interactive agents that have
a custom system prompt (explicit or convention-based).")

(defconst agents-workflow--state-config
  '((running :label "Running" :face (:foreground "#1e1e2e" :background "#a6e3a1"))
    (stopped :label "Stopped" :face (:foreground "#1e1e2e" :background "#6c7086"))
    (error   :label "Error"   :face (:foreground "#1e1e2e" :background "#f38ba8")))
  "Workflow state badge configuration.")

(defun agents-workflow--spinner-icon ()
  "Return the current spinner frame via claude-dashboard."
  (claude-dashboard-spinner))

(defun agents-workflow--status-label (status)
  "Return a propertized icon for STATUS.
Uses `all-the-icons-material' with :face to preserve the icon font
family while applying the status color."
  (let* ((config (alist-get status agents-workflow--status-config))
         (fg (plist-get (plist-get config :face) :foreground))
         (icon-fn (plist-get config :icon-fn))
         (icon-name (plist-get config :icon)))
    (cond
     (icon-fn (propertize (funcall icon-fn) 'face `(:foreground ,fg)))
     (icon-name (condition-case nil
                    (all-the-icons-material icon-name :face `(:foreground ,fg))
                  (error (propertize "?" 'face `(:foreground ,fg)))))
     (t (propertize "?" 'face `(:foreground ,fg))))))

(defun agents-workflow--effective-type (agent)
  "Return the display type for AGENT.
Interactive agents with a custom system prompt (explicit or
convention-based) display as `configured'."
  (let ((type (agents-workflow-agent-type agent)))
    (if (and (eq type 'interactive)
             (or (agents-workflow-agent-system-prompt agent)
                 (agents-workflow--convention-system-prompt
                  (agents-workflow-agent-name agent))))
        'configured
      type)))

(defun agents-workflow--type-label (type &optional backend)
  "Return a propertized icon for agent TYPE.
When BACKEND is provided, look up from backend registry; otherwise
fall back to the default `agents-workflow--type-config'."
  (let* ((config (if backend
                     (agents-workflow--backend-type-config backend type)
                   (alist-get type agents-workflow--type-config)))
         (fg (plist-get (plist-get config :face) :foreground))
         (icon-name (plist-get config :icon)))
    (condition-case nil
        (all-the-icons-material icon-name :face `(:foreground ,fg))
      (error (propertize "?" 'face `(:foreground ,fg))))))

(defun agents-workflow--agent-name-label (agent)
  "Return a propertized agent name with type-appropriate icon."
  (let* ((type (agents-workflow--effective-type agent))
         (backend (agents-workflow-agent-backend agent))
         (config (or (agents-workflow--backend-type-config backend type)
                     (alist-get type agents-workflow--type-config)))
         (face (plist-get config :face))
         (name (agents-workflow-agent-name agent)))
    (propertize name 'face `(:foreground ,(plist-get face :foreground) :weight bold))))

(defun agents-workflow--state-badge (state)
  "Return a propertized badge string for workflow STATE."
  (let* ((config (alist-get state agents-workflow--state-config))
         (label (or (plist-get config :label) (symbol-name state)))
         (face (plist-get config :face)))
    (propertize (format " %s " label) 'face face)))


(defun agents-workflow--format-last-activity (timestamp)
  "Format TIMESTAMP as a relative time string."
  (if (null timestamp)
      (propertize "—" 'face '(:foreground "#585b70"))
    (let* ((delta (- (float-time) timestamp))
           (text (cond
                  ((< delta 60) (format "%ds ago" (truncate delta)))
                  ((< delta 3600) (format "%dm ago" (truncate (/ delta 60))))
                  ((< delta 86400) (format "%dh ago" (truncate (/ delta 3600))))
                  (t (format "%dd ago" (truncate (/ delta 86400)))))))
      (propertize text 'face '(:foreground "#a6adc8")))))

(defun agents-workflow--chrome-line-p (line)
  "Return non-nil if LINE is Claude Code UI chrome, not agent prose."
  (let ((trimmed (string-trim line)))
    (or (string-empty-p trimmed)
        ;; Separator lines (all dashes/box-drawing, possibly with agent name)
        (string-match-p "\\`[─│┌┐└┘├┤┬┴┼╭╮╰╯━┃═║ a-zA-Z0-9_-]*\\'"
                        trimmed)
        ;; But only if it's mostly box-drawing (>50% non-alnum)
        (and (string-match-p "[─━═]" trimmed)
             (> (length (replace-regexp-in-string "[[:alnum:]_ -]" "" trimmed))
                (/ (length trimmed) 2)))
        ;; Status bar
        (string-match-p "⏵" trimmed)
        ;; Tool calls and results
        (string-match-p "\\`⏺" trimmed)
        (string-match-p "\\`⎿" trimmed)
        ;; User prompt
        (string-match-p "\\`❯" trimmed)
        ;; Thinking / completion / status markers
        (string-match-p "\\`[✶✻✢✽·]" trimmed)
        ;; Action menus
        (string-match-p "\\`→ \\[" trimmed)
        ;; Spinner-only lines
        (string-match-p "\\`[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏⠂⠐✳● ]*\\'" trimmed)
        ;; Error noise from terminal
        (string-match-p "\\`\\*ERROR\\*" trimmed)
        ;; No alphanumeric content at all
        (not (string-match-p "[[:alnum:]]" trimmed)))))

(defun agents-workflow--last-meaningful-line (text)
  "Extract the first line of the last prose block from TEXT.
Strips terminal escapes, then finds the last contiguous block of
non-chrome lines and returns its first line.  This gives the opening
sentence of the agent's last response rather than a trailing fragment."
  (when (and text (not (string-empty-p text)))
    (let* (;; Strip OSC sequences: ESC ] ... BEL/ST
           (clean (replace-regexp-in-string "\033\\][^\007\033]*[\007\033\\\\]?" "" text))
           ;; Strip CSI sequences: ESC [ ... letter
           (clean (replace-regexp-in-string "\033\\[[0-9;]*[a-zA-Z]" "" clean))
           ;; Strip remaining control chars except newline
           (clean (replace-regexp-in-string "[[:cntrl:]&&[^\n]]" "" clean))
           (lines (split-string clean "\n"))
           ;; Find the last contiguous block of prose lines
           (block-start nil)
           (block-end nil)
           (in-prose nil)
           (i (1- (length lines))))
      ;; Scan backwards: find end of last prose block, then its start
      (while (>= i 0)
        (let ((chrome (agents-workflow--chrome-line-p (nth i lines))))
          (cond
           ;; Haven't found prose yet — skip chrome
           ((and (not block-end) chrome)
            nil)
           ;; Found first prose line (from bottom)
           ((and (not block-end) (not chrome))
            (setq block-end i
                  in-prose t))
           ;; In prose block, still prose — extend start
           ((and in-prose (not chrome))
            nil)
           ;; In prose block, hit chrome — block starts at next line
           ((and in-prose chrome)
            (setq block-start (1+ i)
                  in-prose nil))))
        (cl-decf i))
      ;; If we never hit chrome before reaching top, block starts at 0
      (when (and block-end (not block-start))
        (setq block-start (if in-prose 0 block-end)))
      (when block-start
        (string-trim (nth block-start lines))))))

(defun agents-workflow--last-output-column-offset ()
  "Compute the character offset where the Last Output column starts.
Sums widths and padding of all preceding columns from the agents
panel definition."
  (let ((offset 2)  ;; leading indent
        (columns [("Agent" 16 t) ("T" 3 t) ("S" 3 nil) ("Dir" 18 nil) ("Activity" 10 nil)])
        (padding 2))
    (cl-loop for col across columns
             do (setq offset (+ offset (cadr col) padding)))
    offset))

(defun agents-workflow--format-output (text _max-width)
  "Extract the last meaningful line from TEXT.
Returns a propertized string with subdued face and wrap-prefix
so wrapped lines align with the Last Output column."
  (let* ((line (agents-workflow--last-meaningful-line text))
         (out (if (or (null line) (string-empty-p line))
                  "—"
                line))
         (prefix (make-string (agents-workflow--last-output-column-offset) ?\s)))
    (propertize out
                'face '(:foreground "#9399b2")
                'wrap-prefix prefix)))

;;;; Dashboard entries and header

(defun agents-workflow--dir-label (agent)
  "Return a propertized directory label for AGENT.
If AGENT has a worktree-path, show the worktree name with a branch icon
colored cyan.  Otherwise show the basename of the agent's directory in gray.
Appends (+N) badge when extra-directories is non-empty."
  (let* ((wt (agents-workflow-agent-worktree-path agent))
         (base (if wt
                   (let ((name (file-name-nondirectory (directory-file-name wt)))
                         (icon (condition-case nil (all-the-icons-octicon "git-branch") (error ""))))
                     (propertize (format "%s %s" icon name) 'face '(:foreground "#89dceb")))
                 (propertize (file-name-nondirectory
                              (directory-file-name (agents-workflow-agent-directory agent)))
                             'face '(:foreground "#6c7086"))))
         (n (agents-workflow-agent-extra-dir-count agent)))
    (if (> n 0)
        (concat base (propertize (format " (+%d)" n) 'face '(:foreground "#6c7086")))
      base)))

(defun agents-workflow--extra-dir-labels (agent)
  "Return list of propertized labels for AGENT's extra directories.
Each label is prefixed with a tree character."
  (let* ((extras (agents-workflow-agent-extra-directories agent))
         (len (length extras))
         (labels nil))
    (dotimes (i len)
      (let* ((ed (nth i extras))
             (dir (plist-get ed :directory))
             (wt (plist-get ed :worktree-path))
             (is-last (= i (1- len)))
             (prefix (propertize (if is-last "└ " "├ ") 'face '(:foreground "#45475a")))
             (name (if wt
                       (let ((icon (condition-case nil (all-the-icons-octicon "git-branch") (error ""))))
                         (propertize (format "%s %s" icon (file-name-nondirectory (directory-file-name wt)))
                                     'face '(:foreground "#89dceb")))
                     (propertize (file-name-nondirectory (directory-file-name dir))
                                 'face '(:foreground "#6c7086")))))
        (push (concat prefix name) labels)))
    (nreverse labels)))

(defun agents-workflow--dashboard-entries (workflow)
  "Generate dashboard entries from WORKFLOW agents.
Returns a list of (ID [col-values ...]) for the agents panel :entries function.
For agents with a live buffer, extracts fresh output on each refresh
cycle rather than relying solely on the title-watcher event.
When an agent is expanded, extra directory sub-rows are appended."
  (mapcan
   (lambda (agent)
     (let ((buf (agents-workflow-agent-buffer agent)))
       (if (and buf (buffer-live-p buf))
           (setf (agents-workflow-agent-last-output agent)
                 (agents-workflow--extract-last-output
                  buf agents-workflow-last-output-lines))
         (when (and (eq (agents-workflow-agent-type agent) 'interactive)
                    buf
                    (not (eq (agents-workflow-agent-status agent) 'idle)))
           (setf (agents-workflow-agent-status agent) 'idle)
           (setf (agents-workflow-agent-buffer agent) nil))))
     (let ((main-row
            (list (agents-workflow-agent-name agent)
                  (vector
                   (agents-workflow--agent-name-label agent)
                   (agents-workflow--type-label (agents-workflow--effective-type agent)
                                               (agents-workflow-agent-backend agent))
                   (agents-workflow--status-label (agents-workflow-agent-status agent))
                   (agents-workflow--dir-label agent)
                   (agents-workflow--format-last-activity
                    (agents-workflow-agent-last-activity agent))
                   (agents-workflow--format-output
                    (agents-workflow-agent-last-output agent)
                    0)))))
       (if (and (gethash (agents-workflow-agent-name agent)
                         agents-workflow--expanded-agents)
                (> (agents-workflow-agent-extra-dir-count agent) 0))
           ;; Expanded: main row + sub-rows
           (let ((sub-rows
                  (let ((idx 0))
                    (mapcar (lambda (label)
                              (prog1
                                  (list (format "%s::extra-%d"
                                               (agents-workflow-agent-name agent) idx)
                                        (vector "" "" "" label "" ""))
                                (cl-incf idx)))
                            (agents-workflow--extra-dir-labels agent)))))
             (cons main-row sub-rows))
         ;; Collapsed: just main row
         (list main-row))))
   (agents-workflow-agents workflow)))

;;;; Dashboard panel (for claude-dashboard.el compositor)

(defun agents-workflow--panel-toggle-expand (_wf-name row-id)
  "Toggle expanded directory view for agent ROW-ID."
  ;; Strip ::extra-N suffix if on a sub-row
  (let ((agent-name (if (string-match "\\(.+\\)::extra-" row-id)
                        (match-string 1 row-id)
                      row-id)))
    (if (gethash agent-name agents-workflow--expanded-agents)
        (remhash agent-name agents-workflow--expanded-agents)
      (puthash agent-name t agents-workflow--expanded-agents))
    (claude-dashboard-refresh-all)))

(defun agents-workflow-agents-panel (workflow)
  "Return a dashboard panel plist for WORKFLOW's agents.
For use with `claude-dashboard-create'."
  (let ((wf-name (agents-workflow-name workflow)))
    (list
     :name "agents"
     :title "Agents"
     :columns [("Agent" 16 t) ("T" 3 t) ("S" 3 nil)
               ("Dir" 18 nil) ("Activity" 10 nil) ("Last Output" 0 nil)]
     :entries (lambda ()
                (when-let ((wf (agents-workflow--get wf-name)))
                  (mapcar (lambda (e) (cons (car e) (cadr e)))
                          (agents-workflow--dashboard-entries wf))))
     :actions `(("RET" . ,(lambda (_panel row-id)
                            (agents-workflow--panel-visit-agent wf-name row-id)))
                ("s" . ,(lambda (_panel row-id)
                          (agents-workflow--panel-send-command wf-name row-id)))
                ("l" . ,(lambda (_panel row-id)
                          (agents-workflow--panel-link-agent wf-name row-id)))
                ("a" . ,(lambda (_panel _row-id)
                          (agents-workflow--panel-add-agent wf-name)))
                ("R" . ,(lambda (_panel row-id)
                          (agents-workflow--panel-rename-agent wf-name row-id)))
                ("d" . ,(lambda (_panel row-id)
                          (if (string-match "\\(.+\\)::extra-\\([0-9]+\\)" row-id)
                              (agents-workflow--panel-remove-extra-dir
                               wf-name (match-string 1 row-id)
                               (string-to-number (match-string 2 row-id)))
                            (agents-workflow--panel-delete-agent wf-name row-id))))
                ("i" . ,(lambda (_panel row-id)
                          (agents-workflow--panel-insert-directory wf-name row-id)))
                ("C-o" . ,(lambda (_panel row-id)
                            (agents-workflow--panel-toggle-expand wf-name row-id)))
                ("C-k C-k" . ,(lambda (_panel row-id)
                                (agents-workflow--panel-kill-agent wf-name row-id))))
     :interval 3
     :context wf-name)))

(defun agents-workflow--panel-find-agent (wf-name row-id)
  "Find agent with name ROW-ID in workflow WF-NAME."
  (when-let ((wf (agents-workflow--get wf-name)))
    (agents-workflow--find-agent-by-name wf row-id)))

;;;; Backend-generic send helper

(defun agents-workflow--send-to-interactive (agent buffer cmd)
  "Send CMD to interactive AGENT via BUFFER, dispatching by backend."
  (pcase (agents-workflow-agent-backend agent)
    ('claude
     (with-current-buffer buffer
       (claude-code--do-send-command cmd)))
    ('codex
     (codex-cli--send-command cmd buffer))
    (_ (error "Unknown backend: %s" (agents-workflow-agent-backend agent)))))

(defun agents-workflow--ensure-agent-buffer (agent)
  "Return AGENT's live buffer, reconnecting if necessary.
If the agent's buffer field is nil or the buffer is dead, search for
a matching buffer by instance name, dispatching by backend.  First
tries the agent's directory, then falls back to all buffers.  Returns
the live buffer or nil."
  (let ((buf (agents-workflow-agent-buffer agent)))
    (if (and buf (buffer-live-p buf))
        buf
      ;; Try to reconnect — first by directory, then globally
      (let* ((dir (agents-workflow-agent-directory agent))
             (name (agents-workflow-agent-name agent))
             (backend (agents-workflow-agent-backend agent))
             (candidates
              (pcase backend
                ('codex
                 (when (fboundp 'codex-cli--find-buffers-for-directory)
                   (or (codex-cli--find-buffers-for-directory dir)
                       (codex-cli--find-buffers))))
                (_ ; claude (default)
                 (when (fboundp 'claude-code--find-claude-buffers-for-directory)
                   (or (claude-code--find-claude-buffers-for-directory dir)
                       (claude-code--find-all-claude-buffers))))))
             (extract-fn
              (pcase backend
                ('codex #'codex-cli--extract-instance-name)
                (_ #'claude-code--extract-instance-name-from-buffer-name)))
             (matching (cl-find-if
                        (lambda (b)
                          (equal (funcall extract-fn (buffer-name b))
                                 name))
                        candidates)))
        (when (and matching (buffer-live-p matching))
          (setf (agents-workflow-agent-buffer agent) matching)
          matching)))))

(defun agents-workflow--panel-visit-agent (wf-name row-id)
  "Visit agent ROW-ID from workflow WF-NAME in another window.
If the agent's buffer field is nil or stale, try to reconnect.
If no buffer exists at all, offer to relaunch the agent."
  (if-let ((agent (agents-workflow--panel-find-agent wf-name row-id)))
      (if-let ((buf (agents-workflow--ensure-agent-buffer agent)))
          (pop-to-buffer buf
                         '((display-buffer-reuse-window
                            display-buffer-use-some-window)
                           (inhibit-same-window . t)))
        ;; No buffer — relaunch the agent
        (when (y-or-n-p (format "Agent %s has no buffer. Relaunch? " row-id))
          (let ((wf (agents-workflow--get wf-name)))
            (agents-workflow--start-agent agent wf)
            (run-at-time 1 nil
                         (lambda ()
                           (when-let ((buf (agents-workflow--ensure-agent-buffer agent)))
                             (pop-to-buffer buf
                                            '((display-buffer-reuse-window
                                               display-buffer-use-some-window)
                                              (inhibit-same-window . t)))))))))
    (message "Agent %s not found" row-id)))

(defun agents-workflow--panel-send-command (wf-name row-id)
  "Send a command to agent ROW-ID from workflow WF-NAME."
  (if-let ((agent (agents-workflow--panel-find-agent wf-name row-id)))
      (let ((cmd (read-string (format "Send to %s: " row-id))))
        (pcase (agents-workflow-agent-type agent)
          ('interactive
           (if-let ((buf (agents-workflow--ensure-agent-buffer agent)))
               (agents-workflow--send-to-interactive agent buf cmd)
             (message "Agent %s has no buffer" row-id)))
          ('autonomous
           (agents-workflow--enqueue-autonomous agent cmd))))
    (message "Agent %s not found" row-id)))

(defun agents-workflow--panel-link-agent (wf-name row-id)
  "Link an existing Claude session to agent ROW-ID in workflow WF-NAME."
  (if-let ((agent (agents-workflow--panel-find-agent wf-name row-id)))
      (if (eq (agents-workflow-agent-type agent) 'interactive)
          (let* ((all-bufs (pcase (agents-workflow-agent-backend agent)
                              ('codex (when (fboundp 'codex-cli--find-buffers)
                                        (codex-cli--find-buffers)))
                              (_ (when (fboundp 'claude-code--find-all-claude-buffers)
                                   (claude-code--find-all-claude-buffers)))))
                 (buf-names (mapcar #'buffer-name all-bufs)))
            (if buf-names
                (let* ((chosen (completing-read
                                (format "Link %s to session: " row-id)
                                buf-names nil t))
                       (buf (get-buffer chosen))
                       (session-name (agents-workflow--extract-session-name buf)))
                  (setf (agents-workflow-agent-buffer agent) buf)
                  (when-let ((sid (agents-workflow--extract-session-id-from-process buf)))
                    (setf (agents-workflow-agent-session-id agent) sid))
                  (when-let ((session-dir (claude-code--extract-directory-from-buffer-name
                                           (buffer-name buf))))
                    (setf (agents-workflow-agent-directory agent)
                          (file-truename session-dir)))
                  (setf (agents-workflow-agent-status agent) 'waiting)
                  (setf (agents-workflow-agent-last-activity agent) (float-time))
                  (agents-workflow--install-title-watcher agent)
                  (when (and session-name
                             (not (equal session-name (agents-workflow-agent-name agent))))
                    (when (y-or-n-p (format "Rename agent to \"%s\"? " session-name))
                      (let ((old-name (agents-workflow-agent-name agent)))
                        (when-let ((wf (agents-workflow--get wf-name)))
                          (dolist (trigger (agents-workflow-triggers wf))
                            (when (equal (plist-get trigger :from) old-name)
                              (plist-put trigger :from session-name))))
                        (setf (agents-workflow-agent-name agent) session-name))))
                  (when-let ((wf (agents-workflow--get wf-name)))
                    (when (agents-workflow--resolve-context-file wf)
                      (when (y-or-n-p "Send workflow context file to this agent? ")
                        (agents-workflow--send-context-to-agent agent wf))))
                  (claude-dashboard-refresh-all)
                  (message "Linked %s to %s" (agents-workflow-agent-name agent) chosen))
              (message "No Claude Code sessions running")))
        (message "Agent %s is autonomous, cannot link" row-id))
    (message "Agent %s not found" row-id)))

(defun agents-workflow--list-worktrees (directory)
  "List git worktrees under DIRECTORY.
Returns an alist of (NAME . PATH) parsed from `git worktree list'."
  (let* ((default-directory directory)
         (output (string-trim
                  (shell-command-to-string "git worktree list --porcelain 2>/dev/null")))
         (worktrees nil))
    (when (not (string-empty-p output))
      (let ((blocks (split-string output "\n\n" t)))
        (dolist (block blocks)
          (let ((path nil) (branch nil))
            (dolist (line (split-string block "\n" t))
              (cond
               ((string-prefix-p "worktree " line)
                (setq path (substring line 9)))
               ((string-prefix-p "branch " line)
                (setq branch (file-name-nondirectory (substring line 7))))))
            (when (and path branch)
              (push (cons branch path) worktrees))))))
    (nreverse worktrees)))

(defun agents-workflow--ensure-worktrees-ignored (directory)
  "Ensure .worktrees is listed in DIRECTORY's .gitignore."
  (let ((gitignore (expand-file-name ".gitignore" directory)))
    (if (file-exists-p gitignore)
        (let ((content (with-temp-buffer
                         (insert-file-contents gitignore)
                         (buffer-string))))
          (unless (string-match-p "^.worktrees$" content)
            (with-temp-file gitignore
              (insert content)
              (unless (string-suffix-p "\n" content)
                (insert "\n"))
              (insert ".worktrees\n"))))
      (with-temp-file gitignore
        (insert ".worktrees\n")))))

(defun agents-workflow--list-branches (directory)
  "List local and remote branch names in DIRECTORY's git repo.
Returns a list of branch name strings (without remotes/origin/ prefix)."
  (let* ((default-directory directory)
         (output (string-trim
                  (shell-command-to-string
                   "git branch -a --format='%(refname:short)' 2>/dev/null")))
         (branches nil))
    (dolist (line (split-string output "\n" t))
      (let ((name (if (string-prefix-p "origin/" line)
                      (substring line 7)
                    line)))
        (unless (or (string-prefix-p "HEAD" name)
                    (member name branches))
          (push name branches))))
    (nreverse branches)))

(defun agents-workflow--create-worktree (directory branch-name &optional new-branch)
  "Create a git worktree at DIRECTORY/.worktrees/BRANCH-NAME.
When NEW-BRANCH is non-nil, create a new branch with `-b'.
Otherwise check out the existing BRANCH-NAME.
Ensures .worktrees is gitignored.  Returns the worktree path."
  (let* ((wt-dir (expand-file-name ".worktrees" directory))
         (wt-path (expand-file-name branch-name wt-dir))
         (default-directory directory))
    (unless (file-directory-p wt-dir)
      (make-directory wt-dir t))
    (agents-workflow--ensure-worktrees-ignored directory)
    (let* ((cmd (if new-branch
                    (format "git worktree add %s -b %s 2>&1"
                            (shell-quote-argument wt-path)
                            (shell-quote-argument branch-name))
                  (format "git worktree add %s %s 2>&1"
                          (shell-quote-argument wt-path)
                          (shell-quote-argument branch-name))))
           (result (shell-command-to-string cmd)))
      (if (file-directory-p wt-path)
          (progn
            (message "Created worktree: %s" result)
            wt-path)
        (user-error "Failed to create worktree: %s" result)))))

(defun agents-workflow--prompt-worktree (base-dir agent-name)
  "Prompt user to select a worktree or branch in BASE-DIR.
AGENT-NAME is used as the default for new branch names.
Returns (DIR . WORKTREE-PATH) where WORKTREE-PATH is nil if
the user chose to use the base directory directly."
  (let* ((existing (agents-workflow--list-worktrees base-dir))
         (choices (append (mapcar (lambda (wt) (format "%s (%s)" (car wt) (cdr wt)))
                                  existing)
                          '("Existing branch" "New branch" "Use base directory")))
         (choice (completing-read "Worktree: " choices nil t)))
    (cond
     ((equal choice "New branch")
      (let* ((branch (read-string "New branch name: " agent-name))
             (path (agents-workflow--create-worktree base-dir branch t)))
        (cons path path)))
     ((equal choice "Existing branch")
      (let* ((branches (agents-workflow--list-branches base-dir))
             (branch (completing-read "Branch: " branches nil t))
             (path (agents-workflow--create-worktree base-dir branch)))
        (cons path path)))
     ((equal choice "Use base directory")
      (cons base-dir nil))
     (t
      ;; Selected an existing worktree
      (let ((selected (nth (cl-position choice choices :test #'equal) existing)))
        (cons (cdr selected) (cdr selected)))))))

(defun agents-workflow--panel-add-agent (wf-name)
  "Add a new interactive agent to workflow WF-NAME."
  (when-let ((wf (agents-workflow--get wf-name)))
    (let ((name (read-string "Agent name: ")))
      (when (string-empty-p name)
        (user-error "Agent name cannot be empty"))
      (when (agents-workflow--find-agent-by-name wf name)
        (user-error "Agent %s already exists" name))
      (let* ((backend-str (completing-read "Backend: " '("claude" "codex") nil t nil nil "claude"))
             (backend (intern backend-str))
             (wf-dir (agents-workflow-directory wf))
             (base-dir (let ((dir-choice (completing-read
                                          "Directory: "
                                          (list (abbreviate-file-name wf-dir) "Other directory...")
                                          nil t nil nil (abbreviate-file-name wf-dir))))
                          (if (equal dir-choice "Other directory...")
                              (read-directory-name "Agent directory: " "~/")
                            (expand-file-name dir-choice))))
             (result (agents-workflow--prompt-worktree base-dir name))
             (agent-dir (car result))
             (wt-path (cdr result)))
        (let ((agent (make-agents-workflow-agent
                      :name name :type 'interactive :status 'idle
                      :backend backend
                      :directory agent-dir
                      :worktree-path wt-path)))
          (setf (agents-workflow-agents wf)
                (append (agents-workflow-agents wf) (list agent)))
          (agents-workflow--start-agent agent wf)
          (claude-dashboard-refresh-all)
          (message "Added and started %s agent %s%s"
                   backend-str name
                   (if wt-path (format " (worktree: %s)" (file-name-nondirectory (directory-file-name wt-path))) "")))))))

(defun agents-workflow--restart-agent-with-dirs (agent workflow)
  "Kill and restart AGENT, preserving session-id with updated --add-dir flags."
  (let ((buf (agents-workflow-agent-buffer agent)))
    ;; Kill existing buffer/process
    (when (and buf (buffer-live-p buf))
      (let ((proc (get-buffer-process buf)))
        (when (and proc (process-live-p proc))
          (kill-process proc)))
      (kill-buffer buf))
    (setf (agents-workflow-agent-buffer agent) nil)
    (setf (agents-workflow-agent-status agent) 'idle)
    ;; Relaunch — session-id is preserved so --resume will be used
    (agents-workflow--start-agent agent workflow)))

(defun agents-workflow--panel-insert-directory (wf-name row-id)
  "Add an extra directory to agent ROW-ID in workflow WF-NAME.
Prompts for directory and optional worktree, then restarts the agent
with the new --add-dir flag if it is running."
  (if-let ((agent (agents-workflow--panel-find-agent wf-name row-id)))
      (let* ((wf (agents-workflow--get wf-name))
             (wf-dir (agents-workflow-directory wf))
             (base-dir (let ((dir-choice (completing-read
                                           "Add directory: "
                                           (list (abbreviate-file-name wf-dir) "Other directory...")
                                           nil t nil nil "Other directory...")))
                         (if (equal dir-choice "Other directory...")
                             (read-directory-name "Directory: " "~/")
                           (expand-file-name dir-choice))))
             (result (agents-workflow--prompt-worktree base-dir
                       (agents-workflow-agent-name agent)))
             (agent-dir (car result))
             (wt-path (cdr result))
             (entry (list :directory agent-dir :worktree-path wt-path)))
        ;; Append to extra-directories
        (setf (agents-workflow-agent-extra-directories agent)
              (append (agents-workflow-agent-extra-directories agent) (list entry)))
        ;; Restart if running
        (when (and (agents-workflow-agent-buffer agent)
                   (buffer-live-p (agents-workflow-agent-buffer agent)))
          (agents-workflow--restart-agent-with-dirs agent wf))
        ;; Save state
        (agents-workflow-save-state wf-name)
        (agents-workflow-save wf-name)
        (claude-dashboard-refresh-all)
        (message "Added %s to %s%s"
                 (file-name-nondirectory (directory-file-name agent-dir))
                 (agents-workflow-agent-name agent)
                 (if wt-path " (worktree)" "")))
    (message "No agent selected")))

(defun agents-workflow--panel-remove-extra-dir (wf-name agent-name index)
  "Remove extra directory at INDEX from agent AGENT-NAME in workflow WF-NAME."
  (when-let* ((wf (agents-workflow--get wf-name))
              (agent (agents-workflow--find-agent-by-name wf agent-name)))
    (let* ((extras (agents-workflow-agent-extra-directories agent))
           (entry (nth index extras))
           (dir-name (file-name-nondirectory
                      (directory-file-name (plist-get entry :directory)))))
      (when (y-or-n-p (format "Remove %s from %s? " dir-name agent-name))
        (setf (agents-workflow-agent-extra-directories agent)
              (append (cl-subseq extras 0 index)
                      (cl-subseq extras (1+ index))))
        ;; Restart if running
        (when (and (agents-workflow-agent-buffer agent)
                   (buffer-live-p (agents-workflow-agent-buffer agent)))
          (agents-workflow--restart-agent-with-dirs agent wf))
        (agents-workflow-save-state wf-name)
        (agents-workflow-save wf-name)
        (claude-dashboard-refresh-all)
        (message "Removed %s from %s" dir-name agent-name)))))

(defun agents-workflow--panel-rename-agent (wf-name row-id)
  "Rename agent ROW-ID in workflow WF-NAME."
  (if-let ((agent (agents-workflow--panel-find-agent wf-name row-id)))
      (let* ((old-name (agents-workflow-agent-name agent))
             (new-name (read-string (format "Rename %s to: " old-name) old-name)))
        (unless (or (string-empty-p new-name) (equal new-name old-name))
          (when-let ((wf (agents-workflow--get wf-name)))
            (dolist (trigger (agents-workflow-triggers wf))
              (when (equal (plist-get trigger :from) old-name)
                (plist-put trigger :from new-name))))
          (setf (agents-workflow-agent-name agent) new-name)
          ;; Best-effort buffer rename — don't let errors block the
          ;; dashboard refresh that follows.
          (ignore-errors
            (when-let ((buf (agents-workflow-agent-buffer agent)))
              (when (buffer-live-p buf)
                (let* ((dir (claude-code--extract-directory-from-buffer-name
                             (buffer-name buf)))
                       (new-buf-name (format "*claude:%s:%s*" dir new-name)))
                  (with-current-buffer buf
                    (rename-buffer new-buf-name t))))))
          (claude-dashboard-refresh-all)
          ;; Restore cursor to the renamed row
          (claude-dashboard--goto-row new-name)
          (message "Renamed %s to %s" old-name new-name)))
    (message "Agent %s not found" row-id)))

(defun agents-workflow--panel-kill-agent (wf-name row-id)
  "Kill agent ROW-ID's buffer/process but keep it in the workflow.
The agent remains in the dashboard showing as idle (sleep icon)."
  (if-let ((wf (agents-workflow--get wf-name)))
      (if-let ((agent (agents-workflow--find-agent-by-name wf row-id)))
          (when (y-or-n-p (format "Kill agent %s? " row-id))
            (agents-workflow--kill-agent agent)
            (agents-workflow--remove-title-watcher agent)
            (claude-dashboard-refresh-all)
            (message "Killed agent %s (still in dashboard, relaunch with RET → s)" row-id))
        (message "Agent %s not found" row-id))
    (message "Workflow %s not found" wf-name)))

(defun agents-workflow--panel-delete-agent (wf-name row-id)
  "Remove agent ROW-ID from workflow WF-NAME without killing its buffer."
  (if-let ((wf (agents-workflow--get wf-name)))
      (if-let ((agent (agents-workflow--find-agent-by-name wf row-id)))
          (when (y-or-n-p (format "Remove agent %s from workflow? " row-id))
            ;; Cancel timer if any
            (when-let ((timer (agents-workflow-agent-timer agent)))
              (cancel-timer timer))
            ;; Kill autonomous process (but not the buffer)
            (when-let ((proc (agents-workflow-agent-process agent)))
              (when (process-live-p proc)
                (kill-process proc)))
            ;; Remove title watcher from buffer
            (agents-workflow--remove-title-watcher agent)
            ;; Unlink buffer from agent (keep the buffer alive)
            (setf (agents-workflow-agent-buffer agent) nil)
            ;; Remove agent from workflow's agent list
            (setf (agents-workflow-agents wf)
                  (cl-remove agent (agents-workflow-agents wf) :test #'eq))
            (claude-dashboard-refresh-all)
            (message "Removed agent %s" row-id))
        ;; Agent not in list (stale dashboard row) — refresh to clear it
        (claude-dashboard-refresh-all)
        (message "Agent %s already removed, refreshing dashboard" row-id))
    (message "Workflow %s not found" wf-name)))

(defun agents-workflow--dashboard-header-line (workflow)
  "Build a header-line string for WORKFLOW with name, state, dir, branch, context."
  (let* ((name (agents-workflow-name workflow))
         (state (agents-workflow-state workflow))
         (dir (agents-workflow-directory workflow))
         (ctx (agents-workflow-context-file workflow))
         (short-dir (abbreviate-file-name dir))
         (folder-icon (condition-case nil (all-the-icons-material "folder_open") (error "")))
         (doc-icon (condition-case nil (all-the-icons-material "description") (error "")))
         (sep (propertize "  │  " 'face '(:foreground "#585b70"))))
    (concat
     "  "
     (propertize name 'face '(:foreground "#cdd6f4" :weight bold))
     "  "
     (agents-workflow--state-badge state)
     sep
     (propertize (format "%s %s" folder-icon short-dir) 'face '(:foreground "#9399b2"))
     (when ctx
       (concat
        sep
        (propertize (format "%s %s" doc-icon (file-name-nondirectory ctx))
                    'face '(:foreground "#f5c2e7")))))))

(defun agents-workflow-dashboard (&optional workflow-name)
  "Open the composable dashboard for WORKFLOW-NAME.
If WORKFLOW-NAME is nil, prompt from registered workflows."
  (interactive)
  (let* ((name (or workflow-name
                   (completing-read "Workflow: "
                                   (hash-table-keys agents-workflow--registry)
                                   nil t)))
         (wf (agents-workflow--get name)))
    (unless wf (error "No workflow named %s" name))
    (let ((panels (list (agents-workflow-agents-panel wf))))
      ;; Add optional panels listed in the workflow definition
      (dolist (panel-name (agents-workflow-panels wf))
        (when-let ((constructor (alist-get panel-name agents-workflow-panel-registry
                                           nil nil #'equal)))
          (when (fboundp constructor)
            (setq panels (append panels (list (funcall constructor)))))))
      (pop-to-buffer
       (claude-dashboard-create
        :name name
        :header (lambda () (agents-workflow--dashboard-header-line wf))
        :panels panels)))))

(defun agents-workflow--extract-session-name (buffer)
  "Extract the Claude Code session name from BUFFER's name.
Returns the instance name (e.g. \"worker-1\") or nil."
  (when (buffer-live-p buffer)
    (claude-code--extract-instance-name-from-buffer-name
     (buffer-name buffer))))

(defun agents-workflow--extract-session-id-from-process (buffer)
  "Extract Claude Code session-id from BUFFER's process arguments.
Looks for --session-id or --resume followed by a UUID argument.
If --resume has no UUID (bare resume), looks up the most recent
session file from Claude's project storage."
  (when (and (buffer-live-p buffer) (get-buffer-process buffer))
    (let* ((args (process-command (get-buffer-process buffer)))
           (uuid-re "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'")
           (id nil)
           (bare-resume nil))
      ;; First try to find explicit UUID in process args
      (let ((a args))
        (while (and a (not id))
          (when (member (car a) '("--session-id" "--resume"))
            (if (and (cadr a) (string-match-p uuid-re (cadr a)))
                (setq id (cadr a))
              ;; Bare --resume (no UUID or next arg is another flag)
              (when (equal (car a) "--resume")
                (setq bare-resume t))))
          (setq a (cdr a))))
      ;; For bare --resume, find the most recent session file
      (when (and bare-resume (not id))
        (when-let* ((dir (claude-code--extract-directory-from-buffer-name
                          (buffer-name buffer)))
                    (abs-dir (directory-file-name (expand-file-name dir)))
                    (slug (replace-regexp-in-string "[/.]" "-" abs-dir))
                    (sessions-dir (expand-file-name slug "~/.claude/projects/"))
                    (_ (file-directory-p sessions-dir))
                    (jsonls (directory-files sessions-dir t "\\.jsonl\\'"))
                    (sorted (sort jsonls
                                  (lambda (a b)
                                    (time-less-p
                                     (file-attribute-modification-time (file-attributes b))
                                     (file-attribute-modification-time (file-attributes a))))))
                    (newest (file-name-sans-extension (file-name-nondirectory (car sorted)))))
          (when (string-match-p uuid-re newest)
            (setq id newest))))
      id)))

(defun agents-workflow--kill-agent (agent)
  "Kill AGENT's process, buffer, and unlink everything."
  ;; Kill autonomous process
  (when-let ((proc (agents-workflow-agent-process agent)))
    (when (process-live-p proc)
      (kill-process proc)))
  (when-let ((timer (agents-workflow-agent-timer agent)))
    (cancel-timer timer)
    (setf (agents-workflow-agent-timer agent) nil))
  ;; Kill linked buffer (interactive agents)
  (when-let ((buf (agents-workflow-agent-buffer agent)))
    (when (buffer-live-p buf)
      ;; Disable process-query so kill-buffer doesn't prompt again
      (when-let ((proc (get-buffer-process buf)))
        (set-process-query-on-exit-flag proc nil))
      (kill-buffer buf)))
  (setf (agents-workflow-agent-status agent) 'idle)
  (setf (agents-workflow-agent-process agent) nil)
  (setf (agents-workflow-agent-buffer agent) nil))

;;;; Autonomous agent execution

(defcustom agents-workflow-claude-program "claude"
  "Path to the Claude CLI program."
  :type 'string
  :group 'agents-workflow)

(defun agents-workflow--build-prompt (agent context)
  "Build prompt for AGENT using CONTEXT.
If agent has a prompt-template, format it with CONTEXT.
Otherwise use CONTEXT directly."
  (if-let ((template (agents-workflow-agent-prompt-template agent)))
      (format template context)
    context))

(defun agents-workflow--parse-output (output)
  "Parse OUTPUT as JSON, falling back to raw string."
  (condition-case nil
      (json-read-from-string output)
    (error output)))

(defun agents-workflow--enqueue-autonomous (agent task)
  "Add TASK to AGENT's queue.
If agent is idle, run immediately."
  (if (eq (agents-workflow-agent-status agent) 'idle)
      (agents-workflow--run-autonomous agent task)
    (setf (agents-workflow-agent-queue agent)
          (append (agents-workflow-agent-queue agent) (list task)))))

(defun agents-workflow--process-queue (agent workflow)
  "Process next item in AGENT's queue within WORKFLOW."
  (when-let ((next-task (pop (agents-workflow-agent-queue agent))))
    (agents-workflow--run-autonomous agent next-task workflow)))

(defun agents-workflow--build-claude-autonomous-args (agent prompt)
  "Build CLI args for AGENT running PROMPT via Claude backend."
  (let ((args (list "--print" prompt "--output-format" "json"))
        (system-prompt (agents-workflow-agent-system-prompt agent)))
    (when system-prompt
      (setq args (append (list "--system-prompt" system-prompt) args)))
    args))

(defun agents-workflow--build-codex-autonomous-args (_agent prompt)
  "Build CLI args for _AGENT running PROMPT via Codex backend.
Note: autonomous codex execution is not yet supported through devbox.
This is a placeholder that launches an interactive session."
  (list prompt))

(defun agents-workflow--parse-codex-output (output)
  "Parse Codex JSONL OUTPUT, extracting the last turn.completed event."
  (condition-case nil
      (let ((lines (split-string (string-trim output) "\n" t))
            (result nil))
        (dolist (line lines)
          (condition-case nil
              (let ((obj (json-read-from-string line)))
                (when (equal (alist-get 'type obj) "turn.completed")
                  (setq result obj)))
            (error nil)))
        (or result output))
    (error output)))

(defun agents-workflow--run-autonomous (agent &optional prompt workflow)
  "Run AGENT with PROMPT as a background Claude process.
WORKFLOW is used for emitting completion events."
  (when (and (agents-workflow-agent-process agent)
             (process-live-p (agents-workflow-agent-process agent)))
    (if prompt
        (progn
          (setf (agents-workflow-agent-queue agent)
                (append (agents-workflow-agent-queue agent) (list prompt)))
          (cl-return-from agents-workflow--run-autonomous))
      (message "Agent %s already running" (agents-workflow-agent-name agent))
      (cl-return-from agents-workflow--run-autonomous)))

  (let* ((actual-prompt (if prompt
                            (agents-workflow--build-prompt agent prompt)
                          (agents-workflow-agent-prompt-template agent)))
         (backend (agents-workflow-agent-backend agent))
         (buf-name (format " *agents-workflow:%s:output*"
                           (agents-workflow-agent-name agent)))
         (output-buffer (get-buffer-create buf-name))
         (backend-args
          (pcase backend
            ('codex (agents-workflow--build-codex-autonomous-args agent actual-prompt))
            (_ (agents-workflow--build-claude-autonomous-args agent actual-prompt))))
         (program (agents-workflow--backend-program backend))
         (args (append (agents-workflow--backend-base-switches backend) backend-args))
         (default-directory (agents-workflow-agent-directory agent)))

    ;; Clear output buffer
    (with-current-buffer output-buffer (erase-buffer))

    ;; Update agent state
    (setf (agents-workflow-agent-status agent) 'running)
    (setf (agents-workflow-agent-last-activity agent) (float-time))
    (setf (agents-workflow-agent-buffer agent) output-buffer)

    ;; Start process with workflow/agent env vars for Databricks integration
    (let* ((process-environment
            (append `(,(format "CLAUDE_WORKFLOW=%s"
                               (if workflow (agents-workflow-name workflow) ""))
                      ,(format "CLAUDE_AGENT=%s"
                               (agents-workflow-agent-name agent)))
                    process-environment))
           (proc (apply #'start-process
                        (format "agents-workflow-%s"
                                (agents-workflow-agent-name agent))
                        output-buffer
                        program
                        args)))

      (setf (agents-workflow-agent-process agent) proc)

      ;; Process filter — capture intermediate output for dashboard
      (set-process-filter
       proc
       (lambda (process output)
         ;; Append to output buffer (default behavior)
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (goto-char (point-max))
             (insert output)))
         ;; Update last-output with tail of accumulated output
         (when (buffer-live-p (process-buffer process))
           (let ((tail (with-current-buffer (process-buffer process)
                         (let ((end (point-max)))
                           (string-trim
                            (buffer-substring-no-properties
                             (max (point-min) (- end 200)) end))))))
             (unless (string-empty-p tail)
               (setf (agents-workflow-agent-last-output agent) tail)
               (setf (agents-workflow-agent-last-activity agent) (float-time)))))))

      ;; Timeout timer
      (let ((timeout-timer
             (run-at-time (agents-workflow-agent-timeout agent) nil
                          (lambda ()
                            (when (process-live-p proc)
                              (kill-process proc)
                              (setf (agents-workflow-agent-status agent) 'error)
                              (setf (agents-workflow-agent-last-output agent)
                                    "Timed out"))))))

        ;; Sentinel
        (set-process-sentinel
         proc
         (lambda (process _event)
           (cancel-timer timeout-timer)
           (let* ((exit-code (process-exit-status process))
                  (raw-output (with-current-buffer (process-buffer process)
                                (buffer-string)))
                  (parsed (pcase backend
                            ('codex (agents-workflow--parse-codex-output raw-output))
                            (_ (agents-workflow--parse-output raw-output)))))

             ;; Update agent
             (setf (agents-workflow-agent-process agent) nil)
             (setf (agents-workflow-agent-last-activity agent) (float-time))

             (if (= exit-code 0)
                 (progn
                   (setf (agents-workflow-agent-status agent) 'idle)
                   (setf (agents-workflow-agent-last-output agent)
                         (if (stringp parsed)
                             (string-trim
                              (substring parsed 0
                                         (min (length parsed) 200)))
                           (json-encode parsed)))
                   (when workflow
                     (agents-workflow--emit
                      workflow
                      `(:event agent-complete
                        :agent ,(agents-workflow-agent-name agent)
                        :output ,parsed
                        :exit-code 0))))
               (progn
                 (setf (agents-workflow-agent-status agent) 'error)
                 (setf (agents-workflow-agent-last-output agent)
                       (format "Exit %d: %s"
                               exit-code (string-trim raw-output)))
                 (when workflow
                   (agents-workflow--emit
                    workflow
                    `(:event agent-error
                      :agent ,(agents-workflow-agent-name agent)
                      :output ,raw-output
                      :exit-code ,exit-code)))))

             ;; Process queue
             (when workflow
               (agents-workflow--process-queue agent workflow)))))))))

;;;; Interactive agent integration

(defcustom agents-workflow-last-output-lines 100
  "Number of lines to extract from interactive agent buffer as last output."
  :type 'integer
  :group 'agents-workflow)

(defun agents-workflow--extract-last-output (buffer n-lines)
  "Extract last N-LINES from BUFFER, stripping ANSI escape codes."
  (with-current-buffer buffer
    (let* ((end (point-max))
           (start (save-excursion
                    (goto-char end)
                    (forward-line (- n-lines))
                    (point)))
           (raw (buffer-substring-no-properties start end)))
      (replace-regexp-in-string "\033\\[[0-9;]*[a-zA-Z]" "" raw))))

(defun agents-workflow--find-agent-by-buffer (workflow buffer-name)
  "Find the agent in WORKFLOW whose buffer matches BUFFER-NAME."
  (cl-find-if
   (lambda (agent)
     (and (agents-workflow-agent-buffer agent)
          (equal (buffer-name (agents-workflow-agent-buffer agent))
                 buffer-name)))
   (agents-workflow-agents workflow)))

(defun agents-workflow--find-agent-by-name (workflow name)
  "Find agent in WORKFLOW by NAME."
  (cl-find name (agents-workflow-agents workflow)
           :key #'agents-workflow-agent-name :test #'equal))

(defun agents-workflow--handle-claude-event (event)
  "Handle an event from `claude-code-event-hook'.
EVENT is a plist with :type and :buffer-name."
  (let ((buffer-name (plist-get event :buffer-name))
        (type (plist-get event :type)))
    (maphash
     (lambda (_name wf)
       (when (eq (agents-workflow-state wf) 'running)
         (when-let ((agent (agents-workflow--find-agent-by-buffer wf buffer-name)))
           (pcase type
             ("notification"
              (agents-workflow--mark-agent-waiting agent wf))))))
     agents-workflow--registry)))

(defun agents-workflow--handle-bell (_terminal)
  "After-advice for `claude-code--notify'.
Detects when Claude finishes responding (bell rings) and updates agent status.
_TERMINAL is the eat terminal parameter (ignored)."
  (let ((buf-name (buffer-name)))
    (maphash
     (lambda (_name wf)
       (when (eq (agents-workflow-state wf) 'running)
         (when-let ((agent (agents-workflow--find-agent-by-buffer wf buf-name)))
           (agents-workflow--mark-agent-waiting agent wf))))
     agents-workflow--registry)))

(defun agents-workflow--mark-agent-waiting (agent workflow)
  "Mark AGENT as waiting and emit worker-waiting event in WORKFLOW."
  (setf (agents-workflow-agent-status agent) 'waiting)
  (setf (agents-workflow-agent-last-activity agent) (float-time))
  (when-let ((buf (agents-workflow-agent-buffer agent)))
    (when (buffer-live-p buf)
      (setf (agents-workflow-agent-last-output agent)
            (agents-workflow--extract-last-output
             buf agents-workflow-last-output-lines))))
  (agents-workflow--emit workflow
                        `(:event worker-waiting
                          :agent ,(agents-workflow-agent-name agent)
                          :output ,(agents-workflow-agent-last-output agent))))

(defun agents-workflow--handle-send-command (result)
  "After-advice for `claude-code--do-send-command'.
RESULT is the return value (the buffer or nil)."
  (when result
    (let ((buffer-name (buffer-name result)))
      (agents-workflow--mark-agent-running-by-buffer buffer-name)))
  result)

(defun agents-workflow--handle-eat-input (_terminal string)
  "After-advice for `eat-term-send-string'.
Detects when a Return key is sent in a Claude workflow buffer,
marking the agent as running.  _TERMINAL is ignored.  STRING is
the text sent to the terminal."
  (when (and (string-match-p "\r\\|\n" string)
             (claude-code--buffer-p (current-buffer)))
    ;; Only for Claude buffers — codex status is handled by
    ;; codex-cli-status-change-functions (fired from codex-cli--send-command).
    (agents-workflow--mark-agent-running-by-buffer (buffer-name))))

(defun agents-workflow--mark-agent-running-by-buffer (buffer-name)
  "Mark the workflow agent associated with BUFFER-NAME as running."
  (maphash
   (lambda (_name wf)
     (when (eq (agents-workflow-state wf) 'running)
       (when-let ((agent (agents-workflow--find-agent-by-buffer wf buffer-name)))
         (setf (agents-workflow-agent-status agent) 'running)
         (setf (agents-workflow-agent-last-activity agent) (float-time))
         (agents-workflow--emit wf
                               `(:event worker-running
                                 :agent ,(agents-workflow-agent-name agent))))))
   agents-workflow--registry))

(defun agents-workflow--handle-codex-status (buffer status)
  "Handle codex status change in BUFFER to STATUS.
STATUS is `idle' or `working'.  Updates the corresponding agent."
  (when (buffer-live-p buffer)
    (let ((buffer-name (buffer-name buffer)))
      (maphash
       (lambda (_name wf)
         (when (eq (agents-workflow-state wf) 'running)
           (when-let ((agent (agents-workflow--find-agent-by-buffer
                              wf buffer-name)))
             (pcase status
               ('idle
                (setf (agents-workflow-agent-status agent) 'waiting)
                (setf (agents-workflow-agent-last-activity agent) (float-time))
                (setf (agents-workflow-agent-last-output agent)
                      (agents-workflow--extract-last-output buffer agents-workflow-last-output-lines))
                (agents-workflow--emit wf
                  `(:event worker-waiting
                    :agent ,(agents-workflow-agent-name agent))))
               ('working
                (setf (agents-workflow-agent-status agent) 'running)
                (setf (agents-workflow-agent-last-activity agent) (float-time))
                (agents-workflow--emit wf
                  `(:event worker-running
                    :agent ,(agents-workflow-agent-name agent))))))))
       agents-workflow--registry))))

(add-hook 'codex-cli-status-change-functions #'agents-workflow--handle-codex-status)

;;;; Workflow lifecycle

(defvar agents-workflow--title-idle-pattern "✳"
  "String in the terminal title that indicates Claude is idle/waiting.
Claude Code CLI sets the terminal title via OSC 0 sequences.
A spinner character (⠂, ⠐, etc.) means Claude is working; the ✳
character means Claude has finished and is waiting for input.")

(defun agents-workflow--install-title-watcher (agent)
  "Install an eat title-change handler on AGENT's buffer.
For claude backend: detects idle via terminal title (✳ pattern).
For codex backend: status detection is handled by
`codex-cli-status-change-functions' hook, installed automatically
by `codex-cli--start'.  This function is a no-op for codex."
  (let ((idle-pattern (agents-workflow--backend-get
                       (agents-workflow-agent-backend agent)
                       :title-idle-pattern)))
    (when idle-pattern
      (when-let ((buf (agents-workflow-agent-buffer agent)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (bound-and-true-p eat-terminal)
              (eat-term-set-parameter eat-terminal 'set-title-function
                (lambda (_terminal title)
                  (agents-workflow--handle-title-change
                   agent buf title idle-pattern))))))))))

(defun agents-workflow--remove-title-watcher (agent)
  "Remove the eat title-change handler from AGENT's buffer.
Called during workflow stop to prevent the watcher from resetting
the agent status after it has been set to idle."
  (when-let ((buf (agents-workflow-agent-buffer agent)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (bound-and-true-p eat-terminal)
          (condition-case nil
              (eat-term-set-parameter eat-terminal 'set-title-function
                #'ignore)
            (args-out-of-range nil)))))))

(defun agents-workflow--handle-title-change (agent buf title &optional idle-pattern)
  "Handle a terminal title change for AGENT with buffer BUF.
TITLE is the new terminal title.  IDLE-PATTERN is the string that
indicates the CLI is idle/waiting (defaults to the global pattern)."
  (let ((pattern (or idle-pattern agents-workflow--title-idle-pattern)))
    (cond
     ;; Title contains idle marker → CLI is done, waiting for input
     ((string-match-p pattern title)
      (unless (eq (agents-workflow-agent-status agent) 'waiting)
        (setf (agents-workflow-agent-status agent) 'waiting)
        (setf (agents-workflow-agent-last-activity agent) (float-time))
        (setf (agents-workflow-agent-last-output agent)
              (when (buffer-live-p buf)
                (agents-workflow--extract-last-output
                 buf agents-workflow-last-output-lines)))
        (when-let ((wf (agents-workflow--find-workflow-for-agent agent)))
          (agents-workflow--emit wf
            `(:event worker-waiting
              :agent ,(agents-workflow-agent-name agent)
              :output ,(agents-workflow-agent-last-output agent))))))
     ;; Title has spinner or other content → CLI is working
     ((not (string-empty-p title))
      (when (memq (agents-workflow-agent-status agent) '(idle waiting))
        (setf (agents-workflow-agent-status agent) 'running)
        (setf (agents-workflow-agent-last-activity agent) (float-time))
        (when-let ((wf (agents-workflow--find-workflow-for-agent agent)))
          (agents-workflow--emit wf
            `(:event worker-running
              :agent ,(agents-workflow-agent-name agent)))))))))

(defun agents-workflow--find-workflow-for-agent (agent)
  "Find the workflow that owns AGENT by scanning the registry."
  (let ((found nil))
    (maphash (lambda (_name wf)
               (when (memq agent (agents-workflow-agents wf))
                 (setq found wf)))
             agents-workflow--registry)
    found))

(defun agents-workflow--extra-dir-switches (agent)
  "Return list of --add-dir switches for AGENT's extra directories."
  (apply #'append
         (mapcar (lambda (ed)
                   (list "--add-dir" (plist-get ed :directory)))
                 (agents-workflow-agent-extra-directories agent))))

(defun agents-workflow--start-claude-interactive (agent)
  "Start interactive AGENT by creating a claude-code.el eat terminal.
Uses the agent name as the instance name, bypassing the interactive prompt.
If the agent has a session-id, resumes that session with --resume."
  (let* ((dir (agents-workflow-agent-directory agent))
         (default-directory dir)
         (instance-name (agents-workflow-agent-name agent))
         (existing (when (fboundp 'claude-code--find-claude-buffers-for-directory)
                     (claude-code--find-claude-buffers-for-directory dir)))
         (matching (cl-find-if
                    (lambda (buf)
                      (equal (claude-code--extract-instance-name-from-buffer-name
                              (buffer-name buf))
                             instance-name))
                    existing)))
    (if matching
        ;; Reuse existing buffer with same instance name
        (setf (agents-workflow-agent-buffer agent) matching)
      ;; Create new instance with predetermined name
      (let* ((session-id (agents-workflow-agent-session-id agent))
             (buffer-name (claude-code--buffer-name instance-name))
             (extra-switches
              (cond
               ;; Resume existing session
               (session-id (list "--resume" session-id))
               ;; New session: assign a UUID so we can save it later
               (t (let ((new-id (agents-workflow--generate-uuid)))
                    (setf (agents-workflow-agent-session-id agent) new-id)
                    (list "--session-id" new-id)))))
             (system-prompt (or (agents-workflow-agent-system-prompt agent)
                               (agents-workflow--convention-system-prompt
                                instance-name)))
             (add-dir-switches (agents-workflow--extra-dir-switches agent))
             (program-switches (append claude-code-program-switches
                                       extra-switches
                                       add-dir-switches
                                       (when system-prompt
                                         (list "--system-prompt" system-prompt
                                               "--dangerously-skip-permissions"))))
             (process-adaptive-read-buffering nil)
             (wf (agents-workflow--find-workflow-for-agent agent))
             (extra-env-variables
              (apply #'append
                     (mapcar (lambda (func)
                               (funcall func buffer-name dir))
                             claude-code-process-environment-functions)))
             (process-environment
              (append `(,(format "CLAUDE_BUFFER_NAME=%s" buffer-name)
                        ,(format "CLAUDE_WORKFLOW=%s"
                                 (if wf (agents-workflow-name wf) ""))
                        ,(format "CLAUDE_AGENT=%s" instance-name))
                      extra-env-variables
                      process-environment))
             (buffer (claude-code--term-make
                      claude-code-terminal-backend
                      buffer-name
                      claude-code-program
                      program-switches)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (claude-code--term-configure claude-code-terminal-backend)
            (claude-code--term-setup-keymap claude-code-terminal-backend)
            (claude-code--term-customize-faces claude-code-terminal-backend)
            (run-hooks 'claude-code-start-hook))
          (setf (agents-workflow-agent-buffer agent) buffer))))))

(defun agents-workflow--start-codex-interactive (agent)
  "Start interactive AGENT by creating a Codex eat terminal."
  (let* ((dir (agents-workflow-agent-directory agent))
         (instance-name (agents-workflow-agent-name agent))
         (existing (codex-cli--find-buffers-for-directory dir))
         (matching (cl-find-if
                    (lambda (buf)
                      (equal (codex-cli--extract-instance-name
                              (buffer-name buf))
                             instance-name))
                    existing)))
    (if matching
        (setf (agents-workflow-agent-buffer agent) matching)
      (let ((buffer (codex-cli--start dir instance-name)))
        (when buffer
          (setf (agents-workflow-agent-buffer agent) buffer)
          (let ((system-prompt (or (agents-workflow-agent-system-prompt agent)
                                   (agents-workflow--convention-system-prompt
                                    instance-name))))
            (when system-prompt
              (run-at-time 3 nil
                           (lambda ()
                             (when (buffer-live-p buffer)
                               (codex-cli--send-command system-prompt buffer)))))))))))

(defun agents-workflow--resolve-context-file (workflow)
  "Return the absolute path to WORKFLOW's context file, or nil."
  (when-let ((cf (agents-workflow-context-file workflow)))
    (let ((path (if (file-name-absolute-p cf)
                    cf
                  (expand-file-name cf (agents-workflow-directory workflow)))))
      (when (file-exists-p path) path))))

(defun agents-workflow--read-context-file (workflow)
  "Read and return the contents of WORKFLOW's context file, or nil."
  (when-let ((path (agents-workflow--resolve-context-file workflow)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun agents-workflow--send-context-to-agent (agent workflow)
  "Send WORKFLOW's context file contents to interactive AGENT.
Sends it as the first message after a short delay to let the
terminal initialize."
  (when-let ((content (agents-workflow--read-context-file workflow)))
    (let ((buf (agents-workflow-agent-buffer agent))
          (msg (format "Read and internalize this workflow context. Do NOT take any action yet, just acknowledge you've read it.\n\n%s" content)))
      (when (and buf (buffer-live-p buf))
        ;; Delay to let the eat terminal finish initializing
        (run-at-time 3 nil
                     (lambda ()
                       (when (buffer-live-p buf)
                         (agents-workflow--send-to-interactive
                          agent buf msg))))))))

;;; --- Inter-agent communication ---

(defvar agents-workflow--pending-asks nil
  "List of pending ask requests.
Each entry is (TARGET-NAME . REQUESTER-NAME).")

(defun agents-workflow--deliver-ask-response (workflow event)
  "Trigger action: deliver response from ask target back to requester.
Checks `agents-workflow--pending-asks' for a matching request,
sends the response, and removes the one-shot entry."
  (let* ((target-name (plist-get event :agent))
         (output (plist-get event :output))
         (pending (assoc target-name agents-workflow--pending-asks)))
    (when pending
      (let* ((requester-name (cdr pending))
             (requester (agents-workflow--find-agent-by-name
                         workflow requester-name)))
        ;; Remove one-shot entry
        (setq agents-workflow--pending-asks
              (delq pending agents-workflow--pending-asks))
        ;; Also remove the one-shot trigger
        (setf (agents-workflow-triggers workflow)
              (cl-remove-if
               (lambda (tr)
                 (and (eq (plist-get tr :on) 'worker-waiting)
                      (equal (plist-get tr :from) target-name)
                      (eq (plist-get tr :do)
                          #'agents-workflow--deliver-ask-response)))
               (agents-workflow-triggers workflow)))
        ;; Send response to requester
        (when-let ((buf (and requester
                             (agents-workflow-agent-buffer requester))))
          (when (buffer-live-p buf)
            (let ((msg (format "Response from %s:\n\n%s"
                               target-name (or output "(no output)"))))
              (agents-workflow--send-to-interactive
               requester buf msg))))))))

(defun agents-workflow-ask-agent (target-name question &optional requester-name)
  "Send QUESTION to agent TARGET-NAME, deliver response to REQUESTER-NAME.
If REQUESTER-NAME is nil, just sends the question without routing the
response back.  Returns a status message.

Designed to be called from agents via emacsclient:
  emacsclient -e \\='(agents-workflow-ask-agent
    \"worker\" \"What is X?\" \"slack-monitor\")\\='"
  (let ((wf nil) (target nil))
    ;; Find the workflow and target agent
    (maphash (lambda (_name w)
               (when-let ((a (agents-workflow--find-agent-by-name w target-name)))
                 (setq wf w target a)))
             agents-workflow--registry)
    (unless target
      (error "Agent %s not found" target-name))
    (let ((buf (agents-workflow-agent-buffer target)))
      (unless (and buf (buffer-live-p buf))
        (error "Agent %s has no buffer" target-name))
      ;; Set up one-shot trigger for response delivery
      (when requester-name
        (push (cons target-name requester-name) agents-workflow--pending-asks)
        (setf (agents-workflow-triggers wf)
              (append (agents-workflow-triggers wf)
                      (list `(:on worker-waiting
                              :from ,target-name
                              :do ,#'agents-workflow--deliver-ask-response)))))
      ;; Send the question
      (agents-workflow--send-to-interactive target buf question)
      (format "Question sent to %s%s"
              target-name
              (if requester-name
                  (format ", response will be routed to %s" requester-name)
                "")))))

(defun agents-workflow--start-agent (agent &optional workflow)
  "Start AGENT based on its type.
If WORKFLOW is provided, send its context file to the agent on startup."
  (pcase (agents-workflow-agent-type agent)
    ('interactive
     (pcase (agents-workflow-agent-backend agent)
       ('claude (agents-workflow--start-claude-interactive agent))
       ('codex  (agents-workflow--start-codex-interactive agent))
       (_ (error "Unknown backend: %s" (agents-workflow-agent-backend agent))))
     (agents-workflow--install-title-watcher agent)
     ;; Offer to send shared context file unless agent has its own system prompt
     (when (and workflow
                (not (agents-workflow-agent-system-prompt agent))
                (not (agents-workflow--convention-system-prompt
                      (agents-workflow-agent-name agent)))
                (agents-workflow--resolve-context-file workflow))
       (run-at-time 1 nil
                    (lambda ()
                      (when (y-or-n-p
                             (format "Send context file to %s? "
                                     (agents-workflow-agent-name agent)))
                        (agents-workflow--send-context-to-agent agent workflow))))))
    ('autonomous
     ;; Record startup time so dashboard doesn't show "never"
     (setf (agents-workflow-agent-last-activity agent) (float-time))
     ;; Start polling timer if interval is set
     (if-let ((interval (agents-workflow-agent-interval agent)))
         (progn
           (setf (agents-workflow-agent-status agent) 'polling)
           (let ((wf-name (agents-workflow-agent-directory agent)))
             (ignore wf-name)
             (setf (agents-workflow-agent-timer agent)
                   (run-at-time interval interval
                                (lambda ()
                                  ;; Find workflow by scanning registry for this agent
                                  (maphash
                                   (lambda (_name wf)
                                     (when (memq agent (agents-workflow-agents wf))
                                       (agents-workflow--emit
                                        wf `(:event timer-fired
                                             :agent ,(agents-workflow-agent-name agent)))
                                       (agents-workflow--run-autonomous agent nil wf)))
                                   agents-workflow--registry))))))
       ;; No interval — event-driven agent, stays idle until triggered
       (setf (agents-workflow-agent-status agent) 'idle)))))

(defun agents-workflow-start (&optional workflow-name)
  "Start workflow WORKFLOW-NAME.
Launches agents, registers hooks, opens dashboard."
  (interactive
   (list (completing-read "Start workflow: "
                          (hash-table-keys agents-workflow--registry) nil t)))
  (let ((wf (agents-workflow--get workflow-name)))
    (unless wf (error "No workflow named %s" workflow-name))
    (when (eq (agents-workflow-state wf) 'running)
      (error "Workflow %s is already running" workflow-name))

    ;; Ensure claude-code is loaded before starting agents
    (require 'claude-code)

    ;; Register hooks
    (add-hook 'claude-code-event-hook #'agents-workflow--handle-claude-event)
    (advice-add 'claude-code--do-send-command :filter-return
                #'agents-workflow--handle-send-command)
    (advice-add 'claude-code--notify :after
                #'agents-workflow--handle-bell)
    (advice-add 'eat-term-send-string :after
                #'agents-workflow--handle-eat-input)

    ;; Load saved session state if available
    (agents-workflow-load-state workflow-name)

    ;; Start agents
    (dolist (agent (agents-workflow-agents wf))
      (agents-workflow--start-agent agent wf))

    ;; Set state
    (setf (agents-workflow-state wf) 'running)
    (setf (agents-workflow-paused wf) nil)

    ;; Open dashboard
    (agents-workflow-dashboard workflow-name)
    (message "Workflow %s started" workflow-name)))

(defun agents-workflow-stop (&optional workflow-name)
  "Stop workflow WORKFLOW-NAME.
Kills autonomous agents, cancels timers, unlinks interactive agents."
  (interactive
   (list (completing-read "Stop workflow: "
                          (hash-table-keys agents-workflow--registry) nil t)))
  (let ((wf (agents-workflow--get workflow-name)))
    (unless wf (error "No workflow named %s" workflow-name))

    ;; Save session state and workflow definition before stopping
    (condition-case err
        (progn
          (agents-workflow-save-state workflow-name)
          (agents-workflow-save workflow-name))
      (error (message "Warning: could not save for %s: %s"
                      workflow-name (error-message-string err))))

    ;; Stop agents (with stale-struct guard)
    (condition-case _err
        (dolist (agent (agents-workflow-agents wf))
          (pcase (agents-workflow-agent-type agent)
            ('interactive
             ;; Remove title watcher before unlinking to prevent
             ;; it from resetting the status back to running.
             (agents-workflow--remove-title-watcher agent)
             ;; Unlink buffer — don't kill it so the user can
             ;; continue interacting with the Claude session.
             (setf (agents-workflow-agent-buffer agent) nil))
            (_
             ;; Kill autonomous agents and their buffers.
             (agents-workflow--kill-agent agent)))
          (setf (agents-workflow-agent-status agent) 'idle))
      (wrong-type-argument
       (message "Warning: stale struct detected in %s, clearing registry entry"
                workflow-name)))

    ;; Remove hooks (only if no other workflows are running)
    (let ((any-running nil))
      (maphash (lambda (name other-wf)
                 (unless (equal name workflow-name)
                   (condition-case nil
                       (when (eq (agents-workflow-state other-wf) 'running)
                         (setq any-running t))
                     (error nil))))
               agents-workflow--registry)
      (unless any-running
        (remove-hook 'claude-code-event-hook #'agents-workflow--handle-claude-event)
        (advice-remove 'claude-code--do-send-command
                       #'agents-workflow--handle-send-command)
        (advice-remove 'claude-code--notify
                       #'agents-workflow--handle-bell)
        (advice-remove 'eat-term-send-string
                       #'agents-workflow--handle-eat-input)))

    ;; Clear from registry so it can be cleanly re-defined
    (condition-case nil
        (setf (agents-workflow-state wf) 'stopped)
      (error (remhash workflow-name agents-workflow--registry)))
    (message "Workflow %s stopped" workflow-name)))

(defun agents-workflow-pause (&optional workflow-name)
  "Pause workflow WORKFLOW-NAME.  Suppresses triggers without killing agents."
  (interactive
   (list (completing-read "Pause workflow: "
                          (hash-table-keys agents-workflow--registry) nil t)))
  (let ((wf (agents-workflow--get workflow-name)))
    (unless wf (error "No workflow named %s" workflow-name))
    (setf (agents-workflow-paused wf) t)
    ;; Pause polling timers
    (dolist (agent (agents-workflow-agents wf))
      (when-let ((timer (agents-workflow-agent-timer agent)))
        (cancel-timer timer)
        (setf (agents-workflow-agent-timer agent) nil)))
    (message "Workflow %s paused" workflow-name)))

(defun agents-workflow-resume (&optional workflow-name)
  "Resume workflow WORKFLOW-NAME.  Re-enables triggers and polling timers."
  (interactive
   (list (completing-read "Resume workflow: "
                          (hash-table-keys agents-workflow--registry) nil t)))
  (let ((wf (agents-workflow--get workflow-name)))
    (unless wf (error "No workflow named %s" workflow-name))
    (setf (agents-workflow-paused wf) nil)
    ;; Restart polling timers
    (dolist (agent (agents-workflow-agents wf))
      (when (and (eq (agents-workflow-agent-type agent) 'autonomous)
                 (agents-workflow-agent-interval agent))
        (agents-workflow--start-agent agent)))
    (message "Workflow %s resumed" workflow-name)))

;;;; Slack monitor

(defun agents-workflow--parse-slack-response (output)
  "Parse Slack monitor OUTPUT into a list of channel alists.
Returns nil if no channels or parse error."
  (condition-case nil
      (let* ((data (if (stringp output) (json-read-from-string output) output))
             (channels (alist-get 'channels data)))
        (when (and channels (> (length channels) 0))
          (append channels nil)))  ; convert vector to list
    (error nil)))

(defun agents-workflow--slack-review-prompt (workflow event)
  "Present Slack summaries from EVENT for user review in WORKFLOW.
Uses minibuffer with approve/edit/skip choices."
  (let* ((output (plist-get event :output))
         (channels (agents-workflow--parse-slack-response
                    (if (stringp output) output (json-encode output)))))
    (when channels
      (agents-workflow--slack-review-channels workflow channels))))

(defun agents-workflow--slack-review-channels (workflow channels)
  "Review each channel in CHANNELS list within WORKFLOW."
  (dolist (channel channels)
    (let* ((name (alist-get 'name channel))
           (summary (alist-get 'summary channel))
           (needs-reply (alist-get 'needs_reply channel))
           (draft (alist-get 'draft_reply channel))
           (prompt (format "[Slack] #%s\nSummary: %s%s\n\n[a]pprove  [e]dit  [s]kip: "
                           name summary
                           (if (and needs-reply draft)
                               (format "\nDraft reply: \"%s\"" draft)
                             ""))))
      (when needs-reply
        (let ((choice (read-char-choice prompt '(?a ?e ?s))))
          (pcase choice
            (?a (agents-workflow--slack-send-reply workflow channel draft))
            (?e (let ((edited (read-string "Edit reply: " draft)))
                  (agents-workflow--slack-send-reply workflow channel edited)))
            (?s (message "Skipped #%s" name))))))))

(defun agents-workflow--slack-send-reply (workflow channel reply)
  "Send REPLY to CHANNEL via an autonomous Claude agent in WORKFLOW.
Respects Slack safety rules by showing confirmation."
  (let* ((channel-id (alist-get 'id channel))
         (channel-name (alist-get 'name channel)))
    ;; Safety gate: confirm before sending
    (when (yes-or-no-p
           (format "Send to #%s (%s):\n\"%s\"\n\nConfirm? "
                   channel-name channel-id reply))
      ;; Find or create a transient autonomous agent for the send
      (let ((send-agent (make-agents-workflow-agent
                         :name (format "slack-send-%s" channel-id)
                         :type 'autonomous
                         :status 'idle
                         :directory (agents-workflow-directory workflow)
                         :system-prompt "Send the following message to the specified Slack channel using the Slack MCP tools. Send exactly the message provided, do not modify it."
                         :timeout 60)))
        (agents-workflow--run-autonomous
         send-agent
         (format "Send this message to Slack channel %s:\n\n%s" channel-id reply)
         workflow)
        (message "Sending reply to #%s..." channel-name)))))

;;;; Persistence (.eld file support)

(defcustom agents-workflow-projects-directory
  (locate-user-emacs-file "agents-workflow/projects")
  "Directory containing workflow definition (.eld) files."
  :type 'directory
  :group 'agents-workflow)

(defcustom agents-workflow-elisp-directory
  (locate-user-emacs-file "agents-workflow/elisp")
  "Directory containing elisp packages.
Used for convention-based system prompt discovery:
when an agent named NAME is started, the file
<elisp-directory>/NAME/system-prompt.md is loaded automatically."
  :type 'directory
  :group 'agents-workflow)

(defun agents-workflow--generate-uuid ()
  "Generate a random UUID v4 string."
  (format "%08x-%04x-4%03x-%04x-%012x"
          (random (expt 16 8))
          (random (expt 16 4))
          (random (expt 16 3))
          (logior #x8000 (random #x3fff))
          (random (expt 16 12))))

(defun agents-workflow--state-file (workflow-name)
  "Return the state file path for WORKFLOW-NAME."
  (expand-file-name (concat "." workflow-name "-state.eld")
                    agents-workflow-projects-directory))

(defun agents-workflow-save-state (&optional workflow-name)
  "Save session state for WORKFLOW-NAME.
Persists session IDs and working directories for all agents,
including dynamically-added ones."
  (interactive
   (list (completing-read "Save state: "
                          (hash-table-keys agents-workflow--registry) nil t)))
  (let* ((wf (agents-workflow--get workflow-name))
         (file (agents-workflow--state-file workflow-name)))
    (unless wf (error "No workflow named %s" workflow-name))
    (let ((agent-states
           (mapcar (lambda (agent)
                     (let ((entry (list :name (agents-workflow-agent-name agent)
                                        :directory (agents-workflow-agent-directory agent))))
                       ;; Try to extract session-id from live process if not already set
                       (let ((sid (or (agents-workflow-agent-session-id agent)
                                      (when-let ((buf (agents-workflow-agent-buffer agent)))
                                        (agents-workflow--extract-session-id-from-process buf)))))
                         (when sid
                           (setf (agents-workflow-agent-session-id agent) sid)
                           (setq entry (plist-put entry :session-id sid))))
                       (let ((backend (agents-workflow-agent-backend agent)))
                         (when (and backend (not (eq backend 'claude)))
                           (setq entry (plist-put entry :backend backend))))
                       (when-let ((wtp (agents-workflow-agent-worktree-path agent)))
                         (setq entry (plist-put entry :worktree-path wtp)))
                       (when-let ((eds (agents-workflow-agent-extra-directories agent)))
                         (setq entry (plist-put entry :extra-directories eds)))
                       entry))
                   (agents-workflow-agents wf))))
      (when agent-states
        (let ((dir (file-name-directory file)))
          (unless (file-directory-p dir) (make-directory dir t)))
        (with-temp-file file
          (insert ";; -*- mode: lisp-data; -*-\n")
          (insert (format ";; Session state for workflow: %s\n" workflow-name))
          (pp (list :workflow workflow-name :agents agent-states)
              (current-buffer)))
        (message "Saved session state for %s" workflow-name)
        file))))

(defun agents-workflow-load-state (&optional workflow-name)
  "Load session state for WORKFLOW-NAME.
Restores session IDs and working directories to existing agents.
Agents present in the state file but not in the workflow definition
\(i.e. dynamically added in a previous session) are re-created."
  (interactive
   (list (completing-read "Load state: "
                          (hash-table-keys agents-workflow--registry) nil t)))
  (let* ((wf (agents-workflow--get workflow-name))
         (file (agents-workflow--state-file workflow-name)))
    (unless wf (error "No workflow named %s" workflow-name))
    (when (file-exists-p file)
      (let* ((data (with-temp-buffer
                     (insert-file-contents file)
                     (read (current-buffer))))
             (agent-states (plist-get data :agents))
             (restored 0)
             (created 0))
        (dolist (state agent-states)
          (let ((name (plist-get state :name))
                (session-id (plist-get state :session-id))
                (directory (plist-get state :directory))
                (backend (plist-get state :backend))
                (worktree-path (plist-get state :worktree-path))
                (extra-dirs (plist-get state :extra-directories)))
            (if-let ((agent (agents-workflow--find-agent-by-name wf name)))
                ;; Existing agent — restore session ID, directory, and worktree
                (progn
                  (when session-id
                    (setf (agents-workflow-agent-session-id agent) session-id))
                  (when directory
                    (setf (agents-workflow-agent-directory agent) directory))
                  (when worktree-path
                    (setf (agents-workflow-agent-worktree-path agent) worktree-path))
                  (when extra-dirs
                    (setf (agents-workflow-agent-extra-directories agent) extra-dirs))
                  (cl-incf restored))
              ;; Agent from a previous session — re-create it
              (let ((agent (make-agents-workflow-agent
                            :name name
                            :type 'interactive
                            :backend (or backend 'claude)
                            :status 'idle
                            :directory (or directory (agents-workflow-directory wf))
                            :session-id session-id
                            :worktree-path worktree-path
                            :extra-directories extra-dirs)))
                (setf (agents-workflow-agents wf)
                      (append (agents-workflow-agents wf) (list agent)))
                (cl-incf created)))))
        (message "Restored %d agents, re-created %d for %s"
                 restored created workflow-name)
        (+ restored created)))))

(defun agents-workflow-clear-state (&optional workflow-name)
  "Delete saved session state for WORKFLOW-NAME."
  (interactive
   (list (completing-read "Clear state: "
                          (hash-table-keys agents-workflow--registry) nil t)))
  (let ((file (agents-workflow--state-file workflow-name)))
    (when (file-exists-p file)
      (delete-file file)
      (message "Cleared session state for %s" workflow-name))))

(defun agents-workflow--save-all-states ()
  "Save session state for all registered workflows.
Called automatically on Emacs exit via `kill-emacs-hook'."
  (maphash (lambda (name _wf)
             (condition-case err
                 (progn
                   (agents-workflow-save-state name)
                   (agents-workflow-save name))
               (error (message "Warning: could not save %s: %s"
                               name (error-message-string err)))))
           agents-workflow--registry))

(add-hook 'kill-emacs-hook #'agents-workflow--save-all-states)

(defun agents-workflow--serialize-agent (agent &optional workflow)
  "Serialize AGENT struct back to a plist suitable for saving.
When WORKFLOW is provided, the agent's directory is only included
if it differs from the workflow directory."
  (let ((plist (list :name (agents-workflow-agent-name agent)
                     :type (agents-workflow-agent-type agent))))
    ;; Save backend only when non-default
    (let ((backend (agents-workflow-agent-backend agent)))
      (when (and backend (not (eq backend 'claude)))
        (setq plist (plist-put plist :backend backend))))
    ;; Save per-agent directory when it differs from the workflow's
    (let ((agent-dir (agents-workflow-agent-directory agent)))
      (when (and agent-dir
                 (not (and workflow
                           (equal (file-truename agent-dir)
                                  (file-truename (agents-workflow-directory workflow))))))
        (setq plist (plist-put plist :directory agent-dir))))
    (if-let ((spf (agents-workflow-agent-system-prompt-file agent)))
        ;; Prefer file reference over inline string
        (setq plist (plist-put plist :system-prompt-file spf))
      (when-let ((sp (agents-workflow-agent-system-prompt agent)))
        (setq plist (plist-put plist :system-prompt sp))))
    (when-let ((pt (agents-workflow-agent-prompt-template agent)))
      (setq plist (plist-put plist :prompt-template pt)))
    (when-let ((iv (agents-workflow-agent-interval agent)))
      (setq plist (plist-put plist :interval iv)))
    (when (/= (agents-workflow-agent-timeout agent) 120)
      (setq plist (plist-put plist :timeout (agents-workflow-agent-timeout agent))))
    (when-let ((md (agents-workflow-agent-metadata agent)))
      (setq plist (plist-put plist :metadata md)))
    (when-let ((wtp (agents-workflow-agent-worktree-path agent)))
      (setq plist (plist-put plist :worktree-path wtp)))
    (when-let ((eds (agents-workflow-agent-extra-directories agent)))
      (setq plist (plist-put plist :extra-directories eds)))
    plist))

(defun agents-workflow--serialize-trigger (trigger)
  "Serialize TRIGGER plist, warning if :when contains a lambda."
  (let ((result (list :on (plist-get trigger :on))))
    (when-let ((from (plist-get trigger :from)))
      (setq result (plist-put result :from from)))
    (when-let ((match (plist-get trigger :match)))
      (setq result (plist-put result :match match)))
    (when-let ((when-fn (plist-get trigger :when)))
      (if (symbolp when-fn)
          (setq result (plist-put result :when when-fn))
        (message "Warning: :when lambda in trigger cannot be saved, skipping")
        nil))
    (let ((do-fn (plist-get trigger :do)))
      (if (symbolp do-fn)
          (setq result (plist-put result :do do-fn))
        (message "Warning: :do lambda in trigger cannot be saved, skipping")
        (setq result nil)))
    result))

(defun agents-workflow--serialize (workflow)
  "Serialize WORKFLOW to a plist for saving to .eld file.
Validates the struct layout before serializing to catch stale
in-memory structs created before a field was added."
  ;; Guard: agents slot must be a list of agent structs, not a symbol.
  ;; A mismatch indicates the in-memory struct uses an outdated layout.
  (let ((agents-val (agents-workflow-agents workflow)))
    (when (and agents-val (not (listp agents-val)))
      (error "Workflow %s has stale struct layout (agents slot is %S, not a list). \
Stop the workflow, re-evaluate agents-workflow.el, and restart it"
             (agents-workflow-name workflow) (type-of agents-val))))
  (let ((triggers (cl-remove nil
                             (mapcar #'agents-workflow--serialize-trigger
                                     (agents-workflow-triggers workflow))))
        (result (list :name (agents-workflow-name workflow)
                      :directory (agents-workflow-directory workflow))))
    (when-let ((cf (agents-workflow-context-file workflow)))
      (setq result (plist-put result :context-file cf)))
    (setq result (plist-put result :agents
                            (mapcar (lambda (a)
                                      (agents-workflow--serialize-agent a workflow))
                                    (agents-workflow-agents workflow))))
    (setq result (plist-put result :triggers triggers))
    (when-let ((panels (agents-workflow-panels workflow)))
      (setq result (plist-put result :panels panels)))
    result))

(defun agents-workflow-save (&optional workflow-name file)
  "Save WORKFLOW-NAME definition to FILE as .eld.
If FILE is nil, save to `agents-workflow-projects-directory'/NAME.eld."
  (interactive
   (list (completing-read "Save workflow: "
                          (hash-table-keys agents-workflow--registry) nil t)))
  (let* ((wf (agents-workflow--get workflow-name))
         (file (or file
                   (expand-file-name (concat workflow-name ".eld")
                                     agents-workflow-projects-directory)))
         (data (agents-workflow--serialize wf))
         (dir (file-name-directory file)))
    (unless wf (error "No workflow named %s" workflow-name))
    (unless (file-directory-p dir) (make-directory dir t))
    (with-temp-file file
      (insert ";; -*- mode: lisp-data; -*-\n")
      (insert (format ";; Claude Workflow: %s\n" workflow-name))
      (pp data (current-buffer)))
    (message "Saved workflow %s to %s" workflow-name file)
    file))

(defun agents-workflow-load-file (file)
  "Load a workflow definition from .eld FILE and register it."
  (interactive
   (list (read-file-name "Load workflow: "
                         agents-workflow-projects-directory nil t nil
                         (lambda (f) (string-suffix-p ".eld" f)))))
  (let* ((data (with-temp-buffer
                 (insert-file-contents file)
                 (read (current-buffer))))
         (name (plist-get data :name))
         (directory (plist-get data :directory))
         (context-file (plist-get data :context-file))
         (agents (plist-get data :agents))
         (triggers (plist-get data :triggers))
         (panels (plist-get data :panels)))
    (unless name (error "Workflow file %s missing :name" file))
    (agents-workflow-define name
      :directory (or directory (file-name-directory file))
      :context-file context-file
      :agents agents
      :triggers triggers
      :panels panels)
    (message "Loaded workflow %s from %s" name file)
    name))

(defun agents-workflow-load-directory (&optional directory)
  "Load all .eld workflow files from DIRECTORY.
Defaults to `agents-workflow-projects-directory'."
  (interactive)
  (let* ((dir (or directory agents-workflow-projects-directory))
         (files (cl-remove-if
                 (lambda (f) (string-match-p "-state\\.eld\\'" f))
                 (directory-files dir t "\\.eld\\'")))
         (loaded nil))
    (dolist (file files)
      (condition-case err
          (push (agents-workflow-load-file file) loaded)
        (error (message "Error loading %s: %s" file (error-message-string err)))))
    (message "Loaded %d workflows from %s" (length loaded) dir)
    (nreverse loaded)))

;;;; Built-in trigger actions

(defun agents-workflow-trigger-pm (workflow event)
  "Trigger the PM agent with context from EVENT in WORKFLOW.
For interactive agents, sends the context as a message to the terminal.
For autonomous agents, enqueues it as a background task."
  (let* ((pm-agent (agents-workflow--find-agent-by-name workflow "pm"))
         (context (or (plist-get event :output) "Task completed")))
    (when pm-agent
      (pcase (agents-workflow-agent-type pm-agent)
        ('interactive
         (if-let ((buf (agents-workflow-agent-buffer pm-agent)))
             (when (buffer-live-p buf)
               (let ((prompt (agents-workflow--build-prompt pm-agent context)))
                 (agents-workflow--send-to-interactive pm-agent buf prompt)
                 (message "Sent task summary to PM agent")))
           (message "PM agent has no linked session — link it first")))
        ('autonomous
         (agents-workflow--enqueue-autonomous pm-agent context))))))

(defun agents-workflow-slack-review (workflow event)
  "Review Slack messages from EVENT in WORKFLOW."
  (agents-workflow--slack-review-prompt workflow event))

(defun agents-workflow-trigger-agent (workflow event agent-name &optional context)
  "Generic trigger: run AGENT-NAME in WORKFLOW with CONTEXT from EVENT."
  (when-let ((agent (agents-workflow--find-agent-by-name workflow agent-name)))
    (agents-workflow--enqueue-autonomous
     agent (or context (plist-get event :output) ""))))

;;;; Auto-load workflow definitions on startup

(defun agents-workflow--auto-load ()
  "Load all workflow definitions from `agents-workflow-projects-directory'."
  (when (file-directory-p agents-workflow-projects-directory)
    (agents-workflow-load-directory agents-workflow-projects-directory)))

(unless noninteractive
  (if after-init-time
      (agents-workflow--auto-load)
    (add-hook 'after-init-hook #'agents-workflow--auto-load)))

(provide 'agents-workflow)
;;; agents-workflow.el ends here
