;;; opencode-cli.el --- Minimal library for OpenCode CLI terminals -*- lexical-binding: t; -*-

;; Author: Moling Zhang
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides buffer management, terminal launch, command sending, and
;; timer-based status detection for OpenCode CLI sessions running in eat
;; terminals.  Companion to claude-workflow.el / codex-cli.el; modeled
;; directly on codex-cli.el because OpenCode is likewise a streaming TUI
;; that does not emit a Claude-style idle title glyph.
;;
;; Status detection: after each meaningful terminal output, reset an idle
;; timer.  When the timer fires (no new output for N seconds), the session
;; is idle.
;;
;; Session capture: OpenCode mints its own session id (ses_...) and stores
;; sessions per project.  `opencode session list --format json' emits, for
;; the current working directory's project, JSON objects carrying `id',
;; `directory', and `updated'.  We resolve an agent's session id post-hoc
;; by running that command in the agent's dir and picking the newest entry
;; whose `directory' matches — mirroring codex-cli's rollout scan.

;;; Code:
(require 'cl-lib)
(require 'json)

;; Forward declarations for eat
(declare-function eat-make "eat")
(declare-function eat-term-send-string "eat")
(declare-function eat-semi-char-mode "eat")
(declare-function eat--cursor-blink-mode "eat" (&optional arg))
(declare-function eat--set-cursor "eat")
(defvar eat-default-cursor-type)
(defvar eat-very-visible-cursor-type)
(defvar eat-vertical-bar-cursor-type)
(defvar eat-very-visible-vertical-bar-cursor-type)
(defvar eat-horizontal-bar-cursor-type)
(defvar eat-very-visible-horizontal-bar-cursor-type)
(defvar eat--cursor-blink-mode)
(defvar eat-terminal)
(defvar eat-update-hook)

;; Forward declarations for claude-code.el terminal abstraction
(declare-function claude-code--term-make "claude-code")
(declare-function claude-code--term-configure "claude-code")
(declare-function claude-code--term-setup-keymap "claude-code")
(declare-function claude-code--term-customize-faces "claude-code")
(declare-function claude-code--term-interactive-mode "claude-code")
(defvar claude-code-terminal-backend)
(defvar claude-code-start-hook)

;;;; Customization

(defgroup opencode-cli nil
  "OpenCode CLI terminal management."
  :group 'tools
  :prefix "opencode-cli-")

(defcustom opencode-cli-program "opencode"
  "Program to run OpenCode CLI."
  :type 'string
  :group 'opencode-cli)

(defcustom opencode-cli-program-switches '("--auto")
  "Base switches for OpenCode CLI, prepended before any extra switches.
Defaults to (\"--auto\") — OpenCode's auto-approve-permissions mode, the
analog of Codex's --dangerously-bypass-approvals-and-sandbox — so agents
launched by agents-workflow run non-interactively.  This auto-approves any
tool call not explicitly denied; scope it with the `permission' block in
opencode.json (allow/ask/deny, bash patterns) if you want finer control.
Set to nil to make OpenCode prompt for permissions."
  :type '(repeat string)
  :group 'opencode-cli)

(defcustom opencode-cli-startup-delay 0.5
  "Seconds to wait after creating the terminal before it is usable."
  :type 'number
  :group 'opencode-cli)

(defcustom opencode-cli-idle-delay 5
  "Seconds of no terminal output before considering OpenCode idle.
After each meaningful output chunk, the idle timer resets.  When no
new output arrives for this many seconds, the status changes to `idle'."
  :type 'number
  :group 'opencode-cli)

(defcustom opencode-cli-working-regexp "esc[^\n]*interrupt"
  "Regexp (case-insensitive) matching OpenCode's active-turn indicator.
While a turn is running OpenCode shows a spinner and an \"esc interrupt\"
hint in the footer, so the interrupt hint is a reliable \"actively
working\" signal — the TUI analog of Claude Code's title spinner.  Status
is set to `working' only when this pattern is visible; without it, the
session settles to `idle' after `opencode-cli-idle-delay' of silence
\(which also recovers from OpenCode's known stuck-streaming indicator)."
  :type 'regexp
  :group 'opencode-cli)

;;;; Faces

(defface opencode-cli-header-line
  '((t :inherit header-line
       :foreground "#f2c14e"
       :weight bold))
  "Face for the OpenCode buffer header line."
  :group 'opencode-cli)

;;;; Status detection hook

(defvar opencode-cli-status-change-functions nil
  "Hook called when an OpenCode terminal's status changes.
Each function receives (BUFFER STATUS) where STATUS is `idle' or `working'.")

;;;; Buffer naming and finding

(defun opencode-cli--buffer-name (dir &optional instance)
  "Return buffer name for an OpenCode session in DIR with optional INSTANCE."
  (format "*opencode:%s:%s*"
          (abbreviate-file-name (file-truename dir))
          (or instance "")))

(defun opencode-cli--buffer-p (buffer)
  "Return non-nil if BUFFER is an OpenCode session buffer."
  (string-match-p "\\`\\*opencode:" (buffer-name buffer)))

(defun opencode-cli--extract-instance-name (buf-name)
  "Extract the instance name from BUF-NAME.
Buffer name format is *opencode:DIR:INSTANCE*."
  (when (string-match "\\`\\*opencode:[^:]+:\\(.*\\)\\*\\'" buf-name)
    (match-string 1 buf-name)))

(defun opencode-cli--extract-directory (buf-name)
  "Extract the directory from BUF-NAME.
Buffer name format is *opencode:DIR:INSTANCE*."
  (when (string-match "\\`\\*opencode:\\([^:]+\\):" buf-name)
    (match-string 1 buf-name)))

(defun opencode-cli--find-buffers ()
  "Return all live OpenCode session buffers."
  (cl-remove-if-not #'opencode-cli--buffer-p (buffer-list)))

(defun opencode-cli--find-buffers-for-directory (dir)
  "Return OpenCode session buffers for DIR."
  (let ((short-dir (abbreviate-file-name (file-truename dir))))
    (cl-remove-if-not
     (lambda (buf)
       (and (opencode-cli--buffer-p buf)
            (string-match-p (regexp-quote short-dir) (buffer-name buf))))
     (buffer-list))))

;;;; Session capture (ses_... ids, per-project storage)

(defun opencode-cli--session-list (dir)
  "Return OpenCode sessions for DIR as a list of alists, or nil.
Runs `opencode session list --format json' with DIR as the working
directory.  Each element carries at least `id', `directory', and
`updated'.  Returns nil on any error or when there are no sessions
\(OpenCode prints nothing for an empty project)."
  (let ((prog (executable-find opencode-cli-program)))
    (when (and prog (file-directory-p dir))
      (ignore-errors
        (with-temp-buffer
          (let ((default-directory (file-name-as-directory (expand-file-name dir))))
            (when (eq 0 (call-process prog nil t nil
                                      "session" "list" "--format" "json"))
              (goto-char (point-min))
              (skip-chars-forward " \t\n\r")
              (unless (eobp)
                (let ((json-object-type 'alist)
                      (json-array-type 'list)
                      (json-key-type 'symbol))
                  (json-read))))))))))

(defun opencode-cli--latest-session-id-for-dir (dir)
  "Return the id of the newest OpenCode session whose directory is DIR.
Filters the project's session list by exact `directory' match (so it is
correct even when sibling git worktrees share a project) and returns the
`id' of the entry with the largest `updated' timestamp.  Returns nil when
none match."
  (let ((target (file-truename (expand-file-name dir)))
        (best nil) (best-ts -1))
    (dolist (s (opencode-cli--session-list dir))
      (let ((sdir (alist-get 'directory s))
            (ts (or (alist-get 'updated s) 0))
            (id (alist-get 'id s)))
        (when (and sdir id
                   (equal (file-truename (expand-file-name sdir)) target)
                   (> ts best-ts))
          (setq best id best-ts ts))))
    best))

(defun opencode-cli--export-last-assistant-text (json-string)
  "Parse JSON-STRING from `opencode export' and return the last assistant
message's text (its text parts concatenated), or nil.
The export shape is {messages: [{info: {role}, parts: [{type,text}]}]}."
  (ignore-errors
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (json-key-type 'symbol)
           ;; `opencode export' may emit a leading "Exporting session: …"
           ;; progress line before the JSON — start parsing at the first
           ;; brace (json-read-from-string ignores any trailing text).
           (brace (string-match "{" json-string))
           (data (and brace (json-read-from-string (substring json-string brace))))
           (msgs (alist-get 'messages data))
           (result nil))
      (dolist (m msgs)
        (when (equal (alist-get 'role (alist-get 'info m)) "assistant")
          (let ((txt ""))
            (dolist (p (alist-get 'parts m))
              (when (equal (alist-get 'type p) "text")
                (setq txt (concat txt (or (alist-get 'text p) "")))))
            (unless (string-empty-p (string-trim txt))
              ;; keep the last assistant message that has text
              (setq result txt)))))
      result)))

;;;; Terminal launch

(defun opencode-cli--start (dir &optional instance extra-switches)
  "Launch an OpenCode eat terminal in DIR with optional INSTANCE name.
EXTRA-SWITCHES are appended to `opencode-cli-program-switches'.
Returns the buffer, or nil if creation failed."
  (let* ((default-directory dir)
         (buffer-name (opencode-cli--buffer-name dir instance))
         (switches (append opencode-cli-program-switches extra-switches))
         (process-adaptive-read-buffering nil)
         (buffer (claude-code--term-make
                  claude-code-terminal-backend
                  buffer-name
                  opencode-cli-program
                  switches)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (claude-code--term-configure claude-code-terminal-backend)
        (claude-code--term-interactive-mode claude-code-terminal-backend)
        (claude-code--term-setup-keymap claude-code-terminal-backend)
        (claude-code--term-customize-faces claude-code-terminal-backend)
        (opencode-cli--setup-buffer-appearance)
        (opencode-cli--apply-cursor-visibility)
        (run-hooks 'claude-code-start-hook))
      (opencode-cli--install-idle-timer buffer)
      buffer)))

;;;; Buffer appearance

(defun opencode-cli--setup-buffer-appearance ()
  "Configure the current buffer to look like a polished OpenCode terminal."
  (setq-local vertical-scroll-bar nil)
  (when-let ((win (get-buffer-window (current-buffer))))
    (set-window-fringes win 0 0))
  (setq-local blink-cursor-mode nil)
  (face-remap-add-relative 'nobreak-space :underline nil)
  (setq-local eat-invisible-cursor-type '(bar nil nil))
  (setq-local header-line-format
              (list
               (propertize " OpenCode " 'face 'opencode-cli-header-line)
               " "
               (propertize
                (or (opencode-cli--extract-instance-name (buffer-name))
                    "")
                'face '(:weight bold))
               "  "
               (propertize
                (abbreviate-file-name default-directory)
                'face 'font-lock-comment-face))))

;;;; Cursor visibility

(defun opencode-cli--nonblinking-cursor-type (cursor-spec)
  "Return CURSOR-SPEC with blinking disabled."
  (list (nth 0 cursor-spec) nil (nth 2 cursor-spec)))

(defun opencode-cli--apply-cursor-visibility ()
  "Ensure OpenCode's hidden cursor renders as a visible bar.
OpenCode's TUI sends hide-cursor escapes; EAT maps that state through
`eat-invisible-cursor-type'.  Force blinking cursor variants to
non-blinking mappings to avoid expensive full-frame repaints, then
re-apply the invisible-cursor state."
  (setq-local eat-invisible-cursor-type '(bar nil nil))
  (setq-local eat-very-visible-cursor-type
              (opencode-cli--nonblinking-cursor-type
               eat-very-visible-cursor-type))
  (setq-local eat-very-visible-vertical-bar-cursor-type
              (opencode-cli--nonblinking-cursor-type
               eat-very-visible-vertical-bar-cursor-type))
  (setq-local eat-very-visible-horizontal-bar-cursor-type
              (opencode-cli--nonblinking-cursor-type
               eat-very-visible-horizontal-bar-cursor-type))
  (when (bound-and-true-p eat--cursor-blink-mode)
    (eat--cursor-blink-mode -1))
  (eat--set-cursor nil :invisible))

;;;; Sending commands

(defun opencode-cli--send-command (cmd &optional buffer)
  "Send CMD followed by RET to OpenCode terminal in BUFFER.
BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (when (bound-and-true-p eat-terminal)
        ;; OpenCode's TUI composer submits on Enter (CR); a bare LF ("\n")
        ;; is treated as a newline in the input box and never sends.  Send
        ;; the text, then an explicit CR to submit.
        (eat-term-send-string eat-terminal cmd)
        (eat-term-send-string eat-terminal "\r")
        (run-hook-with-args 'opencode-cli-status-change-functions buf 'working)))))

;;;; Process management

(defun opencode-cli--alive-p (&optional buffer)
  "Return non-nil if BUFFER has a running OpenCode process.
BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (get-buffer-process buf)
         (process-live-p (get-buffer-process buf)))))

(defun opencode-cli--kill (&optional buffer)
  "Kill the OpenCode process in BUFFER.
BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (let ((proc (get-buffer-process buf)))
        (when (and proc (process-live-p proc))
          (kill-process proc))))))

;;;; Timer-based idle detection

(defvar-local opencode-cli--idle-timer nil
  "Timer that fires when OpenCode has been silent for `opencode-cli-idle-delay'.")

(defvar-local opencode-cli--status nil
  "Current status of this OpenCode buffer: `idle', `working', or nil.")

(defun opencode-cli--install-idle-timer (buffer)
  "Install output watcher on BUFFER for timer-based idle detection.
Each time new terminal output arrives, the idle timer resets.
When the timer fires, the status changes to `idle'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq opencode-cli--status 'working)
      (add-hook 'eat-update-hook
                (lambda () (opencode-cli--on-output buffer))
                nil t)
      (add-hook 'kill-buffer-hook
                #'opencode-cli--cancel-idle-timer nil t))))

(defun opencode-cli--working-p ()
  "Return non-nil if the visible screen shows OpenCode's active-turn indicator.
Scans the tail of the current buffer (the on-screen region) for
`opencode-cli-working-regexp'."
  (let ((start (max (point-min) (- (point-max) 4000)))
        (case-fold-search t))
    (string-match-p opencode-cli-working-regexp
                    (buffer-substring-no-properties start (point-max)))))

(defun opencode-cli--on-output (buffer)
  "Handle new terminal output in BUFFER.
Marks the session `working' only while the interrupt hint is on screen
\(a real active-turn signal, not mere redraw activity); (re)arms the
silence timer that settles the session to `idle'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (opencode-cli--cancel-idle-timer)
      (when (and (opencode-cli--working-p)
                 (not (eq opencode-cli--status 'working)))
        (setq opencode-cli--status 'working)
        (run-hook-with-args 'opencode-cli-status-change-functions buffer 'working))
      (setq opencode-cli--idle-timer
            (run-at-time opencode-cli-idle-delay nil
                         #'opencode-cli--idle-timer-fired buffer)))))

(defun opencode-cli--idle-timer-fired (buffer)
  "Called when BUFFER has had no output for `opencode-cli-idle-delay' seconds.
Sustained silence means the turn ended (or the streaming/interrupt hint
got stuck with no further updates) — either way, settle to `idle'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq opencode-cli--idle-timer nil)
      (unless (eq opencode-cli--status 'idle)
        (setq opencode-cli--status 'idle)
        (run-hook-with-args 'opencode-cli-status-change-functions buffer 'idle)))))

(defun opencode-cli--cancel-idle-timer ()
  "Cancel the idle timer in the current buffer."
  (when opencode-cli--idle-timer
    (cancel-timer opencode-cli--idle-timer)
    (setq opencode-cli--idle-timer nil)))

(provide 'opencode-cli)
;;; opencode-cli.el ends here
