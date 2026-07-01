;;; codex-cli.el --- Minimal library for OpenAI Codex CLI terminals -*- lexical-binding: t; -*-

;; Author: Moling Zhang
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.0"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides buffer management, terminal launch, command sending, and
;; timer-based status detection for OpenAI Codex CLI sessions running
;; in eat terminals.  Designed as a companion to claude-workflow.el.
;;
;; Status detection uses the same approach as ai-code-interface.el:
;; after each meaningful terminal output, reset an idle timer.  When
;; the timer fires (no new output for N seconds), the session is idle.

;;; Code:
(require 'cl-lib)

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

(defgroup codex-cli nil
  "Codex CLI terminal management."
  :group 'tools
  :prefix "codex-cli-")

(defcustom codex-cli-program "codex"
  "Program to run Codex CLI.
Set to \"devbox\" and add (\"ai\" \"--cli\" \"codex\") to
`codex-cli-program-switches' to use the devbox wrapper instead."
  :type 'string
  :group 'codex-cli)

(defcustom codex-cli-program-switches '("--dangerously-bypass-approvals-and-sandbox")
  "Base switches for Codex CLI, prepended before any extra switches.
Set to (\"ai\" \"--cli\" \"codex\" \"--dangerously-bypass-approvals-and-sandbox\")
when using the devbox wrapper."
  :type '(repeat string)
  :group 'codex-cli)

(defcustom codex-cli-startup-delay 0.5
  "Seconds to wait after creating the terminal before it is usable."
  :type 'number
  :group 'codex-cli)

(defcustom codex-cli-idle-delay 5
  "Seconds of no terminal output before considering Codex idle.
After each meaningful output chunk, the idle timer resets.  When
no new output arrives for this many seconds, the status changes
to `idle'."
  :type 'number
  :group 'codex-cli)

(defcustom codex-cli-working-regexp "esc[^\n]*interrupt"
  "Regexp (case-insensitive) matching Codex's active-turn indicator.
Codex renders \"Working (… esc to interrupt)\" only while a turn is
running, so the interrupt hint is a reliable \"actively working\" signal
— the TUI analog of Claude Code's title spinner.  Status is set to
`working' only when this pattern is visible on screen; without it, the
session settles to `idle' after `codex-cli-idle-delay' of silence."
  :type 'regexp
  :group 'codex-cli)

;;;; Faces

(defface codex-cli-header-line
  '((t :inherit header-line
       :foreground "#10a37f"
       :weight bold))
  "Face for the Codex buffer header line."
  :group 'codex-cli)

;;;; Status detection hook

(defvar codex-cli-status-change-functions nil
  "Hook called when a codex terminal's status changes.
Each function receives (BUFFER STATUS) where STATUS is `idle' or `working'.")

;;;; Buffer naming and finding

(defun codex-cli--buffer-name (dir &optional instance)
  "Return buffer name for a Codex session in DIR with optional INSTANCE."
  (format "*codex:%s:%s*"
          (abbreviate-file-name (file-truename dir))
          (or instance "")))

(defun codex-cli--buffer-p (buffer)
  "Return non-nil if BUFFER is a Codex session buffer."
  (string-match-p "\\`\\*codex:" (buffer-name buffer)))

(defun codex-cli--extract-instance-name (buf-name)
  "Extract the instance name from BUF-NAME.
Buffer name format is *codex:DIR:INSTANCE*."
  (when (string-match "\\`\\*codex:[^:]+:\\(.*\\)\\*\\'" buf-name)
    (match-string 1 buf-name)))

(defun codex-cli--extract-directory (buf-name)
  "Extract the directory from BUF-NAME.
Buffer name format is *codex:DIR:INSTANCE*."
  (when (string-match "\\`\\*codex:\\([^:]+\\):" buf-name)
    (match-string 1 buf-name)))

(defun codex-cli--find-buffers ()
  "Return all live Codex session buffers."
  (cl-remove-if-not #'codex-cli--buffer-p (buffer-list)))

(defun codex-cli--find-buffers-for-directory (dir)
  "Return Codex session buffers for DIR."
  (let ((short-dir (abbreviate-file-name (file-truename dir))))
    (cl-remove-if-not
     (lambda (buf)
       (and (codex-cli--buffer-p buf)
            (string-match-p (regexp-quote short-dir) (buffer-name buf))))
     (buffer-list))))

;;;; Terminal launch

(defun codex-cli--start (dir &optional instance extra-switches)
  "Launch a Codex eat terminal in DIR with optional INSTANCE name.
EXTRA-SWITCHES are appended to `codex-cli-program-switches'.
Returns the buffer, or nil if creation failed."
  (let* ((default-directory dir)
         (buffer-name (codex-cli--buffer-name dir instance))
         (switches (append codex-cli-program-switches extra-switches))
         (process-adaptive-read-buffering nil)
         (buffer (claude-code--term-make
                  claude-code-terminal-backend
                  buffer-name
                  codex-cli-program
                  switches)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Terminal configuration (scrollback, bell, startup delay)
        (claude-code--term-configure claude-code-terminal-backend)
        ;; Switch to interactive mode (eat-semi-char-mode)
        (claude-code--term-interactive-mode claude-code-terminal-backend)
        ;; Key bindings (RET, S-RET, C-g)
        (claude-code--term-setup-keymap claude-code-terminal-backend)
        ;; Face remaps for eat terminal faces
        (claude-code--term-customize-faces claude-code-terminal-backend)
        ;; Buffer appearance (must run AFTER interactive-mode which resets
        ;; eat-invisible-cursor-type to nil)
        (codex-cli--setup-buffer-appearance)
        ;; Make Codex's hidden cursor render as a visible bar without
        ;; fighting EAT on every update hook.
        (codex-cli--apply-cursor-visibility)
        (run-hooks 'claude-code-start-hook))
      (codex-cli--install-idle-timer buffer)
      buffer)))

;;;; Buffer appearance

(defun codex-cli--setup-buffer-appearance ()
  "Configure the current buffer to look like a polished Codex terminal."
  ;; Remove visual clutter
  (setq-local vertical-scroll-bar nil)
  (when-let ((win (get-buffer-window (current-buffer))))
    (set-window-fringes win 0 0))
  ;; Emacs cursor blinking causes visible full-frame repaints in active
  ;; Eat windows on macOS, so keep it off for Codex terminals.
  (setq-local blink-cursor-mode nil)
  ;; Remove underline from nobreak-space (same as claude-code)
  (face-remap-add-relative 'nobreak-space :underline nil)
  ;; Keep cursor visible even when Codex CLI sends hide-cursor escapes.
  ;; Codex's TUI redraws can set cursor invisible; override so the user
  ;; always sees the cursor in the eat buffer.
  (setq-local eat-invisible-cursor-type '(bar nil nil))
  ;; Header line showing instance and directory
  (setq-local header-line-format
              (list
               (propertize " Codex CLI " 'face 'codex-cli-header-line)
               " "
               (propertize
                (or (codex-cli--extract-instance-name (buffer-name))
                    "")
                'face '(:weight bold))
               "  "
               (propertize
                (abbreviate-file-name default-directory)
                'face 'font-lock-comment-face))))

;;;; Cursor visibility

(defun codex-cli--nonblinking-cursor-type (cursor-spec)
  "Return CURSOR-SPEC with blinking disabled."
  (list (nth 0 cursor-spec) nil (nth 2 cursor-spec)))

(defun codex-cli--apply-cursor-visibility ()
  "Ensure Codex's hidden cursor renders as a visible bar.
Codex CLI sends hide-cursor escapes.  EAT maps that state through
`eat-invisible-cursor-type', so set that mapping to a bar cursor.

EAT implements blinking cursors by redrawing the entire frame on a
timer, which is expensive and causes visible repainting in active Codex
windows.  Force the blinking cursor variants to non-blinking mappings
before re-applying the current invisible-cursor state."
  (setq-local eat-invisible-cursor-type '(bar nil nil))
  (setq-local eat-very-visible-cursor-type
              (codex-cli--nonblinking-cursor-type
               eat-very-visible-cursor-type))
  (setq-local eat-very-visible-vertical-bar-cursor-type
              (codex-cli--nonblinking-cursor-type
               eat-very-visible-vertical-bar-cursor-type))
  (setq-local eat-very-visible-horizontal-bar-cursor-type
              (codex-cli--nonblinking-cursor-type
               eat-very-visible-horizontal-bar-cursor-type))
  (when (bound-and-true-p eat--cursor-blink-mode)
    (eat--cursor-blink-mode -1))
  (eat--set-cursor nil :invisible))

;;;; Sending commands

(defun codex-cli--send-command (cmd &optional buffer)
  "Send CMD followed by RET to codex terminal in BUFFER.
BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (when (bound-and-true-p eat-terminal)
        ;; The TUI composer submits on Enter (CR); a bare LF ("\n") is
        ;; treated as a newline in the input box and never sends.  Send the
        ;; text, then an explicit CR to submit.
        (eat-term-send-string eat-terminal cmd)
        (eat-term-send-string eat-terminal "\r")
        (run-hook-with-args 'codex-cli-status-change-functions buf 'working)))))

;;;; Process management

(defun codex-cli--alive-p (&optional buffer)
  "Return non-nil if BUFFER has a running codex process.
BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (get-buffer-process buf)
         (process-live-p (get-buffer-process buf)))))

(defun codex-cli--kill (&optional buffer)
  "Kill the codex process in BUFFER.
BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (let ((proc (get-buffer-process buf)))
        (when (and proc (process-live-p proc))
          (kill-process proc))))))

;;;; Timer-based idle detection

(defvar-local codex-cli--idle-timer nil
  "Timer that fires when Codex has been silent for `codex-cli-idle-delay'.")

(defvar-local codex-cli--status nil
  "Current status of this Codex buffer: `idle', `working', or nil.")

(defun codex-cli--install-idle-timer (buffer)
  "Install output watcher on BUFFER for timer-based idle detection.
Each time new terminal output arrives, the idle timer resets.
When the timer fires, the status changes to `idle'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq codex-cli--status 'working)
      (add-hook 'eat-update-hook
                (lambda () (codex-cli--on-output buffer))
                nil t)
      (add-hook 'kill-buffer-hook
                #'codex-cli--cancel-idle-timer nil t))))

(defun codex-cli--working-p ()
  "Return non-nil if the visible screen shows Codex's active-turn indicator.
Scans the tail of the current buffer (the on-screen region) for
`codex-cli-working-regexp'."
  (let ((start (max (point-min) (- (point-max) 4000)))
        (case-fold-search t))
    (string-match-p codex-cli-working-regexp
                    (buffer-substring-no-properties start (point-max)))))

(defun codex-cli--on-output (buffer)
  "Handle new terminal output in BUFFER.
Marks the session `working' only while the interrupt hint is on screen
\(a real active-turn signal, not mere redraw activity); (re)arms the
silence timer that settles the session to `idle'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (codex-cli--cancel-idle-timer)
      (when (and (codex-cli--working-p)
                 (not (eq codex-cli--status 'working)))
        (setq codex-cli--status 'working)
        (run-hook-with-args 'codex-cli-status-change-functions buffer 'working))
      ;; Any output resets the silence timer.  While a turn is active
      ;; Codex updates its elapsed-time counter every second, so the timer
      ;; keeps resetting; when the turn ends, output stops and it fires.
      (setq codex-cli--idle-timer
            (run-at-time codex-cli-idle-delay nil
                         #'codex-cli--idle-timer-fired buffer)))))

(defun codex-cli--idle-timer-fired (buffer)
  "Called when BUFFER has had no output for `codex-cli-idle-delay' seconds.
Sustained silence means the turn ended (or the interrupt hint got stuck
with no further updates) — either way, settle to `idle'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq codex-cli--idle-timer nil)
      (unless (eq codex-cli--status 'idle)
        (setq codex-cli--status 'idle)
        (run-hook-with-args 'codex-cli-status-change-functions buffer 'idle)))))

(defun codex-cli--cancel-idle-timer ()
  "Cancel the idle timer in the current buffer."
  (when codex-cli--idle-timer
    (cancel-timer codex-cli--idle-timer)
    (setq codex-cli--idle-timer nil)))

(provide 'codex-cli)
;;; codex-cli.el ends here
