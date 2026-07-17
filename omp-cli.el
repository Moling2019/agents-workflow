;;; omp-cli.el --- Minimal library for Oh My Pi (omp) CLI terminals -*- lexical-binding: t; -*-

;; Author: Moling Zhang
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides buffer management, terminal launch, command sending, and
;; timer-based status detection for Oh My Pi (omp) CLI sessions running
;; in eat terminals.  Companion to claude-workflow.el / codex-cli.el /
;; opencode-cli.el; modeled directly on opencode-cli.el because omp is
;; likewise a streaming TUI that does not emit a Claude-style idle title
;; glyph.
;;
;; Status detection: after each meaningful terminal output, reset an idle
;; timer.  When the timer fires (no new output for N seconds), the session
;; is idle.  Status is set to `working' only when omp's interrupt hint
;; ("esc … interrupt") is visible on screen — a real active-turn signal,
;; not mere redraw activity.
;;
;; Session capture: omp stores sessions as JSONL files under
;; `~/.omp/agent/sessions/<encoded-path>/<timestamp>_<sessionId>.jsonl'.
;; The `--resume' flag accepts an ID prefix, path, or picker.  Session
;; IDs are resolved post-hoc by scanning the sessions directory,
;; mirroring opencode-cli's approach.  The session id is the UUID in
;; the filename (after the first `_'), which `--resume' accepts
;; directly.

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

(defgroup omp-cli nil
  "Oh My Pi (omp) CLI terminal management."
  :group 'tools
  :prefix "omp-cli-")

(defcustom omp-cli-program "omp"
  "Program to run Oh My Pi (omp) CLI."
  :type 'string
  :group 'omp-cli)

(defcustom omp-cli-program-switches '("--auto-approve" "--model" "portkey/@modal/GLM-5-2-FP8")
  "Base switches for omp CLI, prepended before any extra switches.
Defaults to (\"--auto-approve\" \"--model\" \"portkey/@modal/GLM-5-2-FP8\") —
omp's auto-approve mode plus the GLM 5.2 (Modal FP8) model via the
DoorDash Portkey gateway.  Set to \(\"--auto-approve\") to use omp's
default model, or change the model as needed."
  :type '(repeat string)
  :group 'omp-cli)

(defcustom omp-cli-startup-delay 0.5
  "Seconds to wait after creating the terminal before it is usable."
  :type 'number
  :group 'omp-cli)

(defcustom omp-cli-idle-delay 5
  "Seconds of no terminal output before considering omp idle.
After each meaningful output chunk, the idle timer resets.  When no
new output arrives for this many seconds, the status changes to `idle'."
  :type 'number
  :group 'omp-cli)

(defcustom omp-cli-working-regexp "⟲\\|▶\\|⠂\\|⠐\\|⠒\\|⠓"
  "Regexp matching omp's active-turn indicator in the status bar.
While a turn is running omp shows a spinner symbol (⟲, ▶, or braille
dots ⠂⠐⠒⠓) in the status bar footer.  Status is set to `working' only
when this pattern is visible; without it, the session settles to
`idle' after `omp-cli-idle-delay' of silence."
  :type 'regexp
  :group 'omp-cli)

(defcustom omp-cli-keybindings
  '(("C->" . omp-cli-scroll-to-bottom)  ; ctrl+shift+> -> jump to latest output
    ("C-<" . omp-cli-scroll-to-top)     ; ctrl+shift+< -> jump to start of transcript
    ("C-u" . omp-cli-scroll-page-up)    ; ctrl+u       -> page up (earlier output)
    ("C-n" . omp-cli-scroll-page-down)  ; ctrl+n       -> page down (later output)
    ("C-g" . "\e"))                     ; ctrl+g       -> Esc -> cancel/interrupt in omp
  "Keyboard bindings for omp eat buffers, as (KEY . ACTION).
KEY is a `kbd' string bound with precedence over eat's key-forwarding in
omp buffers.  ACTION is either:
  - a command symbol, bound directly (used for scrolling); or
  - a string, sent to omp as a raw terminal escape sequence.

Scrolling navigates EAT's own buffer scrollback (omp keeps the full
transcript there), NOT omp's TUI — sending PageUp/PageDown into omp only
cycles the composer's prompt history, so those are handled in Emacs.
Cancel (C-g) is the exception that must reach omp: it sends a single Esc,
so — like Emacs's `keyboard-quit' — it aborts the current omp operation
\(press twice for a double-Esc interrupt where the TUI needs it)."
  :type '(alist :key-type string
                :value-type (choice (function :tag "Emacs command")
                                    (string :tag "Escape sequence to omp")))
  :group 'omp-cli)

;;;; Faces

(defface omp-cli-header-line
  '((t :inherit header-line
       :foreground "#c678dd"
       :weight bold))
  "Face for the omp buffer header line."
  :group 'omp-cli)

;;;; Status detection hook

(defvar omp-cli-status-change-functions nil
  "Hook called when an omp terminal's status changes.
Each function receives (BUFFER STATUS) where STATUS is `idle' or `working'.")

;;;; Buffer naming and finding

(defun omp-cli--buffer-name (dir &optional instance)
  "Return buffer name for an omp session in DIR with optional INSTANCE."
  (format "*omp:%s:%s*"
          (abbreviate-file-name (file-truename dir))
          (or instance "")))

(defun omp-cli--buffer-p (buffer)
  "Return non-nil if BUFFER is an omp session buffer."
  (string-match-p "\\`\\*omp:" (buffer-name buffer)))

(defun omp-cli--extract-instance-name (buf-name)
  "Extract the instance name from BUF-NAME.
Buffer name format is *omp:DIR:INSTANCE*."
  (when (string-match "\\`\\*omp:[^:]+:\\(.*\\)\\*\\'" buf-name)
    (match-string 1 buf-name)))

(defun omp-cli--extract-directory (buf-name)
  "Extract the directory from BUF-NAME.
Buffer name format is *omp:DIR:INSTANCE*."
  (when (string-match "\\`\\*omp:\\([^:]+\\):" buf-name)
    (match-string 1 buf-name)))

(defun omp-cli--find-buffers ()
  "Return all live omp session buffers."
  (cl-remove-if-not #'omp-cli--buffer-p (buffer-list)))

(defun omp-cli--find-buffers-for-directory (dir)
  "Return omp session buffers for DIR."
  (let ((short-dir (abbreviate-file-name (file-truename dir))))
    (cl-remove-if-not
     (lambda (buf)
       (and (omp-cli--buffer-p buf)
            (string-match-p (regexp-quote short-dir) (buffer-name buf))))
     (buffer-list))))

;;;; Session capture (JSONL files under ~/.omp/agent/sessions/)

(defun omp-cli--sessions-dir ()
  "Return the directory where omp stores session files."
  (expand-file-name "agent/sessions/" (or (getenv "PI_CODING_AGENT_DIR")
                                           "~/.omp")))

(defun omp-cli--encode-dir (dir)
  "Encode DIR the way omp encodes working directories in session paths.
omp uses home-relative single-dash encoding: paths under the home
directory become `-<rel>' with each `/' replaced by `-'.  Paths under
the OS temp root become `-tmp-<rel>'.  Other absolute paths use the
legacy `--<abs>--' form.  Example: `~/Documents/work/foo' becomes
`-Documents-work-foo'."
  (let* ((truename (file-truename (expand-file-name dir)))
         (home (file-truename (expand-file-name "~")))
         (tmp-dir (temporary-file-directory))
         (tmp (file-truename (expand-file-name tmp-dir))))
    (cond
     ;; Home-relative: -<rel> with / replaced by -
     ((string-prefix-p (concat home "/") truename)
      (let ((rel (substring truename (1+ (length home)))))
        (concat "-" (replace-regexp-in-string "/" "-" rel))))
     ;; /tmp-relative: -tmp-<rel>
     ((string-prefix-p (concat tmp "/") truename)
      (let ((rel (substring truename (1+ (length tmp)))))
        (concat "-tmp-" (replace-regexp-in-string "/" "-" rel))))
     ;; Legacy absolute: --<abs-with-single-dash-internals>--
     (t
      (let* ((stripped (directory-file-name truename))
             ;; Strip leading slash, replace each / with single -
             (rel (substring stripped 1))
             (dashed (replace-regexp-in-string "/" "-" rel)))
        (concat "--" dashed "--"))))))

(defun omp-cli--session-list (dir)
  "Return omp sessions for DIR as a list of alists, or nil.
Scans the encoded session directory under
`~/.omp/agent/sessions/' and globs its `*.jsonl' files.  Each
element carries `id' (the session UUID parsed from the filename),
`path' (the full JSONL file path), `directory', and `updated'
(mtime).  Returns nil on any error or when there are no sessions."
  (let ((sessions-dir (omp-cli--sessions-dir))
        (target (file-truename (expand-file-name dir)))
        (encoded (omp-cli--encode-dir dir))
        (result nil))
    (when (file-directory-p sessions-dir)
      (let ((session-dir (expand-file-name encoded sessions-dir)))
        (when (file-directory-p session-dir)
          (ignore-errors
            (dolist (entry (directory-files session-dir t "\\.jsonl\\'"))
              (let* ((name (file-name-nondirectory entry))
                     ;; Filename: <timestamp>_<sessionId>.jsonl
                     ;; Extract UUID after the first `_'.
                     (id (and (string-match "\\`[^_]+_\\(.+\\)\\.jsonl\\'" name)
                              (match-string 1 name))))
                (when id
                  (push (list (cons 'id id)
                              (cons 'path entry)
                              (cons 'directory target)
                              (cons 'updated (float-time
                                               (file-attribute-modification-time
                                                (file-attributes entry)))))
                        result)))))))
      (sort result (lambda (a b) (> (alist-get 'updated a 0)
                                     (alist-get 'updated b 0)))))))

(defun omp-cli--latest-session-id-for-dir (dir)
  "Return the UUID of the newest omp session whose directory is DIR.
Returns the session UUID (which `--resume' accepts as a prefix) for
the most recently modified session matching DIR.  Returns nil when
none match."
  (when-let ((sessions (omp-cli--session-list dir)))
    (alist-get 'id (car sessions))))

(defun omp-cli--session-file-for-id (dir id)
  "Return the JSONL file path for omp session ID in DIR, or nil.
ID is the session UUID (as returned by
`omp-cli--latest-session-id-for-dir').  Globs the encoded session
directory for `*_<ID>.jsonl' and returns the full path of the first
match."
  (let* ((sessions-dir (omp-cli--sessions-dir))
         (encoded (omp-cli--encode-dir dir))
         (session-dir (expand-file-name encoded sessions-dir)))
    (when (file-directory-p session-dir)
      (ignore-errors
        (let ((files (directory-files session-dir t
                                      (concat (regexp-quote id) "\\.jsonl\\'"))))
          (car files))))))

(defun omp-cli--export-last-assistant-text (jsonl-string)
  "Parse JSONL-STRING from an omp session file and return the last
assistant message's text, or nil.
omp session files are JSONL where line 1 is a header
\(`{\"type\":\"session\",...}') and subsequent lines are entries.
`message' entries carry the conversation under a nested `message'
field: `{\"type\":\"message\",\"message\":{\"role\":\"assistant\",\"content\":[...]}}'.
This function scans all entries and returns the last one whose
`message.role' is `\"assistant\"' with non-empty text."
  (ignore-errors
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          (result nil))
      (dolist (line (split-string jsonl-string "\n"))
        (when (string-prefix-p "{" (string-trim line))
          (let* ((data (json-read-from-string line))
                 (msg (alist-get 'message data))
                 (role (and msg (alist-get 'role msg)))
                 (content (and msg (alist-get 'content msg))))
            (when (and role (equal role "assistant"))
              (let ((text ""))
                (cond
                 ;; content as string
                 ((stringp content)
                  (setq text content))
                 ;; content as array of parts
                 ((listp content)
                  (dolist (part content)
                    (when (stringp part)
                      (setq text (concat text part)))
                    (when-let ((pt (alist-get 'text part)))
                      (when (equal (or (alist-get 'type part) "text") "text")
                        (setq text (concat text pt)))))))
                (unless (string-empty-p (string-trim text))
                  (setq result text)))))))
      result)))


;;;; Terminal launch

(defun omp-cli--start (dir &optional instance extra-switches)
  "Launch an omp eat terminal in DIR with optional INSTANCE name.
EXTRA-SWITCHES are appended to `omp-cli-program-switches'.
Returns the buffer, or nil if creation failed."
  ;; Ensure the shared eat-terminal backend generics (claude-code--term-*)
  ;; are loaded — an omp agent may be launched before any claude agent has
  ;; pulled in claude-code.  Runtime require (not top-level) so the byte
  ;; compiler doesn't drag claude-code's own deps into the build.
  (require 'claude-code)
  (let* ((default-directory dir)
         (buffer-name (omp-cli--buffer-name dir instance))
         (switches (append omp-cli-program-switches extra-switches))
         (process-adaptive-read-buffering nil)
         (buffer (claude-code--term-make
                  claude-code-terminal-backend
                  buffer-name
                  omp-cli-program
                  switches)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (claude-code--term-configure claude-code-terminal-backend)
        (claude-code--term-interactive-mode claude-code-terminal-backend)
        (claude-code--term-setup-keymap claude-code-terminal-backend)
        (claude-code--term-customize-faces claude-code-terminal-backend)
        (omp-cli--setup-buffer-appearance)
        (omp-cli--apply-cursor-visibility)
        (omp-cli--install-keybindings)
        (run-hooks 'claude-code-start-hook))
      (omp-cli--install-idle-timer buffer)
      buffer)))

;;;; Buffer appearance

(defun omp-cli--setup-buffer-appearance ()
  "Configure the current buffer to look like a polished omp terminal."
  (setq-local vertical-scroll-bar nil)
  ;; A high global `scroll-conservatively' (e.g. 100000) makes Emacs crawl
  ;; line-by-line to bring point into view — so switching to a long omp
  ;; buffer scrolls visibly from top to bottom.  Force recenter/jump here
  ;; (buffer-local; the global preference is left untouched).  eat still
  ;; positions the cursor during live output via its own `recenter'.
  (setq-local scroll-conservatively 0)
  (setq-local scroll-margin 0)
  (when-let ((win (get-buffer-window (current-buffer))))
    (set-window-fringes win 0 0))
  (setq-local blink-cursor-mode nil)
  (face-remap-add-relative 'nobreak-space :underline nil)
  (setq-local eat-invisible-cursor-type '(bar nil nil))
  (setq-local header-line-format
              (list
               (propertize " Oh My Pi " 'face 'omp-cli-header-line)
               " "
               (propertize
                (or (omp-cli--extract-instance-name (buffer-name))
                    "")
                'face '(:weight bold))
               "  "
               (propertize
                (abbreviate-file-name default-directory)
                'face 'font-lock-comment-face))))

;;;; Cursor visibility

(defun omp-cli--nonblinking-cursor-type (cursor-spec)
  "Return CURSOR-SPEC with blinking disabled."
  (list (nth 0 cursor-spec) nil (nth 2 cursor-spec)))

(defun omp-cli--apply-cursor-visibility ()
  "Ensure omp's hidden cursor renders as a visible bar.
omp's TUI sends hide-cursor escapes; EAT maps that state through
`eat-invisible-cursor-type'.  Force blinking cursor variants to
non-blinking mappings to avoid expensive full-frame repaints, then
re-apply the invisible-cursor state."
  (setq-local eat-invisible-cursor-type '(bar nil nil))
  (setq-local eat-very-visible-cursor-type
              (omp-cli--nonblinking-cursor-type
               eat-very-visible-cursor-type))
  (setq-local eat-very-visible-vertical-bar-cursor-type
              (omp-cli--nonblinking-cursor-type
               eat-very-visible-vertical-bar-cursor-type))
  (setq-local eat-very-visible-horizontal-bar-cursor-type
              (omp-cli--nonblinking-cursor-type
               eat-very-visible-horizontal-bar-cursor-type))
  (when (bound-and-true-p eat--cursor-blink-mode)
    (eat--cursor-blink-mode -1))
  (eat--set-cursor nil :invisible))

;;;; Keyboard scrolling

;; omp is a full-screen TUI: it maps PageUp/PageDown to composer prompt
;; history, not viewport scrolling, so escape sequences can't scroll its
;; transcript.  EAT, however, retains the whole transcript as buffer
;; scrollback, so scrolling is done in Emacs against that buffer.

(defun omp-cli-scroll-page-up ()
  "Scroll the omp transcript up by a page (show earlier output)."
  (interactive)
  (condition-case nil (scroll-down-command) (error (goto-char (point-min)))))

(defun omp-cli-scroll-page-down ()
  "Scroll the omp transcript down by a page (show later output)."
  (interactive)
  (condition-case nil (scroll-up-command) (error (goto-char (point-max)))))

(defun omp-cli-scroll-to-top ()
  "Jump to the start of the omp transcript."
  (interactive)
  (goto-char (point-min)))

(defun omp-cli-scroll-to-bottom ()
  "Jump to the latest omp output."
  (interactive)
  (goto-char (point-max))
  (when (get-buffer-window) (recenter -1)))

(defun omp-cli--install-keybindings ()
  "Bind `omp-cli-keybindings' in the current omp eat buffer.
Installs them via `minor-mode-overriding-map-alist' for `eat--semi-char-mode'
so they take precedence over eat's key-forwarding for this buffer only; a
parent keymap of `eat-semi-char-mode-map' lets every other key fall through
to eat unchanged.  Each binding either runs an Emacs command (scrolling EAT's
buffer scrollback) or sends a raw escape SEQUENCE to omp (Esc for cancel)."
  (when (boundp 'eat-semi-char-mode-map)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map eat-semi-char-mode-map)
      (dolist (kv omp-cli-keybindings)
        (let ((key (car kv))
              (action (cdr kv)))
          (define-key map (kbd key)
            (if (stringp action)
                (lambda ()
                  (interactive)
                  (when (bound-and-true-p eat-terminal)
                    (eat-term-send-string eat-terminal action)))
              action))))
      (setq-local minor-mode-overriding-map-alist
                  (cons (cons 'eat--semi-char-mode map)
                        (and (boundp 'minor-mode-overriding-map-alist)
                             minor-mode-overriding-map-alist))))))

;;;; Sending commands

(defun omp-cli--send-command (cmd &optional buffer)
  "Send CMD followed by RET to omp terminal in BUFFER.
BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (when (bound-and-true-p eat-terminal)
        ;; omp's TUI composer submits on Enter (CR); a bare LF ("\n")
        ;; is treated as a newline in the input box and never sends.  Send
        ;; the text, then an explicit CR to submit.
        (eat-term-send-string eat-terminal cmd)
        (eat-term-send-string eat-terminal "\r")
        (run-hook-with-args 'omp-cli-status-change-functions buf 'working)))))

;;;; Process management

(defun omp-cli--alive-p (&optional buffer)
  "Return non-nil if BUFFER has a running omp process.
BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (get-buffer-process buf)
         (process-live-p (get-buffer-process buf)))))

(defun omp-cli--kill (&optional buffer)
  "Kill the omp process in BUFFER.
BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (let ((proc (get-buffer-process buf)))
        (when (and proc (process-live-p proc))
          (kill-process proc))))))

;;;; Timer-based idle detection

(defvar-local omp-cli--idle-timer nil
  "Timer that fires when omp has been silent for `omp-cli-idle-delay'.")

(defvar-local omp-cli--status nil
  "Current status of this omp buffer: `idle', `working', or nil.")

(defun omp-cli--install-idle-timer (buffer)
  "Install output watcher on BUFFER for timer-based idle detection.
Each time new terminal output arrives, the idle timer resets.
When the timer fires, the status changes to `idle'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq omp-cli--status 'working)
      (add-hook 'eat-update-hook
                (lambda () (omp-cli--on-output buffer))
                nil t)
      (add-hook 'kill-buffer-hook
                #'omp-cli--cancel-idle-timer nil t))))

(defun omp-cli--working-p ()
  "Return non-nil if the visible screen shows omp's active-turn indicator.
Scans the tail of the current buffer (the on-screen region) for
`omp-cli-working-regexp'."
  (let ((start (max (point-min) (- (point-max) 4000)))
        (case-fold-search t))
    (string-match-p omp-cli-working-regexp
                    (buffer-substring-no-properties start (point-max)))))

(defun omp-cli--on-output (buffer)
  "Handle new terminal output in BUFFER.
This is a FALLBACK for status detection.  The primary signal comes
from omp's --hook extension events (before_provider_request → working,
session.idle → idle) pushed via `agents-workflow-handle-omp-status'.

The timer fallback only kicks in on sustained silence (no output for
`omp-cli-idle-delay' seconds) to settle the session to idle — in case
the hook fails to fire.  We do NOT mark working on every output event,
because omp's TUI redraws on cursor blinks, MCP status updates, and
other non-generation activity, which would cause false positives."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (omp-cli--cancel-idle-timer)
      ;; Only arm the idle fallback timer — don't mark working here.
      ;; The hook handles the working→idle transition deterministically.
      (setq omp-cli--idle-timer
            (run-at-time omp-cli-idle-delay nil
                         #'omp-cli--idle-timer-fired buffer)))))

(defun omp-cli--idle-timer-fired (buffer)
  "Called when BUFFER has had no output for `omp-cli-idle-delay' seconds.
Sustained silence means the turn ended (or the TUI got stuck with no
further updates) — settle to `idle' and trigger last-output capture
as a fallback in case the hook didn't fire."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq omp-cli--idle-timer nil)
      (unless (eq omp-cli--status 'idle)
        (setq omp-cli--status 'idle)
        (run-hook-with-args 'omp-cli-status-change-functions buffer 'idle)))))

(defun omp-cli--cancel-idle-timer ()
  "Cancel the idle timer in the current buffer."
  (when omp-cli--idle-timer
    (cancel-timer omp-cli--idle-timer)
    (setq omp-cli--idle-timer nil)))

(provide 'omp-cli)
;;; omp-cli.el ends here
