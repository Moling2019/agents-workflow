;;; databricks-runs.el --- Monitor Databricks job runs -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Display Databricks job runs in a dedicated buffer with auto-refresh.
;; Usage: M-x list-databricks-runs

;;; Code:

(require 'seq)

;; Optional agents-workflow integration (loaded at runtime)
(declare-function agents-workflow--get "agents-workflow")
(declare-function agents-workflow--find-agent-by-name "agents-workflow")
(declare-function agents-workflow-agent-status "agents-workflow")
(declare-function agents-workflow--ensure-agent-buffer "agents-workflow")
(declare-function agents-workflow--send-to-interactive "agents-workflow")

(defgroup databricks-runs nil
  "Monitor Databricks job runs."
  :group 'tools
  :prefix "databricks-runs-")

(defcustom databricks-runs-refresh-interval 30
  "Seconds between auto-refresh cycles."
  :type 'integer
  :group 'databricks-runs)

(defcustom databricks-runs-python "python3"
  "Path to Python interpreter with the toolkit installed."
  :type 'string
  :group 'databricks-runs)

(defcustom databricks-runs-env-file nil
  "Path to .env file with Databricks credentials."
  :type '(choice (const nil) string)
  :group 'databricks-runs)

(defcustom databricks-runs-cli-profile nil
  "Databricks CLI profile for OAuth."
  :type '(choice (const nil) string)
  :group 'databricks-runs)

(defcustom databricks-runs-repo-dir nil
  "Directory containing the utils.databricks_job_submit package."
  :type '(choice (const nil) string)
  :group 'databricks-runs)

(defvar databricks-runs--timer nil
  "Timer for auto-refresh.")

(defvar databricks-runs--buffer-name "*Databricks Runs*"
  "Name of the Databricks runs buffer.")

(defvar databricks-runs--process nil
  "Current async process for fetching runs.")

(defvar databricks-runs--owners nil
  "Alist: run-id (string) -> (workflow-name . agent-name).")

(defvar databricks-runs--previous-cache nil
  "Previous poll's cache snapshot for state-diff detection.")

(defconst databricks-runs--terminal-states
  '("TERMINATED" "INTERNAL_ERROR" "SKIPPED")
  "Life-cycle states indicating run completion.")

;;; --- Ownership Registry ---

(defun databricks-runs-register-owner (run-id workflow-name agent-name)
  "Register RUN-ID as owned by AGENT-NAME in WORKFLOW-NAME.
Callable via `emacsclient -e'."
  (setq databricks-runs--owners
        (cons (cons run-id (cons workflow-name agent-name))
              (assoc-delete-all run-id databricks-runs--owners))))

(defun databricks-runs-unregister-owner (run-id)
  "Remove ownership entry for RUN-ID."
  (setq databricks-runs--owners
        (assoc-delete-all run-id databricks-runs--owners)))

(defun databricks-runs-list-owners ()
  "Return the current ownership registry (debug helper)."
  databricks-runs--owners)

;;; --- State Transition Detection ---

(defun databricks-runs--run-terminal-p (run)
  "Return non-nil if RUN is in a terminal life-cycle state."
  (member (alist-get 'state run) databricks-runs--terminal-states))

(defun databricks-runs--detect-completions (old-cache new-cache)
  "Return runs terminal in NEW-CACHE but not in OLD-CACHE.
Both caches are vectors/lists of alists with `run_id' and `state' keys."
  (let ((old-states (make-hash-table :test #'equal))
        (result '()))
    (seq-doseq (run (or old-cache []))
      (puthash (alist-get 'run_id run) (alist-get 'state run) old-states))
    (seq-doseq (run (or new-cache []))
      (when (databricks-runs--run-terminal-p run)
        (let ((old-state (gethash (alist-get 'run_id run) old-states)))
          (when (or (null old-state)
                    (not (member old-state databricks-runs--terminal-states)))
            (push run result)))))
    (nreverse result)))

;;; --- Notification Dispatch ---

(defun databricks-runs--notify-owner (run)
  "Notify the registered owner of RUN if it completed.
Looks up owner, delivers message, unregisters on success."
  (condition-case err
      (let* ((run-id (alist-get 'run_id run))
             (owner (cdr (assoc run-id databricks-runs--owners))))
        (when owner
          (let ((wf-name (car owner))
                (agent-name (cdr owner)))
            (databricks-runs--deliver-message wf-name agent-name run))))
    (error
     (message "Databricks: notification error: %S" err))))

(defun databricks-runs--deliver-message (wf-name agent-name run)
  "Deliver completion message for RUN to AGENT-NAME in WF-NAME.
If agent is busy, re-registers for retry on next poll.
If agent/workflow not found, logs warning and unregisters."
  (let* ((run-id (alist-get 'run_id run))
         (run-name (alist-get 'run_name run))
         (result-state (alist-get 'result_state run))
         (url (alist-get 'run_page_url run))
         (msg (format "Your Databricks run has completed. Run: %s, Result: %s, URL: %s"
                       run-name result-state url))
         (wf (and (fboundp 'agents-workflow--get)
                  (agents-workflow--get wf-name)))
         (agent (and wf
                     (fboundp 'agents-workflow--find-agent-by-name)
                     (agents-workflow--find-agent-by-name wf agent-name))))
    (cond
     ((not agent)
      (message "Databricks: workflow %S / agent %S not found, unregistering run %s"
               wf-name agent-name run-id)
      (databricks-runs-unregister-owner run-id))
     ((memq (agents-workflow-agent-status agent) '(idle waiting))
      (let ((buf (agents-workflow--ensure-agent-buffer agent)))
        (agents-workflow--send-to-interactive agent buf msg)
        (message "Databricks: notified %s/%s about run %s (%s)"
                 wf-name agent-name run-id result-state)
        (databricks-runs-unregister-owner run-id)))
     (t
      (message "Databricks: agent %s/%s busy (status: %s), will retry on next poll"
               wf-name agent-name (agents-workflow-agent-status agent))))))

;;; --- Faces ---

(defface databricks-runs-header
  '((t :inherit font-lock-comment-face :italic t))
  "Face for the header line."
  :group 'databricks-runs)

(defface databricks-runs-column-header
  '((t :inherit bold :underline t))
  "Face for column headers."
  :group 'databricks-runs)

(defface databricks-runs-separator
  '((t :inherit font-lock-comment-face))
  "Face for the separator line."
  :group 'databricks-runs)

(defface databricks-runs-running
  '((t :inherit success :bold t))
  "Face for RUNNING state."
  :group 'databricks-runs)

(defface databricks-runs-pending
  '((t :inherit warning :bold t))
  "Face for PENDING state."
  :group 'databricks-runs)

(defface databricks-runs-success
  '((t :inherit font-lock-string-face))
  "Face for SUCCESS state."
  :group 'databricks-runs)

(defface databricks-runs-failed
  '((t :inherit error :bold t))
  "Face for FAILED state."
  :group 'databricks-runs)

(defface databricks-runs-canceled
  '((t :inherit font-lock-comment-face))
  "Face for CANCELED state."
  :group 'databricks-runs)

(defface databricks-runs-run-id
  '((t :inherit font-lock-comment-face))
  "Face for run IDs."
  :group 'databricks-runs)

(defvar databricks-runs-font-lock-keywords
  `(;; Header line
    ("^Databricks Runs.*$" . 'databricks-runs-header)
    ;; Column headers
    ("^  State .*$" . 'databricks-runs-column-header)
    ;; Separator
    ("^  -+$" . 'databricks-runs-separator)
    ;; State keywords at start of data lines
    ("^  \\(RUNNING\\)" 1 'databricks-runs-running)
    ("^  \\(PENDING\\)" 1 'databricks-runs-pending)
    ("^  \\(STOPPING\\)" 1 'databricks-runs-pending)
    ("^  \\(SUCCESS\\)" 1 'databricks-runs-success)
    ("^  \\(FAILED\\)" 1 'databricks-runs-failed)
    ("^  \\(ERROR\\)" 1 'databricks-runs-failed)
    ("^  \\(CANCELED\\)" 1 'databricks-runs-canceled)
    ("^  \\(SKIPPED\\)" 1 'databricks-runs-canceled)
    ;; Run IDs at end of lines (long digit sequences)
    ("\\([0-9]\\{10,\\}\\)$" 1 'databricks-runs-run-id))
  "Font-lock keywords for `databricks-runs-mode'.")

(defun databricks-runs--python-script ()
  "Return the Python script that fetches and prints runs."
  (format "
import sys, os
sys.path.insert(0, %S)
from utils.databricks_job_submit import list_my_runs, format_runs_table
from datetime import datetime
runs = list_my_runs(active_only=False, cli_profile=%S)
now = datetime.now().strftime('%%Y-%%m-%%d %%H:%%M:%%S')
print(f'Databricks Runs  |  {now}  |  refresh: %ds  |  g=refresh  q=quit')
print()
print(format_runs_table(runs))
" databricks-runs-repo-dir
  databricks-runs-cli-profile
  databricks-runs-refresh-interval))

(defun databricks-runs--json-python-script ()
  "Return a Python script that fetches runs and outputs JSON."
  (format "
import sys, os, json
sys.path.insert(0, %S)
from utils.databricks_job_submit import list_my_runs
runs = list_my_runs(active_only=False, cli_profile=%S)
out = []
for r in runs:
    out.append({
        'run_id': str(r.get('run_id', '')),
        'run_name': r.get('run_name', ''),
        'state': r.get('state', {}).get('life_cycle_state', 'UNKNOWN'),
        'result_state': r.get('state', {}).get('result_state', ''),
        'start_time': r.get('start_time', 0),
        'end_time': r.get('end_time', 0),
        'run_page_url': r.get('run_page_url', '')
    })
print(json.dumps(out))
" databricks-runs-repo-dir
  databricks-runs-cli-profile))

(defvar-local databricks-runs--cache nil
  "Cached JSON run data for panel entries.")

(defvar-local databricks-runs--json-process nil
  "Async process for JSON fetch.  Buffer-local to avoid races.")

(defvar-local databricks-runs--expanded nil
  "When non-nil, show up to 30 runs.  When nil, show only top 3.")

(defun databricks-runs--fetch-json ()
  "Fetch runs as JSON and populate `databricks-runs--cache'."
  (when (and databricks-runs--json-process
             (process-live-p databricks-runs--json-process))
    (delete-process databricks-runs--json-process))
  (let* ((buf (current-buffer))
         (cmd (concat (databricks-runs--env-shell-prefix)
                      (shell-quote-argument databricks-runs-python)
                      " -c "
                      (shell-quote-argument (databricks-runs--json-python-script))))
         (proc (start-process-shell-command "databricks-runs-json" nil cmd))
         (output-chunks '()))
    (setq databricks-runs--json-process proc)
    (set-process-filter proc
                        (lambda (_p chunk) (push chunk output-chunks)))
    (set-process-sentinel
     proc
     (lambda (_p event)
       (when (string-match-p "finished" event)
         (let ((json-str (apply #'concat (nreverse output-chunks))))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (let ((new-data (condition-case nil
                                   (json-read-from-string json-str)
                                 (error nil))))
                 (setq databricks-runs--cache new-data)
                 (when databricks-runs--owners
                   (dolist (run (databricks-runs--detect-completions
                                 databricks-runs--previous-cache new-data))
                     (databricks-runs--notify-owner run)))
                 (setq databricks-runs--previous-cache new-data))))))))))

(defun databricks-runs--env-shell-prefix ()
  "Return shell command prefix that sources the .env file."
  (if (file-exists-p databricks-runs-env-file)
      (format "set -a && source %s && set +a && "
              (shell-quote-argument databricks-runs-env-file))
    ""))

(defun databricks-runs-refresh ()
  "Refresh the Databricks runs buffer asynchronously."
  (interactive)
  (let ((buf (get-buffer databricks-runs--buffer-name)))
    (when (and buf (buffer-live-p buf))
      ;; Kill any in-flight process
      (when (and databricks-runs--process
                 (process-live-p databricks-runs--process))
        (delete-process databricks-runs--process))
      (let* ((cmd (concat (databricks-runs--env-shell-prefix)
                          (shell-quote-argument databricks-runs-python)
                          " -c "
                          (shell-quote-argument (databricks-runs--python-script))))
             (proc (start-process-shell-command
                    "databricks-runs" nil cmd)))
        (setq databricks-runs--process proc)
        (let ((output-chunks '()))
          (set-process-filter
           proc
           (lambda (_proc chunk)
             (push chunk output-chunks)))
          (set-process-sentinel
           proc
           (lambda (_proc event)
             (when (string-match-p "finished" event)
               (let ((output (apply #'concat (nreverse output-chunks))))
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (let ((inhibit-read-only t)
                           (pos (point)))
                       (erase-buffer)
                       (insert output)
                       (goto-char (min pos (point-max)))))))))))))))

(defun databricks-runs--start-timer ()
  "Start the auto-refresh timer."
  (databricks-runs--stop-timer)
  (setq databricks-runs--timer
        (run-at-time databricks-runs-refresh-interval
                     databricks-runs-refresh-interval
                     #'databricks-runs-refresh)))

(defun databricks-runs--stop-timer ()
  "Stop the auto-refresh timer."
  (when databricks-runs--timer
    (cancel-timer databricks-runs--timer)
    (setq databricks-runs--timer nil)))

(defun databricks-runs-quit ()
  "Quit the Databricks runs buffer and stop auto-refresh."
  (interactive)
  (databricks-runs--stop-timer)
  (when (and databricks-runs--process
             (process-live-p databricks-runs--process))
    (delete-process databricks-runs--process))
  (quit-window t))

(defvar databricks-runs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'databricks-runs-refresh)
    (define-key map "q" #'databricks-runs-quit)
    map)
  "Keymap for `databricks-runs-mode'.")

(define-derived-mode databricks-runs-mode special-mode "Databricks-Runs"
  "Major mode for viewing Databricks job runs.
\\{databricks-runs-mode-map}"
  (setq font-lock-defaults '(databricks-runs-font-lock-keywords))
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (databricks-runs-refresh)))
  (databricks-runs--start-timer))

(add-hook 'databricks-runs-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook #'databricks-runs--stop-timer nil t)))

;;;###autoload
(defun list-databricks-runs ()
  "Open a buffer showing Databricks job runs with auto-refresh."
  (interactive)
  (let ((buf (get-buffer-create databricks-runs--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'databricks-runs-mode)
        (databricks-runs-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading Databricks runs...")))
    (pop-to-buffer buf)
    (databricks-runs-refresh)))

;;; --- Panel helpers ---

(defun databricks-runs--format-duration (start-ms end-ms)
  "Format duration between START-MS and END-MS (epoch millis)."
  (if (or (= start-ms 0) (= end-ms 0))
      (let ((now-ms (* (float-time) 1000)))
        (if (= start-ms 0) "—"
          (let ((secs (/ (- now-ms start-ms) 1000)))
            (cond ((< secs 60) (format "%ds" (truncate secs)))
                  ((< secs 3600) (format "%dm %ds" (truncate (/ secs 60)) (mod (truncate secs) 60)))
                  (t (format "%dh %dm" (truncate (/ secs 3600)) (mod (truncate (/ secs 60)) 60)))))))
    (let ((secs (/ (- end-ms start-ms) 1000)))
      (cond ((< secs 60) (format "%ds" (truncate secs)))
            ((< secs 3600) (format "%dm %ds" (truncate (/ secs 60)) (mod (truncate secs) 60)))
            (t (format "%dh %dm" (truncate (/ secs 3600)) (mod (truncate (/ secs 60)) 60)))))))

(defun databricks-runs--format-start-time (epoch-ms)
  "Format EPOCH-MS as a human-readable start time."
  (if (= epoch-ms 0) "—"
    (format-time-string "%b %d %H:%M" (seconds-to-time (/ epoch-ms 1000)))))

(defun databricks-runs--status-cell (state result-state)
  "Return a propertized status string for STATE and RESULT-STATE."
  (require 'claude-dashboard)
  (let* ((effective (cond
                     ((and (not (equal result-state ""))
                           (member state '("TERMINATED" "INTERNAL_ERROR")))
                      result-state)
                     (t state)))
         (icon-name (cond
                     ((member effective '("RUNNING" "PENDING")) 'running)
                     ((equal effective "SUCCESS") 'success)
                     ((member effective '("FAILED" "ERROR" "TIMEDOUT" "INTERNAL_ERROR")) 'failed)
                     ((member effective '("CANCELED" "SKIPPED" "CANCELLED")) 'canceled)
                     (t 'idle))))
    (claude-dashboard-icon icon-name)))

(defun databricks-runs--panel-entries ()
  "Return entries from the cached JSON data.
When collapsed, show top 3 runs.  When expanded, show up to 30."
  (if (null databricks-runs--cache)
      nil
    (let* ((all-runs (append databricks-runs--cache nil))
           (limit (if databricks-runs--expanded 30 3))
           (runs (cl-subseq all-runs 0 (min limit (length all-runs)))))
      (mapcar
       (lambda (run)
         (let-alist run
           (list .run_id
                 (vector .run_name
                         (databricks-runs--status-cell .state .result_state)
                         (databricks-runs--format-duration .start_time .end_time)
                         (databricks-runs--format-start-time .start_time)
                         .run_id))))
       runs))))

(defun databricks-runs--toggle-expanded (_panel _row-id)
  "Toggle between showing 3 and 30 Databricks runs."
  (setq databricks-runs--expanded (not databricks-runs--expanded))
  (message "Databricks: showing %s" (if databricks-runs--expanded "top 30" "top 3"))
  (claude-dashboard-refresh-all))

(defun databricks-runs-panel ()
  "Return a dashboard panel plist for Databricks runs.
For use with `claude-dashboard-create'."
  (list
   :name "databricks"
   :title "Databricks Runs"
   :columns [("Job" 24 nil) ("S" 3 nil) ("Duration" 12 nil)
             ("Started" 18 nil) ("Run ID" 16 nil)]
   :entries #'databricks-runs--panel-entries
   :refresh #'databricks-runs--fetch-json
   :actions `(("RET" . ,(lambda (_panel row-id)
                          (if-let ((run (cl-find row-id
                                                  (append databricks-runs--cache nil)
                                                  :key (lambda (r) (alist-get 'run_id r))
                                                  :test #'equal)))
                              (let ((url (alist-get 'run_page_url run)))
                                (if (and url (not (string-empty-p url)))
                                    (browse-url url)
                                  (message "No URL available for run %s" row-id)))
                            (message "Run %s not found — try refreshing with 'g'" row-id))))
              ("C-o" . ,#'databricks-runs--toggle-expanded))
   :interval 30
   :context nil))

(provide 'databricks-runs)
;;; databricks-runs.el ends here
