;;; metaflow-runs.el --- Metaflow job monitoring panel -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Discover Argo workflows and supplement them with local Spark child monitors.
;; Queries run asynchronously and never start jobs.  A failed launcher
;; and a running Spark child remain
;; separate states.  Missing/stale monitoring never means a job succeeded.
;; Usage: M-x list-metaflow-runs, or the agents-workflow Databricks panel.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'button)
(require 'url-util)
(require 'claude-dashboard)
(require 'databricks-runs)

(defgroup metaflow-runs nil
  "Monitor Metaflow flows and their Spark children."
  :group 'tools
  :prefix "metaflow-runs-")

(defcustom metaflow-runs-monitor-directories
  (delete-dups (list (expand-file-name "~/.local/state/metaflow/jobs")
                     (expand-file-name "pin-job-monitors" temporary-file-directory)
                     "/tmp/pin-job-monitors"))
  "Directories containing Metaflow launch monitor output folders.
Local launches contain flow.log; Argo launches contain workflow.json.
Both may contain monitor.json and eventual result files.
Child folders named sjob_ID contain status.json, pid and result.json.
Rearmed child folders may append a suffix to the same sjob_ID.
These are local observations, not a workspace-wide Metaflow service query."
  :type '(repeat directory)
  :group 'metaflow-runs)

(defcustom metaflow-runs-refresh-interval 30
  "Seconds between reads of the local monitor records."
  :type 'integer
  :group 'metaflow-runs)

(defcustom metaflow-runs-stale-seconds 120
  "Age in seconds after which a parent or child observation is stale."
  :type 'integer
  :group 'metaflow-runs)

(defcustom metaflow-runs-argo-context nil
  "Kubectl context used to discover Metaflow runs directly from Argo.
Nil disables remote discovery.  Local monitor records remain available."
  :type '(choice (const nil) string)
  :group 'metaflow-runs)

(defcustom metaflow-runs-argo-namespace "default"
  "Namespace queried for Argo workflows, regardless of launcher or owner."
  :type 'string
  :group 'metaflow-runs)

(defcustom metaflow-runs-argo-ui-base "https://argo.example"
  "Base URL for the Argo workflow UI."
  :type 'string
  :group 'metaflow-runs)

(defvar metaflow-runs--argo-records nil "Last successful Argo observations.")
(defvar metaflow-runs--argo-error nil "Latest discovery error, if any.")
(defvar metaflow-runs--argo-process nil "Shared asynchronous discovery process.")
(defvar metaflow-runs--argo-last-attempt 0 "Epoch of the last discovery attempt.")
(defvar metaflow-runs--argo-target nil "Context and namespace of cached records.")

(defun metaflow-runs--argo-record (workflow)
  "Normalize one remote WORKFLOW without assuming Spark child completion."
  (let* ((metadata (alist-get 'metadata workflow))
         (labels (alist-get 'labels metadata))
         (status (alist-get 'status workflow))
         (name (alist-get 'name metadata))
         (namespace (alist-get 'namespace metadata))
         (flow (alist-get 'metaflow/flow-name labels))
         (phase (upcase (or (alist-get 'phase status) "Pending"))))
    (when flow
      `((id . ,(format "argo:%s/%s" namespace name))
        (flow . ,flow) (flow_name . ,flow) (namespace . ,namespace)
        (run_id . ,(or (alist-get 'metaflow/run-id labels) (concat "argo-" name)))
        (parent_state . ,phase) (parent_source . "Live Argo API")
        (parent_stale . nil) (launcher_state . "UNKNOWN")
        (spark_state . "UNKNOWN") (stale . t) (monitor_state . "LIVE")
        (argo_checked_at . ,(float-time))
        (argo_url . ,(format "%s/workflows/%s/%s"
                             (string-remove-suffix "/" metaflow-runs-argo-ui-base)
                             (url-hexify-string namespace) (url-hexify-string name)))
        (start_time . ,(* 1000 (or (metaflow-runs--epoch
                                   (alist-get 'startedAt status))
                                  (metaflow-runs--epoch
                                   (alist-get 'creationTimestamp metadata)) 0)))
        (end_time . ,(* 1000 (or (metaflow-runs--epoch
                                 (alist-get 'finishedAt status)) 0)))))))

(defun metaflow-runs--argo-fetch ()
  "Start one bounded, asynchronous Argo query shared by all dashboards."
  (when (and metaflow-runs-argo-context
             (not (process-live-p metaflow-runs--argo-process))
             (>= (- (float-time) metaflow-runs--argo-last-attempt)
                 metaflow-runs-refresh-interval))
    (setq metaflow-runs--argo-last-attempt (float-time))
    (let* ((target (list metaflow-runs-argo-context metaflow-runs-argo-namespace))
           (output (generate-new-buffer " *metaflow-argo-output*"))
           (errors (generate-new-buffer " *metaflow-argo-errors*")))
      (unless (equal target metaflow-runs--argo-target)
        (setq metaflow-runs--argo-target target
              metaflow-runs--argo-records nil
              metaflow-runs--argo-error nil))
      (condition-case err
          (let ((proc
                 (make-process
                  :name "metaflow-argo" :buffer output :stderr errors
                  :noquery t :connection-type 'pipe
                  :command (list "kubectl" "--context" (car target)
                                 "--request-timeout=15s" "-n" (cadr target)
                                 "get" "workflows" "-o" "json")
                  :sentinel
                  (lambda (process _event)
                    (when (memq (process-status process) '(exit signal))
                      (when-let ((timer (process-get process 'timeout)))
                        (cancel-timer timer))
                      (unwind-protect
                          (condition-case failure
                              (progn
                                (unless (= (process-exit-status process) 0)
                                  (error "%s"
                                         (with-current-buffer errors
                                           (buffer-substring-no-properties
                                            (point-min) (min (point-max) 2001)))))
                                (let* ((json-object-type 'alist)
                                       (json-array-type 'list)
                                       (json-key-type 'symbol)
                                       (data (with-current-buffer output
                                               (when (> (buffer-size) 20971520)
                                                 (error "Argo response exceeds 20 MiB"))
                                               (goto-char (point-min))
                                               (json-read))))
                                  (unless (assq 'items data)
                                    (error "Argo response has no workflow list"))
                                  (setq metaflow-runs--argo-records
                                        (delq nil (mapcar #'metaflow-runs--argo-record
                                                         (alist-get 'items data)))
                                        metaflow-runs--argo-error nil)))
                            (error
                             (setq metaflow-runs--argo-error
                                   (error-message-string failure))))
                        (kill-buffer output)
                        (kill-buffer errors)
                        (process-put process 'finished t)))))))
            (setq metaflow-runs--argo-process proc)
            (process-put proc 'timeout
                         (run-at-time
                          25 nil (lambda ()
                                   (when (process-live-p proc)
                                     (with-current-buffer errors
                                       (insert "Argo query exceeded 25-second deadline"))
                                     (delete-process proc))))))
        (error
         (setq metaflow-runs--argo-error (error-message-string err))
         (kill-buffer output)
         (kill-buffer errors))))))

(defun metaflow-runs--merge-argo (records)
  "Overlay remote parent observations on local RECORDS, preserving children."
  (dolist (remote metaflow-runs--argo-records records)
    (let* ((row (copy-tree remote))
           (stale (or metaflow-runs--argo-error
                      (> (- (float-time) (alist-get 'argo_checked_at row))
                         metaflow-runs-stale-seconds)))
           (matches (seq-filter
                     (lambda (local)
                       (and (equal (alist-get 'namespace local)
                                   (alist-get 'namespace row))
                            (equal (alist-get 'flow local) (alist-get 'flow row))
                            (equal (alist-get 'run_id local) (alist-get 'run_id row))))
                     records)))
      (setf (alist-get 'parent_stale row) (and stale t))
      (when stale (setf (alist-get 'monitor_state row) "ERROR"))
      (if matches
          (dolist (local matches)
            (dolist (key '(parent_state parent_source parent_stale argo_url argo_checked_at))
              (if-let ((entry (assq key local)))
                  (setcdr entry (alist-get key row))
                (nconc local (list (cons key (alist-get key row)))))))
        (push row records)))))

(defcustom metaflow-runs-ui-url-template nil
  "Metaflow UI URL template for the flow-page shortcut.
Use %f for the URL-encoded flow name and optionally %r for its run ID.
For example: https://metaflow.example/?flow_id=%f.
Configure this separately from the Spark child's Databricks URL."
  :type '(choice (const nil) string)
  :group 'metaflow-runs)

(defvar-local metaflow-runs--cache nil
  "Normalized launch and child records in this dashboard.")

(defvar-local metaflow-runs--expanded nil
  "Whether to show up to thirty flows instead of three.")

(defun metaflow-runs--epoch (value)
  "Convert a timestamp VALUE to epoch seconds, or return nil."
  (cond ((numberp value) value)
        ((stringp value) (ignore-errors (float-time (date-to-time value))))))

(defun metaflow-runs--mtime (file)
  "Return FILE's modification time as epoch seconds, or zero."
  (if-let ((attrs (file-attributes file)))
      (float-time (file-attribute-modification-time attrs))
    0))

(defun metaflow-runs--read-text (file &optional limit)
  "Read at most LIMIT bytes from FILE, or return nil if absent.
LIMIT defaults to 65536, bounding log reads on every dashboard refresh."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 (or limit 65536))
      (buffer-string))))

(defun metaflow-runs--json (file)
  "Read FILE as an alist, or nil if absent.
Signal on malformed or oversized records so they cannot imply success."
  (when (file-exists-p file)
    (when (> (file-attribute-size (file-attributes file)) 1048576)
      (error "Monitor record exceeds 1 MiB: %s" file))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          (json-false :false))
      (json-read-from-string (metaflow-runs--read-text file 1048576)))))

(defun metaflow-runs--alive-p (pid &optional directory)
  "Return non-nil when local PID is a live watcher for DIRECTORY.
Check its command to avoid treating a recycled PID as a working monitor."
  (when (and (integerp pid) (> pid 0))
    (let* ((attrs (process-attributes pid))
           (args (alist-get 'args attrs)))
      (and attrs
           (or (null directory)
               (and (stringp args)
                    (string-match-p "\\(?:launch_monitored\\|monitor_job\\)\\.py" args)
                    (string-match-p (regexp-quote directory) args)))))))

(defun metaflow-runs--capture (regexp text)
  "Return the first captured group of REGEXP in TEXT, or nil."
  (when (and text (string-match regexp text))
    (match-string 1 text)))

(defun metaflow-runs--launcher-state (result monitor directory)
  "Derive launcher state from RESULT and MONITOR for DIRECTORY.
A monitor timeout is UNKNOWN, not a remote job failure."
  (let ((code (alist-get 'flow_exit_code result)))
    (cond
     ((equal (alist-get 'monitor_status result) "deadline_exceeded") "UNKNOWN")
     ((and (numberp code) (= code 0)
           (eq (alist-get 'success result) t)) "SUCCESS")
     ((and (numberp code) (/= code 0)) "FAILED")
     (result "UNKNOWN")
     ((metaflow-runs--alive-p (alist-get 'pid monitor) directory) "RUNNING")
     (t "UNKNOWN"))))

(defun metaflow-runs--child-directories (directory)
  "Return the newest watcher directory per Spark job under DIRECTORY.
Keep separate Spark children; collapse rearmed watchers for the same ID."
  (let ((latest (make-hash-table :test #'equal)))
    (dolist (dir (directory-files directory t "\\`sjob_[0-9]+" t))
      (when (file-directory-p dir)
        (let* ((id (metaflow-runs--capture
                    "\\`\\(sjob_[0-9]+\\)\\(?:-.*\\)?\\'"
                    (file-name-nondirectory dir)))
               (stamp (apply #'max
                             (mapcar (lambda (name)
                                       (metaflow-runs--mtime
                                        (expand-file-name name dir)))
                                     '("monitor.json" "pid" "status.json"
                                       "result.json"))))
               (old (gethash id latest)))
          (when (and id (or (null old) (> stamp (car old))))
            (puthash id (cons stamp dir) latest)))))
    (mapcar #'cdr (hash-table-values latest))))

(defun metaflow-runs--monitor-state (directory result status now)
  "Describe watcher health in DIRECTORY using RESULT, STATUS and NOW."
  (let* ((outcome (alist-get 'monitor_status result))
         (stamp (metaflow-runs--mtime
                 (expand-file-name "status.json" directory)))
         (pid-text (metaflow-runs--read-text
                    (expand-file-name "pid" directory) 32))
         (pid (and pid-text (string-to-number pid-text))))
    (cond
     ((equal outcome "terminal") "DONE")
     ((equal outcome "monitor_failed") "ERROR")
     ((equal outcome "deadline_exceeded") "TIMEOUT")
     ((not (metaflow-runs--alive-p pid directory)) "STOPPED")
     ((null status) "STARTING")
     ((> (- now stamp) metaflow-runs-stale-seconds) "STALE")
     (t "LIVE"))))

(defun metaflow-runs--read-launch (directory)
  "Return one record per Spark child for launch DIRECTORY."
  (let* ((log (metaflow-runs--read-text (expand-file-name "flow.log" directory)))
         (monitor (metaflow-runs--json (expand-file-name "monitor.json" directory)))
         (result (metaflow-runs--json (expand-file-name "flow-result.json" directory)))
         (workflow (metaflow-runs--json (expand-file-name "workflow.json" directory)))
         (metadata (alist-get 'metadata workflow))
         (labels (alist-get 'labels metadata))
         (parent (metaflow-runs--json (expand-file-name "parent-status.json" directory)))
         (parent-result (metaflow-runs--json (expand-file-name "result.json" directory)))
         (phase (or (alist-get 'phase parent-result) (alist-get 'phase parent)
                    (alist-get 'phase (alist-get 'status workflow))))
         (flow-name (or (alist-get 'metaflow/flow-name labels)
                        (metaflow-runs--capture
                         "Monitoring \\([[:alnum:]_]+\\)/" log)
                        (metaflow-runs--capture
                         "executing \\([[:alnum:]_]+\\) for user:" log)))
         (flow (or flow-name (file-name-nondirectory directory)))
         (run-id (or (alist-get 'metaflow/run-id labels)
                     (metaflow-runs--capture "Monitoring [^/\n]+/\\([^ \n]+\\)" log)
                     (metaflow-runs--capture "run-id \\([^): ]+\\)" log) "—"))
         (task (metaflow-runs--capture
                (concat "\\[" (regexp-quote run-id)
                        "/\\(\\(?:[^/]+\\)/[0-9]+\\) (pid") log))
         (now (float-time))
         (start (* 1000 (or (metaflow-runs--epoch
                            (alist-get 'startedAt (alist-get 'status workflow)))
                           (metaflow-runs--epoch (alist-get 'creationTimestamp metadata))
                           (metaflow-runs--mtime
                            (expand-file-name "monitor.json" directory)))))
         (launcher-state (metaflow-runs--launcher-state result monitor directory))
         (parent-state (if phase (upcase phase) launcher-state))
         (parent-stale (and phase
                            (not (member parent-state '("SUCCEEDED" "FAILED" "ERROR")))
                            (> (- now (or (metaflow-runs--epoch
                                           (alist-get 'checked_at parent))
                                          (metaflow-runs--mtime
                                           (expand-file-name "workflow.json" directory))))
                               metaflow-runs-stale-seconds)))
         (parent-end (or (metaflow-runs--epoch
                         (alist-get 'finishedAt (alist-get 'status workflow)))
                        (metaflow-runs--epoch (alist-get 'finished_at parent-result))))
         (children (metaflow-runs--child-directories directory)))
    ;; Prefer the Spark step over the initial start step in the bounded log.
    (when-let ((spark-task (metaflow-runs--capture
                            (concat "/" (regexp-quote run-id)
                                    "/\\([^/\n' ]+\\)/\\([0-9]+\\)/mfjob.py") log)))
      (setq task (concat spark-task "/" (match-string 2 log))))
    (mapcar
     (lambda (child)
       (let* ((status (and child (metaflow-runs--json
                                  (expand-file-name "status.json" child))))
              (child-result (and child (metaflow-runs--json
                                        (expand-file-name "result.json" child))))
              (health (if child
                          (metaflow-runs--monitor-state child child-result status now)
                        (if (equal launcher-state "RUNNING") "WAITING" "NONE")))
              (raw-state (or (alist-get 'job_status child-result)
                             (alist-get 'status status) "UNKNOWN"))
              (spark (string-remove-prefix "JOB_STATUS_" raw-state))
              (job-id (or (alist-get 'jobId status)
                          (alist-get 'job_id child-result)
                          (and child (metaflow-runs--capture
                                      "\\(sjob_[0-9]+\\)"
                                      (file-name-nondirectory child)))))
              (terminal (member spark '("COMPLETED" "FAILED" "CANCELLED")))
              (child-end (or (metaflow-runs--epoch (alist-get 'endTime status))
                             (metaflow-runs--epoch
                              (alist-get 'finished_at child-result))))
              (end (if (and (or (null child) terminal)
                            (member parent-state '("SUCCESS" "SUCCEEDED" "FAILED" "ERROR")))
                       (* 1000 (max (or parent-end
                                        (metaflow-runs--mtime
                                         (expand-file-name "flow-result.json" directory)))
                                    (if child
                                        (or child-end
                                            (metaflow-runs--mtime
                                             (expand-file-name "result.json" child)))
                                      0)))
                     0)))
         `((id . ,(concat directory "::" (or job-id "flow")))
           (flow . ,flow) (flow_name . ,flow-name)
           (namespace . ,(alist-get 'namespace metadata))
           (run_id . ,run-id) (task . ,task)
           (parent_state . ,parent-state) (parent_stale . ,parent-stale)
           (parent_source . ,(if phase "Argo workflow" "Local launcher"))
           (launcher_state . ,launcher-state) (spark_state . ,spark)
           (monitor_state . ,health)
           (stale . ,(and (not terminal) (not (equal health "LIVE"))))
           (start_time . ,start) (end_time . ,end)
           (updated . ,(and child (metaflow-runs--mtime
                                   (expand-file-name "status.json" child))))
           (job_id . ,job-id)
           (databricks_run_id . ,(or (alist-get 'cloudProviderJobId status)
                                     (alist-get 'run_id child-result)))
           (url . ,(or (alist-get 'cloudProviderJobUrl status)
                       (alist-get 'url child-result)))
           (directory . ,directory) (child_directory . ,child)
           (error . ,(alist-get 'error child-result)))))
     (or children '(nil)))))

(defun metaflow-runs--refresh ()
  "Refresh local records and start asynchronous Argo discovery when configured."
  (metaflow-runs--argo-fetch)
  (let ((seen (make-hash-table :test #'equal)) records)
    (dolist (root metaflow-runs-monitor-directories)
      (when (file-directory-p root)
        (dolist (dir (directory-files root t directory-files-no-dot-files-regexp t))
          (when (and (file-directory-p dir)
                     (or (file-exists-p (expand-file-name "flow.log" dir))
                         (file-exists-p (expand-file-name "workflow.json" dir)))
                     (not (gethash (file-truename dir) seen)))
            (puthash (file-truename dir) t seen)
            (condition-case err
                (setq records (append (metaflow-runs--read-launch dir) records))
              (error
               (push `((id . ,dir) (flow . ,(file-name-nondirectory dir))
                       (run_id . "—") (launcher_state . "UNKNOWN")
                       (spark_state . "UNKNOWN") (monitor_state . "READ ERROR")
                       (start_time . 0) (end_time . 0) (directory . ,dir)
                       (error . ,(error-message-string err))) records)))))))
    (setq metaflow-runs--cache
          (sort (if metaflow-runs-argo-context
                    (metaflow-runs--merge-argo records)
                  records)
                (lambda (a b)
                  (> (alist-get 'start_time a) (alist-get 'start_time b)))))))

(defun metaflow-runs--status-text (state &optional stale)
  "Return full STATE text, marking STALE observations as unverified."
  (concat (or state "UNKNOWN") (if stale " (unverified)" "")))

(defun metaflow-runs--status-cell (state &optional stale)
  "Render STATE with Databricks status symbols, fonts and colors.
STALE observations use the unknown-state symbol.  Hover text retains the
original status and indicates when it is unverified."
  (let ((mapped (cond
                 (stale "UNKNOWN")
                 ((member state '("SUCCESS" "SUCCEEDED" "COMPLETED" "DONE")) "SUCCESS")
                 ((member state '("RUNNING" "LIVE")) "RUNNING")
                 ((member state '("PENDING" "STARTING" "WAITING")) "PENDING")
                 ((member state '("FAILED" "ERROR" "READ ERROR")) "FAILED")
                 ((equal state "TIMEOUT") "TIMEDOUT")
                 ((member state '("CANCELED" "CANCELLED")) "CANCELED")
                 (t "UNKNOWN"))))
    (propertize (databricks-runs--status-cell mapped "")
                'help-echo (metaflow-runs--status-text state stale))))

(defun metaflow-runs--flow-key (run)
  "Return RUN's display group without inventing namespace metadata.
Legacy records join a sole known namespace's history for the same flow.
Multiple known namespaces remain separate, with unknown records unassigned."
  (let* ((name (alist-get 'flow_name run))
         (namespace (alist-get 'namespace run))
         (known (and name (null namespace)
                     (delete-dups
                      (delq nil
                            (mapcar (lambda (row)
                                      (when (equal name (alist-get 'flow_name row))
                                        (alist-get 'namespace row)))
                                    metaflow-runs--cache))))))
    (list (or name (alist-get 'directory run))
          (or namespace (and (= (length known) 1) (car known))))))

(defun metaflow-runs--flows ()
  "Return one summary per flow, using its newest run and all that run's children."
  (let ((seen (make-hash-table :test #'equal)) summaries)
    (dolist (record metaflow-runs--cache)
      (let ((key (metaflow-runs--flow-key record)))
        (unless (gethash key seen)
          (puthash key t seen)
          (let* ((history (cl-remove-if-not
                           (lambda (r) (equal (metaflow-runs--flow-key r) key))
                           metaflow-runs--cache))
                 (children (cl-remove-if-not
                            (lambda (r)
                              (and (equal (alist-get 'run_id r)
                                          (alist-get 'run_id record))
                                   (equal (alist-get 'namespace r)
                                          (alist-get 'namespace record))))
                            history))
                 (summary (copy-alist record)))
            (dolist (field '(spark_state monitor_state))
              (let ((states (delete-dups (mapcar (lambda (r) (alist-get field r))
                                                 children))))
                (setf (alist-get field summary)
                      (if (cdr states) "MIXED" (car states)))))
            (setf (alist-get 'stale summary)
                  (cl-some (lambda (r) (alist-get 'stale r)) children))
            (setf (alist-get 'history summary) history)
            (push summary summaries)))))
    (nreverse summaries)))

(defun metaflow-runs--entries ()
  "Return the cached Metaflow rows for the dashboard."
  (append
   (when (and metaflow-runs-argo-context metaflow-runs--argo-error)
     (list (list "metaflow:argo-error"
                 (vector (propertize "Argo discovery unavailable"
                                     'help-echo metaflow-runs--argo-error)
                         "?" "?" "" "ERROR" ""))))
   (if (null metaflow-runs--cache)
      (list (list "metaflow:empty"
                  ["No monitored launches" "" "" "" "" ""]))
    (mapcar
     (lambda (run)
       (list (alist-get 'id run)
             (vector
              (propertize (alist-get 'flow run)
                          'help-echo (format "%s/%s — f: flow UI, d: details, l: log"
                                             (alist-get 'flow run)
                                             (alist-get 'run_id run)))
              (metaflow-runs--status-cell (or (alist-get 'parent_state run)
                                             (alist-get 'launcher_state run))
                                         (alist-get 'parent_stale run))
              (metaflow-runs--status-cell (alist-get 'spark_state run)
                                          (alist-get 'stale run))
              (databricks-runs--format-duration
               (alist-get 'start_time run) (alist-get 'end_time run))
              (metaflow-runs--status-cell (alist-get 'monitor_state run))
              (alist-get 'run_id run))))
     (seq-take (metaflow-runs--flows) (if metaflow-runs--expanded 30 3))))))

(defun metaflow-runs--find (row-id)
  "Find ROW-ID in this dashboard's cache, or signal a user error."
  (or (cl-find row-id (metaflow-runs--flows)
               :key (lambda (run) (alist-get 'id run)) :test #'equal)
      (cl-find row-id metaflow-runs--cache
               :key (lambda (run) (alist-get 'id run)) :test #'equal)
      (user-error "No Metaflow launch on this row")))

(defun metaflow-runs--details (_panel row-id)
  "Show the states, identifiers and local evidence for ROW-ID."
  (let ((run (metaflow-runs--find row-id)))
    (with-current-buffer (get-buffer-create "*Metaflow Run Details*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s/%s\n\n" (alist-get 'flow run) (alist-get 'run_id run)))
        (dolist (key '(parent_state parent_source argo_url launcher_state spark_state monitor_state task job_id
                                      databricks_run_id url error))
          (insert (format "%s: %s\n" key (or (alist-get key run) "—"))))
        (when-let ((updated (alist-get 'updated run)))
          (insert (format "Last Spark observation: %s\n"
                          (format-time-string "%F %T %Z" (seconds-to-time updated)))))
        (when (and metaflow-runs-argo-context metaflow-runs--argo-error)
          (insert (format "Argo discovery error: %s\n" metaflow-runs--argo-error)))
        (insert "\nParent comes from Argo when available, otherwise local records.\n"
                "Launcher is the local Metaflow client's result.\n"
                "A launcher failure does not establish that the remote flow failed.\n"
                "Use f in the dashboard to check the remote Metaflow UI.\n"
                "Spark state is the child's last recorded observation.\n"
                "Unverified states use the unknown icon; hover for the last observation.\n"
                "Monitoring loss does not stop a job.\n\n")
        (when-let ((history (alist-get 'history run)))
          (insert "Run history (newest first; one line per Spark child):\n")
          (dolist (previous history)
            (insert (format "%s  Parent: %s  Spark: %s  Monitor: %s\n"
                            (alist-get 'run_id previous)
                            (metaflow-runs--status-text
                             (or (alist-get 'parent_state previous)
                                 (alist-get 'launcher_state previous))
                             (alist-get 'parent_stale previous))
                            (metaflow-runs--status-text
                             (alist-get 'spark_state previous) (alist-get 'stale previous))
                            (alist-get 'monitor_state previous)))
            (insert (format "Namespace: %s\n"
                            (or (alist-get 'namespace previous) "not recorded")))
            (when-let ((directory (alist-get 'directory previous)))
              (insert-text-button directory 'follow-link t
                                  'action (lambda (_button) (dired directory)))
              (insert "\n")))
          (insert "\n"))
        (dolist (dir (delq nil (list (alist-get 'directory run)
                                     (alist-get 'child_directory run))))
          (dolist (name '("flow.log" "flow-result.json" "workflow.json"
                          "parent-status.json" "monitor.json"
                          "status.json" "result.json" "monitor-error.txt"
                          "diagnosis.json" "databricks-diagnosis.json"))
            (let ((file (expand-file-name name dir)))
              (when (file-exists-p file)
                (insert-text-button file 'follow-link t
                                    'action (lambda (_button) (find-file file)))
                (insert "\n")))))
        (goto-char (point-min)))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

(defun metaflow-runs--open (_panel row-id)
  "Open ROW-ID's Databricks child, falling back to local details."
  (let ((url (alist-get 'url (metaflow-runs--find row-id))))
    (if (and (stringp url) (string-match-p "\\`https?://" url))
        (browse-url url)
      (metaflow-runs--details nil row-id))))

(defun metaflow-runs--flow-url (run)
  "Build RUN's Metaflow UI URL from the configured template."
  (or (alist-get 'argo_url run)
      (let ((template metaflow-runs-ui-url-template)
        (flow (alist-get 'flow_name run))
        (run-id (alist-get 'run_id run)))
    (unless (and (stringp template)
                 (string-match-p "\\`https?://" template)
                 (string-match-p "%f" template))
      (user-error "Set metaflow-runs-ui-url-template to a UI URL containing %%f"))
    (unless (and (stringp flow) (not (string-empty-p flow)))
      (user-error "No recorded Metaflow flow name for this launch"))
    (when (and (string-match-p "%r" template)
               (or (not (stringp run-id)) (member run-id '("" "—"))))
      (user-error "No recorded Metaflow run ID for this launch"))
    (replace-regexp-in-string
     "%[fr]" (lambda (token)
               (url-hexify-string (if (equal token "%f") flow run-id)))
     template t t))))

(defun metaflow-runs--open-flow (_panel row-id)
  "Open ROW-ID's Metaflow flow page in the browser."
  (browse-url (metaflow-runs--flow-url (metaflow-runs--find row-id))))

(defun metaflow-runs--log (_panel row-id)
  "Open the local flow log associated with ROW-ID."
  (let* ((run (metaflow-runs--find row-id))
         (directory (alist-get 'directory run))
         (file (and directory (expand-file-name "flow.log" directory))))
    (if (and file (file-exists-p file))
        (find-file file)
      (if (alist-get 'argo_url run)
          (browse-url (alist-get 'argo_url run))
        (user-error "No local flow log is available")))))

(defun metaflow-runs--toggle (_panel _row-id)
  "Toggle between showing three and thirty Metaflow flows."
  (setq metaflow-runs--expanded (not metaflow-runs--expanded))
  (claude-dashboard-refresh-all))

(defun metaflow-runs-panel ()
  "Return the Metaflow section with asynchronous Argo and local refresh."
  (list :name "metaflow" :title "Metaflow Jobs"
        :columns [("Flow" 22 nil) ("Parent" 6 nil) ("Spark" 5 nil)
                  ("Elapsed" 10 nil) ("Monitor" 7 nil) ("Run ID" 8 nil)]
        :entries #'metaflow-runs--entries :refresh #'metaflow-runs--refresh
        :interval metaflow-runs-refresh-interval
        :actions `(("RET" . ,#'metaflow-runs--open)
                   ("f" . ,#'metaflow-runs--open-flow)
                   ("d" . ,#'metaflow-runs--details)
                   ("l" . ,#'metaflow-runs--log)
                   ("C-o" . ,#'metaflow-runs--toggle))))

;;;###autoload
(defun list-metaflow-runs ()
  "Open a standalone Metaflow jobs dashboard."
  (interactive)
  (pop-to-buffer (claude-dashboard-create
                  :name "metaflow" :panels (list (metaflow-runs-panel)))))

(provide 'metaflow-runs)
;;; metaflow-runs.el ends here
