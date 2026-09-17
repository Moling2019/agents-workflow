;;; metaflow-runs-tests.el --- Metaflow monitoring tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'metaflow-runs)
(require 'agents-workflow)

(defun metaflow-runs-test--argo ()
  "Return an Argo workflow created without this laptop's launch records."
  '((metadata . ((name . "daily-scheduled") (namespace . "team-prod")
                 (labels . ((metaflow/flow-name . "ExampleDailyFlow")
                            (metaflow/run-id . "argo-daily-scheduled")))))
    (status . ((phase . "Succeeded") (startedAt . "2026-09-17T09:00:00Z")
               (finishedAt . "2026-09-17T10:00:00Z")))))

(ert-deftest metaflow-runs-test-remote-without-local-records ()
  "A scheduled remote flow appears without inventing Spark success."
  (let* ((metaflow-runs--argo-error nil)
         (metaflow-runs--argo-records
          (list (metaflow-runs--argo-record (metaflow-runs-test--argo))))
         (rows (metaflow-runs--merge-argo nil))
         (row (car rows)))
    (should (= (length rows) 1))
    (should (equal (alist-get 'parent_state row) "SUCCEEDED"))
    (should (equal (alist-get 'spark_state row) "UNKNOWN"))
    (should-not (alist-get 'directory row))
    (should (string-match-p "/workflows/team-prod/daily-scheduled"
                            (metaflow-runs--flow-url row)))))

(ert-deftest metaflow-runs-test-remote-merges-children-and-marks-auth-loss ()
  "Remote parent state updates each child, and auth loss preserves evidence."
  (let* ((metaflow-runs--argo-error nil)
         (metaflow-runs--argo-records
          (list (metaflow-runs--argo-record (metaflow-runs-test--argo))))
         (local '(((flow . "ExampleDailyFlow") (namespace . "team-prod")
                   (run_id . "argo-daily-scheduled") (job_id . "sjob_100")
                   (spark_state . "RUNNING") (parent_state . "RUNNING"))))
         (rows (metaflow-runs--merge-argo (copy-tree local))))
    (should (= (length rows) 1))
    (should (equal (alist-get 'parent_state (car rows)) "SUCCEEDED"))
    (should (equal (alist-get 'spark_state (car rows)) "RUNNING"))
    (setq metaflow-runs--argo-error "Authentication expired")
    (setq rows (metaflow-runs--merge-argo (copy-tree local)))
    (should (alist-get 'parent_stale (car rows)))
    (should (equal (alist-get 'parent_state (car rows)) "SUCCEEDED"))))

(ert-deftest metaflow-runs-test-async-argo-success-and-auth-error ()
  "Use a fake kubectl to verify asynchronous discovery and retained errors."
  (let* ((root (make-temp-file "argo-cli-test-" t))
         (program (expand-file-name "kubectl" root))
         (exec-path (cons root exec-path))
         (metaflow-runs-argo-context "test-context")
         (metaflow-runs-argo-namespace "team-prod")
         (metaflow-runs-refresh-interval 0)
         (metaflow-runs--argo-target nil)
         (metaflow-runs--argo-last-attempt 0)
         (metaflow-runs--argo-process nil)
         (metaflow-runs--argo-error nil)
         (metaflow-runs--argo-records nil))
    (unwind-protect
        (progn
          (with-temp-file program
            (insert "#!/bin/sh\nprintf '%s\\n' '"
                    (json-encode `((items . [,(metaflow-runs-test--argo)])))
                    "'\n"))
          (set-file-modes program #o700)
          (metaflow-runs--argo-fetch)
          (let ((deadline (+ (float-time) 5)))
            (while (and (not (process-get metaflow-runs--argo-process 'finished))
                        (< (float-time) deadline))
              (accept-process-output metaflow-runs--argo-process 0.05)))
          (should-not metaflow-runs--argo-error)
          (should (= (length metaflow-runs--argo-records) 1))
          (with-temp-file program
            (insert "#!/bin/sh\necho 'Authentication expired' >&2\nexit 1\n"))
          (metaflow-runs--argo-fetch)
          (let ((deadline (+ (float-time) 5)))
            (while (and (not (process-get metaflow-runs--argo-process 'finished))
                        (< (float-time) deadline))
              (accept-process-output metaflow-runs--argo-process 0.05)))
          (should (string-match-p "Authentication expired" metaflow-runs--argo-error))
          (should (= (length metaflow-runs--argo-records) 1)))
      (when (process-live-p metaflow-runs--argo-process)
        (delete-process metaflow-runs--argo-process))
      (delete-directory root t))))

(defun metaflow-runs-test--write (dir name value)
  "Write JSON VALUE to NAME in DIR and return the path."
  (let ((file (expand-file-name name dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert (json-encode value)))
    file))

(defun metaflow-runs-test--launch (root)
  "Create a realistic launch fixture beneath ROOT."
  (let ((dir (expand-file-name "pickup" root)))
    (make-directory dir)
    (with-temp-file (expand-file-name "flow.log" dir)
      (insert "Metaflow 2 executing ExampleFlow for user:test\n"
              "Workflow starting (run-id 123):\n"
              "[123/start/456 (pid 999)] Task is starting.\n"
              "s3://data/ExampleFlow/123/backfill/789/mfjob.py\n"))
    (metaflow-runs-test--write dir "monitor.json" '((pid . 999)))
    (metaflow-runs-test--write
     dir "sjob_100/status.json"
     '((status . "JOB_STATUS_RUNNING") (jobId . "sjob_100")
       (cloudProviderJobId . "700")
       (cloudProviderJobUrl . "https://prod.example/jobs/700")))
    (with-temp-file (expand-file-name "sjob_100/pid" dir) (insert "999"))
    dir))

(defmacro metaflow-runs-test--with-launch (&rest body)
  "Run BODY with an isolated launch DIR and a live mocked watcher."
  (declare (indent 0))
  `(let* ((root (make-temp-file "metaflow-test-" t))
          (dir (metaflow-runs-test--launch root))
          (metaflow-runs-monitor-directories (list root)))
     (unwind-protect
         (cl-letf (((symbol-function 'metaflow-runs--alive-p) (lambda (&rest _) t)))
           (with-temp-buffer ,@body))
       (delete-directory root t))))

(ert-deftest metaflow-runs-test-discover-argo-without-flow-log ()
  "Discover an Argo monitor without a local Metaflow launcher log."
  (metaflow-runs-test--with-launch
    (delete-file (expand-file-name "flow.log" dir))
    (metaflow-runs-test--write
     dir "workflow.json"
     '((metadata . ((namespace . "team-prod")
                    (labels . ((metaflow/flow-name . "ExampleDailyFlow")
                               (metaflow/run-id . "argo-daily-test")))))
       (status . ((phase . "Running")))))
    (metaflow-runs--refresh)
    (should (= (length metaflow-runs--cache) 1))
    (let ((run (car metaflow-runs--cache)))
      (should (equal (alist-get 'flow run) "ExampleDailyFlow"))
      (should (equal (alist-get 'run_id run) "argo-daily-test"))
      (should (equal (alist-get 'spark_state run) "RUNNING")))))

(ert-deftest metaflow-runs-test-failed-parent-running-child ()
  "A failed launcher must not obscure the still-running Spark child."
  (metaflow-runs-test--with-launch
    (metaflow-runs-test--write dir "flow-result.json"
                               '((flow_exit_code . 1) (success . :json-false)))
    (metaflow-runs--refresh)
    (let ((run (car metaflow-runs--cache)))
      (should (equal (alist-get 'flow run) "ExampleFlow"))
      (should (equal (alist-get 'run_id run) "123"))
      (should (equal (alist-get 'task run) "backfill/789"))
      (should (equal (alist-get 'launcher_state run) "FAILED"))
      (should (equal (alist-get 'spark_state run) "RUNNING"))
      (should (equal (alist-get 'monitor_state run) "LIVE"))
      (should (= (alist-get 'end_time run) 0)))))

(ert-deftest metaflow-runs-test-success-requires-both-results ()
  "Completed Spark and a completed flow have separate stable results."
  (metaflow-runs-test--with-launch
    (metaflow-runs-test--write dir "flow-result.json"
                               '((flow_exit_code . 0) (success . t)))
    (metaflow-runs-test--write
     dir "sjob_100/result.json"
     '((monitor_status . "terminal") (job_status . "JOB_STATUS_COMPLETED")
       (success . t) (job_id . "sjob_100")))
    (metaflow-runs--refresh)
    (let ((run (car metaflow-runs--cache)))
      (should (equal (alist-get 'launcher_state run) "SUCCESS"))
      (should (equal (alist-get 'spark_state run) "COMPLETED"))
      (should (equal (alist-get 'monitor_state run) "DONE"))
      (should-not (alist-get 'stale run))
      (should (> (alist-get 'end_time run) 0)))))

(ert-deftest metaflow-runs-test-monitor-error-is-not-job-failure ()
  "A failed watcher retains the last state but marks it unverified."
  (metaflow-runs-test--with-launch
    (metaflow-runs-test--write
     dir "sjob_100/result.json"
     '((monitor_status . "monitor_failed") (error . "sjem missing")))
    (metaflow-runs--refresh)
    (let ((run (car metaflow-runs--cache)))
      (should (equal (alist-get 'spark_state run) "RUNNING"))
      (should (equal (alist-get 'monitor_state run) "ERROR"))
      (should (alist-get 'stale run))
      (should (equal (get-text-property 0 'help-echo (aref (cadar (metaflow-runs--entries)) 2)) "RUNNING (unverified)")))))

(ert-deftest metaflow-runs-test-stale-and-stopped-watchers ()
  "Old snapshots and dead watchers cannot look live."
  (metaflow-runs-test--with-launch
    (set-file-times (expand-file-name "sjob_100/status.json" dir)
                    (time-subtract nil 300))
    (metaflow-runs--refresh)
    (should (equal (alist-get 'monitor_state (car metaflow-runs--cache)) "STALE"))
    (cl-letf (((symbol-function 'metaflow-runs--alive-p) (lambda (&rest _) nil)))
      (metaflow-runs--refresh)
      (should (equal (alist-get 'monitor_state (car metaflow-runs--cache)) "STOPPED"))
      (should (equal (alist-get 'launcher_state (car metaflow-runs--cache)) "UNKNOWN")))))

(ert-deftest metaflow-runs-test-parent-timeout-is-unknown ()
  "A local monitor deadline is not a failed or cancelled remote flow."
  (metaflow-runs-test--with-launch
    (metaflow-runs-test--write dir "flow-result.json"
                               '((monitor_status . "deadline_exceeded")
                                 (success . :json-false)))
    (metaflow-runs--refresh)
    (should (equal (alist-get 'launcher_state (car metaflow-runs--cache)) "UNKNOWN"))))

(ert-deftest metaflow-runs-test-rearmed-watchers-and-multiple-children ()
  "Rearmed watchers replace old evidence without hiding other Spark children."
  (metaflow-runs-test--with-launch
    (metaflow-runs-test--write dir "sjob_100/result.json"
                               '((monitor_status . "monitor_failed")))
    (set-file-times (expand-file-name "sjob_100/result.json" dir)
                    (time-subtract nil 300))
    (set-file-times (expand-file-name "sjob_100/status.json" dir)
                    (time-subtract nil 300))
    (set-file-times (expand-file-name "sjob_100/pid" dir)
                    (time-subtract nil 300))
    (metaflow-runs-test--write
     dir "sjob_100-rearmed/status.json"
     '((jobId . "sjob_100") (status . "JOB_STATUS_RUNNING")))
    (metaflow-runs-test--write
     dir "sjob_200/status.json"
     '((jobId . "sjob_200") (status . "JOB_STATUS_COMPLETED")))
    (metaflow-runs--refresh)
    (should (= (length metaflow-runs--cache) 2))
    (should (= (length (metaflow-runs--entries)) 1))
    (should (equal (get-text-property 0 'help-echo (aref (cadar (metaflow-runs--entries)) 2)) "MIXED"))
    (let ((run (cl-find "sjob_100" metaflow-runs--cache
                        :key (lambda (x) (alist-get 'job_id x)) :test #'equal)))
      (should (string-suffix-p "sjob_100-rearmed" (alist-get 'child_directory run)))
      (should (equal (alist-get 'monitor_state run) "LIVE")))))

(ert-deftest metaflow-runs-test-malformed-record-is-visible ()
  "Partial JSON writes display an error instead of empty or successful jobs."
  (metaflow-runs-test--with-launch
    (with-temp-file (expand-file-name "sjob_100/status.json" dir)
      (insert "{\"status\":"))
    (metaflow-runs--refresh)
    (should (equal (alist-get 'monitor_state (car metaflow-runs--cache)) "READ ERROR"))
    (should (alist-get 'error (car metaflow-runs--cache)))))

(ert-deftest metaflow-runs-test-bounded-log-and-root-dedup ()
  "Large logs are bounded and repeated monitor roots do not duplicate runs."
  (metaflow-runs-test--with-launch
    (let ((metaflow-runs-monitor-directories (list root root)))
      (metaflow-runs--refresh)
      (should (= (length metaflow-runs--cache) 1)))
    (with-temp-file (expand-file-name "large.log" dir)
      (insert (make-string 100000 ?x)))
    (should (= (length (metaflow-runs--read-text
                        (expand-file-name "large.log" dir))) 65536))))

(ert-deftest metaflow-runs-test-open-child-url ()
  "Opening a row uses its recorded workspace URL."
  (metaflow-runs-test--with-launch
    (metaflow-runs--refresh)
    (let (opened)
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (metaflow-runs--open nil (alist-get 'id (car metaflow-runs--cache))))
      (should (equal opened "https://prod.example/jobs/700")))))

(ert-deftest metaflow-runs-test-flow-shortcut ()
  "The f action opens the selected Metaflow flow instead of its Spark URL."
  (metaflow-runs-test--with-launch
    (metaflow-runs--refresh)
    (let ((metaflow-runs-ui-url-template "https://metaflow.example/?flow_id=%f")
          opened)
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (funcall (cdr (assoc "f" (plist-get (metaflow-runs-panel) :actions)))
                 nil (alist-get 'id (car metaflow-runs--cache))))
      (should (equal opened "https://metaflow.example/?flow_id=ExampleFlow")))))

(ert-deftest metaflow-runs-test-flow-url-encoding ()
  "Flow and run values remain encoded URL components."
  (let ((metaflow-runs-ui-url-template "https://metaflow.example/%f/%r"))
    (should (equal (metaflow-runs--flow-url
                    '((flow_name . "Flow & Name") (run_id . "argo/run?x")))
                   "https://metaflow.example/Flow%20%26%20Name/argo%2Frun%3Fx"))))

(ert-deftest metaflow-runs-test-flow-url-missing-config-or-identity ()
  "Unconfigured URLs and unknown flow names cannot open unrelated pages."
  (let ((metaflow-runs-ui-url-template nil))
    (should-error (metaflow-runs--flow-url '((flow_name . "Flow"))) :type 'user-error))
  (let ((metaflow-runs-ui-url-template "https://metaflow.example/?flow_id=%f"))
    (should-error (metaflow-runs--flow-url '((flow . "local-folder"))) :type 'user-error))
  (let ((metaflow-runs-ui-url-template "https://metaflow.example/%f/%r"))
    (should-error (metaflow-runs--flow-url '((flow_name . "Flow") (run_id . "—")))
                  :type 'user-error)))

(ert-deftest metaflow-runs-test-panel-order ()
  "Both dashboard builders receive exactly one Metaflow panel below Databricks."
  (should (equal (agents-workflow--panel-names '("databricks" "jira"))
                 '("databricks" "metaflow" "jira")))
  (should (equal (agents-workflow--panel-names '("metaflow" "databricks" "metaflow"))
                 '("databricks" "metaflow")))
  (should (equal (agents-workflow--panel-names '("github" "pace"))
                 '("github" "pace"))))

(ert-deftest metaflow-runs-test-dashboard-render-and-refresh ()
  "The dashboard renders real records and refreshes them without a subprocess."
  (metaflow-runs-test--with-launch
    (let ((buf (claude-dashboard-create
                :name "metaflow-test" :panels (list (metaflow-runs-panel)))))
      (unwind-protect
          (with-current-buffer buf
            (should (string-match-p "Metaflow Jobs" (buffer-string)))
            (should (string-match-p "Parent" (buffer-string)))
            (should-not (string-match-p "Flow state" (buffer-string)))
            (should (string-match-p "ExampleFlow" (buffer-string)))
            (should (cl-loop for pos from (point-min) below (point-max)
                             thereis (equal (get-text-property pos 'help-echo)
                                            "RUNNING")))
            (metaflow-runs-test--write dir "flow-result.json"
                                       '((flow_exit_code . 1) (success . :json-false)))
            (claude-dashboard--refresh-panel buf (car claude-dashboard--panels))
            (should (cl-loop for pos from (point-min) below (point-max)
                             thereis (equal (get-text-property pos 'help-echo)
                                            "FAILED")))
            (should (equal (alist-get 'spark_state (car metaflow-runs--cache)) "RUNNING")))
        (kill-buffer buf)))))

(ert-deftest metaflow-runs-test-no-launches ()
  "An empty monitor directory produces a useful empty state."
  (with-temp-buffer
    (let ((metaflow-runs-monitor-directories nil))
      (metaflow-runs--refresh)
      (should (equal (aref (cadar (metaflow-runs--entries)) 0)
                     "No monitored launches")))))

(ert-deftest metaflow-runs-test-legacy-history-joins-sole-namespace ()
  "Old manual runs do not create a second identically named flow row."
  (with-temp-buffer
    (setq metaflow-runs--cache
          '(((flow . "ExampleDailyFlow") (flow_name . "ExampleDailyFlow")
             (namespace . "team-prod") (run_id . "argo-today")
             (spark_state . "RUNNING") (monitor_state . "LIVE"))
            ((flow . "ExampleDailyFlow") (flow_name . "ExampleDailyFlow")
             (namespace . nil) (run_id . "384819")
             (spark_state . "FAILED") (monitor_state . "STOPPED"))))
    (let ((rows (metaflow-runs--flows)))
      (should (= (length rows) 1))
      (should (= (length (alist-get 'history (car rows))) 2))
      (should (equal (alist-get 'spark_state (car rows)) "RUNNING"))
      (should-not (alist-get 'namespace (cadr metaflow-runs--cache))))))

(ert-deftest metaflow-runs-test-unknown-namespace-stays-ambiguous ()
  "Unknown history must not be assigned when multiple namespaces exist."
  (with-temp-buffer
    (setq metaflow-runs--cache
          '(((flow_name . "Flow") (namespace . "prod") (run_id . "1"))
            ((flow_name . "Flow") (namespace . "staging") (run_id . "2"))
            ((flow_name . "Flow") (namespace . nil) (run_id . "3"))))
    (should (= (length (metaflow-runs--flows)) 3))))

(ert-deftest metaflow-runs-test-recycled-pid ()
  "An unrelated process reusing a watcher PID must not appear live."
  (cl-letf (((symbol-function 'process-attributes)
             (lambda (_pid) '((args . "python unrelated.py")))))
    (should-not (metaflow-runs--alive-p 999 "/tmp/launch")))
  (cl-letf (((symbol-function 'process-attributes)
             (lambda (_pid) '((args . "python monitor_job.py --output /tmp/launch")))))
    (should (metaflow-runs--alive-p 999 "/tmp/launch"))))

(ert-deftest metaflow-runs-test-failed-before-child-submission ()
  "A launch that fails before submitting Spark has a fixed elapsed time."
  (metaflow-runs-test--with-launch
    (delete-directory (expand-file-name "sjob_100" dir) t)
    (metaflow-runs-test--write dir "flow-result.json"
                               '((flow_exit_code . 1) (success . :json-false)))
    (metaflow-runs--refresh)
    (let ((run (car metaflow-runs--cache)))
      (should (equal (alist-get 'launcher_state run) "FAILED"))
      (should (equal (alist-get 'monitor_state run) "NONE"))
      (should (> (alist-get 'end_time run) 0)))))

(ert-deftest metaflow-runs-test-argo-reruns-grouped-with-failed-history ()
  "Argo reruns share one flow row; both old failures remain in history."
  (let* ((root (make-temp-file "metaflow-argo-" t))
         (metaflow-runs-monitor-directories (list root)))
    (unwind-protect
        (with-temp-buffer
          (dotimes (i 3)
            (let* ((dir (expand-file-name (format "attempt-%d" i) root))
                   (id (format "argo-attempt-%d" i))
                   (phase (if (= i 2) "Running" "Failed"))
                   (stamp (format "2026-09-%02dT12:00:00Z" (+ 10 i))))
              (make-directory dir)
              (with-temp-file (expand-file-name "flow.log" dir)
                (insert (format "Monitoring ExampleBackfillFlow/%s\n" id)))
              (metaflow-runs-test--write dir "monitor.json" '((pid . 999)))
              (metaflow-runs-test--write
               dir "workflow.json"
               `((metadata . ((namespace . "team-prod")
                              (labels . ((metaflow/flow-name . "ExampleBackfillFlow")
                                         (metaflow/run-id . ,id)))))
                 (status . ((phase . ,phase) (startedAt . ,stamp)))))
              (metaflow-runs-test--write
               dir "parent-status.json" `((phase . ,phase) (checked_at . ,(float-time))))
              (unless (= i 2)
                (metaflow-runs-test--write
                 dir "result.json"
                 `((phase . "Failed") (monitor_status . "terminal")
                   (finished_at . ,stamp))))))
          ;; A rearmed monitor for an older attempt must not become the latest run.
          (set-file-times (expand-file-name "attempt-0/monitor.json" root)
                          (time-add nil 1000))
          (metaflow-runs--refresh)
          (should (= (length metaflow-runs--cache) 3))
          (should (= (length (metaflow-runs--entries)) 1))
          (let* ((entry (car (metaflow-runs--entries)))
                 (run (metaflow-runs--find (car entry))))
            (should (equal (aref (cadr entry) 0) "ExampleBackfillFlow"))
            (should (equal (get-text-property 0 'help-echo (aref (cadr entry) 1)) "RUNNING"))
            (should (equal (aref (cadr entry) 5) "argo-attempt-2"))
            (should (equal (mapcar (lambda (r) (alist-get 'parent_state r))
                                   (alist-get 'history run))
                           '("RUNNING" "FAILED" "FAILED")))
            (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
              (metaflow-runs--details nil (car entry)))
            (with-current-buffer "*Metaflow Run Details*"
              (should (string-match-p "argo-attempt-0  Parent: FAILED" (buffer-string)))
              (should (string-match-p "argo-attempt-1  Parent: FAILED" (buffer-string))))))
      (when (get-buffer "*Metaflow Run Details*") (kill-buffer "*Metaflow Run Details*"))
      (delete-directory root t))))

(ert-deftest metaflow-runs-test-argo-log-identity-and-stale-parent ()
  "The monitor log supplies identity; old nonterminal parent states are stale."
  (metaflow-runs-test--with-launch
    (with-temp-file (expand-file-name "flow.log" dir)
      (insert "Monitoring ExampleBackfillFlow/argo-recovery-123\n"))
    (metaflow-runs-test--write
     dir "parent-status.json" `((phase . "Running") (checked_at . ,(- (float-time) 300))))
    (metaflow-runs--refresh)
    (should (equal (alist-get 'flow_name (car metaflow-runs--cache)) "ExampleBackfillFlow"))
    (should (equal (get-text-property 0 'help-echo (aref (cadar (metaflow-runs--entries)) 1)) "RUNNING (unverified)"))
    (metaflow-runs-test--write
     dir "result.json" '((phase . "Failed") (monitor_status . "terminal")))
    (metaflow-runs--refresh)
    (should (equal (get-text-property 0 'help-echo (aref (cadar (metaflow-runs--entries)) 1)) "FAILED"))
    ;; Parent failure still cannot overwrite a live Spark child's state.
    (should (equal (get-text-property 0 'help-echo (aref (cadar (metaflow-runs--entries)) 2)) "RUNNING"))))

(ert-deftest metaflow-runs-test-flow-namespaces-stay-separate ()
  "Identically named flows in different namespaces remain separate."
  (let ((metaflow-runs--cache
         '(((flow_name . "SameFlow") (namespace . "prod") (directory . "/a"))
           ((flow_name . "SameFlow") (namespace . "staging") (directory . "/b")))))
    (should (= (length (metaflow-runs--flows)) 2))))

(provide 'metaflow-runs-tests)
;;; metaflow-runs-tests.el ends here
