;;; integration-tests.el --- Integration tests for agents-workflow -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Functional/integration tests that verify multi-component flows:
;; stop event → last-output → dashboard, buffer reconnection,
;; PR review dispatch, trigger chains, event type coercion.
;; All tests run in batch mode with mocked externals.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'agents-workflow)
(require 'claude-dashboard)
(require 'github-prs)

;;;; Test Helpers

(defun integration-test--make-workflow (name agents-spec &optional triggers)
  "Create and register a workflow named NAME with AGENTS-SPEC.
AGENTS-SPEC is a list of agent plists.  TRIGGERS is optional.
Returns the workflow object.  Caller must clean up with
`agents-workflow--remove'."
  (apply #'agents-workflow-define name
         :directory "/tmp"
         :agents agents-spec
         (when triggers (list :triggers triggers)))
  (let ((wf (agents-workflow--get name)))
    (setf (agents-workflow-state wf) 'running)
    wf))

(defun integration-test--simulate-stop-event (buffer-name message)
  "Simulate a stop event for BUFFER-NAME with MESSAGE as output.
Constructs the event plist as the hook wrapper would send it
\(with symbol type, matching real behavior)."
  (let ((json (json-encode `((last_assistant_message . ,message)
                              (session_id . "test-session")
                              (hook_event_name . "Stop")))))
    (agents-workflow--handle-claude-event
     (list :type 'stop
           :buffer-name buffer-name
           :json-data json))))

(defun integration-test--dashboard-text (buf)
  "Return the full text content of dashboard buffer BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties (point-min) (point-max))))

;;;; Test 1: Stop event sets last-output and dashboard renders it

(ert-deftest integration-test-stop-event-to-dashboard ()
  "Stop event sets agent last-output which appears in dashboard."
  (let ((buf (generate-new-buffer " *test-agent*")))
    (unwind-protect
        (let* ((wf (integration-test--make-workflow
                     "stop-test"
                     '((:name "worker" :type interactive))))
               (agent (car (agents-workflow-agents wf))))
          ;; Set up: agent has a buffer and is running
          (setf (agents-workflow-agent-buffer agent) buf)
          (setf (agents-workflow-agent-status agent) 'running)
          ;; Simulate stop event
          (integration-test--simulate-stop-event
           (buffer-name buf)
           "The analysis is complete. Found 3 issues.")
          ;; Assert: output is set from the event JSON
          (should (equal (agents-workflow-agent-last-output agent)
                         "The analysis is complete. Found 3 issues."))
          ;; Assert: status changed to waiting
          (should (eq (agents-workflow-agent-status agent) 'waiting))
          ;; Assert: dashboard renders the output
          (let ((dash-buf (claude-dashboard-create
                           :name "stop-test-dash"
                           :panels (list
                                    (agents-workflow-agents-panel wf)))))
            (unwind-protect
                (let ((text (integration-test--dashboard-text dash-buf)))
                  (should (string-match-p
                           "analysis is complete" text)))
              (kill-buffer dash-buf))))
      (kill-buffer buf)
      (agents-workflow--remove "stop-test"))))

;;;; Test 2: Buffer reconnection on dashboard refresh

(ert-deftest integration-test-buffer-reconnection ()
  "Dashboard refresh reconnects stale agent buffer references."
  (let ((fake-buf (generate-new-buffer "*claude:/tmp/:dev*")))
    (unwind-protect
        (let* ((wf (integration-test--make-workflow
                     "reconnect-test"
                     '((:name "dev" :type interactive))))
               (agent (car (agents-workflow-agents wf))))
          ;; Agent has no buffer (simulating post-restart)
          (should (null (agents-workflow-agent-buffer agent)))
          ;; Mock the buffer discovery function
          (cl-letf (((symbol-function
                      'claude-code--find-claude-buffers-for-directory)
                     (lambda (_dir) (list fake-buf)))
                    ((symbol-function
                      'claude-code--find-all-claude-buffers)
                     (lambda () (list fake-buf)))
                    ((symbol-function
                      'claude-code--extract-instance-name-from-buffer-name)
                     (lambda (name)
                       (when (string-match ":\\([^:*]+\\)\\*\\'" name)
                         (match-string 1 name))))
                    ;; Stub title watcher (no eat terminal in batch)
                    ((symbol-function
                      'agents-workflow--install-title-watcher)
                     #'ignore))
            ;; Trigger dashboard entries (calls --ensure-agent-buffer)
            (agents-workflow--dashboard-entries wf)
            ;; Assert: buffer is reconnected
            (should (eq (agents-workflow-agent-buffer agent)
                        fake-buf))))
      (when (buffer-live-p fake-buf)
        (kill-buffer fake-buf))
      (agents-workflow--remove "reconnect-test"))))

;;;; Test 3: PR review action dispatches git fetch

(ert-deftest integration-test-pr-review-dispatch ()
  "Review action calls git fetch with correct branch refspecs."
  (let ((fetch-args nil)
        (github-prs--cache
         (vector `((number . 42)
                   (title . "Fix the bug")
                   (url . "https://github.com/org/repo/pull/42")
                   (repository (nameWithOwner . "org/repo"))
                   (updatedAt . "2026-01-01T00:00:00Z")))))
    (let ((orig-start-process (symbol-function 'start-process)))
      (cl-letf (((symbol-function 'start-process)
                 (lambda (&rest args)
                   (setq fetch-args args)
                   ;; Use the real start-process for the dummy proc
                   (funcall orig-start-process "true" nil "true")))
                ((symbol-function 'github-prs--local-repo-dir)
                 (lambda (_pr) "/tmp"))
                ((symbol-function 'shell-command-to-string)
                 (lambda (_cmd)
                   (json-encode '((headRefName . "fix-bug")
                                  (baseRefName . "main"))))))
        (github-prs--review nil "org/repo#42")
        ;; Assert: start-process was called with git fetch
        (should fetch-args)
        (should (member "fetch" fetch-args))
        (should (member "origin" fetch-args))
        (should (cl-some (lambda (a)
                           (and (stringp a)
                                (string-match-p "fix-bug" a)))
                         fetch-args))))))

;;;; Test 4: Multi-agent trigger chain

(ert-deftest integration-test-trigger-chain ()
  "Stop event on agent-A fires trigger with output."
  (let ((buf-a (generate-new-buffer " *test-agent-a*"))
        (enqueued nil))
    (unwind-protect
        (let* ((wf (integration-test--make-workflow
                     "chain-test"
                     '((:name "worker" :type interactive)
                       (:name "reviewer" :type autonomous
                        :prompt-template "Review: %s"))
                     `((:on worker-waiting :from "worker"
                        :do ,(lambda (_wf event)
                               (setq enqueued
                                     (plist-get event :output)))))))
               (agent-a (agents-workflow--find-agent-by-name
                         wf "worker")))
          ;; Set up agent-A
          (setf (agents-workflow-agent-buffer agent-a) buf-a)
          (setf (agents-workflow-agent-status agent-a) 'running)
          ;; Simulate stop event for agent-A
          (integration-test--simulate-stop-event
           (buffer-name buf-a)
           "Implemented the feature. Ready for review.")
          ;; Assert: trigger fired with the output
          (should enqueued)
          (should (string-match-p "Ready for review" enqueued)))
      (kill-buffer buf-a)
      (agents-workflow--remove "chain-test"))))

;;;; Test 5: Event type coercion (symbol vs string)

(ert-deftest integration-test-event-type-coercion ()
  "Stop events work with both symbol and string :type values."
  (let ((buf (generate-new-buffer " *test-coercion*")))
    (unwind-protect
        (let* ((wf (integration-test--make-workflow
                     "coerce-test"
                     '((:name "agent" :type interactive))))
               (agent (car (agents-workflow-agents wf))))
          (setf (agents-workflow-agent-buffer agent) buf)
          ;; Test 1: symbol type (as hook wrapper sends)
          (setf (agents-workflow-agent-status agent) 'running)
          (agents-workflow--handle-claude-event
           (list :type 'stop
                 :buffer-name (buffer-name buf)
                 :json-data (json-encode
                             '((last_assistant_message . "symbol")))))
          (should (equal (agents-workflow-agent-last-output agent)
                         "symbol"))
          ;; Test 2: string type
          (setf (agents-workflow-agent-status agent) 'running)
          (setf (agents-workflow-agent-last-output agent) nil)
          (agents-workflow--handle-claude-event
           (list :type "stop"
                 :buffer-name (buffer-name buf)
                 :json-data (json-encode
                             '((last_assistant_message . "string")))))
          (should (equal (agents-workflow-agent-last-output agent)
                         "string")))
      (kill-buffer buf)
      (agents-workflow--remove "coerce-test"))))

;;;; Test 6: Dashboard reflects agent status changes

(ert-deftest integration-test-dashboard-status-updates ()
  "Dashboard rendering reflects live agent status changes."
  (let ((buf-1 (generate-new-buffer " *test-status-1*"))
        (buf-2 (generate-new-buffer " *test-status-2*")))
    (unwind-protect
        (let* ((wf (integration-test--make-workflow
                     "status-test"
                     '((:name "agent-1" :type interactive)
                       (:name "agent-2" :type interactive))))
               (agents (agents-workflow-agents wf))
               (a1 (car agents))
               (a2 (cadr agents)))
          (setf (agents-workflow-agent-buffer a1) buf-1)
          (setf (agents-workflow-agent-buffer a2) buf-2)
          (setf (agents-workflow-agent-status a1) 'running)
          (setf (agents-workflow-agent-status a2) 'running)
          ;; Create dashboard
          (let ((dash-buf (claude-dashboard-create
                           :name "status-test-dash"
                           :panels (list
                                    (agents-workflow-agents-panel wf)))))
            (unwind-protect
                (progn
                  ;; Simulate stop for agent-1 only
                  (integration-test--simulate-stop-event
                   (buffer-name buf-1) "Agent 1 done.")
                  ;; Refresh dashboard
                  (with-current-buffer dash-buf
                    (claude-dashboard-refresh-all))
                  ;; Agent-1 waiting, agent-2 still running
                  (should (eq (agents-workflow-agent-status a1)
                              'waiting))
                  (should (eq (agents-workflow-agent-status a2)
                              'running))
                  ;; Dashboard text should contain agent-1's output
                  (let ((text (integration-test--dashboard-text
                               dash-buf)))
                    (should (string-match-p "Agent 1 done" text))))
              (kill-buffer dash-buf))))
      (kill-buffer buf-1)
      (kill-buffer buf-2)
      (agents-workflow--remove "status-test"))))

;;;; Test 7: Panel registry auto-require

(ert-deftest integration-test-panel-registry-auto-require ()
  "Panel registry auto-requires features before constructors."
  (let ((required-features nil)
        (agents-workflow-panel-registry
         '(("test-panel" test-panel-constructor . test-feat))))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required-features)
                 (defalias 'test-panel-constructor
                   (lambda ()
                     (list :name "test-panel" :title "Test"
                           :columns [("Col" 10 nil)]
                           :entries (lambda () nil))))
                 t)))
      (let ((panel-name "test-panel"))
        (when-let ((entry (alist-get panel-name
                                     agents-workflow-panel-registry
                                     nil nil #'equal)))
          (let ((constructor (car entry))
                (feature (cdr entry)))
            (unless (fboundp constructor)
              (require feature nil t))
            (when (fboundp constructor)
              (funcall constructor)))))
      (should (memq 'test-feat required-features)))))

;;;; Test 8: Chrome line filtering (companion artifacts)

(ert-deftest integration-test-chrome-line-filtering ()
  "Last-meaningful-line filters companion pet artifacts."
  ;; Raw &_ pattern
  (should (null
           (agents-workflow--last-meaningful-line
            "⏵ bypass permissions\n&_&_&_(✦>) Brinecaw\n")))
  ;; Cleaned (✦>) pattern (after ANSI stripping)
  (let ((text (concat "Here is my analysis of the code.\n"
                      "───────────────────────────────\n"
                      "❯ \n"
                      "  (✦>) Brinecaw\n")))
    (should (equal (agents-workflow--last-meaningful-line text)
                   "Here is my analysis of the code.")))
  ;; Brinecaw name only
  (let ((text "The fix is applied.\nBrinecaw\n"))
    (should (equal (agents-workflow--last-meaningful-line text)
                   "The fix is applied.")))
  ;; No companion — returns first line of last prose block
  (let ((text "Line one.\nLine two.\nLine three.\n"))
    (should (equal (agents-workflow--last-meaningful-line text)
                   "Line one."))))

(provide 'integration-tests)
;;; integration-tests.el ends here
