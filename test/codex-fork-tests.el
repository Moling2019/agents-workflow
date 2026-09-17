;;; codex-fork-tests.el --- Codex dashboard fork tests -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'server)
(require 'agents-workflow)

(ert-deftest codex-fork-test-launch-complete-resume ()
  "Fork the selected session, preserve account settings, then resume its ID."
  (let* ((src (make-agents-workflow-agent
               :name "source" :backend 'codex :directory temporary-file-directory
               :session-id "source-id" :worktree-path "/shared/worktree"
               :extra-directories '("/extra")
               :metadata '(:config-home "~/.codex-personal" :section personal)))
         (wf (make-agents-workflow :name "fork-test" :agents (list src)))
         (agents-workflow--registry (make-hash-table :test 'equal))
         (buf (generate-new-buffer " *codex-fork-test*"))
         launches saved)
    (puthash "fork-test" wf agents-workflow--registry)
    (unwind-protect
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
                  ((symbol-function 'agents-workflow--start-agent)
                   (lambda (a _wf) (agents-workflow--start-codex-interactive a)))
                  ((symbol-function 'agents-workflow--persist-workflow)
                   (lambda (name) (push name saved)))
                  ((symbol-function 'claude-dashboard-refresh-all) #'ignore)
                  ((symbol-function 'codex-cli--find-buffers-for-directory)
                   (lambda (_dir) nil))
                  ((symbol-function 'codex-cli--start)
                   (lambda (dir name args)
                     (push (list dir name args (getenv "CODEX_HOME")) launches)
                     buf))
                  ((symbol-function 'agents-workflow--convention-system-prompt)
                   (lambda (_name) "Do not resend this inherited prompt."))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _) (ert-fail "Fork resent a startup prompt"))))
          (agents-workflow--panel-fork-agent "fork-test" "source")
          (let ((fork (cadr (agents-workflow-agents wf))))
            (should (equal (car launches)
                           (list temporary-file-directory "source-fork-1"
                                 '("fork" "source-id")
                                 (expand-file-name "~/.codex-personal"))))
            (should-not (agents-workflow-agent-session-id fork))
            (should (equal (agents-workflow-agent-worktree-path fork)
                           "/shared/worktree"))
            (should (equal (agents-workflow-agent-extra-directories fork)
                           '("/extra")))
            (should-not (plist-member (agents-workflow-agent-metadata src)
                                      :codex-fork-source))
            (let ((server-eval-args-left
                   (list (json-serialize
                          '(:type "agent-turn-complete" :thread-id "fork-id"
                            :last-assistant-message "Independent reply.")))))
              (agents-workflow-handle-codex-reply
               temporary-file-directory "source-fork-1" "fork-test"))
            (should (equal (agents-workflow-agent-session-id fork) "fork-id"))
            (should (equal (agents-workflow-agent-session-id src) "source-id"))
            (should (equal (agents-workflow-agent-last-output fork)
                           "Independent reply."))
            (should-not (agents-workflow-agent-last-output src))
            (should-not (plist-member (agents-workflow-agent-metadata fork)
                                      :codex-fork-source))
            (should (eq (plist-get (agents-workflow-agent-metadata fork) :section)
                        'personal))
            (should (= (length saved) 2))
            (cl-letf (((symbol-function 'agents-workflow--convention-system-prompt)
                       (lambda (_name) nil)))
              (agents-workflow--start-codex-interactive fork))
            (should (equal (nth 2 (car launches)) '("resume" "fork-id")))))
      (kill-buffer buf))))

(ert-deftest codex-fork-test-pending-state-does-not-claim-source ()
  "Save a pending fork without assigning another session from its directory."
  (let* ((dir (make-temp-file "codex-fork-state-" t))
         (file (expand-file-name "state.eld" dir))
         (agent (make-agents-workflow-agent
                 :name "fork" :backend 'codex :directory dir
                 :metadata '(:codex-fork-source "source-id")))
         (wf (make-agents-workflow :name "pending" :agents (list agent)))
         (agents-workflow--registry (make-hash-table :test 'equal)))
    (puthash "pending" wf agents-workflow--registry)
    (unwind-protect
        (cl-letf (((symbol-function 'agents-workflow--state-file)
                   (lambda (_name) file))
                  ((symbol-function 'agents-workflow--codex-latest-session-id)
                   (lambda (&rest _) (ert-fail "Ambiguous session lookup"))))
          (agents-workflow--codex-capture-session-id agent dir)
          (agents-workflow-save-state "pending")
          (with-temp-buffer
            (insert-file-contents file)
            (let ((entry (car (plist-get (read (current-buffer)) :agents))))
              (should-not (plist-get entry :session-id))
              (should (equal (plist-get (plist-get entry :metadata)
                                       :codex-fork-source)
                             "source-id")))))
      (delete-directory dir t))))

(ert-deftest codex-fork-test-missing-id-does-not-guess ()
  "A source with unknown identity cannot fork a sibling's conversation."
  (let* ((src (make-agents-workflow-agent
               :name "source" :backend 'codex :directory temporary-file-directory))
         (wf (make-agents-workflow :name "unknown" :agents (list src)))
         (agents-workflow--registry (make-hash-table :test 'equal)))
    (puthash "unknown" wf agents-workflow--registry)
    (should-error (agents-workflow--panel-fork-agent "unknown" "source")
                  :type 'user-error)
    (should (equal (agents-workflow-agents wf) (list src)))))

(ert-deftest codex-fork-test-failed-launch-keeps-source ()
  "A failed launch can retry the fork without resuming the source session."
  (let ((agent (make-agents-workflow-agent
                :name "fork" :backend 'codex :directory temporary-file-directory
                :metadata '(:codex-fork-source "source-id"))))
    (cl-letf (((symbol-function 'codex-cli--find-buffers-for-directory)
               (lambda (_dir) nil))
              ((symbol-function 'codex-cli--start) (lambda (&rest _) nil)))
      (agents-workflow--start-codex-interactive agent)
      (should-not (agents-workflow-agent-buffer agent))
      (should-not (agents-workflow-agent-session-id agent))
      (should (equal (plist-get (agents-workflow-agent-metadata agent)
                                :codex-fork-source)
                     "source-id")))))

(provide 'codex-fork-tests)
;;; codex-fork-tests.el ends here
