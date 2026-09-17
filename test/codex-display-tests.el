;;; codex-display-tests.el --- Codex display recovery tests -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'agents-workflow)

(ert-deftest codex-display-test-refresh-recovers-working-and-output ()
  "Refresh repairs a restored agent even without a terminal transition."
  (with-temp-buffer
    (insert "• Working (12m 10s • esc to interrupt)")
    (setq codex-cli--status 'working)
    (let* ((agent (make-agents-workflow-agent
                   :name "working" :backend 'codex :buffer (current-buffer)
                   :directory temporary-file-directory :session-id "exact-id"
                   :metadata '(:config-home "~/.codex-personal")))
           (wf (make-agents-workflow :name "display" :agents (list agent)))
           (reads 0))
      (cl-letf (((symbol-function 'agents-workflow--codex-last-assistant-message)
                 (lambda (_dir sid config-home)
                   (should (equal sid "exact-id"))
                   (should (equal config-home "~/.codex-personal"))
                   (cl-incf reads)
                   "Prior response. The final sentence."))
                ((symbol-function 'agents-workflow--emit)
                 (lambda (&rest _) (ert-fail "Rendering emitted an event"))))
        (agents-workflow--dashboard-entries wf)
        (should (eq (agents-workflow-agent-status agent) 'running))
        (should (equal (agents-workflow-agent-last-output agent)
                       "The final sentence."))
        ;; Simulate another workflow reload while the terminal stays alive.
        (setf (agents-workflow-agent-last-output agent) nil
              (agents-workflow-agent-status agent) 'idle)
        (agents-workflow--dashboard-entries wf)
        (should (eq (agents-workflow-agent-status agent) 'running))
        (should (equal (agents-workflow-agent-last-output agent)
                       "The final sentence."))
        (should (= reads 1))
        (erase-buffer)
        (setq codex-cli--status 'idle)
        (agents-workflow--dashboard-entries wf)
        (should (eq (agents-workflow-agent-status agent) 'waiting))))))

(ert-deftest codex-display-test-missing-reply-cached ()
  "A session without replies is checked only once, with no directory guess."
  (with-temp-buffer
    (let ((agent (make-agents-workflow-agent :backend 'codex))
          (reads 0))
      (cl-letf (((symbol-function 'agents-workflow--codex-last-assistant-message)
                 (lambda (&rest _) (cl-incf reads) nil)))
        (agents-workflow--sync-codex-display agent (current-buffer))
        (should (= reads 0))
        (setf (agents-workflow-agent-session-id agent) "new-id")
        (agents-workflow--sync-codex-display agent (current-buffer))
        (agents-workflow--sync-codex-display agent (current-buffer))
        (should (= reads 1))
        (should-not (agents-workflow-agent-last-output agent))))))

(ert-deftest codex-display-test-old-reply-exact-session ()
  "Find an older final reply in the correct account and session."
  (let* ((home (make-temp-file "codex-display-" t))
         (root (expand-file-name "sessions" home))
         (file (expand-file-name "rollout-exact-id.jsonl" root)))
    (unwind-protect
        (progn
          (make-directory root)
          (with-temp-file file
            (insert (json-serialize
                     '(:type "response_item"
                       :payload (:role "assistant" :type "message"
                                 :phase "final_answer"
                                 :content [(:type "output_text"
                                            :text "Saved final reply.")]))))
            ;; A long current turn pushes the prior reply out of a 256 KB tail.
            (dotimes (_ 4000)
              (insert "\n{\"type\":\"tool-output\",\"padding\":\""
                      (make-string 80 ?x) "\"}")))
          (with-temp-file (expand-file-name "rollout-sibling.jsonl" root)
            (insert "another session"))
          (cl-letf (((symbol-function 'agents-workflow--codex-latest-rollout-file)
                     (lambda (&rest _) (ert-fail "Used ambiguous directory lookup"))))
            (should (equal (agents-workflow--codex-last-assistant-message
                            "/unused" "exact-id" home)
                           "Saved final reply."))
            (should-not (agents-workflow--codex-last-assistant-message
                         "/unused" "missing-id" home))))
      (delete-directory home t))))

(provide 'codex-display-tests)
;;; codex-display-tests.el ends here
