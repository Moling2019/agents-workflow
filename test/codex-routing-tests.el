;;; codex-routing-tests.el --- Session ownership tests -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'server)
(require 'agents-workflow)

(defmacro codex-routing-test--with-agents (&rest body)
  "Run BODY with two agents sharing a directory and isolated persistence."
  (declare (indent 0))
  `(let* ((planning (make-agents-workflow-agent
                     :name "planning" :backend 'codex :directory "/shared"
                     :session-id "planning-id" :last-output "Planning before."))
          (geofence (make-agents-workflow-agent
                     :name "geofence" :backend 'codex :directory "/shared"
                     :session-id "geofence-id" :last-output "Geofence before."))
          (wf (make-agents-workflow :name "routing"
                                   :agents (list planning geofence)))
          (agents-workflow--registry (make-hash-table :test #'equal))
          (saved 0))
     (puthash "routing" wf agents-workflow--registry)
     (cl-letf (((symbol-function 'agents-workflow--persist-workflow)
                (lambda (_) (cl-incf saved)))
               ((symbol-function 'claude-dashboard-refresh-all) #'ignore))
       ,@body)))

(defun codex-routing-test--reply (sid &optional name token)
  "Deliver a reply for SID with optional NAME and explicit launch TOKEN."
  (let ((server-eval-args-left
         (list (json-serialize
                `(:type "agent-turn-complete" :thread-id ,sid
                  :last-assistant-message "Correct reply.")))))
    (agents-workflow-handle-codex-reply "/shared" name "routing" token)))

(ert-deftest codex-routing-test-legacy-ignores-daemon-identity ()
  "A known thread routes correctly even with another agent's inherited name."
  (codex-routing-test--with-agents
    (codex-routing-test--reply "planning-id" "geofence")
    (should (equal (agents-workflow-agent-last-output planning) "Correct reply."))
    (should (equal (agents-workflow-agent-last-output geofence)
                   "Geofence before."))
    (should (= saved 0))))

(ert-deftest codex-routing-test-unknown-and-duplicate-ids-are-rejected ()
  "Unknown or multiply owned threads cannot overwrite any row."
  (codex-routing-test--with-agents
    (codex-routing-test--reply "unknown-id" "planning")
    (setf (agents-workflow-agent-session-id geofence) "planning-id")
    (codex-routing-test--reply "planning-id" "planning")
    (should (equal (agents-workflow-agent-last-output planning)
                   "Planning before."))
    (should (equal (agents-workflow-agent-last-output geofence)
                   "Geofence before."))
    (should (= saved 0))))

(ert-deftest codex-routing-test-explicit-binding-and-conflicts ()
  "Only the current launch can claim an unowned thread, once."
  (codex-routing-test--with-agents
    (setf (agents-workflow-agent-session-id planning) nil
          (agents-workflow-agent-metadata planning)
          '(:codex-notify-token "current"))
    (dolist (token '(nil "" "stale"))
      (codex-routing-test--reply "new-id" "planning" token)
      (should-not (agents-workflow-agent-session-id planning)))
    ;; A valid launch token must not steal a sibling's thread.
    (codex-routing-test--reply "geofence-id" "planning" "current")
    (should-not (agents-workflow-agent-session-id planning))
    (codex-routing-test--reply "new-id" "planning" "current")
    (should (equal (agents-workflow-agent-session-id planning) "new-id"))
    (should (= saved 1))
    (setf (agents-workflow-agent-last-output planning) "Keep this.")
    (codex-routing-test--reply "other-thread" "planning" "current")
    (codex-routing-test--reply "new-id" "geofence" "current")
    (codex-routing-test--reply "new-id" "planning" "stale")
    (codex-routing-test--reply "new-id" "planning")
    (should (equal (agents-workflow-agent-last-output planning) "Keep this."))
    (should (equal (agents-workflow-agent-session-id planning) "new-id"))
    (should (equal (agents-workflow-agent-last-output geofence)
                   "Geofence before."))))

(ert-deftest codex-routing-test-pending-fork-rejects-source ()
  "A fork cannot claim its source, even if the source agent was removed."
  (codex-routing-test--with-agents
    (setf (agents-workflow-agent-session-id planning) nil
          (agents-workflow-agent-metadata planning)
          '(:codex-notify-token "fork" :codex-fork-source "source-id"))
    (codex-routing-test--reply "source-id" "planning" "fork")
    (should-not (agents-workflow-agent-session-id planning))
    (codex-routing-test--reply "fork-id" "planning" "fork")
    (should (equal (agents-workflow-agent-session-id planning) "fork-id"))
    (should-not (plist-get (agents-workflow-agent-metadata planning)
                           :codex-fork-source))
    (should (= saved 1))))

(ert-deftest codex-routing-test-saving-does-not-discover-by-directory ()
  "Saving or inspecting an unbound agent cannot adopt a sibling's transcript."
  (codex-routing-test--with-agents
    (let ((file (make-temp-file "routing-state-")))
      (unwind-protect
          (cl-letf (((symbol-function 'agents-workflow--state-file)
                     (lambda (_) file))
                    ((symbol-function 'agents-workflow--codex-latest-session-id)
                     (lambda (&rest _) (ert-fail "Guessed session by directory")))
                    ((symbol-function 'agents-workflow--codex-last-assistant-message)
                     (lambda (&rest _) (ert-fail "Read unowned transcript"))))
            (setf (agents-workflow-agent-session-id planning) nil)
            (agents-workflow--codex-capture-session-id planning "/shared")
            (agents-workflow--capture-last-message planning)
            (agents-workflow-save-state "routing")
            (with-temp-buffer
              (insert-file-contents file)
              (should-not
               (plist-get (car (plist-get (read (current-buffer)) :agents))
                          :session-id))))
        (delete-file file)))))

(ert-deftest codex-routing-test-session-change-invalidates-display-cache ()
  "Changing a verified identity cannot leave another session's cached reply."
  (with-temp-buffer
    (let ((agent (make-agents-workflow-agent
                  :backend 'codex :session-id "new"
                  :last-output "Wrong old reply."))
          (agents-workflow--codex-reply-cache '("old" . "Wrong old reply.")))
      (cl-letf (((symbol-function 'agents-workflow--codex-last-assistant-message)
                 (lambda (_dir sid _home)
                   (should (equal sid "new"))
                   "Correct reply.")))
        (agents-workflow--sync-codex-display agent (current-buffer))
        (should (equal (agents-workflow-agent-last-output agent)
                       "Correct reply."))))))

(ert-deftest codex-routing-test-notify-arguments-ignore-environment ()
  "The shell bridge preserves explicit routing and ignores daemon identity."
  (let* ((root (make-temp-file "notify-bridge-" t))
         (client (expand-file-name "emacsclient" root))
         (capture (expand-file-name "args" root))
         (process-environment (copy-sequence process-environment))
         (payload "{\"last-assistant-message\":\"quoted reply\"}"))
    (unwind-protect
        (progn
          (with-temp-file client
            (insert "#!/bin/sh\nprintf '%s\\000' \"$@\" > \"$CAPTURE\"\n"))
          (set-file-modes client #o700)
          (setenv "PATH" (concat root path-separator (getenv "PATH")))
          (setenv "CAPTURE" capture)
          (setenv "AGENTS_WORKFLOW_AGENT" "wrong-inherited-agent")
          (setenv "AGENTS_WORKFLOW_NAME" "wrong-workflow")
          (dolist (explicit '(nil t))
            (should
             (= 0 (apply #'call-process "/bin/bash" nil nil nil
                         agents-workflow--codex-notify-program
                         (append (when explicit
                                   '("--route" "wf" "quoted \"agent\"" "token"))
                                 (list payload)))))
            (with-temp-buffer
              (insert-file-contents capture)
              (let* ((args (split-string (buffer-string) "\0" t))
                     (form (read (nth 1 args))))
                (should (equal (nth 2 args) payload))
                (should (eq (car form) 'agents-workflow-handle-codex-reply))
                (should (equal (nth 2 form)
                               (if explicit "quoted \"agent\"" "")))
                (should (equal (nth 3 form) (if explicit "wf" "")))
                (should (equal (nth 4 form) (if explicit "token" "")))))))
      (delete-directory root t))))

(provide 'codex-routing-tests)
;;; codex-routing-tests.el ends here
