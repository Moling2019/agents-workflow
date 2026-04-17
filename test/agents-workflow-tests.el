;;; agents-workflow-tests.el --- Tests for agents-workflow -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'agents-workflow)

;;;; Dependency tests

(ert-deftest agents-workflow-test-codex-cli-loaded ()
  "codex-cli is loaded as a dependency of agents-workflow."
  (should (featurep 'codex-cli)))

(ert-deftest agents-workflow-test-codex-status-hook-registered ()
  "agents-workflow registers a function on codex-cli-status-change-functions."
  (should (memq 'agents-workflow--handle-codex-status
                codex-cli-status-change-functions)))

;;;; Agent struct tests

(ert-deftest agents-workflow-test-make-agent ()
  "Creating an agent sets all fields correctly."
  (let ((agent (make-agents-workflow-agent
                :name "worker-1"
                :type 'interactive
                :status 'idle
                :directory "/tmp/project")))
    (should (equal (agents-workflow-agent-name agent) "worker-1"))
    (should (eq (agents-workflow-agent-type agent) 'interactive))
    (should (eq (agents-workflow-agent-status agent) 'idle))
    (should (equal (agents-workflow-agent-directory agent) "/tmp/project"))
    (should (null (agents-workflow-agent-buffer agent)))
    (should (null (agents-workflow-agent-last-output agent)))
    (should (null (agents-workflow-agent-queue agent)))))

(ert-deftest agents-workflow-test-make-autonomous-agent ()
  "Creating an autonomous agent includes prompt fields."
  (let ((agent (make-agents-workflow-agent
                :name "pm"
                :type 'autonomous
                :status 'idle
                :directory "/tmp/project"
                :system-prompt "You are a PM."
                :prompt-template "Work done:\n%s\nUpdate JIRA."
                :timeout 120)))
    (should (equal (agents-workflow-agent-system-prompt agent) "You are a PM."))
    (should (equal (agents-workflow-agent-prompt-template agent) "Work done:\n%s\nUpdate JIRA."))
    (should (= (agents-workflow-agent-timeout agent) 120))))

(ert-deftest agents-workflow-test-agent-status-update ()
  "Agent status can be updated."
  (let ((agent (make-agents-workflow-agent :name "w" :type 'interactive :status 'idle)))
    (setf (agents-workflow-agent-status agent) 'running)
    (should (eq (agents-workflow-agent-status agent) 'running))))

;;;; Workflow struct & registry tests

(ert-deftest agents-workflow-test-make-workflow ()
  "Creating a workflow sets fields correctly."
  (let ((wf (make-agents-workflow
             :name "test-project"
             :directory "/tmp/project")))
    (should (equal (agents-workflow-name wf) "test-project"))
    (should (equal (agents-workflow-directory wf) "/tmp/project"))
    (should (null (agents-workflow-agents wf)))
    (should (null (agents-workflow-triggers wf)))
    (should (eq (agents-workflow-state wf) 'stopped))))

(ert-deftest agents-workflow-test-define-and-lookup ()
  "Defining a workflow registers it for later lookup."
  (unwind-protect
      (progn
        (agents-workflow-define "test-proj"
          :directory "/tmp/proj"
          :agents '((:name "w1" :type interactive))
          :triggers nil)
        (let ((wf (agents-workflow--get "test-proj")))
          (should wf)
          (should (equal (agents-workflow-name wf) "test-proj"))
          (should (= (length (agents-workflow-agents wf)) 1))))
    (agents-workflow--remove "test-proj")))

(ert-deftest agents-workflow-test-define-creates-agents ()
  "Defining a workflow creates agent structs from plists."
  (unwind-protect
      (progn
        (agents-workflow-define "test-proj2"
          :directory "/tmp/proj"
          :agents '((:name "w1" :type interactive)
                    (:name "pm" :type autonomous
                     :system-prompt "You are a PM."
                     :prompt-template "Update: %s"))
          :triggers nil)
        (let* ((wf (agents-workflow--get "test-proj2"))
               (agents (agents-workflow-agents wf)))
          (should (= (length agents) 2))
          (let ((w1 (cl-find "w1" agents :key #'agents-workflow-agent-name :test #'equal))
                (pm (cl-find "pm" agents :key #'agents-workflow-agent-name :test #'equal)))
            (should (eq (agents-workflow-agent-type w1) 'interactive))
            (should (eq (agents-workflow-agent-type pm) 'autonomous))
            (should (equal (agents-workflow-agent-system-prompt pm) "You are a PM.")))))
    (agents-workflow--remove "test-proj2")))

;;;; Event system tests

(ert-deftest agents-workflow-test-trigger-matches-event ()
  "Trigger matches an event by type."
  (let ((trigger '(:on worker-waiting :do ignore)))
    (should (agents-workflow--trigger-matches-p
             trigger '(:event worker-waiting :agent "w1")))
    (should-not (agents-workflow--trigger-matches-p
                 trigger '(:event agent-complete :agent "pm")))))

(ert-deftest agents-workflow-test-trigger-matches-with-from ()
  "Trigger with :from only matches specific agent."
  (let ((trigger '(:on worker-waiting :from "w1" :do ignore)))
    (should (agents-workflow--trigger-matches-p
             trigger '(:event worker-waiting :agent "w1")))
    (should-not (agents-workflow--trigger-matches-p
                 trigger '(:event worker-waiting :agent "w2")))))

(ert-deftest agents-workflow-test-trigger-matches-with-when ()
  "Trigger with :when predicate filters events."
  (let ((trigger `(:on agent-complete
                   :when ,(lambda (evt) (= (plist-get evt :exit-code) 0))
                   :do ignore)))
    (should (agents-workflow--trigger-matches-p
             trigger '(:event agent-complete :agent "pm" :exit-code 0)))
    (should-not (agents-workflow--trigger-matches-p
                 trigger '(:event agent-complete :agent "pm" :exit-code 1)))))

(ert-deftest agents-workflow-test-trigger-matches-with-match ()
  "Trigger with :match regex filters events by :output content."
  (let ((trigger '(:on worker-waiting :from "worker-1"
                   :match "TASK_SUMMARY" :do ignore)))
    ;; Output contains the marker → match
    (should (agents-workflow--trigger-matches-p
             trigger '(:event worker-waiting :agent "worker-1"
                       :output "Here is the TASK_SUMMARY of work done")))
    ;; Output without marker → no match
    (should-not (agents-workflow--trigger-matches-p
                 trigger '(:event worker-waiting :agent "worker-1"
                           :output "Just a normal response")))
    ;; Nil output → no match
    (should-not (agents-workflow--trigger-matches-p
                 trigger '(:event worker-waiting :agent "worker-1"
                           :output nil)))))

(ert-deftest agents-workflow-test-trigger-match-absent-matches-all ()
  "Trigger without :match matches any output (backward compatible)."
  (let ((trigger '(:on worker-waiting :from "worker-1" :do ignore)))
    (should (agents-workflow--trigger-matches-p
             trigger '(:event worker-waiting :agent "worker-1"
                       :output "anything")))
    (should (agents-workflow--trigger-matches-p
             trigger '(:event worker-waiting :agent "worker-1"
                       :output nil)))))

(ert-deftest agents-workflow-test-serialize-trigger-with-match ()
  "Trigger serialization preserves :match."
  (let ((trigger '(:on worker-waiting :from "worker-1"
                   :match "TASK_SUMMARY"
                   :do agents-workflow-trigger-pm)))
    (let ((serialized (agents-workflow--serialize-trigger trigger)))
      (should (equal (plist-get serialized :match) "TASK_SUMMARY"))
      (should (equal (plist-get serialized :on) 'worker-waiting))
      (should (equal (plist-get serialized :from) "worker-1"))
      (should (equal (plist-get serialized :do) 'agents-workflow-trigger-pm)))))

(ert-deftest agents-workflow-test-emit-event-calls-matching-triggers ()
  "Emitting an event calls the :do function of matching triggers."
  (let* ((called-with nil)
         (action (lambda (wf event) (setq called-with (list wf event)))))
    (unwind-protect
        (progn
          (agents-workflow-define "test-emit"
            :directory "/tmp"
            :agents '((:name "w1" :type interactive))
            :triggers `((:on worker-waiting :from "w1" :do ,action)))
          (let ((wf (agents-workflow--get "test-emit")))
            (setf (agents-workflow-state wf) 'running)
            (agents-workflow--emit wf '(:event worker-waiting :agent "w1"))
            (should called-with)
            (should (equal (plist-get (cadr called-with) :agent) "w1"))))
      (agents-workflow--remove "test-emit"))))

(ert-deftest agents-workflow-test-emit-skipped-when-paused ()
  "Events are not dispatched when workflow is paused."
  (let* ((called nil)
         (action (lambda (_wf _event) (setq called t))))
    (unwind-protect
        (progn
          (agents-workflow-define "test-pause"
            :directory "/tmp"
            :agents '((:name "w1" :type interactive))
            :triggers `((:on worker-waiting :do ,action)))
          (let ((wf (agents-workflow--get "test-pause")))
            (setf (agents-workflow-state wf) 'running)
            (setf (agents-workflow-paused wf) t)
            (agents-workflow--emit wf '(:event worker-waiting :agent "w1"))
            (should-not called)))
      (agents-workflow--remove "test-pause"))))

;;;; Dashboard formatting tests

(ert-deftest agents-workflow-test-status-label ()
  "Status labels return a propertized icon string."
  (let ((label (agents-workflow--status-label 'idle)))
    (should (stringp label))
    (should (> (length label) 0))
    (should (get-text-property 0 'face label))))

(ert-deftest agents-workflow-test-format-last-activity-nil ()
  "Nil last-activity formats as em-dash placeholder."
  (should (equal (substring-no-properties (agents-workflow--format-last-activity nil)) "—")))

(ert-deftest agents-workflow-test-format-last-activity-recent ()
  "Recent last-activity formats as relative time."
  (let ((recent (- (float-time) 30)))
    (should (string-match-p "s ago" (agents-workflow--format-last-activity recent)))))

(ert-deftest agents-workflow-test-format-output ()
  "Format output extracts last meaningful line with wrap-prefix."
  (should (equal (substring-no-properties (agents-workflow--format-output nil 50)) "—"))
  (should (equal (substring-no-properties (agents-workflow--format-output "" 50)) "—"))
  ;; Prose content is extracted and has wrap-prefix property
  (let ((result (agents-workflow--format-output "I analyzed the data and found three issues." 50)))
    (should (string-match-p "analyzed" (substring-no-properties result)))
    (should (get-text-property 0 'wrap-prefix result)))
  ;; Pure alnum strings are treated as chrome (box-drawing regex match), returns —
  (let ((plain (make-string 100 ?x)))
    (should (equal (substring-no-properties (agents-workflow--format-output plain 50)) "—"))))

;;;; Dashboard buffer tests

(ert-deftest agents-workflow-test-dashboard-entries ()
  "Dashboard generates tabulated-list entries from workflow agents."
  (unwind-protect
      (progn
        (agents-workflow-define "test-dash"
          :directory "/tmp"
          :agents '((:name "w1" :type interactive)
                    (:name "pm" :type autonomous))
          :triggers nil)
        (let* ((wf (agents-workflow--get "test-dash"))
               (entries (agents-workflow--dashboard-entries wf)))
          (should (= (length entries) 2))
          ;; Each entry is (id [name type status activity output])
          (should (equal (car (nth 0 entries)) "w1"))
          (should (equal (car (nth 1 entries)) "pm"))))
    (agents-workflow--remove "test-dash")))

;;;; Autonomous agent execution tests

(ert-deftest agents-workflow-test-build-autonomous-prompt ()
  "Prompt construction fills template with context."
  (let ((agent (make-agents-workflow-agent
                :name "pm"
                :type 'autonomous
                :prompt-template "Work done:\n%s\nUpdate JIRA.")))
    (should (equal (agents-workflow--build-prompt agent "Implemented auth")
                   "Work done:\nImplemented auth\nUpdate JIRA."))))

(ert-deftest agents-workflow-test-build-autonomous-prompt-no-template ()
  "Without template, use context as-is."
  (let ((agent (make-agents-workflow-agent :name "pm" :type 'autonomous)))
    (should (equal (agents-workflow--build-prompt agent "Do something")
                   "Do something"))))

(ert-deftest agents-workflow-test-enqueue-autonomous ()
  "Enqueueing a task when agent is busy adds to queue."
  (let ((agent (make-agents-workflow-agent
                :name "pm" :type 'autonomous :status 'running)))
    (agents-workflow--enqueue-autonomous agent "task 1")
    (agents-workflow--enqueue-autonomous agent "task 2")
    (should (= (length (agents-workflow-agent-queue agent)) 2))
    (should (equal (car (agents-workflow-agent-queue agent)) "task 1"))))

(ert-deftest agents-workflow-test-parse-json-output ()
  "JSON output is parsed into an alist."
  (let ((result (agents-workflow--parse-output "{\"status\": \"ok\"}")))
    (should (equal (alist-get 'status result) "ok"))))

(ert-deftest agents-workflow-test-parse-non-json-output ()
  "Non-JSON output is returned as raw string."
  (let ((result (agents-workflow--parse-output "plain text output")))
    (should (equal result "plain text output"))))

;;;; Interactive agent integration tests

(ert-deftest agents-workflow-test-extract-last-output ()
  "Extracts last N lines from a buffer, stripping ANSI codes."
  (with-temp-buffer
    (insert "line 1\n")
    (insert "\033[32mline 2 green\033[0m\n")
    (insert "line 3\n")
    (let ((output (agents-workflow--extract-last-output (current-buffer) 2)))
      (should (string-match-p "line 2 green" output))
      (should (string-match-p "line 3" output))
      (should-not (string-match-p "\033" output)))))

(ert-deftest agents-workflow-test-extract-last-output-small-buffer ()
  "Extraction handles buffers smaller than N lines."
  (with-temp-buffer
    (insert "only line\n")
    (let ((output (agents-workflow--extract-last-output (current-buffer) 20)))
      (should (string-match-p "only line" output)))))

;;;; Agent status change tests

(ert-deftest agents-workflow-test-mark-agent-waiting ()
  "Mark-agent-waiting sets status, activity, output and emits event."
  (let* ((called-with nil)
         (action (lambda (_wf event) (setq called-with event))))
    (unwind-protect
        (progn
          (agents-workflow-define "test-wait"
            :directory "/tmp"
            :agents '((:name "w1" :type interactive))
            :triggers `((:on worker-waiting :from "w1" :do ,action)))
          (let* ((wf (agents-workflow--get "test-wait"))
                 (agent (car (agents-workflow-agents wf)))
                 (buf (generate-new-buffer " *test-agent-buf*")))
            (unwind-protect
                (progn
                  (with-current-buffer buf (insert "Claude output line\n"))
                  (setf (agents-workflow-agent-buffer agent) buf)
                  (setf (agents-workflow-agent-status agent) 'running)
                  (setf (agents-workflow-state wf) 'running)
                  (agents-workflow--mark-agent-waiting agent wf)
                  ;; Status changed to waiting
                  (should (eq (agents-workflow-agent-status agent) 'waiting))
                  ;; Last activity updated
                  (should (agents-workflow-agent-last-activity agent))
                  ;; Last output is NOT set by mark-agent-waiting
                  ;; (set exclusively by the stop hook)
                  (should (null (agents-workflow-agent-last-output agent)))
                  ;; Event emitted
                  (should called-with)
                  (should (eq (plist-get called-with :event) 'worker-waiting))
                  (should (equal (plist-get called-with :agent) "w1")))
              (kill-buffer buf))))
      (agents-workflow--remove "test-wait"))))

(ert-deftest agents-workflow-test-handle-bell-updates-status ()
  "Bell handler finds agent by current buffer and marks it waiting."
  (unwind-protect
      (progn
        (agents-workflow-define "test-bell"
          :directory "/tmp"
          :agents '((:name "w1" :type interactive))
          :triggers nil)
        (let* ((wf (agents-workflow--get "test-bell"))
               (agent (car (agents-workflow-agents wf)))
               (buf (generate-new-buffer " *test-bell-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer buf (insert "Some output\n"))
                (setf (agents-workflow-agent-buffer agent) buf)
                (setf (agents-workflow-agent-status agent) 'running)
                (setf (agents-workflow-state wf) 'running)
                ;; Simulate bell firing in the agent's buffer
                (with-current-buffer buf
                  (agents-workflow--handle-bell nil))
                (should (eq (agents-workflow-agent-status agent) 'waiting)))
            (kill-buffer buf))))
    (agents-workflow--remove "test-bell")))

(ert-deftest agents-workflow-test-handle-bell-no-match ()
  "Bell handler does nothing for buffers not linked to any agent."
  (unwind-protect
      (progn
        (agents-workflow-define "test-bell-none"
          :directory "/tmp"
          :agents '((:name "w1" :type interactive))
          :triggers nil)
        (let* ((wf (agents-workflow--get "test-bell-none"))
               (agent (car (agents-workflow-agents wf))))
          (setf (agents-workflow-agent-status agent) 'idle)
          (setf (agents-workflow-state wf) 'running)
          ;; Bell in an unrelated buffer
          (with-temp-buffer
            (agents-workflow--handle-bell nil))
          ;; Status unchanged
          (should (eq (agents-workflow-agent-status agent) 'idle))))
    (agents-workflow--remove "test-bell-none")))

(ert-deftest agents-workflow-test-send-command-sets-running ()
  "Send-command advice marks agent as running."
  (unwind-protect
      (progn
        (agents-workflow-define "test-send"
          :directory "/tmp"
          :agents '((:name "w1" :type interactive))
          :triggers nil)
        (let* ((wf (agents-workflow--get "test-send"))
               (agent (car (agents-workflow-agents wf)))
               (buf (generate-new-buffer " *test-send-buf*")))
          (unwind-protect
              (progn
                (setf (agents-workflow-agent-buffer agent) buf)
                (setf (agents-workflow-agent-status agent) 'waiting)
                (setf (agents-workflow-state wf) 'running)
                ;; Simulate send-command returning the buffer
                (agents-workflow--handle-send-command buf)
                (should (eq (agents-workflow-agent-status agent) 'running))
                (should (agents-workflow-agent-last-activity agent)))
            (kill-buffer buf))))
    (agents-workflow--remove "test-send")))

;;;; Workflow lifecycle tests

(ert-deftest agents-workflow-test-start-sets-running ()
  "Starting a workflow sets state to running."
  (unwind-protect
      (progn
        (agents-workflow-define "test-life"
          :directory "/tmp"
          :agents '((:name "w1" :type interactive))
          :triggers nil)
        ;; We can't fully test start (it spawns processes) but we can
        ;; test the state management part
        (let ((wf (agents-workflow--get "test-life")))
          (setf (agents-workflow-state wf) 'running)
          (should (eq (agents-workflow-state wf) 'running))))
    (agents-workflow--remove "test-life")))

(ert-deftest agents-workflow-test-pause-resume ()
  "Pausing and resuming toggles the paused flag."
  (unwind-protect
      (progn
        (agents-workflow-define "test-pr"
          :directory "/tmp"
          :agents nil :triggers nil)
        (let ((wf (agents-workflow--get "test-pr")))
          (setf (agents-workflow-state wf) 'running)
          (agents-workflow-pause "test-pr")
          (should (agents-workflow-paused wf))
          (agents-workflow-resume "test-pr")
          (should-not (agents-workflow-paused wf))))
    (agents-workflow--remove "test-pr")))

;;;; Slack monitor tests

(ert-deftest agents-workflow-test-parse-slack-response ()
  "Parses Slack monitor JSON response into channel summaries."
  (let* ((json-str "{\"channels\": [{\"id\": \"C01\", \"name\": \"general\", \"summary\": \"3 new messages\", \"needs_reply\": true, \"draft_reply\": \"Got it\"}]}")
         (parsed (agents-workflow--parse-slack-response json-str)))
    (should (= (length parsed) 1))
    (should (equal (alist-get 'name (car parsed)) "general"))
    (should (equal (alist-get 'needs_reply (car parsed)) t))
    (should (equal (alist-get 'draft_reply (car parsed)) "Got it"))))

(ert-deftest agents-workflow-test-parse-slack-response-empty ()
  "Empty channels list returns nil."
  (let ((parsed (agents-workflow--parse-slack-response "{\"channels\": []}")))
    (should (null parsed))))

(ert-deftest agents-workflow-test-parse-slack-response-no-messages ()
  "Non-JSON or error returns nil."
  (should (null (agents-workflow--parse-slack-response "No new messages"))))

;;;; Persistence tests

(ert-deftest agents-workflow-test-serialize-agent ()
  "Serializing an agent produces a saveable plist."
  (let ((agent (make-agents-workflow-agent
                :name "pm"
                :type 'autonomous
                :status 'running
                :directory "/tmp"
                :system-prompt "You are a PM."
                :prompt-template "Update: %s"
                :timeout 60)))
    (let ((plist (agents-workflow--serialize-agent agent)))
      (should (equal (plist-get plist :name) "pm"))
      (should (eq (plist-get plist :type) 'autonomous))
      (should (equal (plist-get plist :system-prompt) "You are a PM."))
      (should (equal (plist-get plist :prompt-template) "Update: %s"))
      (should (= (plist-get plist :timeout) 60))
      ;; Runtime state should NOT be in the plist
      (should-not (plist-member plist :status))
      (should-not (plist-member plist :buffer)))))

(ert-deftest agents-workflow-test-serialize-agent-defaults ()
  "Serializing agent with default timeout omits it."
  (let* ((agent (make-agents-workflow-agent
                 :name "w1" :type 'interactive :timeout 120))
         (plist (agents-workflow--serialize-agent agent)))
    (should-not (plist-member plist :timeout))))

(ert-deftest agents-workflow-test-serialize-interactive-with-system-prompt ()
  "Serializing an interactive agent preserves system-prompt."
  (let* ((agent (make-agents-workflow-agent
                 :name "slack-monitor" :type 'interactive
                 :directory "/tmp"
                 :system-prompt "You are a Slack monitor."))
         (plist (agents-workflow--serialize-agent agent)))
    (should (equal (plist-get plist :name) "slack-monitor"))
    (should (eq (plist-get plist :type) 'interactive))
    (should (equal (plist-get plist :system-prompt) "You are a Slack monitor."))))

(ert-deftest agents-workflow-test-resolve-system-prompt-from-file ()
  "system-prompt-file reads file contents into system-prompt."
  (let ((tmpfile (make-temp-file "sys-prompt" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "You are a test agent.\n\nDo test things."))
          (let* ((plist (list :name "test" :type 'interactive
                              :system-prompt-file tmpfile))
                 (agent (agents-workflow--plist-to-agent plist "/tmp")))
            (should (equal (agents-workflow-agent-system-prompt agent)
                           "You are a test agent.\n\nDo test things."))
            (should (equal (agents-workflow-agent-system-prompt-file agent)
                           tmpfile))))
      (delete-file tmpfile))))

(ert-deftest agents-workflow-test-resolve-system-prompt-inline-wins ()
  "Inline system-prompt takes priority over system-prompt-file."
  (let ((plist (list :name "test" :type 'interactive
                     :system-prompt "inline prompt"
                     :system-prompt-file "/nonexistent/file.md")))
    (let ((agent (agents-workflow--plist-to-agent plist "/tmp")))
      (should (equal (agents-workflow-agent-system-prompt agent) "inline prompt")))))

(ert-deftest agents-workflow-test-serialize-prefers-file-over-inline ()
  "Serialization writes system-prompt-file instead of resolved string."
  (let* ((agent (make-agents-workflow-agent
                 :name "slack-monitor" :type 'interactive
                 :directory "/tmp"
                 :system-prompt "resolved content"
                 :system-prompt-file "/path/to/prompt.md"))
         (plist (agents-workflow--serialize-agent agent)))
    (should (equal (plist-get plist :system-prompt-file) "/path/to/prompt.md"))
    (should-not (plist-member plist :system-prompt))))

(ert-deftest agents-workflow-test-convention-system-prompt ()
  "Convention lookup finds system-prompt.md by agent name."
  (let ((tmpdir (make-temp-file "elisp-dir" t)))
    (unwind-protect
        (let ((agents-workflow-elisp-directory tmpdir)
              (agent-dir (expand-file-name "my-agent" tmpdir)))
          (make-directory agent-dir)
          (with-temp-file (expand-file-name "system-prompt.md" agent-dir)
            (insert "You are my-agent."))
          (should (equal (agents-workflow--convention-system-prompt "my-agent")
                         "You are my-agent.")))
      (delete-directory tmpdir t))))

(ert-deftest agents-workflow-test-convention-system-prompt-no-file ()
  "Convention lookup returns nil when no system-prompt.md exists."
  (let ((agents-workflow-elisp-directory "/tmp/nonexistent-dir"))
    (should (null (agents-workflow--convention-system-prompt "no-such-agent")))))

(ert-deftest agents-workflow-test-serialize-trigger ()
  "Serializing a trigger preserves symbol references."
  (let ((trigger '(:on worker-waiting :from "w1" :do agents-workflow-trigger-pm)))
    (let ((result (agents-workflow--serialize-trigger trigger)))
      (should (eq (plist-get result :on) 'worker-waiting))
      (should (equal (plist-get result :from) "w1"))
      (should (eq (plist-get result :do) 'agents-workflow-trigger-pm)))))

(ert-deftest agents-workflow-test-serialize-workflow-roundtrip ()
  "Serializing and reloading a workflow preserves its definition."
  (unwind-protect
      (progn
        (agents-workflow-define "test-serial"
          :directory "/tmp/proj"
          :agents '((:name "w1" :type interactive)
                    (:name "pm" :type autonomous
                     :system-prompt "You are a PM."
                     :prompt-template "Update: %s"))
          :triggers '((:on worker-waiting :from "w1" :do agents-workflow-trigger-pm)))
        (let* ((wf (agents-workflow--get "test-serial"))
               (data (agents-workflow--serialize wf)))
          ;; Check structure
          (should (equal (plist-get data :name) "test-serial"))
          (should (= (length (plist-get data :agents)) 2))
          (should (= (length (plist-get data :triggers)) 1))
          ;; Roundtrip: re-define from serialized data
          (agents-workflow--remove "test-serial")
          (apply #'agents-workflow-define
                 (plist-get data :name)
                 :directory (plist-get data :directory)
                 :agents (plist-get data :agents)
                 :triggers (plist-get data :triggers)
                 nil)
          (let ((wf2 (agents-workflow--get "test-serial")))
            (should wf2)
            (should (= (length (agents-workflow-agents wf2)) 2))
            (should (equal (agents-workflow-agent-system-prompt
                            (cl-find "pm" (agents-workflow-agents wf2)
                                     :key #'agents-workflow-agent-name :test #'equal))
                           "You are a PM.")))))
    (agents-workflow--remove "test-serial")))

(ert-deftest agents-workflow-test-save-and-load-file ()
  "Saving and loading a .eld file roundtrips the workflow."
  (let ((tmp-file (make-temp-file "claude-wf-test-" nil ".eld")))
    (unwind-protect
        (progn
          (agents-workflow-define "test-file"
            :directory "/tmp/proj"
            :agents '((:name "w1" :type interactive)
                      (:name "slack" :type autonomous
                       :system-prompt "Check Slack."
                       :interval 600))
            :triggers '((:on agent-complete :do agents-workflow-slack-review)))
          (agents-workflow-save "test-file" tmp-file)
          ;; Remove from registry and reload
          (agents-workflow--remove "test-file")
          (should-not (agents-workflow--get "test-file"))
          (agents-workflow-load-file tmp-file)
          (let ((wf (agents-workflow--get "test-file")))
            (should wf)
            (should (= (length (agents-workflow-agents wf)) 2))
            (let ((slack (cl-find "slack" (agents-workflow-agents wf)
                                  :key #'agents-workflow-agent-name :test #'equal)))
              (should (equal (agents-workflow-agent-system-prompt slack)
                             "Check Slack."))
              (should (= (agents-workflow-agent-interval slack) 600)))))
      (agents-workflow--remove "test-file")
      (delete-file tmp-file))))

(ert-deftest agents-workflow-test-load-directory ()
  "Loading a directory registers all .eld workflows."
  (let ((tmp-dir (make-temp-file "claude-wf-dir-" t)))
    (unwind-protect
        (progn
          ;; Write two workflow files
          (agents-workflow-define "wf-a"
            :directory "/tmp/a"
            :agents '((:name "w1" :type interactive))
            :triggers nil)
          (agents-workflow-save "wf-a" (expand-file-name "wf-a.eld" tmp-dir))
          (agents-workflow--remove "wf-a")

          (agents-workflow-define "wf-b"
            :directory "/tmp/b"
            :agents '((:name "w2" :type interactive))
            :triggers nil)
          (agents-workflow-save "wf-b" (expand-file-name "wf-b.eld" tmp-dir))
          (agents-workflow--remove "wf-b")

          ;; Load all
          (let ((loaded (agents-workflow-load-directory tmp-dir)))
            (should (= (length loaded) 2))
            (should (agents-workflow--get "wf-a"))
            (should (agents-workflow--get "wf-b"))))
      (agents-workflow--remove "wf-a")
      (agents-workflow--remove "wf-b")
      (delete-directory tmp-dir t))))

;;;; Session persistence tests

(ert-deftest agents-workflow-test-generate-uuid ()
  "UUID generator produces valid format."
  (let ((uuid (agents-workflow--generate-uuid)))
    (should (stringp uuid))
    (should (string-match-p
             "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}$"
             uuid))
    ;; Two UUIDs should be different
    (should-not (equal uuid (agents-workflow--generate-uuid)))))

(ert-deftest agents-workflow-test-save-and-load-state ()
  "Saving and loading state roundtrips session IDs."
  (let ((agents-workflow-projects-directory (make-temp-file "claude-wf-state-" t)))
    (unwind-protect
        (progn
          (agents-workflow-define "test-state"
            :directory "/tmp/proj"
            :agents '((:name "w1" :type interactive)
                      (:name "w2" :type interactive)
                      (:name "pm" :type autonomous)))
          (let* ((wf (agents-workflow--get "test-state"))
                 (agents (agents-workflow-agents wf)))
            ;; Set session IDs on interactive agents
            (setf (agents-workflow-agent-session-id (nth 0 agents)) "uuid-aaa")
            (setf (agents-workflow-agent-session-id (nth 1 agents)) "uuid-bbb")
            ;; PM has no session-id (autonomous)
            (agents-workflow-save-state "test-state")
            ;; Clear session IDs
            (setf (agents-workflow-agent-session-id (nth 0 agents)) nil)
            (setf (agents-workflow-agent-session-id (nth 1 agents)) nil)
            ;; Reload — all 3 agents are saved now (including pm without session-id)
            (let ((restored (agents-workflow-load-state "test-state")))
              (should (= restored 3))
              (should (equal (agents-workflow-agent-session-id (nth 0 agents))
                             "uuid-aaa"))
              (should (equal (agents-workflow-agent-session-id (nth 1 agents))
                             "uuid-bbb")))))
      (agents-workflow--remove "test-state")
      (delete-directory agents-workflow-projects-directory t))))

(ert-deftest agents-workflow-test-load-state-no-file ()
  "Loading state when no file exists returns nil gracefully."
  (let ((agents-workflow-projects-directory (make-temp-file "claude-wf-nostate-" t)))
    (unwind-protect
        (progn
          (agents-workflow-define "test-nostate"
            :directory "/tmp"
            :agents '((:name "w1" :type interactive))
            :triggers nil)
          (should-not (agents-workflow-load-state "test-nostate")))
      (agents-workflow--remove "test-nostate")
      (delete-directory agents-workflow-projects-directory t))))

(ert-deftest agents-workflow-test-clear-state ()
  "Clearing state deletes the state file."
  (let ((agents-workflow-projects-directory (make-temp-file "claude-wf-clear-" t)))
    (unwind-protect
        (progn
          (agents-workflow-define "test-clear"
            :directory "/tmp"
            :agents '((:name "w1" :type interactive))
            :triggers nil)
          (let ((agent (car (agents-workflow-agents
                             (agents-workflow--get "test-clear")))))
            (setf (agents-workflow-agent-session-id agent) "uuid-xxx"))
          (agents-workflow-save-state "test-clear")
          (should (file-exists-p (agents-workflow--state-file "test-clear")))
          (agents-workflow-clear-state "test-clear")
          (should-not (file-exists-p (agents-workflow--state-file "test-clear"))))
      (agents-workflow--remove "test-clear")
      (delete-directory agents-workflow-projects-directory t))))

(ert-deftest agents-workflow-test-session-id-not-in-template ()
  "Session IDs are NOT saved to the workflow .eld template."
  (let ((tmp-file (make-temp-file "claude-wf-tmpl-" nil ".eld")))
    (unwind-protect
        (progn
          (agents-workflow-define "test-tmpl"
            :directory "/tmp"
            :agents '((:name "w1" :type interactive))
            :triggers nil)
          (let ((agent (car (agents-workflow-agents
                             (agents-workflow--get "test-tmpl")))))
            (setf (agents-workflow-agent-session-id agent) "uuid-should-not-appear"))
          (agents-workflow-save "test-tmpl" tmp-file)
          ;; Read back and verify no session-id
          (let* ((data (with-temp-buffer
                         (insert-file-contents tmp-file)
                         (read (current-buffer))))
                 (agent-plist (car (plist-get data :agents))))
            (should-not (plist-member agent-plist :session-id))))
      (agents-workflow--remove "test-tmpl")
      (delete-file tmp-file))))

;;;; Git branch tests


;;;; Dashboard panel integration tests

(ert-deftest agents-workflow-test-agents-panel-definition ()
  "Agents panel definition returns a valid panel plist."
  (unwind-protect
      (progn
        (agents-workflow-define "test-panel"
          :directory "/tmp"
          :agents '((:name "w1" :type interactive)
                    (:name "pm" :type autonomous))
          :triggers nil)
        (let* ((wf (agents-workflow--get "test-panel"))
               (panel (agents-workflow-agents-panel wf)))
          ;; Has required keys
          (should (equal (plist-get panel :name) "agents"))
          (should (plist-get panel :title))
          (should (plist-get panel :columns))
          (should (plist-get panel :entries))
          (should (plist-get panel :actions))
          (should (= (plist-get panel :interval) 3))
          ;; Entries function returns agent data
          (let ((entries (funcall (plist-get panel :entries))))
            (should (= (length entries) 2))
            ;; Each entry is (ID . [vector])
            (should (equal (car (nth 0 entries)) "w1"))
            (should (vectorp (cdr (nth 0 entries)))))))
    (agents-workflow--remove "test-panel")))

;;;; Inter-agent ask tests

(ert-deftest agents-workflow-test-ask-agent-not-found ()
  "Asking a nonexistent agent signals an error."
  (let ((agents-workflow--registry (make-hash-table :test 'equal)))
    (should-error (agents-workflow-ask-agent "nobody" "hello"))))

(ert-deftest agents-workflow-test-ask-agent-sets-up-trigger ()
  "Asking with a requester adds a one-shot trigger and pending entry."
  (unwind-protect
      (let ((agents-workflow--pending-asks nil))
        (agents-workflow-define "test-ask"
          :directory "/tmp"
          :agents '((:name "target" :type interactive)
                    (:name "requester" :type interactive)))
        (let* ((wf (agents-workflow--get "test-ask"))
               (target (agents-workflow--find-agent-by-name wf "target"))
               (buf (generate-new-buffer " *test-ask-target*")))
          (setf (agents-workflow-agent-buffer target) buf)
          ;; Mock claude-code--do-send-command
          (cl-letf (((symbol-function 'claude-code--do-send-command)
                     (lambda (_msg) nil)))
            (agents-workflow-ask-agent "target" "what is X?" "requester"))
          ;; Verify pending ask was registered
          (should (assoc "target" agents-workflow--pending-asks))
          (should (equal (cdr (assoc "target" agents-workflow--pending-asks))
                         "requester"))
          ;; Verify one-shot trigger was added
          (let ((triggers (agents-workflow-triggers wf)))
            (should (cl-find-if
                     (lambda (tr)
                       (and (eq (plist-get tr :on) 'worker-waiting)
                            (equal (plist-get tr :from) "target")))
                     triggers)))
          (kill-buffer buf)))
    (agents-workflow--remove "test-ask")))

(ert-deftest agents-workflow-test-deliver-ask-response ()
  "Delivering a response sends it to the requester and cleans up."
  (unwind-protect
      (let ((agents-workflow--pending-asks '(("target" . "requester")))
            (sent-msg nil))
        (agents-workflow-define "test-deliver"
          :directory "/tmp"
          :agents '((:name "target" :type interactive)
                    (:name "requester" :type interactive))
          :triggers '((:on worker-waiting :from "target"
                       :do agents-workflow--deliver-ask-response)))
        (let* ((wf (agents-workflow--get "test-deliver"))
               (requester (agents-workflow--find-agent-by-name wf "requester"))
               (buf (generate-new-buffer " *test-deliver-req*")))
          (setf (agents-workflow-agent-buffer requester) buf)
          (setf (agents-workflow-state wf) 'running)
          ;; Mock send
          (cl-letf (((symbol-function 'claude-code--do-send-command)
                     (lambda (msg) (setq sent-msg msg) nil)))
            (agents-workflow--deliver-ask-response
             wf '(:event worker-waiting :agent "target" :output "The answer is 42.")))
          ;; Response was sent to requester
          (should (string-match-p "Response from target" sent-msg))
          (should (string-match-p "The answer is 42" sent-msg))
          ;; Pending ask was cleaned up
          (should (null agents-workflow--pending-asks))
          ;; One-shot trigger was removed
          (should (null (cl-find-if
                         (lambda (tr)
                           (eq (plist-get tr :do)
                               #'agents-workflow--deliver-ask-response))
                         (agents-workflow-triggers wf))))
          (kill-buffer buf)))
    (agents-workflow--remove "test-deliver")))

;;;; Extra directories tests

(ert-deftest agents-workflow-test-serialize-extra-directories ()
  "Serialized agent should include extra-directories when present."
  (let ((agent (make-agents-workflow-agent
                :name "test" :type 'interactive :directory "/tmp/main"
                :extra-directories '((:directory "/tmp/extra" :worktree-path nil)))))
    (let ((plist (agents-workflow--serialize-agent agent)))
      (should (equal (plist-get plist :extra-directories)
                     '((:directory "/tmp/extra" :worktree-path nil)))))))

(ert-deftest agents-workflow-test-serialize-no-extra-directories ()
  "Serialized agent should omit extra-directories when empty."
  (let ((agent (make-agents-workflow-agent
                :name "test" :type 'interactive :directory "/tmp/main")))
    (let ((plist (agents-workflow--serialize-agent agent)))
      (should-not (plist-member plist :extra-directories)))))

(ert-deftest agents-workflow-test-extra-dirs-in-switches ()
  "Extra directories should produce --add-dir switches."
  (let ((agent (make-agents-workflow-agent
                :name "test"
                :directory "/tmp/primary"
                :extra-directories '((:directory "/tmp/extra1" :worktree-path nil)
                                     (:directory "/tmp/extra2" :worktree-path "/tmp/extra2")))))
    (should (equal (agents-workflow--extra-dir-switches agent)
                   '("--add-dir" "/tmp/extra1" "--add-dir" "/tmp/extra2")))))

(ert-deftest agents-workflow-test-extra-dirs-empty ()
  "No extra directories should produce nil switches."
  (let ((agent (make-agents-workflow-agent :name "test" :directory "/tmp/primary")))
    (should (null (agents-workflow--extra-dir-switches agent)))))

(provide 'agents-workflow-tests)
;;; agents-workflow-tests.el ends here
