;;; databricks-runs-tests.el --- Tests for databricks-runs auto-notification -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'databricks-runs)

;;; --- run-terminal-p ---

(ert-deftest databricks-runs-test-terminal-p-terminated ()
  "TERMINATED is a terminal state."
  (should (databricks-runs--run-terminal-p
           '((state . "TERMINATED") (run_id . "1")))))

(ert-deftest databricks-runs-test-terminal-p-internal-error ()
  "INTERNAL_ERROR is a terminal state."
  (should (databricks-runs--run-terminal-p
           '((state . "INTERNAL_ERROR") (run_id . "2")))))

(ert-deftest databricks-runs-test-terminal-p-skipped ()
  "SKIPPED is a terminal state."
  (should (databricks-runs--run-terminal-p
           '((state . "SKIPPED") (run_id . "3")))))

(ert-deftest databricks-runs-test-terminal-p-running ()
  "RUNNING is not a terminal state."
  (should-not (databricks-runs--run-terminal-p
               '((state . "RUNNING") (run_id . "4")))))

(ert-deftest databricks-runs-test-terminal-p-pending ()
  "PENDING is not a terminal state."
  (should-not (databricks-runs--run-terminal-p
               '((state . "PENDING") (run_id . "5")))))

;;; --- detect-completions ---

(ert-deftest databricks-runs-test-detect-newly-terminated ()
  "Detect a run that moved from RUNNING to TERMINATED."
  (let ((old [((run_id . "100") (state . "RUNNING"))])
        (new [((run_id . "100") (state . "TERMINATED") (result_state . "SUCCESS"))]))
    (let ((result (databricks-runs--detect-completions old new)))
      (should (= 1 (length result)))
      (should (equal "100" (alist-get 'run_id (car result)))))))

(ert-deftest databricks-runs-test-detect-no-change ()
  "Return nil when nothing changed."
  (let ((old [((run_id . "100") (state . "RUNNING"))])
        (new [((run_id . "100") (state . "RUNNING"))]))
    (should-not (databricks-runs--detect-completions old new))))

(ert-deftest databricks-runs-test-detect-already-terminal ()
  "Return nil when run was already terminal in old cache."
  (let ((old [((run_id . "100") (state . "TERMINATED"))])
        (new [((run_id . "100") (state . "TERMINATED"))]))
    (should-not (databricks-runs--detect-completions old new))))

(ert-deftest databricks-runs-test-detect-new-run-already-terminal ()
  "Detect a brand-new run that appears already terminal."
  (let ((old [])
        (new [((run_id . "200") (state . "TERMINATED") (result_state . "FAILED"))]))
    (let ((result (databricks-runs--detect-completions old new)))
      (should (= 1 (length result)))
      (should (equal "200" (alist-get 'run_id (car result)))))))

(ert-deftest databricks-runs-test-detect-nil-old-cache ()
  "Handle nil old cache (first poll)."
  (let ((new [((run_id . "300") (state . "TERMINATED") (result_state . "SUCCESS"))
              ((run_id . "301") (state . "RUNNING"))]))
    (let ((result (databricks-runs--detect-completions nil new)))
      (should (= 1 (length result)))
      (should (equal "300" (alist-get 'run_id (car result)))))))

(ert-deftest databricks-runs-test-detect-multiple-completions ()
  "Detect multiple runs completing at once."
  (let ((old [((run_id . "400") (state . "RUNNING"))
              ((run_id . "401") (state . "RUNNING"))])
        (new [((run_id . "400") (state . "TERMINATED") (result_state . "SUCCESS"))
              ((run_id . "401") (state . "INTERNAL_ERROR") (result_state . "FAILED"))]))
    (let ((result (databricks-runs--detect-completions old new)))
      (should (= 2 (length result))))))

;;; --- Owner registry ---

(ert-deftest databricks-runs-test-register-unregister ()
  "Register and unregister ownership roundtrip."
  (let ((databricks-runs--owners nil))
    (databricks-runs-register-owner "500" "my-wf" "worker")
    (should (equal '(("500" . ("my-wf" . "worker"))) databricks-runs--owners))
    (databricks-runs-unregister-owner "500")
    (should-not databricks-runs--owners)))

(ert-deftest databricks-runs-test-register-overwrites ()
  "Re-registering the same run-id updates the owner."
  (let ((databricks-runs--owners nil))
    (databricks-runs-register-owner "600" "wf1" "agent1")
    (databricks-runs-register-owner "600" "wf2" "agent2")
    (should (= 1 (length databricks-runs--owners)))
    (should (equal '("wf2" . "agent2")
                   (cdr (assoc "600" databricks-runs--owners))))))

(ert-deftest databricks-runs-test-list-owners ()
  "list-owners returns the current registry."
  (let ((databricks-runs--owners '(("700" . ("wf" . "ag")))))
    (should (equal '(("700" . ("wf" . "ag")))
                   (databricks-runs-list-owners)))))

(provide 'databricks-runs-tests)
;;; databricks-runs-tests.el ends here
